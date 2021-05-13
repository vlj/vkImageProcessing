#include <WindowingSystem.hpp>

#include <GPUCommandAsync.hpp>

namespace WindowingSystem {

vk::UniqueSurfaceKHR GetSurfaceFromGLFWWindows(vk::Instance instance, GLFWwindow *window) {
  VkSurfaceKHR surfaceptr;
  glfwCreateWindowSurface(instance, window, nullptr, &surfaceptr);
  return vk::UniqueSurfaceKHR(surfaceptr, instance);
}

SwapChainSupport::SwapChainSupport(Renderer &r, vk::SurfaceKHR surface, size_t width, size_t height) : renderer(r) {
  auto surfaceSupport = r.physdev.getSurfaceSupportKHR(0, surface);
  auto capabilities = r.physdev.getSurfaceCapabilitiesKHR(surface);
  auto presentModes = r.physdev.getSurfacePresentModesKHR(surface);
  auto formats = r.physdev.getSurfaceFormatsKHR(surface);

  auto swapChainCreateInfo = vk::SwapchainCreateInfoKHR()
                                 .setMinImageCount(2)
                                 .setImageExtent(capabilities.currentExtent)
                                 .setImageFormat(vk::Format::eB8G8R8A8Unorm)
                                 .setPresentMode(presentModes[0])
                                 .setImageArrayLayers(1)
                                 .setImageUsage(vk::ImageUsageFlagBits::eTransferDst | vk::ImageUsageFlagBits::eColorAttachment)
                                 .setSurface(surface);
  swapChain = r.dev->createSwapchainKHRUnique(swapChainCreateInfo);
  swapChainImages = r.dev->getSwapchainImagesKHR(*swapChain);

  extent = capabilities.currentExtent;

  Base::GPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool)
      .then(renderer.queue,
            [&](auto &commandBuffer, auto &&dummy) {
              for (auto img : swapChainImages) {
                auto imgViewInfo =
                    vk::ImageViewCreateInfo()
                        .setImage(img)
                        .setFormat(vk::Format::eB8G8R8A8Unorm)
                        .setViewType(vk::ImageViewType::e2D)
                        .setSubresourceRange(
                            vk::ImageSubresourceRange().setAspectMask(vk::ImageAspectFlagBits::eColor).setLayerCount(1).setLevelCount(1));
                swapChainImageViews.emplace_back(r.dev->createImageViewUnique(imgViewInfo));
                Base::LowLevelTransition<vk::ImageLayout::ePresentSrcKHR, vk::ImageLayout::eUndefined>(*commandBuffer, img);
              }
              return dummy;
            })
      .Sync();
}

vk::ResultValue<uint32_t> SwapChainSupport::GetPresentImage() {
  auto fenceinfo = vk::FenceCreateInfo();
  auto fence = renderer.dev->createFenceUnique(fenceinfo);

  auto res = renderer.dev->acquireNextImageKHR(*swapChain, -1, vk::Semaphore(), *fence);

  auto res2 = renderer.dev->waitForFences(*fence, true, -1);
  return res;
}

void SwapChainSupport::Present(uint32_t idx) {
  auto indexes = std::vector({idx});
  auto swapChains = std::vector({*swapChain});

  auto info = vk::PresentInfoKHR().setImageIndices(indexes).setSwapchains(swapChains);

  auto res = renderer.queue.presentKHR(info);
}

void CopyToPresentImage(vk::Device dev, vk::CommandPool commandPool, vk::Queue queue, vk::DescriptorPool descriptorSetPool,
                               Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eB8G8R8A8Unorm> &texout, size_t width,
                               size_t height, vk::Image presentImage) {

  auto cmdbuf = Base::CreateOneShotStartedBuffer(dev, commandPool);

  // TODO: Factorise this with transition
  {
    auto barriers = std::vector({vk::ImageMemoryBarrier()
                                     .setImage(presentImage)
                                     .setOldLayout(vk::ImageLayout::ePresentSrcKHR)
                                     .setNewLayout(vk::ImageLayout::eTransferDstOptimal)
                                     .setSrcAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                     .setDstAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                     .setSubresourceRange(vk::ImageSubresourceRange().setLevelCount(1).setLayerCount(1).setAspectMask(
                                         vk::ImageAspectFlagBits::eColor))});
    (*cmdbuf).pipelineBarrier(vk::PipelineStageFlagBits::eAllCommands, vk::PipelineStageFlagBits::eAllCommands, vk::DependencyFlags(), {},
                              {}, barriers);
  }

  auto regions =
      std::vector({vk::ImageCopy()
                       .setExtent(vk::Extent3D().setWidth(width).setHeight(height).setDepth(1))
                       .setSrcSubresource(vk::ImageSubresourceLayers().setLayerCount(1).setAspectMask(vk::ImageAspectFlagBits::eColor))
                       .setDstSubresource(vk::ImageSubresourceLayers().setLayerCount(1).setAspectMask(vk::ImageAspectFlagBits::eColor))});
  (*cmdbuf).copyImage(*texout.tex->image, vk::ImageLayout::eGeneral, presentImage, vk::ImageLayout::eTransferDstOptimal, regions);

  // TODO: Factorise this with transition
  {
    auto barriers = std::vector({vk::ImageMemoryBarrier()
                                     .setImage(presentImage)
                                     .setOldLayout(vk::ImageLayout::eTransferDstOptimal)
                                     .setNewLayout(vk::ImageLayout::ePresentSrcKHR)
                                     .setSrcAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                     .setDstAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                     .setSubresourceRange(vk::ImageSubresourceRange().setLevelCount(1).setLayerCount(1).setAspectMask(
                                         vk::ImageAspectFlagBits::eColor))});
    (*cmdbuf).pipelineBarrier(vk::PipelineStageFlagBits::eAllCommands, vk::PipelineStageFlagBits::eAllCommands, vk::DependencyFlags(), {},
                              {}, barriers);
  }

  auto endedCmdBuffer = Base::EndBufferRecording(std::move(cmdbuf));

  auto [fence, usedcmdBuffer] = Base::SubmitBuffer(dev, queue, std::move(endedCmdBuffer));
  Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));
}

} // namespace WindowingSystem