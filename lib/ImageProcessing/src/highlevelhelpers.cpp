#include "highlevelhelpers.h"
#include <v2utils.h>

HighLevelHelpers::HighLevelHelpers(Renderer &_renderer) : renderer(_renderer), r32fToRgba8Pipeline(*renderer.dev) {}

void HighLevelHelpers::CopyF32ToRGBA8Sync(v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &tex,
                                          v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eB8G8R8A8Unorm> &texout) {
  assert(tex.tex->width == texout.tex->width);
  assert(tex.tex->height == texout.tex->height);
  size_t width = tex.tex->width;
  size_t height = tex.tex->height;
  auto cmdbuffer = v2::CreateOneShotStartedBuffer(*renderer.dev, *renderer.commandPool);
  r32fToRgba8Pipeline({size_t((width + 15) / 16), size_t((height + 15) / 16)}, cmdbuffer, *renderer.descriptorSetPool,
                      tex, texout);
  auto endedCmdBuffer = v2::EndBufferRecording(std::move(cmdbuffer));
  auto [fence, bufferToClean] = v2::SubmitBuffer(*renderer.dev, renderer.queue, std::move(endedCmdBuffer));
  v2::WaitAndReset(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool, std::move(*fence));
}

vk::UniqueSurfaceKHR GetSurfaceFromGLFWWindows(vk::Instance instance,
                                               GLFWwindow *window) {
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

    v2::utils::GPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool)
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
                  v2::LowLevelTransition<vk::ImageLayout::ePresentSrcKHR, vk::ImageLayout::eUndefined>(*commandBuffer, img);
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
