#pragma once

#include <list>
#include <memory>
#include <vector>
#include <vulkan/vulkan.hpp>
#include <vulkan/vulkan_win32.h>

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#include <Context.h>
#include <v2api.h>

namespace WindowingSystem {

vk::UniqueSurfaceKHR GetSurfaceFromGLFWWindows(vk::Instance instance, GLFWwindow *window);

struct SwapChainSupport {
  Renderer &renderer;

  vk::Extent2D extent;

  SwapChainSupport(Renderer &r, vk::SurfaceKHR surface, size_t width, size_t height);

  vk::ResultValue<uint32_t> GetPresentImage();

  void Present(uint32_t idx);

  vk::UniqueSwapchainKHR swapChain;
  std::vector<vk::Image> swapChainImages;
  std::vector<vk::UniqueImageView> swapChainImageViews;
};

inline void CopyToPresentImage(vk::Device dev, vk::CommandPool commandPool, vk::Queue queue, vk::DescriptorPool descriptorSetPool,
                               v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eB8G8R8A8Unorm> &texout, size_t width,
                               size_t height, vk::Image presentImage) {

  auto cmdbuf = v2::CreateOneShotStartedBuffer(dev, commandPool);

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

  auto endedCmdBuffer = v2::EndBufferRecording(std::move(cmdbuf));

  auto [fence, usedcmdBuffer] = v2::SubmitBuffer(dev, queue, std::move(endedCmdBuffer));
  v2::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));
}
} // namespace WindowingSystem