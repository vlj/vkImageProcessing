#pragma once


#ifdef WIN32
#include <codeanalysis\warnings.h>
#pragma warning(push)
#pragma warning(disable : ALL_CODE_ANALYSIS_WARNINGS)
#endif

#include <list>
#include <memory>
#include <vector>
#include <vulkan/vulkan.hpp>
#include <vulkan/vulkan_win32.h>

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#ifdef WIN32
#pragma warning(pop)
#endif


#include <Context.hpp>
#include <Base.hpp>

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

void CopyToPresentImage(vk::Device dev, vk::CommandPool commandPool, vk::Queue queue, vk::DescriptorPool descriptorSetPool,
                        Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eB8G8R8A8Unorm> &texout, size_t width, size_t height,
                        vk::Image presentImage);
} // namespace WindowingSystem