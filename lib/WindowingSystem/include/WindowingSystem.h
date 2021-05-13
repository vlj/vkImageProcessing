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
