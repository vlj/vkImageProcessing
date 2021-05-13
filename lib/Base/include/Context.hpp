#pragma once

#ifdef WIN32
#include <codeanalysis\warnings.h>
#pragma warning(push)
#pragma warning(disable : ALL_CODE_ANALYSIS_WARNINGS)
#endif

#include <vulkan/vulkan.hpp>

#ifdef WIN32
#pragma warning(pop)
#endif

extern vk::DispatchLoaderDynamic dispatcher;

vk::UniqueInstance createInstance(std::vector<const char *> extensions);
vk::UniqueDevice createLogicalDevice(vk::PhysicalDevice physDev, std::vector<const char *> extensions);
vk::UniqueHandle<vk::DebugReportCallbackEXT, vk::DispatchLoaderDynamic> createDebugCallback(vk::Instance instance);
vk::UniqueCommandPool createCommandPool(vk::Device dev, uint32_t queueFamilyIndex);

struct Renderer {
  vk::PhysicalDevice physdev;
  vk::UniqueDevice dev;
  vk::Queue queue;
  vk::UniqueCommandPool commandPool;
  vk::UniqueDescriptorPool descriptorSetPool;
  vk::PhysicalDeviceMemoryProperties memprop;

  Renderer(vk::PhysicalDevice _physDev, std::vector<const char *> extensions);
};
