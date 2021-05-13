#include <Context.hpp>
#include <iostream>

const char *DEBUG_LAYER = "VK_LAYER_KHRONOS_validation";
std::vector<const char *> layers{DEBUG_LAYER};

vk::DispatchLoaderDynamic dispatcher;

// Debug callback
VkBool32 debugCallback(VkDebugReportFlagsEXT flags, VkDebugReportObjectTypeEXT objType, uint64_t srcObject, size_t location,
                       int32_t msgCode, const char *pLayerPrefix, const char *pMsg, void *pUserData) {
  if (flags & VK_DEBUG_REPORT_ERROR_BIT_EXT) {
    std::cerr << "ERROR: [" << pLayerPrefix << "] Code " << msgCode << " : " << pMsg << std::endl;
    throw;
  } else if (flags & VK_DEBUG_REPORT_WARNING_BIT_EXT) {
    std::cerr << "WARNING: [" << pLayerPrefix << "] Code " << msgCode << " : " << pMsg << std::endl;
  }
  // std::cout << pMsg << std::endl;;

  return VK_FALSE;
}

vk::UniqueInstance createInstance(std::vector<const char *> glfwExtensions) {
  auto appInfo = vk::ApplicationInfo{}
                     .setPApplicationName("VulkanClear")
                     .setApplicationVersion(1)
                     .setEngineVersion(1)
                     .setApiVersion(VK_API_VERSION_1_2)
                     .setPEngineName("ClearScreenEngine");

  std::vector<const char *> extensions{VK_EXT_DEBUG_REPORT_EXTENSION_NAME};
  for (auto ext : glfwExtensions) {
    extensions.push_back(ext);
  }

  extensions.push_back(VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME);
  extensions.push_back(VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME);

  auto createInfo =
      vk::InstanceCreateInfo{}.setPApplicationInfo(&appInfo).setPEnabledExtensionNames(extensions).setPEnabledLayerNames(layers);

  return vk::createInstanceUnique(createInfo);
}

auto findQueue(vk::PhysicalDevice physDev) {
  auto tmp = physDev.getQueueFamilyProperties();
  for (auto t : tmp) {
  }
}

vk::UniqueDevice createLogicalDevice(vk::PhysicalDevice physDev, std::vector<const char *> extensions) {
  extensions.push_back(VK_EXT_DEBUG_MARKER_EXTENSION_NAME);
  extensions.push_back(VK_KHR_STORAGE_BUFFER_STORAGE_CLASS_EXTENSION_NAME);
  findQueue(physDev);
  // Greate one graphics queue and optionally a separate presentation queue
  float queuePriority = 1.0f;

  auto queueCreateInfo = vk::DeviceQueueCreateInfo{}.setPQueuePriorities(&queuePriority).setQueueFamilyIndex(0).setQueueCount(1);

  auto queues = std::vector({queueCreateInfo});
  auto deviceCreateInfo =
      vk::DeviceCreateInfo{}.setQueueCreateInfos(queues).setPEnabledLayerNames(layers).setPEnabledExtensionNames(extensions);

  return physDev.createDeviceUnique(deviceCreateInfo);
}

vk::UniqueHandle<vk::DebugReportCallbackEXT, vk::DispatchLoaderDynamic> createDebugCallback(vk::Instance instance) {
  vk::DynamicLoader dl;
  PFN_vkGetInstanceProcAddr vkGetInstanceProcAddr = dl.getProcAddress<PFN_vkGetInstanceProcAddr>("vkGetInstanceProcAddr");

  dispatcher.init(vkGetInstanceProcAddr);
  dispatcher.init(instance);
  auto createInfo =
      vk::DebugReportCallbackCreateInfoEXT{}
          .setFlags(vk::DebugReportFlagBitsEXT::eDebug | vk::DebugReportFlagBitsEXT::eError | vk::DebugReportFlagBitsEXT::eInformation |
                    vk::DebugReportFlagBitsEXT::eWarning | vk::DebugReportFlagBitsEXT::ePerformanceWarning)
          .setPfnCallback(debugCallback);
  return instance.createDebugReportCallbackEXTUnique(createInfo, nullptr, dispatcher);
}

vk::UniqueCommandPool createCommandPool(vk::Device dev, uint32_t queueFamilyIndex) {
  // Create graphics command pool
  auto poolCreateInfo =
      vk::CommandPoolCreateInfo{}.setQueueFamilyIndex(queueFamilyIndex).setFlags(vk::CommandPoolCreateFlagBits::eResetCommandBuffer);
  return dev.createCommandPoolUnique(poolCreateInfo);
}