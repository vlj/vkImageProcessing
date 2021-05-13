#include <Base.hpp>

namespace Base {

StartedCommandBuffer CreateOneShotStartedBuffer(vk::Device dev, vk::CommandPool commandPool) {
  auto commandBufferCreateInfo = vk::CommandBufferAllocateInfo().setCommandBufferCount(1).setCommandPool(commandPool);
  auto cmdbufs = dev.allocateCommandBuffersUnique(commandBufferCreateInfo);
  auto &&cmdbuf = cmdbufs[0];
  auto beginCreateInfo = vk::CommandBufferBeginInfo().setFlags(vk::CommandBufferUsageFlagBits::eOneTimeSubmit);
  cmdbuf->begin(beginCreateInfo);
  return StartedCommandBuffer{std::move(cmdbuf)};
}

EndedCommandBuffer EndBufferRecording(StartedCommandBuffer &&buffer) {
  buffer.buffer->end();
  return {std::move(buffer.buffer)};
}

std::tuple<vk::UniqueFence, vk::UniqueCommandBuffer> SubmitBuffer(vk::Device dev, vk::Queue queue, EndedCommandBuffer &&buffer) {
  auto tosubmit = std::vector({*buffer.buffer});
  auto submitInfo = vk::SubmitInfo().setCommandBuffers(tosubmit);
  auto fenceinfo = vk::FenceCreateInfo();
  auto fence = dev.createFenceUnique(fenceinfo);
  queue.submit(submitInfo, *fence);
  return std::make_tuple(std::move(fence), std::move(buffer.buffer));
}

void WaitAndReset(vk::Device dev, vk::DescriptorPool descriptorSetPool, vk::CommandPool commandPool, vk::Fence &&fence) {
  auto res2 = dev.waitForFences(fence, true, -1);

  dev.resetDescriptorPool(descriptorSetPool);
  dev.resetCommandPool(commandPool, vk::CommandPoolResetFlagBits::eReleaseResources);
}


namespace internal {
inline uint32_t getMemoryTypeIndex(uint32_t typeBits, vk::MemoryPropertyFlags properties, vk::PhysicalDeviceMemoryProperties memProps) {
  // Iterate over all memory types available for the device used in this example
  for (uint32_t i = 0; i < memProps.memoryTypeCount; i++) {
    if ((typeBits & 1) == 1) {
      if ((memProps.memoryTypes[i].propertyFlags & properties) == properties) {
        return i;
      }
    }
    typeBits >>= 1;
  }

  throw "Could not find a suitable memory type!";
}

inline int GetHostVisibleMemory(uint32_t typeBits, vk::PhysicalDeviceMemoryProperties memprop) {
  return getMemoryTypeIndex(typeBits, vk::MemoryPropertyFlagBits::eHostCoherent | vk::MemoryPropertyFlagBits::eHostVisible, memprop);
}

} // namespace internal

std::tuple<vk::UniqueBuffer, vk::UniqueDeviceMemory>
getTransientBufferAndMemory(vk::Device dev, vk::PhysicalDeviceMemoryProperties memprop, size_t copySize) {
  auto bufferInfo = vk::BufferCreateInfo().setSize(copySize).setUsage(
      vk::BufferUsageFlagBits::eTransferSrc | vk::BufferUsageFlagBits::eTransferDst | vk::BufferUsageFlagBits::eUniformBuffer);
  auto transientBuffer = dev.createBufferUnique(bufferInfo);
  auto memreq = dev.getBufferMemoryRequirements(*transientBuffer);
  auto allocateInfo = vk::MemoryAllocateInfo()
                          .setMemoryTypeIndex(internal::GetHostVisibleMemory(memreq.memoryTypeBits, memprop))
                          .setAllocationSize(memreq.size);
  auto memory = dev.allocateMemoryUnique(allocateInfo);
  dev.bindBufferMemory(*transientBuffer, *memory, 0);
  return std::make_tuple(std::move(transientBuffer), std::move(memory));
}

std::tuple<vk::UniqueBuffer, vk::UniqueDeviceMemory> GetMemoryBufferFrom(vk::Device dev, vk::PhysicalDeviceMemoryProperties memprop,
                                                                        gsl::span<const std::byte> img) {
  // Transient buffer
  auto copySize = img.size_bytes();
  auto &&[transientBuffer, memory] = getTransientBufferAndMemory(dev, memprop, copySize);
  auto *ptr = static_cast<char *>(dev.mapMemory(*memory, 0, copySize));
  memcpy(ptr, img.data(), copySize);

  dev.unmapMemory(*memory);
  return std::make_tuple(std::move(transientBuffer), std::move(memory));
}

void CopyFromBuffer(vk::Device dev, vk::DeviceMemory memory, gsl::span<std::byte> img) {
  auto ptr = dev.mapMemory(memory, 0, img.size_bytes());
  memcpy(img.data(), ptr, img.size_bytes());
  dev.unmapMemory(memory);
}


RegionMarker::RegionMarker(vk::CommandBuffer _cmdBufer, std::string name, std::array<float, 4> color) : cmdBufer(_cmdBufer) {
  auto markerInfo = vk::DebugMarkerMarkerInfoEXT{}.setColor(color).setPMarkerName(name.c_str());
  cmdBufer.debugMarkerBeginEXT(markerInfo, dispatcher);
}

RegionMarker::~RegionMarker() { cmdBufer.debugMarkerEndEXT(dispatcher); }


Texture::Texture(vk::Device dev, size_t _width, size_t _height, vk::Format _format, std::string name)
    : width(_width), height(_height), format(_format) {
  auto info = vk::ImageCreateInfo()
                  .setFormat(format)
                  .setImageType(vk::ImageType::e2D)
                  .setMipLevels(1)
                  .setArrayLayers(1)
                  .setExtent(vk::Extent3D().setWidth(width).setHeight(height).setDepth(1))
                  .setUsage(vk::ImageUsageFlagBits::eStorage | vk::ImageUsageFlagBits::eTransferDst | vk::ImageUsageFlagBits::eTransferSrc)
                  .setInitialLayout(vk::ImageLayout::eUndefined)
                  .setTiling(vk::ImageTiling::eOptimal);
  image = dev.createImageUnique(info);
  NameObject(dev, *image, name);
  auto size = dev.getImageMemoryRequirements(*image);
  auto meminfo = vk::MemoryAllocateInfo().setMemoryTypeIndex(0).setAllocationSize(size.size);
  memory = dev.allocateMemoryUnique(meminfo);
  dev.bindImageMemory(*image, *memory, 0);
  auto viewinfo =
      vk::ImageViewCreateInfo()
          .setFormat(format)
          .setSubresourceRange(vk::ImageSubresourceRange().setAspectMask(vk::ImageAspectFlagBits::eColor).setLayerCount(1).setLevelCount(1))
          .setViewType(vk::ImageViewType::e2D)
          .setImage(*image);
  view = dev.createImageViewUnique(viewinfo);
}

}

Renderer::Renderer(vk::PhysicalDevice _physDev, std::vector<const char *> extensions) : physdev(_physDev) {
  memprop = physdev.getMemoryProperties();
  dev = createLogicalDevice(physdev, extensions);
  queue = dev->getQueue(0, 0);
  commandPool = createCommandPool(*dev, 0);
  auto poolSizes = std::vector({vk::DescriptorPoolSize().setType(vk::DescriptorType::eStorageImage).setDescriptorCount(100000),
                                vk::DescriptorPoolSize().setType(vk::DescriptorType::eUniformBuffer).setDescriptorCount(100000)});
  auto createInfo = vk::DescriptorPoolCreateInfo().setMaxSets(100000).setPoolSizes(poolSizes);
  descriptorSetPool = dev->createDescriptorPoolUnique(createInfo);
}
