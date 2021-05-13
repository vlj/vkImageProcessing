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

#include <cstddef>
#include <gsl/gsl>

#ifdef WIN32
#pragma warning(pop)
#endif



namespace Base {

struct WorkgroupGeometry {
  size_t x;
  size_t y;
};

struct Texture {
  Texture(vk::Device dev, size_t width, size_t height, vk::Format format, std::string);
  vk::UniqueImage image;
  vk::UniqueDeviceMemory memory;
  vk::UniqueImageView view;

  size_t width;
  size_t height;
  vk::Format format;
};


template <vk::ImageLayout layout, vk::Format format = vk::Format::eR32Sfloat> struct DecoratedState {

  DecoratedState &operator=(DecoratedState<layout, format> &&in) noexcept {
    tex = std::move(in.tex);
    return *this;
  }

  DecoratedState(DecoratedState<layout, format> &&in) = default;

  std::unique_ptr<Texture> tex;
};

template <vk::ImageLayout Output, vk::ImageLayout Input, vk::Format Format>
DecoratedState<Output, Format> Transition(vk::CommandBuffer cmdbuf, DecoratedState<Input, Format> &&in);

template <vk::ImageLayout Output, vk::ImageLayout Input>
void LowLevelTransition(vk::CommandBuffer cmdbuf, vk::Image in);

struct StartedCommandBuffer {
  vk::UniqueCommandBuffer buffer;

  vk::CommandBuffer operator*() { return *buffer; }
};

struct EndedCommandBuffer {
  vk::UniqueCommandBuffer buffer;
};

StartedCommandBuffer CreateOneShotStartedBuffer(vk::Device dev, vk::CommandPool commandPool);

EndedCommandBuffer EndBufferRecording(StartedCommandBuffer &&buffer);

std::tuple<vk::UniqueFence, vk::UniqueCommandBuffer> SubmitBuffer(vk::Device dev, vk::Queue queue, EndedCommandBuffer &&buffer);

void WaitAndReset(vk::Device dev, vk::DescriptorPool descriptorSetPool, vk::CommandPool commandPool, vk::Fence &&fence);

template<vk::Format Format>
auto CopyImageToBuffer(StartedCommandBuffer &cmdbuf, DecoratedState<vk::ImageLayout::eTransferSrcOptimal, Format> &tex, vk::Buffer buffer);

template<vk::Format Format>
DecoratedState<vk::ImageLayout::eUndefined, Format> CreateTexture(vk::Device dev, int width, int height, std::string name);

std::tuple<vk::UniqueBuffer, vk::UniqueDeviceMemory> getTransientBufferAndMemory(vk::Device dev, vk::PhysicalDeviceMemoryProperties memprop,
                                                                                 size_t copySize);

std::tuple<vk::UniqueBuffer, vk::UniqueDeviceMemory> GetMemoryBufferFrom(vk::Device dev, vk::PhysicalDeviceMemoryProperties memprop,
                                                                         gsl::span<const std::byte> img);

void CopyFromBuffer(vk::Device dev, vk::DeviceMemory memory, gsl::span<std::byte> img);


template<vk::Format Format>
inline DecoratedState<vk::ImageLayout::eTransferDstOptimal, Format>
CopyToTexture(StartedCommandBuffer &cmdbuf, DecoratedState<vk::ImageLayout::eTransferDstOptimal, Format> &texture,
              vk::Buffer transientBuffer);


struct RegionMarker {
  vk::CommandBuffer cmdBufer;

  RegionMarker(vk::CommandBuffer cmdBufer, std::string name, std::array<float, 4> color);
  ~RegionMarker();
};

template <typename T> void NameObject(vk::Device dev, T object, std::string name) {
  auto createInfo = vk::DebugMarkerObjectNameInfoEXT()
                        .setObject((uint64_t)T::CType(object))
                        .setObjectType(object.debugReportObjectType)
                        .setPObjectName(name.c_str());
  dev.debugMarkerSetObjectNameEXT(createInfo, dispatcher);
}

} // namespace Base

#include "Base_impl.hpp"