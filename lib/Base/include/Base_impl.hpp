#pragma once
#include <Base.hpp>

#include <Context.hpp>


namespace Base {

template <vk::ImageLayout Output, vk::ImageLayout Input, vk::Format Format>
DecoratedState<Output, Format> Transition(vk::CommandBuffer cmdbuf, DecoratedState<Input, Format> &&in) {
  auto barriers = std::vector({vk::ImageMemoryBarrier()
                                   .setImage(in.image)
                                   .setOldLayout(Input)
                                   .setNewLayout(Output)
                                   .setSrcAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                   .setDstAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                   .setSubresourceRange(vk::ImageSubresourceRange().setLevelCount(1).setLayerCount(1).setAspectMask(
                                       vk::ImageAspectFlagBits::eColor))});
  cmdbuf.pipelineBarrier(vk::PipelineStageFlagBits::eAllCommands, vk::PipelineStageFlagBits::eAllCommands, vk::DependencyFlags(), {}, {},
                         barriers);
  return {in.image, in.view, in.width, in.height};
}

template <vk::ImageLayout Output, vk::ImageLayout Input>
void LowLevelTransition(vk::CommandBuffer cmdbuf, vk::Image in) {
  auto barriers = std::vector({vk::ImageMemoryBarrier()
                                   .setImage(in)
                                   .setOldLayout(Input)
                                   .setNewLayout(Output)
                                   .setSrcAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                   .setDstAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                   .setSubresourceRange(vk::ImageSubresourceRange().setLevelCount(1).setLayerCount(1).setAspectMask(
                                       vk::ImageAspectFlagBits::eColor))});
  cmdbuf.pipelineBarrier(vk::PipelineStageFlagBits::eAllCommands, vk::PipelineStageFlagBits::eAllCommands, vk::DependencyFlags(), {}, {},
                         barriers);
  return;
}


template <vk::Format Format>
auto CopyImageToBuffer(StartedCommandBuffer &cmdbuf, DecoratedState<vk::ImageLayout::eTransferSrcOptimal, Format> &tex, vk::Buffer buffer) {
  size_t width = tex.width;
  size_t height = tex.height;

  auto regions =
      std::vector({vk::BufferImageCopy()
                       .setBufferOffset(0)
                       .setBufferImageHeight(height)
                       .setBufferRowLength(width)
                       .setImageExtent(vk::Extent3D().setWidth(width).setHeight(height).setDepth(1))
                       .setImageSubresource(vk::ImageSubresourceLayers().setLayerCount(1).setAspectMask(vk::ImageAspectFlagBits::eColor))});

  (*cmdbuf).copyImageToBuffer(tex.image, vk::ImageLayout::eTransferSrcOptimal, buffer, regions);
  return;
}

template <vk::Format Format>
std::tuple<DecoratedState<vk::ImageLayout::eUndefined, Format>, std::unique_ptr<Texture>> CreateTexture(vk::Device dev, int width,
                                                                                                        int height, std::string name) {
  auto textureStorage = std::make_unique<Texture>(dev, width, height, Format, name);
  return std::make_tuple(DecoratedState<vk::ImageLayout::eUndefined, Format>{*textureStorage->image, *textureStorage->view,
                                                                             textureStorage->width, textureStorage->height},
                         std::move(textureStorage));
}

template <vk::Format Format>
inline DecoratedState<vk::ImageLayout::eTransferDstOptimal, Format>
CopyToTexture(StartedCommandBuffer &cmdbuf, DecoratedState<vk::ImageLayout::eTransferDstOptimal, Format> &texture,
              vk::Buffer transientBuffer) {

  auto regions =
      std::vector({vk::BufferImageCopy()
                       .setBufferOffset(0)
                       .setBufferImageHeight(texture.height)
                       .setBufferRowLength(texture.width)
                       .setImageExtent(vk::Extent3D().setWidth(texture.width).setHeight(texture.height).setDepth(1))
                       .setImageSubresource(vk::ImageSubresourceLayers().setLayerCount(1).setAspectMask(vk::ImageAspectFlagBits::eColor))});

  (*cmdbuf).copyBufferToImage(transientBuffer, texture.image, vk::ImageLayout::eTransferDstOptimal, regions);

  return {texture.image, texture.view, texture.width, texture.height};
}

} // namespace Base