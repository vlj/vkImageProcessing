#pragma once
#include <v2api.h>

#include <Context.h>


namespace v2 {

template <vk::ImageLayout Output, vk::ImageLayout Input, vk::Format Format>
DecoratedState<Output, Format> Transition(vk::CommandBuffer cmdbuf, DecoratedState<Input, Format> &&in) {
  auto barriers = std::vector({vk::ImageMemoryBarrier()
                                   .setImage(*(in.tex->image))
                                   .setOldLayout(Input)
                                   .setNewLayout(Output)
                                   .setSrcAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                   .setDstAccessMask(vk::AccessFlagBits::eShaderRead | vk::AccessFlagBits::eShaderWrite)
                                   .setSubresourceRange(vk::ImageSubresourceRange().setLevelCount(1).setLayerCount(1).setAspectMask(
                                       vk::ImageAspectFlagBits::eColor))});
  cmdbuf.pipelineBarrier(vk::PipelineStageFlagBits::eAllCommands, vk::PipelineStageFlagBits::eAllCommands, vk::DependencyFlags(), {}, {},
                         barriers);
  return {std::move(in.tex)};
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
  size_t width = tex.tex->width;
  size_t height = tex.tex->height;

  auto regions =
      std::vector({vk::BufferImageCopy()
                       .setBufferOffset(0)
                       .setBufferImageHeight(height)
                       .setBufferRowLength(width)
                       .setImageExtent(vk::Extent3D().setWidth(width).setHeight(height).setDepth(1))
                       .setImageSubresource(vk::ImageSubresourceLayers().setLayerCount(1).setAspectMask(vk::ImageAspectFlagBits::eColor))});

  (*cmdbuf).copyImageToBuffer(*(tex.tex->image), vk::ImageLayout::eTransferSrcOptimal, buffer, regions);
  return;
}

template <vk::Format Format>
DecoratedState<vk::ImageLayout::eUndefined, Format> CreateTexture(vk::Device dev, int width, int height, std::string name) {
  return {std::make_unique<Texture>(dev, width, height, Format, name)};
}

template <vk::Format Format>
inline DecoratedState<vk::ImageLayout::eTransferDstOptimal, Format>
CopyToTexture(StartedCommandBuffer &cmdbuf, DecoratedState<vk::ImageLayout::eTransferDstOptimal, Format> &texture,
              vk::Buffer transientBuffer) {

  auto regions =
      std::vector({vk::BufferImageCopy()
                       .setBufferOffset(0)
                       .setBufferImageHeight(texture.tex->height)
                       .setBufferRowLength(texture.tex->width)
                       .setImageExtent(vk::Extent3D().setWidth(texture.tex->width).setHeight(texture.tex->height).setDepth(1))
                       .setImageSubresource(vk::ImageSubresourceLayers().setLayerCount(1).setAspectMask(vk::ImageAspectFlagBits::eColor))});

  (*cmdbuf).copyBufferToImage(transientBuffer, *texture.tex->image, vk::ImageLayout::eTransferDstOptimal, regions);

  return {std::move(texture.tex)};
}

} // namespace v2