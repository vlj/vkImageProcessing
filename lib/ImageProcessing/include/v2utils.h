#pragma once

#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>

#include <v2api.h>
#include <ShadersCollection.h>

namespace v2 {

namespace utils {

namespace internal {
inline auto GetFormatFromCVType(int type) {
  switch (type) {
  case CV_32FC1:
    return vk::Format::eR32Sfloat;
  case CV_8UC4:
    return vk::Format::eR8G8B8A8Unorm;
  }
}

inline auto GetCVTypeFromFormat(vk::Format format) {
  switch (format) {
  case vk::Format::eR32Sfloat:
    return CV_32FC1;
  case vk::Format::eR8G8B8A8Unorm:
  case vk::Format::eB8G8R8A8Unorm:
    return CV_8UC4;
  }
}

} // namespace internal

template <vk::Format Format>
auto CreateTextureSync(vk::Device dev, vk::CommandPool commandPool, vk::PhysicalDeviceMemoryProperties memprop, vk::Queue queue,
                       vk::DescriptorPool descriptorSetPool,
                       const cv::Mat &img, std::string name) {
  assert(Format == internal::GetFormatFromCVType(img.type()));
  auto cmdbuffer = CreateOneShotStartedBuffer(dev, commandPool);

  auto texture = v2::CreateTexture<Format>(dev, img.cols, img.rows, name);
  auto textureDest = v2::Transition<vk::ImageLayout::eTransferDstOptimal>(*cmdbuffer, std::move(texture));
  auto tmp = gsl::span<const std::byte>(reinterpret_cast<const std::byte*>(img.ptr()), img.cols * img.rows * 4);
  auto [buffer, buffermem] = v2::GetMemoryBufferFrom(dev, memprop, tmp);
  auto updatedTexture = v2::CopyToTexture(cmdbuffer, textureDest, *buffer);
  auto readableTexture = v2::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(updatedTexture));

  auto endedCmdBuffer = v2::EndBufferRecording(std::move(cmdbuffer));

  auto [fence, bufferToClean] = v2::SubmitBuffer(dev, queue, std::move(endedCmdBuffer));
  v2::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));

  return std::move(readableTexture);
}

template <vk::Format Format>
auto TextureToCVMat(vk::Device dev, vk::CommandPool commandPool, vk::PhysicalDeviceMemoryProperties memprop, vk::Queue queue,
                    vk::DescriptorPool descriptorSetPool, DecoratedState<vk::ImageLayout::eGeneral, Format>&& tex) {

  cv::Mat exportedimg(tex.tex->height, tex.tex->width, internal::GetCVTypeFromFormat(tex.tex->format));

  auto cmdbuffer = CreateOneShotStartedBuffer(dev, commandPool);
  auto copySize = tex.tex->width * tex.tex->height * 4;
  auto &&[outputBuffer, memory] = v2::getTransientBufferAndMemory(dev, memprop, copySize);

  auto texAsSrc = v2::Transition<vk::ImageLayout::eTransferSrcOptimal>(*cmdbuffer, std::move(tex));
  v2::CopyImageToBuffer(cmdbuffer, texAsSrc, *outputBuffer);

  auto endedCmdBuffer = v2::EndBufferRecording(std::move(cmdbuffer));

  auto [fence, bufferToClean] = v2::SubmitBuffer(dev, queue, std::move(endedCmdBuffer));
  v2::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));
  auto exportedImgPtr = gsl::span<std::byte>(reinterpret_cast<std::byte*>(exportedimg.ptr()), 4 * exportedimg.rows * exportedimg.cols);
  v2::CopyFromBuffer(dev, *memory, exportedImgPtr);
  
  return exportedimg;
}


inline void CopyToPresentImage(vk::Device dev, vk::CommandPool commandPool, vk::Queue queue,
                        vk::DescriptorPool descriptorSetPool,
  v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eB8G8R8A8Unorm>& texout,
  size_t width, size_t height, vk::Image presentImage) {

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
    (*cmdbuf).pipelineBarrier(vk::PipelineStageFlagBits::eAllCommands, vk::PipelineStageFlagBits::eAllCommands, vk::DependencyFlags(), {}, {},
                           barriers);
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

struct ShaderList {

  
  v2::copy_h_spv r32fToRgba8Pipeline;

  v2::guidedFilterPass1_h_spv helperGuidedFilterPass1;
  v2::guidedFilterPass2_h_spv helperIntegralPass2;
  v2::meanAandBPass1_h_spv helperMeanAAndBPass1;
  v2::meanAandBPass2_h_spv helperMeanAAndBPass2;
  v2::guidedFilterFinal_h_spv helperFinalPass;
  v2::averagingISqI_h_spv helperAveraging;


  static ShaderList Build(vk::Device dev) { return ShaderList(dev);
  }

  private:
  ShaderList(vk::Device dev) : r32fToRgba8Pipeline(dev), helperGuidedFilterPass1(dev), helperIntegralPass2(dev),
          helperMeanAAndBPass1(dev), helperMeanAAndBPass2(dev), helperFinalPass(dev),
          helperAveraging(dev) {}
};

template<typename TextureStatesTuple>
struct GPUAsyncCommand {
  vk::Device dev;
  vk::DescriptorPool descriptorSetPool;
  vk::CommandPool commandPool;
  std::list<vk::UniqueFence> fences;
  std::list<vk::UniqueCommandBuffer> commandBuffers;
  TextureStatesTuple textureStates;

  GPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple&& tuple)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), commandPool(_commandPool), textureStates(std::move(tuple)) {}

  template <typename FuncType> auto then(vk::Queue queue, FuncType f) {
    auto startedCommandBuffer = v2::CreateOneShotStartedBuffer(dev, commandPool);
    auto newTextureStates = f(startedCommandBuffer, std::move(textureStates));
    auto endedCommandBuffer = v2::EndBufferRecording(std::move(startedCommandBuffer));
    auto [fence, cmdbuf] = v2::SubmitBuffer(dev, queue, std::move(endedCommandBuffer));

    fences.push_back(std::move(fence));
    commandBuffers.push_back(std::move(cmdbuf));

    auto args = std::tuple_cat(std::make_tuple(dev, descriptorSetPool, commandPool), std::move(newTextureStates));
    auto result = std::apply([](auto &&...a) { return GPUAsyncUnit(std::move(a)...); }, std::move(args));
    result.fences = std::move(fences);
    result.commandBuffers = std::move(commandBuffers);
    return std::move(result);
  }

  auto Sync() {
    for (auto &f : fences) {
      v2::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*f));
    }
    fences.clear();
    commandBuffers.clear();
    return std::move(textureStates);
  }
};

template <typename... TextureStates>
GPUAsyncCommand<std::tuple<TextureStates...>> GPUAsyncUnit(vk::Device _dev, vk::DescriptorPool _descriptorSetPool,
                                                           vk::CommandPool _commandPool, TextureStates&&... t) {
  return {_dev, _descriptorSetPool, _commandPool, std::make_tuple(std::move(t)...)};
}

} // namespace utils

} // namespace v2