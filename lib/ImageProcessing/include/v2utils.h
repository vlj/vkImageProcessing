#pragma once


#ifdef WIN32
#include <codeanalysis\warnings.h>
#pragma warning(push)
#pragma warning(disable : ALL_CODE_ANALYSIS_WARNINGS)
#endif

#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>

#ifdef WIN32
#pragma warning(pop)
#endif

#include <Base.hpp>
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
  auto cmdbuffer = Base::CreateOneShotStartedBuffer(dev, commandPool);

  auto [texture, storage] = Base::CreateTexture<Format>(dev, img.cols, img.rows, name);
  auto textureDest = Base::Transition<vk::ImageLayout::eTransferDstOptimal>(*cmdbuffer, std::move(texture));
  auto tmp = gsl::span<const std::byte>(reinterpret_cast<const std::byte*>(img.ptr()), img.cols * img.rows * 4);
  auto [buffer, buffermem] = Base::GetMemoryBufferFrom(dev, memprop, tmp);
  auto updatedTexture = Base::CopyToTexture(cmdbuffer, textureDest, *buffer);
  auto readableTexture = Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(updatedTexture));

  auto endedCmdBuffer = Base::EndBufferRecording(std::move(cmdbuffer));

  auto [fence, bufferToClean] = Base::SubmitBuffer(dev, queue, std::move(endedCmdBuffer));
  Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));

  return std::make_tuple(std::move(readableTexture), std::move(storage));
}

template <vk::Format Format>
auto TextureToCVMat(vk::Device dev, vk::CommandPool commandPool, vk::PhysicalDeviceMemoryProperties memprop, vk::Queue queue,
                    vk::DescriptorPool descriptorSetPool, Base::DecoratedState<vk::ImageLayout::eGeneral, Format> &&tex) {

  cv::Mat exportedimg(tex.height, tex.width, internal::GetCVTypeFromFormat(Format));

  auto cmdbuffer = Base::CreateOneShotStartedBuffer(dev, commandPool);
  auto copySize = tex.width * tex.height * 4;
  auto &&[outputBuffer, memory] = Base::getTransientBufferAndMemory(dev, memprop, copySize);

  auto texAsSrc = Base::Transition<vk::ImageLayout::eTransferSrcOptimal>(*cmdbuffer, std::move(tex));
  Base::CopyImageToBuffer(cmdbuffer, texAsSrc, *outputBuffer);

  auto endedCmdBuffer = Base::EndBufferRecording(std::move(cmdbuffer));

  auto [fence, bufferToClean] = Base::SubmitBuffer(dev, queue, std::move(endedCmdBuffer));
  Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));
  auto exportedImgPtr = gsl::span<std::byte>(reinterpret_cast<std::byte*>(exportedimg.ptr()), 4 * exportedimg.rows * exportedimg.cols);
  Base::CopyFromBuffer(dev, *memory, exportedImgPtr);
  
  return exportedimg;
}

struct ShaderList {

  
  Shaders::copy_h_spv r32fToRgba8Pipeline;

  Shaders::guidedFilterPass1_h_spv helperGuidedFilterPass1;
  Shaders::guidedFilterPass2_h_spv helperIntegralPass2;
  Shaders::meanAandBPass1_h_spv helperMeanAAndBPass1;
  Shaders::meanAandBPass2_h_spv helperMeanAAndBPass2;
  Shaders::guidedFilterFinal_h_spv helperFinalPass;
  Shaders::averagingISqI_h_spv helperAveraging;


  static ShaderList Build(vk::Device dev) { return ShaderList(dev);
  }

  private:
  ShaderList(vk::Device dev) : r32fToRgba8Pipeline(dev), helperGuidedFilterPass1(dev), helperIntegralPass2(dev),
          helperMeanAAndBPass1(dev), helperMeanAAndBPass2(dev), helperFinalPass(dev),
          helperAveraging(dev) {}
};

} // namespace utils

} // namespace v2