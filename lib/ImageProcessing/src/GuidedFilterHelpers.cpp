#include "highlevelhelpers.h"
#include <v2utils.h>

#include <Context.hpp>

#include <Base.hpp>
#include <IntegralImageHelpers.h>
#include <ShadersCollection.h>
#include <GuidedFilterHelpers.hpp>

namespace GuidedFilter {

  AuxiliarySummer::ImageStorage AuxiliarySummer::BuildImageStorage(vk::Device dev, vk::CommandBuffer cmdbuf, size_t width, size_t height) {
    auto [columnReducedMatrix, columnReducedMatrixStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "columnReducedMatrix");
    auto [squaredColumnReducedMatrix, squaredColumnReducedMatrixStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "squaredColumnReducedMatrix");
    auto [columnReducedMatrixSummed, columnReducedMatrixSummedStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "columnReducedMatrixSummed");
    auto [squaredColumnReducedMatrixSummed, squaredColumnReducedMatrixSummedStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "squaredColumnReducedMatrixSummed");
    auto [rowReducedMatrix, rowReducedMatrixStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "rowReducedMatrix");
    auto [squaredRowReducedMatrix, squaredRowReducedMatrixStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "squaredRowReducedMatrix");
    auto [rowReducedMatrixSummed, rowReducedMatrixSummedStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "rowReducedMatrixSummed");
    auto [rowReducedMatrixSummed2, rowReducedMatrixSummed2Storage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "rowReducedMatrixSummed2");
    auto [squaredRowReducedMatrixSummed, squaredRowReducedMatrixSummedStorage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "squaredRowReducedMatrixSummed");
    auto [squaredRowReducedMatrixSummed2, squaredRowReducedMatrixSummed2Storage] =
        Base::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "squaredRowReducedMatrixSummed2");

    return {
        std::move(columnReducedMatrixStorage),
        std::move(squaredColumnReducedMatrixStorage),
        std::move(columnReducedMatrixSummedStorage),
        std::move(squaredColumnReducedMatrixSummedStorage),
        std::move(rowReducedMatrixStorage),
        std::move(squaredRowReducedMatrixStorage),
        std::move(rowReducedMatrixSummedStorage),
        std::move(rowReducedMatrixSummed2Storage),
        std::move(squaredRowReducedMatrixSummedStorage),
        std::move(squaredRowReducedMatrixSummed2Storage),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(columnReducedMatrix)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredColumnReducedMatrix)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(columnReducedMatrixSummed)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredColumnReducedMatrixSummed)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(rowReducedMatrix)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredRowReducedMatrix)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(rowReducedMatrixSummed)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(rowReducedMatrixSummed2)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredRowReducedMatrixSummed)),
        Base::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredRowReducedMatrixSummed2)),
    };
  }

  AuxiliarySummer::AuxiliarySummer(Renderer &_renderer) : renderer(_renderer), helperVertical(renderer), helperHorizontal(renderer) {}

  void AuxiliarySummer::draw(Base::StartedCommandBuffer &commandBuffer, ImageStorage &imageStorage) {
    helperVertical(commandBuffer, imageStorage.rowReducedMatrix, imageStorage.rowReducedMatrixSummed);
    helperVertical(commandBuffer, imageStorage.squaredRowReducedMatrix, imageStorage.squaredRowReducedMatrixSummed);

    helperHorizontal(commandBuffer, imageStorage.columnReducedMatrix, imageStorage.columnReducedMatrixSummed);
    helperHorizontal(commandBuffer, imageStorage.squaredColumnReducedMatrix, imageStorage.squaredColumnReducedMatrixSummed);

    imageStorage.rowReducedMatrixSummed =
        Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.rowReducedMatrixSummed));
    imageStorage.squaredRowReducedMatrixSummed =
        Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.squaredRowReducedMatrixSummed));

    helperHorizontal(commandBuffer, imageStorage.rowReducedMatrixSummed, imageStorage.rowReducedMatrixSummed2);
    helperHorizontal(commandBuffer, imageStorage.squaredRowReducedMatrixSummed, imageStorage.squaredRowReducedMatrixSummed2);

    imageStorage.rowReducedMatrixSummed2 =
        Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.rowReducedMatrixSummed2));

    imageStorage.squaredRowReducedMatrixSummed2 =
        Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.squaredRowReducedMatrixSummed2));

    imageStorage.columnReducedMatrixSummed =
        Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.columnReducedMatrixSummed));

    imageStorage.squaredColumnReducedMatrixSummed =
        Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.squaredColumnReducedMatrixSummed));
  }


    IntegralImageHelper::ImageStorage IntegralImageHelper::BuildImageStorage(vk::Device dev, vk::CommandPool commandPool,
                                                     vk::PhysicalDeviceMemoryProperties memprop,
                                          vk::Queue queue, vk::DescriptorPool descriptorSetPool, const cv::Mat &img) {

      auto [teximageIntegral, teximageIntegralStorage] =
          Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "integralImage");
      auto [texsquaredImageIntegral, texsquaredImageIntegralStorage] =
          Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "squaredIntegralImage");
      auto [meanA, meanAStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanA");
      auto [meanI, meanIStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanI");
      auto [meanSqI, meanSqIStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanSqI");
      auto [meanB, meanBStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanB");
      auto [result, resultStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "result");
      auto cmdBuf = Base::CreateOneShotStartedBuffer(dev, commandPool);

      ImageStorage returnValue{
          size_t(img.cols),
          size_t(img.rows),
          AuxiliarySummer::BuildImageStorage(dev, *cmdBuf, img.cols, img.rows),
          std::move(teximageIntegralStorage),
          std::move(texsquaredImageIntegralStorage),
          std::move(meanAStorage),
          std::move(meanIStorage),
          std::move(meanSqIStorage),
          std::move(meanBStorage),
          std::move(resultStorage),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(teximageIntegral)),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(texsquaredImageIntegral)),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanA)),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanI)),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanSqI)),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanB)),
          Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(result)),
      };

      auto [fence, commandBuffer] = Base::SubmitBuffer(dev, queue, Base::EndBufferRecording(std::move(cmdBuf)));
      Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));
      return returnValue;
    }


    IntegralImageHelper::IntegralImageHelper(Renderer &_renderer) : renderer(_renderer), aux(renderer) {}

    void IntegralImageHelper::draw(v2::utils::ShaderList &shaderList, ImageStorage &storedImage,
                                   Base::DecoratedState<vk::ImageLayout::eGeneral> &tex, Base::StartedCommandBuffer &commandBuffer,
                                   size_t width, size_t height) {
      std::list<vk::UniqueBuffer> currentBufferList;
      std::list<vk::UniqueDeviceMemory> currentMemoryList;

      {
        Base::RegionMarker marker(*commandBuffer, "Compute meanI and mean I Square", {1.f, 1.f, 0.f, 1.f});
        shaderList.helperGuidedFilterPass1(v2::WorkgroupFromDomainGrid<Shaders::GuidedFilterPass1>(width, height), commandBuffer,
                                           *renderer.descriptorSetPool, tex, storedImage.auxiliaryImage.columnReducedMatrix,
                                           storedImage.auxiliaryImage.squaredColumnReducedMatrix,
                                           storedImage.auxiliaryImage.rowReducedMatrix, storedImage.auxiliaryImage.squaredRowReducedMatrix);

        storedImage.auxiliaryImage.columnReducedMatrix =
            Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.columnReducedMatrix));
        storedImage.auxiliaryImage.squaredColumnReducedMatrix =
            Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.squaredColumnReducedMatrix));
        storedImage.auxiliaryImage.rowReducedMatrix =
            Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.rowReducedMatrix));
        storedImage.auxiliaryImage.squaredRowReducedMatrix =
            Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.squaredRowReducedMatrix));

        aux.draw(commandBuffer, storedImage.auxiliaryImage);

        shaderList.helperIntegralPass2(
            v2::WorkgroupFromDomainGrid<Shaders::GuidedFilterPass2>(width, height), commandBuffer, *renderer.descriptorSetPool, tex,
            storedImage.auxiliaryImage.columnReducedMatrixSummed, storedImage.auxiliaryImage.squaredColumnReducedMatrixSummed,
            storedImage.auxiliaryImage.rowReducedMatrixSummed2, storedImage.auxiliaryImage.squaredRowReducedMatrixSummed2,
            storedImage.teximageIntegral, storedImage.texsquaredImageIntegral);
      }

      shaderList.helperAveraging(v2::WorkgroupFromDomainGrid<Shaders::AveragingShader>(width, height), commandBuffer,
                                 *renderer.descriptorSetPool, storedImage.teximageIntegral, storedImage.texsquaredImageIntegral,
                                 storedImage.meanI, storedImage.meanSqI);

      {
        Base::RegionMarker marker(*commandBuffer, "Compute meanA, meanB and output", {1.f, 0.f, 1.f, 1.f});

        shaderList.helperMeanAAndBPass1(v2::WorkgroupFromDomainGrid<Shaders::MeanAAndBPass1>(width, height), commandBuffer,
                                        *renderer.descriptorSetPool, storedImage.meanI, storedImage.meanSqI,
                                        storedImage.auxiliaryImage.columnReducedMatrix,
                                        storedImage.auxiliaryImage.squaredColumnReducedMatrix, storedImage.auxiliaryImage.rowReducedMatrix,
                                        storedImage.auxiliaryImage.squaredRowReducedMatrix);

        aux.draw(commandBuffer, storedImage.auxiliaryImage);

        shaderList.helperMeanAAndBPass2(
            v2::WorkgroupFromDomainGrid<Shaders::MeanAAndBPass2>(width, height), commandBuffer, *renderer.descriptorSetPool,
            storedImage.meanI, storedImage.meanSqI, storedImage.auxiliaryImage.columnReducedMatrixSummed,
            storedImage.auxiliaryImage.squaredColumnReducedMatrixSummed, storedImage.auxiliaryImage.rowReducedMatrixSummed2,
            storedImage.auxiliaryImage.squaredRowReducedMatrixSummed2, storedImage.meanA, storedImage.meanB);

        shaderList.helperFinalPass(v2::WorkgroupFromDomainGrid<Shaders::GuidedFilterFinalPass>(width, height), commandBuffer,
                                   *renderer.descriptorSetPool, tex, storedImage.meanA, storedImage.meanB, storedImage.result);

        storedImage.result = Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.result));
      }
    }

} // namespace GuidedFilter
