//#include "highlevelhelpers.h"
//#include <v2utils.h>
//
//#include <Context.hpp>
//
//#include <Base.hpp>
//#include <IntegralImageHelpers.h>
//#include <ShadersCollection.h>
//#include <GuidedFilterHelpers.hpp>
//
//namespace GuidedFilter {
//
//  AuxiliarySummer::ImageStorage AuxiliarySummer::BuildImageStorage(vk::Device dev, vk::CommandBuffer cmdbuf, size_t width, size_t height) {
//    auto columnReducedMatrix = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "columnReducedMatrix");
//    auto squaredColumnReducedMatrix =
//        v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "squaredColumnReducedMatrix");
//    auto columnReducedMatrixSummed =
//        v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "columnReducedMatrixSummed");
//    auto squaredColumnReducedMatrixSummed =
//        v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width / kSubgroupSize, height, "squaredColumnReducedMatrixSummed");
//    auto rowReducedMatrix = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "rowReducedMatrix");
//    auto squaredRowReducedMatrix = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "squaredRowReducedMatrix");
//    auto rowReducedMatrixSummed = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "rowReducedMatrixSummed");
//    auto rowReducedMatrixSummed2 = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "rowReducedMatrixSummed2");
//    auto squaredRowReducedMatrixSummed =
//        v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "squaredRowReducedMatrixSummed");
//    auto squaredRowReducedMatrixSummed2 =
//        v2::CreateTexture<vk::Format::eR32Sfloat>(dev, width, height / kSubgroupSize, "squaredRowReducedMatrixSummed2");
//
//    return {
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(columnReducedMatrix)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredColumnReducedMatrix)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(columnReducedMatrixSummed)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredColumnReducedMatrixSummed)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(rowReducedMatrix)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredRowReducedMatrix)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(rowReducedMatrixSummed)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(rowReducedMatrixSummed2)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredRowReducedMatrixSummed)),
//        v2::Transition<vk::ImageLayout::eGeneral>(cmdbuf, std::move(squaredRowReducedMatrixSummed2)),
//    };
//  }
//
//  AuxiliarySummer::AuxiliarySummer(Renderer &_renderer) : renderer(_renderer), helperVertical(renderer), helperHorizontal(renderer) {}
//
//  void AuxiliarySummer::draw(v2::StartedCommandBuffer &commandBuffer, ImageStorage &imageStorage) {
//    helperVertical(commandBuffer, imageStorage.rowReducedMatrix, imageStorage.rowReducedMatrixSummed);
//    helperVertical(commandBuffer, imageStorage.squaredRowReducedMatrix, imageStorage.squaredRowReducedMatrixSummed);
//
//    helperHorizontal(commandBuffer, imageStorage.columnReducedMatrix, imageStorage.columnReducedMatrixSummed);
//    helperHorizontal(commandBuffer, imageStorage.squaredColumnReducedMatrix, imageStorage.squaredColumnReducedMatrixSummed);
//
//    imageStorage.rowReducedMatrixSummed =
//        v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.rowReducedMatrixSummed));
//    imageStorage.squaredRowReducedMatrixSummed =
//        v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.squaredRowReducedMatrixSummed));
//
//    helperHorizontal(commandBuffer, imageStorage.rowReducedMatrixSummed, imageStorage.rowReducedMatrixSummed2);
//    helperHorizontal(commandBuffer, imageStorage.squaredRowReducedMatrixSummed, imageStorage.squaredRowReducedMatrixSummed2);
//
//    imageStorage.rowReducedMatrixSummed2 =
//        v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.rowReducedMatrixSummed2));
//
//    imageStorage.squaredRowReducedMatrixSummed2 =
//        v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.squaredRowReducedMatrixSummed2));
//
//    imageStorage.columnReducedMatrixSummed =
//        v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.columnReducedMatrixSummed));
//
//    imageStorage.squaredColumnReducedMatrixSummed =
//        v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(imageStorage.squaredColumnReducedMatrixSummed));
//  }
//
//
//    IntegralImageHelper::ImageStorage IntegralImageHelper::BuildImageStorage(vk::Device dev, vk::CommandPool commandPool,
//                                                     vk::PhysicalDeviceMemoryProperties memprop,
//                                          vk::Queue queue, vk::DescriptorPool descriptorSetPool, const cv::Mat &img) {
//
//      auto teximageIntegral = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "integralImage");
//      auto texsquaredImageIntegral = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "squaredIntegralImage");
//      auto meanA = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanA");
//      auto meanI = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanI");
//      auto meanSqI = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanSqI");
//      auto meanB = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "meanB");
//      auto result = v2::CreateTexture<vk::Format::eR32Sfloat>(dev, img.cols, img.rows, "result");
//      auto cmdBuf = v2::CreateOneShotStartedBuffer(dev, commandPool);
//
//      ImageStorage returnValue{
//          size_t(img.cols),
//          size_t(img.rows),
//          AuxiliarySummer::BuildImageStorage(dev, *cmdBuf, img.cols, img.rows),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(teximageIntegral)),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(texsquaredImageIntegral)),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanA)),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanI)),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanSqI)),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(meanB)),
//          v2::Transition<vk::ImageLayout::eGeneral>(*cmdBuf, std::move(result)),
//      };
//
//      auto [fence, commandBuffer] = v2::SubmitBuffer(dev, queue, v2::EndBufferRecording(std::move(cmdBuf)));
//      v2::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*fence));
//      return returnValue;
//    }
//
//
//    IntegralImageHelper::IntegralImageHelper(Renderer &_renderer) : renderer(_renderer), aux(renderer) {}
//
//    void IntegralImageHelper::draw(v2::utils::ShaderList &shaderList, ImageStorage &storedImage,
//                                 v2::DecoratedState<vk::ImageLayout::eGeneral> &tex,
//              v2::StartedCommandBuffer &commandBuffer, size_t width, size_t height) {
//      std::list<vk::UniqueBuffer> currentBufferList;
//      std::list<vk::UniqueDeviceMemory> currentMemoryList;
//
//      {
//        v2::RegionMarker marker(*commandBuffer, "Compute meanI and mean I Square", {1.f, 1.f, 0.f, 1.f});
//        shaderList.helperGuidedFilterPass1(v2::WorkgroupFromDomainGrid<Shaders::GuidedFilterPass1>(width, height), commandBuffer,
//                                           *renderer.descriptorSetPool, tex, storedImage.auxiliaryImage.columnReducedMatrix,
//                                           storedImage.auxiliaryImage.squaredColumnReducedMatrix,
//                                           storedImage.auxiliaryImage.rowReducedMatrix, storedImage.auxiliaryImage.squaredRowReducedMatrix);
//
//        storedImage.auxiliaryImage.columnReducedMatrix =
//            v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.columnReducedMatrix));
//        storedImage.auxiliaryImage.squaredColumnReducedMatrix =
//            v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.squaredColumnReducedMatrix));
//        storedImage.auxiliaryImage.rowReducedMatrix =
//            v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.rowReducedMatrix));
//        storedImage.auxiliaryImage.squaredRowReducedMatrix =
//            v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.auxiliaryImage.squaredRowReducedMatrix));
//
//        aux.draw(commandBuffer, storedImage.auxiliaryImage);
//
//        shaderList.helperIntegralPass2(
//            v2::WorkgroupFromDomainGrid<Shaders::GuidedFilterPass2>(width, height), commandBuffer, *renderer.descriptorSetPool, tex,
//            storedImage.auxiliaryImage.columnReducedMatrixSummed, storedImage.auxiliaryImage.squaredColumnReducedMatrixSummed,
//            storedImage.auxiliaryImage.rowReducedMatrixSummed2, storedImage.auxiliaryImage.squaredRowReducedMatrixSummed2,
//            storedImage.teximageIntegral, storedImage.texsquaredImageIntegral);
//      }
//
//      shaderList.helperAveraging(v2::WorkgroupFromDomainGrid<Shaders::AveragingShader>(width, height), commandBuffer,
//                                 *renderer.descriptorSetPool, storedImage.teximageIntegral, storedImage.texsquaredImageIntegral,
//                                 storedImage.meanI, storedImage.meanSqI);
//
//      {
//        v2::RegionMarker marker(*commandBuffer, "Compute meanA, meanB and output", {1.f, 0.f, 1.f, 1.f});
//
//        shaderList.helperMeanAAndBPass1(v2::WorkgroupFromDomainGrid<Shaders::MeanAAndBPass1>(width, height), commandBuffer,
//                                        *renderer.descriptorSetPool, storedImage.meanI, storedImage.meanSqI,
//                                        storedImage.auxiliaryImage.columnReducedMatrix,
//                                        storedImage.auxiliaryImage.squaredColumnReducedMatrix, storedImage.auxiliaryImage.rowReducedMatrix,
//                                        storedImage.auxiliaryImage.squaredRowReducedMatrix);
//
//        aux.draw(commandBuffer, storedImage.auxiliaryImage);
//
//        shaderList.helperMeanAAndBPass2(
//            v2::WorkgroupFromDomainGrid<Shaders::MeanAAndBPass2>(width, height), commandBuffer, *renderer.descriptorSetPool,
//            storedImage.meanI, storedImage.meanSqI, storedImage.auxiliaryImage.columnReducedMatrixSummed,
//            storedImage.auxiliaryImage.squaredColumnReducedMatrixSummed, storedImage.auxiliaryImage.rowReducedMatrixSummed2,
//            storedImage.auxiliaryImage.squaredRowReducedMatrixSummed2, storedImage.meanA, storedImage.meanB);
//
//        shaderList.helperFinalPass(v2::WorkgroupFromDomainGrid<Shaders::GuidedFilterFinalPass>(width, height), commandBuffer,
//                                   *renderer.descriptorSetPool, tex, storedImage.meanA, storedImage.meanB, storedImage.result);
//
//        storedImage.result = v2::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(storedImage.result));
//      }
//    }
//
//} // namespace GuidedFilter
