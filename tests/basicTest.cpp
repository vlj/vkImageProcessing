#define CATCH_CONFIG_MAIN // This tells Catch to provide a main() - only do this in one cpp file
#include "catch.hpp"

#include <list>
#include <memory>
#include <vector>
#include <vulkan/vulkan.hpp>
#include <vulkan/vulkan_win32.h>

#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgproc.hpp>

#include "highlevelhelpers.h"
#include <Context.hpp>
#include <ShadersCollection.h>
#include <IntegralImageHelpers.h>
#include <GPUCommandAsync.hpp>
#include <v2utils.h>

namespace {
  bool IsSame(const cv::Mat& a, const cv::Mat& b) {
    auto diff = cv::sum(a != b);
    return (diff == cv::Scalar(0., 0., 0., 0.));
}

  float GetDiff(const cv::Mat &a, const cv::Mat &b, cv::Range subSetX, cv::Range subSetY) {
    return cv::norm(a(subSetX, subSetY) - b(subSetX, subSetY));
  }

  void ViewMat(const cv::Mat& a) {
    cv::imshow("viewImage", a);
    cv::waitKey();
  }
}

TEST_CASE("Object creations and destruction is ok", "[API]") {
  auto instance = createInstance({});
  REQUIRE(instance);
  auto callback = createDebugCallback(*instance);
  REQUIRE(callback);

  REQUIRE(!instance->enumeratePhysicalDevices().empty());

  SECTION("Can create renderer") {
    Renderer renderer(instance->enumeratePhysicalDevices()[0], {});
  }
}

TEST_CASE("Misc test", "[API]") {
  auto instance = createInstance({});
  REQUIRE(instance);
  auto callback = createDebugCallback(*instance);
  REQUIRE(callback);

  REQUIRE(!instance->enumeratePhysicalDevices().empty());

  Renderer renderer(instance->enumeratePhysicalDevices()[0], {});

  SECTION("Texture transfer forth and back 8UC4") {
    cv::Mat img(cv::Size(1024, 1024), CV_8UC4, cv::Scalar(1));
    cv::randu(img, cv::Scalar(0), cv::Scalar(255));
    
    auto [readableTexture, storage] = v2::utils::CreateTextureSync<vk::Format::eR8G8B8A8Unorm>(
        *renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue, *renderer.descriptorSetPool, img, "r32fOnes");
    auto exportedimg = v2::utils::TextureToCVMat(*renderer.dev, *renderer.commandPoolForCopy, renderer.memprop, renderer.copyqueue,
                                                 *renderer.descriptorSetPool, std::move(readableTexture));
    REQUIRE(IsSame(img, exportedimg));
  }

  SECTION("R32F to RGBA8") {
    auto r32fToRgba8Pipeline = Shaders::copy_h_spv(*renderer.dev);

    cv::Mat img(cv::Size(1536, 1024), CV_32FC1, cv::Scalar(1.f));

    auto [readableTexture, storage] = v2::utils::CreateTextureSync<vk::Format::eR32Sfloat>(
        *renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue, *renderer.descriptorSetPool, img, "r32fOnes");

    auto [textureOut, storage0] = Base::CreateTexture<vk::Format::eB8G8R8A8Unorm>(*renderer.dev, img.cols, img.rows, "convertedOutput");

    auto [writeableTextureOut] =
        Experimental::MakeGPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool,
                                       std::make_tuple(std::move(readableTexture), std::move(textureOut)))
            .then([&](auto &&state) {

              return Experimental::MakeGPUAsync(
                  *renderer.dev, *renderer.descriptorSetPool, {renderer.queue, *renderer.commandPool},
                  [&, state = std::move(state)](auto &commandBuffer) mutable {
                    auto &&[readableTexture, textureOut] = std::move(state);
                    auto writeableTextureOut = Base::Transition<vk::ImageLayout::eGeneral>(*commandBuffer, std::move(textureOut));
                    r32fToRgba8Pipeline({size_t((img.cols + 15) / 16), size_t((img.rows + 15) / 16)}, commandBuffer,
                                        *renderer.descriptorSetPool, readableTexture, writeableTextureOut);
                    return std::make_tuple(std::move(writeableTextureOut));
                  });
            })
            .Sync();

    auto exportedimg = v2::utils::TextureToCVMat(*renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue,
                                                 *renderer.descriptorSetPool, std::move(writeableTextureOut));

    auto ref = cv::Mat(cv::Size(1536, 1024), CV_8UC4, cv::Scalar(255, 255, 255, 255));
    REQUIRE(IsSame(ref, exportedimg));
  }

  SECTION("Can load UBO and samplers") {
    auto [textureIn, storage0] = Base::CreateTexture<vk::Format::eB8G8R8A8Unorm>(*renderer.dev, 1024, 1024, "someInput");
    auto [textureOut, storage1] = Base::CreateTexture<vk::Format::eB8G8R8A8Unorm>(*renderer.dev, 1024, 1024, "someOutput");

    auto testShader = Shaders::
  }

  SECTION("Horizontal prefix sum") {
    cv::Mat img(cv::Size(128, 64), CV_32FC1, cv::Scalar(1));

    IntegralImageHelper::HorizontalSummer horizontalSummer(renderer);

    auto [tex, texStorage] = v2::utils::CreateTextureSync<vk::Format::eR32Sfloat>(*renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue,
                                                                    *renderer.descriptorSetPool, img, "inputImage");
    auto [texOut, texOutStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(*renderer.dev, 128, 64, "horizontalPrefixSumResult");

    auto [texOutInGeneralForm] = Base::MakeGPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool,
                                                         std::make_tuple(std::move(tex), std::move(texOut)))
            .then(
                  [&](auto &&cmdBuffer, auto &&textures) {
                    auto [tex, texOut] = std::move(textures);
                    auto texOutInGeneralForm = Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuffer, std::move(texOut));
                    horizontalSummer(cmdBuffer, tex, texOutInGeneralForm);
                    return Base::MakeGPUAsync(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool, renderer.queue,
                                         std::move(cmdBuffer), std::make_tuple(std::move(texOutInGeneralForm)));
                  })
            .Sync();
    auto exportedimg = v2::utils::TextureToCVMat(*renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue,
                                                 *renderer.descriptorSetPool, std::move(texOutInGeneralForm));

    REQUIRE(exportedimg.at<float>(63, 127) == 128.f);
    cv::Mat dx;
    cv::Sobel(exportedimg, dx, CV_32FC1, 1, 0);
    REQUIRE(cv::norm(img - dx / 8) < 12);
  }

  SECTION("Vertical prefix sum") {
    cv::Mat img(cv::Size(128, 64), CV_32FC1, cv::Scalar(1));

    IntegralImageHelper::VerticalSummer verticalSummer(renderer);

    auto [tex, storage0] = v2::utils::CreateTextureSync<vk::Format::eR32Sfloat>(
        *renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue, *renderer.descriptorSetPool, img, "inputImage");
    auto [texOut, storage] = Base::CreateTexture<vk::Format::eR32Sfloat>(*renderer.dev, 128, 64, "verticalPrefixSumResult");
    auto [texOutInGeneralForm] =
        Base::MakeGPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool,
                                std::make_tuple(std::move(tex), std::move(texOut)))
            .then([&](auto &&cmdbuffer, auto &&textures) {
              auto [tex, texOut] = std::move(textures);
              auto texOutInGeneralForm = Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(texOut));
              verticalSummer(cmdbuffer, tex, texOutInGeneralForm);
              return Base::MakeGPUAsync(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool, renderer.queue,
                                         std::move(cmdbuffer), std::make_tuple(std::move(texOutInGeneralForm)));
            })
            .Sync();

    auto exportedimg = v2::utils::TextureToCVMat(*renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue,
                                                 *renderer.descriptorSetPool, std::move(texOutInGeneralForm));

    REQUIRE(exportedimg.at<float>(63, 127) == 64.f);
    cv::Mat dy;
    cv::Sobel(exportedimg, dy, CV_32FC1, 0, 1);
    REQUIRE(cv::norm(img - dy / 8) < 17);
  }

  SECTION("Averaging") {
    cv::Mat img(cv::Size(128, 64), CV_32FC1, cv::Scalar(1));

    IntegralImageHelper::VerticalSummer verticalSummer(renderer);
    IntegralImageHelper::HorizontalSummer horizontalSummer(renderer);
    Shaders::test_average_h_spv averagerFromIntegralImage(*renderer.dev);
    
    auto [tex, storage] = v2::utils::CreateTextureSync<vk::Format::eR32Sfloat>(*renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue,
                                                                    *renderer.descriptorSetPool, img, "inputImage");
    auto [horizontallySummed, storage1] = Base::CreateTexture<vk::Format::eR32Sfloat>(*renderer.dev, 128, 64, "horizontallySummed");
    auto [allSummed, storage2] = Base::CreateTexture<vk::Format::eR32Sfloat>(*renderer.dev, 128, 64, "AllSummed");
    auto [averaged, storage3] = Base::CreateTexture<vk::Format::eR32Sfloat>(*renderer.dev, 128, 64, "averaged");

    auto [averagedInGeneralForm] =
        Base::MakeGPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool,
                                std::make_tuple(std::move(tex), std::move(horizontallySummed), std::move(allSummed), std::move(averaged)))
            .then([&](auto &&cmdbuffer, auto &&textures) {
              auto [tex, horizontallySummed, allSummed, averaged] = std::move(textures);
              auto horizontallySummedGeneralForm = Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(horizontallySummed));
              auto allSummedInGeneralForm = Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(allSummed));
              auto averagedInGeneralForm = Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(averaged));

              horizontalSummer(cmdbuffer, tex, horizontallySummedGeneralForm);
              horizontallySummedGeneralForm =
                  Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(horizontallySummedGeneralForm));
              verticalSummer(cmdbuffer, horizontallySummedGeneralForm, allSummedInGeneralForm);
              allSummedInGeneralForm = Base::Transition<vk::ImageLayout::eGeneral>(*cmdbuffer, std::move(allSummedInGeneralForm));
              averagerFromIntegralImage(v2::WorkgroupFromDomainGrid<Shaders::TestAverage>(128, 64), cmdbuffer, *renderer.descriptorSetPool,
                                        allSummedInGeneralForm, averagedInGeneralForm);
              return Base::MakeGPUAsync(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool, renderer.queue,
                                         std::move(cmdbuffer), std::make_tuple(std::move(averagedInGeneralForm)));
            })
            .Sync();
    auto exportedimg = v2::utils::TextureToCVMat(*renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue,
                                                 *renderer.descriptorSetPool, std::move(averagedInGeneralForm));

    REQUIRE(GetDiff(exportedimg / 4, img, cv::Range(16, 47), cv::Range(16, 111)) < 1);
  }

  //SECTION("Full Image integral") {

  //  auto helperIntegralImage = Helper<Shaders::IntegralImage>(renderer, drawer, "IntegralImage");
  //  auto helperAverager = Helper<Shaders::TestAverage>(renderer, drawer, "Average");

  //  cv::Mat img(cv::Size(1024, 1024), CV_32FC1, cv::Scalar(1));
  //  // cv::randu(img, cv::Scalar(0), cv::Scalar(1));

  //  auto _img = cv::imread(R"(C:\Users\vljno\OneDrive\Images\4cz7ewldo0k3y6xpcysx - Copie.jpg)");
  //  cv::cvtColor(_img, img, cv::COLOR_RGB2GRAY);
  //  img.convertTo(img, CV_32FC1);

  //  auto tex = renderer.CreateTexture(img / 1024, "inputImage");
  //  auto teximageIntegral = renderer.CreateTextureOut(img.cols, img.rows, vk::Format::eR32Sfloat, "integralImage");
  //  auto texout = renderer.CreateTextureOut(img.cols, img.rows, vk::Format::eR32Sfloat, "averagedImage");
  //  helperIntegralImage.LaunchWavefront(tex, teximageIntegral, img.cols, img.rows);
  //  helperAverager.LaunchGrid(std::vector<Texture *>{&teximageIntegral, &texout}, img.cols, img.rows);

  //  auto exportedimg = drawer.Export(texout, img.cols, img.rows, CV_32FC1);

  //  REQUIRE(cv::norm(img / 1024 - exportedimg / 128) < 200);
  //  auto diff = cv::sum(img != exportedimg);
  //}

  
  SECTION("Horizontal summation") {

    cv::Mat img(cv::Size(128, 128), CV_32FC1, cv::Scalar(1.));

    //auto texIN = renderer.CreateTexture(img, "texIN");
    //auto texInter = renderer.CreateTextureOut(img.rows, img.cols, vk::Format::eR32Sfloat, "texOUT");

/*    auto horizontalSummer = IntegralImageHelper::HorizontalSummer(renderer, drawer);
    auto texOUT = horizontalSummer(texIN, std::move(texInter));
    renderer.Reclaim();

    auto exportedimg = drawer.Export(texOUT, img.rows, img.cols, CV_32FC1);

    cv::imshow("horizontal summation", exportedimg / 128);
    cv::waitKey();*/

    //REQUIRE(cv::norm(img / 1024 - exportedimg / 128) < 200);
    //auto diff = cv::sum(img != exportedimg);
  }


  //SECTION("Grid Image Integral - row and col matrix") {

  //  cv::Mat _img(cv::Size(500, 281), CV_32FC1, cv::Scalar(0.));
  //  _img.col(0).setTo(cv::Scalar(1.f));
  //  cv::Mat img;
  //  cv::integral(_img, img);
  //  img.convertTo(img, CV_32FC1);

  //  auto tex = renderer.CreateTexture(img / 8192, "input");
  //  auto columnReducedMatrix = renderer.CreateTextureOut(img.cols / 32, img.rows, vk::Format::eR32Sfloat, "columnReducedMatrix");
  //  auto columnReducedMatrixSummed =
  //      renderer.CreateTextureOut(img.cols / 32, img.rows, vk::Format::eR32Sfloat, "columnReducedMatrixSummed");
  //  auto columnReducedMatrixSummed2 =
  //      renderer.CreateTextureOut(img.cols / 32, img.rows, vk::Format::eR32Sfloat, "columnReducedMatrixSummed2");
  //  auto rowReducedMatrix = renderer.CreateTextureOut(img.cols, img.rows / 32, vk::Format::eR32Sfloat, "rowReducedMatrix");
  //  auto rowReducedMatrixSummed = renderer.CreateTextureOut(img.cols, img.rows / 32, vk::Format::eR32Sfloat, "rowReducedMatrixSummed");
  //  auto rowReducedMatrixSummed2 = renderer.CreateTextureOut(img.cols, img.rows / 32, vk::Format::eR32Sfloat, "rowReducedMatrixSummed2");
  //  auto teximageIntegral = renderer.CreateTextureOut(img.cols, img.rows, vk::Format::eR32Sfloat, "integralImage");
  //  auto texhorizontal = renderer.CreateTextureOut(img.cols, img.rows, vk::Format::eR32Sfloat, "integralImage");
  //  auto texout = renderer.CreateTextureOut(img.cols, img.rows, vk::Format::eR32Sfloat, "averagedImage");
  //  auto texrgba8 = renderer.CreateTextureOut(img.cols, img.rows, vk::Format::eB8G8R8A8Unorm, "rgba8output");

  //  auto helperIntegral = Helper<Shaders::NonWavefrontTest>(renderer, drawer, "IntegralImagePass1");
  //  auto helperHorizontal = Helper<Shaders::VerticalSumation>(renderer, drawer, "RowSum");
  //  auto helperVertical = Helper<Shaders::HorizontalSumation>(renderer, drawer, "ColSum");
  //  auto helperIntegralPass2 = Helper<Shaders::IntegralImagePass2>(renderer, drawer, "IntegralImagePass2");
  //  auto helperAverager = Helper<Shaders::TestAverage>(renderer, drawer, "Averager");

  //  for (int i = 0; i < 4; ++i) {

  //    helperIntegral.LaunchGrid(std::vector<Texture *>{&tex, &columnReducedMatrix, &rowReducedMatrix}, img.cols, img.rows);
  //    helperHorizontal.LaunchScanTtB(std::vector<Texture *>{&rowReducedMatrix, &rowReducedMatrixSummed}, img.cols, img.rows);
  //    helperVertical.LaunchScanTtB(std::vector<Texture *>{&rowReducedMatrixSummed, &rowReducedMatrixSummed2}, img.cols, img.rows);
  //    helperVertical.LaunchScanLtR(std::vector<Texture *>{&columnReducedMatrix, &columnReducedMatrixSummed}, img.cols, img.rows);
  //    helperIntegralPass2.LaunchGrid(std::vector<Texture *>{&tex, &columnReducedMatrixSummed, &rowReducedMatrixSummed2, &teximageIntegral},
  //                                   img.cols, img.rows);

  //    helperAverager.LaunchGrid(std::vector<Texture *>{&teximageIntegral, &texout}, img.cols, img.rows);
  //  }

  //  renderer.Reclaim();
  //}

}
