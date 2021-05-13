#include <list>
#include <memory>
#include <vector>
#include <vulkan/vulkan.hpp>
#include <vulkan/vulkan_win32.h>

#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>


#include <Context.hpp>
#include <v2utils.h>
#include <GPUCommandAsync.hpp>

#include <GuidedFilterHelpers.hpp>
#include <IntegralImageHelpers.h>
#include <ShadersCollection.h>
#include <highlevelhelpers.h>
#include <WindowingSystem.hpp>

int main() {
  cv::Mat img(cv::Size(500, 281), CV_32FC1, cv::Scalar(1.));
  auto _img = cv::imread(R"(C:\Users\vljno\OneDrive\Images\20200822_153658.jpg)");
  cv::cvtColor(_img, img, cv::COLOR_RGB2GRAY);
  //_img.col(0).setTo(cv::Scalar(1.f));
  // cv::Mat img;
  // cv::integral(_img, img);
  img.convertTo(img, CV_32FC1);

  // for (int i = 0; i < 500; ++i) {
  //  for (int j = 0; j < 281; ++j) {
  //    float tmp = 0;
  //    if (i % 4 == 3)
  //      tmp += 1;
  //    if (j % 4 == 3)
  //      tmp += 1;
  //    img.at<float>(j, i) = 1024.f; // std::sqrt(img.at<float>(j, i));
  //  }
  //}

  glfwInit();
  glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
  glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
  auto window = glfwCreateWindow(img.cols, img.rows, "ImageView", nullptr, nullptr);

  uint32_t glfwExtensionCount = 0;
  const char **glfwExtensions = glfwGetRequiredInstanceExtensions(&glfwExtensionCount);
  std::vector<const char *> extensions(glfwExtensions, glfwExtensions + glfwExtensionCount);
  std::vector<const char *> deviceextensions{VK_KHR_SWAPCHAIN_EXTENSION_NAME};

  auto instance = createInstance(extensions);
  auto callback = createDebugCallback(*instance);

  {
    auto surface = WindowingSystem::GetSurfaceFromGLFWWindows(*instance, window);

    auto physDev = instance->enumeratePhysicalDevices();
    Renderer renderer(physDev[0], deviceextensions);
    auto swapChain = WindowingSystem::SwapChainSupport(renderer, *surface, img.cols, img.rows);

    auto shaderList = v2::utils::ShaderList::Build(*renderer.dev);

    size_t width = img.cols;
    size_t height = img.rows;

    IntegralImageHelper::VerticalSummer verticalSummer(renderer);
    IntegralImageHelper::HorizontalSummer horizontalSummer(renderer);
    GuidedFilter::IntegralImageHelper IIH(renderer);
    gsl::span<std::byte> imgData((std::byte*)(img.ptr()), 4 * img.cols * img.rows);
    auto [buf, buffermem] = Base::GetMemoryBufferFrom(*renderer.dev, renderer.memprop, imgData);
    auto buffer = std::move(buf);

    auto [texUndef, texStorage] = Base::CreateTexture<vk::Format::eR32Sfloat>(*renderer.dev, img.cols, img.rows, "input");
    auto [texrgba8Undef, texrgba8Storage] =
        Base::CreateTexture<vk::Format::eB8G8R8A8Unorm>(*renderer.dev, img.cols, img.rows, "rgba8output");

    auto guidedFilterResources = GuidedFilter::IntegralImageHelper::BuildImageStorage(
        *renderer.dev, *renderer.commandPool, renderer.memprop, renderer.queue, *renderer.descriptorSetPool, img);

    auto newTexturesState =
        Base::GPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool, std::move(texrgba8Undef),
                                std::move(texUndef))
            .then(renderer.queue,
                  [&](auto &cmdBuffer, auto &&textureState) {
                    auto [rgba8undef, texundef] = std::move(textureState);
                    auto rgba8 = Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuffer, std::move(rgba8undef));

                    auto textureDest = Base::Transition<vk::ImageLayout::eTransferDstOptimal>(*cmdBuffer, std::move(texundef));

                    auto updatedTexture = Base::CopyToTexture(cmdBuffer, textureDest, *buffer);
                    auto readableTexture = Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuffer, std::move(updatedTexture));
                    auto tex = Base::Transition<vk::ImageLayout::eGeneral>(*cmdBuffer, std::move(readableTexture));
                    return std::make_tuple(std::move(rgba8), std::move(tex));
                  })
            .Sync();
    auto texrgba8 = std::move(std::get<0>(newTexturesState));
    auto tex = std::move(std::get<1>(newTexturesState));

    while (!glfwWindowShouldClose(window)) {
      glfwPollEvents();

      Base::GPUAsyncUnit(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool)
          .then(renderer.queue,
                [&](auto &cmdbuffer, auto &&textureStates) {
                  IIH.draw(shaderList, guidedFilterResources, tex, cmdbuffer, width, height);
                  shaderList.r32fToRgba8Pipeline({size_t((width + 15) / 16), size_t((height + 15) / 16)}, cmdbuffer,
                                                 *renderer.descriptorSetPool, guidedFilterResources.result, texrgba8);
                  return std::move(textureStates);
                })
          .Sync();
      auto idx = swapChain.GetPresentImage();
      WindowingSystem::CopyToPresentImage(*renderer.dev, *renderer.commandPool, renderer.queue, *renderer.descriptorSetPool, texrgba8,
                                    swapChain.extent.width, swapChain.extent.height, swapChain.swapChainImages[idx]);
      swapChain.Present(idx);
    }
  }

  glfwDestroyWindow(window);

  glfwTerminate();

  return 0;
}