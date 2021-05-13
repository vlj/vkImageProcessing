#include <Context.hpp>

#include <ShadersCollection.h>
#include <highlevelhelpers.h>
#include <IntegralImageHelpers.h>
#include <Base.hpp>


namespace GuidedFilter {


struct AuxiliarySummer {

  Renderer &renderer;

  struct ImageStorage {
    v2::DecoratedState<vk::ImageLayout::eGeneral> columnReducedMatrix;
    v2::DecoratedState<vk::ImageLayout::eGeneral> squaredColumnReducedMatrix;

    v2::DecoratedState<vk::ImageLayout::eGeneral> columnReducedMatrixSummed;
    v2::DecoratedState<vk::ImageLayout::eGeneral> squaredColumnReducedMatrixSummed;

    v2::DecoratedState<vk::ImageLayout::eGeneral> rowReducedMatrix;
    v2::DecoratedState<vk::ImageLayout::eGeneral> squaredRowReducedMatrix;

    v2::DecoratedState<vk::ImageLayout::eGeneral> rowReducedMatrixSummed;
    v2::DecoratedState<vk::ImageLayout::eGeneral> rowReducedMatrixSummed2;
    v2::DecoratedState<vk::ImageLayout::eGeneral> squaredRowReducedMatrixSummed;
    v2::DecoratedState<vk::ImageLayout::eGeneral> squaredRowReducedMatrixSummed2;
  };

  IntegralImageHelper::VerticalSummer helperVertical;
  IntegralImageHelper::HorizontalSummer helperHorizontal;

  static constexpr auto kSubgroupSize = 16;

  static ImageStorage BuildImageStorage(vk::Device dev, vk::CommandBuffer cmdbuf, size_t width, size_t height);

  AuxiliarySummer(Renderer &_renderer);
  void draw(v2::StartedCommandBuffer &commandBuffer, ImageStorage &imageStorage);
};

struct IntegralImageHelper {

  struct ImageStorage {
    size_t width;
    size_t height;
    AuxiliarySummer::ImageStorage auxiliaryImage;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> teximageIntegral;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> texsquaredImageIntegral;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> meanI;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> meanSqI;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> meanA;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> meanB;
    v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> result;
  };

  static ImageStorage BuildImageStorage(vk::Device dev, vk::CommandPool commandPool, vk::PhysicalDeviceMemoryProperties memprop,
                                        vk::Queue queue, vk::DescriptorPool descriptorSetPool, const cv::Mat &img);

  Renderer &renderer;
  AuxiliarySummer aux;

  IntegralImageHelper(Renderer &_renderer);

  void draw(v2::utils::ShaderList &shaderList, ImageStorage &storedImage, v2::DecoratedState<vk::ImageLayout::eGeneral> &tex,
            v2::StartedCommandBuffer &commandBuffer, size_t width, size_t height);
};
} // namespace
