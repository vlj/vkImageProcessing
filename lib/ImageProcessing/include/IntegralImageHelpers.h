#pragma once

#include <Context.hpp>

#include <ShadersCollection.h>
#include <highlevelhelpers.h>

namespace IntegralImageHelper {

struct VerticalSummer {
  Renderer &renderer;

  v2::verticalSumation_h_spv helper;

  VerticalSummer(Renderer &_renderer) : renderer(_renderer), helper(*renderer.dev) {}

  void operator()(v2::StartedCommandBuffer &commandBuffer, v2::DecoratedState<vk::ImageLayout::eGeneral> &_input,
                        v2::DecoratedState<vk::ImageLayout::eGeneral> &_output) {
    assert(_input.tex->width == _output.tex->width);
    assert(_input.tex->height == _output.tex->height);

    auto blockGeometry = v2::LaunchScanTtB<Shaders::VerticalSumation>(_input.tex->height, _input.tex->width);
    helper(blockGeometry, commandBuffer, *renderer.descriptorSetPool, std::move(_input), std::move(_output));
  }
};

struct HorizontalSummer {
  Renderer &renderer;

  v2::horizontalSumation_h_spv helper;

  HorizontalSummer(Renderer &_renderer)
      : renderer(_renderer), helper(*renderer.dev) {}

  void operator()(v2::StartedCommandBuffer &commandBuffer, v2::DecoratedState<vk::ImageLayout::eGeneral> &_input,
                        v2::DecoratedState<vk::ImageLayout::eGeneral> &_output) {
    assert(_input.tex->width == _output.tex->width);
    assert(_input.tex->height == _output.tex->height);

    auto blockGeometry = v2::LaunchScanLtR<Shaders::HorizontalSumation>(_input.tex->width, _input.tex->height);
    helper(blockGeometry, commandBuffer, *renderer.descriptorSetPool, std::move(_input), std::move(_output));
  }
};

} // namespace IntegralImageHelper
