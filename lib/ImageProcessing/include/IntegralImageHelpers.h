#pragma once

#include <Context.hpp>

#include <ShadersCollection.h>
#include <highlevelhelpers.h>

namespace IntegralImageHelper {

struct VerticalSummer {
  Renderer &renderer;

  Shaders::verticalSumation_h_spv helper;

  VerticalSummer(Renderer &_renderer) : renderer(_renderer), helper(*renderer.dev) {}

  void operator()(Base::StartedCommandBuffer &commandBuffer, Base::DecoratedState<vk::ImageLayout::eGeneral> &_input,
                  Base::DecoratedState<vk::ImageLayout::eGeneral> &_output) {
    assert(_input.width == _output.width);
    assert(_input.height == _output.height);

    auto blockGeometry = v2::LaunchScanTtB<Shaders::VerticalSumation>(_input.height, _input.width);
    helper(blockGeometry, commandBuffer, *renderer.descriptorSetPool, std::move(_input), std::move(_output));
  }
};

struct HorizontalSummer {
  Renderer &renderer;

  Shaders::horizontalSumation_h_spv helper;

  HorizontalSummer(Renderer &_renderer)
      : renderer(_renderer), helper(*renderer.dev) {}

  void operator()(Base::StartedCommandBuffer &commandBuffer, Base::DecoratedState<vk::ImageLayout::eGeneral> &_input,
                  Base::DecoratedState<vk::ImageLayout::eGeneral> &_output) {
    assert(_input.width == _output.width);
    assert(_input.height == _output.height);

    auto blockGeometry = v2::LaunchScanLtR<Shaders::HorizontalSumation>(_input.width, _input.height);
    helper(blockGeometry, commandBuffer, *renderer.descriptorSetPool, std::move(_input), std::move(_output));
  }
};

} // namespace IntegralImageHelper
