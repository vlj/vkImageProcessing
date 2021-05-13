#pragma once

#include <Base.hpp>

namespace Base {

template <typename TextureStatesTuple> struct GPUAsyncCommand {
  vk::Device dev;
  vk::DescriptorPool descriptorSetPool;
  vk::CommandPool commandPool;
  std::list<vk::UniqueFence> fences;
  std::list<vk::UniqueCommandBuffer> commandBuffers;
  TextureStatesTuple textureStates;

  GPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple &&tuple)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), commandPool(_commandPool), textureStates(std::move(tuple)) {}

  template <typename FuncType> auto then(vk::Queue queue, FuncType f) {
    auto startedCommandBuffer = Base::CreateOneShotStartedBuffer(dev, commandPool);
    auto newTextureStates = f(startedCommandBuffer, std::move(textureStates));
    auto endedCommandBuffer = Base::EndBufferRecording(std::move(startedCommandBuffer));
    auto [fence, cmdbuf] = Base::SubmitBuffer(dev, queue, std::move(endedCommandBuffer));

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
      Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*f));
    }
    fences.clear();
    commandBuffers.clear();
    return std::move(textureStates);
  }
};

template <typename... TextureStates>
GPUAsyncCommand<std::tuple<TextureStates...>> GPUAsyncUnit(vk::Device _dev, vk::DescriptorPool _descriptorSetPool,
                                                           vk::CommandPool _commandPool, TextureStates &&...t) {
  return {_dev, _descriptorSetPool, _commandPool, std::make_tuple(std::move(t)...)};
}

} // namespace Base