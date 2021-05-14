#pragma once

#include <Base.hpp>

namespace Base {

struct TemporaryThings {
  std::list<vk::UniqueFence> fences;
  std::list<vk::UniqueCommandBuffer> commandBuffers;
};

template <typename TextureStatesTuple, typename CallbackType> struct GPUAsyncCommand {
  vk::Device dev;
  vk::DescriptorPool descriptorSetPool;
  vk::CommandPool commandPool;

  TextureStatesTuple textureStates;



  CallbackType toCall;

  GPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple &&tuple,
                  CallbackType&& f)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), commandPool(_commandPool), textureStates(std::move(tuple)), toCall(std::move(f)) {}

  template <typename FuncType> auto then(vk::Queue queue, FuncType f) {
    auto startedCommandBuffer = Base::CreateOneShotStartedBuffer(dev, commandPool);
    auto newTextureStates = f(startedCommandBuffer, std::move(textureStates));
    auto endedCommandBuffer = Base::EndBufferRecording(std::move(startedCommandBuffer));

    auto newToCall = [this, toCall = std::move(toCall), queue, endedCommandBuffer = std::move(endedCommandBuffer)](TemporaryThings &&previous) mutable {
      previous = toCall(std::move(previous));

      auto [fence, cmdbuf] = Base::SubmitBuffer(dev, queue, std::move(endedCommandBuffer));

      previous.fences.push_back(std::move(fence));
      previous.commandBuffers.push_back(std::move(cmdbuf));
      return std::move(previous);
    };

    auto result = GPUAsyncUnit(dev, descriptorSetPool, commandPool, std::move(newTextureStates), std::move(newToCall));
    return std::move(result);
  }

  auto Sync() {
    auto collected = toCall(TemporaryThings{});
    for (auto &f : collected.fences) {
      Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*f));
    }
    return std::move(textureStates);
  }
};

template <typename TextureStateTuple, typename CallbackType>
auto GPUAsyncUnit(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStateTuple &&t,
                  CallbackType &&f) {
  return GPUAsyncCommand(_dev, _descriptorSetPool, _commandPool, std::move(t), std::move(f));
}

template <typename TextureStateTuple>
auto GPUAsyncUnit(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStateTuple &&t) {
  auto defaultFunc = [](TemporaryThings &&in) { return std::move(in); };
  return GPUAsyncCommand(_dev, _descriptorSetPool, _commandPool, std::move(t), std::move(defaultFunc));
}

} // namespace Base