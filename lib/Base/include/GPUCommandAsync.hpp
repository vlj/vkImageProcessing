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

template <typename TextureStatesTuple, typename CallbackType> struct GPUAsyncCommand2 {
  vk::Device dev;
  vk::DescriptorPool descriptorSetPool;
  vk::CommandPool commandPool;

  TextureStatesTuple textureStates;

  CallbackType toCall;

  GPUAsyncCommand2(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple &&tuple,
                   CallbackType &&f)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), commandPool(_commandPool), textureStates(std::move(tuple)), toCall(std::move(f)) {
  }

  GPUAsyncCommand2(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool,
                   Base::StartedCommandBuffer &&startedCommandBuffer, TextureStatesTuple &&tuple, CallbackType &&f)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), commandPool(_commandPool), textureStates(std::move(tuple)), toCall(std::move(f)) {

  }

   template <typename FuncType> 
   auto then(FuncType f) {
    auto startedCommandBuffer = Base::CreateOneShotStartedBuffer(dev, commandPool);
    auto resultGPUAsync = f(std::move(startedCommandBuffer), std::move(textureStates));

    auto newToCall = [this, toCall = std::move(toCall),
                      resultGPUAsyncToCall = std::move(resultGPUAsync.toCall)](TemporaryThings &&previous) mutable {
      previous = toCall(std::move(previous));
      return resultGPUAsyncToCall(std::move(previous));
    };
    return MakeGPUAsyncUnit2(dev, descriptorSetPool, commandPool, std::move(resultGPUAsync.textureStates), std::move(newToCall));
  }

  auto Sync() {
    auto collected = toCall(TemporaryThings{});
    for (auto &f : collected.fences) {
      Base::WaitAndReset(dev, descriptorSetPool, commandPool, std::move(*f));
    }
    return std::move(textureStates);
  }
};

template <typename TextureStatesTuple>
auto MakeGPUAsync2(vk::Device dev, vk::DescriptorPool descriptorSetPool, vk::CommandPool commandPool, vk::Queue queue,
                   Base::StartedCommandBuffer &&startedCommandBuffer, TextureStatesTuple &&tuple) {
  auto endedCommandBuffer = Base::EndBufferRecording(std::move(startedCommandBuffer));
  auto toCall = [dev, descriptorSetPool, commandPool, queue, endedCommandBuffer = std::move(endedCommandBuffer)](TemporaryThings &&previous) mutable {
    auto [fence, cmdbuf] = Base::SubmitBuffer(dev, queue, std::move(endedCommandBuffer));

    previous.fences.push_back(std::move(fence));
    previous.commandBuffers.push_back(std::move(cmdbuf));
    return std::move(previous);
  };
  return GPUAsyncCommand2(dev, descriptorSetPool, commandPool, std::move(tuple), std::move(toCall));
}

template <typename TextureStateTuple, typename CallbackType>
auto MakeGPUAsyncUnit2(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStateTuple &&t,
                  CallbackType &&f) {
  return GPUAsyncCommand(_dev, _descriptorSetPool, _commandPool, std::move(t), std::move(f));
}

template <typename TextureStatesTuple>
auto MakeGPUAsyncUnit2(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple &&tuple) {
  auto defaultFunc = [](TemporaryThings &&in) { return std::move(in); };
  return GPUAsyncCommand2(_dev, _descriptorSetPool, _commandPool, std::move(tuple), std::move(defaultFunc));
}

} // namespace Base