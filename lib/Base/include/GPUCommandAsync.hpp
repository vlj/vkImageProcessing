#pragma once

#include <Base.hpp>

namespace Base {

  struct QueueData {
  vk::Queue queue;
  vk::CommandPool commandPool;
};

struct TemporaryThings {
  std::list<vk::UniqueFence> fences;
  std::list<vk::UniqueCommandBuffer> commandBuffers;
  std::list<vk::CommandPool> commandPoolsToReset;
};

template <typename TextureStatesTuple, typename CallbackType> struct GPUAsyncCommand {
  vk::Device dev;
  vk::DescriptorPool descriptorSetPool;
  vk::CommandPool commandPool;

  TextureStatesTuple textureStates;

  CallbackType toCall;

  GPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple &&tuple,
                   CallbackType &&f)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), commandPool(_commandPool), textureStates(std::move(tuple)), toCall(std::move(f)) {
  }

  GPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool,
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
    return MakeGPUAsyncUnit(dev, descriptorSetPool, commandPool, std::move(resultGPUAsync.textureStates), std::move(newToCall));
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
auto MakeGPUAsync(vk::Device dev, vk::DescriptorPool descriptorSetPool, vk::CommandPool commandPool, vk::Queue queue,
                   Base::StartedCommandBuffer &&startedCommandBuffer, TextureStatesTuple &&tuple) {
  auto endedCommandBuffer = Base::EndBufferRecording(std::move(startedCommandBuffer));
  auto toCall = [dev, descriptorSetPool, commandPool, queue, endedCommandBuffer = std::move(endedCommandBuffer)](TemporaryThings &&previous) mutable {
    auto [fence, cmdbuf] = Base::SubmitBuffer(dev, queue, std::move(endedCommandBuffer));

    previous.fences.push_back(std::move(fence));
    previous.commandBuffers.push_back(std::move(cmdbuf));
    return std::move(previous);
  };
  return GPUAsyncCommand(dev, descriptorSetPool, commandPool, std::move(tuple), std::move(toCall));
}

template <typename TextureStateTuple, typename CallbackType>
auto MakeGPUAsyncUnit(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStateTuple &&t,
                  CallbackType &&f) {
  return GPUAsyncCommand(_dev, _descriptorSetPool, _commandPool, std::move(t), std::move(f));
}

template <typename TextureStatesTuple>
auto MakeGPUAsyncUnit(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, vk::CommandPool _commandPool, TextureStatesTuple &&tuple) {
  auto defaultFunc = [](TemporaryThings &&in) { return std::move(in); };
  return GPUAsyncCommand(_dev, _descriptorSetPool, _commandPool, std::move(tuple), std::move(defaultFunc));
}

} // namespace Base

namespace Experimental {

template <typename TextureStatesTuple, typename CallbackType> struct GPUAsyncCommand {
  vk::Device dev;
  vk::DescriptorPool descriptorSetPool;

  TextureStatesTuple textureStates;

  CallbackType toCall;

  GPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, TextureStatesTuple &&tuple,
                  CallbackType &&f)
      : dev(_dev), descriptorSetPool(_descriptorSetPool), textureStates(std::move(tuple)), toCall(std::move(f)) {
  }

  template <typename FuncType> auto then(FuncType f) {
    auto resultGPUAsync = f(std::move(textureStates));

    auto newToCall = [this, toCall = std::move(toCall),
                      resultGPUAsyncToCall = std::move(resultGPUAsync.toCall)](Base::TemporaryThings &&previous) mutable {
      previous = toCall(std::move(previous));
      return resultGPUAsyncToCall(std::move(previous));
    };
    return MakeGPUAsyncCommand(dev, descriptorSetPool, std::move(resultGPUAsync.textureStates), std::move(newToCall));
  }

  auto Sync() {
    auto collected = toCall(Base::TemporaryThings{});
    for (auto &f : collected.fences) {
      auto res2 = dev.waitForFences(*f, true, -1);
    }
    for (auto &pool : collected.commandPoolsToReset) {

      dev.resetCommandPool(pool, vk::CommandPoolResetFlagBits::eReleaseResources);
    }

    dev.resetDescriptorPool(descriptorSetPool);
    return std::move(textureStates);
  }
};

template <typename TextureStatesTuple, typename CallbackType>
auto MakeGPUAsyncCommand(vk::Device _dev, vk::DescriptorPool _descriptorSetPool, TextureStatesTuple &&tuple, CallbackType &&f) {
  return GPUAsyncCommand(_dev, _descriptorSetPool, std::move(tuple), std::move(f));
}

template <typename Callback>
auto MakeGPUAsync(vk::Device dev, vk::DescriptorPool descriptorSetPool, Base::QueueData queueData, Callback &&function) {
  auto startedCommandBuffer = Base::CreateOneShotStartedBuffer(dev, queueData.commandPool);
  auto resultGPUAsync = function(std::move(startedCommandBuffer));
  auto endedCommandBuffer = Base::EndBufferRecording(std::move(startedCommandBuffer));
  auto toCall = [dev, descriptorSetPool, queueData,
                 endedCommandBuffer = std::move(endedCommandBuffer)](Base::TemporaryThings &&previous) mutable {
    auto [fence, cmdbuf] = Base::SubmitBuffer(dev, queueData.queue, std::move(endedCommandBuffer));

    previous.fences.push_back(std::move(fence));
    previous.commandBuffers.push_back(std::move(cmdbuf));
    previous.commandPoolsToReset.push_back(queueData.commandPool);
    return std::move(previous);
  };
  return GPUAsyncCommand(dev, descriptorSetPool, std::move(resultGPUAsync), std::move(toCall));
}


template <typename TextureStatesTuple>
auto MakeGPUAsyncUnit(vk::Device dev, vk::DescriptorPool descriptorSetPool, TextureStatesTuple &&textures) {

  auto toCall = [](Base::TemporaryThings &&previous) mutable {
    return std::move(previous);
  };
  return GPUAsyncCommand(dev, descriptorSetPool, std::move(textures), std::move(toCall));
}
} // namespace Experimental