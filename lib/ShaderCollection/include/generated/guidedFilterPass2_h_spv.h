struct guidedFilterPass2_h_spv 
{
  static const std::vector<uint32_t> bytecode;
  vk::Device dev;
  vk::UniqueShaderModule shaderModule;
  std::array<vk::UniqueDescriptorSetLayout, 1> descriptorSetLayout;
  vk::UniquePipelineLayout pipelineLayout;
  vk::UniquePipeline pipeline;
  guidedFilterPass2_h_spv (vk::Device d) : dev(d)
  {

      vk::ShaderModuleCreateInfo moduleCreateInfo{
        vk::ShaderModuleCreateFlags{},
        bytecode.size() * 4,
        bytecode.data()
      };
      shaderModule = dev.createShaderModuleUnique(moduleCreateInfo);

      {
        std::vector<vk::DescriptorSetLayoutBinding> bindings;                    

        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(0)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }


        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(1)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }


        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(2)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }


        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(3)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }


        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(4)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }


        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(5)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }


        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(6)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }

        auto createInfo = vk::DescriptorSetLayoutCreateInfo().setBindings(bindings);
        descriptorSetLayout[0] = std::move(dev.createDescriptorSetLayoutUnique(createInfo));
      }

      std::vector<vk::DescriptorSetLayout> conv_layout;
      for (auto &&tmp : descriptorSetLayout) {
        conv_layout.push_back(*tmp);
      }
      auto pipelineLayoutCreateInfo = vk::PipelineLayoutCreateInfo()
        .setSetLayouts(conv_layout);

      pipelineLayout = dev.createPipelineLayoutUnique(pipelineLayoutCreateInfo);

      auto stage = vk::PipelineShaderStageCreateInfo()
        .setStage(vk::ShaderStageFlagBits::eCompute)
        .setPName("main")
        .setModule(*shaderModule);    
      auto computePipelineCreateInfo = vk::ComputePipelineCreateInfo{}
        .setLayout(*pipelineLayout)
        .setStage(stage);
      auto pipelineInfos = std::vector({computePipelineCreateInfo});
      auto pipelines = dev.createComputePipelinesUnique(vk::PipelineCache(), pipelineInfos, nullptr);
      pipeline = std::move(pipelines.value[0]);
      Base::NameObject(dev, *pipeline, "guidedFilterPass2_h_spv");  }

    std::vector<vk::DescriptorSet> CreateDescriptorSets(
        vk::DescriptorPool descriptorSetPool,
      vk::ImageView _input,
      vk::ImageView summedColumnReducedMatrix,
      vk::ImageView summedSquaredColumnReducedMatrix,
      vk::ImageView summedRowReducedMatrix,
      vk::ImageView summedSquaredRowReducedMatrix,
      vk::ImageView outputMean,
      vk::ImageView outputSquaredMean
    )
    {
        std::vector<vk::DescriptorSetLayout> conv_layout;
        for (auto &&tmp : descriptorSetLayout) {
          conv_layout.push_back(*tmp);
        }

        auto allocateInfo =
            vk::DescriptorSetAllocateInfo()
                .setDescriptorPool(descriptorSetPool)
                .setDescriptorSetCount(1)
                .setSetLayouts(conv_layout);
        std::vector<vk::DescriptorSet> descriptorSets{
            dev.allocateDescriptorSets(allocateInfo)
        };

        std::vector<vk::DescriptorImageInfo> descriptorImageInfos;

        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(_input)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(summedColumnReducedMatrix)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(summedSquaredColumnReducedMatrix)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(summedRowReducedMatrix)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(summedSquaredRowReducedMatrix)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(outputMean)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(outputSquaredMean)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        int idx = 0;
        std::vector<vk::WriteDescriptorSet> writes;



        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            

        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            

        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            

        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            

        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            

        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            

        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }
            
    
        dev.updateDescriptorSets(writes, {});
        return descriptorSets;
      }

  [[nodiscard]]
  auto operator()(
    Base::WorkgroupGeometry workgroupGeometry,
    Base::StartedCommandBuffer& commandBuffer,
    vk::DescriptorPool descriptorSetPool,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &_input,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &summedColumnReducedMatrix,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &summedSquaredColumnReducedMatrix,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &summedRowReducedMatrix,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &summedSquaredRowReducedMatrix,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &outputMean,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &outputSquaredMean
  )
{
    auto [xBlockCount, yBlockCount] = workgroupGeometry;

    auto descriptorSets = CreateDescriptorSets(descriptorSetPool, _input.view,summedColumnReducedMatrix.view,summedSquaredColumnReducedMatrix.view,summedRowReducedMatrix.view,summedSquaredRowReducedMatrix.view,outputMean.view,outputSquaredMean.view);

    std::vector<uint32_t> dynamicOffsets;
    (*commandBuffer).bindDescriptorSets(vk::PipelineBindPoint::eCompute, *pipelineLayout, 0, descriptorSets, dynamicOffsets);
    (*commandBuffer).bindPipeline(vk::PipelineBindPoint::eCompute, *pipeline);
    (*commandBuffer).dispatch(xBlockCount, yBlockCount, 1);
  }
};
