struct test_pushconstant_h_spv 
{
  static const std::vector<uint32_t> bytecode;
  vk::Device dev;
  vk::UniqueShaderModule shaderModule;
  std::array<vk::UniqueDescriptorSetLayout, 1> descriptorSetLayout;
  vk::UniquePipelineLayout pipelineLayout;
  vk::UniquePipeline pipeline;

  struct UBO {
    glm::vec2 someInput;
  };

  test_pushconstant_h_spv (vk::Device d) : dev(d)
  {

      vk::ShaderModuleCreateInfo moduleCreateInfo{
        vk::ShaderModuleCreateFlags{},
        bytecode.size() * 4,
        bytecode.data()
      };
      shaderModule = dev.createShaderModuleUnique(moduleCreateInfo);

      std::vector<vk::PushConstantRange> pushConstantRanges;  

      {
        std::vector<vk::DescriptorSetLayoutBinding> bindings;                    

        {
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding(0)
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::eCombinedImageSampler)
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


        auto createInfo = vk::DescriptorSetLayoutCreateInfo().setBindings(bindings);
        descriptorSetLayout[0] = std::move(dev.createDescriptorSetLayoutUnique(createInfo));
      }

    {
        auto range = vk::PushConstantRange{}.setOffset(0).setSize(sizeof(UBO)).setStageFlags(vk::ShaderStageFlagBits::eCompute);
        pushConstantRanges.push_back(range);
    }
    
      std::vector<vk::DescriptorSetLayout> conv_layout;
      for (auto &&tmp : descriptorSetLayout) {
        conv_layout.push_back(*tmp);
      }
      auto pipelineLayoutCreateInfo = vk::PipelineLayoutCreateInfo()
        .setPushConstantRanges(pushConstantRanges)
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
      Base::NameObject(dev, *pipeline, "test_pushconstant_h_spv");  }

    std::vector<vk::DescriptorSet> CreateDescriptorSets(
        vk::DescriptorPool descriptorSetPool,
        vk::ImageView someSamplerImage,
        vk::Sampler someSamplerSampler,
        vk::ImageView result
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
            .setImageView(someSamplerImage)
            .setSampler(someSamplerSampler)
            .setImageLayout(vk::ImageLayout::eShaderReadOnlyOptimal);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        {
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView(result)
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }


        int idx = 0;
        std::vector<vk::WriteDescriptorSet> writes;



        {
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eCombinedImageSampler)
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

  template <vk::Format coreSamplerFormat>
  [[nodiscard]]
  auto operator()(
    Base::WorkgroupGeometry workgroupGeometry,
    Base::StartedCommandBuffer& commandBuffer,
    vk::DescriptorPool descriptorSetPool,
    Base::DecoratedState<vk::ImageLayout::eShaderReadOnlyOptimal, coreSamplerFormat> &someSamplerImage,
    vk::Sampler someSamplerSampler,
    Base::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR16G16B16A16Sfloat> &result,
    UBO pushCst
  )
{
    auto [xBlockCount, yBlockCount] = workgroupGeometry;

    auto descriptorSets = CreateDescriptorSets(descriptorSetPool, someSamplerImage.view, someSamplerSampler,result.view);

    std::vector<uint32_t> dynamicOffsets;
    (*commandBuffer).bindDescriptorSets(vk::PipelineBindPoint::eCompute, *pipelineLayout, 0, descriptorSets, dynamicOffsets);
    (*commandBuffer).pushConstants(*pipelineLayout, vk::ShaderStageFlagBits::eCompute, 0, sizeof(UBO), &pushCst);
    (*commandBuffer).bindPipeline(vk::PipelineBindPoint::eCompute, *pipeline);
    (*commandBuffer).dispatch(xBlockCount, yBlockCount, 1);
  }
};
