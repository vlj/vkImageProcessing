#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use super::SPV;

impl ToString for SPV::DescriptorType {
    fn to_string(&self) -> String {
        let as_str = match self {
            SPV::DescriptorType::COMBINED_IMAGE_SAMPLER => "vk::DescriptorType::eCombinedImageSampler",
            SPV::DescriptorType::STORAGE_IMAGE => "vk::DescriptorType::eStorageImage",
            SPV::DescriptorType::UNIFORM_BUFFER => "vk::DescriptorType::eUniformBuffer",
            _ => panic!("Unsupported texture descriptor")
        };
        as_str.to_string()
    }
}

impl ToString for SPV::SpvImageFormat {
    fn to_string(&self) -> String {
        let as_str = 
            match self {
                SPV::SpvImageFormat::R32f => "eR32Sfloat",
                SPV::SpvImageFormat::Rgba8 => "eB8G8R8A8Unorm",
                SPV::SpvImageFormat::Rgba16f => "eR16G16B16A16Sfloat",
                SPV::SpvImageFormat::Unknown => "eUndefined",
                _ => panic!("Unsupported texture type")
            };
        as_str.to_string()
    }
}


pub fn generate_buffer_type_declaration(module: &SPV::CleanSpvReflectShaderModule) -> String
{
    
    let mut headerContent = String::new();

    let getFlattenedBindingIterator = || {
        module.descriptor_sets
            .iter()
            .map(|x| {
                x.descriptor_bindings.iter()
            })
            .flatten()
    };
    
    let uniformBufferIterator  =  getFlattenedBindingIterator()
        .filter_map(|descriptor_bindings| {
            match &descriptor_bindings.content {
                SPV::DescriptorBindingContent::Block(blockInfo) => Some(blockInfo.type_description.clone()),
                _ => None
            }
        });

    let listOfStructures: Vec<_> = uniformBufferIterator.map(
        |type_description| {
            let members: Vec<_> = type_description.members
                .iter()
                .map(|member| {
                    format!("    {} {};", "float", member.struct_member_name)
                })
                .collect();
            format!("\n  struct {} {{\n{}\n  }};", type_description.type_name, members.join("\n"))
        }
    ).collect();

    headerContent.push_str(&format!("{}\n\n", listOfStructures.join("\n")));
    headerContent
}


pub fn generate_constructor_code(shader_name: &str, module: &SPV::CleanSpvReflectShaderModule) -> String
{
    let mut headerContent = String::new();
    
    // Constructor
    headerContent.push_str(&format!("  {} (vk::Device d) : dev(d)\n", shader_name));
    headerContent.push_str("  {\n");

    // Create shaderModule
    headerContent.push_str(
"
      vk::ShaderModuleCreateInfo moduleCreateInfo{
        vk::ShaderModuleCreateFlags{},
        bytecode.size() * 4,
        bytecode.data()
      };
      shaderModule = dev.createShaderModuleUnique(moduleCreateInfo);
");

    // Create descriptorSetLayout
    let descriptorSetLayoutCode = {

        let populateLayout: Vec<_> = {

            module.descriptor_sets.iter()
                .map(|descriptorSet| {
                    let populateBindings: Vec<_> = descriptorSet.descriptor_bindings
                        .iter()
                        .map(|binding| {
                            format!("
        {{
          auto binding = vk::DescriptorSetLayoutBinding()
            .setBinding({})
            .setDescriptorCount(1)
            .setDescriptorType(vk::DescriptorType::{})
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }}
", binding.binding, binding.descriptor_type.to_string())
                        })
                        .collect();
                    format!("
      {{
        std::vector<vk::DescriptorSetLayoutBinding> bindings;                    
{}
        auto createInfo = vk::DescriptorSetLayoutCreateInfo().setBindings(bindings);
        descriptorSetLayout[{}] = std::move(dev.createDescriptorSetLayoutUnique(createInfo));
      }}
", populateBindings.join("\n"), descriptorSet.set)
                })
                .collect()
        };
        populateLayout.join("\n")
    };

    headerContent.push_str(&descriptorSetLayoutCode);

    // Create PipelineLayout

    // Create pipeline

    let pipelineCode = format!("
      std::vector<vk::DescriptorSetLayout> conv_layout;
      for (auto &&tmp : descriptorSetLayout) {{
        conv_layout.push_back(*tmp);
      }}
      auto pipelineLayoutCreateInfo = vk::PipelineLayoutCreateInfo()
        .setSetLayouts(conv_layout);

      pipelineLayout = dev.createPipelineLayoutUnique(pipelineLayoutCreateInfo);

      auto stage = vk::PipelineShaderStageCreateInfo()
        .setStage(vk::ShaderStageFlagBits::eCompute)
        .setPName(\"main\")
        .setModule(*shaderModule);    
      auto computePipelineCreateInfo = vk::ComputePipelineCreateInfo{{}}
        .setLayout(*pipelineLayout)
        .setStage(stage);
      auto pipelineInfos = std::vector({{computePipelineCreateInfo}});
      auto pipelines = dev.createComputePipelinesUnique(vk::PipelineCache(), pipelineInfos, nullptr);
      pipeline = std::move(pipelines.value[0]);
");
    headerContent.push_str(&pipelineCode);

    // Name pipeline
    headerContent.push_str(&format!("      Base::NameObject(dev, *pipeline, \"{}\");", shader_name));

    headerContent.push_str("  }\n");
    headerContent
}

pub fn build_operator(module: &SPV::CleanSpvReflectShaderModule) -> String
{
    let getFlattenedBindingIterator = || {
        module.descriptor_sets
            .iter()
            .map(|x| {
                x.descriptor_bindings.iter()
            })
            .flatten()
    };

    let mut headerContent = String::new();

    let createDescriptorSetCode = {
        let mut descriptorDefinition = String::new();
        let arguments: Vec<_> = getFlattenedBindingIterator()
            .map(|x|{ format!("      vk::ImageView {}", &x.name)})
            .collect();

        descriptorDefinition.push_str(&format!("
    std::vector<vk::DescriptorSet> CreateDescriptorSets(
        vk::DescriptorPool descriptorSetPool,
{}
    )
", arguments.join(",\n")));

        descriptorDefinition.push_str("    {");
        descriptorDefinition.push_str(&format!("
        std::vector<vk::DescriptorSetLayout> conv_layout;
        for (auto &&tmp : descriptorSetLayout) {{
          conv_layout.push_back(*tmp);
        }}

        auto allocateInfo =
            vk::DescriptorSetAllocateInfo()
                .setDescriptorPool(descriptorSetPool)
                .setDescriptorSetCount(1)
                .setSetLayouts(conv_layout);
        std::vector<vk::DescriptorSet> descriptorSets{{
            dev.allocateDescriptorSets(allocateInfo)
        }};
"));
        let setResourceCode: Vec<_> =
            getFlattenedBindingIterator()
                .map(|x| {
                    format!("
        {{
          auto descriptorImageInfo = vk::DescriptorImageInfo()
            .setImageView({})
            .setImageLayout(vk::ImageLayout::eGeneral);
          descriptorImageInfos.push_back(descriptorImageInfo);
        }}
", x.name)
                })
                .collect();
        
        descriptorDefinition.push_str(&format!("
        std::vector<vk::DescriptorImageInfo> descriptorImageInfos;
{}

        int idx = 0;
        std::vector<vk::WriteDescriptorSet> writes;
", setResourceCode.join("\n")));

        let writeDescriptor: Vec<_> = {
            getFlattenedBindingIterator()
                .map(|_| {
                    format!("
        {{
            auto writeDescriptor = vk::WriteDescriptorSet()
                .setDescriptorCount(1)
                .setDescriptorType(vk::DescriptorType::eStorageImage)
                .setDstBinding(idx)
                .setDstSet(descriptorSets[0])
                .setImageInfo(descriptorImageInfos[idx]);
            writes.push_back(writeDescriptor);
            idx++;
        }}
            ")
                })
                .collect()
        };

        descriptorDefinition.push_str(&format!("

{}
    
        dev.updateDescriptorSets(writes, {{}});
        return descriptorSets;
      }}
", writeDescriptor.join("\n")));
        descriptorDefinition
    };

    headerContent.push_str(&createDescriptorSetCode);

    let declaration = {


        let convertDescriptorType = |descriptor_type| {
            match descriptor_type {
                SPV::DescriptorType::COMBINED_IMAGE_SAMPLER => "eShaderReadOnlyOptimal",
                SPV::DescriptorType::STORAGE_IMAGE => "eGeneral",
                _ => panic!("Unsupported texture layout")
            }
        };

        let arguments: Vec<_> = getFlattenedBindingIterator()
            .map(|descriptorBinding| {
                let typename = match &descriptorBinding.content {
                    SPV::DescriptorBindingContent::Image(imgInfo) => {
                        let textureType = imgInfo.image_format.to_string();
                        let textureLayout = convertDescriptorType(descriptorBinding.descriptor_type);
                        format!("Base::DecoratedState<vk::ImageLayout::{}, vk::Format::{}>", textureLayout, textureType)
                    },
                    SPV::DescriptorBindingContent::Block(blockInfo) => {
                        format!("{}", descriptorBinding.type_description.type_name)
                    },
                    _ => panic!("Not an image descriptor !!")
                };
                format!("    {} &{}", typename, &descriptorBinding.name)
            })
            .collect();
        format!("
  [[nodiscard]]
  auto operator()(
    Base::WorkgroupGeometry workgroupGeometry,
    Base::StartedCommandBuffer& commandBuffer,
    vk::DescriptorPool descriptorSetPool,
{}
  )
", arguments.join(",\n"))
    };

    headerContent.push_str(&declaration);

    let body = {
        let arguments : Vec<_> = getFlattenedBindingIterator()
        .map(|descriptorBinding| {
            format!("{}.view", &descriptorBinding.name)
        })
        .collect();

        format!("{{
    auto [xBlockCount, yBlockCount] = workgroupGeometry;

    auto descriptorSets = CreateDescriptorSets(descriptorSetPool, {});

    std::vector<uint32_t> dynamicOffsets;
    (*commandBuffer).bindDescriptorSets(vk::PipelineBindPoint::eCompute, *pipelineLayout, 0, descriptorSets, dynamicOffsets);
    (*commandBuffer).bindPipeline(vk::PipelineBindPoint::eCompute, *pipeline);
    (*commandBuffer).dispatch(xBlockCount, yBlockCount, 1);
  }}
", arguments.join(","))
    };
    headerContent.push_str(&body);
    headerContent
}

pub fn build_header(shader_name: &str, module: SPV::CleanSpvReflectShaderModule) -> String
{
    let mut headerContent = String::new();
    headerContent.push_str(&format!("struct {} \n", shader_name));
    headerContent.push_str("{\n");

    // Member
    headerContent.push_str("  static const std::vector<uint32_t> bytecode;\n");
    headerContent.push_str("  vk::Device dev;\n");    
    headerContent.push_str("  vk::UniqueShaderModule shaderModule;\n");
    headerContent.push_str(&format!("  std::array<vk::UniqueDescriptorSetLayout, {}> descriptorSetLayout;\n", module.descriptor_sets.len()));
    headerContent.push_str("  vk::UniquePipelineLayout pipelineLayout;\n");
    headerContent.push_str("  vk::UniquePipeline pipeline;\n");

    headerContent.push_str(&generate_buffer_type_declaration(&module));

    headerContent.push_str(&generate_constructor_code(shader_name, &module));

    headerContent.push_str(&build_operator(&module));

    headerContent.push_str("};\n");

    headerContent
}