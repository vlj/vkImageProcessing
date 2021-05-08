#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate clap;

#[macro_use]
extern crate bitflags;

use clap::{Arg, App, SubCommand};


use std::io;
use std::fs::File;
use std::io::prelude::*;


mod SPV;



fn generate_constructor_code(shader_name: &str, module: &SPV::CleanSpvReflectShaderModule) -> String
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
            .setDescriptorType(vk::DescriptorType::eStorageImage)
            .setStageFlags(vk::ShaderStageFlagBits::eCompute);
            bindings.push_back(binding);
        }}
", binding.binding)
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

    headerContent.push_str("  }\n");
    headerContent
}

fn build_operator(module: &SPV::CleanSpvReflectShaderModule) -> String
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
        let arguments: Vec<_> = getFlattenedBindingIterator()
            .map(|descriptorBinding| {
                format!("    DecoratedState<vk::ImageLayout::eGeneral> &&{}", &descriptorBinding.name)
            })
            .collect();
        format!("
  [[nodiscard]]
  auto operator()(
    WorkgroupGeometry workgroupGeometry,
    StartedCommandBuffer& commandBuffer,
    vk::DescriptorPool descriptorSetPool,
{}
  )
", arguments.join(",\n"))
    };

    headerContent.push_str(&declaration);

    let body = {
        let arguments : Vec<_> = getFlattenedBindingIterator()
        .map(|descriptorBinding| {
            format!("*{}.tex->view", &descriptorBinding.name)
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

fn build_header(shader_name: &str, module: SPV::CleanSpvReflectShaderModule) -> String
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

    headerContent.push_str(&generate_constructor_code(shader_name, &module));

    headerContent.push_str(&build_operator(&module));

    headerContent.push_str("};\n");

    headerContent
}

fn main() -> io::Result<()> {
    let matches = App::new("shader2headers")
                          .arg(Arg::with_name("input")
                               .long("input")
                               .required(true)
                               .value_name("FILE")
                               .takes_value(true))
                          .arg(Arg::with_name("output_folder")
                                .value_name("FILE")
                               .required(true)
                               .long("output_folder"))
                          .get_matches();

    let filename = match matches.value_of("input") {
        Some(v) => v,
        None => return Err(io::Error::from(io::ErrorKind::InvalidData)),
    };

    
    let output_folder = match matches.value_of("output_folder") {
        Some(v) => v,
        None => return Err(io::Error::from(io::ErrorKind::InvalidData)),
    };

    const shader_name : &str = "guidedFilterFinal";
    //const filename : &str = "C:\\Users\\vljno\\OneDrive\\Bureau\\VkClearView\\out\\build\\x64-Debug (default)\\lib\\ShaderCollection\\meanAandBPass2.h.spv";
    let mut f = File::open(filename)?;

    let mut buf = Vec::new();
    f.read_to_end(&mut buf)?;

    let properModule = SPV::spvReflectCreateShaderModule(buf).expect("failed to do the conversion");


    println!("{}", build_header(shader_name, properModule));


    Ok(())
}