extern crate std;

pub mod raw {
    #![allow(dead_code)]
    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

pub enum SourceLanguage {
    LanguageUnknown,
    ESSL,
    GLSL,
    OpenCL_C,
    OpenCL_CPP,
    HLSL,
}

pub enum ExecutionModel {
    Vertex,
    TessellationControl,
    TessellationEvaluation,
    Geometry,
    Fragment,
    GLCompute,
    Kernel,
    TaskNV,
    MeshNV,
    RayGenerationKHR,
    IntersectionKHR,
    AnyHitKHR,
    ClosestHitKHR,
    MissKHR,
    CallableKHR,
}

#[derive(Copy, Clone)]
pub enum DescriptorType {
    SAMPLER,
    COMBINED_IMAGE_SAMPLER,
    SAMPLED_IMAGE,
    STORAGE_IMAGE,
    UNIFORM_TEXEL_BUFFER,
    STORAGE_TEXEL_BUFFER,
    UNIFORM_BUFFER,
    STORAGE_BUFFER,
    UNIFORM_BUFFER_DYNAMIC,
    STORAGE_BUFFER_DYNAMIC,
    INPUT_ATTACHMENT,
    ACCELERATION_STRUCTURE_KHR,
}

#[derive(Copy, Clone)]
pub enum SpvImageFormat {
    Unknown,
    Rgba32f,
    Rgba16f,
    R32f,
    Rgba8,
    Rgba8Snorm,
    Rg32f,
    Rg16f,
    R11fG11fB10f,
    R16f,
    Rgba16,
    Rgb10A2,
    Rg16,
    Rg8,
    R16,
    R8,
    Rgba16Snorm,
    Rg16Snorm,
    Rg8Snorm,
    R16Snorm,
    R8Snorm,
    Rgba32i,
    Rgba16i,
    Rgba8i,
    R32i,
    Rg32i,
    Rg16i,
    Rg8i,
    R16i,
    R8i,
    Rgba32ui,
    Rgba16ui,
    Rgba8ui,
    R32ui,
    Rgb10a2ui,
    Rg32ui,
    Rg16ui,
    Rg8ui,
    R16ui,
    R8ui,
    R64ui,
    R64i,
}

pub struct SpvReflectImageTraits {
    pub input_attachment_index: u32,
    pub dim: raw::SpvDim,
    pub depth: u32,
    pub arrayed: u32,
    pub ms: u32,
    pub sampled: u32,
    pub image_format: SpvImageFormat,
}

pub struct SpvReflectBlockVariable {
    pub spirv_id: u32,
    pub name: String,
    pub offset: u32,
    pub absolute_offset: u32,
    pub size: u32,
    pub padded_size: u32,
    // pub decoration_flags: SpvReflectDecorationFlags,
    // pub numeric: SpvReflectNumericTraits,
    // pub array: SpvReflectArrayTraits,
    // pub flags: SpvReflectVariableFlags,
    // pub member_count: u32,
    pub members: Vec<SpvReflectBlockVariable>,
    pub type_description: SpvReflectTypeDescription,
}

pub enum DescriptorBindingContent {
    Image(SpvReflectImageTraits),
    Block(SpvReflectBlockVariable),
    Array(raw::SpvReflectBindingArrayTraits),
    None,
}

#[derive(Clone)]
pub struct SpvReflectTypeDescription {
    pub id: u32,
//    pub op: SpvOp,
    pub type_name: String,
    pub struct_member_name: String,
  //  pub storage_class: SpvStorageClass,
  //  pub type_flags: SpvReflectTypeFlags,
  //  pub decoration_flags: SpvReflectDecorationFlags,
  //  pub traits: SpvReflectTypeDescription_Traits,
    //pub member_count: u32,
    pub members: Vec<SpvReflectTypeDescription>,
}

pub struct SpvReflectDescriptorBinding {
    pub spirv_id: u32,
    pub name: String,
    pub binding: u32,
    pub set: u32,
    pub descriptor_type: DescriptorType,
    // pub resource_type: raw::SpvReflectResourceType,
    pub content: DescriptorBindingContent,
    pub count: u32,
    pub accessed: u32,
    // pub uav_counter_id: u32,
    // pub uav_counter_binding: *mut raw::SpvReflectDescriptorBinding,
    pub type_description: SpvReflectTypeDescription,
    //pub word_offset: SpvReflectDescriptorBinding__bindgen_ty_1,
}

pub struct SpvReflectDescriptorSet {
    pub set: u32,
    pub descriptor_bindings: Vec<SpvReflectDescriptorBinding>,
}

bitflags! {
    pub struct ShaderStageBits: i32
    {
    const Vertex = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_VERTEX_BIT;
    const TesselationControl = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_TESSELLATION_CONTROL_BIT;
    const TesselationEvaluation = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_TESSELLATION_EVALUATION_BIT;
    const Geometry = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_GEOMETRY_BIT;
    const Fragment = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_FRAGMENT_BIT;
    const Compute = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_COMPUTE_BIT;
    const Task = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_TASK_BIT_NV;
    const Mesh = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_MESH_BIT_NV;
    const Raygen = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_RAYGEN_BIT_KHR;
    const AnyHit = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_ANY_HIT_BIT_KHR;
    const ClosestHit = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_CLOSEST_HIT_BIT_KHR;
    const Miss = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_MISS_BIT_KHR;
    const Intersection = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_INTERSECTION_BIT_KHR;
    const Callable = raw::SpvReflectShaderStageFlagBits_SPV_REFLECT_SHADER_STAGE_CALLABLE_BIT_KHR;
    }
}

pub struct CleanSpvReflectShaderModule {
    //pub generator: SpvReflectGenerator,
    pub entry_point_name: String,
    pub entry_point_id: u32,
    pub entry_points: Vec<raw::SpvReflectEntryPoint>,
    pub source_language: SourceLanguage,
    pub source_language_version: u32,
    // pub source_file: *const ::std::os::raw::c_char,
    // pub source_source: *const ::std::os::raw::c_char,
    pub spirv_execution_model: ExecutionModel,
    pub shader_stage: ShaderStageBits,
    pub descriptor_sets: Vec<SpvReflectDescriptorSet>,
    pub input_variables: Vec<*mut raw::SpvReflectInterfaceVariable>,
    pub output_variables: Vec<*mut raw::SpvReflectInterfaceVariable>,
    pub interface_variables: Vec<raw::SpvReflectInterfaceVariable>,
    pub push_constant_blocks: Vec<raw::SpvReflectBlockVariable>,
    // pub _internal: *mut SpvReflectShaderModule_Internal,
}

fn toVec<a: Copy>(pointer: *mut a, count: isize) -> Vec<a> {
    (0..count).map(|n| unsafe { *pointer.offset(n) }).collect()
}

fn get_source_language(source_language: raw::SpvSourceLanguage_) -> SourceLanguage {
    match source_language {
        raw::SpvSourceLanguage__SpvSourceLanguageGLSL => SourceLanguage::GLSL,
        raw::SpvSourceLanguage__SpvSourceLanguageHLSL => SourceLanguage::HLSL,
        raw::SpvSourceLanguage__SpvSourceLanguageESSL => SourceLanguage::ESSL,
        raw::SpvSourceLanguage__SpvSourceLanguageOpenCL_C => SourceLanguage::OpenCL_C,
        raw::SpvSourceLanguage__SpvSourceLanguageOpenCL_CPP => SourceLanguage::OpenCL_CPP,
        _ => SourceLanguage::LanguageUnknown,
    }
}

fn get_execution_model(execution_model: raw::SpvExecutionModel_) -> ExecutionModel {
    match execution_model {
        raw::SpvExecutionModel__SpvExecutionModelVertex => ExecutionModel::Vertex,
        raw::SpvExecutionModel__SpvExecutionModelTessellationControl => {
            ExecutionModel::TessellationControl
        }
        raw::SpvExecutionModel__SpvExecutionModelTessellationEvaluation => {
            ExecutionModel::TessellationEvaluation
        }
        raw::SpvExecutionModel__SpvExecutionModelGeometry => ExecutionModel::Geometry,
        raw::SpvExecutionModel__SpvExecutionModelFragment => ExecutionModel::Fragment,
        raw::SpvExecutionModel__SpvExecutionModelGLCompute => ExecutionModel::GLCompute,
        raw::SpvExecutionModel__SpvExecutionModelKernel => ExecutionModel::Kernel,
        raw::SpvExecutionModel__SpvExecutionModelTaskNV => ExecutionModel::TaskNV,
        raw::SpvExecutionModel__SpvExecutionModelMeshNV => ExecutionModel::MeshNV,
        raw::SpvExecutionModel__SpvExecutionModelRayGenerationKHR => {
            ExecutionModel::RayGenerationKHR
        }
        //            SpvExecutionModel__SpvExecutionModelRayGenerationNV => ExecutionModel::RayGenerationNV,
        raw::SpvExecutionModel__SpvExecutionModelIntersectionKHR => ExecutionModel::IntersectionKHR,
        //            SpvExecutionModel__SpvExecutionModelIntersectionNV => ExecutionModel::IntersectionNV,
        raw::SpvExecutionModel__SpvExecutionModelAnyHitKHR => ExecutionModel::AnyHitKHR,
        //            SpvExecutionModel__SpvExecutionModelAnyHitNV => ExecutionModel::AnyHitNV,
        raw::SpvExecutionModel__SpvExecutionModelClosestHitKHR => ExecutionModel::ClosestHitKHR,
        //            SpvExecutionModel__SpvExecutionModelClosestHitNV => ExecutionModel::ClosestHitNV,
        raw::SpvExecutionModel__SpvExecutionModelMissKHR => ExecutionModel::MissKHR,
        //           SpvExecutionModel__SpvExecutionModelMissNV => ExecutionModel::MissNV,
        raw::SpvExecutionModel__SpvExecutionModelCallableKHR => ExecutionModel::CallableKHR,
        //           SpvExecutionModel__SpvExecutionModelCallableNV => ExecutionModel::CallableNV,
        _ => panic!("Unknow value"),
    }
}

fn get_image_format(format: raw::SpvImageFormat) -> SpvImageFormat {
    match format {
        raw::SpvImageFormat__SpvImageFormatUnknown => SpvImageFormat::Unknown,
        raw::SpvImageFormat__SpvImageFormatRgba32f => SpvImageFormat::Rgba32f,
        raw::SpvImageFormat__SpvImageFormatRgba16f => SpvImageFormat::Rgba16f,
        raw::SpvImageFormat__SpvImageFormatR32f => SpvImageFormat::R32f,
        raw::SpvImageFormat__SpvImageFormatRgba8 => SpvImageFormat::Rgba8,
        raw::SpvImageFormat__SpvImageFormatRgba8Snorm => SpvImageFormat::Rgba8Snorm,
        raw::SpvImageFormat__SpvImageFormatRg32f => SpvImageFormat::Rg32f,
        raw::SpvImageFormat__SpvImageFormatRg16f => SpvImageFormat::Rg16f,
        raw::SpvImageFormat__SpvImageFormatR11fG11fB10f => SpvImageFormat::R11fG11fB10f,
        raw::SpvImageFormat__SpvImageFormatR16f => SpvImageFormat::R16f,
        raw::SpvImageFormat__SpvImageFormatRgba16 => SpvImageFormat::Rgba16,
        raw::SpvImageFormat__SpvImageFormatRgb10A2 => SpvImageFormat::Rgb10A2,
        raw::SpvImageFormat__SpvImageFormatRg16 => SpvImageFormat::Rg16,
        raw::SpvImageFormat__SpvImageFormatRg8 => SpvImageFormat::Rg8,
        raw::SpvImageFormat__SpvImageFormatR16 => SpvImageFormat::R16,
        raw::SpvImageFormat__SpvImageFormatR8 => SpvImageFormat::R8,
        raw::SpvImageFormat__SpvImageFormatRgba16Snorm => SpvImageFormat::Rgba16Snorm,
        raw::SpvImageFormat__SpvImageFormatRg16Snorm => SpvImageFormat::Rg16Snorm,
        raw::SpvImageFormat__SpvImageFormatRg8Snorm => SpvImageFormat::Rg8Snorm,
        raw::SpvImageFormat__SpvImageFormatR16Snorm => SpvImageFormat::R16Snorm,
        raw::SpvImageFormat__SpvImageFormatR8Snorm => SpvImageFormat::R8Snorm,
        raw::SpvImageFormat__SpvImageFormatRgba32i => SpvImageFormat::Rgba32i,
        raw::SpvImageFormat__SpvImageFormatRgba16i => SpvImageFormat::Rgba16i,
        raw::SpvImageFormat__SpvImageFormatRgba8i => SpvImageFormat::Rgba8i,
        raw::SpvImageFormat__SpvImageFormatR32i => SpvImageFormat::R32i,
        raw::SpvImageFormat__SpvImageFormatRg32i => SpvImageFormat::Rg32i,
        raw::SpvImageFormat__SpvImageFormatRg16i => SpvImageFormat::Rg16i,
        raw::SpvImageFormat__SpvImageFormatRg8i => SpvImageFormat::Rg8i,
        raw::SpvImageFormat__SpvImageFormatR16i => SpvImageFormat::R16i,
        raw::SpvImageFormat__SpvImageFormatR8i => SpvImageFormat::R8i,
        raw::SpvImageFormat__SpvImageFormatRgba32ui => SpvImageFormat::Rgba32ui,
        raw::SpvImageFormat__SpvImageFormatRgba16ui => SpvImageFormat::Rgba16ui,
        raw::SpvImageFormat__SpvImageFormatRgba8ui => SpvImageFormat::Rgba8ui,
        raw::SpvImageFormat__SpvImageFormatR32ui => SpvImageFormat::R32ui,
        raw::SpvImageFormat__SpvImageFormatRgb10a2ui => SpvImageFormat::Rgb10a2ui,
        raw::SpvImageFormat__SpvImageFormatRg32ui => SpvImageFormat::Rg32ui,
        raw::SpvImageFormat__SpvImageFormatRg16ui => SpvImageFormat::Rg16ui,
        raw::SpvImageFormat__SpvImageFormatRg8ui => SpvImageFormat::Rg8ui,
        raw::SpvImageFormat__SpvImageFormatR16ui => SpvImageFormat::R16ui,
        raw::SpvImageFormat__SpvImageFormatR8ui => SpvImageFormat::R8ui,
        raw::SpvImageFormat__SpvImageFormatR64ui => SpvImageFormat::R64ui,
        raw::SpvImageFormat__SpvImageFormatR64i => SpvImageFormat::R64i,
        _ => SpvImageFormat::Unknown,
    }
}


fn ConvertStr(string: *const ::std::os::raw::c_char) -> String {
    unsafe {
        if string.is_null() {
            String::new()
        }
        else {
            std::ffi::CStr::from_ptr(string)
                .to_str()
                .unwrap()
                .to_string()
        }
    }
}


pub trait Convertable {
    type Converted;

    fn Convert(&self) -> Self::Converted;
}

impl Convertable for raw::SpvReflectTypeDescription {
    type Converted = SpvReflectTypeDescription;

    fn Convert(&self) -> SpvReflectTypeDescription
    {
            SpvReflectTypeDescription{
                id: self.id,
                type_name: ConvertStr(self.type_name),
                struct_member_name: ConvertStr(self.struct_member_name),
                members : (0..self.member_count).map(|offset| {
                        let member = unsafe { *self.members.offset(offset as isize) };
                        member.Convert()
                    }).collect()
            }
    }
}

impl Convertable for raw::SpvReflectBlockVariable {
    type Converted = SpvReflectBlockVariable;

    fn Convert(&self) -> SpvReflectBlockVariable 
    {
        let members : Vec<_> =
        (0..self.member_count).map(|offset| {
            unsafe {
                let child = &*self.members.offset(offset as isize);
                child.Convert()
            }
        })
        .collect();

        SpvReflectBlockVariable {
            spirv_id: self.spirv_id,
            name: unsafe {
                std::ffi::CStr::from_ptr(self.name)
                .to_str()
                .unwrap()
                .to_string()},
            offset: self.offset,
            absolute_offset: self.absolute_offset,
            size: self.size,
            padded_size: self.padded_size,
            members: members,
            type_description: unsafe {(*self.type_description).Convert() }
        }
    }
}

impl Convertable for raw::SpvReflectDescriptorBinding {
    type Converted = SpvReflectDescriptorBinding;

    fn Convert(&self) -> SpvReflectDescriptorBinding {
        let name = unsafe {
            std::ffi::CStr::from_ptr(self.name)
                .to_str()
                .unwrap()
                .to_string()
        };
        let tmp = self.descriptor_type;
        let descriptor_type = match tmp {
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLER => {
                DescriptorType::SAMPLER
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER => {
                DescriptorType::COMBINED_IMAGE_SAMPLER
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_SAMPLED_IMAGE => {
                DescriptorType::SAMPLED_IMAGE
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_IMAGE => {
                DescriptorType::STORAGE_IMAGE
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER => {
                DescriptorType::UNIFORM_TEXEL_BUFFER
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER => {
                DescriptorType::STORAGE_TEXEL_BUFFER
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER => {
                DescriptorType::UNIFORM_BUFFER
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER => {
                DescriptorType::STORAGE_BUFFER
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC => {
                DescriptorType::UNIFORM_BUFFER_DYNAMIC
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC => {
                DescriptorType::STORAGE_BUFFER_DYNAMIC
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_INPUT_ATTACHMENT => {
                DescriptorType::INPUT_ATTACHMENT
            }
            raw::SpvReflectDescriptorType_SPV_REFLECT_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR => {
                DescriptorType::ACCELERATION_STRUCTURE_KHR
            }
            _ => panic!("unknow value"),
        };

        let content = match descriptor_type {
            DescriptorType::SAMPLED_IMAGE
            | DescriptorType::COMBINED_IMAGE_SAMPLER
            | DescriptorType::STORAGE_IMAGE => DescriptorBindingContent::Image(SpvReflectImageTraits {
                input_attachment_index: self.input_attachment_index,
                arrayed: self.image.arrayed,
                ms: self.image.ms,
                depth: self.image.depth,
                dim: self.image.dim,
                image_format: get_image_format(self.image.image_format),
                sampled: self.image.sampled,
            }),
            | DescriptorType::UNIFORM_BUFFER => DescriptorBindingContent::Block((&self.block).Convert()),
            _ => DescriptorBindingContent::None,
        };

        SpvReflectDescriptorBinding {
            spirv_id: self.spirv_id,
            name: name,
            binding: self.binding,
            set: self.set,
            descriptor_type: descriptor_type,
            content: content,
            count: self.count,
            accessed: self.accessed,
            type_description: unsafe {(*self.type_description).Convert() },
        }
    }
}

fn HigherLevelSpvReflectShaderModule(
    module: raw::SpvReflectShaderModule,
) -> CleanSpvReflectShaderModule {
    let entry_point_name = unsafe {
        std::ffi::CStr::from_ptr(module.entry_point_name)
            .to_str()
            .unwrap()
            .to_string()
    };

    let descriptor_set = {
        module
            .descriptor_sets
            .iter()
            .take(module.descriptor_set_count as usize)
            .map(|rawDescriptorSet| {
                let descriptor_bindings = {
                    (0..rawDescriptorSet.binding_count)
                        .map(|offset| unsafe {
                            let elem = rawDescriptorSet.bindings.offset(offset as isize);
                            (&**elem).Convert()
                        })
                        .collect()
                };
                SpvReflectDescriptorSet {
                    set: rawDescriptorSet.set,
                    descriptor_bindings: descriptor_bindings,
                }
            })
            .collect()
    };

    let result = CleanSpvReflectShaderModule {
        entry_point_name: entry_point_name,
        entry_point_id: module.entry_point_id,
        source_language_version: module.source_language_version,
        spirv_execution_model: get_execution_model(module.spirv_execution_model),
        shader_stage: ShaderStageBits {
            bits: module.shader_stage,
        },
        source_language: get_source_language(module.source_language),
        entry_points: toVec(module.entry_points, module.entry_point_count as isize),
        push_constant_blocks: toVec(
            module.push_constant_blocks,
            module.push_constant_block_count as isize,
        ),
        interface_variables: toVec(
            module.interface_variables,
            module.interface_variable_count as isize,
        ),
        input_variables: toVec(module.input_variables, module.input_variable_count as isize),
        output_variables: toVec(
            module.output_variables,
            module.output_variable_count as isize,
        ),
        descriptor_sets: descriptor_set,
    };

    result
}

pub fn spvReflectCreateShaderModule(mut buf: Vec<u8>) -> Result<CleanSpvReflectShaderModule, ()> {
    let module = unsafe {
        let mut module = std::mem::MaybeUninit::uninit();
        let result = raw::spvReflectCreateShaderModule(
            buf.len() as u64,
            buf.as_mut_ptr() as *const std::ffi::c_void,
            module.as_mut_ptr(),
        );
        if result != 0 {
            return Err(());
        };
        module.assume_init()
    };

    Ok(HigherLevelSpvReflectShaderModule(module))
}
