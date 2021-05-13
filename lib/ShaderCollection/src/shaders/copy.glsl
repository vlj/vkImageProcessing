#version 450

layout(local_size_x = 16, local_size_y = 16) in;
layout(binding = 0, r32f) uniform readonly image2D inputImage;
layout(binding = 1, rgba8) uniform image2D resultImage;

void main()
{
	vec4 inputRGBA = imageLoad(inputImage, ivec2(gl_GlobalInvocationID.x, gl_GlobalInvocationID.y));
	imageStore(resultImage, ivec2(gl_GlobalInvocationID.xy), inputRGBA.xxxx);
}