#include <ShadersCollection.h>
#include <vulkan/vulkan.hpp>


constexpr auto kSubgroupSize = 16;

namespace Shaders {


std::pair<uint16_t, uint16_t> CopyShader::GetBlockGeometry() { return {32, 32}; }


std::pair<uint16_t, uint16_t>
TestPerBlockHorizontalPrefixSum::GetBlockGeometry() {
  return {32, 32};
}

std::pair<uint16_t, uint16_t> TestTranspose::GetBlockGeometry() {
  return {32, 32};
}

std::pair<uint16_t, uint16_t> TestFullBlockSum::GetBlockGeometry() {
  return {32, 32};
}

std::pair<uint16_t, uint16_t>
TestPartialVerticalPrefixSumInSharedMemory::GetBlockGeometry() {
  return {32, 32};
}

std::pair<uint16_t, uint16_t>
TestVerticalPrefixSumInSharedMemory::GetBlockGeometry() {
  return {32, 32};
}

std::pair<uint16_t, uint16_t> TestAverage::GetBlockGeometry() { return {16, 16}; }

std::pair<uint16_t, uint16_t> IntegralImage::GetBlockGeometry() {
  return {32, 32};
}

std::pair<uint16_t, uint16_t> NonWavefrontTest::GetBlockGeometry() {
  return {kSubgroupSize, 32};
}

std::pair<uint16_t, uint16_t> VerticalSumation::GetBlockGeometry() { return {32, 32}; }


std::pair<uint16_t, uint16_t> HorizontalSumation::GetBlockGeometry() { return {32, 32}; }


std::pair<uint16_t, uint16_t> IntegralImagePass2::GetBlockGeometry() { return {16, 32}; }

std::pair<uint16_t, uint16_t> GuidedFilterPass1::GetBlockGeometry() { return {kSubgroupSize, kSubgroupSize}; }


std::pair<uint16_t, uint16_t> GuidedFilterPass2::GetBlockGeometry() { return {kSubgroupSize, kSubgroupSize}; }

std::pair<uint16_t, uint16_t> AveragingShader::GetBlockGeometry() { return {32, 16}; }

std::pair<uint16_t, uint16_t> MeanAAndBPass1::GetBlockGeometry() { return {kSubgroupSize, kSubgroupSize}; }

std::pair<uint16_t, uint16_t> MeanAAndBPass2::GetBlockGeometry() { return {kSubgroupSize, kSubgroupSize}; }

std::pair<uint16_t, uint16_t> GuidedFilterFinalPass::GetBlockGeometry() { return {16, 32}; }

} // namespace Shaders