#pragma once

#include <cstdint>
#include <vector>

#include <Base.hpp>

namespace Shaders {

struct CopyShader {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct TestPerBlockHorizontalPrefixSum {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct TestTranspose {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct TestFullBlockSum {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct TestPartialVerticalPrefixSumInSharedMemory {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct TestVerticalPrefixSumInSharedMemory {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct TestAverage {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct IntegralImage {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct NonWavefrontTest {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct VerticalSumation {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct HorizontalSumation {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct IntegralImagePass2 {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct GuidedFilterPass1 {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct GuidedFilterPass2 {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};


struct AveragingShader {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct MeanAAndBPass1 {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct MeanAAndBPass2 {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

struct GuidedFilterFinalPass {
  static std::pair<uint16_t, uint16_t> GetBlockGeometry();
};

} // namespace Shaders

namespace v2 {
#include "generated/meanAandBPass1_h_spv.h"
#include "generated/meanAandBPass2_h_spv.h"
#include "generated/guidedFilterFinal_h_spv.h"
#include "generated/horizontalSumation_h_spv.h"
#include "generated/verticalSumation_h_spv.h"
#include "generated/guidedFilterPass1_h_spv.h"
#include "generated/guidedFilterPass2_h_spv.h"
#include "generated/averagingISqI_h_spv.h"
#include "generated/copy_h_spv.h"
#include "generated/test_average_h_spv.h"
}