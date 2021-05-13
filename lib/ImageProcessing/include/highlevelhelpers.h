#pragma once

#include <list>
#include <memory>
#include <vector>
#include <vulkan/vulkan.hpp>
#include <vulkan/vulkan_win32.h>

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

#include <opencv2/core.hpp>
#include <opencv2/highgui.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>

#include <Context.hpp>
#include <Base.hpp>

#include <ShadersCollection.h>

namespace v2 {
  template<typename T>
  inline WorkgroupGeometry WorkgroupFromDomainGrid(size_t cols, size_t rows) {
    auto [blockX, blockY] = T::GetBlockGeometry();
    auto xBlockCount = (cols + blockX - 1) / blockX;
    auto yBlockCount = (rows + blockY - 1) / blockY;
    return {xBlockCount, yBlockCount};
  }

  template <typename T> inline WorkgroupGeometry LaunchScanLtR(size_t cols, size_t rows) {
    auto [blockX, blockY] = T::GetBlockGeometry();
    auto lineOfThread = std::max(blockX, blockY);
    auto yBlockCount = (rows + lineOfThread - 1) / lineOfThread;
    return {yBlockCount, 1};
  }

  template <typename T> inline WorkgroupGeometry LaunchScanTtB(size_t cols, size_t rows) {
    auto [blockX, blockY] = T::GetBlockGeometry();
    auto lineOfThread = std::max(blockX, blockY);
    auto xBlockCount = (rows + lineOfThread - 1) / lineOfThread;
    return {xBlockCount, 1};
  }

} // namespace v2 