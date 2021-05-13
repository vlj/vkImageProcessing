add_library(VkImageProcessing
    include/GuidedFilterHelpers.h
    include/IntegralImageHelpers.h
    "include/v2utils.h"
    src/highlevelhelpers.cpp
)

target_link_libraries(VkImageProcessing PUBLIC Base ShaderCollection opencv_core opencv_imgcodecs opencv_highgui)
target_compile_features(VkImageProcessing PRIVATE cxx_std_17)

target_include_directories(VkImageProcessing PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)