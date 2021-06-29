find_package(Vulkan REQUIRED)


set(Vulkan_GLSLC_EXECUTABLE ${Vulkan_INCLUDE_DIRS}/../Bin/glslc.exe)
macro(compile_shader SRC DST)
    add_custom_command(OUTPUT ${DST}
                       COMMAND ${Vulkan_GLSLC_EXECUTABLE}
                       ARGS -fshader-stage=compute -mfmt=c ${SRC} -o ${DST} -I ${CMAKE_CURRENT_SOURCE_DIR}/include/shaders/ --target-spv=spv1.3
                       COMMAND ${Vulkan_GLSLC_EXECUTABLE}
                       ARGS -fshader-stage=compute ${SRC} -o ${DST}.spv -I ${CMAKE_CURRENT_SOURCE_DIR}/include/shaders/ --target-spv=spv1.3
                       DEPENDS ${SRC} ${CMAKE_CURRENT_SOURCE_DIR}/include/shaders/common.h ${CMAKE_CURRENT_SOURCE_DIR}/include/shaders/prefixSum.h ${CMAKE_CURRENT_SOURCE_DIR}/include/shaders/sweepPhases.h)

endmacro()

compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/copy.glsl ${CMAKE_CURRENT_BINARY_DIR}/copy.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_HorizontalBlockSum.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_HorizontalBlockSum.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_transpose.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_transpose.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_FullBlockSum.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_FullBlockSum.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_average.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_average.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/integralImage.glsl ${CMAKE_CURRENT_BINARY_DIR}/integralImage.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_partialVerticalPrefixSumInSharedMemory.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_partialVerticalPrefixSumInSharedMemory.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_verticalPrefixSumInSharedMemory.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_verticalPrefixSumInSharedMemory.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_nonWavefront.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_nonWavefront.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/verticalSumation.glsl ${CMAKE_CURRENT_BINARY_DIR}/verticalSumation.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/horizontalSumation.glsl ${CMAKE_CURRENT_BINARY_DIR}/horizontalSumation.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_integralImagePass2.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_integralImagePass2.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/guidedfilterPass1.glsl ${CMAKE_CURRENT_BINARY_DIR}/guidedfilterPass1.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/guidedfilterPass2.glsl ${CMAKE_CURRENT_BINARY_DIR}/guidedfilterPass2.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/averagingISqI.glsl ${CMAKE_CURRENT_BINARY_DIR}/averagingISqI.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/meanAandBPass1.glsl ${CMAKE_CURRENT_BINARY_DIR}/meanAandBPass1.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/meanAandBPass2.glsl ${CMAKE_CURRENT_BINARY_DIR}/meanAandBPass2.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/guidedFilterFinal.glsl ${CMAKE_CURRENT_BINARY_DIR}/guidedFilterFinal.h)
compile_shader(${CMAKE_CURRENT_SOURCE_DIR}/src/shaders/test_pushconstant.glsl ${CMAKE_CURRENT_BINARY_DIR}/test_pushconstant.h)

add_library(ShaderCollection STATIC
    include/ShadersCollection.h
    src/ShadersCollection.cpp

    ${CMAKE_CURRENT_BINARY_DIR}/copy.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_HorizontalBlockSum.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_transpose.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_FullBlockSum.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_average.h
    ${CMAKE_CURRENT_BINARY_DIR}/integralImage.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_partialVerticalPrefixSumInSharedMemory.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_verticalPrefixSumInSharedMemory.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_nonWavefront.h
    ${CMAKE_CURRENT_BINARY_DIR}/verticalSumation.h
    ${CMAKE_CURRENT_BINARY_DIR}/horizontalSumation.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_integralImagePass2.h
    ${CMAKE_CURRENT_BINARY_DIR}/guidedfilterPass1.h
    ${CMAKE_CURRENT_BINARY_DIR}/guidedfilterPass2.h
    ${CMAKE_CURRENT_BINARY_DIR}/averagingISqI.h
    ${CMAKE_CURRENT_BINARY_DIR}/meanAandBPass1.h
    ${CMAKE_CURRENT_BINARY_DIR}/meanAandBPass2.h
    ${CMAKE_CURRENT_BINARY_DIR}/guidedFilterFinal.h
    ${CMAKE_CURRENT_BINARY_DIR}/ssbo_filter.h
    ${CMAKE_CURRENT_BINARY_DIR}/test_pushconstant.h
    
    "include/generated/test_average_h_spv.cpp"
    "include/generated/verticalSumation_h_spv.cpp"
    "include/generated/horizontalSumation_h_spv.cpp"
    "include/generated/guidedFilterPass1_h_spv.cpp"
    "include/generated/guidedFilterFinal_h_spv.cpp"
    "include/generated/copy_h_spv.cpp"
    "include/generated/averagingISqI_h_spv.cpp"
    "include/generated/guidedFilterPass2_h_spv.cpp"
    "include/generated/meanAandBPass1_h_spv.cpp"
    "include/generated/meanAandBPass2_h_spv.cpp"
    "include/generated/test_pushconstant_h_spv.cpp"
     )
target_include_directories(ShaderCollection PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_include_directories(ShaderCollection PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_link_libraries(ShaderCollection PRIVATE Base)
target_compile_features(ShaderCollection PRIVATE cxx_std_17)

