
find_package(Catch2 CONFIG REQUIRED)

add_executable(basic basicTest.cpp)
target_link_libraries(basic PRIVATE Catch2::Catch2 ShaderCollection VkImageProcessing)
target_compile_features(basic PRIVATE cxx_std_17)