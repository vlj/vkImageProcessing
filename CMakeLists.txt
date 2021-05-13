cmake_minimum_required(VERSION 3.16)

project(VkImageProcessing)


find_package(OpenCV CONFIG REQUIRED)
find_package(glfw3 CONFIG REQUIRED)
find_package(Vulkan REQUIRED)

add_subdirectory(lib)

add_executable(ImageView main.cpp )
target_include_directories(ImageView PRIVATE ${CMAKE_BINARY_DIR})
target_compile_features(ImageView PRIVATE cxx_std_17)
target_link_libraries(ImageView PRIVATE glfw ShaderCollection VkImageProcessing)

add_subdirectory(tests)