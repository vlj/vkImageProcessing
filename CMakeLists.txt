cmake_minimum_required(VERSION 3.16)

project(VkImageProcessing)


option(GLFW3_VIEWER "Build Image Viewer" ON)


find_package(OpenCV CONFIG REQUIRED)

if (${GLFW3_VIEWER})
find_package(glfw3 CONFIG REQUIRED)
endif()
find_package(Vulkan REQUIRED)

add_subdirectory(lib)


if (${GLFW3_VIEWER})
add_executable(ImageView main.cpp  "lib/WindowingSystem/include/WindowingSystem.hpp" "lib/WindowingSystem/src/WindowingSystem.cpp")
target_include_directories(ImageView PRIVATE ${CMAKE_BINARY_DIR})
target_compile_features(ImageView PRIVATE cxx_std_17)
target_link_libraries(ImageView PRIVATE glfw ShaderCollection VkImageProcessing WindowingSystem)
endif()
add_subdirectory(tests)