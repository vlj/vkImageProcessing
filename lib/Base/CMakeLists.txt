find_package(Microsoft.GSL CONFIG REQUIRED)

find_package(Vulkan REQUIRED)

find_package(glm REQUIRED)

add_library(Base STATIC
    include/Base.hpp
    include/Base_impl.hpp
    include/Context.hpp
    include/GPUCommandAsync.hpp
    src/Base.cpp
    src/Context.cpp
)
target_link_libraries(Base PUBLIC Vulkan::Vulkan Microsoft.GSL::GSL glm::glm)
target_compile_features(Base PRIVATE cxx_std_17)
target_compile_definitions(Base PUBLIC VK_USE_PLATFORM_WIN32_KHR NOMINMAX)

target_include_directories(Base PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)