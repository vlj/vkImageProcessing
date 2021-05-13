find_package(Microsoft.GSL CONFIG REQUIRED)

find_package(Vulkan REQUIRED)

add_library(Base
    include/v2api.h
    include/Base_impl.h
    include/Context.h
    include/GPUCommandAsync.hpp
    src/Base.cpp
    src/Context.cpp
)
target_link_libraries(Base PUBLIC Vulkan::Vulkan Microsoft.GSL::GSL)
target_compile_features(Base PRIVATE cxx_std_17)
target_compile_definitions(Base PUBLIC VK_USE_PLATFORM_WIN32_KHR)

target_include_directories(Base PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)