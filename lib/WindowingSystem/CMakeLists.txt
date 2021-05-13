add_library(WindowingSystem
    include/WindowingSystem.hpp
    src/WindowingSystem.cpp
)

target_link_libraries(WindowingSystem PUBLIC Base glfw)
target_include_directories(WindowingSystem PUBLIC include)
target_compile_features(WindowingSystem PRIVATE cxx_std_17)