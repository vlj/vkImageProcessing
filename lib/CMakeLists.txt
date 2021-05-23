add_subdirectory(Base)

add_subdirectory(ShaderCollection)

add_subdirectory(ImageProcessing)

if (${GLFW3_VIEWER})
add_subdirectory(WindowingSystem)
endif()