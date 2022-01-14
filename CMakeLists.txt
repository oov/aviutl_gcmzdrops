cmake_minimum_required(VERSION 3.13)
project(aviutl_gcmzdrops C)
enable_language(RC)
enable_testing()

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB sources "${PROJECT_SOURCE_DIR}/src/c/*.c" "${PROJECT_SOURCE_DIR}/src/c/*.h")
add_custom_target(${PROJECT_NAME}-format ALL
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

add_subdirectory(src/c)
