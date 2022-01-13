option(JANSSON_WITHOUT_TESTS "" ON)
option(JANSSON_BUILD_DOCS "" OFF)
set(CMAKE_REQUIRED_LINK_OPTIONS -no-pthread)
add_subdirectory(3rd/jansson)

add_custom_target(generate_version_h COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${PROJECT_SOURCE_DIR}/src/cmake/version.cmake"
)

find_program(LUA51DLL lua51.dll REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
add_custom_target(generate_lua51_delayed COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_BINARY_DIR}"
  -Ddll="${LUA51DLL}"
  -Ddef="${CMAKE_CURRENT_BINARY_DIR}/lua51.def"
  -Dlib="${CMAKE_CURRENT_BINARY_DIR}/liblua51_dynamic.a"
  -Ddelayed_lib="${CMAKE_CURRENT_BINARY_DIR}/liblua51_delayed.a"
  -P "${PROJECT_SOURCE_DIR}/src/cmake/makelib.cmake"
)

find_program(CLANG_FORMAT_EXE clang-format)
file(GLOB sources "*.c" "*.h")
add_custom_target(${PROJECT_NAME}-format ALL
  COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
)

add_subdirectory(3rd/ovbase)
add_subdirectory(3rd/ovutil)

include_directories(
  "${ovbase_SOURCE_DIR}/include"
  "${ovutil_SOURCE_DIR}/include"
  "${CMAKE_CURRENT_BINARY_DIR}/3rd/jansson/include"
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
)
link_directories(
  "${CMAKE_CURRENT_BINARY_DIR}/3rd/jansson/lib"
  "${CMAKE_CURRENT_BINARY_DIR}" # for liblua51_delayed.a
)
add_compile_definitions(
  __STDC_NO_THREADS__
  _WIN32_WINNT=0x0502
  _WINDOWS
  USE_STR
  USE_WSTR
  $<$<BOOL:${LEAK_DETECTOR}>:LEAK_DETECTOR>
  $<$<BOOL:${ALLOCATE_LOGGER}>:ALLOCATE_LOGGER>
  $<$<CONFIG:Release>:NDEBUG>
)
add_compile_options(
  --rtlib=compiler-rt
  -mstackrealign
  -Wall
  -Wextra
  -Werror
  -Weverything
  -Wshadow
  -Werror=return-type
  -pedantic-errors
  -ffunction-sections
  -fdata-sections
  $<$<CONFIG:Debug>:-O0>
  $<$<CONFIG:Release>:-O2>
)
add_link_options(
  -Wl,--gc-sections
  # -Wl,--print-gc-sections
  --rtlib=compiler-rt
  -no-pthread
  -static
  -Wl,--kill-at
  $<$<CONFIG:Release>:-s>
)

add_library(crc64 STATIC 3rd/crc64/crc64.c)
add_library(detect STATIC 3rd/detect/detect.c)

link_libraries(
  comctl32
  crc64
  detect
  jansson
  lua51_delayed
  ovbase
  ovutil
  shlwapi
)
add_library(gcmzdrops_auf SHARED
  api.c
  aviutl.c
  datauri.c
  droptarget.c
  error_gcmz.c
  files.c
  gcmz.rc
  gcmzdrops.c
  gcmzfuncs.c
  gui.c
  lua.c
  luafuncs.c
  main.c
  scpopup.c
  sniffer.c
  task.c
)
set_target_properties(gcmzdrops_auf PROPERTIES
  OUTPUT_NAME "GCMZDrops.auf"
  PREFIX ""
  SUFFIX ""
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)
add_dependencies(gcmzdrops_auf generate_version_h generate_lua51_delayed)

file(COPY "${PROJECT_SOURCE_DIR}/src/lua/" DESTINATION "${CMAKE_BINARY_DIR}/bin/GCMZDrops/")
file(GENERATE OUTPUT "${CMAKE_BINARY_DIR}/bin/GCMZDrops.txt" INPUT "${PROJECT_SOURCE_DIR}/README.md")

add_executable(test_datauri error_gcmz.c sniffer.c datauri_test.c)
list(APPEND tests test_datauri)
add_executable(test_gcmzfuncs error_gcmz.c task.c gcmzfuncs_test.c)
list(APPEND tests test_gcmzfuncs)
add_executable(test_luafuncs error_gcmz.c files.c luafuncs_test.c)
list(APPEND tests test_luafuncs)

foreach(target ${tests})
  add_test(NAME ${target} COMMAND ${target})
  target_compile_definitions(${target} PRIVATE
    TEST_IMAGE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/test
  )
endforeach(target)
