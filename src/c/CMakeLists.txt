add_subdirectory(3rd/ovbase)
add_subdirectory(3rd/ovutil)

option(JANSSON_WITHOUT_TESTS "" ON)
option(JANSSON_BUILD_DOCS "" OFF)
set(CMAKE_REQUIRED_LINK_OPTIONS -no-pthread)
add_subdirectory(3rd/jansson)

add_custom_target(generate_version_h COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${PROJECT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${ovutil_SOURCE_DIR}/src/cmake/version.cmake"
)

find_program(LUA51DLL lua51.dll REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
add_custom_target(generate_lua51_importlib COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${CMAKE_CURRENT_BINARY_DIR}"
  -Ddll="${LUA51DLL}"
  -Ddef="${CMAKE_CURRENT_BINARY_DIR}/lua51.def"
  -Dlib="${CMAKE_CURRENT_BINARY_DIR}/liblua51.a"
  -P "${ovutil_SOURCE_DIR}/src/cmake/makelib-llvm.cmake"
)

add_custom_target(copy_related_files
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_SOURCE_DIR}/src/lua/" "${CMAKE_BINARY_DIR}/bin/GCMZDrops/"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/README.md" "${CMAKE_BINARY_DIR}/bin/GCMZDrops.txt"
)

add_library(crc64 STATIC 3rd/crc64/crc64.c)
list(APPEND targets crc64)
add_library(detect STATIC 3rd/detect/detect.c)
list(APPEND targets detect)

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
add_dependencies(gcmzdrops_auf generate_version_h generate_lua51_importlib copy_related_files)
list(APPEND targets gcmzdrops_auf)

add_executable(test_datauri error_gcmz.c sniffer.c datauri_test.c)
list(APPEND targets test_datauri)
add_executable(test_gcmzfuncs error_gcmz.c task.c gcmzfuncs_test.c)
list(APPEND targets test_gcmzfuncs)
add_executable(test_luafuncs error_gcmz.c files.c luafuncs_test.c)
list(APPEND targets test_luafuncs)

foreach(target ${targets})
  if(target MATCHES "^test_")
    add_test(NAME ${target} COMMAND ${target})
    target_compile_definitions(${target} PRIVATE
      TEST_IMAGE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/test
    )
  endif()
  target_include_directories(${target} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
  )
  target_link_directories(${target} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}" # for liblua51_delayed.a
  )
  target_compile_definitions(${target} PRIVATE
    _WIN32_WINNT=0x0502
    _WINDOWS
    $<$<CONFIG:Release>:NDEBUG>
  )
  target_compile_options(${target} PRIVATE
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
    -flto
  )
  target_link_options(${target} PRIVATE
    -fuse-ld=lld
    -Wl,-delayload,lua51.dll
    -Wl,--gc-sections
    # -Wl,--print-gc-sections
    --rtlib=compiler-rt
    -no-pthread
    -static
    -Wl,--kill-at
    $<$<CONFIG:Release>:-s>
  )
  target_link_libraries(${target} PRIVATE
    comctl32
    $<$<NOT:$<STREQUAL:${target},crc64>>:crc64>
    $<$<NOT:$<STREQUAL:${target},detect>>:detect>
    jansson
    lua51
    ovbase
    ovutil
    shlwapi
  )
endforeach(target)
