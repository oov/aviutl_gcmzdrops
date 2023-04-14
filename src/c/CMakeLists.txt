option(USE_COMPILER_RT "use compiler-rt runtime" OFF)
add_compile_options(-flto)
add_link_options(-fuse-ld=lld)

add_subdirectory(3rd/ovbase)
add_subdirectory(3rd/ovutil)

option(JANSSON_WITHOUT_TESTS "" ON)
option(JANSSON_BUILD_DOCS "" OFF)
# set(CMAKE_REQUIRED_LINK_OPTIONS -no-pthread)
add_subdirectory(3rd/jansson)

add_custom_target(generate_version_h COMMAND
  ${CMAKE_COMMAND}
  -Dlocal_dir="${PROJECT_SOURCE_DIR}"
  -Dinput_file="${CMAKE_CURRENT_SOURCE_DIR}/version.h.in"
  -Doutput_file="${CMAKE_CURRENT_BINARY_DIR}/version.h"
  -P "${ovutil_SOURCE_DIR}/src/cmake/version.cmake"
)

# generate i18n.rc
set(LANGCSV "${CMAKE_CURRENT_SOURCE_DIR}/../i18n/langs.csv")
file(READ "${LANGCSV}" langs)
string(STRIP ${langs} langs)
string(REPLACE "\n" ";" langs "${langs}")
foreach(line IN LISTS langs)
  if (line MATCHES "^#.*$|^([^,]+),$")
    continue()
  endif()
  if (line MATCHES "^([^,]+),([^,]+)$")
    list(APPEND polist "${CMAKE_CURRENT_SOURCE_DIR}/../i18n/${CMAKE_MATCH_1}.po.DO_NOT_EDIT")
  else()
    message(FATAL_ERROR "invalid language difinition: ${line}")
  endif()
endforeach()
add_custom_command(
  OUTPUT
    "${CMAKE_CURRENT_BINARY_DIR}/i18n.rc"
  COMMAND
    ${CMAKE_COMMAND}
    -Doutput_dir="${CMAKE_CURRENT_BINARY_DIR}"
    -Drctmpl="${CMAKE_CURRENT_SOURCE_DIR}/i18n.rc.tmpl"
    -P "${CMAKE_CURRENT_SOURCE_DIR}/i18n_rc.cmake"
  WORKING_DIRECTORY
    "${CMAKE_CURRENT_SOURCE_DIR}/../i18n"
  DEPENDS
    "${CMAKE_CURRENT_SOURCE_DIR}/i18n.rc.tmpl"
    ${polist}
)
add_custom_target(generate_rc DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/i18n.rc")

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

add_executable(gentransmap error_gcmz.c task.c luafuncs_convertencoding.c luautil.c util/gentransmap.c)
set_target_properties(gentransmap PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/util"
)
list(APPEND targets gentransmap)
add_dependencies(gentransmap generate_version_h)
add_custom_target(generate_transmap
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/_iniobj.lua" "_iniobj.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/util/gentransmap.lua" "gentransmap.lua"
  COMMAND
    "gentransmap"
    "${CMAKE_CURRENT_SOURCE_DIR}/util/0.exo" 932
    "${CMAKE_CURRENT_SOURCE_DIR}/util/1.exo" 932
    "${CMAKE_BINARY_DIR}/bin/GCMZDrops/_transmap1.lua"
  COMMAND
    "gentransmap"
    "${CMAKE_CURRENT_SOURCE_DIR}/util/0.exo" 932
    "${CMAKE_CURRENT_SOURCE_DIR}/util/2.exo" 936
    "${CMAKE_BINARY_DIR}/bin/GCMZDrops/_transmap2.lua"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/util"
)
add_dependencies(generate_transmap gentransmap copy_related_files)

add_library(gcmzdrops_auf SHARED
  api.c
  aviutl.c
  datauri.c
  droptarget.c
  error_gcmz.c
  exoloadhook.c
  files.c
  gcmz.rc
  gcmzdrops.c
  gcmzfuncs.c
  gui.c
  i18n.rc
  lua.c
  luafuncs.c
  luafuncs_convertencoding.c
  luautil.c
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
add_dependencies(gcmzdrops_auf generate_version_h generate_rc generate_lua51_importlib copy_related_files generate_transmap)
list(APPEND targets gcmzdrops_auf)
target_link_options(gcmzdrops_auf PRIVATE
  -Wl,-delayload,lua51.dll
)

add_executable(datauri_test error_gcmz.c task.c sniffer.c datauri_test.c)
add_dependencies(datauri_test generate_version_h)
list(APPEND targets datauri_test)
add_executable(gcmzfuncs_test error_gcmz.c task.c gcmzfuncs_test.c)
add_dependencies(gcmzfuncs_test generate_version_h)
list(APPEND targets gcmzfuncs_test)
add_executable(luafuncs_test error_gcmz.c task.c files.c luautil.c luafuncs_test.c)
add_dependencies(luafuncs_test generate_version_h)
list(APPEND targets luafuncs_test)

set(is_clang "$<C_COMPILER_ID:Clang>")
set(v16_or_later "$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,16>")
foreach(target ${targets})
  if(target MATCHES "_test$")
    add_test(NAME ${target} COMMAND ${target})
    target_compile_definitions(${target} PRIVATE
      TEST_IMAGE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/test
    )
  endif()
  target_include_directories(${target} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
    "${jansson_BINARY_DIR}/include"
  )
  target_link_directories(${target} PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}" # for liblua51_delayed.a
  )
  target_compile_definitions(${target} PRIVATE
    _WIN32_WINNT=0x0501
    _WINDOWS
    $<$<CONFIG:Release>:NDEBUG>
  )
  target_compile_options(${target} PRIVATE
  $<$<AND:$<BOOL:${WIN32}>,$<BOOL:${USE_COMPILER_RT}>>:--rtlib=compiler-rt>
    -mstackrealign
    -Wall
    -Wextra
    -Werror
    -Weverything
    -Wshadow
    -Werror=return-type
    -pedantic-errors
    -Wno-declaration-after-statement
    -Wno-padded
    $<$<AND:${is_clang},${v16_or_later}>:-Wno-unsafe-buffer-usage>
    -ffunction-sections
    -fdata-sections
    $<$<CONFIG:Debug>:-O0>
    $<$<CONFIG:Release>:-O2>
    -flto
  )
  target_link_options(${target} PRIVATE
    -fuse-ld=lld
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
