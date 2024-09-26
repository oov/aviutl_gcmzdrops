option(USE_COMPILER_RT "use compiler-rt runtime" OFF)
option(FORMAT_SOURCES "execute clang-format" ON)
enable_testing()
enable_language(RC)

if(FORMAT_SOURCES)
  file(GLOB_RECURSE sources LIST_DIRECTORIES false CONFIGURE_DEPENDS "*.h" "*.c")
  list(FILTER sources EXCLUDE REGEX "${CMAKE_CURRENT_SOURCE_DIR}/3rd")
  find_program(CLANG_FORMAT_EXE clang-format)
  add_custom_target(${PROJECT_NAME}-format ALL
    COMMAND ${CLANG_FORMAT_EXE} -style=file -i ${sources}
  )
endif()

set(LUA51_PLATFORM i686)
set(LUA51_URL "https://github.com/oov/lua-5.1.5/releases/download/v5.1.5/lua_v5.1.5_${LUA51_PLATFORM}.zip")
string(REGEX MATCH "[^/]+$" LUA51_ARCHIVE_NAME "${LUA51_URL}")
set(LUA51_ARCHIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}/${LUA51_ARCHIVE_NAME}")
set(LUA51_DIR "${CMAKE_CURRENT_BINARY_DIR}/lua51")
set(LUA51_DLL "${LUA51_DIR}/bin/lua51.dll")
set(LUA51_INCLUDE "${LUA51_DIR}/include")
if(NOT EXISTS "${LUA51_DIR}")
  if(NOT EXISTS "${LUA51_ARCHIVE_PATH}")
    file(DOWNLOAD "${LUA51_URL}" "${LUA51_ARCHIVE_PATH}")
  endif()
  string(REGEX REPLACE "\\.[^.]+$" "" LUA51_ARCHIVE_NOEXT "${LUA51_ARCHIVE_NAME}")
  file(MAKE_DIRECTORY "${LUA51_DIR}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E tar xf ${LUA51_ARCHIVE_PATH}
    WORKING_DIRECTORY "${LUA51_DIR}"
  )
  execute_process(
    COMMAND ${CMAKE_COMMAND} -E copy "${LUA51_DLL}" "${CMAKE_CURRENT_BINARY_DIR}/lua51.dll"
  )
endif()

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

add_custom_target(copy_related_files
  COMMAND ${CMAKE_COMMAND} -E copy_directory "${PROJECT_SOURCE_DIR}/src/lua/" "${CMAKE_BINARY_DIR}/bin/GCMZDrops/"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/util/_transmap1.lua" "${CMAKE_BINARY_DIR}/bin/GCMZDrops/_transmap1.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/util/_transmap2.lua" "${CMAKE_BINARY_DIR}/bin/GCMZDrops/_transmap2.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/README.md" "${CMAKE_BINARY_DIR}/bin/GCMZDrops.txt"
)

set(is_clang "$<C_COMPILER_ID:Clang>")
set(v16_or_later "$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,16>")
set(v18_or_later "$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,18>")
set(v19_or_later "$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,19>")
add_library(gcmzdrops_intf INTERFACE)
target_include_directories(gcmzdrops_intf INTERFACE
  "${CMAKE_CURRENT_BINARY_DIR}" # for version.h
  "${jansson_BINARY_DIR}/include"
  "${LUA51_INCLUDE}"
)
target_compile_definitions(gcmzdrops_intf INTERFACE
  _WIN32_WINNT=0x0501
  _WINDOWS
  $<$<CONFIG:Release>:NDEBUG>
)
target_compile_options(gcmzdrops_intf INTERFACE
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
  $<$<AND:${is_clang},${v18_or_later}>:-Wno-switch-default>
  $<$<AND:${is_clang},${v19_or_later}>:-Wno-pre-c11-compat>
  -ffunction-sections
  -fdata-sections
  $<$<CONFIG:Debug>:-O0>
  $<$<CONFIG:Release>:-O2>
  -flto
)
target_link_options(gcmzdrops_intf INTERFACE
  -fuse-ld=lld
  -Wl,--gc-sections
  # -Wl,--print-gc-sections
  --rtlib=compiler-rt
  -static
  -Wl,--kill-at
  $<$<CONFIG:Release>:-s>
)
target_link_libraries(gcmzdrops_intf INTERFACE
  comctl32
  shlwapi
  ovbase
  ovutil
  jansson
  "${LUA51_DLL}"
)

add_library(gcmzdrops_test_intf INTERFACE)
target_compile_definitions(gcmzdrops_test_intf INTERFACE
  TEST_IMAGE_DIR=${CMAKE_CURRENT_SOURCE_DIR}/test
)

add_library(crc64 STATIC 3rd/crc64/crc64.c)
target_link_libraries(crc64 PRIVATE gcmzdrops_intf)
add_library(detect STATIC 3rd/detect/detect.c)
target_link_libraries(detect PRIVATE gcmzdrops_intf)

add_executable(gentransmap EXCLUDE_FROM_ALL error_gcmz.c task.c luafuncs_convertencoding.c luautil.c util/gentransmap.c)
target_link_libraries(gentransmap PRIVATE gcmzdrops_intf)
set_target_properties(gentransmap PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/util"
)
add_dependencies(gentransmap generate_version_h)

add_custom_target(generate_transmap
  COMMAND ${CMAKE_COMMAND} -E copy "${PROJECT_SOURCE_DIR}/src/lua/_iniobj.lua" "_iniobj.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_SOURCE_DIR}/util/gentransmap.lua" "gentransmap.lua"
  COMMAND ${CMAKE_COMMAND} -E copy "${LUA51_DLL}" "./"
  COMMAND
    "gentransmap"
    "${CMAKE_CURRENT_SOURCE_DIR}/util/0.exo" 932
    "${CMAKE_CURRENT_SOURCE_DIR}/util/1.exo" 932
    "${CMAKE_CURRENT_SOURCE_DIR}/util/_transmap1.lua"
  COMMAND
    "gentransmap"
    "${CMAKE_CURRENT_SOURCE_DIR}/util/0.exo" 932
    "${CMAKE_CURRENT_SOURCE_DIR}/util/2.exo" 936
    "${CMAKE_CURRENT_SOURCE_DIR}/util/_transmap2.lua"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/util"
)
add_dependencies(generate_transmap gentransmap)

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
target_link_options(gcmzdrops_auf PRIVATE
  -Wl,-delayload,lua51.dll
)
target_link_libraries(gcmzdrops_auf PRIVATE
  gcmzdrops_intf
  crc64
  detect
)
add_dependencies(gcmzdrops_auf generate_version_h generate_rc copy_related_files)

add_executable(datauri_test error_gcmz.c task.c sniffer.c datauri_test.c)
target_link_libraries(datauri_test PRIVATE gcmzdrops_intf crc64 detect gcmzdrops_test_intf)
add_dependencies(datauri_test generate_version_h)
add_test(NAME datauri_test COMMAND datauri_test)

add_executable(gcmzfuncs_test error_gcmz.c task.c gcmzfuncs_test.c)
target_link_libraries(gcmzfuncs_test PRIVATE gcmzdrops_intf gcmzdrops_test_intf)
add_dependencies(gcmzfuncs_test generate_version_h)
add_test(NAME gcmzfuncs_test COMMAND gcmzfuncs_test)

add_executable(luafuncs_test error_gcmz.c task.c files.c luautil.c luafuncs_test.c)
target_link_libraries(luafuncs_test PRIVATE gcmzdrops_intf crc64 detect gcmzdrops_test_intf)
add_dependencies(luafuncs_test generate_version_h)
add_test(NAME luafuncs_test COMMAND luafuncs_test)
