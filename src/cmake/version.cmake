# Based on
# https://bravenewmethod.com/2017/07/14/git-revision-as-compiler-definition-in-build-with-cmake/
# https://github.com/tikonen/blog/tree/master/cmake/git_version
cmake_minimum_required(VERSION 3.0.0)

message(STATUS "Resolving git version")
set(_version "${version}")
set(_git_revision "unknown")
find_package(Git)
if(GIT_FOUND)
  execute_process(
    COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
    WORKING_DIRECTORY "${local_dir}"
    OUTPUT_VARIABLE _git_revision
    ERROR_QUIET
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  message(STATUS "git hash: ${_git_revision}")
else()
  message(STATUS "git not found")
endif()

configure_file(${input_file} ${output_file} @ONLY)
