cmake_minimum_required(VERSION 3.0.0)

find_program(GENDEF gendef CMAKE_FIND_ROOT_PATH_BOTH)
if(NOT GENDEF)
  message(FATAL_ERROR "gendef not found")
endif()

find_program(DLLTOOL dlltool CMAKE_FIND_ROOT_PATH_BOTH)
if(NOT DLLTOOL)
  message(FATAL_ERROR "dlltool not found")
endif()

find_program(LUA51 lua51.dll CMAKE_FIND_ROOT_PATH_BOTH)
if(NOT LUA51)
  message(FATAL_ERROR "lua51.dll not found")
endif()

# Generate lua51.def
execute_process(
  COMMAND ${GENDEF} ${LUA51}
  WORKING_DIRECTORY "${local_dir}"
  ERROR_QUIET
)

execute_process(
  COMMAND ${DLLTOOL} -d lua51.def -y liblua51dl.a
  WORKING_DIRECTORY "${local_dir}"
  OUTPUT_VARIABLE _git_revision
  ERROR_QUIET
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
