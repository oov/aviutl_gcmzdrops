cmake_minimum_required(VERSION 3.0.0)

find_program(GENDEF gendef REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
find_program(DLLTOOL dlltool REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)
find_program(LUA51 lua51.dll REQUIRED CMAKE_FIND_ROOT_PATH_BOTH)

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
