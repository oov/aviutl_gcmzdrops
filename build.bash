#!/usr/bin/env bash
set -eu

CUR_DIR="${PWD}"
cd "$(dirname "${BASH_SOURCE:-$0}")"

INSTALL_TOOLS=1
REBUILD=0
SKIP_TESTS=0
CREATE_ZIP=0
CMAKE_BUILD_TYPE=Release
GENERATE_TRANSMAP=0
ADDITIONAL_CMAKE_OPTIONS=""
FORMAT_SOURCES=ON

while [[ $# -gt 0 ]]; do
  case $1 in
    --no-install-tools)
      INSTALL_TOOLS=0
      shift
      ;;
    -d|--debug)
      CMAKE_BUILD_TYPE=Debug
      shift
      ;;
    --no-format)
      FORMAT_SOURCES=OFF
      shift
      ;;
    -r|--rebuild)
      REBUILD=1
      shift
      ;;
    -s|--skip-tests)
      SKIP_TESTS=1
      shift
      ;;
    -z|--zip)
      CREATE_ZIP=1
      shift
      ;;
    --generate-transmap)
      GENERATE_TRANSMAP=1
      shift
      ;;
    -*|--*)
      echo "Unknown option $1"
      exit 1
      ;;
    *)
      shift
      ;;
  esac
done

if [ "${INSTALL_TOOLS}" -eq 1 ]; then
  mkdir -p "build/tools"
  . "src/c/3rd/ovbase/setup-llvm-mingw.sh" --dir "${PWD}/build/tools"
fi

TARGETS=ALL
if [ "${GENERATE_TRANSMAP}" -eq 1 ]; then
  TARGETS=generate_transmap
fi

ARCHDIR=${ARCHDIR:-i686}
destdir="${PWD}/build/${CMAKE_BUILD_TYPE}/${ARCHDIR}"
CMAKE_INSTALL_PREFIX=${CMAKE_INSTALL_PREFIX:-"${destdir}/local"}
CMAKE_C_COMPILER=${CMAKE_C_COMPILER:-i686-w64-mingw32-clang}
CMAKE_TOOL_CHANIN_FILE=${CMAKE_TOOL_CHANIN_FILE:-"src/c/3rd/ovbase/cmake/llvm-mingw.cmake"}

if [ "${REBUILD}" -eq 1 ] || [ ! -e "${destdir}/CMakeCache.txt" ]; then
  rm -rf "${destdir}"
  cmake -S . -B "${destdir}" --preset debug \
    -DCMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE}" \
    -DFORMAT_SOURCES="${FORMAT_SOURCES}" \
    -DCMAKE_INSTALL_PREFIX="${CMAKE_INSTALL_PREFIX}" \
    -DCMAKE_C_COMPILER="${CMAKE_C_COMPILER}" \
    -DCMAKE_TOOLCHAIN_FILE="${CMAKE_TOOL_CHANIN_FILE}"
fi
if [ "${GENERATE_TRANSMAP}" -eq 1 ]; then
  cmake --build "${destdir}" --target "${TARGETS}"
  exit
fi
cmake --build "${destdir}"
if [ "${SKIP_TESTS}" -eq 0 ]; then
  ctest --test-dir "${destdir}/src/c" --output-on-failure --output-junit testlog.xml
fi

if [ "${REBUILD}" -eq 1 ]; then
  rm -rf "${destdir}/../bin"
fi
mkdir -p "${destdir}/../bin"
cp -r "${destdir}/bin/"* "${destdir}/../bin"

if [ "${CREATE_ZIP}" -eq 1 ]; then
  rm -rf "${destdir}/../dist"
  mkdir -p "${destdir}/../dist"
  cd "${destdir}/../bin"
  cmake -E tar cf "${destdir}/../dist/${CMAKE_BUILD_TYPE}.zip" --format=zip .
fi

cd "${CUR_DIR}"
