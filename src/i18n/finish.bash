#!/bin/bash
SCRIPT_DIR=$(cd $(dirname ${0}); pwd)

# generate files
while IFS=, read -a cols; do
  if [[ "${cols[0]}" == "#"* ]] || [[ "${cols[1]}" == "" ]]; then
    continue
  fi
  # update *.po.DO_NOT_EDIT
  po="${SCRIPT_DIR}/${cols[0]}.po"
  porepo="${po}.DO_NOT_EDIT"
  if [ -e "${po}" ]; then
    newpo=$(cat "${po}" | msgcat --no-wrap --no-location --sort-output - | sed -r '/^"(Project-Id-Version|POT-Creation-Date|PO-Revision-Date|X-Generator): /d')
    if [ $? -ne 0 ]; then
      echo "failed to generate: ${porepo}"
      continue
    fi
    echo "${newpo}" > "${porepo}"
    echo "generated: ${cols[0]}.po.DO_NOT_EDIT"
  fi
  # generate *.mo
  if [ -e "${porepo}" ]; then
    mo="${SCRIPT_DIR}/${cols[0]}.mo"
    msgfmt --no-hash --output-file "${mo}" "${porepo}"
    if [ $? -ne 0 ]; then
      echo "failed to generate: ${cols[0]}.mo"
      continue
    fi
    echo "generated: ${cols[0]}.mo"
  fi
done < "${SCRIPT_DIR}/langs.csv"

if [ "${1}" == "" ]; then
  exit 0
fi

if ! file "${1}" | grep 'PE32+\?\sexecutable.*for MS Windows' > /dev/null; then
  echo "this file cannot be modified: ${1}"
  exit 1
fi

# install minirh
FILETYPE=$(file $(which file))
MINIRH_URL_BASE="https://github.com/oov/minirh/releases/download/v0.1.0/minirh-v0.1.0"
target=""
if [[ $FILETYPE == *"PE32"* ]]; then
  if [[ $FILETYPE == *"Intel 80386"* ]]; then
    # PE32 executable (console) Intel 80386
    target="windows-386"
  elif [[ $FILETYPE == *"x86-64"* ]]; then
    # PE32+ executable (console) x86-64
    target="windows-amd64"
  elif [[ $FILETYPE == *"Aarch64"* ]]; then
    # PE32+ executable (console) Aarch64
    target="windows-arm64"
  fi
elif [[ $FILETYPE == *"ELF"* ]]; then
  if [[ $FILETYPE == *"Intel 80386"* ]]; then
    # ELF 32-bit LSB executable, Intel 80386
    target="linux-386"
  elif [[ $FILETYPE == *"x86-64"* ]]; then
    # ELF 64-bit LSB executable, x86-64
    target="linux-amd64"
  elif [[ $FILETYPE == *"ARM aarch64"* ]]; then
    # ELF 64-bit LSB executable, ARM aarch64
    target="linux-arm64"
  fi
elif [[ $FILETYPE == *"Mach-O"* ]]; then
  if [[ $FILETYPE == *"x86_64"* ]]; then
    # Mach-O 64-bit x86_64 executable
    target="darwin-amd64"
  elif [[ $FILETYPE == *"arm64"* ]]; then
    # Mach-O 64-bit arm64 executable
    target="darwin-arm64"
  fi
fi
if [[ "${target}" == "" ]]; then
  echo "failed to detect running environment."
  exit 1
fi

mimirh=""
url=""
if [[ "${target}" == *"windows"* ]]; then
  minirh="${SCRIPT_DIR}/.tool/minirh.exe"
else
  minirh="${SCRIPT_DIR}/.tool/minirh"
fi

if [ ! -e "${minirh}" ]; then
  echo "Downloading mimirh..."
  mkdir -p "${SCRIPT_DIR}/.tool"
  if [[ "${target}" == *"windows"* ]]; then
    url="${MINIRH_URL_BASE}-${target}.zip"
    archive="${SCRIPT_DIR}/.tool/minirh.zip"
  else
    url="${MINIRH_URL_BASE}-${target}.tar.gz"
    archive="${SCRIPT_DIR}/.tool/minirh.tar.gz"
  fi
  echo "Download: ${url}"
  curl -Lfso "${archive}" "${url}"
  if [ $? -ne 0 ]; then
    echo "failed to download from ${url}"
    exit 1
  fi
  echo "Extracting..."
  if [[ "${target}" == *"windows"* ]]; then
    unzip -o -d "${SCRIPT_DIR}/.tool" "${archive}"
  else
    tar zxf "${archive}" -C "${SCRIPT_DIR}/.tool"
  fi
fi

# update binary
echo "modifying...: ${1}"
while IFS=, read -a cols; do
  if [[ "${cols[0]}" == "#"* ]] || [[ "${cols[1]}" == "" ]]; then
    continue
  fi
  mo="${SCRIPT_DIR}/${cols[0]}.mo"
  lang="${cols[1]}"
  if [ -e "${mo}" ]; then
    $minirh -in "${1}" -out "${1}" -bin "${mo}" -type "RT_RCDATA" -res "MO" -lang "${lang}"
    if [ $? -ne 0 ]; then
      echo "failed to update resource."
      exit 1
    else
      echo "updated: RT_RCDATA MO ${lang} ${cols[0]}.mo"
    fi
  fi
done < "${SCRIPT_DIR}/langs.csv"
