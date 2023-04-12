#!/bin/bash
SCRIPT_DIR=$(cd $(dirname ${0}); pwd)

# find application name from csv
APP_NAME="Noname"
while IFS=, read -a cols; do
  if [[ "${cols[0]}" == "#"* ]]; then
    continue
  fi
  if [[ "${cols[0]}" != "" ]] && [[ "${cols[1]}" == "" ]]; then
    APP_NAME="${cols[0]}"
    break
  fi
done < "${SCRIPT_DIR}/langs.csv"

# generate *.pot
POTFILE="${SCRIPT_DIR}/current.pot"
xgettext \
  --add-comments="trans:" \
  --from-code="UTF-8" \
  --package-name="${APP_NAME}" \
  --package-version="$(git tag --points-at HEAD | grep . || echo "vX.X.X") ( $(git rev-parse --short HEAD | grep . || echo "unknown") ) " \
  --copyright-holder="${APP_NAME} Developers" \
  --no-wrap --sort-by-file --output - ../c/*.c > "${POTFILE}"
if [ $? -ne 0 ]; then
  echo "failed to generate pot."
  exit 1
fi

# generate *.po
while IFS=, read -a cols; do
  if [[ "${cols[0]}" == "#"* ]] || [[ "${cols[1]}" == "" ]]; then
    continue
  fi
  poname="${cols[0]}.po"
  po="${SCRIPT_DIR}/${poname}"
  if [ -e "${po}" ]; then
    echo "already exists: ${poname}"
    continue
  fi
  porepo="${po}.DO_NOT_EDIT"
  if [ -e "${porepo}" ]; then
    msgmerge --no-wrap --output - "${porepo}" "${POTFILE}" > ${po}
  else
    msginit --no-translator --input="${POTFILE}" --no-wrap --locale=${cols[0]}.UTF-8 --output-file=- > ${po} 2> /dev/null
  fi
  if [ $? -ne 0 ]; then
    echo "failed to generate: ${poname}"
    continue
  fi
  echo "generated: ${poname}"
done < "${SCRIPT_DIR}/langs.csv"
