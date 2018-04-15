#!/bin/bash

mkdir -p bin/GCMZDrops bin/GCMZDrops/dropper

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/GCMZDrops.txt

# update version string
VERSION='v0.3.4'
GITHASH=`git rev-parse --short HEAD`
cat << EOS | sed 's/\r$//' | sed 's/$/\r/' > 'src/lazarus/ver.pas'
unit Ver;

{\$mode objfpc}{\$H+}
{\$CODEPAGE UTF-8}

interface

const
  Version = '$VERSION ( $GITHASH )';

implementation

end.
EOS

# copy script files
sed 's/\r$//' 'src/lua/_entrypoint.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/_entrypoint.lua'
sed 's/\r$//' 'src/lua/example.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/example.lua'
sed 's/\r$//' 'src/lua/generic.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/generic.lua'
sed 's/\r$//' 'src/lua/textsjis.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/textsjis.lua'
sed 's/\r$//' 'src/lua/avoiddup.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/avoiddup.lua'
sed 's/\r$//' 'src/lua/wmvmask.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/wmvmask.lua'
sed 's/\r$//' 'src/lua/dropper/example.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/dropper/example.lua'
sed 's/\r$//' 'src/lua/dropper/clipboard.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/dropper/clipboard.lua'

# build lazarus project
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/GCMZDrops.lpi

# install
# mkdir -p aviutl/GCMZDrops aviutl/GCMZDrops/dropper
# cp bin/*.auf aviutl/
# cp bin/GCMZDrops/* aviutl/GCMZDrops/
# cp bin/GCMZDrops/dropper/* aviutl/GCMZDrops/dropper/
