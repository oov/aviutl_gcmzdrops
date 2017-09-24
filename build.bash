#!/bin/bash

mkdir bin bin/GCMZDrops

# copy readme
sed 's/\r$//' README.md | sed 's/$/\r/' > bin/README.txt

# copy script files
sed 's/\r$//' 'src/lua/_entrypoint.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/_entrypoint.lua'
sed 's/\r$//' 'src/lua/example.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/example.lua'
sed 's/\r$//' 'src/lua/generic.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/generic.lua'
sed 's/\r$//' 'src/lua/textsjis.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/textsjis.lua'
sed 's/\r$//' 'src/lua/avoiddup.lua' | sed 's/$/\r/' > 'bin/GCMZDrops/avoiddup.lua'

# build lazarus project
cmd.exe /c C:/lazarus/lazbuild.exe --build-all src/lazarus/GCMZDrops.lpi

# install
# mkdir aviutl/GCMZDrops
# cp bin/*.auf aviutl/
# cp bin/GCMZDrops/* aviutl/GCMZDrops/
