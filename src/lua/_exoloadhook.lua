local P = {}

local map = require("_transmap").new()
local iniobj = require("_iniobj")

function P.on_load(filename, is_exa)
  local ini = iniobj.newfile(filename)
  local oini = iniobj.new("")
  for i, sect in ipairs(ini:sections()) do
    local name = ini:get(sect, "_name", nil)
    if name ~= nil then
      local tr = map[name]
      for j, key in ipairs(ini:keys(sect)) do
        oini:set(sect, tr[key], tr[ini:get(sect, key, nil)])
      end
    else
      for j, key in ipairs(ini:keys(sect)) do
        oini:set(sect, key, ini:get(sect, key, nil))
      end
    end
  end
  local newfile = GCMZDrops.createtempfile("translated", is_exa and ".exa" or ".exo")
  f, err = io.open(newfile, "wb")
  if f == nil then
    error(err)
  end
  f:write(tostring(oini))
  f:close()
  return newfile
end

return P
