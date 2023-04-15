local P = {}

local function readfile(path)
  local f, err = io.open(path, "rb")
  if f == nil then
    error(err)
  end
  local r = f:read("*all")
  f:close()
  return r
end

local function writefile(path, content)
  local f, err = io.open(path, "wb")
  if f == nil then
    error(err)
  end
  f:write(content)
  f:close()
  return r
end

function P.main(exo0, cp0, exo1, cp1, luapath)
  local iniobj = require("_iniobj")
  local ini0 = iniobj.new(convertencoding(readfile(exo0), cp0, 65001))
  local ini1 = iniobj.new(convertencoding(readfile(exo1), cp1, 65001))
  local entries = {}
  local used = {}
  local sections0 = ini0:sections()
  local sections1 = ini1:sections()
  if #sections0 ~= #sections1 then
    error("num of sections does not match.")
  end
  for i, sect0 in ipairs(sections0) do
    local sect1 = sections1[i]
    if sect0 ~= sect1 then
      error("section name does not match " .. sect0 .. " != " .. sect1 .. ".")
    end
    local keys0 = ini0:keys(sect0)
    local keys1 = ini1:keys(sect1)
    if #keys0 ~= #keys1 then
      error("num of keys on section " .. sect0 .. " does not match.")
    end
    local name0 = ini0:get(sect0, "_name", nil)
    local name1 = ini1:get(sect1, "_name", nil)
    if (name0 ~= nil)and(name1 ~= nil)and(used[name0] == nil) then
      local lines = {}
      local used2 = {}
      if name0 ~= name1 then
        table.insert(lines, string.format([==[    [ [=[%s]=] ] = [=[%s]=],]==], convertencoding(name0, 65001, cp0), convertencoding(name1, 65001, cp1)))
        used2[name0] = 1
      end
      for j, key0 in ipairs(keys0) do
        local key1 = keys1[j]
        if (key0 ~= key1)and(used2[key0] == nil) then
          table.insert(lines, string.format([==[    [ [=[%s]=] ] = [=[%s]=],]==], convertencoding(key0, 65001, cp0), convertencoding(key1, 65001, cp1)))
          used2[key0] = 1
        end
      end
      if #lines > 0 then
        table.insert(lines, 1, string.format([==[  [ [=[%s]=] ] = {]==], convertencoding(name0, 65001, cp0)))
        table.insert(lines, "  },")
        for j, line in ipairs(lines) do
          table.insert(entries, line)
        end
        used[name0] = 1
      end
    end
  end
  table.insert(entries, 1, "-- DO NOT EDIT MANUALLY.")
  table.insert(entries, 2, "-- This file is automatically generated at build time.")
  table.insert(entries, 3, "return {")
  table.insert(entries, "}")
  writefile(luapath, table.concat(entries, "\r\n"))
end

return P