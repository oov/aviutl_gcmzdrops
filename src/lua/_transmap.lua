local P = {}

P.code_page_table = {
  [ 1 ] = 932,
  [ 2 ] = 936,
}

P.translation_table = {
  -- English patched environment
  [ 1 ] = require("_transmap1"),
  -- Simplified Chinese patched environment
  [ 2 ] = require("_transmap2"),
}

function P.getpatchid()
  return GCMZDrops.getpatchid()
end

function P.convert(str, from, to)
  return GCMZDrops.convertencoding(str, from, to)
end

local function return_key(self, key) return key end
local empty_table = setmetatable({}, {__index = return_key})

local function gen_table(dest, code_page, table)
  if (code_page ~= nil)and(table ~= nil) then
    for name, map in pairs(table) do
      local t = {}
      for k, v in pairs(map) do
        t[P.convert(k, 65001, 932)] = P.convert(v, 65001, code_page)
      end
      dest[P.convert(name, 65001, 932)] = setmetatable(t, {__index = return_key})
    end
  end
end

local function gen_reverse_table(dest, code_page, table)
  if (code_page ~= nil)and(table ~= nil) then
    for name, map in pairs(table) do
      local t = {}
      for k, v in pairs(map) do
        t[P.convert(v, 65001, code_page)] = P.convert(k, 65001, 932)
      end
      dest[P.convert(map[name], 65001, code_page)] = setmetatable(t, {__index = return_key})
    end
  end
end

function P.to0()
  local r = {}
  local patchid = P.getpatchid()
  if patchid ~= 0 then
    local code_page = P.code_page_table[patchid]
    local table = P.translation_table[patchid]
    if (code_page ~= nil)and(table ~= nil) then
      gen_table(r, code_page, table)
    end
  end
  return setmetatable(r, {__index = function(self, key) return empty_table end})
end

function P.new()
  local r = {}
  local patchid = P.getpatchid()
  for pid, table in pairs(P.translation_table) do
    local code_page = P.code_page_table[pid]
    if pid ~= patchid then
      gen_reverse_table(r, code_page, table)
    else
      gen_table(r, code_page, table)
    end
  end
  return setmetatable(r, {__index = function(self, key) return empty_table end})
end

return P