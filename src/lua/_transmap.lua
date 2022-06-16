local P = {}

P.translation_table = {
  -- The following files are generated at build time so do not exist in the repository.
  -- patchid: 1 - English patched environment
  [ 1 ] = require("_transmap1"),
  -- patchid: 2 - Simplified Chinese patched environment
  [ 2 ] = require("_transmap2"),
}

function P.getpatchid()
  return GCMZDrops.getpatchid()
end

local function return_key(self, key) return key end
local empty_table = setmetatable({}, {__index = return_key})

local function gen_table(dest, table)
  if table ~= nil then
    for name, map in pairs(table) do
      local t = {}
      for k, v in pairs(map) do
        t[k] = v
      end
      dest[name] = setmetatable(t, {__index = return_key})
    end
  end
end

local function gen_reverse_table(dest, table)
  if table ~= nil then
    for name, map in pairs(table) do
      local t = {}
      for k, v in pairs(map) do
        t[v] = k
      end
      dest[map[name]] = setmetatable(t, {__index = return_key})
    end
  end
end

function P.to0()
  local r = {}
  local patchid = P.getpatchid()
  if patchid ~= 0 then
    local table = P.translation_table[patchid]
    if table ~= nil then
      gen_table(r, table)
    end
  end
  return setmetatable(r, {__index = function(self, key) return empty_table end})
end

function P.new()
  local r = {}
  local patchid = P.getpatchid()
  for pid, table in pairs(P.translation_table) do
    if pid ~= patchid then
      gen_reverse_table(r, table)
    else
      gen_table(r, table)
    end
  end
  return setmetatable(r, {__index = function(self, key) return empty_table end})
end

return P