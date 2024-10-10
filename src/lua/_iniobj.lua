local P = {}

function P.new(str)
  return P.new_core(tostring(str):gmatch("[^\r\n]+"))
end

function P.newfile(file)
  return P.new_core(io.lines(tostring(file)))
end

function P.new_core(iter)
  local o = setmetatable({ data = {}, idx = 0 }, { __index = P, __tostring = P.__tostring })
  local sect = ""
  for line in iter do
    local m = line:match("^%[([^%]]+)%]$")
    if m ~= nil then
      sect = m
    else
      local key, value = line:match("^([^=]+)=(.*)$")
      if key ~= nil then
        o:set(sect, key, value)
      end
    end
  end
  return o
end

function P:__tostring()
  local sects = {}
  for sect, t in pairs(self.data) do
    local values = {}
    for k, v in pairs(t.t) do
      table.insert(values, v)
    end
    table.sort(values, function(a, b)
      return (a.idx < b.idx)
    end)
    table.insert(sects, { key = sect, idx = t.idx, values = values })
  end
  table.sort(sects, function(a, b)
    return (a.idx < b.idx)
  end)
  local r = {}
  for i, sect in ipairs(sects) do
    table.insert(r, "[" .. sect.key .. "]")
    for j, value in ipairs(sect.values) do
      table.insert(r, value.key .. "=" .. value.v)
    end
  end
  return table.concat(r, "\r\n") .. "\r\n"
end

function P:get(sect, key, default)
  sect = tostring(sect)
  key = tostring(key)
  if (self.data[sect] ~= nil) and (self.data[sect].t[key] ~= nil) then
    return self.data[sect].t[key].v
  end
  return default
end

function P:set(sect, key, value)
  sect = tostring(sect)
  key = tostring(key)
  if self.data[sect] == nil then
    self.idx = self.idx + 1
    self.data[sect] = { sect = sect, idx = self.idx, t = {} }
  end
  if self.data[sect].t[key] == nil then
    self.idx = self.idx + 1
    self.data[sect].t[key] = { key = key, idx = self.idx }
  end
  self.data[sect].t[key].v = tostring(value)
end

function P:delete(sect, key)
  sect = tostring(sect)
  key = tostring(key)
  if self.data[sect] ~= nil then
    self.data[sect].t[key] = nil
  end
end

function P:deletesection(sect)
  sect = tostring(sect)
  self.data[sect] = nil
end

function P:sections()
  local sects = {}
  for sect, t in pairs(self.data) do
    table.insert(sects, t)
  end
  table.sort(sects, function(a, b)
    return (a.idx < b.idx)
  end)
  local r = {}
  for i, v in ipairs(sects) do
    table.insert(r, v.sect)
  end
  return r
end

function P:keys(sect)
  sect = tostring(sect)
  local r = {}
  if self.data[sect] ~= nil then
    local values = {}
    for key, v in pairs(self.data[sect].t) do
      table.insert(values, v)
    end
    table.sort(values, function(a, b)
      return (a.idx < b.idx)
    end)
    for i, v in ipairs(values) do
      table.insert(r, v.key)
    end
  end
  return r
end

function P:sectionexists(sect)
  sect = tostring(sect)
  return self.data[sect] ~= nil
end

function P:exists(sect, key)
  sect = tostring(sect)
  key = tostring(key)
  return (self.data[sect] ~= nil) and (self.data[sect].t[key] ~= nil)
end

return P
