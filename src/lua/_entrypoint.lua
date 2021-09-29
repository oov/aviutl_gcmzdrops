-- これはごちゃまぜドロップスのエントリーポイントです。
-- 通常の用途ではこのファイルを書き換える必要はありません。
local P = {}

P.handlers = {}

function P.init(handlers)
  for i, v in ipairs(handlers) do
    local h = require(v)
    if (type(h.name) == "string")and
      (type(h.priority) == "number")and
      (type(h.ondragenter) == "function")and
      (type(h.ondragover) == "function")and
      (type(h.ondragleave) == "function")and
      (type(h.ondrop) == "function") then
      table.insert(P.handlers, h)
    end
  end
  table.sort(P.handlers, function(a, b)
    return a.priority > b.priority
  end)
end

function P.ondragenter(files, state)
  local r = false
  for i, h in ipairs(P.handlers) do
    if h ~= false then
      if h.ondragenter(files, state) then
        r = true
      else
        debug_print("イベントハンドラー ["..h.name.."] が ondragenter で false を返しました")
        P.handlers[i] = false
      end
    end
  end
  return r
end

function P.ondragover(files, state)
  local r = false
  for i, h in ipairs(P.handlers) do
    if h ~= false then
      if h.ondragover(files, state) then
        r = true
      else
        debug_print("イベントハンドラー ["..h.name.."] が ondragover で false を返しました")
        P.handlers[i] = false
      end
    end
  end
  return r
end

function P.ondragleave()
  for i, h in ipairs(P.handlers) do
    if h ~= false then
      h.ondragleave()
      P.handlers[i] = false
    end
  end
end

function P.ondrop(files, state)
  for i, h in ipairs(P.handlers) do
    if h ~= false then
      local f, s = h.ondrop(files, state)
      if f == nil then
        debug_print("イベントハンドラー ["..h.name.."] で処理がキャンセルされました")
        return false
      elseif f ~= false then
        for i2, f2 in ipairs(f) do
          debug_print("[" .. i2 .. "] " .. f2.filepath)
        end
        GCMZDrops.drop(f, s)
        debug_print("イベントハンドラー ["..h.name.."] で処理が完了しました")
        return true
      else
        debug_print("イベントハンドラー ["..h.name.."] では完了しなかったため次へ移行")
      end
    end
  end
  return false
end

function P.ondropsimulated(files, state)
  if not P.ondragenter(files, state) then
    return false
  end
  if not P.ondragover(files, state) then
    return false
  end
  if not P.ondrop(files, state) then
    return false
  end
  return true
end

P.droppers = {}

function P.initdropper(droppers)
  table.sort(droppers, function(a, b)
    return a > b
  end)
  for i, v in ipairs(droppers) do
    local d = require(v)
    if (type(d.name) == "string")and
      (type(d.oninitmenu) == "function")and
      (type(d.onselect) == "function") then
      table.insert(P.droppers, d)
    end
  end
end

function P.getdroppermenuitems()
  local r = {}
  for i, d in ipairs(P.droppers) do
    local m = d.oninitmenu()
    if type(m) == "table" then
      m.name = d.name
    elseif m == nil then
      m = false
    end
    table.insert(r, m)
  end
  return r
end

function P.selectdropper(dropperindex, menuindex, state)
  local files, rstate = P.droppers[dropperindex].onselect(menuindex, state)
  if files == nil then
    return false
  end
  if type(rstate) ~= "table" then
    rstate = {x = state.x, y = state.y}
  end
  rstate.control = rstate.control or false
  rstate.shift = rstate.shift or false
  rstate.alt = rstate.alt or false
  rstate.lbutton = rstate.lbutton or false
  rstate.mbutton = rstate.mbutton or false
  rstate.rbutton = rstate.rbutton or false
  rstate.frameadvance = rstate.frameadvance or 0
  if (not rstate.lbutton)and(not rstate.mbutton)and(not rstate.rbutton) then
    rstate.lbutton = true
  end
  return P.ondropsimulated(files, rstate)
end

local iniobj = {}

function iniobj.new(str)
  return iniobj.new_core(tostring(str):gmatch('[^\r\n]+'))
end

function iniobj.newfile(file)
  return iniobj.new_core(io.lines(tostring(file)))
end

function iniobj.new_core(iter)
  local o = setmetatable({data = {}, idx = 0}, {__index = iniobj, __tostring = iniobj.__tostring})
  local sect = ""
  for line in iter do
    local m = line:match('^%[([^%]]+)%]$')
    if m ~= nil then
      sect = m
    else
      local key, value = line:match('^([^=]+)=(.*)$')
      if key ~= nil then
        o:set(sect, key, value)
      end
    end
  end
  return o
end

function iniobj:__tostring()
  local sects = {}
  for sect, t in pairs(self.data) do
    local values = {}
    for k, v in pairs(t.t) do
      table.insert(values, v)
    end
    table.sort(values, function(a, b) return (a.idx < b.idx) end)
    table.insert(sects, { key = sect, idx = t.idx, values = values })
  end
  table.sort(sects, function(a, b) return (a.idx < b.idx) end)
  local r = {}
  for i, sect in ipairs(sects) do
    table.insert(r, "[" .. sect.key .. "]")
    for j, value in ipairs(sect.values) do
      table.insert(r, value.key .. "=" .. value.v)
    end
  end
  return table.concat(r, "\r\n") .. "\r\n"
end

function iniobj:get(sect, key, default)
  sect = tostring(sect)
  key = tostring(key)
  if (self.data[sect] ~= nil)and(self.data[sect].t[key] ~= nil) then
    return self.data[sect].t[key].v
  end
  return default
end

function iniobj:set(sect, key, value)
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

function iniobj:delete(sect, key)
  sect = tostring(sect)
  key = tostring(key)
  if self.data[sect] ~= nil then
    self.data[sect].t[key] = nil
  end
end

function iniobj:deletesection(sect)
  sect = tostring(sect)
  self.data[sect] = nil
end

function iniobj:sections()
  local sects = {}
  for sect, t in pairs(self.data) do
    table.insert(sects, t)
  end
  table.sort(sects, function(a, b) return (a.idx < b.idx) end)
  local r = {}
  for i, v in ipairs(sects) do
    table.insert(r, v.sect)
  end
  return r
end

function iniobj:keys(sect)
  sect = tostring(sect)
  local r = {}
  if self.data[sect] ~= nil then
    local values = {}
    for key, v in pairs(self.data[sect].t) do
      table.insert(values, v)
    end
    table.sort(values, function(a, b) return (a.idx < b.idx) end)
    for i, v in ipairs(values) do
      table.insert(r, v.key)
    end
  end
  return r
end

function iniobj:sectionexists(sect)
  sect = tostring(sect)
  return self.data[sect] ~= nil
end

function iniobj:exists(sect, key)
  sect = tostring(sect)
  key = tostring(key)
  return (self.data[sect] ~= nil)and(self.data[sect].t[key] ~= nil)
end

GCMZDrops.inistring = iniobj.new
GCMZDrops.inifile = iniobj.newfile

return P
