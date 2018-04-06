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
  if (not rstate.lbutton)and(not rstate.mbutton)and(not rstate.rbutton) then
    rstate.lbutton = true
  end
  return P.ondropsimulated(files, rstate)
end

return P
