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

return P
