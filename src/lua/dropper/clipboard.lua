local P = {}

P.name = "クリップボードから貼\り付け"

function P.oninitmenu()
  return "クリップボードから貼\り付け"
end

function P.onselect(index, state)
  local files = GCMZDrops.getclipboard()
  if files == nil then
    return nil
  end
  return files, state
end

return P
