local P = {}

P.name = [=[クリップボードから貼り付け]=]

function P.oninitmenu()
  return P.name
end

function P.onselect(index, state)
  local files = GCMZDrops.getclipboard()
  if files == nil then
    return nil
  end
  return files, state
end

return P
