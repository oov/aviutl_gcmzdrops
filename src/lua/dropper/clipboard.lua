local P = {}

P.name = i18n({
  ja_JP = [=[クリップボードから貼り付け]=],
  en_US = [=[Paste from clipboard]=],
})

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
