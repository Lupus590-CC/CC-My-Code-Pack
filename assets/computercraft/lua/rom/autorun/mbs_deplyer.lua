--  add mbs to startup
if fs.exists("/startup/00_mbs.lua") then
  return
else
  fs.makeDir("/startup")
  local f = fs.open("/startup/00_mbs.lua", "w")
  f.writeLine([[
  shell.setAlias("mbs", "/rom/.mbs/mbs.lua")
  shell.run("/rom/.mbs/mbs.lua startup")
  ]])
  f.close()
end
