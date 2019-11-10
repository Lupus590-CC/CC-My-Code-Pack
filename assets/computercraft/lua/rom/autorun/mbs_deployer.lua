--  add mbs to startup
if fs.exists("/startup/00_mbs.lua") then
  return
else
  fs.makeDir("/startup")
  local f = fs.open("/startup/00_mbs.lua", "w")
  f.writeLine("assert(loadfile(\"rom/.mbs/mbs.lua\", _ENV))('startup')")
  f.close()
end
