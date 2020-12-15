fs.makeDir("/startup")
local f = fs.open("/startup/00_mbs.lua", "w")
f.writeLine('shell.run("rom/.mbs/mbs.lua", "startup")')
f.close()
