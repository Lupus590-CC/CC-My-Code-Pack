local f = fs.open("/startup/00_mbs.lua", "w")
local current = shell.getRunningProgram()
f.writeLine(("assert(loadfile(%q, _ENV))('startup', %q)"):format(current, current))
f.close()
