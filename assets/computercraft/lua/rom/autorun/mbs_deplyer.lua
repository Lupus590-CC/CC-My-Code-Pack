--  update detection (per CC computer vs ROM copy)

local function deploy()
  if fs.exists("/.mbs") then 
    shell.run("delete /.mbs")
  end
  shell.run("copy /rom/.mbs /.mbs")
  shell.run("copy /rom/.mbsVersion.txt /.mbs/version")
  
  -- create startup
  if not fs.exists("/startup") then
    shell.run("mkdir /startup")
  end
  local f = fs.open("/startup/99_mbs.lua", "w")
  f.writeLine("\nshell.run(\"rom/programs/mbs.lua startup\")")
  f.close()
end


if not fs.exists("/.mbs/version") then
  deploy()
else

  -- read version file and compair to ROM
  local f = fs.open("/.mbs/version", "r")
  local rootVersion = f.readAll()
  f.close()
  local f = fs.exists("/rom/.mbsVersion.txt") and fs.open("/rom/.mbsVersion.txt", "r") or {readAll = function() return nil end}
  local romVersion = f.readAll()
  f.close()
  
  
  if rootVersion ~= romVersion then
    deploy()
  else
    return -- up to date, do nothing
  end
end
  



