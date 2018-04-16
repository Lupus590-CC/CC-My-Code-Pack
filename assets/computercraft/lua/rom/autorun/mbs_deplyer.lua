--  update detection (per CC computer vs ROM copy)


local function deploy()
  if fs.exists("/.mbs") then 
    shell.run("delete /.mbs")
  end
  
  local whiteList = {"bin", "lib", "modules", "mbs.lua"}
  for k,v in ipairs(whiteList) do
    shell.run("copy /rom/.mbs/"..v.." /.mbs/"..v)
  end
  
  -- create startup
  if not fs.exists("/startup") then
    shell.run("mkdir /startup")
  end
  
  local startupFileCommand = "shell.run(\"/.mbs/mbs.lua startup\")"
  if fs.isDir("/startup") then
    local f = fs.open("/startup/99_mbs.lua", "w")
    f.writeLine(startupFileCommand)
    f.close()
  else
    -- TODO: startup file support
  end
  
  shell.run("copy /rom/.mbsVersion.txt /.mbs/version") -- do this last, just in case of interuption
end


if not fs.exists("/.mbs/version") then
  deploy()
else

  -- read version file and compair to ROM
  local f = fs.open("/.mbs/version", "r")
  local rootVersion = f.readAll()
  f.close()
  local f = fs.exists("/rom/.mbsVersion.txt") and fs.open("/rom/.mbsVersion.txt", "r") or {readAll = function() return nil end} -- TODO: make this less hacky
  local romVersion = f.readAll()
  f.close()
  
  
  if rootVersion ~= romVersion then
    deploy()
  else
    return -- up to date, do nothing
  end
end
  



