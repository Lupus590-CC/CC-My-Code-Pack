--  update detection (per CC computer vs ROM copy)

local deployFolder = false
local mbsVersionFilePathAndName = "/.mbsVersion"
local beVerbose = false
local messageDisplayTime = 0.5 -- if verbose that this is how long to sleep at the end of the file. Ignored if not verbose.

local function deploy()
  if fs.exists("/.mbs") then 
    shell.run("delete /.mbs")
  end
  
  if deployFolder then -- TODO: date stamp is in this folder, need to find somewhere else to put it
    local whiteList = {"bin", "lib", "modules", "mbs.lua"}
    for k,v in ipairs(whiteList) do
      shell.run("copy /rom/.mbs/"..v.." /.mbs/"..v)
    end
  end
  
  -- create startup
  -- TODO: determine startup folder support
  if not fs.exists("/startup") then
    shell.run("mkdir /startup")
  end
  
  local startupFileCommand = "shell.run(\"/.mbs/mbs.lua startup\")"
  if fs.isDir("/startup") then
    local f = fs.open("/startup/99_mbs.lua", "w") -- startup file doesn't need to change, this could probably be skipped if already present
    f.writeLine(startupFileCommand)
    f.close()
  else
    -- TODO: startup file support
    -- NOTE: it's posible to have a startup.lua file and a startup folder, the file then the folder contents will run
  end
  
  shell.run("copy /rom/.mbsVersion.txt "..mbsVersionFilePathAndName) -- do this last, just in case of interuption
end


if not fs.exists("/.mbs/version") then
  deploy()
  if beVerbose then
    print("MBS installed")
  end
else

  -- read version file and compair to ROM
  local f = fs.open(mbsVersionFilePathAndName, "r")
  local rootVersion = f.readAll()
  f.close()
  local f = fs.exists("/rom/.mbsVersion.txt") and fs.open("/rom/.mbsVersion.txt", "r") or {readAll = function() return nil end} -- TODO: make this less hacky
  local romVersion = f.readAll()
  f.close()
  
  
  if rootVersion ~= romVersion then
    deploy()
    if beVerbose then
      print("MBS updated")
    end
  else
    if beVerbose then
      print("MBS already up to date")
    end
    -- up to date, do nothing
  end
end

if beVerbose then
  sleep(messageDisplayTime)
end
  



