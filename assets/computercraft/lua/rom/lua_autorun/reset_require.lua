
local packagePreloadBackup = {}
for k, v in pairs(package.preload) do
  packagePreloadBackup[k] = v
end
local packageLoadedBackup = {}
for k, v in pairs(package.loaded) do
  packageLoadedBackup[k] = v
end

local function resetRequire()
  package.preload = {}
  for k, v in pairs(packagePreloadBackup) do
    package.preload[k] = v
  end
  package.loaded = {}
  for k, v in pairs(packageLoadedBackup) do
    package.loaded[k] = v
  end
end

_ENV.resetRequire = resetRequire
