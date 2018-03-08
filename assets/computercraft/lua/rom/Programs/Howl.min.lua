local e={}local t,a,o=require,{},{startup=e}
local function i(n)local s=o[n]
if s~=nil then if s==e then
error("loop or previous error loading module '"..n..
"'",2)end;return s end;o[n]=e;local h=a[n]if h then s=h(n)elseif t then s=t(n)else
error("cannot load '"..n.."'",2)end;if s==nil then s=true end;o[n]=s;return s end
a["howl.tasks.Task"]=function(...)local n=i"howl.lib.assert"local s=i"howl.class"
local h=i"howl.lib.colored"local r=i"howl.class.mixin"local d=i"howl.platform".os
local l=i"howl.lib.utils"local u=table.insert
local function c(f,w)local y=l.parsePattern(f,true)
local p=l.parsePattern(w)local v=y.Type
n(v==p.Type,"Both from and to must be the same type "..v.." and "..y.Type)return{Type=v,From=y.Text,To=p.Text}end
local m=s("howl.tasks.Task"):include(r.configurable):include(r.optionGroup):addOptions{"description"}
function m:initialize(f,w,y)n.argType(f,"string","Task",1)if type(w)=="function"then y=w
w={}end;self.options={}self.name=f;self.action=y
self.dependencies={}self.maps={}self.produces={}if w then self:depends(w)end end
function m.static:addDependency(s,f)
local function w(y,...)
if

select('#',...)==1 and type(...)=="table"and(# (...)>0 or next(...)==nil)then local p=...for v=1,#p do u(y.dependencies,s(y,p[v]))end else
u(y.dependencies,s(y,...))end;return y end;self[f]=w;self[f:gsub("^%l",string.upper)]=w;return
self end;function m:setup(f,w)end
function m:Produces(f)
if type(f)=="table"then local w=self.produces;for y,f in ipairs(f)do
table.insert(w,f)end else table.insert(self.produces,f)end;return self end
function m:Maps(f,w)table.insert(self.maps,c(f,w))return self end;function m:Action(f)self.action=f;return self end
function m:runAction(f,...)if self.action then return
self.action(self,f,...)else return true end end
function m:Run(f,...)local w=false
if#self.dependencies==0 then w=true else for k,q in ipairs(self.dependencies)do if
q:resolve(f.env,f)then w=true end end end;if not w then return false end;for k,q in ipairs(self.produces)do
f.filesProduced[q]=true end;local y={...}local p=""if#y>0 then local k={}for q,j in ipairs(y)do
table.insert(k,tostring(j))end
p=" ("..table.concat(k,", ")..")"end;f.env.logger:info("Running %s",
self.name..p)local v=d.clock()
local b,g=true,nil
if f.Traceback then
xpcall(function()self:runAction(f.env,unpack(y))end,function(k)
for q=5,15
do local j,g=pcall(function()error("",q)end)if
k:match("Howlfile")then break end;k=k.."\n  "..g end;g=k;b=false end)else b,g=pcall(self.runAction,self,f.env,...)end
if b then
f.env.logger:success("%s finished",self.name)else
f.env.logger:error("%s: %s",self.name,g or"no message")error("Error running tasks",0)end;if f.ShowTime then
print(" ","Took "..d.clock()-v.."s")end;return true end;return m end
a["howl.tasks.Runner"]=function(...)local n=i"howl.class"local s=i"howl.lib.colored"
local h=i"howl.tasks.Context"local r=i"howl.class.mixin"local d=i"howl.platform".os
local l=i"howl.tasks.Task"
local u=n("howl.tasks.Runner"):include(r.sealed)
function u:initialize(c)self.tasks={}self.default=nil;self.env=c end
function u:setup()
for c,m in pairs(self.tasks)do m:setup(self.env,self)end;if self.env.logger.hasError then return false end;for c,m in
pairs(self.tasks)do
for c,f in ipairs(m.dependencies)do f:setup(self.env,self)end end;if self.env.logger.hasError then return
false end;return true end;function u:Task(c)
return function(m,f)return self:addTask(c,m,f)end end;function u:addTask(c,m,f)
return self:injectTask(l(c,m,f))end;function u:injectTask(c,m)
self.tasks[m or c.name]=c;return c end
function u:Default(c)local m
if c==nil then self.default=nil elseif type(c)==
"string"then self.default=self.tasks[c]if not self.default then
error("Cannot find task "..c)end else self.default=l("<default>",{},c)end;return self end;function u:Run(c)return self:RunMany({c})end
function u:RunMany(c)
local m=d.clock()local f=true;local w=h(self)if#c==0 then w:Start()else
for y,p in ipairs(c)do f=w:Start(p)end end;if w.ShowTime then
s.printColor("orange","Took "..
d.clock()-m.."s in total")end;return f end;return u end
a["howl.tasks.OptionTask"]=function(...)local n=i"howl.lib.assert"local s=i"howl.class.mixin"
local h=rawset;local r=i"howl.tasks.Task"
local d=r:subclass("howl.tasks.OptionTask"):include(s.configurable)
function d:initialize(l,u,c,m)r.initialize(self,l,u,m)self.options={}
self.optionKeys={}for f,w in ipairs(c or{})do self:addOption(w)end end
function d:addOption(l)local u=self.options
local c=function(m,f)if f==nil then f=true end;u[l]=f;return m end;self[l:gsub("^%l",string.upper)]=c;self[l]=c
self.optionKeys[l]=true end;function d:configure(l)n.argType(l,"table","configure",1)
for u,c in pairs(l)do if
self.optionKeys[u]then self.options[u]=c else end end end;return d end
a["howl.tasks.Dependency"]=function(...)local n=i"howl.class"
local s=n("howl.tasks.Dependency")
function s:initialize(h)if self.class==s then
error("Cannot create instance of abstract class "..tostring(s),2)end;self.task=h end;function s:setup(h,r)
error("setup has not been overridden in "..self.class,2)end;function s:resolve(h,r)
error("resolve has not been overridden in "..self.class,2)end;return s end
a["howl.tasks.Context"]=function(...)local n=i"howl.class"local s=i"howl.platform".fs
local h=i"howl.class.mixin"local r=i"howl.platform"
local d=n("howl.tasks.Context"):include(h.sealed)
function d:initialize(u)self.ran={}self.filesProduced={}self.tasks=u.tasks
self.default=u.default;self.Traceback=u.Traceback;self.ShowTime=u.ShowTime;self.env=u.env
self:BuildCache()end
function d:DoRequire(u,c)if self.filesProduced[u]then return true end
local m=self.producesCache[u]
if m then self.filesProduced[u]=true;return self:Run(m)end;m=self.normalMapsCache[u]local f,w;local y=u;if m then
self.filesProduced[u]=true;w=m.Name;f=m.Pattern.From end
for p,v in
pairs(self.patternMapsCache)do if u:match(p)then self.filesProduced[u]=true;w=v.Name
f=u:gsub(p,v.Pattern.From)break end end
if w then local p=self:DoRequire(f,true)if not p then
if not c then self.env.logger:error(
"Cannot find '"..f.."'")end;return false end
return self:Run(w,f,y)end;if s.exists(s.combine(self.env.root,u))then
self.filesProduced[u]=true;return true end;if not c then
self.env.logger:error(
"Cannot find a task matching '"..u.."'")end;return false end;local function l(u,c)local m=#u;if#u~=#c then return false end
for f=1,m do if u[f]~=c[f]then return false end end;return true end
function d:Run(u,...)
local c=u
if type(u)=="string"then c=self.tasks[u]if not c then
error("Cannot find a task called '"..u.."'")return false end elseif not c or not c.Run then
error(
"Cannot call task "..tostring(c).." as it has no 'Run' method")return false end;local m={...}local f=self.ran[c]
if not f then f={m}self.ran[c]=f else for w=1,#f do if l(m,f[w])then
return false end end;f[#f+1]=m end;r.refreshYield()return c:Run(self,...)end;d.run=d.Run
function d:Start(u)local c
if u then c=self.tasks[u]else c=self.default;u="<default>"end;if not c then
self.env.logger:error("Cannot find a task called '"..u.."'")return false end;return self:Run(c)end
function d:BuildCache()local u={}local c={}local m={}self.producesCache=u;self.patternMapsCache=c
self.normalMapsCache=m
for f,w in pairs(self.tasks)do local y=w.produces;if y then
for v,b in ipairs(y)do local g=u[b]if g then
error(string.format("Both '%s' and '%s' produces '%s'",g,f,b))end;u[b]=f end end
local p=w.maps
if p then
for v,b in ipairs(p)do
local g=(b.Type=="Pattern"and c or m)local k=b.To;local q=g[k]if q then
error(string.format("Both '%s' and '%s' match '%s'",q,f,k))end;g[k]={Name=f,Pattern=b}end end end;return self end;return d end
a["howl.scratchpad"]=function(...)local n=i"howl.lib.utils"local s=i"howl.lib.dump".dump
local h=i"howl.lib.colored".printColor;local r=n.parsePattern;local d=n.createLookup
local l={{name="input",provides=d{"foo.un.lua"}},{name="output",requires=d{"foo.min.lua"}},{name="minify",maps={{from=r("wild:*.lua",true),to=r("wild:*.min.lua")}}},{name="licence",maps={{from=r("wild:*.un.lua",true),to=r("wild:*.lua")}}}}
for f,w in pairs(l)do l[w.name]=w;if not w.maps then w.maps={}end
w.mapper=#w.maps>0;if not w.provides then w.provides={}end
if not w.requires then w.requires={}end end
local function u(f)local w={}
for y,p in ipairs(l)do
if p.provides[f]then w[#w+1]={task=p.name}end
for y,v in ipairs(p.maps)do
if v.to.Type=="Text"then if v.to.Text==f then
w[#w+1]={task=p.name,v.from.Text,f}end else if f:find(v.to.Text)then
w[#w+1]={task=p.name,f:gsub(v.to.Text,v.from.Text),f}end end end end;return w end
local function c(...)local f={}local w={}local y={}
local function p(b,g)
local k=b.task.."|"..table.concat(b,"|")local q=y[k]
if q then q.depth=math.min(q.depth,g)return q else b.depth=g;b.needed={}
b.solutions={}
b.name=b.task..": "..table.concat(b," \26 ")y[k]=b;w[#w+1]=b;return b end end
local function v(b,g)local b=p(b,g.depth+1)b.needed[#b.needed+1]=g;return b end
for b=1,select('#',...)do p({task=select(b,...)},1)end
while#w>0 do local b=table.remove(w,1)local g=l[b.task]
print("Task '"..b.name)if#b.needed>0 then print("  Needed for")
for k=1,#b.needed do h("lightGrey","    "..
b.needed[k].name)end end
if b.depth>4 then
h("red","  Too deep")elseif#b.solutions>0 or
(#g.requires==0 and not g.mapper)then h("green","  Endpoint")f[#f+1]=b
for k=1,#
b.needed do local q=b.needed[k]
q.solutions[#q.solutions+1]=b;if#q.solutions==1 then w[#w+1]=q end end else
for k=1,#g.requires do local q=g.requires[k]
print("  Depends on '"..q.."'")local u=u(q)for k=1,#u do local j=v(u[k],b)
h("yellow","    Maybe: "..j.name)end end
if g.mapper then local k=b[1]print("  Depends on '"..k.."'")
local u=u(k)for q=1,#u do local j=v(u[q],b)
h("yellow","    Maybe: "..j.name)end end end end;return f end;local m=c("output")for f=1,#m do print(m[f].name)end end
a["howl.platform.oc"]=function(...)local n=i("filesystem")local s=i("term")
local h=i("component")local r=pcall(function()return h.internet end)
local d=i("internet")local l=h.gpu;local function u(b)local g=getSize(b)local k=n.open(b)local q=k:read(g)k:close()
return q end
local function c(b)local g=#b+2;local k,q={b},1;local j={}while q>0 do
local x=k[q]q=q-1
if fs.isDir(x)then
for z,_ in ipairs(n.list(x))do q=q+1;k[q]=n.combine(x,_)end else j[x:sub(g)]=u(x)end end;return j end
local function m(b,g)for k,q in pairs(g)do write(n.combine(b,k),q)end end
local function f(b,g)local k=n.open(b,"w")local q,j=k:write(g)if not q then
io.stderr:write(j)end;k:close()end;local function w(b,g,k)
if not n.exists(b)then error("Cannot find "..g.." (looking for "..b..")",
k or 1)end end;local function y(b)local g=n.open(b)
local k=g:seek("end")g:close()return k end;local function p(b,g,k)if not r then
error("No internet card found",0)end;local q=""for j in d.request(b,g,k)do q=q..j end
return q end;local function v(b)
return function()
error(b..
" has not been implemented for OpenComputers!",2)end end
return
{os={clock=os.clock,time=os.time,getEnv=os.getEnv},fs={combine=n.concat,normalise=n.canonical,getDir=n.path,getName=n.name,currentDir=shell.getWorkingDirectory,currentProgram=function()return
process.info().command end,read=u,write=f,readDir=c,writeDir=m,getSize=y,assertExists=w,exists=n.exists,isDir=n.isDir,list=n.list,makeDir=n.makeDir,delete=n.delete,move=n.move,copy=n.copy},term={setColor=l.setForeground,resetColor=function()
l.setForeground(colors.white)end,print=print,write=io.write},http={request=p},log=function()
return end,refreshYield=function()os.sleep(0)end}end
a["howl.platform.native"]=function(...)local n=string.char(27)..'['
local s={white=97,orange=33,magenta=95,lightBlue=94,yellow=93,lime=92,pink=95,gray=90,grey=90,lightGray=37,lightGrey=37,cyan=96,purple=35,blue=36,brown=31,green=32,red=91,black=30}local function h(u)return
function()error(u.." is not implemented",2)end end
local r=i('pl.path')local d=i('pl.dir')local l=i('pl.file')
return
{fs={combine=r.join,normalise=r.normpath,getDir=r.dirname,getName=r.basename,currentDir=function()
return r.currentdir end,read=l.read,write=l.write,readDir=h("fs.readDir"),writeDir=h("fs.writeDir"),getSize=function(u)
local l=io:open(u,"r")local c=l:seek("end")l:close()return c end,assertExists=function(l)
if
not r.exists(l)then error("File does not exist")end end,exists=r.exists,isDir=r.isdir,list=function(d)local u={}
for r in r.dir(d)do u[#u+1]=r end;return u end,makeDir=d.makepath,delete=function(u)if r.isdir(u)then d.rmtree(u)else
l.delete(u)end end,move=l.move,copy=l.copy},http={request=h("http.request")},term={setColor=function(u)
local c=s[u]if not c then
error("Cannot find color "..tostring(u),2)end;io.write(n..c.."m")
io.flush()end,resetColor=function()
io.write(n.."0m")io.flush()end},refreshYield=function()
end}end
a["howl.platform"]=function(...)
if fs and term then return i"howl.platform.cc"elseif _G.component then
return i"howl.platform.oc"else return i"howl.platform.native"end end
a["howl.platform.cc"]=function(...)local n=term.getTextColor and term.getTextColor()or
colors.white
local function s(v)
local b=fs.open(v,"r")local g=b.readAll()b.close()return g end
local function h(v,b)local g=fs.open(v,"w")g.write(b)g.close()end;local function r(v,b,g)
if not fs.exists(v)then error("Cannot find "..b.." (Looking for "..v..")",
g or 1)end end
local d,l=os.queueEvent,coroutine.yield;local function u()d("sleep")
if l()=="terminate"then error("Terminated")end end
local function c(v)local b=#v+2;local g,k={v},1;local q={}while k>0 do
local j=g[k]k=k-1
if fs.isDir(j)then
for x,z in ipairs(fs.list(j))do k=k+1;g[k]=fs.combine(j,z)end else q[j:sub(b)]=s(j)end end;return q end
local function m(v,b)for g,k in pairs(b)do h(fs.combine(v,g),k)end end;local f
if http.fetch then
f=function(v,b,g)local k,q=http.fetch(v,b,g)
if k then
while true do local j,x,z,_=os.pullEvent(e)if j==
"http_success"and x==v then return true,z elseif j=="http_failure"and x==v then
return false,_,z end end end;return false,nil,q end else
f=function(...)local v,b=http.post(...)
if v then return true,b else return false,nil,b end end end;local w;if settings and fs.exists(".settings")then
settings.load(".settings")end
if settings and shell.getEnv then
w=function(v,n)
local b=shell.getEnv(v)if b~=nil then return b end;return settings.get(v,n)end elseif settings then w=settings.get elseif shell.getEnv then
w=function(v,n)local b=shell.getEnv(v)
if b~=nil then return b end;return n end else w=function(v,n)return n end end;local y
if profiler and profiler.milliTime then y=function()
return profiler.milliTime()*1e-3 end else y=os.time end;local p;if howlci then p=howlci.log else p=function()end end
return
{os={clock=os.clock,time=y,getEnv=w},fs={combine=fs.combine,normalise=function(v)return
fs.combine(v,"")end,getDir=fs.getDir,getName=fs.getName,currentDir=shell.dir,currentProgram=shell.getRunningProgram,read=s,write=h,readDir=c,writeDir=m,getSize=fs.getSize,assertExists=r,exists=fs.exists,isDir=fs.isDir,list=fs.list,makeDir=fs.makeDir,delete=fs.delete,move=fs.move,copy=fs.copy},term={setColor=function(v)local b=
colours[v]or colors[v]if not b then
error("Unknown color "..v,2)end;term.setTextColor(b)end,resetColor=function()
term.setTextColor(n)end,print=print,write=io.write},http={request=f},log=p,refreshYield=u}end
a["howl.packages.Proxy"]=function(...)local n=i"howl.class"local s=i"howl.platform".fs
local h=i"howl.class.mixin"local r=n("howl.packages.Proxy")function r:initialize(d,l,u)self.name=l;self.manager=d
self.package=u end
function r:getName()return self.name end;function r:files()local d=self.manager:getCache(self.name)return
self.package:files(d)end;function r:require(d,l)return
self.manager:require(self.package,d,l)end;return r end
a["howl.packages.Package"]=function(...)local n=i"howl.class"local s=i"howl.platform".fs
local h=i"howl.class.mixin"
local r=n("howl.packages.Package"):include(h.configurable):include(h.optionGroup)
function r:initialize(d,l)if self.class==r then
error("Cannot create instance of abstract class "..tostring(r),2)end;self.context=d;self.root=l
self.options={}end;function r:setup()
error("setup has not been overridden in "..tostring(self.class),2)end;function r:getName()
error("name has not been overridden in "..
tostring(self.class),2)end;function r:files(d)
error("files has not been overridden in "..
tostring(self.class),2)end;function r:require(d,l)
error("require has not been overrriden in "..
tostring(self.class),2)end;return r end
a["howl.packages.Manager"]=function(...)local n=i"howl.class"local s=i"howl.platform".fs
local h=i"howl.lib.dump"local r=i"howl.class.mixin"local d=i"howl.packages.Proxy"local l={}
local u=n("howl.packages.Manager")u.providers={}
function u:initialize(c)self.context=c;self.packages={}
self.packageLookup={}self.cache={}self.root=".howl/packages"self.alwaysRefresh=false end;function u.static:addProvider(n,c)self.providers[c]=n end
function u:addPackage(c,m)
local f=u.providers[c]
if not f then error("No such package provider "..c,2)end;local w=f(self.context,self.root)w:configure(m)local y=c.."-"..
w:getName()
w.installDir=s.combine(self.root,y)self.packages[y]=w;self.packageLookup[w]=y
w:setup(self.context)if self.context.logger.hasError then
error("Error setting up "..y,2)end;return d(self,y,w)end
function u:getCache(c)if not self.packages[c]then
error("No such package "..c,2)end;local m=self.cache[c]
local f=s.combine(self.root,c..".lua")
if m==nil and s.exists(f)then m=h.unserialise(s.read(f))end;if m==l then m=nil end;return m end
function u:require(c,m,f)local w=self.packageLookup[c]if not w then
error("No such package "..c:getName(),2)end;f=f or self.alwaysRefresh
local y=self:getCache(w)if y and m and not f then local b=c:files(y)
for g,k in ipairs(m)do if not b[k]then f=true;break end end end
local p=c:require(y,f)
if p~=y then
self.context.logger:verbose("Package "..w.." updated")if p==nil then self.cache[w]=l else self.cache[w]=p
s.write(s.combine(self.root,w..".lua"),h.serialise(p))end end;local v=c:files(p)
if m then for b,g in ipairs(m)do if not v[g]then
error("Cannot resolve "..g.." for "..w)end end end;return v end;return u end
a["howl.modules.tasks.require"]=function(...)local n=i"howl.lib.assert"
local s=i"howl.platform".fs;local h=i"howl.class.mixin"local r=i"howl.lib.Buffer"local d=i"howl.files.CopySource"
local l=i"howl.tasks.Runner"local u=i"howl.tasks.Task"local c=i"howl.modules.tasks.require.header"
local m="local env = setmetatable({ require = require, preload = preload, }, { __index = getfenv() })\n"local function f(b)
if b:find("%.lua$")then return
b:gsub("%.lua$",""):gsub("/","."):gsub("^(.*)%.init$","%1")end end
local function w(b)if
b.relative:find("%.res%.")then b.name=b.name:gsub("%.res%.",".")return
("return %q"):format(b.contents)end end
local y=u:subclass("howl.modules.require.RequireTask"):include(h.filterable):include(h.delegate("sources",{"from","include","exclude"})):addOptions{"link","startup","output","api"}
function y:initialize(b,g,k)u.initialize(self,g,k)self.sources=d()self.sources:rename(function(q)return
f(q.name)end)
self.sources:modify(w)
self:exclude{".git",".svn",".gitignore",b.out}
self:description("Packages files together to allow require")end;function y:configure(b)u.configure(self,b)
self.sources:configure(b)end
function y:output(b)
n.argType(b,"string","output",1)if self.options.output then
error("Cannot set output multiple times")end;self.options.output=b
self:Produces(b)end
function y:setup(b,g)u.setup(self,b,g)if not self.options.startup then
b.logger:error("Task '%s': No startup file",self.name)end
self:requires(self.options.startup)if not self.options.output then
b.logger:error("Task '%s': No output file",self.name)end end
function y:runAction(b)local g=self.sources:gatherFiles(b.root)
local k=s.combine(b.root,self.options.startup)local q=nil;local j=self.options.output;local x=self.options.link;local z=r()
z:append(c):append("\n")if x then z:append(m)end
for E,T in pairs(g)do
b.logger:verbose("Including "..T.relative)
z:append("preload[\""..T.name.."\"] = ")
if x then
n(s.exists(T.path),"Cannot find "..T.relative)
z:append("setfenv(assert(loadfile(\""..T.path.."\")), env)\n")else
z:append("function(...)\n"..T.contents.."\nend\n")end;if T.path==k then q=T.name end end;if not q then
error("Cannot find startup file "..self.options.startup.." in file list",0)end
if self.options.api then
z:append("if not shell or type(... or nil) == 'table' then\n")z:append("local tbl = ... or {}\n")
z:append("tbl.require = require tbl.preload = preload\n")z:append("return tbl\n")z:append("else\n")end
z:append("return preload[\""..q.."\"](...)\n")if self.options.api then z:append("end\n")end
s.write(s.combine(b.root,j),z:toString())end;local p={}
function p:require(b,g)return self:injectTask(y(self.env,b,g))end;local function v()l:include(p)end;return
{name="require task",description="A task that combines files that can be loaded using `require`.",apply=v,RequireTask=y}end
a["howl.modules.tasks.require.header"]=function(...)
return
"local loading = {}\
local oldRequire, preload, loaded = require, {}, { startup = loading }\
\
local function require(name)\
\009local result = loaded[name]\
\
\009if result ~= nil then\
\009\009if result == loading then\
\009\009\009error(\"loop or previous error loading module '\" .. name .. \"'\", 2)\
\009\009end\
\
\009\009return result\
\009end\
\
\009loaded[name] = loading\
\009local contents = preload[name]\
\009if contents then\
\009\009result = contents(name)\
\009elseif oldRequire then\
\009\009result = oldRequire(name)\
\009else\
\009\009error(\"cannot load '\" .. name .. \"'\", 2)\
\009end\
\
\009if result == nil then result = true end\
\009loaded[name] = result\
\009return result\
end"end
a["howl.modules.tasks.pack.vfs"]=function(...)
return
"local fs = fs\
\
local matches = {\
\009[\"^\"] = \"%^\",\
\009[\"$\"] = \"%$\",\
\009[\"(\"] = \"%(\",\
\009[\")\"] = \"%)\",\
\009[\"%\"] = \"%%\",\
\009[\".\"] = \"%.\",\
\009[\"[\"] = \"%[\",\
\009[\"]\"] = \"%]\",\
\009[\"*\"] = \"%*\",\
\009[\"+\"] = \"%+\",\
\009[\"-\"] = \"%-\",\
\009[\"?\"] = \"%?\",\
\009[\"\\0\"] = \"%z\",\
}\
\
--- Escape a string for using in a pattern\
-- @tparam string pattern The string to escape\
-- @treturn string The escaped pattern\
local function escapePattern(pattern)\
\009return (pattern:gsub(\".\", matches))\
end\
\
local function matchesLocal(root, path)\
\009return root == \"\" or path == root or path:sub(1, #root + 1) == root .. \"/\"\
end\
\
local function extractLocal(root, path)\
\009if root == \"\" then\
\009\009return path\
\009else\
\009\009return path:sub(#root + 2)\
\009end\
end\
\
\
local function copy(old)\
\009local new = {}\
\009for k, v in pairs(old) do new[k] = v end\
\009return new\
end\
\
--[[\
\009Emulates a basic file system.\
\009This doesn't have to be too advanced as it is only for Howl's use\
\009The files is a list of paths to file contents, or true if the file\
\009is a directory.\
\009TODO: Override IO\
]]\
local function makeEnv(root, files)\
\009-- Emulated filesystem (partially based of Oeed's)\
\009files = copy(files)\
\009local env\
\009env = {\
\009\009fs = {\
\009\009\009list = function(path)\
\009\009\009\009path = fs.combine(path, \"\")\
\009\009\009\009local list = fs.isDir(path) and fs.list(path) or {}\
\
\009\009\009\009if matchesLocal(root, path) then\
\009\009\009\009\009local pattern = \"^\" .. escapePattern(extractLocal(root, path))\
\009\009\009\009\009if pattern ~= \"^\" then pattern = pattern .. '/' end\
\009\009\009\009\009pattern = pattern .. '([^/]+)$'\
\
\009\009\009\009\009for file, _ in pairs(files) do\
\009\009\009\009\009\009local name = file:match(pattern)\
\009\009\009\009\009\009if name then list[#list + 1] = name end\
\009\009\009\009\009end\
\009\009\009\009end\
\
\009\009\009\009return list\
\009\009\009end,\
\
\009\009\009exists = function(path)\
\009\009\009\009path = fs.combine(path, \"\")\
\009\009\009\009if fs.exists(path) then\
\009\009\009\009\009return true\
\009\009\009\009elseif matchesLocal(root, path) then\
\009\009\009\009\009return files[extractLocal(root, path)] ~= nil\
\009\009\009\009end\
\009\009\009end,\
\
\009\009\009isDir = function(path)\
\009\009\009\009path = fs.combine(path, \"\")\
\009\009\009\009if fs.isDir(path) then\
\009\009\009\009\009return true\
\009\009\009\009elseif matchesLocal(root, path) then\
\009\009\009\009\009return files[extractLocal(root, path)] == true\
\009\009\009\009end\
\009\009\009end,\
\
\009\009\009isReadOnly = function(path)\
\009\009\009\009path = fs.combine(path, \"\")\
\009\009\009\009if fs.exists(path) then\
\009\009\009\009\009return fs.isReadOnly(path)\
\009\009\009\009elseif matchesLocal(root, path) and files[extractLocal(root, path)] ~= nil then\
\009\009\009\009\009return true\
\009\009\009\009else\
\009\009\009\009\009return false\
\009\009\009\009end\
\009\009\009end,\
\
\009\009\009getName = fs.getName,\
\009\009\009getDir = fs.getDir,\
\009\009\009getSize = fs.getSize,\
\009\009\009getFreeSpace = fs.getFreeSpace,\
\009\009\009combine = fs.combine,\
\
\009\009\009-- TODO: This should be implemented\
\009\009\009move = fs.move,\
\009\009\009copy = fs.copy,\
\009\009\009makeDir = function(dir)\
\
\009\009\009end,\
\009\009\009delete = fs.delete,\
\
\009\009\009open = function(path, mode)\
\009\009\009\009path = fs.combine(path, \"\")\
\009\009\009\009if matchesLocal(root, path) then\
\009\009\009\009\009local localPath = extractLocal(root, path)\
\009\009\009\009\009if type(files[localPath]) == 'string' then\
\009\009\009\009\009\009local handle = {close = function()end}\
\009\009\009\009\009\009if mode == 'r' then\
\009\009\009\009\009\009\009local content = files[localPath]\
\009\009\009\009\009\009\009handle.readAll = function()\
\009\009\009\009\009\009\009\009return content\
\009\009\009\009\009\009\009end\
\
\009\009\009\009\009\009\009local line = 1\
\009\009\009\009\009\009\009local lines\
\009\009\009\009\009\009\009handle.readLine = function()\
\009\009\009\009\009\009\009\009if not lines then -- Lazy load lines\
\009\009\009\009\009\009\009\009\009lines = {content:match((content:gsub(\"[^\\n]+\\n?\", \"([^\\n]+)\\n?\")))}\
\009\009\009\009\009\009\009\009end\
\009\009\009\009\009\009\009\009if line > #lines then\
\009\009\009\009\009\009\009\009\009return nil\
\009\009\009\009\009\009\009\009else\
\009\009\009\009\009\009\009\009\009return lines[line]\
\009\009\009\009\009\009\009\009end\
\009\009\009\009\009\009\009\009line = line + 1\
\009\009\009\009\009\009\009end\
\
\009\009\009\009\009\009\009return handle\
\009\009\009\009\009\009else\
\009\009\009\009\009\009\009error('Cannot write to read-only file.', 2)\
\009\009\009\009\009\009end\
\009\009\009\009\009end\
\009\009\009\009end\
\
\009\009\009\009return fs.open(path, mode)\
\009\009\009end\
\009\009},\
\
\009\009loadfile = function(name)\
\009\009\009local file = env.fs.open(name, \"r\")\
\009\009\009if file then\
\009\009\009\009local func, err = load(file.readAll(), fs.getName(name), nil, env)\
\009\009\009\009file.close()\
\009\009\009\009return func, err\
\009\009\009end\
\009\009\009return nil, \"File not found: \"..name\
\009\009end,\
\
\009\009dofile = function(name)\
\009\009\009local file, e = env.loadfile(name, env)\
\009\009\009if file then\
\009\009\009\009return file()\
\009\009\009else\
\009\009\009\009error(e, 2)\
\009\009\009end\
\009\009end,\
\009}\
\
\009env._G = env\
\009env._ENV = env\
\009return setmetatable(env, {__index = _ENV or getfenv()})\
end\
\
local function extract(root, files, from, to)\
\009local pattern = \"^\" .. escapePattern(extractLocal(root, from))\
\009if pattern ~= \"^\" then pattern = pattern .. '/' end\
\009pattern = pattern .. '(.*)$'\
\
\009for file, contents in pairs(files) do\
\009\009local name = file:match(pattern)\
\009\009if name then\
\009\009\009print(\"Extracting \" .. name)\
\009\009\009local handle = fs.open(fs.combine(to, name), \"w\")\
\009\009\009handle.write(contents)\
\009\009\009handle.close()\
\009\009end\
\009end\
end"end
a["howl.modules.tasks.pack.template"]=function(...)
return
"local files = ${files}\
\
${vfs}\
\
local root = \"\"\
local args = {...}\
if #args == 1 and args[1] == '--extract' then\
\009extract(root, files, \"\", root)\
else\
\009local env = makeEnv(root, files)\
\009local func, err = env.loadfile(${startup})\
\009if not func then error(err, 0) end\
\009return func(...)\
end"end
a["howl.modules.tasks.pack"]=function(...)local n=i"howl.lib.assert"local s=i"howl.lib.dump"
local h=i"howl.platform".fs;local r=i"howl.class.mixin"local d=i"howl.lexer.rebuild"
local l=i"howl.files.CopySource"local u=i"howl.tasks.Runner"local c=i"howl.tasks.Task"
local m=i"howl.lib.utils".formatTemplate;local f=i"howl.modules.tasks.pack.template"local w=i"howl.modules.tasks.pack.vfs"
local y=c:subclass("howl.modules.tasks.pack.PackTask"):include(r.filterable):include(r.delegate("sources",{"from","include","exclude"})):addOptions{"minify","startup","output"}
function y:initialize(b,g,k)c.initialize(self,g,k)self.root=b.root;self.sources=l()
self.sources:modify(function(q)
local j=q.contents;if self.options.minify and loadstring(j)then
return d.minifyString(j)end end)
self:exclude{".git",".svn",".gitignore",b.out}
self:description("Combines multiple files using Pack")end;function y:configure(b)c.configure(self,b)
self.sources:configure(b)end
function y:output(b)
n.argType(b,"string","output",1)if self.options.output then
error("Cannot set output multiple times")end;self.options.output=b
self:Produces(b)end
function y:setup(b,g)c.setup(self,b,g)if not self.options.startup then
b.logger:error("Task '%s': No startup file",self.name)end
self:requires(self.options.startup)if not self.options.output then
b.logger:error("Task '%s': No output file",self.name)end end
function y:runAction(b)local g=self.sources:gatherFiles(self.root)
local k=self.options.startup;local q=self.options.output;local j=self.options.minify;local x={}for E,T in pairs(g)do b.logger:verbose(
"Including "..T.relative)
x[T.name]=T.contents end
local z=m(f,{files=s.serialise(x),startup=("%q"):format(k),vfs=w})if j then z=d.minifyString(z)end
h.write(h.combine(b.root,q),z)end;local p={}
function p:pack(b,g)return self:injectTask(y(self.env,b,g))end;local function v()u:include(p)end
return
{name="pack task",description="A task to combine multiple files into one which are then executed within a virtual file system.",apply=v,PackTask=y}end
a["howl.modules.tasks.minify"]=function(...)local n=i"howl.lib.assert"
local s=i"howl.lexer.rebuild"local h=i"howl.tasks.Runner"local r=i"howl.tasks.Task"local d=s.minifyFile;local l=function(w,y,p,v)
return d(y.root,p,v)end
local u=r:subclass("howl.modules.minify.tasks.MinifyTask"):addOptions{"input","output"}function u:initialize(w,y,p)r.initialize(self,y,p)
self:description"Minify a file"end
function u:input(w)
n.argType(w,"string","input",1)if self.options.input then
error("Cannot set input multiple times")end;self.options.input=w
self:requires(w)end
function u:output(w)n.argType(w,"string","output",1)if self.options.output then
error("Cannot set output multiple times")end;self.options.output=w
self:Produces(w)end
function u:setup(w,y)r.setup(self,w,y)if not self.options.input then
w.logger:error("Task '%s': No input file specified",self.name)end;if
not self.options.output then
w.logger:error("Task '%s': No output file specified",self.name)end end
function u:runAction(w)
local y,p=d(w.root,self.options.input,self.options.output)local v=(y-p)/y*100;v=math.floor(v*100)/100
w.logger:verbose(("%.20f%% decrease in file size"):format(v))end;local c={}
function c:minify(w,y)return self:injectTask(u(self.env,w,y))end
function c:addMinifier(w,y,p)w=w or"_minify"return
self:addTask(w,{},l):Description("Minifies files"):Maps(
y or"wild:*.lua",p or"wild:*.min.lua")end;local function m()h:include(c)end;local function f(w)
w.mediator:subscribe({"HowlFile","env"},function(y)
y.minify=d end)end;return
{name="minify task",description="Adds various tasks to minify files.",apply=m,setup=f}end
a["howl.modules.tasks.gist"]=function(...)local n=i"howl.lib.assert"local s=i"howl.class.mixin"
local h=i"howl.lib.settings"local r=i"howl.lib.json"local d=i"howl.platform"local l=d.http;local u=i"howl.lib.Buffer"
local c=i"howl.tasks.Task"local m=i"howl.tasks.Runner"local f=i"howl.files.CopySource"
local w=c:subclass("howl.modules.tasks.gist.GistTask"):include(s.filterable):include(s.delegate("sources",{"from","include","exclude"})):addOptions{"gist","summary"}
function w:initialize(v,b,g)c.initialize(self,b,g)self.root=v.root;self.sources=f()
self:exclude{".git",".svn",".gitignore"}self:description"Uploads files to a gist"end;function w:configure(v)c.configure(self,context,runner)
self.sources:configure(v)end
function w:setup(v,b)c.setup(self,v,b)if not
self.options.gist then
v.logger:error("Task '%s': No gist ID specified",self.name)end
if not h.githubKey then
v.logger:error("Task '%s': No GitHub API key specified. Goto https://github.com/settings/tokens/new to create one.",self.name)end end
function w:runAction(v)local b=self.sources:gatherFiles(self.root)
local g=self.options.gist;local k=h.githubKey;local q={}
for A,O in pairs(b)do
v.logger:verbose("Including "..O.relative)q[O.name]={content=O.contents}end
local j="https://api.github.com/gists/"..g.."?access_token="..k
local x={Accept="application/vnd.github.v3+json",["X-HTTP-Method-Override"]="PATCH"}
local z=r.encodePretty({files=q,description=self.options.summary})local _,E,T=l.request(j,z,x)
if not _ then if E then
v.logger:error(E.readAll())end;error(result,0)end end;local y={}
function y:gist(v,b)return self:injectTask(w(self.env,v,b))end;local function p()m:include(y)end;return
{name="gist task",description="A task that uploads files to a Gist.",apply=p,GistTask=w}end
a["howl.modules.tasks.clean"]=function(...)local n=i"howl.class.mixin"
local s=i"howl.platform".fs;local h=i"howl.tasks.Task"local r=i"howl.tasks.Runner"local d=i"howl.files.Source"
local l=h:subclass("howl.modules.tasks.clean.CleanTask"):include(n.configurable):include(n.filterable):include(n.delegate("sources",{"from","include","exclude"}))
function l:initialize(m,f,w)h.initialize(self,f,w)self.root=m.root;self.sources=d()
self:exclude{".git",".svn",".gitignore"}self:description"Deletes all files matching a pattern"end
function l:configure(m)self.sources:configure(m)end;function l:setup(m,f)h.setup(self,m,f)local w=self.sources
if
w.allowEmpty and#w.includes==0 then w:include(s.combine(m.out,"*"))end end
function l:runAction(m)for f,w in
ipairs(self.sources:gatherFiles(self.root,true))do m.logger:verbose("Deleting "..w.path)
s.delete(w.path)end end;local u={}function u:clean(m,f)
return self:injectTask(l(self.env,m or"clean",f))end;local function c()r:include(u)end;return
{name="clean task",description="A task that deletes all specified files.",apply=c,CleanTask=l}end
a["howl.modules.plugins"]=function(...)local n=i"howl.class"local s=i"howl.class.mixin"
local h=i"howl.platform".fs
local r=n("howl.modules.plugins"):include(s.configurable)function r:initialize(l)self.context=l end;function r:configure(l)
if#l==0 then
self:addPlugin(l,l)else for u=1,#l do self:addPlugin(l[u])end end end
local function d(l,u)
local c=u:gsub("%.lua$",""):gsub("/","."):gsub("^(.*)%.init$","%1")if c==""or c=="init"then return l else return l.."."..c end end
function r:addPlugin(l)
if not l.type then error("No plugin type specified")end;local u=l.type;l.type=nil;local c;if l.file then c=l.file;l.file=nil end
local m=self.context.packageManager;local f=m:addPackage(u,l)
self.context.logger:verbose("Using plugin from package "..
f:getName())local w=f:require(c and{c})
local y="external."..f:getName()local p=0
for c,b in pairs(w)do
if c:find("%.lua$")then p=p+1;local g,k=loadfile(w[c],_ENV)
if g then
local q=d(y,c)a[q]=g
self.context.logger:verbose("Including plugin file "..c.." as "..q)else
self.context.logger:warning("Cannot load plugin file "..c..": "..k)end end end
if not c then
if w["init.lua"]then c="init.lua"elseif p==1 then c=next(w)elseif p==0 then
self.context.logger:error(
f:getName().." does not export any files")error("Error adding plugin")else
self.context.logger:error(
"Cannot guess a file for "..f:getName())error("Error adding plugin")end end
self.context.logger:verbose("Using package "..f:getName().." with "..c)local v=d(y,c)
if not a[v]then
self.context.logger:error("Cannot load plugin as "..
v.." could not be loaded")error("Error adding plugin")end;self.context:include(i(v))return self end
return
{name="plugins",description="Inject plugins into Howl at runtime.",setup=function(l)
l.mediator:subscribe({"HowlFile","env"},function(u)u.plugins=r(l)end)end}end
a["howl.modules.packages.pastebin"]=function(...)local n=i"howl.class"local s=i"howl.platform"
local h=i"howl.packages.Manager"local r=i"howl.packages.Package"
local d=r:subclass("howl.modules.packages.pastebin.PastebinPackage"):addOptions{"id"}
function d:setup(l)if not self.options.id then
self.context.logger:error("Pastebin has no ID")end end;function d:getName()return self.options.id end
function d:files(l)if l then return{}else return
{["init.lua"]=s.fs.combine(self.installDir,"init.lua")}end end
function d:require(l,u)local c=self.options.id;local m=self.installDir
if not u and l then return l end
local f,w=s.http.request("http://pastebin.com/raw/"..c)if not f or not w then
self.context.logger:error("Cannot find pastebin "..c)return l end;local y=w.readAll()
w.close()s.fs.write(s.fs.combine(m,"init.lua"),y)return
{}end
return
{name="pastebin package",description="Allows downloading a pastebin dependency.",apply=function()h:addProvider(d,"pastebin")end,PastebinPackage=d}end
a["howl.modules.packages.gist"]=function(...)local n=i"howl.class"local s=i"howl.lib.json"
local h=i"howl.platform"local r=i"howl.packages.Manager"local d=i"howl.packages.Package"
local l=d:subclass("howl.modules.packages.gist.GistPackage"):addOptions{"id"}
function l:setup(u)if not self.options.id then
self.context.logger:error("Gist has no ID")end end;function l:getName()return self.options.id end
function l:files(u)if u then local c={}
for m,f in
pairs(u.files)do c[m]=h.fs.combine(self.installDir,m)end;return c else return{}end end
function l:require(u,c)local m=self.options.id;local f=self.installDir
if not c and u then return u end
local w,y=h.http.request("https://api.github.com/gists/"..m)if not w or not y then
self.context.logger:error("Cannot find gist "..m)return false end;local p=y.readAll()
y.close()local v=s.decode(p)local b=v.history[1].version;local g
if
u and b==u.hash then g=u else g={hash=b,files={}}
for k,q in pairs(v.files)do if q.truncated then
self.context.logger:error(
"Skipping "..k.." as it is truncated")else h.fs.write(h.fs.combine(f,k),q.content)
g.files[k]=true end end end;return g end;return
{name="gist package",description="Allows downloading a gist dependency.",apply=function()r:addProvider(l,"gist")end,GistPackage=l}end
a["howl.modules.packages.file"]=function(...)local n=i"howl.class"local s=i"howl.class.mixin"
local h=i"howl.platform".fs;local r=i"howl.packages.Manager"local d=i"howl.packages.Package"
local l=i"howl.files.Source"
local u=d:subclass("howl.modules.packages.file.FilePackage"):include(s.filterable):include(s.delegate("sources",{"from","include","exclude"}))function u:initialize(c,m)d.initialize(self,c,m)self.sources=l(false)
self.name=tostring({}):sub(8)
self:exclude{".git",".svn",".gitignore",c.out}end
function u:setup(c)if
not self.sources:hasFiles()then
self.context.logger:error("No files specified")end end;function u:configure(c)d.configure(self,c)
self.sources:configure(c)end;function u:getName()return self.name end
function u:files(c)
local m={}for f,w in
pairs(self.sources:gatherFiles(self.context.root))do m[w.name]=w.path end;return m end;function u:require(c,m)end;return
{name="file package",description="Allows using a local file as a dependency",apply=function()
r:addProvider(u,"file")end,FilePackage=u}end
a["howl.modules.list"]=function(...)local n=i"howl.lib.assert"local s=i"howl.lib.colored"
local h=i"howl.tasks.Runner"local r={}
function r:listTasks(l,u)local c={}local m=0
for f,w in pairs(self.tasks)do local y=f:sub(1,1)if u or
(y~="_"and y~=".")then local p=w.options.description or""local v=#f
if v>m then m=v end;c[f]=p end end;m=m+2;l=l or""for f,w in pairs(c)do s.writeColor("white",l..f)
s.printColor("lightGray",string.rep(" ",
m-#f)..w)end;return self end;local function d()h:include(r)end;return
{name="list",description="List all tasks on a runner.",apply=d}end
a["howl.modules.dependencies.task"]=function(...)local n=i"howl.lib.assert"
local s=i"howl.tasks.Task"local h=i"howl.tasks.Dependency"
local r=h:subclass("howl.modules.dependencies.task.TaskDependency")function r:initialize(d,l)h.initialize(self,d)
n.argType(l,"string","initialize",1)self.name=l end
function r:setup(d,l)if not
l.tasks[self.name]then
d.logger:error("Task '%s': cannot resolve dependency '%s'",self.task.name,self.name)end end;function r:resolve(d,l)return l:run(self.name)end;return
{name="task dependency",description="Allows depending on a task.",apply=function()
s:addDependency(r,"depends")end,TaskDependency=r}end
a["howl.modules.dependencies.file"]=function(...)local n=i"howl.lib.assert"
local s=i"howl.tasks.Task"local h=i"howl.tasks.Dependency"
local r=h:subclass("howl.modules.dependencies.file.FileDependency")function r:initialize(d,l)h.initialize(self,d)
n.argType(l,"string","initialize",1)self.path=l end
function r:setup(d,l)end;function r:resolve(d,l)return l:DoRequire(self.path)end;return
{name="file dependency",description="Allows depending on a file.",apply=function()
s:addDependency(r,"requires")end,FileDependency=r}end
a["howl.loader"]=function(...)local n=i"howl.platform".fs;local s=i"howl.tasks.Runner"
local h=i"howl.lib.utils"local r={"Howlfile","Howlfile.lua"}
local function d()local c=n.currentDir()
while true do for m,f in
ipairs(r)do local w=n.combine(c,f)
if n.exists(w)and not n.isDir(w)then return f,c end end
if c=="/"or c==""then break end;c=n.getDir(c)end;return nil,
"Cannot find HowlFile. Looking for '"..table.concat(r,"', '").."'."end
local function l(c)local m=setmetatable(c or{},{__index=_ENV})function m.loadfile(f)return
assert(loadfile(f,m))end;function m.dofile(f)
return m.loadfile(f)()end;return m end
local function u(c,m)local f=s(c)
c.mediator:subscribe({"ArgParse","changed"},function(y)f.ShowTime=y:Get"time"
f.Traceback=y:Get"trace"end)
local w=l({require=i,CurrentDirectory=c.root,Tasks=f,Options=c.arguments,Verbose=c.logger/"verbose",Log=c.logger/"dump",File=function(...)
return n.combine(c.root,...)end})c.mediator:publish({"HowlFile","env"},w,c)
return f,w end;return{FindHowl=d,SetupEnvironment=l,SetupTasks=u,Names=r}end
a["howl.lib.utils"]=function(...)local n=i"howl.lib.assert"
local s={["^"]="%^",["$"]="%$",["("]="%(",[")"]="%)",["%"]="%%",["."]="%.",["["]="%[",["]"]="%]",["*"]="%*",["+"]="%+",["-"]="%-",["?"]="%?",["\0"]="%z"}local function h(w)return(w:gsub(".",s))end
local r={["^"]="%^",["$"]="%$",["("]="%(",[")"]="%)",["%"]="%%",["."]="%.",["["]="%[",["]"]="%]",["+"]="%+",["-"]="%-",["?"]="%?",["\0"]="%z"}
local function d(w,y)local p=w:sub(1,5)
if p=="ptrn:"or p=="wild:"then local w=w:sub(6)
if p=="wild:"then
if y then
local v=0
w=((w:gsub(".",r)):gsub("(%*)",function()v=v+1;return"%"..v end))else w="^"..
((w:gsub(".",r)):gsub("(%*)","(.*)")).."$"end end;return{Type="Pattern",Text=w}else return{Type="Normal",Text=w}end end;local function l(w)for y,p in ipairs(w)do w[p]=true end;return w end;local function u(w,y)
local p=#w;if p~=#y then return false end
for v=1,p do if w[v]~=y[v]then return false end end;return true end;local function c(w,y)
if w:sub(1,
#y)==y then return w:sub(#y+1)else return false end end
local function m(w,y)return
(w:gsub("${([^}]+)}",function(p)local v=y[p]if v==nil then return
"${"..p.."}"else return tostring(v)end end))end
local function f(w,y,p)n.argType(w,"string","deprecated",1)
n.argType(y,"function","deprecated",2)
if p~=nil then n.argType(p,"string","msg",4)p=" "..p else p=""end;local v=false
return
function(...)if not v then local b,g=pcall(error,"",3)g=g:gsub(":%s*$","")
print(w..
" is deprecated (called at "..g..")."..p)v=true end
return y(...)end end
return{escapePattern=h,parsePattern=d,createLookup=l,matchTables=u,startsWith=c,formatTemplate=m,deprecated=f}end
a["howl.lib.settings"]=function(...)local n=i"howl.platform"local s=n.fs;local h=i"howl.lib.dump"
local r={}
if s.exists(".howl.settings.lua")then local d=s.read(".howl.settings.lua")for l,u in
pairs(h.unserialise(d))do r[l]=u end end
if s.exists(".howl/settings.lua")then local d=s.read(".howl/settings.lua")for l,u in
pairs(h.unserialise(d))do r[l]=u end end
for d,l in pairs(r)do r[d]=n.os.getEnv("howl."..d,l)end;return r end
a["howl.lib.mediator"]=function(...)local n=i"howl.class"local s=i"howl.class.mixin"local function h()return
tonumber(tostring({}):match(':%s*[0xX]*(%x+)'),16)end
local r=n("howl.lib.mediator.Subscriber"):include(s.sealed)
function r:initialize(u,c)self.id=h()self.options=c or{}self.fn=u end;function r:update(u)self.fn=u.fn or self.fn
self.options=u.options or self.options end
local d=n("howl.lib.mediator.Channel"):include(s.sealed)function d:initialize(u,c)self.stopped=false;self.namespace=u;self.callbacks={}
self.channels={}self.parent=c end
function d:addSubscriber(u,c)
local m=r(u,c)local f=(#self.callbacks+1)c=c or{}
if c.priority and
c.priority>=0 and c.priority<f then f=c.priority end;table.insert(self.callbacks,f,m)return m end
function d:getSubscriber(u)for m=1,#self.callbacks do local f=self.callbacks[m]if f.id==u then
return{index=m,value=f}end end;local c
for m,f in
pairs(self.channels)do c=f:getSubscriber(u)if c then break end end;return c end
function d:setPriority(u,c)local m=self:getSubscriber(u)
if m.value then
table.remove(self.callbacks,m.index)table.insert(self.callbacks,c,m.value)end end
function d:addChannel(u)self.channels[u]=d(u,self)return self.channels[u]end
function d:hasChannel(u)return u and self.channels[u]and true end;function d:getChannel(u)
return self.channels[u]or self:addChannel(u)end
function d:removeSubscriber(u)
local c=self:getSubscriber(u)
if c and c.value then
for m,f in pairs(self.channels)do f:removeSubscriber(u)end;return table.remove(self.callbacks,c.index)end end
function d:publish(u,...)
for c=1,#self.callbacks do local m=self.callbacks[c]
if
not m.options.predicate or m.options.predicate(...)then local f,w=m.fn(...)if
w~=nil then table.insert(u,w)end
if f==false then return false,u end end end
if self.parent then return self.parent:publish(u,...)else return true,u end end
local l=setmetatable({Channel=d,Subscriber=r},{__call=function(u,c)
return
{channel=d('root'),getChannel=function(m,f)local w=m.channel
for y=1,#f do w=w:getChannel(f[y])end;return w end,subscribe=function(m,f,u,c)return
m:getChannel(f):addSubscriber(u,c)end,getSubscriber=function(m,f,w)return
m:getChannel(w):getSubscriber(f)end,removeSubscriber=function(m,f,w)return
m:getChannel(w):removeSubscriber(f)end,publish=function(m,f,...)return
m:getChannel(f):publish({},...)end}end})return l()end
a["howl.lib.Logger"]=function(...)local n=i"howl.class"local s=i"howl.class.mixin"
local h=i"howl.lib.dump".dump;local r=i"howl.lib.colored"local d=i"howl.platform".log
local l,u=select,tostring;local function c(...)local y={}for p=1,l('#',...)do y[p]=u(l(p,...))end;return
table.concat(y," ")end
local m=n("howl.lib.Logger"):include(s.sealed):include(s.curry)function m:initialize(y)self.isVerbose=false
y.mediator:subscribe({"ArgParse","changed"},function(p)self.isVerbose=
p:Get"verbose"or false end)end
function m:verbose(...)if
self.isVerbose then local y,p=pcall(function()error("",4)end)
r.writeColor("gray",p)r.printColor("lightGray",...)
d("verbose",p..c(...))end end
function m:dump(...)
if self.isVerbose then
local y,p=pcall(function()error("",4)end)r.writeColor("gray",p)local v=l('#',...)local b={...}for g=1,v do local k=b[g]
local q=type(k)if q=="table"then k=h(k)else k=u(k)end;if g>1 then k=" "..k end
r.writeColor("lightGray",k)end
print()end end
local f={{"success","ok","green"},{"error","error","red"},{"info","info","cyan"},{"warning","warn","yellow"}}local w=0;for y,p in ipairs(f)do w=math.max(w,#p[2])end
for y,p in ipairs(f)do
local v=p[3]
local b='['..p[2]..']'.. (' '):rep(w-#p[2]+1)
local g="has"..p[2]:gsub("^%l",string.upper)local k=p[1]
m[k]=function(q,j,...)q[g]=true;r.writeColor(v,b)local x;if type(j)=="string"then
x=j:format(...)else end;r.printColor(v,x)d(k,x)end end;return m end
a["howl.lib.json"]=function(...)
local n={["\n"]="\\n",["\r"]="\\r",["\t"]="\\t",["\b"]="\\b",["\f"]="\\f",["\""]="\\\"",["\\"]="\\\\"}
local function s(j)local x=0
for z,_ in pairs(j)do if type(z)~="number"then return false elseif z>x then x=z end end;return x==#j end
local function h(j,x,z,_,E)local T=""
local function A(I)T=T.. ("\t"):rep(z)..I end
local function O(j,I,N,S,H)T=T..I;if x then T=T.."\n"z=z+1 end;for R,D in S(j)do A("")H(R,D)T=T..","if x then
T=T.."\n"end end;if x then z=z-1 end
if
T:sub(-2)==",\n"then T=T:sub(1,-3).."\n"elseif T:sub(-1)==","then T=T:sub(1,-2)end;A(N)end
if type(j)=="table"then
assert(not _[j],"Cannot encode a table holding itself recursively")_[j]=true
if s(j)then
O(j,"[","]",ipairs,function(I,N)T=T..h(N,x,z,_)end)else
O(j,"{","}",pairs,function(I,N)
assert(type(I)=="string","JSON object keys must be strings",2)T=T..h(I,x,z,_)
T=T.. (x and": "or":")..h(N,x,z,_,I)end)end elseif type(j)=="string"then
T='"'..j:gsub("[%c\"\\]",n)..'"'elseif type(j)=="number"or type(j)=="boolean"then T=tostring(j)else
error(
"JSON only supports arrays, objects, numbers, booleans, and strings, got "..type(j).." in "..tostring(E),2)end;return T end;local function r(j)return h(j,false,0,{})end
local function d(j)return h(j,true,0,{})end
local l={['\n']=true,['\r']=true,['\t']=true,[' ']=true,[',']=true,[':']=true}
local function u(j)while l[j:sub(1,1)]do j=j:sub(2)end;return j end;local c={}for j,x in pairs(n)do c[x]=j end
local function m(j)if j:sub(1,4)=="true"then
return true,u(j:sub(5))else return false,u(j:sub(6))end end;local function f(j)return nil,u(j:sub(5))end
local w={['e']=true,['E']=true,['+']=true,['-']=true,['.']=true}
local function y(j)local x=1
while w[j:sub(x,x)]or tonumber(j:sub(x,x))do x=x+1 end;local z=tonumber(j:sub(1,x-1))j=u(j:sub(x))return z,j end
local function p(j)j=j:sub(2)local x=""
while j:sub(1,1)~="\""do local z=j:sub(1,1)j=j:sub(2)
assert(z~="\n","Unclosed string")if z=="\\"then local _=j:sub(1,1)j=j:sub(2)
z=assert(c[z.._],"Invalid escape character")end;x=x..z end;return x,u(j:sub(2))end;local v
local function b(j)j=u(j:sub(2))local x={}local z=1;while j:sub(1,1)~="]"do local _=nil;_,j=v(j)x[z]=_;z=
z+1;j=u(j)end;j=u(j:sub(2))return x,j end;local function g(j)local x=nil;x,j=v(j)local z=nil;z,j=v(j)return x,z,j end
local function k(j)
j=u(j:sub(2))local x={}
while j:sub(1,1)~="}"do local z,_=nil,nil;z,_,j=g(j)x[z]=_;j=u(j)end;j=u(j:sub(2))return x,j end
function v(j)local x=j:sub(1,1)
if x=="{"then return k(j)elseif x=="["then return b(j)elseif
tonumber(x)~=nil or w[x]then return y(j)elseif j:sub(1,4)=="true"or j:sub(1,5)=="false"then
return m(j)elseif x=="\""then return p(j)elseif j:sub(1,4)=="null"then return f(j)end;return nil end;local function q(j)j=u(j)return v(j)end;return{encode=r,encodePretty=d,decode=q}end
a["howl.lib.dump"]=function(...)local n=i("howl.lib.Buffer")
local s=i("howl.lib.utils").createLookup;local h,r,d=type,tostring,string.format;local l,u=getmetatable,error
local function c(v,b,g,k)local q=h(v)
if
q=="table"and not b[v]then b[v]=true
if next(v)==nil then return"{}"else local j=false;local x=#v;local z=0
for S,H in
pairs(v)do
if h(S)=="table"or h(H)=="table"then j=true;break elseif
h(S)=="number"and S>=1 and S<=x and S%1 ==0 then z=z+#r(H)+2 else z=
z+#r(H)+#r(S)+2 end;if z>40 then j=true;break end end;local _,E,T="",", "," "if j then _="\n"E=",\n"T=g.." "end;local A,O={
(k and"("or"{").._},1;local I={}local N=true
for S=1,x do I[S]=true;O=O+1;local H=T..
c(v[S],b,T)if not N then H=E..H else N=false end;A[O]=H end
for S,H in pairs(v)do
if not I[S]then local R;if
h(S)=="string"and string.match(S,"^[%a_][%a%d_]*$")then R=S.." = "..c(H,b,T)else
R="["..c(S,b,T).."] = "..c(H,b,T)end;R=T..R;if not N then
R=E..R else N=false end;O=O+1;A[O]=R end end;O=O+1;A[O]=_..g.. (k and")"or"}")return
table.concat(A)end elseif q=="string"then return
(string.format("%q",v):gsub("\\\n","\\n"))else return r(v)end end;local function m(v,b)return c(v,{},"",b)end
local f=s{"and","break","do","else","elseif","end","false","for","function","if","in","local","nil","not","or","repeat","return","then","true","until","while"}
local function w(v,b,g)local k=h(v)
if k=="table"then if b[v]then
u("Cannot serialise table with recursive entries",1)end;b[v]=true
if next(v)==nil then
g:append("{}")else g:append("{")local q={}
for j,x in ipairs(v)do q[j]=true;w(x,b,g)g:append(",")end
for j,x in pairs(v)do
if not q[j]then
if
h(j)=="string"and not f[j]and j:match("^[%a_][%a%d_]*$")then g:append(j.."=")else g:append("[")w(j,b,g)g:append("]=")end;w(x,b,g)g:append(",")end end;g:append("}")end elseif k=="string"then g:append(d("%q",v))elseif k=="number"or k=="boolean"or
k=="nil"then g:append(r(v))else
u("Cannot serialise type "..k)end;return g end;local function y(v)return w(v,{},n()):toString()end
local function p(v)local b=loadstring(
"return "..v,"unserialise-temp",nil,{})if
not b then return nil end;local g,k=pcall(b)return g and k end;return{serialise=y,unserialise=p,deserialise=p,dump=m}end
a["howl.lib.colored"]=function(...)local n=i"howl.platform".term;local function s(r,...)n.setColor(r)
n.print(...)n.resetColor(r)end;local function h(r,d)n.setColor(r)
n.write(d)n.resetColor(r)end
return{printColor=s,writeColor=h}end
a["howl.lib.Buffer"]=function(...)local n=table.concat
local function s(r,d)local l=r.n+1;r[l]=d;r.n=l;return r end;local function h(r)return n(r)end;return
function()return{n=0,append=s,toString=h}end end
a["howl.lib.assert"]=function(...)local n,s,h,r=type,error,select,math.floor;local d=assert
local l=setmetatable({assert=d},{__call=function(m,...)return
d(...)end})
local function u(n,m,f)if f then return s(f:format(n))else
return s(m.." expected, got "..n)end end
function l.type(m,f,w)local y=n(m)if y~=f then return u(y,f,w)end end
local function c(n,m,f,w)return
s("bad argument #"..w.." for "..
f.." (expected "..m..", got "..n..")")end
function l.argType(m,f,w,y)local p=n(m)if p~=f then return c(p,f,w,y)end end
function l.args(m,...)local f=r('#',...)local w={...}
for y=1,f,2 do local p=n(w[i])local v=w[i+1]if p~=v then return
c(p,v,m,math.floor(y/2))end end end;l.typeError=u;l.argError=c
function l.class(m,f,w)local y=n(m)
if
y~="table"or not m.isInstanceOf then return u(y,f,w)elseif not m:isInstanceOf(f)then return u(m.class.name,f,w)end end;return l end
a["howl.lib.argparse"]=function(...)local n=i"howl.lib.colored"
local s={__index=function(d,l)
return function(d,...)local u=d.parser
local c=u[l](u,d.name,...)if c==u then return d end;return c end end}local h={}
function h:Get(d,l)local u=self.options;local c=u[d]if c~=nil then return c end
local m=self.settings[d]
if m then local f=m.aliases;if f then
for w,y in ipairs(f)do c=u[y]if c~=nil then return c end end end;c=m.default;if c~=nil then return c end end;return l end;function h:Ensure(d)local l=self:Get(d)
if l==nil then error(d.." must be set")end;return l end;function h:Default(d,l)
if l==nil then l=true end;self:_SetSetting(d,"default",l)self:_Changed()
return self end
function h:Alias(d,l)
local u=self.settings;local c=u[d]if c then local m=c.aliases
if m==nil then c.aliases={l}else table.insert(m,l)end else u[d]={aliases={l}}end
self:_Changed()return self end
function h:Description(d,l)return self:_SetSetting(d,"description",l)end;function h:TakesValue(d,l)if l==nil then l=true end
return self:_SetSetting(d,"takesValue",l)end
function h:_SetSetting(d,l,u)local c=self.settings
local m=c[d]if m then m[l]=u else c[d]={[l]=u}end;return self end
function h:Option(d)return setmetatable({name=d,parser=self},s)end;function h:Arguments()return self.arguments end;function h:_Changed()
self.mediator:publish({"ArgParse","changed"},self)end
function h:Help(d)
for l,u in pairs(self.settings)do local c='-'if
u.takesValue then c="--"l=l.."=value"end;if#l>1 then c='--'end;n.writeColor("white",d..c..
l)local m=""local f=u.aliases
if f and#f>0 then local y=#f
m=m.." ("for p=1,y do local v="-"..f[p]if#v>2 then v="-"..v end;if p<y then v=v..', 'end
m=m..v end;m=m..")"end;n.writeColor("brown",m)local w=u.description;if w and w~=""then
n.printColor("lightGray"," "..w)end end end
function h:Parse(d)local l=self.options;local u=self.arguments
for c,m in ipairs(d)do
if m:sub(1,1)=="-"then
if
m:sub(2,2)=="-"then local f,w=m:match("([%w_%-]+)=([%w_%-]+)",3)if f then l[f]=w else
m=m:sub(3)local y=m:sub(1,4)local w=true
if y=="not-"or y=="not_"then w=false;m=m:sub(5)end;l[m]=w end else for f=2,#m do
l[m:sub(f,f)]=true end end else table.insert(u,m)end end;return self end
local function r(d,l)return
setmetatable({options={},arguments={},mediator=d,settings={}},{__index=h}):Parse(l)end;return{Parser=h,Options=r}end
a["howl.lexer.walk"]=function(...)local function n()end;local function s(l,u)u(l.Base)
for c,m in ipairs(l.Arguments)do u(m)end end
local function h(l,u)u(l.Base)u(l.Index)end;local r
local function d(l,u)local c=r[l.AstType]if not c then
error("No visitor for "..l.AstType)end;c(l,u)end
r={VarExpr=n,NumberExpr=n,StringExpr=n,BooleanExpr=n,NilExpr=n,DotsExpr=n,Eof=n,BinopExpr=function(l,u)u(l.Lhs)u(l.Rhs)end,UnopExpr=function(l,u)u(l.Rhs)end,CallExpr=s,TableCallExpr=s,StringCallExpr=s,IndexExpr=h,MemberExpr=h,Function=function(l,u)if l.Name and not
l.IsLocal then u(l.Name)end;u(l.Body)end,ConstructorExpr=function(l,u)
for c,m in
ipairs(l.EntryList)do if m.Type=="Key"then u(m.Key)end;u(m.Value)end end,Parentheses=function(l,u)u(v.Inner)end,Statlist=function(l,u)for c,m in
ipairs(l.Body)do u(m)end end,ReturnStatement=function(l,u)
for c,m in ipairs(l.Arguments)do u(m)end end,AssignmentStatement=function(l,u)for c,m in ipairs(l.Lhs)do u(m)end;for c,m in
ipairs(l.Rhs)do u(m)end end,LocalStatement=function(l,u)for c,m in
ipairs(l.InitList)do u(m)end end,CallStatement=function(l,u)u(v.Expression)end,IfStatement=function(l,u)
for c,m in
ipairs(l.Clauses)do if m.Condition then u(m.Condition)end;u(m.Body)end end,WhileStatement=function(l,u)u(l.Condition)
u(l.Body)end,DoStatement=function(l,u)u(l.Body)end,BreakStatement=n,LabelStatement=n,GotoStatement=n,RepeatStatement=function(l,u)u(l.Body)
u(l.Condition)end,GenericForStatement=function(l,u)for c,m in ipairs(l.Generators)do u(m)end
u(l.Body)end,NumericForStatement=function(l,u)u(l.Start)u(l.End)if l.Step then
u(l.Step)end;u(l.Body)end}return d end
a["howl.lexer.TokenList"]=function(...)local n=math.min;local s=table.insert
return
function(h)local r=#h;local d=1;local function l(b)return h[n(r,d+
(b or 0))]end;local function u(b)local g=h[d]
d=n(d+1,r)if b then s(b,g)end;return g end;local function c(b)
return l().Type==b end
local function m(b,g)local k=l()
if k.Type=='Symbol'then if b then if k.Data==b then if g then s(g,k)end
d=d+1;return true else return nil end else if g then s(g,k)end;d=d+1
return k end else return nil end end
local function f(b,g)local k=l()if k.Type=='Keyword'and k.Data==b then if g then s(g,k)end;d=d+1
return true else return nil end end
local function w(b)local g=l()return g.Type=='Keyword'and g.Data==b end
local function y(b)local g=l()return g.Type=='Symbol'and g.Data==b end;local function p()return l().Type=='Eof'end
local function v(b)
b=(b==nil and true or b)local g=""
for k,q in ipairs(h)do if b then
for k,j in ipairs(q.LeadingWhite)do g=g..j:Print().."\n"end end;g=g..q:Print().."\n"end;return g end
return{Peek=l,Get=u,Is=c,ConsumeSymbol=m,ConsumeKeyword=f,IsKeyword=w,IsSymbol=y,IsEof=p,Print=v,Tokens=h}end end
a["howl.lexer.Scope"]=function(...)local n=i"howl.lexer.constants".Keywords;local s={}function s:AddLocal(r,d)
table.insert(self.Locals,d)self.LocalMap[r]=d end
function s:CreateLocal(r)
local d=self:GetLocal(r)if d then return d end
d={Scope=self,Name=r,IsGlobal=false,CanRename=true,References=1}self:AddLocal(r,d)return d end;function s:GetLocal(r)
repeat local d=self.LocalMap[r]if d then return d end;self=self.Parent until not self end;function s:GetOldLocal(r)if
self.oldLocalNamesMap[r]then return self.oldLocalNamesMap[r]end;return
self:GetLocal(r)end
function s:RenameLocal(r,d)r=
type(r)=='string'and r or r.Name
repeat
local l=self.LocalMap[r]
if l then l.Name=d;self.oldLocalNamesMap[r]=l
self.LocalMap[r]=nil;self.LocalMap[d]=l;break end;self=self.Parent until not self end;function s:AddGlobal(r,d)table.insert(self.Globals,d)
self.GlobalMap[r]=d end
function s:CreateGlobal(r)
local d=self:GetGlobal(r)if d then return d end
d={Scope=self,Name=r,IsGlobal=true,CanRename=true,References=1}self:AddGlobal(r,d)return d end
function s:GetGlobal(r)repeat local d=self.GlobalMap[r]if d then return d end;self=self.Parent until
not self end;function s:GetVariable(r)
return self:GetLocal(r)or self:GetGlobal(r)end;function s:GetAllVariables()return
self:getVars(true,self:getVars(true))end
function s:getVars(r,d)
local d=d or{}
if r then for l,u in pairs(self.Children)do u:getVars(true,d)end else for l,u in
pairs(self.Locals)do table.insert(d,u)end;for l,u in pairs(self.Globals)do
table.insert(d,u)end;if self.Parent then
self.Parent:getVars(false,d)end end;return d end
function s:ObfuscateLocals(r)
local d=r or"etaoinshrdlucmfwypvbgkqjxz_ETAOINSHRDLUCMFWYPVBGKQJXZ"
local l=r or"etaoinshrdlucmfwypvbgkqjxz_0123456789ETAOINSHRDLUCMFWYPVBGKQJXZ"local u,c=#d,#l;local m=0;local f=math.floor
for w,y in pairs(self.Locals)do local p
repeat
if m<u then m=m+1
p=d:sub(m,m)else
if m<u then m=m+1;p=d:sub(m,m)else local v=f(m/u)local b=m%u;p=d:sub(b,b)while v>0 do b=v%c;p=
l:sub(b,b)..p;v=f(v/c)end;m=m+1 end end until not(n[p]or self:GetVariable(p))self:RenameLocal(y.Name,p)end end;function s:ToString()return'<Scope>'end
local function h(r)
local d=setmetatable({Parent=r,Locals={},LocalMap={},Globals={},GlobalMap={},oldLocalNamesMap={},Children={}},{__index=s})if r then table.insert(r.Children,d)end;return d end;return h end
a["howl.lexer.rebuild"]=function(...)local n=i"howl.lexer.constants"local s=i"howl.lexer.parse"
local h=i"howl.platform"local r=n.LowerChars;local d=n.UpperChars;local l=n.Digits;local u=n.Symbols
local function c(y,p,v)v=v or' '
local b,g=y:sub(-1,-1),p:sub(1,1)
if d[b]or r[b]or b=='_'then
if not
(g=='_'or d[g]or r[g]or l[g])then return y..p else return y..v..p end elseif l[b]then
if g=='('then return y..p elseif u[g]then return y..p else return y..v..p end elseif b==''then return y..p else if g=='('then return y..v..p else return y..p end end end
local function m(y)local p,v;local b=0;local function g(q,j,x)
if b>150 then b=0;return q.."\n"..j else return c(q,j,x)end end
v=function(q,j)local j=j or 0;local x=0;local z=false;local _=""
if
q.AstType=='VarExpr'then
if q.Variable then _=_..q.Variable.Name else _=_..q.Name end elseif q.AstType=='NumberExpr'then _=_..q.Value.Data elseif q.AstType=='StringExpr'then _=_..
q.Value.Data elseif q.AstType=='BooleanExpr'then
_=_..tostring(q.Value)elseif q.AstType=='NilExpr'then _=g(_,"nil")elseif q.AstType=='BinopExpr'then
x=q.OperatorPrecedence;_=g(_,v(q.Lhs,x))_=g(_,q.Op)_=g(_,v(q.Rhs))if q.Op=='^'or q.Op==
'..'then x=x-1 end;if x<j then z=false else z=true end elseif
q.AstType=='UnopExpr'then _=g(_,q.Op)_=g(_,v(q.Rhs))elseif q.AstType=='DotsExpr'then _=_..
"..."elseif q.AstType=='CallExpr'then _=_..v(q.Base)_=_.."("for E=1,#q.Arguments do _=
_..v(q.Arguments[E])
if E~=#q.Arguments then _=_..","end end;_=_..")"elseif
q.AstType=='TableCallExpr'then _=_..v(q.Base)_=_..v(q.Arguments[1])elseif q.AstType==
'StringCallExpr'then _=_..v(q.Base)
_=_..q.Arguments[1].Data elseif q.AstType=='IndexExpr'then _=_..
v(q.Base).."["..v(q.Index).."]"elseif q.AstType=='MemberExpr'then _=_..v(q.Base)..q.Indexer..
q.Ident.Data elseif q.AstType==
'Function'then q.Scope:ObfuscateLocals()_=_.."function("
if#
q.Arguments>0 then for E=1,#q.Arguments do _=_..q.Arguments[E].Name
if E~=#
q.Arguments then _=_..","elseif q.VarArg then _=_..",..."end end elseif q.VarArg then
_=_.."..."end;_=_..")"_=g(_,p(q.Body))_=g(_,"end")elseif
q.AstType=='ConstructorExpr'then _=_.."{"
for E=1,#q.EntryList do local T=q.EntryList[E]
if T.Type=='Key'then _=_.."["..v(T.Key).."]="..
v(T.Value)elseif
T.Type=='Value'then _=_..v(T.Value)elseif T.Type=='KeyString'then _=_..T.Key..
"="..v(T.Value)end;if E~=#q.EntryList then _=_..","end end;_=_.."}"elseif q.AstType=='Parentheses'then
_=_.."("..v(q.Inner)..")"end;if not z then
_=string.rep('(',q.ParenCount or 0).._
_=_..string.rep(')',q.ParenCount or 0)end;b=b+#_;return _ end
local k=function(q)local j=''
if q.AstType=='AssignmentStatement'then for x=1,#q.Lhs do j=j..v(q.Lhs[x])if x~=#
q.Lhs then j=j..","end end
if
#q.Rhs>0 then j=j.."="for x=1,#q.Rhs do j=j..v(q.Rhs[x])
if x~=#q.Rhs then j=j..","end end end elseif q.AstType=='CallStatement'then j=v(q.Expression)elseif
q.AstType=='LocalStatement'then j=j.."local "
for x=1,#q.LocalList do
j=j..q.LocalList[x].Name;if x~=#q.LocalList then j=j..","end end;if#q.InitList>0 then j=j.."="
for x=1,#q.InitList do
j=j..v(q.InitList[x])if x~=#q.InitList then j=j..","end end end elseif
q.AstType=='IfStatement'then j=g("if",v(q.Clauses[1].Condition))
j=g(j,"then")j=g(j,p(q.Clauses[1].Body))
for x=2,#q.Clauses do
local z=q.Clauses[x]if z.Condition then j=g(j,"elseif")j=g(j,v(z.Condition))
j=g(j,"then")else j=g(j,"else")end
j=g(j,p(z.Body))end;j=g(j,"end")elseif q.AstType=='WhileStatement'then
j=g("while",v(q.Condition))j=g(j,"do")j=g(j,p(q.Body))j=g(j,"end")elseif
q.AstType=='DoStatement'then j=g(j,"do")j=g(j,p(q.Body))j=g(j,"end")elseif
q.AstType=='ReturnStatement'then j="return"
for x=1,#q.Arguments do j=g(j,v(q.Arguments[x]))if x~=#
q.Arguments then j=j..","end end elseif q.AstType=='BreakStatement'then j="break"elseif q.AstType=='RepeatStatement'then j="repeat"
j=g(j,p(q.Body))j=g(j,"until")j=g(j,v(q.Condition))elseif q.AstType=='Function'then
q.Scope:ObfuscateLocals()if q.IsLocal then j="local"end;j=g(j,"function ")if q.IsLocal then
j=j..q.Name.Name else j=j..v(q.Name)end;j=j.."("
if
#q.Arguments>0 then
for x=1,#q.Arguments do j=j..q.Arguments[x].Name;if
x~=#q.Arguments then j=j..","elseif q.VarArg then j=j..",..."end end elseif q.VarArg then j=j.."..."end;j=j..")"j=g(j,p(q.Body))j=g(j,"end")elseif
q.AstType=='GenericForStatement'then q.Scope:ObfuscateLocals()j="for "for x=1,#q.VariableList do j=j..
q.VariableList[x].Name
if x~=#q.VariableList then j=j..","end end;j=j.." in"
for x=1,#q.Generators do
j=g(j,v(q.Generators[x]))if x~=#q.Generators then j=g(j,',')end end;j=g(j,"do")j=g(j,p(q.Body))j=g(j,"end")elseif
q.AstType=='NumericForStatement'then q.Scope:ObfuscateLocals()j="for "
j=j..q.Variable.Name.."="j=j..v(q.Start)..","..v(q.End)if q.Step then j=j..","..
v(q.Step)end;j=g(j,"do")
j=g(j,p(q.Body))j=g(j,"end")elseif q.AstType=='LabelStatement'then
j="::"..q.Label.."::"elseif q.AstType=='GotoStatement'then j="goto "..q.Label elseif q.AstType=='Comment'then elseif q.AstType==
'Eof'then else
error("Unknown AST Type: "..q.AstType)end;b=b+#j;return j end
p=function(q)local j=''q.Scope:ObfuscateLocals()for x,z in pairs(q.Body)do
j=g(j,k(z),';')end;return j end;return p(y)end
local function f(y)local p=s.LexLua(y)h.refreshYield()local v=s.ParseLua(p)
h.refreshYield()local b=m(v)h.refreshYield()return b end;local function w(y,p,v)v=v or p;local b=h.fs.read(h.fs.combine(y,p))local g=f(b)
h.fs.write(h.fs.combine(y,v),g)return#b,#g end;return
{minify=m,minifyString=f,minifyFile=w}end
a["howl.lexer.parse"]=function(...)local n=i"howl.lexer.constants"local s=i"howl.lexer.Scope"
local h=i"howl.lexer.TokenList"local r=n.LowerChars;local d=n.UpperChars;local l=n.Digits;local u=n.Symbols;local c=n.HexDigits
local m=n.Keywords;local f=n.StatListCloseKeywords;local w=n.UnOps;local y,p=table.insert,setmetatable
local v={}
function v:Print()return
"<".. (self.Type..
string.rep(' ',math.max(3,12-#self.Type))).."  "..
(self.Data or'').." >"end;local b={__index=v}
local function g(q)local j={}
do local z=string.sub;local _=1;local E=1;local T=1
local function A()local R=z(q,_,_)if R=='\n'then
T=1;E=E+1 else T=T+1 end;_=_+1;return R end;local function O(R)R=R or 0;return z(q,_+R,_+R)end
local function I(R)local D=O()for L=1,#R do if
D==R:sub(L,L)then return A()end end end;local function N(R)
error(">> :"..E..":"..T..": "..R,0)end
local function S()local R=_
if O()=='['then local D=0;local L=1
while O(D+1)=='='do D=D+1 end
if O(D+1)=='['then for F=0,D+1 do A()end;local U=_
while true do if O()==''then
N("Expected `]"..string.rep('=',D)..
"]` near <eof>.",3)end;local F=true
if O()==']'then for W=1,D do
if O(W)~='='then F=false end end;if O(D+1)~=']'then F=false end else
if O()=='['then local W=true;for Y=1,D do if
O(Y)~='='then W=false;break end end;if
O(D+1)=='['and W then L=L+1;for Y=1,(D+2)do A()end end end;F=false end
if F then L=L-1;if L==0 then break else for W=1,D+2 do A()end end else A()end end;local C=q:sub(U,_-1)for F=0,D+1 do A()end;local M=q:sub(R,_-1)return C,M else return nil end else return nil end end;local function H(R)return R>='0'and R<='9'end
while true do local R,D
while true do local F=z(q,_,_)
if F==
'#'and O(1)=='!'and E==1 then A()A()leadingWhite="#!"while O()~='\n'and
O()~=''do A()end end
if F==' 'or F=='\t'then T=T+1;_=_+1 elseif F=='\n'or F=='\r'then T=1;E=E+1;_=_+1 elseif F=='-'and
O(1)=='-'then A()A()local W,Y,P=E,T,_;local V,B=S()
if not V then local G=z(q,_,_)while G~='\n'and G~=''do A()
G=z(q,_,_)end;V=z(q,P,_-1)end;if not R then R={}D=0 end;D=D+1;R[D]={Data=V,Line=W,Char=Y}else break end end;local L=E;local U=T;local C=z(q,_,_)local M=nil
if C==''then M={Type='Eof'}elseif

(C>='A'and C<='Z')or(C>='a'and C<='z')or C=='_'then local F=_
repeat A()C=z(q,_,_)until not
(
(C>='A'and C<='Z')or(C>='a'and C<='z')or C=='_'or(C>='0'and C<='9'))local W=q:sub(F,_-1)if m[W]then M={Type='Keyword',Data=W}else
M={Type='Ident',Data=W}end elseif(C>='0'and C<='9')or
(C=='.'and l[O(1)])then local F=_
if C=='0'and O(1)=='x'then A()A()while c[O()]do A()end;if
I('Pp')then I('+-')while l[O()]do A()end end else while l[O()]do A()end;if
I('.')then while l[O()]do A()end end
if I('Ee')then I('+-')if not l[O()]then
N("Expected exponent")end;repeat A()until not l[O()]end;local W=O():lower()if(W>='a'and W<='z')or W=='_'then
N("Invalid number format")end end;M={Type='Number',Data=q:sub(F,_-1)}elseif C=='\''or C=='\"'then
local F=_;local W=A()local Y=_
while true do local C=A()if C=='\\'then A()elseif C==W then break elseif C==''then
N("Unfinished string near <eof>")end end;local P=q:sub(Y,_-2)local V=q:sub(F,_-1)
M={Type='String',Data=V,Constant=P}elseif C=='['then local F,W=S()if W then M={Type='String',Data=W,Constant=F}else A()
M={Type='Symbol',Data='['}end elseif
C=='>'or C=='<'or C=='='then A()
if I('=')then M={Type='Symbol',Data=C..'='}else M={Type='Symbol',Data=C}end elseif C=='~'then A()if I('=')then M={Type='Symbol',Data='~='}else
N("Unexpected symbol `~` in source.",2)end elseif C=='.'then A()if I('.')then if I('.')then
M={Type='Symbol',Data='...'}else M={Type='Symbol',Data='..'}end else
M={Type='Symbol',Data='.'}end elseif C==':'then A()
if
I(':')then M={Type='Symbol',Data='::'}else M={Type='Symbol',Data=':'}end elseif u[C]then A()M={Type='Symbol',Data=C}else local F,W=S()if F then
M={Type='String',Data=W,Constant=F}else
N("Unexpected Symbol `"..C.."` in source.",2)end end;M.Line=L;M.Char=U;if R then M.Comments=R end;j[#j+1]=M
if M.Type=='Eof'then break end end end;local x=h(j)return x end
local function k(q,j)
local function x(H)
local R=q.Peek().Line..":"..q.Peek().Char..": "..H.."\n"local D=q.Peek()R=R..
" got "..D.Type..": "..D.Data.."\n"local L=0
if type(j)=='string'then
for U in
j:gmatch("[^\n]*\n?")do if U:sub(-1,-1)=='\n'then U=U:sub(1,-2)end;L=L+1;if L==
q.Peek().Line then
R=R..""..U:gsub('\t','    ').."\n"for C=1,q.Peek().Char do local M=U:sub(C,C)R=R..' 'end
R=R.."^"break end end end;error(R)end;local z,_,E,T,A
local function O(H,R)local D=s(H)if not q.ConsumeSymbol('(',R)then
x("`(` expected.")end;local L={}local U=false
while
not q.ConsumeSymbol(')',R)do
if q.Is('Ident')then local M=D:CreateLocal(q.Get(R).Data)
L[#L+1]=M
if not q.ConsumeSymbol(',',R)then if q.ConsumeSymbol(')',R)then break else
x("`)` expected.")end end elseif q.ConsumeSymbol('...',R)then U=true;if not q.ConsumeSymbol(')',R)then
x("`...` must be the last argument of a function.")end;break else
x("Argument name or `...` expected")end end;local C=_(D)if not q.ConsumeKeyword('end',R)then
x("`end` expected after function body")end;return
{AstType='Function',Scope=D,Arguments=L,Body=C,VarArg=U,Tokens=R}end
function T(H)local R={}
if q.ConsumeSymbol('(',R)then local D=z(H)if not q.ConsumeSymbol(')',R)then
x("`)` Expected.")end
return{AstType='Parentheses',Inner=D,Tokens=R}elseif q.Is('Ident')then local D=q.Get(R)local L=H:GetLocal(D.Data)
if not L then
L=H:GetGlobal(D.Data)
if not L then L=H:CreateGlobal(D.Data)else L.References=L.References+1 end else L.References=L.References+1 end;return{AstType='VarExpr',Name=D.Data,Variable=L,Tokens=R}else
x("primary expression expected")end end
function A(H,R)local D=T(H)
while true do local L={}
if q.IsSymbol('.')or q.IsSymbol(':')then
local U=q.Get(L).Data
if not q.Is('Ident')then x("<Ident> expected.")end;local C=q.Get(L)
D={AstType='MemberExpr',Base=D,Indexer=U,Ident=C,Tokens=L}elseif not R and q.ConsumeSymbol('[',L)then local U=z(H)if
not q.ConsumeSymbol(']',L)then x("`]` expected.")end
D={AstType='IndexExpr',Base=D,Index=U,Tokens=L}elseif not R and q.ConsumeSymbol('(',L)then local U={}
while
not q.ConsumeSymbol(')',L)do U[#U+1]=z(H)
if not q.ConsumeSymbol(',',L)then if q.ConsumeSymbol(')',L)then break else
x("`)` Expected.")end end end;D={AstType='CallExpr',Base=D,Arguments=U,Tokens=L}elseif
not R and q.Is('String')then
D={AstType='StringCallExpr',Base=D,Arguments={q.Get(L)},Tokens=L}elseif not R and q.IsSymbol('{')then local U=E(H)
D={AstType='TableCallExpr',Base=D,Arguments={U},Tokens=L}else break end end;return D end
function E(H)local R={}local D=q.Peek()local L=D.Type
if L=='Number'then return
{AstType='NumberExpr',Value=q.Get(R),Tokens=R}elseif L=='String'then
return{AstType='StringExpr',Value=q.Get(R),Tokens=R}elseif L=='Keyword'then local U=D.Data
if U=='nil'then q.Get(R)
return{AstType='NilExpr',Tokens=R}elseif U=='false'or U=='true'then return
{AstType='BooleanExpr',Value=(q.Get(R).Data=='true'),Tokens=R}elseif U=='function'then q.Get(R)local C=O(H,R)
C.IsLocal=true;return C end elseif L=='Symbol'then local U=D.Data
if U=='...'then q.Get(R)
return{AstType='DotsExpr',Tokens=R}elseif U=='{'then q.Get(R)local C={}
local M={AstType='ConstructorExpr',EntryList=C,Tokens=R}
while true do
if q.IsSymbol('[',R)then q.Get(R)local F=z(H)if not q.ConsumeSymbol(']',R)then
x("`]` Expected")end;if not q.ConsumeSymbol('=',R)then
x("`=` Expected")end;local W=z(H)
C[#C+1]={Type='Key',Key=F,Value=W}elseif q.Is('Ident')then local F=q.Peek(1)
if F.Type=='Symbol'and F.Data=='='then
local W=q.Get(R)
if not q.ConsumeSymbol('=',R)then x("`=` Expected")end;local Y=z(H)C[#C+1]={Type='KeyString',Key=W.Data,Value=Y}else
local W=z(H)C[#C+1]={Type='Value',Value=W}end elseif q.ConsumeSymbol('}',R)then break else local F=z(H)C[#C+1]={Type='Value',Value=F}end
if q.ConsumeSymbol(';',R)or q.ConsumeSymbol(',',R)then elseif
q.ConsumeSymbol('}',R)then break else x("`}` or table entry Expected")end end;return M end end;return A(H)end;local I=8
local N={['+']={6,6},['-']={6,6},['%']={7,7},['/']={7,7},['*']={7,7},['^']={10,9},['..']={5,4},['==']={3,3},['<']={3,3},['<=']={3,3},['~=']={3,3},['>']={3,3},['>=']={3,3},['and']={2,2},['or']={1,1}}
function z(H,R)R=R or 0;local D
if w[q.Peek().Data]then local L={}local U=q.Get(L).Data;D=z(H,I)
local C={AstType='UnopExpr',Rhs=D,Op=U,OperatorPrecedence=I,Tokens=L}D=C else D=E(H)end
while true do local L=N[q.Peek().Data]
if L and L[1]>R then local U={}
local C=q.Get(U).Data;local M=z(H,L[2])
local F={AstType='BinopExpr',Lhs=D,Op=C,OperatorPrecedence=L[1],Rhs=M,Tokens=U}D=F else break end end;return D end
local function S(H)local R=nil;local D={}local L=q.Peek()
if L.Type=="Keyword"then local U=L.Data
if U=='if'then q.Get(D)
local C={}local M={AstType='IfStatement',Clauses=C}
repeat local F=z(H)
if not
q.ConsumeKeyword('then',D)then x("`then` expected.")end;local W=_(H)C[#C+1]={Condition=F,Body=W}until
not q.ConsumeKeyword('elseif',D)
if q.ConsumeKeyword('else',D)then local F=_(H)C[#C+1]={Body=F}end
if not q.ConsumeKeyword('end',D)then x("`end` expected.")end;M.Tokens=D;R=M elseif U=='while'then q.Get(D)local C=z(H)if
not q.ConsumeKeyword('do',D)then return x("`do` expected.")end;local M=_(H)if not
q.ConsumeKeyword('end',D)then x("`end` expected.")end
R={AstType='WhileStatement',Condition=C,Body=M,Tokens=D}elseif U=='do'then q.Get(D)local C=_(H)if not q.ConsumeKeyword('end',D)then
x("`end` expected.")end
R={AstType='DoStatement',Body=C,Tokens=D}elseif U=='for'then q.Get(D)
if not q.Is('Ident')then x("<ident> expected.")end;local C=q.Get(D)
if q.ConsumeSymbol('=',D)then local M=s(H)
local F=M:CreateLocal(C.Data)local W=z(H)
if not q.ConsumeSymbol(',',D)then x("`,` Expected")end;local Y=z(H)local P;if q.ConsumeSymbol(',',D)then P=z(H)end;if not
q.ConsumeKeyword('do',D)then x("`do` expected")end
local V=_(M)
if not q.ConsumeKeyword('end',D)then x("`end` expected")end
R={AstType='NumericForStatement',Scope=M,Variable=F,Start=W,End=Y,Step=P,Body=V,Tokens=D}else local M=s(H)local F={M:CreateLocal(C.Data)}
while
q.ConsumeSymbol(',',D)do
if not q.Is('Ident')then x("for variable expected.")end;F[#F+1]=M:CreateLocal(q.Get(D).Data)end
if not q.ConsumeKeyword('in',D)then x("`in` expected.")end;local W={z(H)}while q.ConsumeSymbol(',',D)do W[#W+1]=z(H)end;if not
q.ConsumeKeyword('do',D)then x("`do` expected.")end
local Y=_(M)
if not q.ConsumeKeyword('end',D)then x("`end` expected.")end
R={AstType='GenericForStatement',Scope=M,VariableList=F,Generators=W,Body=Y,Tokens=D}end elseif U=='repeat'then q.Get(D)local C=_(H)if not q.ConsumeKeyword('until',D)then
x("`until` expected.")end;local M=z(C.Scope)
R={AstType='RepeatStatement',Condition=M,Body=C,Tokens=D}elseif U=='function'then q.Get(D)if not q.Is('Ident')then
x("Function name expected")end;local C=A(H,true)local M=O(H,D)M.IsLocal=false
M.Name=C;R=M elseif U=='local'then q.Get(D)
if q.Is('Ident')then local C={q.Get(D).Data}
while
q.ConsumeSymbol(',',D)do
if not q.Is('Ident')then x("local var name expected")end;C[#C+1]=q.Get(D).Data end;local M={}if q.ConsumeSymbol('=',D)then repeat M[#M+1]=z(H)until
not q.ConsumeSymbol(',',D)end;for F,W in pairs(C)do
C[F]=H:CreateLocal(W)end
R={AstType='LocalStatement',LocalList=C,InitList=M,Tokens=D}elseif q.ConsumeKeyword('function',D)then if not q.Is('Ident')then
x("Function name expected")end;local C=q.Get(D).Data
local M=H:CreateLocal(C)local F=O(H,D)F.Name=M;F.IsLocal=true;R=F else
x("local var or function def expected")end elseif U=='::'then q.Get(D)
if not q.Is('Ident')then x('Label name expected')end;local C=q.Get(D).Data;if not q.ConsumeSymbol('::',D)then
x("`::` expected")end
R={AstType='LabelStatement',Label=C,Tokens=D}elseif U=='return'then q.Get(D)local C={}if not q.IsKeyword('end')then
local M,F=pcall(function()return z(H)end)
if M then C[1]=F;while q.ConsumeSymbol(',',D)do C[#C+1]=z(H)end end end
R={AstType='ReturnStatement',Arguments=C,Tokens=D}elseif U=='break'then q.Get(D)R={AstType='BreakStatement',Tokens=D}elseif U=='goto'then
q.Get(D)if not q.Is('Ident')then x("Label expected")end
local C=q.Get(D).Data;R={AstType='GotoStatement',Label=C,Tokens=D}end end
if not R then local U=A(H)
if q.IsSymbol(',')or q.IsSymbol('=')then if
(U.ParenCount or 0)>0 then
x("Can not assign to parenthesized expression, is not an lvalue")end;local C={U}while
q.ConsumeSymbol(',',D)do C[#C+1]=A(H)end;if not q.ConsumeSymbol('=',D)then
x("`=` Expected.")end;local M={z(H)}while q.ConsumeSymbol(',',D)do
M[#M+1]=z(H)end
R={AstType='AssignmentStatement',Lhs=C,Rhs=M,Tokens=D}elseif U.AstType=='CallExpr'or U.AstType=='TableCallExpr'or
U.AstType=='StringCallExpr'then
R={AstType='CallStatement',Expression=U,Tokens=D}else x("Assignment Statement Expected")end end
if q.IsSymbol(';')then R.Semicolon=q.Get(R.Tokens)end;return R end
function _(H)local R={}local D={Scope=s(H),AstType='Statlist',Body=R,Tokens={}}
while not
f[q.Peek().Data]and not q.IsEof()do local L=S(D.Scope)R[#R+1]=L end
if q.IsEof()then local L={}L.AstType='Eof'L.Tokens={q.Get()}R[#R+1]=L end;return D end;return _(s())end;return{LexLua=g,ParseLua=k}end
a["howl.lexer.constants"]=function(...)
local function n(s)for h,r in ipairs(s)do s[r]=h end;return s end
return
{WhiteChars=n{' ','\n','\t','\r'},EscapeLookup={['\r']='\\r',['\n']='\\n',['\t']='\\t',['"']='\\"',["'"]="\\'"},LowerChars=n{'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'},UpperChars=n{'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'},Digits=n{'0','1','2','3','4','5','6','7','8','9'},HexDigits=n{'0','1','2','3','4','5','6','7','8','9','A','a','B','b','C','c','D','d','E','e','F','f'},Symbols=n{'+','-','*','/','^','%',',','{','}','[',']','(',')',';','#'},Keywords=n{'and','break','do','else','elseif','end','false','for','function','goto','if','in','local','nil','not','or','repeat','return','then','true','until','while'},StatListCloseKeywords=n{'end','else','elseif','until'},UnOps=n{'-','not','#'}}end
a["howl.files.Source"]=function(...)local n=i"howl.lib.assert"local s=i"howl.class"
local h=i"howl.files.matcher"local r=i"howl.class.mixin"local d=i"howl.platform".fs;local l=table.insert
local u=s("howl.files.Source"):include(r.configurable):include(r.filterable)
local function c(w)local y=type(w)
if y=="function"or y=="string"then
return h.createMatcher(w)elseif y=="table"and w.tag and w.predicate then return w elseif
y=="table"and w.isInstanceOf and w:isInstanceOf(u)then return
h.createMatcher(function(p)return w:matches(p)end)else return nil end end
local function m(w,y,p,v)local b=c(y)local g=type(y)
if b then l(w,b)elseif g=="table"then
for v,k in ipairs(y)do local b=c(k)if b then l(w,b)else
error("bad item #"..v..

" for "..p.." (expected pattern, got "..type(k)..")")end end else
error("bad argument #"..v..
" for "..p.." (expected pattern, got "..g..")")end end;local function f(w,y)for p,v in pairs(w)do if v:match(y)then return true end end
return false end;function u:initialize(w,y)
if w==nil then w=true end;self.parent=y;self.children={}self.includes={}self.excludes={}
self.allowEmpty=w end
function u:from(w,y)
n.argType(w,"string","from",1)w=d.normalise(w)local p=self.children[w]
if not p then
p=self.class(true,self)self.children[w]=p;self.allowEmpty=false end;if y~=nil then return p:configureWith(y)else return p end end
function u:include(...)local w=select('#',...)local y={...}for p=1,w do
m(self.includes,y[p],"include",p)end;return self end
function u:exclude(...)local w=select('#',...)local y={...}for p=1,w do
m(self.excludes,y[p],"exclude",p)end;return self end
function u:excluded(w)if f(self.excludes,w)then return true elseif self.parent then
return self.parent:excluded(w)else return false end end
function u:included(w)if#self.includes==0 then return self.allowEmpty else
return f(self.includes,w)end end
function u:configure(w)n.argType(w,"table","configure",1)if w.include~=nil then
self:include(w.include)end
if w.exclude~=nil then self:exclude(w.exclude)end
if w.with~=nil then
n.type(w.with,"table","expected table for with, got %s")for y,p in ipairs(w.with)do self:with(p)end end end;function u:matches(w)
return self:included(w)and not self:excluded(w)end
function u:hasFiles()if
self.allowEmpty or#self.includes>0 then return true end;for w,y in pairs(self.children)do if y:hasFiles()then
return true end end;return false end
function u:gatherFiles(w,y,p)if not p then p={}end;for v,b in pairs(self.children)do local g=d.combine(w,v)
b:gatherFiles(g,y,p)end
if
self.allowEmpty or#self.includes>0 then local v,b={w},1;local g=#p
while b>0 do local k=v[b]local q=k;if w~=""then q=q:sub(#w+2)end
b=b-1
if d.isDir(k)then
if not self:excluded(q)then if y and self:included(q)then g=g+1
p[g]=self:buildFile(k,q)end;for j,x in ipairs(d.list(k))do b=b+1
v[b]=d.combine(k,x)end end elseif self:included(q)and not self:excluded(q)then g=g+1
p[g]=self:buildFile(k,q)end end end;return p end;function u:buildFile(w,y)return{path=w,relative=y,name=y}end;return u end
a["howl.files.matcher"]=function(...)local n=i"howl.lib.utils"
local s={["^"]="%^",["$"]="%$",["("]="%(",[")"]="%)",["%"]="%%",["."]="%.",["["]="%[",["]"]="%]",["+"]="%+",["-"]="%-",["\0"]="%z"}local h={["*"]="(.*)"}for c,m in pairs(s)do h[c]=m end;local function r(c,m)
return m:match(c.text)end;local function d(c,m)
return c.text==""or c.text==m or
m:sub(1,#c.text+1)==c.text.."/"end
local function l(c,m)return c.func(m)end
local function u(c)local m=type(c)
if m=="string"then
local f=n.startsWith(c,"pattern:")or n.startsWith(c,"ptrn:")if f then return{tag="pattern",text=f,match=r}end;if c:find("%*")then local c=
"^"..c:gsub(".",h).."$"
return{tag="pattern",text=c,match=r}end
return{tag="text",text=c,match=d}elseif m=="function"or
(m=="table"and(getmetatable(c)or{}).__call)then return{tag="function",func=c,match=l}else
error("Expected string or function")end end;return{createMatcher=u}end
a["howl.files.CopySource"]=function(...)local n=i"howl.lib.assert"local s=i"howl.files.matcher"
local h=i"howl.class.mixin"local r=i"howl.platform".fs;local d=i"howl.files.Source"local l=table.insert
local u=d:subclass("howl.files.CopySource")
function u:initialize(c,m)d.initialize(self,c,m)self.renames={}self.modifiers={}end
function u:configure(c)n.argType(c,"table","configure",1)
d.configure(self,c)if c.rename~=nil then self:rename(c.rename)end;if
c.modify~=nil then self:modify(c.modify)end end
function u:rename(c,m)local f,w=type(c),type(m)
if f=="table"and m==nil then for y,p in ipairs(c)do
self:rename(p)end elseif f=="function"and m==nil then l(self.renames,c)elseif f==
"string"and w=="string"then l(self.renames,function(y)
return(y.name:gsub(c,m))end)else
error(
"bad arguments for rename (expected table, function or string, string pair, got "..f.." and "..w..")",2)end end
function u:modify(c)local m=type(c)
if m=="table"then
for f,w in ipairs(c)do self:modify(w)end elseif m=="function"then l(self.modifiers,c)else
error("bad argument #1 for modify (expected table or function, got "..m..
")",2)end end
function u:doMutate(c)
for m,f in ipairs(self.modifiers)do local w=f(c)if w then c.contents=w end end
for m,f in ipairs(self.renames)do local w=f(c)if w then c.name=w end end
if self.parent then return self.parent:doMutate(c)else return c end end
function u:buildFile(c,m)return
self:doMutate{path=c,relative=m,name=m,contents=r.read(c)}end;return u end
a["howl.context"]=function(...)local n=i"howl.lib.assert"local s=i"howl.class"
local h=i"howl.class.mixin"local r=i"howl.lib.mediator"local d=i"howl.lib.argparse"local l=i"howl.lib.Logger"
local u=i"howl.packages.Manager"local c=s("howl.Context"):include(h.sealed)
function c:initialize(m,f)
n.type(m,"string","bad argument #1 for Context expected string, got %s")
n.type(f,"table","bad argument #2 for Context expected table, got %s")self.root=m;self.out="build"self.mediator=r
self.arguments=d.Options(self.mediator,f)self.logger=l(self)self.packageManager=u(self)self.modules={}end
function c:include(m)if type(m)~="table"then m=i(m)end;if self.modules[m.name]then
self.logger:warn(
m.name.." already included, skipping")return end;local f={module=m}
self.modules[m.name]=f
self.logger:verbose("Including "..m.name..": "..m.description)
if not m.applied then m.applied=true;if m.apply then m.apply()end end;if m.setup then m.setup(self,f)end end;function c:getModuleData(m)return self.modules[m]end;return c end
a["howl.cli"]=function(...)local n=i"howl.loader"local s=i"howl.lib.colored"
local h=i"howl.platform".fs;local r,d=n.FindHowl()
local l=i"howl.context"(d or h.currentDir(),{...})local u=l.arguments
u:Option"verbose":Alias"v":Description"Print verbose output"
u:Option"time":Alias"t":Description"Display the time taken for tasks"
u:Option"trace":Description"Print a stack trace on errors"
u:Option"help":Alias"?":Alias"h":Description"Print this help"l:include"howl.modules.dependencies.file"
l:include"howl.modules.dependencies.task"l:include"howl.modules.list"l:include"howl.modules.plugins"
l:include"howl.modules.packages.file"l:include"howl.modules.packages.gist"
l:include"howl.modules.packages.pastebin"l:include"howl.modules.tasks.clean"
l:include"howl.modules.tasks.gist"l:include"howl.modules.tasks.minify"
l:include"howl.modules.tasks.pack"l:include"howl.modules.tasks.require"local c=u:Arguments()local function m()if u:Get"help"then
c={"help"}end end
l.mediator:subscribe({"ArgParse","changed"},m)m()
if not r then
if#c==1 and c[1]=="help"then
s.writeColor("yellow","Howl")
s.printColor("lightGrey"," is a simple build system for Lua")
s.printColor("grey","You can read the full documentation online: https://github.com/SquidDev-CC/Howl/wiki/")
s.printColor("white",(([[
			The key thing you are missing is a HowlFile. This can be "Howlfile" or "Howlfile.lua".
			Then you need to define some tasks. Maybe something like this:
		]]):gsub("\t",""):gsub("\n+$","")))s.printColor("magenta",'Tasks:minify "minify" {')
s.printColor("magenta",'  input = "build/Howl.lua",')
s.printColor("magenta",'  output = "build/Howl.min.lua",')s.printColor("magenta",'}')
s.printColor("white","Now just run '"..
h.getName(h.currentProgram()).." minify'!")s.printColor("orange","\nOptions:")u:Help("  ")elseif#c==0 then
error(
d.." Use "..
h.getName(h.currentProgram()).." --help to dislay usage.",0)else error(d,0)end;return end
l.logger:verbose("Found HowlFile at "..h.combine(d,r))local f,w=n.SetupTasks(l,r)
f:Task"list"(function()f:listTasks()end):description"Lists all the tasks"
f:Task"help"(function()print("Howl [options] [task]")
s.printColor("orange","Tasks:")f:listTasks("  ")
s.printColor("orange","\nOptions:")u:Help("  ")end):description"Print out a detailed usage for Howl"
f:Default(function()l.logger:error("No default task exists.")
l.logger:verbose("Use 'Tasks:Default' to define a default task")s.printColor("orange","Choose from: ")
f:listTasks("  ")end)w.dofile(h.combine(d,r))if not f:setup()then
error("Error setting up tasks",0)end;if not f:RunMany(c)then
error("Error running tasks",0)end end
a["howl.class.mixin"]=function(...)local n=i"howl.lib.assert"local s=rawset;local h={}
h.sealed={static={subclass=function(r,d)
n(type(r)=='table',"Make sure that you are using 'Class:subclass' instead of 'Class.subclass'")
n(type(d)=="string","You must provide a name(string) for your class")
error("Cannot subclass '"..
tostring(r).."' (attempting to create '"..d.."')",2)end}}
h.curry={curry=function(r,d)
n.type(r,"table","Bad argument #1 to class:curry (expected table, got %s)")
n.type(d,"string","Bad argument #2 to class:curry (expected string, got %s)")local l=r[d]
n.type(l,"function","No such function "..d)return function(...)return l(r,...)end end,__div=function(r,d)return
r:curry(d)end}
h.configurable={configureWith=function(r,d)local l=type(d)if l=="table"then r:configure(d)return r elseif l=="function"then d(r)
return r else
error("Expected table or function, got "..type(d),2)end;return r end,__call=function(r,...)return
r:configureWith(...)end}
h.filterable={__add=function(r,...)return r:include(...)end,__sub=function(r,...)return r:exclude(...)end,with=function(r,...)return
r:configure(...)end}
function h.delegate(r,d)local l={}for u,c in ipairs(d)do
l[c]=function(m,...)local f=m[r]return f[c](f,...)end end;return l end
h.optionGroup={static={addOption=function(r,d)
local l=function(r,u)if u==nil then u=true end;r.options[d]=u;return r end;r[d:gsub("^%l",string.upper)]=l;r[d]=l
if not
rawget(r.static,"options")then local u={}r.static.options=u
local c=r.super and r.super.static.options;if c then setmetatable(u,{__index=c})end end;r.static.options[d]=true;return r end,addOptions=function(r,d)for l=1,
#d do r:addOption(d[l])end;return r end},configure=function(r,d)
n.argType(d,"table","configure",1)local l=r.class;local u=l.options
while l and not u do u=l.options;l=l.super end;if not u then return end
for c,m in pairs(d)do if u[c]then r[c](r,m)end end end,__newindex=function(r,d,l)
if
r.class.options and r.class.options[d]then r[d](r,l)else s(r,d,l)end end}return h end
a["howl.class"]=function(...)
local n={_VERSION='middleclass v4.0.0',_DESCRIPTION='Object Orientation for Lua',_URL='https://github.com/kikito/middleclass',_LICENSE=[[
		MIT LICENSE

		Copyright (c) 2011 Enrique García Cota

		Permission is hereby granted, free of charge, to any person obtaining a
		copy of this software and associated documentation files (the
		"Software"), to deal in the Software without restriction, including
		without limitation the rights to use, copy, modify, merge, publish,
		distribute, sublicense, and/or sell copies of the Software, and to
		permit persons to whom the Software is furnished to do so, subject to
		the following conditions:

		The above copyright notice and this permission notice shall be included
		in all copies or substantial portions of the Software.

		THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
		OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
		MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
		IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
		CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
		TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
		SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	]]}
local function s(w,y)
if y==nil then return w.__instanceDict else
return function(f,p)local v=w.__instanceDict[p]
if v~=nil then return v elseif type(y)==
"function"then return(y(f,p))else return y[p]end end end end
local function h(w,y,p)p=y=="__index"and s(w,p)or p
w.__instanceDict[y]=p;for f in pairs(w.subclasses)do
if rawget(f.__declaredMethods,y)==nil then h(f,y,p)end end end
local function r(w,y,p)w.__declaredMethods[y]=p;if p==nil and w.super then
p=w.super.__instanceDict[y]end;h(w,y,p)end;local function d(f)return"class "..f.name end
local function l(f,...)return f:new(...)end
local function u(f,w)local y={}y.__index=y
local p={name=f,super=w,static={},__instanceDict=y,__declaredMethods={},subclasses=setmetatable({},{__mode='k'})}
if w then
setmetatable(p.static,{__index=function(v,b)return rawget(y,b)or w.static[b]end})else
setmetatable(p.static,{__index=function(v,b)return rawget(y,b)end})end
setmetatable(p,{__index=p.static,__tostring=d,__call=l,__newindex=r})return p end
local function c(f,w)
assert(type(w)=='table',"mixin must be a table")
for y,p in pairs(w)do if y~="included"and y~="static"then f[y]=p end end;for y,p in pairs(w.static or{})do f.static[y]=p end;if
type(w.included)=="function"then w:included(f)end;return f end
local m={__tostring=function(f)return"instance of "..tostring(f.class)end,initialize=function(f,...)
end,isInstanceOf=function(f,w)
return type(f)=='table'and type(f.class)=='table'and
type(w)=='table'and
(w==f.class or type(w.isSubclassOf)==
'function'and f.class:isSubclassOf(w))end,static={allocate=function(f)
assert(
type(f)=='table',"Make sure that you are using 'Class:allocate' instead of 'Class.allocate'")return setmetatable({class=f},f.__instanceDict)end,new=function(f,...)
assert(
type(f)=='table',"Make sure that you are using 'Class:new' instead of 'Class.new'")local w=f:allocate()w:initialize(...)return w end,subclass=function(f,w)
assert(
type(f)=='table',"Make sure that you are using 'Class:subclass' instead of 'Class.subclass'")
assert(type(w)=="string","You must provide a name(string) for your class")local y=u(w,f)for p,v in pairs(f.__instanceDict)do h(y,p,v)end;y.initialize=function(p,...)return
f.initialize(p,...)end
f.subclasses[y]=true;f:subclassed(y)return y end,subclassed=function(f,w)
end,isSubclassOf=function(f,w)
return type(w)=='table'and type(f)=='table'and type(f.super)==
'table'and
(f.super==w or

type(f.super.isSubclassOf)=='function'and f.super:isSubclassOf(w))end,include=function(f,...)
assert(
type(f)=='table',"Make sure you that you are using 'Class:include' instead of 'Class.include'")for w,y in ipairs({...})do c(f,y)end;return f end}}return
function(f,w)
assert(type(f)=='string',"A name (string) is needed for the new class")return w and w:subclass(f)or c(u(f),m)end end
if not shell or type(...or nil)=='table'then local n=...or{}
n.require=i;n.preload=a;return n else return a["howl.cli"](...)end