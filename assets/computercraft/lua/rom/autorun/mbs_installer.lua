if not fs.exists("/.mbs") then
	shell.run("copy /rom/.mbs /.mbs")
	local f = fs.open("/startup", "a")
	f.writeLine("\nshell.run(\"rom/programs/mbs.lua startup\")")
	f.close()
end
