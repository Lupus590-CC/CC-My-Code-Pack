--# Shell Utility Extended v3.0 - Program to extend/modify Computercraft autocompletion system.
--# Made By Wojbie
--# http://pastebin.com/MbcnJRAU

--   Copyright (c) 2017-2021 Wojbie (wojbie@wojbie.net)
--   Redistribution and use in source and binary forms, with or without modification, are permitted (subject to the limitations in the disclaimer below) provided that the following conditions are met:
--   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
--   4. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
--   5. The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
--   NO EXPRESS OR IMPLIED LICENSES TO ANY PARTY'S PATENT RIGHTS ARE GRANTED BY THIS LICENSE. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--# New settings system usage functions. Allows auto correction of common errors.
local defaultSettings = {
["shellUtil.use_ghost"] = true,
["shellUtil.chat_names"] = "",
["shellUtil.pastebin_names"] = "",
}

--Restore cleared settings
for i,k in pairs(defaultSettings) do
	if settings.get(i) == nil then
		settings.set(i,k)
	end
end

local validSettingsTypes = {
["shellUtil.use_ghost"] = {["boolean"]=true},
["shellUtil.chat_names"] = {["string"]=true},
["shellUtil.pastebin_names"] = {["string"]=true},
}

local validSettingsTest = {
}

local getSetting = function(A)
	local data = settings.get(A)
	if	(not validSettingsTypes[A] or validSettingsTypes[A][type(data)]) and --See if setting type matches (when defined).
		(not validSettingsTest[A] or validSettingsTest[A](data)) --See if testing function agrees (when defined).
	then --All tests OK
		return data
	else --Any of test Failed. Reset to default setting.
		data = defaultSettings[A]
		settings.set(A,data)
		return data
	end
end

--# Basic file operators.
local function append(A,B) local file = fs.open(tostring(A),"a") if not file then return false end file.write(B) file.close() return true end

local function save(A,B) local file = fs.open(tostring(A),"w") if not file then return false end file.write(B) file.close() return true end
local function saveT(A,B) return save(A,textutils.serialize(B)) end
local function saveTL(A,B) return save(A,string.gsub(textutils.serialize(B),"\n%s*","")) end
local function saveLines(A,B) local file = fs.open(tostring(A),"w") if not file then return false end for i=1,#B,1 do file.writeLine(B[i]) end file.close() return true end
local function saveBin(A,B) local file = fs.open(tostring(A),"wb") if not file then return false end for i=1,#B,1 do file.write(B[i]) end file.close() return true end
local function saveDump(A,B) return saveBin(A,{string.byte(B,1,#B)}) end

local function get(A) local file = fs.open(tostring(A),"r") if not file then return false end local data = file.readAll() file.close() if data then return data end end
local function getT(A) local data = get(A) if data then data = textutils.unserialize(data) end if data then return data end end
local function getLines(A) local file = fs.open(tostring(A),"r") if not file then return false end local data = {} for k in file.readLine do data[#data+1] = k end file.close() return data end
local function getBin(A) local file = fs.open(tostring(A),"rb") if not file then return false end local data = {} local b = file.read() while b do table.insert(data,b) b = file.read() end file.close() if data then return data end end
local function getDump(A) local file = getBin(A) if not file then return false end local data = string.char(table.unpack(file)) if data then return data end end
local function getHttp(A,B,C) if not http.checkURL(A) then return false end	local file = B and http.post(A,B,C) or http.get(A,C) if not file then return false end local data = file.readAll() file.close() if data then return data end end

local function makeLog(A) local file = fs.open(tostring(A),"a") if not file then return false end local on = true return function(m) if not on then return false end file.writeLine(m) file.flush() return true end,function() on = false file.close() end end
local function makePrintLog(A) local logfile,logstop = makeLog(A) if not logfile then return false end return function(m) print(m) logfile(m) end,logstop,logfile end

--# Useafull functions

local function completeMultipleChoice( sText, tOptions, bAddSpaces, tOptionsGhosts ) --Copy of function cause its usefull
    local tResults = {}
	local tGhosts = {}
    for n=1,#tOptions do
        local sOption = tOptions[n]
        if #sOption + (bAddSpaces and 1 or 0) > #sText and string.sub( sOption, 1, #sText ) == sText then
            local sResult = string.sub( sOption, #sText + 1 )
            if bAddSpaces then
                table.insert( tResults, sResult .. " " )
            else
                table.insert( tResults, sResult )
            end
			if tOptionsGhosts then
				if bAddSpaces then
					table.insert( tGhosts, tOptionsGhosts[n] or "")
				else
					table.insert( tGhosts, (tOptionsGhosts[n] and " "..tOptionsGhosts[n]) or "")
				end
				
			end
        end
    end
    return tResults,tGhosts
end

local function peripherallook(sType,fTest) --Fast way to make table of peripheral names.
	local tNames={}
	peripheral.find(sType,function(sName,tObject) if ( not fTest ) or fTest(sName,tObject) then table.insert(tNames,sName) end return false end)
	return tNames
end


local function hostnameslook(sProtocol,nTime) --Program to lookup hostnames that are in set sProtocol. nTime is time it will look. Defaults to 0,5.
    -- Build list of host IDs
    local tResults = {}
	local close=false
	
    if not rednet.isOpen() then
		for i,k in pairs(rs.getSides()) do
			if peripheral.getType( k ) == "modem" then
				rednet.open(k)
				close=k
				break
			end
		end
		if not close then return tResults end
    end

    -- Broadcast a lookup packet
    rednet.broadcast( {
        sType = "lookup",
        sProtocol = sProtocol,
        sHostname = sHostname,
    }, "dns" )

    -- Start a timer
    local timer = os.startTimer( nTime or 0.5 )

    -- Wait for events
    while true do
        local event, p1, p2, p3 = os.pullEvent()
        if event == "rednet_message" then
            -- Got a rednet message, check if it's the response to our request
            local nSenderID, tMessage, sMessageProtocol = p1, p2, p3
            if sMessageProtocol == "dns" and tMessage.sType == "lookup response" then
                if tMessage.sProtocol == sProtocol then
                        table.insert( tResults, tMessage.sHostname )
                end
            end
        else
            -- Got a timer event, check it's the end of our timeout
            if p1 == timer then
                break
            end
        end
    end

	if close then
		rednet.close(close)
	end
	
    return tResults
end

--## Main Program Parts ##--

--# Read overwrite to add ghosting and change to few vanilla auto-completitions to add ghost capabilities.

if getSetting("shellUtil.use_ghost") then --This setting is only tested once at moment program is run.

	-- Overwriting read with one that supports 2nd Ghost table.
	function _G.read( _sReplaceChar, _tHistory, _fnComplete, _sDefault )
	    if _sReplaceChar ~= nil and type( _sReplaceChar ) ~= "string" then
			error( "bad argument #1 (expected string, got " .. type( _sReplaceChar ) .. ")", 2 ) 
		end
		if _tHistory ~= nil and type( _tHistory ) ~= "table" then
			error( "bad argument #2 (expected table, got " .. type( _tHistory ) .. ")", 2 ) 
		end
		if _fnComplete ~= nil and type( _fnComplete ) ~= "function" then
			error( "bad argument #3 (expected function, got " .. type( _fnComplete ) .. ")", 2 ) 
		end
		if _sDefault ~= nil and type( _sDefault ) ~= "string" then
			error( "bad argument #4 (expected string, got " .. type( _sDefault ) .. ")", 2 ) 
		end
		term.setCursorBlink( true )

		local sLine = ""
		if type( _sDefault ) == "string" then
			sLine = _sDefault
		else
			sLine = ""
		end
		local nHistoryPos
		local nPos = #sLine
		if _sReplaceChar then
			_sReplaceChar = string.sub( _sReplaceChar, 1, 1 )
		end

		local tCompletions
		local nCompletion
		local tGhosts --#
		local function recomplete()
			if _fnComplete and nPos == string.len(sLine) then
				tCompletions,tGhosts = _fnComplete( sLine ) --#
				if tCompletions and #tCompletions > 0 then
					nCompletion = 1
					tGhosts = tGhosts or {} --#
				else
					nCompletion = nil
				end
			else
				tCompletions = nil
				nCompletion = nil
			end
		end

		local function uncomplete()
			tCompletions = nil
			nCompletion = nil
			tGhosts = nil --#
		end

		local w = term.getSize()
		local sx = term.getCursorPos()

		local function redraw( _bClear )
			local nScroll = 0
			if sx + nPos >= w then
				nScroll = (sx + nPos) - w
			end

			local cx,cy = term.getCursorPos()
			term.setCursorPos( sx, cy )
			local sReplace = (_bClear and " ") or _sReplaceChar
			if sReplace then
				term.write( string.rep( sReplace, math.max( string.len(sLine) - nScroll, 0 ) ) )
			else
				term.write( string.sub( sLine, nScroll + 1 ) )
			end

			if nCompletion then
				local sCompletion = tCompletions[ nCompletion ]
				local sGhost = tGhosts[ nCompletion ] --#
				local oldText, oldBg
				if not _bClear then
					oldText = term.getTextColor()
					oldBg = term.getBackgroundColor()
					term.setTextColor( colors.white )
					term.setBackgroundColor( colors.gray )
				end
				if sReplace then
					term.write( string.rep( sReplace, string.len( sCompletion ) ) )
				else
					term.write( sCompletion )
				end
				--#
				if sGhost then
					if not _bClear then
						term.setTextColor( colors.lightGray )
						--term.setBackgroundColor( colors.gray )
					end
					if sReplace then
						term.write( string.rep( sReplace, string.len( sGhost ) ) )
					else
						term.write( sGhost )
					end
				end
				--#
				if not _bClear then
					term.setTextColor( oldText )
					term.setBackgroundColor( oldBg )
				end
			end

			term.setCursorPos( sx + nPos - nScroll, cy )
		end
		
		local function clear()
			redraw( true )
		end

		recomplete()
		redraw()

		local function acceptCompletion()
			if nCompletion then
				-- Clear
				clear()

				-- Find the common prefix of all the other suggestions which start with the same letter as the current one
				local sCompletion = tCompletions[ nCompletion ]
				sLine = sLine .. sCompletion
				nPos = string.len( sLine )

				-- Redraw
				recomplete()
				redraw()
			end
		end
		while true do
			local sEvent, param = os.pullEvent()
			if sEvent == "char" then
				-- Typed key
				clear()
				sLine = string.sub( sLine, 1, nPos ) .. param .. string.sub( sLine, nPos + 1 )
				nPos = nPos + 1
				recomplete()
				redraw()

			elseif sEvent == "paste" then
				-- Pasted text
				clear()
				sLine = string.sub( sLine, 1, nPos ) .. param .. string.sub( sLine, nPos + 1 )
				nPos = nPos + string.len( param )
				recomplete()
				redraw()

			elseif sEvent == "key" then
				if param == keys.enter then
					-- Enter
					if nCompletion then
						clear()
						uncomplete()
						redraw()
					end
					break
					
				elseif param == keys.left then
					-- Left
					if nPos > 0 then
						clear()
						nPos = nPos - 1
						recomplete()
						redraw()
					end
					
				elseif param == keys.right then
					-- Right                
					if nPos < string.len(sLine) then
						-- Move right
						clear()
						nPos = nPos + 1
						recomplete()
						redraw()
					else
						-- Accept autocomplete
						acceptCompletion()
					end

				elseif param == keys.up or param == keys.down then
					-- Up or down
					if nCompletion then
						-- Cycle completions
						clear()
						if param == keys.up then
							nCompletion = nCompletion - 1
							if nCompletion < 1 then
								nCompletion = #tCompletions
							end
						elseif param == keys.down then
							nCompletion = nCompletion + 1
							if nCompletion > #tCompletions then
								nCompletion = 1
							end
						end
						redraw()

					elseif _tHistory then
						-- Cycle history
						clear()
						if param == keys.up then
							-- Up
							if nHistoryPos == nil then
								if #_tHistory > 0 then
									nHistoryPos = #_tHistory
								end
							elseif nHistoryPos > 1 then
								nHistoryPos = nHistoryPos - 1
							end
						else
							-- Down
							if nHistoryPos == #_tHistory then
								nHistoryPos = nil
							elseif nHistoryPos ~= nil then
								nHistoryPos = nHistoryPos + 1
							end                        
						end
						if nHistoryPos then
							sLine = _tHistory[nHistoryPos]
							nPos = string.len( sLine ) 
						else
							sLine = ""
							nPos = 0
						end
						uncomplete()
						redraw()

					end

				elseif param == keys.backspace then
					-- Backspace
					if nPos > 0 then
						clear()
						sLine = string.sub( sLine, 1, nPos - 1 ) .. string.sub( sLine, nPos + 1 )
						nPos = nPos - 1
						recomplete()
						redraw()
					end

				elseif param == keys.home then
					-- Home
					if nPos > 0 then
						clear()
						nPos = 0
						recomplete()
						redraw()
					end

				elseif param == keys.delete then
					-- Delete
					if nPos < string.len(sLine) then
						clear()
						sLine = string.sub( sLine, 1, nPos ) .. string.sub( sLine, nPos + 2 )                
						recomplete()
						redraw()
					end

				elseif param == keys["end"] then
					-- End
					if nPos < string.len(sLine ) then
						clear()
						nPos = string.len(sLine)
						recomplete()
						redraw()
					end

				elseif param == keys.tab then
					-- Tab (accept autocomplete)
					acceptCompletion()

				end

			elseif sEvent == "term_resize" then
				-- Terminal resized
				w = term.getSize()
				redraw()

			end
		end

		local cx, cy = term.getCursorPos()
		term.setCursorBlink( false )
		term.setCursorPos( w + 1, cy )
		print()
		
		return sLine
	end
	

end


--#Generate Shell Completitions

-- /rom/programs/
-- Eject --List only disk drives. Ghost drive content type and label/songname.
local function completeEject( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
		local tNames = peripherallook("drive")
		local tGhosts = {}
		for i=1,#tNames do
			local sName = tNames[i]
			if disk.hasData(sName) then
				tGhosts[i] = " Data "..(disk.getLabel(sName) or "")
			elseif disk.hasAudio(sName) then
				tGhosts[i] = " Music "..(disk.getAudioTitle(sName) or "")
			elseif disk.isPresent(sName) then
				tGhosts[i] = " Unknown"
			else 
				tGhosts[i] = " Empty"
			end
		end
		return completeMultipleChoice(sText,tNames,false,tGhosts)
	end
end
-- Gps -- Move order of options so locate is first.
local tGPSOptions = {"locate" , "host", "host "}
local function completeGPS( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
        return completeMultipleChoice( sText, tGPSOptions )
    end
end
-- Label -- List only disk drives.
local tLabelOptions = { "get", "get ", "set ", "clear", "clear " }
local function completeLabel( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
        return completeMultipleChoice( sText, tLabelOptions )
    elseif nIndex == 2 then
        return completeMultipleChoice(sText,peripherallook("drive"))
    end
end
-- Monitor -- List only monitors. Ghost current size of selected monitor.
local function completeMonitor( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
		local tNames = peripherallook("monitor")
		local tGhosts = {}
		for i=1,#tNames do
			local x,y = peripheral.call(tNames[i],"getSize")
			tGhosts[i]= x.."x"..y
		end
        return completeMultipleChoice(sText,peripherallook("monitor"), true ,tGhosts)
    elseif nIndex == 2 then
        return shell.completeProgram( sText )
    end
end
-- Set -- Ghost the current setting (if table say [table])
local function completeSet( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
		local tNames = settings.getNames()
		local tGhosts = {}
		for i=1,#tNames do
			local data = settings.get(tNames[i])
			tGhosts[i] = type(data) == "table" and "Table Detected" or tostring(data)
		end
        return completeMultipleChoice( sText, tNames, true , tGhosts)
    end
end

--/rom/programs/fun
-- DJ -- List only disk drives with music. Ghosts dong names.
local tDJOptions = { "play", "play ", "stop" }
local function Audiotest(sName,tObject)
	return tObject.hasAudio()
end
local function completeDJ( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
        return completeMultipleChoice( sText, tDJOptions )
    elseif nIndex == 2 and tPreviousText[2] == "play" then
		local tNames = peripherallook("drive",Audiotest)
		local tGhosts = {}
		for i=1,#tNames do
			tGhosts[i] = disk.getAudioTitle(tNames[i])
		end
		return completeMultipleChoice(sText,tNames,false,tGhosts)
    end
end

--rom/programs/http/
--Pastebin -- List pastes from "shellUtil.pastebin_names" user(s), Ghost paste names. Suggest File Names based of pasteName?

--get website https://pastebin.com/u/..name
--<td><img src="/i/t.gif" class="i_p0" title="Public paste, anybody can see this paste." alt="" /> <a href="/DW3LCC3L">Monitor Mirror v2.1</a></td>
--local tPastes={"DW3LCC3L"}
--local tPasteNames={"Monitor Mirror v2.1"}
--local tPasteSuggestNames = {["DW3LCC3L"] = "Monitor_Mirror_v2.1}

--Make table of usernames from settings
local tPastebinUserNames = {}
for sName in string.gmatch( getSetting("shellUtil.pastebin_names"), "[^,]+" ) do table.insert(tPastebinUserNames,sName) end

--Code to get all the public pastes on Ext-util load.
local tPastes = {}
local tPasteNames = {}
local tPasteSuggestNames = {}
for _,name in pairs(tPastebinUserNames) do
	local site = "https://pastebin.com/u/"..textutils.urlEncode( name )
	if http.checkURL(site) then
		local data = getHttp(site)
		if data then
			--get pastes from data here.
			for i,k in string.gmatch (data, '<td><img src="/i/t.gif" class="i_p0" title="Public paste, anybody can see this paste." alt="" /> <a href="/(%w+)">(.-)</a></td>') do
				table.insert(tPastes,i)
				table.insert(tPasteNames,k)
				tPasteSuggestNames[i] = string.gsub(k,"%s","_")
			end
		end
	end
end
local tPastebinOptions = { "get ", "run ", "put" }
local function completePastebin( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
        return completeMultipleChoice( sText, tPastebinOptions )
    elseif nIndex == 2 then
        if tPreviousText[2] == "put" then
            return fs.complete( sText, shell.dir(), true, false )
		elseif tPreviousText[2] == "get" then
			return completeMultipleChoice( sText, tPastes, true,tPasteNames)
		elseif tPreviousText[2] == "run" then
			return completeMultipleChoice( sText, tPastes, true,tPasteNames)
        end
	elseif nIndex == 3 then
		if tPreviousText[2] == "get" and tPasteSuggestNames[tPreviousText[3]]  then 
			return completeMultipleChoice( sText, {tPasteSuggestNames[tPreviousText[3]]} )
		end
    end
end

--rom/programs/rednet/
-- Chat -- On join allow for duble tap of Tab to scan area for chat servers. Automaticly suggest name(s) from "shellUtil.chat_names"
--##FIND WAY TO EXTRACT ANY SERVER INFO DATA FROM CHAT SERVER WITHOUT LOGGING INTO IT
local tChatOptions = {"join ", "host "}
local tServers = {}
local nLasttab = 0
local function completeChat( shell, nIndex, sText, tPreviousText )
    if nIndex == 1 then
        return completeMultipleChoice( sText, tChatOptions )
	elseif nIndex == 2 and tPreviousText[2] == "join" then
		local tGhosts = {}
		if sText =="" then --Act only is sText field is empty.
			local nTime=os.clock()
			if (nTime-nLasttab) < 0 then --Still Blocked from last scan.
				--do nothing			
			elseif (nTime-nLasttab) < 0.5 then
				tServers = hostnameslook("chat") --When 2 empty inputs in 0.5 sec range do a rednet scan, if not empty dont re-scan.
				if #tServers == 0 then
					tServers = {""}
					tGhosts  =  {"[No Servers Found. Re-Tap to Re-Scan]"}
					nLasttab = os.clock()
				else
					nLasttab = os.clock() + 30 -- Block scanning for 30 sec so it won't scan over and over again in row.
				end
			else
				tServers = {""}
				tGhosts =  {"[Double-Tap Tab to Scan]"}
				nLasttab = os.clock()
			end
		end
		return completeMultipleChoice( sText, tServers , true ,tGhosts)
	elseif nIndex == 3 and tPreviousText[2] == "join" then
		local tNames = {}
		for sName in string.gmatch( getSetting("shellUtil.chat_names"), "[^,]+" ) do table.insert(tNames,sName) end
		return completeMultipleChoice( sText, tNames)
    end
end

--# Apply said functions
shell.setCompletionFunction( "rom/programs/eject.lua", completeEject )
shell.setCompletionFunction( "rom/programs/gps.lua", completeGPS )
shell.setCompletionFunction( "rom/programs/label.lua", completeLabel )
shell.setCompletionFunction( "rom/programs/monitor.lua", completeMonitor )
shell.setCompletionFunction( "rom/programs/set.lua", completeSet )

shell.setCompletionFunction( "rom/programs/fun/dj.lua", completeDJ )

shell.setCompletionFunction( "rom/programs/http/pastebin.lua", completePastebin )

shell.setCompletionFunction( "rom/programs/rednet/chat.lua", completeChat )
