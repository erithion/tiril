--require "http"
--http = require("socket.http")

local options = {
  language = nil,
  downloadBehaviour = 'save',
  langExt = false,
  removeTag = false,
  showMediaInformation = true,
  progressBarSize = 80,
  intLang = 'eng',
  translations_avail = {
    eng = 'English'
  }
}

            --[[ VLC extension stuff ]]--

function descriptor()
  return {
    title = "Viril",
    version = "0.1",
    author = "erithion",
    url = 'http://google.com',
        shortdesc = "Learn a language while watching TV!",
        description = [[<html></html>]],
    capabilities = {"menu", "input-listener" }
--    capabilities = {"input-listener", "trigger" }
  }
end

-- %s: original word
header_page = [[<HTML>
<HEAD>
<!-- <TITLE>Your Title Here</TITLE> -->
</HEAD>
<BODY>
<!-- <CENTER><IMG SRC="F:\\git\\vlc\\vlc-3.0.0\\lua\\extensions\\example.png" ALIGN="BOTTOM"> </CENTER> -->
<!-- <a href="http://somegreatsite.com">Link Name</a> is a link to another nifty site  --> 
<h1><b>%s</b><sup style="font-family:Arial;color:lightgrey;font-size:50%%;"><i>original</i></sup></h1> <!-- Original word --> 
<hr>]]

-- %s %s: translator translaion
tr = [[ <h2><b style="color:grey;">%s</h2></p>
<ul><li style="font-family:Helvetica;text-indent: 30%%"><big> %s</big></li></ul>
]]

error = [[ <p>%s ]]
footer_page = [[ </BODY> </HTML>]]

function google_get(data)
   return data:match([[%[Tir {sourceText = (.-), sourceLang = (.-), targetText = Just "(.-)",]])
end

local mainDialogWindow = nil

function lexin_get(data)
  lem = {}
  mor = {}
  def = {}
  eks ={}
  idi = {}
  sms = {}
  alt = {}
  switch = {
    ["LEM"] = function (word_, lang_) table.insert( lem, { w=word_, l=lang_} ) end,
    ["MOR"] = function (word_, lang_) table.insert( mor, { w=word_, l=lang_} ) end,
    ["DEF"] = function (word_, lang_) table.insert( def, { w=word_, l=lang_} ) end,
    ["EKS"] = function (word_, lang_) table.insert( eks, { w=word_, l=lang_} ) end,
    ["IDI"] = function (word_, lang_) table.insert( idi, { w=word_, l=lang_} ) end,
    ["SMS"] = function (word_, lang_) table.insert( sms, { w=word_, l=lang_} ) end,
    ["ALT"] = function (word_, lang_) table.insert( alt, { w=word_, l=lang_} ) end,
  }
  for word, lang, type in
                data:gmatch([[.-LexinWord {lexinWord = "(.-)", lexinLang = "(.-)", lexinType = "(.-)"}]])
        do
           switch[type](word, lang)
        end
  return lem, mor, def, eks, idi, sms, alt
end

function format_lex(name, arr)
-- %s %s: type words
	local tr = [[ <h3><i>%s</i></h3><ul>%s</ul>]]
	local tr2 = [[<li style="font-family:Helvetica;"> <big>%s</big> <sub style="font-family:Arial;color:grey;"><i>%s</i></sub></li>]]
	local dt = ""
    for k,v in pairs(arr)
		do
		  dt =  dt .. string.format( tr2, v["w"], v["l"] )
--			print( v["w"].." "..v["l"].." "..v["t"])
		end
    if dt ~= "" then dt = string.format( tr, name, dt ) end
	return dt
end

function addWord(word)
    req = "http://localhost:3000/add/" .. word
    vlc.msg.dbg("[viril] Requesting " .. req)
    r, status, resp = get(req)
	if not r then 
           vlc.msg.dbg("[viril] status "..tostring(status))
           vlc.msg.dbg("[viril] resp "..tostring(resp))
	else 
           vlc.msg.dbg("[viril] word sent "..tostring(resp))
    end
end

-- add component function parameters : col, row, col_span, row_span ...; aligned on the QGridLayout
function activate()

  vlc.msg.dbg("[viril] In activate()")

  mainDialogWindow = vlc.dialog( "Viril" )
  local lib = vlc.object.libvlc()
  local srcWord = vlc.var.get( lib, "tiril_word" )
  -- we'll take only first 2 chars of language abbr.
  local langFrom = string.sub( vlc.var.get( lib, "tiril_src_lng" ), 1, 2)
  if langFrom == nil then langFrom = "en"  else string.sub( langFrom, 1, 2 ) end
  local langTo = vlc.var.get( lib, "tiril_tgt_lng" ) 
  if langTo == nil then langTo = "en"  else string.sub( langTo, 1, 2 ) end

  vlc.msg.dbg("[viril] tiril_word "..tostring(srcWord))
  vlc.msg.dbg("[viril] tiril_src_lng "..tostring(langFrom))
  vlc.msg.dbg("[viril] tiril_tgt_lng "..tostring(langTo))

  local text = ""
  if srcWord ~= nil then 
	text = string.format(header_page, srcWord)
	-- google
    local req = "http://localhost:3000/goo/" .. langFrom .."/" .. langTo .. "/" .. srcWord
    vlc.msg.dbg("[viril] Requesting Google" .. req)
    local r, status, resp = get(req)
	if not r then 
		   text = text .. string.format( error, "can't reach out to tiril" )
           vlc.msg.dbg("[viril] status "..tostring(status))
           vlc.msg.dbg("[viril] resp "..tostring(resp))
	else 
	  local s, _, word = google_get( r )
	  text =  text .. string.format( tr, "Google Translate", word )
      vlc.msg.dbg("[viril] resp " .. r)
    end
    -- lexin
    local req = "http://localhost:3000/lex/" .. langFrom .."/" .. langTo .. "/" .. srcWord
    vlc.msg.dbg("[viril] Requesting Lexin" .. req)
    local r, status, resp = get(req)
	if not r then 
		   text = text .. string.format( error, "can't reach out to tiril" )
           vlc.msg.dbg("[viril] status "..tostring(status))
           vlc.msg.dbg("[viril] resp "..tostring(resp))
	else 
	  local lem, mor, def, eks, idi, sms, alt = lexin_get(r)
	  text =  text .. string.format( tr, "Lexin", "" )
	  text = text..format_lex("Lemma", lem)
	  text = text..format_lex("Morphology", mor)
	  text = text..format_lex("Definition", def)
	  text = text..format_lex("Expressions", eks)
	  text = text..format_lex("Idioms", idi)
	  text = text..format_lex("Sms", sms)
	  text = text..format_lex("Alt", alt)

      vlc.msg.dbg("[viril] Resp " .. r)
    end
    --
	text = text .. footer_page
  else
    text = get("http://mirrors.ibiblio.org/CTAN/info/greek/gentle/readme.txt")
  end

  -- It seems the more you indent at the beginning, the more QGridLayout will allow you to extend
  --                        col    row    col_span  row_span
  htm =  mainDialogWindow:add_html(text, 10,     1,      75,	      120)
  btn = mainDialogWindow:add_button("Add the word to Tiril", function() 
		addWord(srcWord) 
		mainDialogWindow:del_widget(btn)
		btn = nil
		end, 10, 121, 1, 1)
  mainDialogWindow:add_label("Translate from", 11, 121, 1, 1)
  fromTextCtrl = mainDialogWindow:add_text_input(langFrom, 12, 121, 1, 1); 
  mainDialogWindow:add_label("to", 13, 121, 1, 1)
  toTextCtrl = mainDialogWindow:add_text_input(langTo, 14, 121, 1, 1); 

  mainDialogWindow:show()
end

function close()
  local lib = vlc.object.libvlc()
  -- Saving the chosen languages for future
  vlc.var.set( lib, "tiril_src_lng", fromTextCtrl:get_text() )
  vlc.var.create( lib, "tiril_tgt_lng", toTextCtrl:get_text() )

  vlc.msg.dbg("[viril] In close()")
  vlc.deactivate()
end

function deactivate()
	-- Close & reset
	if mainDialogWindow ~= nil then
	    mainDialogWindow:delete()
		mainDialogWindow = nil
	end
  
    vlc.msg.dbg("[viril] in  deactivate!")
end

userAgentHTTP = "Viril"

function get(url)
  local host, path, port = parse_url(url)
  if port == 0 then port = 80 end
  vlc.msg.dbg("[viril] port " .. tostring(port))
  local header = {
    "GET "..path.." HTTP/1.0",
    "Host: "..host,
    "User-Agent: "..userAgentHTTP,
    "",
    ""
  }
  local request = table.concat(header, "\r\n")

  local response
  local status, response = http_req(host, port, request)

  if status == 200 then
    return response
  else
    vlc.msg.dbg("[viril] status " .. tostring(status))
    vlc.msg.dbg("[viril] resp " .. tostring(resp))
    return false, status, response
  end
end

function http_req(host, port, request)
  local fd = vlc.net.connect_tcp(host, port)
  if not fd then return false end
  local pollfds = {}

  pollfds[fd] = vlc.net.POLLIN
  vlc.net.send(fd, request)
  vlc.net.poll(pollfds)

  local chunk = vlc.net.recv(fd, 2048)
  local response = ""
  local headerStr, header, body
  local contentLength, status
  local pct = 0

  while chunk do
    response = response..chunk
    if not header then
        headerStr, body = response:match("(.-\r?\n)\r?\n(.*)")
        if headerStr then
            response = body
            header = parse_header(headerStr)
            contentLength = tonumber(header["Content-Length"])
            status = tonumber(header["statuscode"])
        end
    end

    if contentLength then
        bodyLenght = #response
        pct = bodyLenght / contentLength * 100
--        setMessage(openSub.actionLabel..": "..progressBarContent(pct))
      if bodyLenght >= contentLength then
        break
      end
    end

    vlc.net.poll(pollfds)
    chunk = vlc.net.recv(fd, 1024)
  end

  vlc.net.close(fd)

  if status == 301
  and header["Location"] then
    local host, path = parse_url(trim(header["Location"]))
    request = request
    :gsub("^([^%s]+ )([^%s]+)", "%1"..path)
    :gsub("(Host: )([^\n]*)", "%1"..host)

    return http_req(host, port, request)
  end

  return status, response
end

function parse_header(data)
  local header = {}

  for name, s, val in string.gmatch(
    data,
    "([^%s:]+)(:?)%s([^\n]+)\r?\n")
  do
    if s == "" then
    header['statuscode'] = tonumber(string.sub(val, 1 , 3))
    else
      header[name] = val
    end
  end
  return header
end

function parse_url(url)
  local url_parsed = vlc.strings.url_parse(url)
  return  url_parsed["host"],
    url_parsed["path"],
	url_parsed["port"],
    url_parsed["option"]
end

