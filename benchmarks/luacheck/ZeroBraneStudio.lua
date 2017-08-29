-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local q = EscapeMagic

-- api loading depends on Lua interpreter
-- and loaded specs

------------
-- API

local function newAPI(api)
  api = api or {}
  for i in pairs(api) do
    api[i] = nil
  end
  -- tool tip info and reserved names
  api.tip = {
    staticnames = {},
    keys = {},
    finfo = {},
    finfoclass = {},
    shortfinfo = {},
    shortfinfoclass = {},
  }
  -- autocomplete hierarchy
  api.ac = {
    childs = {},
  }

  return api
end

local apis = {
  none = newAPI(),
  lua = newAPI(),
}

function GetApi(apitype) return apis[apitype] or apis.none end

----------
-- API loading

local function gennames(tab, prefix)
  for i,v in pairs(tab) do
    v.classname = (prefix and (prefix..".") or "")..i
    if (v.childs) then
      gennames(v.childs,v.classname)
    end
  end
end

local function addAPI(ftype, fname) -- relative to API directory
  local env = apis[ftype] or newAPI()

  local res
  local api = ide.apis[ftype][fname]

  if type(api) == 'table' then
    res = api
  else
    local fn, err = loadfile(api)
    if err then
      ide:Print(TR("Error while loading API file: %s"):format(err))
      return
    end
    local suc
    suc, res = pcall(fn, env.ac.childs)
    if (not suc) then
      ide:Print(TR("Error while processing API file: %s"):format(res))
      return
    end
    -- cache the result
    ide.apis[ftype][fname] = res
  end
  apis[ftype] = env

  gennames(res)
  for i,v in pairs(res) do env.ac.childs[i] = v end
end

local function loadallAPIs(only, subapis, known)
  for ftype, v in pairs(only and {[only] = ide.apis[only]} or ide.apis) do
    if (not known or known[ftype]) then
      for fname in pairs(v) do
        if (not subapis or subapis[fname]) then addAPI(ftype, fname) end
      end
    end
  end
end

---------
-- ToolTip and reserved words list
-- also fixes function descriptions

local function fillTips(api,apibasename)
  local apiac = api.ac
  local tclass = api.tip

  tclass.staticnames = {}
  tclass.keys = {}
  tclass.finfo = {}
  tclass.finfoclass = {}
  tclass.shortfinfo = {}
  tclass.shortfinfoclass = {}

  local staticnames = tclass.staticnames
  local keys = tclass.keys
  local finfo = tclass.finfo
  local finfoclass = tclass.finfoclass
  local shortfinfo = tclass.shortfinfo
  local shortfinfoclass = tclass.shortfinfoclass

  local function traverse (tab, libname, format)
    if not tab.childs then return end
    format = tab.format or format
    for key,info in pairs(tab.childs) do
      local fullkey = (libname ~= "" and libname.."." or "")..key
      traverse(info, fullkey, format)

      if info.type == "function" or info.type == "method" or info.type == "value" then
        local frontname = (info.returns or "(?)").." "..fullkey.." "..(info.args or "(?)")
        frontname = frontname:gsub("\n"," "):gsub("\t","")
        local description = info.description or ""

        -- build info
        local inf = ((info.type == "value" and "" or frontname.."\n")
          ..description)
        local sentence = description:match("^(.-)%. ?\n")
        local infshort = ((info.type == "value" and "" or frontname.."\n")
          ..(sentence and sentence.."..." or description))
        if type(format) == 'function' then -- apply custom formatting if requested
          inf = format(fullkey, info, inf)
          infshort = format(fullkey, info, infshort)
        end
        local infshortbatch = (info.returns and info.args) and frontname or infshort

        -- add to infoclass
        if not finfoclass[libname] then finfoclass[libname] = {} end
        if not shortfinfoclass[libname] then shortfinfoclass[libname] = {} end
        finfoclass[libname][key] = inf
        shortfinfoclass[libname][key] = infshort

        -- add to info
        if not finfo[key] or #finfo[key]<200 then
          if finfo[key] then finfo[key] = finfo[key] .. "\n\n"
          else finfo[key] = "" end
          finfo[key] = finfo[key] .. inf
        elseif not finfo[key]:match("\n %(%.%.%.%)$") then
          finfo[key] = finfo[key].."\n (...)"
        end

        -- add to shortinfo
        if not shortfinfo[key] or #shortfinfo[key]<200 then
          if shortfinfo[key] then shortfinfo[key] = shortfinfo[key] .. "\n"
          else shortfinfo[key] = "" end
          shortfinfo[key] = shortfinfo[key] .. infshortbatch
        elseif not shortfinfo[key]:match("\n %(%.%.%.%)$") then
          shortfinfo[key] = shortfinfo[key].."\n (...)"
        end
      end
      if info.type == "keyword" then
        keys[key] = true
      end
      staticnames[key] = true
    end
  end
  traverse(apiac,apibasename)
end

local function generateAPIInfo(only)
  for i,api in pairs(apis) do
    if ((not only) or i == only) then
      fillTips(api,"")
    end
  end
end

local function updateAssignCache(editor)
  if (editor.spec.typeassigns and not editor.assignscache) then
    local assigns = editor.spec.typeassigns(editor)
    editor.assignscache = {
      assigns = assigns,
      line = editor:GetCurrentLine(),
    }
  end
end

-- assumes a tidied up string (no spaces, braces..)
local function resolveAssign(editor,tx)
  local ac = editor.api.ac
  local sep = editor.spec.sep
  local anysep = "["..q(sep).."]"
  local assigns = editor.assignscache and editor.assignscache.assigns
  local function getclass(tab,a)
    local key,rest = a:match("([%w_]+)"..anysep.."(.*)")
    key = tonumber(key) or key -- make this work for childs[0]

    if (key and rest and tab.childs) then
      if (tab.childs[key]) then
        return getclass(tab.childs[key],rest)
      end
      -- walk inheritance if we weren't in childs
      if tab.inherits then
        local bestTab = tab
        local bestRest = a
        for base in tab.inherits:gmatch("[%w_"..q(sep).."]+") do
          local tab = ac
          -- map "a.b.c" to class hierarchy (a.b.c)
          for class in base:gmatch("[%w_]+") do tab = tab.childs[class] end
          if tab then
              local t,r = getclass(tab, a)
              if (string.len(r) < string.len(bestRest)) then
                 --we found a better match
                 bestTab = t
                 bestRest = r
              end
          end
        end
        -- did we find anything good in our inherits, then return it
        if string.len(bestRest) < string.len(a) then
          return bestTab, bestRest
        end
      end
    end

    -- process valuetype, but only if it doesn't reference the current tab
    if (tab.valuetype and tab ~= ac.childs[tab.valuetype]) then
      return getclass(ac,tab.valuetype..sep:sub(1,1)..a)
    end

    return tab,a
  end

  local c
  if (assigns) then
    -- find assign
    local change, n, refs, stopat = true, 0, {}, os.clock() + 0.2
    while (change) do
      -- abort the check if the auto-complete is taking too long
      if n > 50 and os.clock() > stopat then
        if ide.config.acandtip.warning then
          ide:Print("Warning: Auto-complete was aborted after taking too long to complete."
            .. " Please report this warning along with the text you were typing to support@zerobrane.com.")
        end
        break
      else
        n = n + 1
      end

      local classname = nil
      c = ""
      change = false
      for w,s in tx:gmatch("([%w_]+)("..anysep.."?)") do
        local old = classname
        -- check if what we have so far can be matched with a class name
        -- this can happen if it's a reference to a value with a known type
        classname = classname or assigns[c..w]
        if (s ~= "" and old ~= classname) then
          -- continue checking unless this can lead to recursive substitution
          change = not classname:find("^"..w..anysep) and not classname:find("^"..c..w..anysep)
          c = classname..s
        else
          c = c..w..s
        end
      end
      -- check for loops in type assignment
      if refs[tx] then break end
      refs[tx] = c
      tx = c
      -- if there is any class duplication, abort the loop
      if classname and select(2, c:gsub(classname, classname)) > 1 then break end
    end
  else
    c = tx
  end

  -- then work from api
  return getclass(ac,c)
end

function GetTipInfo(editor, content, short, fullmatch)
  if not content then return end

  updateAssignCache(editor)

  -- try to resolve the class
  content = content:gsub("%b[]",".0")
  local tab = resolveAssign(editor, content)
  local sep = editor.spec.sep
  local anysep = "["..q(sep).."]"

  local caller = content:match("([%w_]+)%s*%(?%s*$")
  local class = (tab and tab.classname
    or caller and content:match("([%w_]+)"..anysep..caller.."%s*%(?%s*$") or "")
  local tip = editor.api.tip

  local classtab = short and tip.shortfinfoclass or tip.finfoclass
  local funcstab = short and tip.shortfinfo or tip.finfo

  if (editor.assignscache and not (class and classtab[class])) then
    local assigns = editor.assignscache.assigns
    class = assigns and assigns[class] or class
  end

  local res = (caller and (class and classtab[class]) and classtab[class][caller]
    or (not fullmatch and funcstab[caller] or nil))
  -- some values may not have descriptions (for example, true/false);
  -- don't return empty strings as they are displayed as empty tooltips.
  return res and #res > 0 and res or nil
end

local function reloadAPI(only, subapis, known)
  if only then newAPI(apis[only]) end
  loadallAPIs(only, subapis, known)
  generateAPIInfo(only)
end

function ReloadAPIs(group, known)
  -- special case to reload all
  if group == "*" then
    if not known then
      known = {}
      for _, spec in pairs(ide.specs) do
        if (spec.apitype) then
          known[spec.apitype] = true
        end
      end
      -- by default load every known api except lua
      known.lua = false
    end
    reloadAPI(nil, nil, known)
    return
  end
  local interp = ide.interpreter
  local cfgapi = ide.config.api
  local fname = interp and interp.fname
  local intapi = cfgapi and fname and cfgapi[fname]
  local apinames = {}
  -- general APIs as configured
  for _, v in ipairs(type(cfgapi) == 'table' and cfgapi or {}) do apinames[v] = true end
  -- interpreter-specific APIs as configured
  for _, v in ipairs(type(intapi) == 'table' and intapi or {}) do apinames[v] = true end
  -- interpreter APIs
  for _, v in ipairs(interp and interp.api or {}) do apinames[v] = true end
  reloadAPI(group, apinames, known)
end

-------------
-- Dynamic Words

local dywordentries = {}
local dynamicwords = {}

local function addDynamicWord (api,word)
  if api.tip.keys[word] or api.tip.staticnames[word] then return end
  local cnt = dywordentries[word]
  if cnt then
    dywordentries[word] = cnt + 1
    return
  end
  dywordentries[word] = 1
  local wlow = word:lower()
  for i=0,#word do
    local k = wlow:sub(1,i)
    dynamicwords[k] = dynamicwords[k] or {}
    table.insert(dynamicwords[k], word)
  end
end
local function removeDynamicWord (api,word)
  if api.tip.keys[word] or api.tip.staticnames[word] then return end
  local cnt = dywordentries[word]
  if not cnt then return end

  if (cnt == 1) then
    dywordentries[word] = nil
    for i=0,#word do
      local wlow = word:lower()
      local k = wlow : sub (1,i)
      local page = dynamicwords[k]
      if page then
        local cnt  = #page
        for n=1,cnt do
          if page[n] == word then
            if cnt == 1 then
              dynamicwords[k] = nil
            else
              table.remove(page,n)
            end
            break
          end
        end
      end
    end
  else
    dywordentries[word] = cnt - 1
  end
end
function DynamicWordsReset ()
  dywordentries = {}
  dynamicwords = {}
end

local function getEditorLines(editor,line,numlines)
  return editor:GetTextRangeDyn(
    editor:PositionFromLine(line),editor:PositionFromLine(line+numlines+1))
end

function DynamicWordsAdd(editor,content,line,numlines)
  if ide.config.acandtip.nodynwords then return end
  local api = editor.api
  local anysep = "["..q(editor.spec.sep).."]"
  content = content or getEditorLines(editor,line,numlines)
  for word in content:gmatch(anysep.."?%s*([a-zA-Z_]+[a-zA-Z_0-9]+)") do
    addDynamicWord(api,word)
  end
end

function DynamicWordsRem(editor,content,line,numlines)
  if ide.config.acandtip.nodynwords then return end
  local api = editor.api
  local anysep = "["..q(editor.spec.sep).."]"
  content = content or getEditorLines(editor,line,numlines)
  for word in content:gmatch(anysep.."?%s*([a-zA-Z_]+[a-zA-Z_0-9]+)") do
    removeDynamicWord(api,word)
  end
end

function DynamicWordsRemoveAll(editor)
  if ide.config.acandtip.nodynwords then return end
  DynamicWordsRem(editor,editor:GetTextDyn())
end

------------
-- Final Autocomplete

local cachemain = {}
local cachemethod = {}
local laststrategy
local function getAutoCompApiList(childs,fragment,method)
  fragment = fragment:lower()
  local strategy = ide.config.acandtip.strategy
  if (laststrategy ~= strategy) then
    cachemain = {}
    cachemethod = {}
    laststrategy = strategy
  end

  local cache = method and cachemethod or cachemain

  if (strategy == 2) then
    local wlist = cache[childs]
    if not wlist then
      wlist = " "
      for i,v in pairs(childs) do
        -- in some cases (tip.finfo), v may be a string; check for that first.
        -- if a:b typed, then value (type == "value") not allowed
        -- if a.b typed, then method (type == "method") not allowed
        if type(v) ~= 'table' or (v.type and
          ((method and v.type ~= "value")
            or (not method and v.type ~= "method"))) then
          wlist = wlist..i.." "
        end
      end
      cache[childs] = wlist
    end
    local ret = {}
    local g = string.gmatch
    local pat = fragment ~= "" and ("%s("..fragment:gsub(".",
        function(c)
          local l = c:lower()..c:upper()
          return "["..l.."][%w_]*"
        end)..")") or "([%w_]+)"
    pat = pat:gsub("%s","")
    for c in g(wlist,pat) do
      table.insert(ret,c)
    end

    return ret
  end

  if cache[childs] and cache[childs][fragment] then
    return cache[childs][fragment]
  end

  local t = {}
  cache[childs] = t

  local sub = strategy == 1
  for key,v in pairs(childs) do
    -- in some cases (tip.finfo), v may be a string; check for that first.
    -- if a:b typed, then value (type == "value") not allowed
    -- if a.b typed, then method (type == "method") not allowed
    if type(v) ~= 'table' or (v.type and
      ((method and v.type ~= "value")
        or (not method and v.type ~= "method"))) then
      local used = {}
      local kl = key:lower()
      for i=0,#key do
        local k = kl:sub(1,i)
        t[k] = t[k] or {}
        used[k] = true
        table.insert(t[k],key)
      end
      if (sub) then
        -- find camel case / _ separated subwords
        -- glfwGetGammaRamp -> g, gg, ggr
        -- GL_POINT_SPRIT -> g, gp, gps
        local last = ""
        for ks in string.gmatch(key,"([A-Z%d]*[a-z%d]*_?)") do
          local k = last..(ks:sub(1,1):lower())
          last = k

          t[k] = t[k] or {}
          if (not used[k]) then
            used[k] = true
            table.insert(t[k],key)
          end
        end
      end
    end
  end

  return t[fragment] or {}
end

function CreateAutoCompList(editor,key,pos)
  local api = editor.api
  local tip = api.tip
  local ac = api.ac
  local sep = editor.spec.sep

  local method = key:match(":[^"..q(sep).."]*$") ~= nil

  -- ignore keywords
  if tip.keys[key] then return end

  updateAssignCache(editor)

  local tab,rest = resolveAssign(editor,key)
  local progress = tab and tab.childs
  ide:SetStatusFor(progress and tab.classname and ("Auto-completing '%s'..."):format(tab.classname) or "")
  if not progress then return end

  if (tab == ac) then
    local _, krest = rest:match("([%w_]+)["..q(sep).."]([%w_]*)%s*$")
    if (krest) then
      tab = #krest >= (ide.config.acandtip.startat or 2) and tip.finfo or {}
      rest = krest:gsub("[^%w_]","")
    else
      rest = rest:gsub("[^%w_]","")
    end
  else
    rest = rest:gsub("[^%w_]","")
  end

  -- list from api
  local apilist = getAutoCompApiList(tab.childs or tab,rest,method)

  local function addInheritance(tab, apilist, seen)
    if not tab.inherits then return end
    for base in tab.inherits:gmatch("[%w_"..q(sep).."]+") do
      local tab = ac
      -- map "a.b.c" to class hierarchy (a.b.c)
      for class in base:gmatch("[%w_]+") do tab = tab.childs[class] end
  
      if tab and not seen[tab] then
        seen[tab] = true
        for _,v in pairs(getAutoCompApiList(tab.childs,rest,method)) do
          table.insert(apilist, v)
        end
        addInheritance(tab, apilist, seen)
    end
    end
  end

  -- handle (multiple) inheritance; add matches from the parent class/lib
  addInheritance(tab, apilist, {[tab] = true})

  -- include local/global variables
  if ide.config.acandtip.symbols and not key:find(q(sep)) then
    local vars, context = {}
    local tokens = editor:GetTokenList()
    for _, token in ipairs(tokens) do
      if token.fpos and pos and token.fpos > pos then break end
      if token[1] == 'Id' or token[1] == 'Var' then
        local var = token.name
        if var:find(key, 1, true) == 1
        -- skip the variable formed by what's being typed
        and (not token.fpos or not pos or token.fpos < pos-#key) then
          -- if it's a global variable, store in the auto-complete list,
          -- but if it's local, store separately as it needs to be checked
          table.insert(token.context[var] and vars or apilist, var)
        end
        context = token.context
      end
    end
    for _, var in pairs(context and vars or {}) do
      if context[var] then table.insert(apilist, var) end
    end
  end

  -- include dynamic words
  local last = key:match("([%w_]+)%s*$")
  if (last and #last >= (ide.config.acandtip.startat or 2)) then
    last = last:lower()
    for _, v in ipairs(dynamicwords[last] or {}) do
      -- ignore if word == last and sole user
      if (v:lower() == last and dywordentries[v] == 1) then break end
      table.insert(apilist, v)
    end
  end

  local li
  if apilist then
    if (#rest > 0) then
      local strategy = ide.config.acandtip.strategy

      if (strategy == 2 and #apilist < 128) then
        -- when matching "ret": "ret." < "re.t" < "r.et"
        local patany = rest:gsub(".", function(c) return "["..c:lower()..c:upper().."](.-)" end)
        local patcase = rest:gsub(".", function(c) return c.."(.-)" end)
        local weights = {}
        local penalty = 0.1
        local function weight(str)
          if not weights[str] then
            local w = 0
            str:gsub(patany,function(...)
                local l = {...}
                -- penalize gaps between matches, more so at the beginning
                for n, v in ipairs(l) do w = w + #v * (1 + (#l-n)*penalty) end
              end)
            weights[str] = w + (str:find(patcase) and 0 or penalty)
          end
          return weights[str]
        end
        table.sort(apilist,function(a,b)
            local ma, mb = weight(a), weight(b)
            if (ma == mb) then return a:lower()<b:lower() end
            return ma<mb
          end)
      else
        table.sort(apilist,function(a,b)
            local ma,mb = a:sub(1,#rest)==rest, b:sub(1,#rest)==rest
            if (ma and mb) or (not ma and not mb) then return a<b end
            return ma
          end)
      end
    else
      table.sort(apilist)
    end

    local prev = apilist[#apilist]
    for i = #apilist-1,1,-1 do
      if prev == apilist[i] then
        table.remove(apilist, i+1)
      else prev = apilist[i] end
    end

    li = table.concat(apilist," ")
  end
  return li and #li > 1024 and li:sub(1,1024).."..." or li
end
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
---------------------------------------------------------

local ide = ide
local q = EscapeMagic
local unpack = table.unpack or unpack

local dc = wx.wxMemoryDC()
local function getFontHeight(font)
  dc:SetFont(font)
  local _, h = dc:GetTextExtent("AZ")
  dc:SetFont(wx.wxNullFont)
  return h
end

local pending
local function pendingInput()
  if ide.osname ~= 'Unix' then
    ide:GetApp():SafeYieldFor(wx.NULL, wx.wxEVT_CATEGORY_USER_INPUT + wx.wxEVT_CATEGORY_UI)
  end
  return pending
end
local showProgress
local function showCommandBar(params)
  local onDone, onUpdate, onItem, onSelection, defaultText, selectedText =
    params.onDone, params.onUpdate, params.onItem, params.onSelection,
    params.defaultText, params.selectedText
  local row_width = ide.config.commandbar.width or 0
  if row_width < 1 then
    row_width = math.max(450, math.floor(row_width * ide:GetMainFrame():GetClientSize():GetWidth()))
  end

  local maxlines = ide.config.commandbar.maxlines
  local lines = {}
  local linenow = 0

  local nb = ide:GetEditorNotebook()
  local pos = nb:GetScreenPosition()
  if pos then
    local miny
    for p = 0, nb:GetPageCount()-1 do
      local y = nb:GetPage(p):GetScreenPosition():GetY()
      -- just in case, compare with the position of the notebook itself;
      -- this is needed because the tabs that haven't been refreshed yet
      -- may report 0 as their screen position on Linux, which is incorrect.
      if y > pos:GetY() and (not miny or y < miny) then miny = y end
    end
    pos:SetX(pos:GetX()+nb:GetClientSize():GetWidth()-row_width-16)
    pos:SetY((miny or pos:GetY())+2)
  else
    pos = wx.wxDefaultPosition
  end

  local tempctrl = ide:IsValidCtrl(ide:GetProjectTree()) and ide:GetProjectTree() or wx.wxTreeCtrl()
  local tfont = tempctrl:GetFont()
  local ffont = (ide:GetEditor() or ide:CreateBareEditor()):GetFont()
  ffont:SetPointSize(ffont:GetPointSize()+2)
  local sfont = wx.wxFont(tfont)
  tfont:SetPointSize(tfont:GetPointSize()+2)

  local sash = ide:GetUIManager():GetArtProvider():GetMetric(wxaui.wxAUI_DOCKART_SASH_SIZE)
  local border = sash + 2
  local hoffset = 4
  local voffset = 2

  local line_height = getFontHeight(ffont)
  local row_height = line_height + getFontHeight(sfont) + voffset * 3 -- before, after, and between

  local frame = wx.wxFrame(ide:GetMainFrame(), wx.wxID_ANY, "Command Bar",
    pos, wx.wxDefaultSize,
    wx.wxFRAME_NO_TASKBAR + wx.wxFRAME_FLOAT_ON_PARENT + wx.wxNO_BORDER)
  local panel = wx.wxPanel(frame or ide:GetMainFrame(), wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize, wx.wxFULL_REPAINT_ON_RESIZE)
  local search = wx.wxTextCtrl(panel, wx.wxID_ANY, "\1",
    wx.wxDefaultPosition,
    -- make the text control proportional to the font size
    wx.wxSize(row_width, getFontHeight(tfont) + voffset),
    wx.wxTE_PROCESS_ENTER + wx.wxTE_PROCESS_TAB + wx.wxNO_BORDER)
  local results = wx.wxScrolledWindow(panel, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(0, 0))

  local style, styledef = ide.config.styles, StylesGetDefault()
  local textcolor = wx.wxColour(unpack(style.text.fg or styledef.text.fg))
  local backcolor = wx.wxColour(unpack(style.text.bg or styledef.text.bg))
  local selcolor = wx.wxColour(unpack(style.caretlinebg.bg or styledef.caretlinebg.bg))
  local pancolor = ide:GetUIManager():GetArtProvider():GetColour(wxaui.wxAUI_DOCKART_SASH_COLOUR)
  local borcolor = ide:GetUIManager():GetArtProvider():GetColour(wxaui.wxAUI_DOCKART_BORDER_COLOUR)

  search:SetBackgroundColour(backcolor)
  search:SetForegroundColour(textcolor)
  search:SetFont(tfont)

  local nbrush = wx.wxBrush(backcolor, wx.wxSOLID)
  local sbrush = wx.wxBrush(selcolor, wx.wxSOLID)
  local bbrush = wx.wxBrush(pancolor, wx.wxSOLID)
  local lpen = wx.wxPen(borcolor, 1, wx.wxDOT)
  local bpen = wx.wxPen(borcolor, 1, wx.wxSOLID)
  local npen = wx.wxPen(backcolor, 1, wx.wxSOLID)

  local topSizer = wx.wxFlexGridSizer(2, 1, -border*2, 0)
  topSizer:SetFlexibleDirection(wx.wxVERTICAL)
  topSizer:AddGrowableRow(1, 1)
  topSizer:Add(search, wx.wxSizerFlags(0):Expand():Border(wx.wxALL, border))
  topSizer:Add(results, wx.wxSizerFlags(1):Expand():Border(wx.wxALL, border))
  panel:SetSizer(topSizer)
  topSizer:Fit(frame) -- fit the frame/panel around the controls

  local minheight = frame:GetClientSize():GetHeight()

  -- make a one-time callback;
  -- needed because KILL_FOCUS handler can be called after closing window
  local function onExit(index)
    onExit = function() end
    onDone(index and lines[index], index, search:GetValue())
    -- delay destroying the frame until all the related processing is done
    ide:DoWhenIdle(function() if ide:IsValidCtrl(frame) then frame:Destroy() end end)
  end

  local linesnow
  local function onPaint(event)
    if not ide:IsValidCtrl(frame) then return end

    -- adjust the scrollbar before working with the canvas
    local _, starty = results:GetViewStart()
    -- recalculate the scrollbars if the number of lines shown has changed
    if #lines ~= linesnow then
      -- adjust the starting line when the current line is the last one
      if linenow > starty+maxlines then starty = starty + 1 end
      results:SetScrollbars(1, row_height, 1, #lines, 0, starty*row_height, false)
      linesnow = #lines
    end

    local dc = wx.wxMemoryDC(results)
    results:PrepareDC(dc)

    local size = results:GetVirtualSize()
    local w,h = size:GetWidth(),size:GetHeight()
    local bitmap = wx.wxBitmap(w,h)
    dc:SelectObject(bitmap)

    -- clear the background
    dc:SetBackground(nbrush)
    dc:Clear()

    dc:SetTextForeground(textcolor)
    dc:SetBrush(sbrush)
    for r = 1, #lines do
      if r == linenow then
        dc:SetPen(wx.wxTRANSPARENT_PEN)
        dc:DrawRectangle(0, row_height*(r-1), row_width, row_height+1)
      end
      dc:SetPen(lpen)
      dc:DrawLine(hoffset, row_height*(r-1), row_width-hoffset*2, row_height*(r-1))

      local fline, sline = onItem(lines[r])
      if fline then
        dc:SetFont(ffont)
        dc:DrawText(fline, hoffset, row_height*(r-1)+voffset)
      end
      if sline then
        dc:SetFont(sfont)
        dc:DrawText(sline, hoffset, row_height*(r-1)+line_height+voffset*2)
      end
    end

    dc:SetPen(wx.wxNullPen)
    dc:SetBrush(wx.wxNullBrush)
    dc:SelectObject(wx.wxNullBitmap)
    dc:delete()

    dc = wx.wxPaintDC(results)
    dc:DrawBitmap(bitmap, 0, 0, true)
    dc:delete()
  end

  local progress = 0
  showProgress = function(newprogress)
    progress = newprogress
    if not ide:IsValidCtrl(panel) then return end
    panel:Refresh()
    panel:Update()
  end

  local function onPanelPaint(event)
    if not ide:IsValidCtrl(frame) then return end

    local dc = wx.wxBufferedPaintDC(panel)
    dc:SetBrush(bbrush)
    dc:SetPen(bpen)

    local psize = panel:GetClientSize()
    dc:DrawRectangle(0, 0, psize:GetWidth(), psize:GetHeight())
    dc:DrawRectangle(sash+1, sash+1, psize:GetWidth()-2*(sash+1), psize:GetHeight()-2*(sash+1))

    if progress > 0 then
      dc:SetBrush(nbrush)
      dc:SetPen(npen)
      dc:DrawRectangle(sash+2, 1, math.floor((row_width-4)*progress), sash)
    end

    dc:SetPen(wx.wxNullPen)
    dc:SetBrush(wx.wxNullBrush)
    dc:delete()
  end

  local linewas -- line that was reported when updated
  local function onTextUpdated()
    pending = ide:GetApp():GetMainLoop():IsYielding()
    if pending then return end

    local text = search:GetValue()
    lines = onUpdate(text)
    linenow = #text > 0 and #lines > 0 and 1 or 0
    linewas = nil

    -- the control can disappear during onUpdate as it can be closed, so check for that
    if not ide:IsValidCtrl(frame) then return end

    local size = frame:GetClientSize()
    local height = minheight + row_height*math.min(maxlines,#lines)
    if height ~= size:GetHeight() then
      results:SetScrollbars(1, 1, 1, 1, 0, 0, false)
      size:SetHeight(height)
      frame:SetClientSize(size)
    end

    results:Refresh()
  end

  local function onKeyDown(event)
    if ide:GetApp():GetMainLoop():IsYielding() then
      event:Skip()
      return
    end

    local linesnow = #lines
    local keycode = event:GetKeyCode()
    if keycode == wx.WXK_RETURN then
      onExit(linenow)
      return
    elseif event:GetModifiers() ~= wx.wxMOD_NONE then
      event:Skip()
      return
    elseif keycode == wx.WXK_UP then
      if linesnow > 0 then
        linenow = linenow - 1
        if linenow <= 0 then linenow = linesnow end
      end
    elseif keycode == wx.WXK_DOWN then
      if linesnow > 0 then
        linenow = linenow % linesnow + 1
      end
    elseif keycode == wx.WXK_PAGEDOWN then
      if linesnow > 0 then
        linenow = linenow + maxlines
        if linenow > linesnow then linenow = linesnow end
      end
    elseif keycode == wx.WXK_PAGEUP then
      if linesnow > 0 then
        linenow = linenow - maxlines
        if linenow <= 0 then linenow = 1 end
      end
    elseif keycode == wx.WXK_ESCAPE then
      onExit(false)
      return
    else
      event:Skip()
      return
    end

    local _, starty = results:GetViewStart()
    if linenow < starty+1 then results:Scroll(-1, linenow-1)
    elseif linenow > starty+maxlines then results:Scroll(-1, linenow-maxlines) end
    results:Refresh()
  end

  local function onMouseLeftDown(event)
    local pos = event:GetPosition()
    local _, y = results:CalcUnscrolledPosition(pos.x, pos.y)
    onExit(math.floor(y / row_height)+1)
  end

  local function onIdle(event)
    if pending then return onTextUpdated() end
    if linewas == linenow then return end
    linewas = linenow
    if linenow == 0 then return end

    -- save the selection/insertion point as it's reset on Linux (wxwidgets 2.9.5)
    local ip = search:GetInsertionPoint()
    local f, t = search:GetSelection()

    -- this may set focus to a different object/tab,
    -- so disable the focus event and then set the focus back
    search:SetEvtHandlerEnabled(false)
    onSelection(lines[linenow], search:GetValue())
    search:SetFocus()
    search:SetEvtHandlerEnabled(true)
    if ide.osname == 'Unix' then
      search:SetInsertionPoint(ip)
      search:SetSelection(f, t)
    end
  end

  panel:Connect(wx.wxEVT_PAINT, onPanelPaint)
  panel:Connect(wx.wxEVT_ERASE_BACKGROUND, function() end)
  panel:Connect(wx.wxEVT_IDLE, onIdle)

  results:Connect(wx.wxEVT_PAINT, onPaint)
  results:Connect(wx.wxEVT_LEFT_DOWN, onMouseLeftDown)
  results:Connect(wx.wxEVT_ERASE_BACKGROUND, function() end)

  search:SetFocus()
  search:Connect(wx.wxEVT_KEY_DOWN, onKeyDown)
  search:Connect(wx.wxEVT_COMMAND_TEXT_UPDATED, onTextUpdated)
  search:Connect(wx.wxEVT_COMMAND_TEXT_ENTER, function() onExit(linenow) end)
  -- this could be done with calling `onExit`, but on OSX KILL_FOCUS is called before
  -- mouse LEFT_DOWN, which closes the panel before the results are taken;
  -- to avoid this, `onExit` call is delayed and handled in IDLE event
  search:Connect(wx.wxEVT_KILL_FOCUS, function() onExit() end)

  frame:Show(true)
  frame:Update()
  frame:Refresh()

  search:SetValue((defaultText or "")..(selectedText or ""))
  search:SetSelection(#(defaultText or ""), -1)
end

local sep = "[/\\%-_ ]+"
local weights = {onegram = 0.1, digram = 0.4, trigram = 0.5}
local cache = {}
local missing = 3 -- penalty for missing symbols (1 missing == N matching)
local casemismatch = 0.9 -- score for case mismatch (%% of full match)
local function score(p, v)
  local function ngrams(str, num, low, needcache)
    local key = str..'\1'..num
    if cache[key] then return unpack(cache[key]) end

    local t, l, p = {}, {}, 0
    for i = 1, #str-num+1 do
      local pair = str:sub(i, i+num-1)
      p = p + (t[pair] and 0 or 1)
      if low and pair:find('%u') then l[pair:lower()] = casemismatch end
      t[pair] = 1
    end
    if needcache then cache[key] = {t, p, l} end
    return t, p, l
  end

  local function overlap(pattern, value, num)
    local ph, ps = ngrams(pattern, num, false, true)
    local vh, vs, vl = ngrams(value, num, true)
    if ps + vs == 0 then return 0 end

    local is = 0 -- intersection of two sets of ngrams
    for k in pairs(ph) do is = is + (vh[k] or vl[k:lower()] or 0) end
    return is / (ps + vs) - (num == 1 and missing * (ps - is) / (ps + vs) or 0)
  end

  local key = p..'\2'..v
  if not cache[key] then
    -- ignore all whitespaces in the pattern for one-gram comparison
    local score = weights.onegram * overlap(p:gsub("%s+",""), v, 1)
    if score > 0 then -- don't bother with those that can't even score 1grams
      p = ' '..(p:gsub(sep, ' '))
      v = ' '..(v:gsub(sep, ' '))
      score = score + weights.digram * overlap(p, v, 2)
      score = score + weights.trigram * overlap(' '..p, ' '..v, 3)
    end
    cache[key] = 2 * 100 * score
  end
  return cache[key]
end

local function commandBarScoreItems(t, pattern, limit)
  local r, plen = {}, #(pattern:gsub("%s+",""))
  local maxp = 0
  local num = 0
  local total = #t
  local prefilter = ide.config.commandbar and tonumber(ide.config.commandbar.prefilter)
  -- anchor for 1-2 symbol patterns to speed up search
  local needanchor = prefilter and prefilter * 4 <= #t and plen <= 2
  local pref = pattern:gsub("[^%w_]+",""):sub(1,4):lower()
  local filter = prefilter and prefilter <= #t
    -- expand `abc` into `a.*b.*c`, but limit the prefix to avoid penalty for `s.*s.*s.*....`
    -- if there are too many records to filter (prefilter*20), then only search for substrings
    and (prefilter * 10 <= #t and pref or pref:gsub(".", "%1.*"):gsub("%.%*$",""))
    or nil
  local lastpercent = 0
  for n, v in ipairs(t) do
    -- there was additional input while scoring, so abort to check for it
    local timeToCheck = n % ((prefilter or 250) * 10) == 0
    if timeToCheck and pendingInput() then r = {}; break end
    local progress = n/total
    local percent = math.floor(progress * 100 + 0.5)
    if timeToCheck and percent ~= lastpercent then
      lastpercent = percent
      if showProgress then showProgress(progress) end
    end

    if #v >= plen then
      local match = filter and v:lower():find(filter)
      -- check if the current name needs to be prefiltered or anchored (for better performance);
      -- if it needs to be anchored, then anchor it at the beginning of the string or the word
      if not filter or (match and (not needanchor or match == 1 or v:find("^[%p%s]", match-1))) then
        local p = score(pattern, v)
        maxp = math.max(p, maxp)
        if p > 1 and p > maxp / 4 then
          num = num + 1
          r[num] = {v, p}
        end
      end
    end
  end
  table.sort(r, function(a, b) return a[2] > b[2] end)
  -- limit the list to be displayed
  -- `r[limit+1] = nil` is not desired as the resulting table may be sorted incorrectly
  if tonumber(limit) and limit < #r then
    local tmp = r
    r = {}
    for i = 1, limit do r[i] = tmp[i] end
  end
  if showProgress then showProgress(0) end
  return r
end

local markername = "commandbar.background"
local mac = ide.osname == 'Macintosh'
local win = ide.osname == 'Windows'
local special = {SYMBOL = '@', LINE = ':', METHOD = ';'}
local tabsep = "\0"
local function name2index(name)
  local p = name:find(tabsep)
  return p and tonumber(name:sub(p + #tabsep)) or nil
end
function ShowCommandBar(default, selected)
  local styles = ide.config.styles
  -- re-register the marker as the colors might have changed
  local marker = ide:AddMarker(markername,
    wxstc.wxSTC_MARK_BACKGROUND, styles.text.fg, styles.caretlinebg.bg)

  local nb = ide:GetEditorNotebook()
  local selection = nb:GetSelection()
  local maxitems = ide.config.commandbar.maxitems
  local files, preview, origline, functions, methods

  local function markLine(ed, toline)
    ed:MarkerDefine(ide:GetMarker(markername))
    ed:MarkerDeleteAll(marker)
    ed:MarkerAdd(toline-1, marker)
    -- store the original line if not stored yet
    origline = origline or (ed:GetCurrentLine()+1)
    ed:EnsureVisibleEnforcePolicy(toline-1)
  end

  showCommandBar({
    defaultText = default or "",
    selectedText = selected or "",
    onDone = function(t, enter, text)
      if not mac then nb:Freeze() end

      -- delete all current line markers if any; restore line position
      local ed = ide:GetEditor()
      if ed and origline then
        ed:MarkerDeleteAll(marker)
        -- only restore original line if Escape was used (enter == false)
        if enter == false then ed:EnsureVisibleEnforcePolicy(origline-1) end
      end

      local pindex = preview and nb:GetPageIndex(preview)
      if enter then
        local fline, sline, tabindex = unpack(t or {})

        -- jump to symbol; tabindex has the position of the symbol
        if text and text:find(special.SYMBOL) then
          if sline and tabindex then
            local index = name2index(sline)
            local editor = index and nb:GetPage(index):DynamicCast("wxStyledTextCtrl")
            if not editor then
              local doc = ide:FindDocument(sline)
              -- reload the file (including the preview to refresh its symbols in the outline)
              editor = LoadFile(sline, (not doc or doc:GetTabIndex() == pindex) and preview or nil)
            end
            if editor then
              if pindex and pindex ~= ide:GetDocument(editor):GetTabIndex() then ClosePage(pindex) end
              editor:SetFocus() -- in case the focus is on some other panel
              editor:GotoPos(tabindex-1)
              editor:EnsureVisibleEnforcePolicy(editor:LineFromPosition(tabindex-1))
            end
          end
        -- insert selected method
        elseif text and text:find('^%s*'..special.METHOD) then
          if ed then -- clean up text and insert at the current location
            local method = sline
            local isfunc = methods.desc[method][1]:find(q(method).."%s*%(")
            local text = method .. (isfunc and "()" or "")
            local pos = ed:GetCurrentPos()
            ed:InsertTextDyn(pos, text)
            ed:EnsureVisibleEnforcePolicy(ed:LineFromPosition(pos))
            ed:GotoPos(pos + #method + (isfunc and 1 or 0))
            if isfunc then -- show the tooltip
              ide.frame:AddPendingEvent(wx.wxCommandEvent(
                wx.wxEVT_COMMAND_MENU_SELECTED, ID_SHOWTOOLTIP))
            end
          end
        -- set line position in the (current) editor if requested
        elseif text and text:find(special.LINE..'(%d*)%s*$') then
          local toline = tonumber(text:match(special.LINE..'(%d+)'))
          if toline and ed then
            ed:GotoLine(toline-1)
            ed:EnsureVisibleEnforcePolicy(toline-1)
            ed:SetFocus() -- in case the focus is on some other panel
          end
        elseif tabindex then -- switch to existing tab
          SetEditorSelection(tabindex)
          if pindex and pindex ~= tabindex then ClosePage(pindex) end
        -- load a new file (into preview if set)
        elseif sline or text then
          -- 1. use "text" if Ctrl/Cmd-Enter is used
          -- 2. otherwise use currently selected file
          -- 3. otherwise use "text"
          local file = (wx.wxGetKeyState(wx.WXK_CONTROL) and text) or sline or text
          local fullPath = MergeFullPath(ide:GetProject(), file)
          local doc = ide:FindDocument(fullPath)
          -- if the document is already opened (not in the preview)
          -- or can't be opened as a file or folder, then close the preview
          if doc and doc.index ~= pindex
          or not LoadFile(fullPath, preview or nil) and not ide:SetProject(fullPath) then
            if pindex then ClosePage(pindex) end
          end
        end
      elseif enter == nil then -- changed focus
        -- do nothing; keep everything as is
      else
        -- close preview
        if pindex then ClosePage(pindex) end
        -- restore original selection if canceled
        if nb:GetSelection() ~= selection then nb:SetSelection(selection) end
      end
      preview = nil
      if not mac then nb:Thaw() end
    end,
    onUpdate = function(text)
      local lines = {}
      local projdir = ide:GetProject()

      -- delete all current line markers if any
      -- restore the original position if search text is updated
      local ed = ide:GetEditor()
      if ed and origline then ed:MarkerDeleteAll(marker) end

      -- reset cached functions if no symbol search
      if text and not text:find(special.SYMBOL) then
        functions = nil
        if ed and origline then ed:EnsureVisibleEnforcePolicy(origline-1) end
      end
      -- reset cached methods if no method search
      if text and not text:find(special.METHOD) then methods = nil end

      if text and text:find(special.SYMBOL) then
        local file, symbol = text:match('^(.*)'..special.SYMBOL..'(.*)')
        if not functions then
          local nums, paths = {}, {}
          functions = {pos = {}, src = {}}

          local function populateSymbols(path, symbols)
            for _, func in ipairs(symbols) do
              table.insert(functions, func.name)
              nums[func.name] = (nums[func.name] or 0) + 1
              local num = nums[func.name]
              functions.src[func.name..num] = path
              functions.pos[func.name..num] = func.pos
            end
          end

          local currentonly = #file > 0 and ed
          local outline = ide:GetOutline()
          for _, doc in pairs(currentonly and {ide:GetDocument(ed)} or ide:GetDocuments()) do
            local path, editor = doc:GetFilePath(), doc:GetEditor()
            if path then paths[path] = true end
            populateSymbols(path or doc:GetFileName()..tabsep..doc:GetTabIndex(), outline:GetEditorSymbols(editor))
          end

          -- now add all other files in the project
          if not currentonly and ide.config.commandbar.showallsymbols then
            local n = 0
            outline:RefreshSymbols(projdir, function(path)
                local symbols = outline:GetFileSymbols(path)
                if not paths[path] and symbols then populateSymbols(path, symbols) end
                if not symbols then n = n + 1 end
              end)
            if n > 0 then ide:SetStatusFor(TR("Queued %d files to index."):format(n)) end
          end
        end
        local nums = {}
        if #symbol > 0 then
          local topscore
          for _, item in ipairs(commandBarScoreItems(functions, symbol, maxitems)) do
            local func, score = unpack(item)
            topscore = topscore or score
            nums[func] = (nums[func] or 0) + 1
            local num = nums[func]
            if score > topscore / 4 and score > 1 then
              table.insert(lines, {("%2d %s"):format(score, func),
                  functions.src[func..num], functions.pos[func..num]})
            end
          end
        else
          for n, name in ipairs(functions) do
            if n > maxitems then break end
            nums[name] = (nums[name] or 0) + 1
            local num = nums[name]
            lines[n] = {name, functions.src[name..num], functions.pos[name..num]}
          end
        end
      elseif ed and text and text:find('^%s*'..special.METHOD) then
        if not methods then
          methods = {desc = {}}
          local num = 1
          if ed.api and ed.api.tip and ed.api.tip.shortfinfoclass then
            for libname, lib in pairs(ed.api.tip.shortfinfoclass) do
              for method, val in pairs(lib) do
                local signature, desc = val:match('(.-)\n(.*)')
                local m = libname..'.'..method
                desc = desc and desc:gsub("\n", " ") or val
                methods[num] = m
                methods.desc[m] = {signature or (libname..'.'..method), desc}
                num = num + 1
              end
            end
          end
        end
        local method = text:match(special.METHOD..'(.*)')
        if #method > 0 then
          local topscore
          for _, item in ipairs(commandBarScoreItems(methods, method, maxitems)) do
            local method, score = unpack(item)
            topscore = topscore or score
            if score > topscore / 4 and score > 1 then
              table.insert(lines, { score, method })
            end
          end
        end
      elseif text and text:find(special.LINE..'(%d*)%s*$') then
        local toline = tonumber(text:match(special.LINE..'(%d+)'))
        if toline and ed then markLine(ed, toline) end
      elseif text and #text > 0 and projdir and #projdir > 0 then
        -- populate the list of files
        files = files or FileSysGetRecursive(projdir, true, "*",
          {sort = false, path = false, folder = false, skipbinary = true})
        local topscore
        for _, item in ipairs(commandBarScoreItems(files, text, maxitems)) do
          local file, score = unpack(item)
          topscore = topscore or score
          if score > topscore / 4 and score > 1 then
            table.insert(lines, {
                ("%2d %s"):format(score, wx.wxFileName(file):GetFullName()),
                file,
            })
          end
        end
      else
        for _, doc in pairs(ide:GetDocuments()) do
          lines[doc:GetTabIndex()+1] = {doc:GetFileName(), doc:GetFilePath(), doc:GetTabIndex()}
        end
      end
      return lines
    end,
    onItem = function(t)
      if methods then
        local score, method = unpack(t)
        return ("%2d %s"):format(score, methods.desc[method][1]), methods.desc[method][2]
      else
        return unpack(t)
      end
    end,
    onSelection = function(t, text)
      local _, file, tabindex = unpack(t)
      local pos
      if text and text:find(special.SYMBOL) then
        pos, tabindex = tabindex, name2index(file)
      elseif text and text:find(special.METHOD) then
        return
      end

      if file then file = MergeFullPath(ide:GetProject(), file) end
      -- disabling event handlers for the notebook and the editor
      -- to minimize changes in the UI when editors are switched
      -- or files in the preview are updated.
      nb:SetEvtHandlerEnabled(false)
      local doc = file and ide:FindDocument(file)
      if doc and not tabindex then tabindex = doc:GetTabIndex() end
      if tabindex then
        local ed = nb:GetPage(tabindex)
        ed:SetEvtHandlerEnabled(false)
        if nb:GetSelection() ~= tabindex then nb:SetSelection(tabindex) end
        ed:SetEvtHandlerEnabled(true)
      elseif file then
        -- skip binary files with unknown extensions
        if #ide:GetKnownExtensions(GetFileExt(file)) > 0
        -- file may not be read if there is an error, so provide a default for that case
        or not IsBinary(FileRead(file, 2048) or "") then
          preview = preview or NewFile()
          preview:SetEvtHandlerEnabled(false)
          LoadFile(file, preview, true, true)
          preview:SetFocus()
          -- force refresh since the panel covers the editor on OSX/Linux
          -- this fixes the preview window not always redrawn on Linux
          if not win then preview:Update() preview:Refresh() end
          preview:SetEvtHandlerEnabled(true)
        elseif preview then
          ClosePage(nb:GetPageIndex(preview))
          preview = nil
        end
      end
      nb:SetEvtHandlerEnabled(true)

      if text and text:find(special.SYMBOL) then
        local ed = ide:GetEditor()
        if ed then markLine(ed, ed:LineFromPosition(pos-1)+1) end
      end
    end,
  })
end

ide.test.commandBarScoreItems = commandBarScoreItems

ide:AddPackage('core.commandbar', {
    -- reset ngram cache when switching projects to conserve memory
    onProjectLoad = function() cache = {} end
  })
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local frame = ide.frame
local notebook = frame.notebook
local openDocuments = ide.openDocuments
local uimgr = frame.uimgr
local unpack = table.unpack or unpack

local CURRENT_LINE_MARKER = StylesGetMarker("currentline")
local CURRENT_LINE_MARKER_VALUE = 2^CURRENT_LINE_MARKER

function NewFile(filename)
  filename = filename or ide:GetDefaultFileName()
  local editor = CreateEditor()
  editor:SetupKeywords(GetFileExt(filename))
  local doc = AddEditor(editor, filename)
  if doc then
    SetEditorSelection(doc.index)
    PackageEventHandle("onEditorNew", editor)
  end
  return editor
end

-- Find an editor page that hasn't been used at all, eg. an untouched NewFile()
local function findUnusedEditor()
  local editor
  for _, document in pairs(openDocuments) do
    if (document.editor:GetLength() == 0) and
    (not document.isModified) and (not document.filePath) and
    not (document.editor:GetReadOnly() == true) then
      editor = document.editor
      break
    end
  end
  return editor
end

function LoadFile(filePath, editor, file_must_exist, skipselection)
  filePath = filePath:gsub("%s+$","")

  -- if the file name is empty or is a directory, don't do anything
  if filePath == '' or wx.wxDirExists(filePath) then return nil end

  filePath = FileNormalizePath(filePath)
  -- on some Windows versions, normalization doesn't return "original" file name,
  -- so detect that and use LongPath instead
  if ide.osname == "Windows" and wx.wxFileExists(filePath)
  and FileNormalizePath(filePath:upper()) ~= FileNormalizePath(filePath:lower()) then
    filePath = FileGetLongPath(filePath)
  end

  -- prevent files from being reopened again
  if (not editor) then
    local doc = ide:FindDocument(filePath)
    if doc then
      if not skipselection and doc.index ~= notebook:GetSelection() then
        -- selecting the same tab doesn't trigger PAGE_CHANGE event,
        -- but moves the focus to the tab bar, which needs to be avoided.
        notebook:SetSelection(doc.index)
      end
      return doc.editor
    end
  end

  local filesize = FileSize(filePath)
  if not filesize and file_must_exist then return nil end

  local current = editor and editor:GetCurrentPos()
  editor = editor or findUnusedEditor() or CreateEditor()
  editor:Freeze()
  editor:SetupKeywords(GetFileExt(filePath))
  editor:MarkerDeleteAll(-1)
  if filesize then editor:Allocate(filesize) end
  editor:SetReadOnly(false) -- disable read-only status if set on the editor
  editor:SetTextDyn("")
  editor.bom = string.char(0xEF,0xBB,0xBF)

  local inputfilter = GetConfigIOFilter("input")
  local file_text
  ide:PushStatus("")
  FileRead(filePath, 1024*1024, function(s) -- callback is only called when the file exists
      if not file_text then
        -- remove BOM from UTF-8 encoded files; store BOM to add back when saving
        if s and editor:GetCodePage() == wxstc.wxSTC_CP_UTF8 and s:find("^"..editor.bom) then
          s = s:gsub("^"..editor.bom, "")
        else
          -- set to 'false' as checks for nil on wxlua objects may fail at run-time
          editor.bom = false
        end
        file_text = s
      end
      if inputfilter then s = inputfilter(filePath, s) end
      local expected = editor:GetLength() + #s
      editor:AppendTextDyn(s)
      -- if the length is not as expected, then either it's a binary file or invalid UTF8
      if editor:GetLength() ~= expected then
        -- skip binary files with unknown extensions as they may have any sequences
        -- when using Raw methods, this can only happen for binary files (that include \0 chars)
        if editor.useraw or editor.spec == ide.specs.none and IsBinary(s) then
          ide:Print(("%s: %s"):format(filePath,
              TR("Binary file is shown as read-only as it is only partially loaded.")))
          file_text = ''
          editor:SetReadOnly(true)
          return false
        end

        -- handle invalid UTF8 characters
        -- fix: doesn't handle characters split by callback buffer
        local replacement, invalid = "\022"
        s, invalid = FixUTF8(s, replacement)
        if #invalid > 0 then
          editor:AppendTextDyn(s)
          local lastline = nil
          for _, n in ipairs(invalid) do
            local line = editor:LineFromPosition(n)
            if line ~= lastline then
              ide:Print(("%s:%d: %s"):format(filePath, line+1,
                  TR("Replaced an invalid UTF8 character with %s."):format(replacement)))
              lastline = line
            end
          end
        end
      end
      if filesize and filesize > 0 then
        ide:PopStatus()
        ide:PushStatus(TR("%s%% loaded..."):format(math.floor(100*editor:GetLength()/filesize)))
      end
    end)
  ide:PopStatus()

  -- empty or non-existing files don't have bom
  if not file_text then editor.bom = false end

  editor:Colourise(0, -1)
  editor:ResetTokenList() -- reset list of tokens if this is a reused editor
  editor:Thaw()

  local edcfg = ide.config.editor
  if current then editor:GotoPos(current) end
  if (file_text and edcfg.autotabs) then
    -- use tabs if they are already used
    -- or if "usetabs" is set and no space indentation is used in a file
    editor:SetUseTabs(string.find(file_text, "\t") ~= nil
      or edcfg.usetabs and (file_text:find("%f[^\r\n] ") or file_text:find("^ ")) == nil)
  end
  
  if (file_text and edcfg.checkeol) then
    -- Auto-detect CRLF/LF line-endings
    local foundcrlf = string.find(file_text,"\r\n") ~= nil
    local foundlf = (string.find(file_text,"[^\r]\n") ~= nil)
      or (string.find(file_text,"^\n") ~= nil) -- edge case: file beginning with LF and having no other LF
    if foundcrlf and foundlf then -- file with mixed line-endings
      ide:Print(("%s: %s")
        :format(filePath, TR("Mixed end-of-line encodings detected.")..' '..
          TR("Use '%s' to show line endings and '%s' to convert them.")
        :format("ide:GetEditor():SetViewEOL(1)", "ide:GetEditor():ConvertEOLs(ide:GetEditor():GetEOLMode())")))
    elseif foundcrlf then
      editor:SetEOLMode(wxstc.wxSTC_EOL_CRLF)
    elseif foundlf then
      editor:SetEOLMode(wxstc.wxSTC_EOL_LF)
    -- else (e.g. file is 1 line long or uses another line-ending): use default EOL mode
    end
  end

  editor:EmptyUndoBuffer()
  local doc = ide:GetDocument(editor)
  if doc then -- existing editor; switch to the tab
    notebook:SetSelection(doc:GetTabIndex())
  else -- the editor has not been added to notebook
    doc = AddEditor(editor, wx.wxFileName(filePath):GetFullName() or ide:GetDefaultFileName())
  end
  doc.filePath = filePath
  doc.fileName = wx.wxFileName(filePath):GetFullName()
  doc.modTime = GetFileModTime(filePath)

  doc:SetModified(false)
  doc:SetTabText(doc:GetFileName())

  -- activate the editor; this is needed for those cases when the editor is
  -- created from some other element, for example, from a project tree.
  if not skipselection then SetEditorSelection() end

  PackageEventHandle("onEditorLoad", editor)

  return editor
end

function ReLoadFile(filePath, editor, ...)
  if not editor then return LoadFile(filePath, editor, ...) end

  -- save all markers
  local markers = editor:MarkerGetAll()
  -- add the current line content to retrieved markers to compare later if needed
  for _, marker in ipairs(markers) do marker[3] = editor:GetLineDyn(marker[1]) end
  local lines = editor:GetLineCount()

  -- load file into the same editor
  editor = LoadFile(filePath, editor, ...)
  if not editor then return end

  if #markers > 0 then -- restore all markers
    -- delete all markers as they may be restored by a different mechanism,
    -- which may be limited to only restoring some markers
    editor:MarkerDeleteAll(-1)
    local samelinecount = lines == editor:GetLineCount()
    for _, marker in ipairs(markers) do
      local line, mask, text = unpack(marker)
      if samelinecount then
        -- restore marker at the same line number
        editor:MarkerAddSet(line, mask)
      else
        -- find matching line in the surrounding area and restore marker there
        for _, l in ipairs({line, line-1, line-2, line+1, line+2}) do
          if text == editor:GetLineDyn(l) then
            editor:MarkerAddSet(l, mask)
            break
          end
        end
      end
    end
    PackageEventHandle("onEditorMarkerUpdate", editor)
  end

  return editor
end

local function getExtsString(ed)
  local exts = ed and ed.spec and ed.spec.exts or {}
  local knownexts = #exts > 0 and "*."..table.concat(exts, ";*.") or nil
  return (knownexts and TR("Known Files").." ("..knownexts..")|"..knownexts.."|" or "")
  .. TR("All files").." (*)|*"
end

function ReportError(msg)
  ide:RequestAttention() -- request attention first in case the app is minimized or in the background
  return wx.wxMessageBox(msg, TR("Error"), wx.wxICON_ERROR + wx.wxOK + wx.wxCENTRE, ide.frame)
end

function OpenFile(event)
  local editor = ide:GetEditor()
  local path = editor and ide:GetDocument(editor):GetFilePath() or nil
  local fileDialog = wx.wxFileDialog(ide.frame, TR("Open file"),
    (path and GetPathWithSep(path) or FileTreeGetDir() or ""),
    "",
    getExtsString(editor),
    wx.wxFD_OPEN + wx.wxFD_FILE_MUST_EXIST + wx.wxFD_MULTIPLE)
  if fileDialog:ShowModal() == wx.wxID_OK then
    for _, path in ipairs(fileDialog:GetPaths()) do
      if not LoadFile(path, nil, true) then
        ReportError(TR("Unable to load file '%s'."):format(path))
      end
    end
  end
  fileDialog:Destroy()
end

-- save the file to filePath or if filePath is nil then call SaveFileAs
function SaveFile(editor, filePath)
  -- this event can be aborted
  -- as SaveFileAs calls SaveFile, this event may be called two times:
  -- first without filePath and then with filePath
  if PackageEventHandle("onEditorPreSave", editor, filePath) == false then
    return false
  end

  if not filePath then
    return SaveFileAs(editor)
  else
    if ide.config.savebak then
      local ok, err = FileRename(filePath, filePath..".bak")
      if not ok then
        ReportError(TR("Unable to save file '%s': %s"):format(filePath..".bak", err))
        return
      end
    end

    local st = ((editor:GetCodePage() == wxstc.wxSTC_CP_UTF8 and editor.bom or "")
      .. editor:GetTextDyn())
    if GetConfigIOFilter("output") then
      st = GetConfigIOFilter("output")(filePath,st)
    end

    local ok, err = FileWrite(filePath, st)
    if ok then
      editor:SetSavePoint()
      local doc = ide:GetDocument(editor)
      doc.filePath = filePath
      doc.fileName = wx.wxFileName(filePath):GetFullName()
      doc.modTime = GetFileModTime(filePath)
      doc:SetModified(false)
      doc:SetTabText(doc:GetFileName())
      SetAutoRecoveryMark()
      FileTreeMarkSelected(filePath)

      PackageEventHandle("onEditorSave", editor)

      return true
    else
      ReportError(TR("Unable to save file '%s': %s"):format(filePath, err))
    end
  end

  return false
end

function ApproveFileOverwrite()
  return wx.wxMessageBox(
    TR("File already exists.").."\n"..TR("Do you want to overwrite it?"),
    ide:GetProperty("editormessage"),
    wx.wxYES_NO + wx.wxCENTRE, ide.frame) == wx.wxYES
end

function SaveFileAs(editor)
  local id = editor:GetId()
  local saved = false
  local filePath = (openDocuments[id].filePath
    or ((FileTreeGetDir() or "")
        ..(openDocuments[id].fileName or ide.config.default.name)))

  local fn = wx.wxFileName(filePath)
  fn:Normalize() -- want absolute path for dialog

  local ext = fn:GetExt()
  if (not ext or #ext == 0) and editor.spec and editor.spec.exts then
    ext = editor.spec.exts[1]
    -- set the extension on the file if assigned as this is used by OSX/Linux
    -- to present the correct default "save as type" choice.
    if ext then fn:SetExt(ext) end
  end
  local fileDialog = wx.wxFileDialog(ide.frame, TR("Save file as"),
    fn:GetPath(wx.wxPATH_GET_VOLUME),
    fn:GetFullName(),
    -- specify the current extension plus all other extensions based on specs
    (ext and #ext > 0 and "*."..ext.."|*."..ext.."|" or "")..getExtsString(editor),
    wx.wxFD_SAVE)

  if fileDialog:ShowModal() == wx.wxID_OK then
    local filePath = fileDialog:GetPath()

    -- check if there is another tab with the same name and prepare to close it
    local existing = (ide:FindDocument(filePath) or {}).index
    local cansave = fn:GetFullName() == filePath -- saving into the same file
       or not wx.wxFileName(filePath):FileExists() -- or a new file
       or ApproveFileOverwrite()

    if cansave and SaveFile(editor, filePath) then
      SetEditorSelection() -- update title of the editor
      -- new extension, this will reset keywords and indicators
      if ext ~= GetFileExt(filePath) then LoadFile(filePath, editor) end
      saved = true

      if existing then
        -- save the current selection as it may change after closing
        local current = notebook:GetSelection()
        ClosePage(existing)
        -- restore the selection if it changed
        if current ~= notebook:GetSelection() then
          notebook:SetSelection(current)
        end
      end
    end
  end

  fileDialog:Destroy()
  return saved
end

function SaveAll(quiet)
  for _, document in pairs(openDocuments) do
    local editor = document.editor
    local filePath = document.filePath

    if (document.isModified or not document.filePath) -- need to save
    and (document.filePath or not quiet) then -- have path or can ask user
      SaveFile(editor, filePath) -- will call SaveFileAs if necessary
    end
  end
end

local function removePage(index)
  local prevIndex = nil
  local nextIndex = nil
  
  -- try to preserve old selection
  local selectIndex = notebook:GetSelection()
  selectIndex = selectIndex ~= index and selectIndex

  local delid = nil
  for id, document in pairsSorted(openDocuments,
    function(a, b) -- sort by document index
      return openDocuments[a].index < openDocuments[b].index
    end) do
    local wasselected = document.index == selectIndex
    if document.index < index then
      prevIndex = document.index
    elseif document.index == index then
      delid = id
      document.editor:Destroy()
    elseif document.index > index then
      document.index = document.index - 1
      if nextIndex == nil then
        nextIndex = document.index
      end
    end
    if (wasselected) then
      selectIndex = document.index
    end
  end

  if (delid) then
    openDocuments[delid] = nil
  end

  notebook:RemovePage(index)
  
  if selectIndex then
    notebook:SetSelection(selectIndex)
  elseif nextIndex then
    notebook:SetSelection(nextIndex)
  elseif prevIndex then
    notebook:SetSelection(prevIndex)
  end

  -- need to set editor selection as it's called *after* PAGE_CHANGED event
  SetEditorSelection()
end

function ClosePage(selection)
  local editor = ide:GetEditor(selection)
  local id = editor:GetId()

  if PackageEventHandle("onEditorPreClose", editor) == false then
    return false
  end

  if SaveModifiedDialog(editor, true) ~= wx.wxID_CANCEL then
    DynamicWordsRemoveAll(editor)
    local debugger = ide:GetDebugger()
    -- check if the window with the scratchpad running is being closed
    if debugger and debugger.scratchpad and debugger.scratchpad.editors
    and debugger.scratchpad.editors[editor] then
      debugger:ScratchpadOff()
    end
    -- check if the debugger is running and is using the current window;
    -- abort the debugger if the current marker is in the window being closed
    if debugger and debugger:IsConnected() and
      (editor:MarkerNext(0, CURRENT_LINE_MARKER_VALUE) >= 0) then
      debugger:Stop()
    end
    PackageEventHandle("onEditorClose", editor)
    removePage(ide.openDocuments[id].index)

    -- disable full screen if the last tab is closed
    if not (notebook:GetSelection() >= 0) then ShowFullScreen(false) end
    return true
  end
  return false
end

function CloseAllPagesExcept(selection)
  local toclose = {}
  for _, document in pairs(ide.openDocuments) do
    table.insert(toclose, document.index)
  end

  table.sort(toclose)

  -- close pages for those files that match the project in the reverse order
  -- (as ids shift when pages are closed)
  for i = #toclose, 1, -1 do
    if toclose[i] ~= selection then ClosePage(toclose[i]) end
  end
end

-- Show a dialog to save a file before closing editor.
-- returns wxID_YES, wxID_NO, or wxID_CANCEL if allow_cancel
function SaveModifiedDialog(editor, allow_cancel)
  local result = wx.wxID_NO
  local id = editor:GetId()
  local document = openDocuments[id]
  local filePath = document.filePath
  local fileName = document.fileName
  if document.isModified then
    local message = TR("Do you want to save the changes to '%s'?")
      :format(fileName or ide.config.default.name)
    local dlg_styles = wx.wxYES_NO + wx.wxCENTRE + wx.wxICON_QUESTION
    if allow_cancel then dlg_styles = dlg_styles + wx.wxCANCEL end
    local dialog = wx.wxMessageDialog(ide.frame, message,
      TR("Save Changes?"),
      dlg_styles)
    result = dialog:ShowModal()
    dialog:Destroy()
    if result == wx.wxID_YES then
      if not SaveFile(editor, filePath) then
        return wx.wxID_CANCEL -- cancel if canceled save dialog
      end
    end
  end

  return result
end

function SaveOnExit(allow_cancel)
  for _, document in pairs(openDocuments) do
    if (SaveModifiedDialog(document.editor, allow_cancel) == wx.wxID_CANCEL) then
      return false
    end
  end

  -- if all documents have been saved or refused to save, then mark those that
  -- are still modified as not modified (they don't need to be saved)
  -- to keep their tab names correct
  for _, document in pairs(openDocuments) do
    if document.isModified then document:SetModified(false) end
  end

  return true
end

function SetAllEditorsReadOnly(enable)
  for _, document in pairs(openDocuments) do
    document.editor:SetReadOnly(enable)
  end
end

-----------------
-- Debug related

function ClearAllCurrentLineMarkers()
  for _, document in pairs(openDocuments) do
    document.editor:MarkerDeleteAll(CURRENT_LINE_MARKER)
    document.editor:Refresh() -- needed for background markers that don't get refreshed (wx2.9.5)
  end
end

-- remove shebang line (#!) as it throws a compilation error as
-- loadstring() doesn't allow it even though lua/loadfile accepts it.
-- replace with a new line to keep the number of lines the same.
function StripShebang(code) return (code:gsub("^#!.-\n", "\n")) end

local compileOk, compileTotal = 0, 0
function CompileProgram(editor, params)
  local params = {
    jumponerror = (params or {}).jumponerror ~= false,
    reportstats = (params or {}).reportstats ~= false,
    keepoutput = (params or {}).keepoutput,
  }
  local doc = ide:GetDocument(editor)
  local filePath = doc:GetFilePath() or doc:GetFileName()
  local func, err = loadstring(StripShebang(editor:GetTextDyn()), '@'..filePath)
  local line = not func and tonumber(err:match(":(%d+)%s*:")) or nil

  if not params.keepoutput then ClearOutput() end

  compileTotal = compileTotal + 1
  if func then
    compileOk = compileOk + 1
    if params.reportstats then
      ide:Print(TR("Compilation successful; %.0f%% success rate (%d/%d).")
        :format(compileOk/compileTotal*100, compileOk, compileTotal))
    end
  else
    ide:GetOutput():Activate()
    ide:Print(TR("Compilation error").." "..TR("on line %d"):format(line)..":")
    ide:Print((err:gsub("\n$", "")))
    -- check for escapes invalid in LuaJIT/Lua 5.2 that are allowed in Lua 5.1
    if err:find('invalid escape sequence') then
      local s = editor:GetLineDyn(line-1)
      local cleaned = s
        :gsub('\\[abfnrtv\\"\']', '  ')
        :gsub('(\\x[0-9a-fA-F][0-9a-fA-F])', function(s) return string.rep(' ', #s) end)
        :gsub('(\\%d%d?%d?)', function(s) return string.rep(' ', #s) end)
        :gsub('(\\z%s*)', function(s) return string.rep(' ', #s) end)
      local invalid = cleaned:find("\\")
      if invalid then
        ide:Print(TR("Consider removing backslash from escape sequence '%s'.")
          :format(s:sub(invalid,invalid+1)))
      end
    end
    if line and params.jumponerror and line-1 ~= editor:GetCurrentLine() then
      editor:GotoLine(line-1)
    end
  end

  return func ~= nil -- return true if it compiled ok
end

------------------
-- Save & Close

function SaveIfModified(editor)
  local id = editor:GetId()
  if openDocuments[id].isModified then
    local saved = false
    if not openDocuments[id].filePath then
      local ret = wx.wxMessageBox(
        TR("You must save the program first.").."\n"..TR("Press cancel to abort."),
        TR("Save file?"), wx.wxOK + wx.wxCANCEL + wx.wxCENTRE, ide.frame)
      if ret == wx.wxOK then
        saved = SaveFileAs(editor)
      end
    else
      saved = SaveFile(editor, openDocuments[id].filePath)
    end

    if saved then
      openDocuments[id].isModified = false
    else
      return false -- not saved
    end
  end

  return true -- saved
end

function GetOpenFiles()
  local opendocs = {}
  for _, document in pairs(ide.openDocuments) do
    if (document.filePath) then
      local wxfname = wx.wxFileName(document.filePath)
      wxfname:Normalize()

      table.insert(opendocs, {filename=wxfname:GetFullPath(),
        id=document.index, cursorpos = document.editor:GetCurrentPos()})
    end
  end

  -- to keep tab order
  table.sort(opendocs,function(a,b) return (a.id < b.id) end)

  local id = ide:GetEditor()
  id = id and id:GetId()
  return opendocs, {index = (id and openDocuments[id].index or 0)}
end

function SetOpenFiles(nametab,params)
  for _, doc in ipairs(nametab) do
    local editor = LoadFile(doc.filename,nil,true,true) -- skip selection
    if editor then editor:GotoPosDelayed(doc.cursorpos or 0) end
  end
  notebook:SetSelection(params and params.index or 0)
  SetEditorSelection()
end

local beforeFullScreenPerspective
local statusbarShown

function ShowFullScreen(setFullScreen)
  if setFullScreen then
    beforeFullScreenPerspective = uimgr:SavePerspective()

    local panes = frame.uimgr:GetAllPanes()
    for index = 0, panes:GetCount()-1 do
      local name = panes:Item(index).name
      if name ~= "notebook" then frame.uimgr:GetPane(name):Hide() end
    end
    uimgr:Update()
    SetEditorSelection() -- make sure the focus is on the editor
  end

  -- On OSX, status bar is not hidden when switched to
  -- full screen: http://trac.wxwidgets.org/ticket/14259; do manually.
  -- need to turn off before showing full screen and turn on after,
  -- otherwise the window is restored incorrectly and is reduced in size.
  if ide.osname == 'Macintosh' and setFullScreen then
    statusbarShown = frame:GetStatusBar():IsShown()
    frame:GetStatusBar():Hide()
  end

  -- protect from systems that don't have ShowFullScreen (GTK on linux?)
  pcall(function() frame:ShowFullScreen(setFullScreen) end)

  if not setFullScreen and beforeFullScreenPerspective then
    uimgr:LoadPerspective(beforeFullScreenPerspective, true)
    beforeFullScreenPerspective = nil
  end

  if ide.osname == 'Macintosh' and not setFullScreen then
    if statusbarShown then
      frame:GetStatusBar():Show()
      -- refresh AuiManager as the statusbar may be shown below the border
      uimgr:Update()
    end
  end
end

function ProjectConfig(dir, config)
  if config then ide.session.projects[dir] = config
  else return unpack(ide.session.projects[dir] or {}) end
end

function SetOpenTabs(params)
  local recovery, nametab = LoadSafe("return "..params.recovery)
  if not recovery or not nametab then
    ide:Print(TR("Can't process auto-recovery record; invalid format: %s."):format(nametab or "unknown"))
    return
  end
  if not params.quiet then
    ide:Print(TR("Found auto-recovery record and restored saved session."))
  end
  for _,doc in ipairs(nametab) do
    -- check for missing file if no content is stored
    if doc.filepath and not doc.content and not wx.wxFileExists(doc.filepath) then
      ide:Print(TR("File '%s' is missing and can't be recovered."):format(doc.filepath))
    else
      local editor = (doc.filepath and LoadFile(doc.filepath,nil,true,true)
        or findUnusedEditor() or NewFile(doc.filename))
      local opendoc = ide:GetDocument(editor)
      if doc.content then
        editor:SetTextDyn(doc.content)
        if doc.filepath and opendoc.modTime and doc.modified < opendoc.modTime:GetTicks() then
          ide:Print(TR("File '%s' has more recent timestamp than restored '%s'; please review before saving.")
            :format(doc.filepath, opendoc:GetTabText()))
        end
        opendoc:SetModified(true)
      end
      editor:GotoPosDelayed(doc.cursorpos or 0)
    end
  end
  notebook:SetSelection(params and params.index or 0)
  SetEditorSelection()
end

local function getOpenTabs()
  local opendocs = {}
  for _, document in pairs(ide.openDocuments) do
    local editor = document:GetEditor()
    table.insert(opendocs, {
      filename = document:GetFileName(),
      filepath = document:GetFilePath(),
      tabname = document:GetTabText(),
      modified = document:GetModTime() and document:GetModTime():GetTicks(), -- get number of seconds
      content = document:IsModified() and editor:GetTextDyn() or nil,
      id = document:GetTabIndex(),
      cursorpos = editor:GetCurrentPos()})
  end

  -- to keep tab order
  table.sort(opendocs, function(a,b) return (a.id < b.id) end)

  local ed = ide:GetEditor()
  local doc = ed and ide:GetDocument(ed)
  return opendocs, {index = (doc and doc:GetTabIndex() or 0)}
end

function SetAutoRecoveryMark()
  ide.session.lastupdated = os.time()
end

local function saveHotExit()
  local opentabs, params = getOpenTabs()
  if #opentabs > 0 then
    params.recovery = DumpPlain(opentabs)
    params.quiet = true
    SettingsSaveFileSession({}, params)
  end
end

local function saveAutoRecovery(force)
  if not ide.config.autorecoverinactivity then return end

  local lastupdated = ide.session.lastupdated
  if not force then
    if not lastupdated or lastupdated < (ide.session.lastsaved or 0) then return end
  end

  local now = os.time()
  if not force and lastupdated + ide.config.autorecoverinactivity > now then return end

  -- find all open modified files and save them
  local opentabs, params = getOpenTabs()
  if #opentabs > 0 then
    params.recovery = DumpPlain(opentabs)
    SettingsSaveAll()
    SettingsSaveFileSession({}, params)
    ide.settings:Flush()
  end
  ide.session.lastsaved = now
  ide:SetStatus(TR("Saved auto-recover at %s."):format(os.date("%H:%M:%S")))
end

local function fastWrap(func, ...)
  -- ignore SetEditorSelection that is not needed as `func` may work on
  -- multipe files, but editor needs to be selected once.
  local SES = SetEditorSelection
  SetEditorSelection = function() end
  func(...)
  SetEditorSelection = SES
end

function StoreRestoreProjectTabs(curdir, newdir, intfname)
  local win = ide.osname == 'Windows'
  local interpreter = intfname or ide.interpreter.fname
  local current, closing, restore = notebook:GetSelection(), 0, false

  if ide.osname ~= 'Macintosh' then notebook:Freeze() end

  if curdir and #curdir > 0 then
    local lowcurdir = win and string.lower(curdir) or curdir
    local lownewdir = win and string.lower(newdir) or newdir
    local projdocs, closdocs = {}, {}
    for _, document in ipairs(GetOpenFiles()) do
      local dpath = win and string.lower(document.filename) or document.filename
      -- check if the filename is in the same folder
      if dpath:find(lowcurdir, 1, true) == 1
      and dpath:find("^[\\/]", #lowcurdir+1) then
        table.insert(projdocs, document)
        closing = closing + (document.id < current and 1 or 0)
        -- only close if the file is not in new project as it would be reopened
        if not dpath:find(lownewdir, 1, true)
        or not dpath:find("^[\\/]", #lownewdir+1) then
          table.insert(closdocs, document)
        end
      elseif document.id == current then restore = true end
    end

    -- adjust for the number of closing tabs on the left from the current one
    current = current - closing

    -- save opened files from this project
    ProjectConfig(curdir, {projdocs,
      {index = notebook:GetSelection() - current, interpreter = interpreter}})

    -- close pages for those files that match the project in the reverse order
    -- (as ids shift when pages are closed)
    for i = #closdocs, 1, -1 do fastWrap(ClosePage, closdocs[i].id) end
  end

  local files, params = ProjectConfig(newdir)
  if files then
    -- provide fake index so that it doesn't activate it as the index may be not
    -- quite correct if some of the existing files are already open in the IDE.
    fastWrap(SetOpenFiles, files, {index = #files + notebook:GetPageCount()})
  end

  -- either interpreter is chosen for the project or the default value is set
  if (params and params.interpreter) or (not params and ide.config.interpreter) then
    ProjectSetInterpreter(params and params.interpreter or ide.config.interpreter)
  end

  if ide.osname ~= 'Macintosh' then notebook:Thaw() end

  local index = params and params.index
  if notebook:GetPageCount() == 0 then NewFile()
  elseif restore and current >= 0 then notebook:SetSelection(current)
  elseif index and index >= 0 and files[index+1] then
    -- move the editor tab to the front with the file from the config
    LoadFile(files[index+1].filename, nil, true)
    SetEditorSelection() -- activate the editor in the active tab
  end

  -- remove current config as it may change; the current configuration is
  -- stored with the general config.
  -- The project configuration will be updated when the project is changed.
  ProjectConfig(newdir, {})
end

local function closeWindow(event)
  -- if the app is already exiting, then help it exit; wxwidgets on Windows
  -- is supposed to report Shutdown/logoff events by setting CanVeto() to
  -- false, but it doesn't happen. We simply leverage the fact that
  -- CloseWindow is called several times in this case and exit. Similar
  -- behavior has been also seen on Linux, so this logic applies everywhere.
  if ide.exitingProgram then os.exit() end

  ide.exitingProgram = true -- don't handle focus events

  if not ide.config.hotexit and not SaveOnExit(event:CanVeto()) then
    event:Veto()
    ide.exitingProgram = false
    return
  end

  ShowFullScreen(false)

  if ide:GetProject() then PackageEventHandle("onProjectClose", ide:GetProject()) end
  PackageEventHandle("onAppClose")

  -- first need to detach all processes IDE has launched as the current
  -- process is likely to terminate before child processes are terminated,
  -- which may lead to a crash when EVT_END_PROCESS event is called.
  DetachChildProcess()
  ide:GetDebugger():Shutdown()

  SettingsSaveAll()
  if ide.config.hotexit then saveHotExit() end
  ide.settings:Flush()

  do -- hide all floating panes first
    local panes = frame.uimgr:GetAllPanes()
    for index = 0, panes:GetCount()-1 do
      local pane = frame.uimgr:GetPane(panes:Item(index).name)
      if pane:IsFloating() then pane:Hide() end
    end
  end
  frame.uimgr:Update() -- hide floating panes
  frame.uimgr:UnInit()
  frame:Hide() -- hide the main frame while the IDE exits

  -- stop all the timers
  for _, timer in pairs(ide.timers) do timer:Stop() end
  wx.wxGetApp():Disconnect(wx.wxEVT_TIMER)

  event:Skip()

  PackageEventHandle("onAppShutdown")
end
frame:Connect(wx.wxEVT_CLOSE_WINDOW, closeWindow)

local function restoreFocus()
  -- check if the window is shown before returning focus to it,
  -- as it may lead to a recursion in event handlers on OSX (wxwidgets 2.9.5).
  if ide:IsWindowShown(ide.infocus) then
    ide.infocus:SetFocus()
    -- if switching to the editor, then also call SetSTCFocus,
    -- otherwise the cursor is not shown in the editor on OSX.
    if ide.infocus:GetClassInfo():GetClassName() == "wxStyledTextCtrl" then
      ide.infocus:DynamicCast("wxStyledTextCtrl"):SetSTCFocus(true)
    end
  end
end

-- in the presence of wxAuiToolbar, when (1) the app gets focus,
-- (2) a floating panel is closed or (3) a toolbar dropdown is closed,
-- the focus is always on the toolbar when the app gets focus,
-- so to restore the focus correctly, need to track where the control is
-- and to set the focus to the last element that had focus.
-- it would be easier to track KILL_FOCUS events, but controls on OSX
-- don't always generate KILL_FOCUS events (see relevant wxwidgets
-- tickets: http://trac.wxwidgets.org/ticket/14142
-- and http://trac.wxwidgets.org/ticket/14269)

ide.editorApp:Connect(wx.wxEVT_SET_FOCUS, function(event)
  if ide.exitingProgram then return end

  local win = ide.frame:FindFocus()
  if win then
    local class = win:GetClassInfo():GetClassName()
    -- don't set focus on the main frame or toolbar
    if ide.infocus and (class == "wxAuiToolBar" or class == "wxFrame") then
      pcall(restoreFocus)
      return
    end

    -- keep track of the current control in focus, but only on the main frame
    -- don't try to "remember" any of the focus changes on various dialog
    -- windows as those will disappear along with their controls
    local grandparent = win:GetGrandParent()
    local frameid = ide.frame:GetId()
    local mainwin = grandparent and grandparent:GetId() == frameid
    local parent = win:GetParent()
    while parent do
      local class = parent:GetClassInfo():GetClassName()
      if (class == "wxFrame" or class:find("^wx.*Dialog$"))
      and parent:GetId() ~= frameid then
        mainwin = false; break
      end
      parent = parent:GetParent()
    end
    if mainwin then
      if ide.osname == "Macintosh"
      and ide:IsValidCtrl(ide.infocus) and ide.infocus:DynamicCast("wxWindow") ~= win then
        -- kill focus on the control that had the focus as wxwidgets on OSX
        -- doesn't do it: http://trac.wxwidgets.org/ticket/14142;
        -- wrap into pcall in case the window is already deleted
        local ev = wx.wxFocusEvent(wx.wxEVT_KILL_FOCUS)
        pcall(function() ide.infocus:GetEventHandler():ProcessEvent(ev) end)
      end
      ide.infocus = win
    end
  end

  event:Skip()
end)

local updateInterval = 250 -- time in ms
wx.wxUpdateUIEvent.SetUpdateInterval(updateInterval)

ide.editorApp:Connect(wx.wxEVT_ACTIVATE_APP,
  function(event)
    if not ide.exitingProgram then
      local active = event:GetActive()
      -- restore focus to the last element that received it;
      -- wrap into pcall in case the element has disappeared
      -- while the application was out of focus
      if ide.osname == "Macintosh" and active and ide.infocus then pcall(restoreFocus) end

      -- save auto-recovery record when making the app inactive
      if not active then saveAutoRecovery(true) end

      -- disable UI refresh when app is inactive, but only when not running
      wx.wxUpdateUIEvent.SetUpdateInterval(
        (active or ide:GetLaunchedProcess()) and updateInterval or -1)

      PackageEventHandle(active and "onAppFocusSet" or "onAppFocusLost", ide.editorApp)
    end
    event:Skip()
  end)

if ide.config.autorecoverinactivity then
  ide.timers.session = ide:AddTimer(frame, function() saveAutoRecovery() end)
  -- check at least 5s to be never more than 5s off
  ide.timers.session:Start(math.min(5, ide.config.autorecoverinactivity)*1000)
end

function PaneFloatToggle(window)
  local pane = uimgr:GetPane(window)
  if pane:IsFloating() then
    pane:Dock()
  else
    pane:Float()
    pane:FloatingPosition(pane.window:GetScreenPosition())
    pane:FloatingSize(pane.window:GetSize())
  end
  uimgr:Update()
end

local cma, cman = 0, 1
frame:Connect(wx.wxEVT_IDLE,
  function(event)
    if ide:GetDebugger():Update() then event:RequestMore(true) end
    -- there is a chance that the current debugger can change after `Update` call
    -- (as the debugger may be suspended during initial socket connection),
    -- so retrieve the current debugger again to make sure it's properly set up.
    local debugger = ide:GetDebugger()
    if (debugger.scratchpad) then debugger:ScratchpadRefresh() end
    if IndicateIfNeeded() then event:RequestMore(true) end
    PackageEventHandleOnce("onIdleOnce", event)
    PackageEventHandle("onIdle", event)

    -- process onidle events if any
    if #ide.onidle > 0 then table.remove(ide.onidle, 1)() end
    if #ide.onidle > 0 then event:RequestMore(true) end -- request more if anything left

    if ide.config.showmemoryusage then
      local mem = collectgarbage("count")
      local alpha = math.max(tonumber(ide.config.showmemoryusage) or 0, 1/cman)
      cman = cman + 1
      cma = alpha * mem + (1-alpha) * cma
      ide:SetStatus(("cur: %sKb; avg: %sKb"):format(math.floor(mem), math.floor(cma)))
    end

    event:Skip() -- let other EVT_IDLE handlers to work on the event
  end)
editor = {
  editor = {
    autoactivate = false,
    autoreload = true,
    autotabs = false,
    backspaceunindent = true,
    calltipdelay = 500,
    caretline = true,
    checkeol = true,
    commentlinetoggle = false,
    edge = false,
    edgemode = wxstc.wxSTC_EDGE_NONE,
    fold = true,
    foldcompact = true,
    indentguide = true,
    linenumber = true,
    saveallonrun = false,
    showfncall = false,
    smartindent = true,
    -- extension-to-lexer mapping from TextAdept (modules/textadept/file_types.lua)
    specmap = {--[[Actionscript]]as='actionscript',asc='actionscript',--[[Ada]]adb='ada',ads='ada',--[[ANTLR]]g='antlr',g4='antlr',--[[APDL]]ans='apdl',inp='apdl',mac='apdl',--[[APL]]apl='apl',--[[Applescript]]applescript='applescript',--[[ASM]]asm='asm',ASM='asm',s='asm',S='asm',--[[ASP]]asa='asp',asp='asp',hta='asp',--[[AutoIt]]au3='autoit',a3x='autoit',--[[AWK]]awk='awk',--[[Batch]]bat='batch',cmd='batch',--[[BibTeX]]bib='bibtex',--[[Boo]]boo='boo',--[[C#]]cs='csharp',--[[C/C++]]c='ansi_c',cc='cpp',C='ansi_c',cpp='cpp',cxx='cpp',['c++']='cpp',h='cpp',hh='cpp',hpp='cpp',hxx='cpp',['h++']='cpp',--[[ChucK]]ck='chuck',--[[CMake]]cmake='cmake',['cmake.in']='cmake',ctest='cmake',['ctest.in']='cmake',--[[CoffeeScript]]coffee='coffeescript',--[[Crystal]]cr='crystal',--[[CSS]]css='css',--[[CUDA]]cu='cuda',cuh='cuda',--[[D]]d='dmd',di='dmd',--[[Dart]]dart='dart',--[[Desktop]]desktop='desktop',--[[diff]]diff='diff',patch='diff',--[[Dockerfile]]Dockerfile='dockerfile',--[[dot]]dot='dot',--[[Eiffel]]e='eiffel',eif='eiffel',--[[Elixir]]ex='elixir',exs='elixir',--[[Erlang]]erl='erlang',hrl='erlang',--[[F#]]fs='fsharp',--[[Faust]]dsp='faust',--[[Fish]]fish='fish',--[[Forth]]forth='forth',frt='forth',fs='forth',--[[Fortran]]f='fortran',['for']='fortran',ftn='fortran',fpp='fortran',f77='fortran',f90='fortran',f95='fortran',f03='fortran',f08='fortran',--[[Gap]]g='gap',gd='gap',gi='gap',gap='gap',--[[Gettext]]po='gettext',pot='gettext',--[[Gherkin]]feature='gherkin',--[[GLSL]]glslf='glsl',glslv='glsl',--[[GNUPlot]]dem='gnuplot',plt='gnuplot',--[[Go]]go='go',--[[Groovy]]groovy='groovy',gvy='groovy',--[[Gtkrc]]gtkrc='gtkrc',--[[Haskell]]hs='haskell',--[[HTML]]htm='html',html='html',shtm='html',shtml='html',xhtml='html',--[[Icon]]icn='icon',--[[IDL]]idl='idl',odl='idl',--[[Inform]]inf='inform',ni='inform',--[[ini]]cfg='ini',cnf='ini',inf='ini',ini='ini',reg='ini',--[[Io]]io='io_lang',--[[Java]]bsh='java',java='java',--[[Javascript]]js='javascript',jsfl='javascript',--[[JSON]]json='json',--[[JSP]]jsp='jsp',--[[LaTeX]]bbl='latex',dtx='latex',ins='latex',ltx='latex',tex='latex',sty='latex',--[[Ledger]]ledger='ledger',journal='ledger',--[[LESS]]less='less',--[[LilyPond]]lily='lilypond',ly='lilypond',--[[Lisp]]cl='lisp',el='lisp',lisp='lisp',lsp='lisp',--[[Literate Coffeescript]]litcoffee='litcoffee',--[[Lua]]lua='lua',--[[Makefile]]GNUmakefile='makefile',iface='makefile',mak='makefile',makefile='makefile',Makefile='makefile',--[[Man]]['1']='man',['2']='man',['3']='man',['4']='man',['5']='man',['6']='man',['7']='man',['8']='man',['9']='man',['1x']='man',['2x']='man',['3x']='man',['4x']='man',['5x']='man',['6x']='man',['7x']='man',['8x']='man',['9x']='man',--[[Markdown]]md='markdown',--[[MoonScript]]moon='moonscript',--[[Nemerle]]n='nemerle',--[[Nim]]nim='nim',--[[NSIS]]nsh='nsis',nsi='nsis',nsis='nsis',--[[Objective C]]m='objective_c',mm='objective_c',objc='objective_c',--[[OCaml]]caml='caml',ml='caml',mli='caml',mll='caml',mly='caml',--[[Pascal]]dpk='pascal',dpr='pascal',p='pascal',pas='pascal',--[[Perl]]al='perl',perl='perl',pl='perl',pm='perl',pod='perl',--[[PHP]]inc='php',php='php',php3='php',php4='php',phtml='php',--[[PICO-8]]p8='pico',--[[Pike]]pike='pike',pmod='pike',--[[PKGBUILD]]PKGBUILD='pkgbuild',--[[Postscript]]eps='ps',ps='ps',--[[PowerShell]]ps1='powershell',--[[Prolog]]prolog='prolog',--[[Properties]]props='props',properties='props',--[[Protobuf]]proto='protobuf',--[[Pure]]pure='pure',--[[Python]]sc='python',py='python',pyw='python',--[[R]]R='rstats',Rout='rstats',Rhistory='rstats',Rt='rstats',['Rout.save']='rstats',['Rout.fail']='rstats',S='rstats',--[[REBOL]]r='rebol',reb='rebol',--[[reST]]rst='rest',--[[Rexx]]orx='rexx',rex='rexx',--[[RHTML]]erb='rhtml',rhtml='rhtml',--[[Ruby]]Rakefile='ruby',rake='ruby',rb='ruby',rbw='ruby',--[[Rust]]rs='rust',--[[Sass CSS]]sass='sass',scss='sass',--[[Scala]]scala='scala',--[[Scheme]]sch='scheme',scm='scheme',--[[Shell]]bash='bash',bashrc='bash',bash_profile='bash',configure='bash',csh='bash',sh='bash',zsh='bash',--[[Smalltalk]]changes='smalltalk',st='smalltalk',sources='smalltalk',--[[SML]]sml='sml',fun='sml',sig='sml',--[[SNOBOL4]]sno='snobol4',SNO='snobol4',--[[SQL]]ddl='sql',sql='sql',--[[TaskPaper]]taskpaper='taskpaper',--[[Tcl]]tcl='tcl',tk='tcl',--[[Texinfo]]texi='texinfo',--[[TOML]]toml='toml',--[[Vala]]vala='vala',--[[vCard]]vcf='vcard',vcard='vcard',--[[Verilog]]v='verilog',ver='verilog',--[[VHDL]]vh='vhdl',vhd='vhdl',vhdl='vhdl',--[[Visual Basic]]asa='vb',bas='vb',cls='vb',ctl='vb',dob='vb',dsm='vb',dsr='vb',frm='vb',pag='vb',vb='vb',vba='vb',vbs='vb',--[[WSF]]wsf='wsf',--[[XML]]dtd='xml',svg='xml',xml='xml',xsd='xml',xsl='xml',xslt='xml',xul='xml',--[[Xtend]]xtend='xtend',--[[YAML]]yaml='yaml'},
    tabwidth = 2,
    usetabs  = false,
    usewrap = true,
    whitespacesize = 1,
    wrapmode = wxstc.wxSTC_WRAP_WORD,
  },
  debugger = {
    allowediting = false,
    hostname = nil,
    ignorecase = false,
    linetobreakpoint = false,
    maxdatalength = 256,
    maxdatalevel = 3,
    maxdatanum = 128,
    numformat = "%.16g",
    port = nil,
    redirect = nil,
    refuseonconflict = true,
    runonstart = nil,
    verbose = false,
  },
  default = {
    extension = 'lua',
    interpreter = 'luadeb',
    name = 'untitled',
    usecurrentextension = true,
  },
  outputshell = {
    usewrap = true,
  },
  filetree = {
    mousemove = true,
    showchanges = true,
    iconmap = {},
  },
  outline = {
    activateonclick = true,
    jumptocurrentfunction = true,
    showanonymous = '~',
    showcurrentfunction = true,
    showcompact = false,
    showflat = false,
    showmethodindicator = false,
    showonefile = false,
    sort = false,
  },
  commandbar = {
    prefilter = 250, -- number of records after which to apply filtering
    maxitems = 30, -- max number of items to show
    maxlines = 8, -- max number of lines to show
    width = 0.35, -- <1 -- size in proportion to the app frame width; >=1 -- size in pixels
    showallsymbols = true, -- show all symbols in a project
  },
  staticanalyzer = {
    infervalue = false, -- run more detailed static analysis; off by default as it's a slower mode
    luacheck = false, -- don't use luacheck by default; can be set to `true` to enable or a table
  },
  search = {
    autocomplete = true,
    contextlinesbefore = 2,
    contextlinesafter = 2,
    showaseditor = false,
    zoom = 0,
    autohide = false,
  },
  print = {
    magnification = -3,
    wrapmode = wxstc.wxSTC_WRAP_WORD,
    colourmode = wxstc.wxSTC_PRINT_BLACKONWHITE,
    header = "%S\t%D\t%p/%P",
    footer = nil,
  },
  toolbar = {
    icons = {},
    iconmap = {},
    iconsize = nil, -- icon size is set dynamically unless specified in the config
  },

  keymap = {},
  imagemap = {
    ['VALUE-MCALL'] = 'VALUE-SCALL',
  },
  language = "en",

  styles = nil,
  stylesoutshell = nil,

  autocomplete = true,
  autoanalyzer = true,
  acandtip = {
    startat = 2,
    shorttip = true,
    nodynwords = true,
    ignorecase = false,
    fillups = nil,
    symbols = true,
    droprest = true,
    strategy = 2,
    width = 60,
    maxlength = 450,
    warning = true,
  },
  arg = {}, -- command line arguments
  api = {}, -- additional APIs to load

  format = { -- various formatting strings
    menurecentprojects = "%f | %i",
    apptitle = "%T - %F",
  },

  activateoutput = true, -- activate output/console on Run/Debug/Compile
  unhidewindow = false, -- to unhide a gui window
  projectautoopen = true,
  autorecoverinactivity = 10, -- seconds
  outlineinactivity = 0.250, -- seconds
  markersinactivity = 0.500, -- seconds
  symbolindexinactivity = 2, -- seconds
  filehistorylength = 20,
  projecthistorylength = 20,
  commandlinehistorylength = 10,
  bordersize = 3,
  savebak = false,
  singleinstance = false,
  singleinstanceport = 8172,
  showmemoryusage = false,
  showhiddenfiles = false,
  hidpi = false, -- HiDPI/Retina display support
  hotexit = false,
  imagetint = nil,
  markertint = true,
  menuicon = true,
  -- file exclusion lists
  excludelist = {
    [".svn/"] = true,
    [".git/"] = true,
    [".hg/"] = true,
    ["CVS/"] = true,
    ["*.pyc"] = true,
    ["*.pyo"] = true,
    ["*.exe"] = true,
    ["*.dll"] = true,
    ["*.obj"] = true,
    ["*.o"] = true,
    ["*.a"] = true,
    ["*.lib"] = true,
    ["*.so"] = true,
    ["*.dylib"] = true,
    ["*.ncb"] = true,
    ["*.sdf"] = true,
    ["*.suo"] = true,
    ["*.pdb"] = true,
    ["*.idb"] = true,
    [".DS_Store"] = true,
    ["*.class"] = true,
    ["*.psd"] = true,
    ["*.db"] = true,
  },
  binarylist = {
    ["*.jpg"] = true,
    ["*.jpeg"] = true,
    ["*.png"] = true,
    ["*.gif"] = true,
    ["*.ttf"] = true,
    ["*.tga"] = true,
    ["*.dds"] = true,
    ["*.ico"] = true,
    ["*.eot"] = true,
    ["*.pdf"] = true,
    ["*.swf"] = true,
    ["*.jar"] = true,
    ["*.zip"] = true,
    ["*.gz"] = true,
    ["*.rar"] = true,
  },
}
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- Original authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
-- Integration with MobDebug
---------------------------------------------------------

local copas = require "copas"
local socket = require "socket"
local mobdebug = require "mobdebug"
local unpack = table.unpack or unpack

local ide = ide
local protodeb = setmetatable(ide:GetDebugger(), ide.proto.Debugger)
local debugger = protodeb
debugger.running = false -- true when the debuggee is running
debugger.listening = false -- true when the debugger is listening for a client
debugger.portnumber = ide.config.debugger.port or mobdebug.port -- the port # to use for debugging
debugger.watchCtrl = nil -- the watch ctrl that shows watch information
debugger.stackCtrl = nil -- the stack ctrl that shows stack information
debugger.toggleview = {
  bottomnotebook = true, -- output/console is "on" by default
  stackpanel = false, watchpanel = false, toolbar = false }
debugger.needrefresh = {} -- track components that may need a refresh
debugger.hostname = ide.config.debugger.hostname or (function()
  local hostname = socket.dns.gethostname()
  return hostname and socket.dns.toip(hostname) and hostname or "localhost"
end)()
debugger.imglist = ide:CreateImageList("STACK", "VALUE-CALL", "VALUE-LOCAL", "VALUE-UP")

local image = { STACK = 0, LOCAL = 1, UPVALUE = 2 }
local notebook = ide.frame.notebook

local CURRENT_LINE_MARKER = StylesGetMarker("currentline")
local CURRENT_LINE_MARKER_VALUE = 2^CURRENT_LINE_MARKER
local BREAKPOINT_MARKER = StylesGetMarker("breakpoint")
local BREAKPOINT_MARKER_VALUE = 2^BREAKPOINT_MARKER

local activate = {CHECKONLY = "checkonly", NOREPORT = "noreport", CLEARALL = "clearall"}

local function serialize(value, options) return mobdebug.line(value, options) end

local function displayError(...) return ide:GetOutput():Error(...) end

local function fixUTF8(...)
  local t = {...}
  -- convert to escaped decimal code as these can only appear in strings
  local function fix(s) return '\\'..string.byte(s) end
  for i = 1, #t do t[i] = FixUTF8(t[i], fix) end
  return unpack(t)
end

local q = EscapeMagic
local MORE = "{...}"

function debugger:init(init)
  local o = {}
  -- merge known self and init values
  for k, v in pairs(self) do o[k] = v end
  for k, v in pairs(init or {}) do o[k] = v end
  return setmetatable(o, {__index = protodeb})
end

function debugger:updateWatchesSync(onlyitem)
  local debugger = self
  local watchCtrl = debugger.watchCtrl
  local pane = ide.frame.uimgr:GetPane("watchpanel")
  local shown = watchCtrl and (pane:IsOk() and pane:IsShown() or not pane:IsOk() and watchCtrl:IsShown())
  local canupdate = (debugger.server and not debugger.running and not debugger.scratchpad
    and not (debugger.options or {}).noeval)
  if shown and canupdate then
    local bgcl = watchCtrl:GetBackgroundColour()
    local hicl = wx.wxColour(math.floor(bgcl:Red()*.9),
      math.floor(bgcl:Green()*.9), math.floor(bgcl:Blue()*.9))

    local root = watchCtrl:GetRootItem()
    if not root or not root:IsOk() then return end

    local params = debugger:GetDataOptions({maxlength=false})
    local item = onlyitem or watchCtrl:GetFirstChild(root)
    while true do
      if not item:IsOk() then break end

      local expression = watchCtrl:GetItemExpression(item)
      if expression then
        local _, values, error = debugger:evaluate(expression, params)
        local curchildren = watchCtrl:GetItemChildren(item)
        if error then
          error = error:gsub("%[.-%]:%d+:%s+","")
          watchCtrl:SetItemValueIfExpandable(item, nil)
        else
          if #values == 0 then values = {'nil'} end
          local _, res = LoadSafe("return "..values[1])
          watchCtrl:SetItemValueIfExpandable(item, res)
        end

        local newval = fixUTF8(expression .. ' = '
          .. (error and ('error: '..error) or table.concat(values, ", ")))
        local val = watchCtrl:GetItemText(item)

        watchCtrl:SetItemBackgroundColour(item, val ~= newval and hicl or bgcl)
        watchCtrl:SetItemText(item, newval)

        if onlyitem or val ~= newval then
          local newchildren = watchCtrl:GetItemChildren(item)
          if next(curchildren) ~= nil and next(newchildren) == nil then
            watchCtrl:SetItemHasChildren(item, true)
            watchCtrl:CollapseAndReset(item)
            watchCtrl:SetItemHasChildren(item, false)
          elseif next(curchildren) ~= nil and next(newchildren) ~= nil then
            watchCtrl:CollapseAndReset(item)
            watchCtrl:Expand(item)
          end
        end
      end

      if onlyitem then break end
      item = watchCtrl:GetNextSibling(item)
    end
    debugger.needrefresh.watches = false
  elseif not shown and canupdate then
    debugger.needrefresh.watches = true
  end
end

local callData = {}

function debugger:updateStackSync()
  local debugger = self
  local stackCtrl = debugger.stackCtrl
  local pane = ide.frame.uimgr:GetPane("stackpanel")
  local shown = stackCtrl and (pane:IsOk() and pane:IsShown() or not pane:IsOk() and stackCtrl:IsShown())
  local canupdate = debugger.server and not debugger.running and not debugger.scratchpad
  if shown and canupdate then
    local stack, _, err = debugger:stack(debugger:GetDataOptions({maxlength=false}))
    if not stack or #stack == 0 then
      stackCtrl:DeleteAll()
      if err then -- report an error if any
        stackCtrl:AppendItem(stackCtrl:AddRoot("Stack"), "Error: " .. err, image.STACK)
      end
      return
    end
    stackCtrl:Freeze()
    stackCtrl:DeleteAll()

    local forceexpand = ide.config.debugger.maxdatalevel == 1
    local params = debugger:GetDataOptions({maxlevel=false})

    local root = stackCtrl:AddRoot("Stack")
    callData = {} -- reset call cache
    for _,frame in ipairs(stack) do
      -- check if the stack includes expected structures
      if type(frame) ~= "table" or type(frame[1]) ~= "table" or #frame[1] < 7 then break end

      -- "main chunk at line 24"
      -- "foo() at line 13 (defined at foobar.lua:11)"
      -- call = { source.name, source.source, source.linedefined,
      --   source.currentline, source.what, source.namewhat, source.short_src }
      local call = frame[1]

      -- format the function name to a readable user string
      local func = call[5] == "main" and "main chunk"
        or call[5] == "C" and (call[1] or "C function")
        or call[5] == "tail" and "tail call"
        or (call[1] or "anonymous function")

      -- format the function treeitem text string, including the function name
      local text = func ..
        (call[4] == -1 and '' or " at line "..call[4]) ..
        (call[5] ~= "main" and call[5] ~= "Lua" and ''
         or (call[3] > 0 and " (defined at "..call[7]..":"..call[3]..")"
                          or " (defined in "..call[7]..")"))

      -- create the new tree item for this level of the call stack
      local callitem = stackCtrl:AppendItem(root, text, image.STACK)

      -- register call data to provide stack navigation
      callData[callitem:GetValue()] = { call[2], call[4] }

      -- add the local variables to the call stack item
      for name,val in pairs(type(frame[2]) == "table" and frame[2] or {}) do
        -- format the variable name, value as a single line and,
        -- if not a simple type, the string value.
        local value = val[1]
        local text = ("%s = %s"):format(name, fixUTF8(serialize(value, params)))
        local item = stackCtrl:AppendItem(callitem, text, image.LOCAL)
        stackCtrl:SetItemValueIfExpandable(item, value, forceexpand)
        stackCtrl:SetItemName(item, name)
      end

      -- add the upvalues for this call stack level to the tree item
      for name,val in pairs(type(frame[3]) == "table" and frame[3] or {}) do
        local value = val[1]
        local text = ("%s = %s"):format(name, fixUTF8(serialize(value, params)))
        local item = stackCtrl:AppendItem(callitem, text, image.UPVALUE)
        stackCtrl:SetItemValueIfExpandable(item, value, forceexpand)
        stackCtrl:SetItemName(item, name)
      end

      stackCtrl:SortChildren(callitem)
      stackCtrl:Expand(callitem)
    end
    stackCtrl:EnsureVisible(stackCtrl:GetFirstChild(root))
    stackCtrl:SetScrollPos(wx.wxHORIZONTAL, 0, true)
    stackCtrl:Thaw()
    debugger.needrefresh.stack = false
  elseif not shown and canupdate then
    debugger.needrefresh.stack = true
  end
end

function debugger:updateStackAndWatches()
  local debugger = self
  -- check if the debugger is running and may be waiting for a response.
  -- allow that request to finish, otherwise this function does nothing.
  if debugger.running then debugger:Update() end
  if debugger.server and not debugger.running then
    copas.addthread(function()
        local debugger = debugger
        debugger:updateStackSync()
        debugger:updateWatchesSync()
      end)
  end
end

function debugger:updateWatches(item)
  local debugger = self
  -- check if the debugger is running and may be waiting for a response.
  -- allow that request to finish, otherwise this function does nothing.
  if debugger.running then debugger:Update() end
  if debugger.server and not debugger.running then
    copas.addthread(function()
        local debugger = debugger
        debugger:updateWatchesSync(item)
      end)
  end
end

function debugger:updateStack()
  local debugger = self
  -- check if the debugger is running and may be waiting for a response.
  -- allow that request to finish, otherwise this function does nothing.
  if debugger.running then debugger:Update() end
  if debugger.server and not debugger.running then
    copas.addthread(function()
        local debugger = debugger
        debugger:updateStackSync()
      end)
  end
end

function debugger:toggleViews(show)
  local debugger = self
  -- don't toggle if the current state is the same as the new one
  local shown = debugger.toggleview.shown
  if (show and shown) or (not show and not shown) then return end

  debugger.toggleview.shown = nil

  local mgr = ide.frame.uimgr
  local refresh = false
  for view, needed in pairs(debugger.toggleview) do
    local bar = view == 'toolbar'
    local pane = mgr:GetPane(view)
    if show then -- starting debugging and pane is not shown
      -- show toolbar during debugging if hidden and not fullscreen
      debugger.toggleview[view] = (not pane:IsShown()
        and (not bar or not ide.frame:IsFullScreen()))
      if debugger.toggleview[view] and (needed or bar) then
        pane:Show()
        refresh = true
      end
    else -- completing debugging and pane is shown
      debugger.toggleview[view] = pane:IsShown() and needed
      if debugger.toggleview[view] then
        pane:Hide()
        refresh = true
      end
    end
  end
  if refresh then mgr:Update() end
  if show then debugger.toggleview.shown = true end
end

local function killProcess(pid)
  if not pid then return false end
  if wx.wxProcess.Exists(pid) then
    local _ = wx.wxLogNull() -- disable error popup; will report as needed
    -- using SIGTERM for some reason kills not only the debugee process,
    -- but also some system processes, which leads to a blue screen crash
    -- (at least on Windows Vista SP2)
    local ret = wx.wxProcess.Kill(pid, wx.wxSIGKILL, wx.wxKILL_CHILDREN)
    if ret == wx.wxKILL_OK then
      ide:Print(TR("Program stopped (pid: %d)."):format(pid))
    elseif ret ~= wx.wxKILL_NO_PROCESS then
      wx.wxMilliSleep(250)
      if wx.wxProcess.Exists(pid) then
        displayError(TR("Unable to stop program (pid: %d), code %d."):format(pid, ret))
        return false
      end
    end
  end
  return true
end

function debugger:ActivateDocument(file, line, activatehow)
  if activatehow == activate.CLEARALL then ClearAllCurrentLineMarkers() end

  local debugger = self
  if not file then return end
  line = tonumber(line)

  -- file can be a filename or serialized file content; deserialize first.
  -- check if the filename starts with '"' and is deserializable
  -- to avoid showing filenames that may look like valid lua code
  -- (for example: 'mobdebug.lua').
  local content
  if not wx.wxFileName(file):FileExists() and file:find('^"') then
    local ok, res = LoadSafe("return "..file)
    if ok then content = res end
  end

  -- in some cases filename can be returned quoted if the chunk is loaded with
  -- loadstring(chunk, "filename") instead of loadstring(chunk, "@filename")
  if content then
    -- if the returned content can be matched with a file, it's a file name
    local fname = GetFullPathIfExists(debugger.basedir, content) or content
    if wx.wxFileName(fname):FileExists() then file, content = fname, nil end
  elseif not wx.wxIsAbsolutePath(file) and debugger.basedir then
    file = debugger.basedir .. file
  end

  if PackageEventHandle("onDebuggerPreActivate", debugger, file, line) == false then return end

  local activated = false
  local indebugger = file:find('mobdebug%.lua$')
  local fileName = wx.wxFileName(file)
  local fileNameLower = wx.wxFileName(file:lower())

  for _, document in pairs(ide.openDocuments) do
    local editor = document.editor
    -- either the file name matches, or the content;
    -- when checking for the content remove all newlines as they may be
    -- reported differently from the original by the Lua engine.
    local ignorecase = ide.config.debugger.ignorecase or (debugger.options or {}).ignorecase
    local filePath = document:GetFilePath()
    if filePath and (fileName:SameAs(wx.wxFileName(filePath))
      or ignorecase and fileNameLower:SameAs(wx.wxFileName(filePath:lower())))
    or content and content:gsub("[\n\r]","") == editor:GetTextDyn():gsub("[\n\r]","") then
      ClearAllCurrentLineMarkers()
      if line then
        if line == 0 then -- special case; find the first executable line
          line = math.huge
          local func = loadstring(editor:GetTextDyn())
          if func then -- .activelines == {[3] = true, [4] = true, ...}
            for l in pairs(debug.getinfo(func, "L").activelines) do
              if l < line then line = l end
            end
          end
          if line == math.huge then line = 1 end
        end
        if debugger.runtocursor then
          local ed, ln = unpack(debugger.runtocursor)
          if ed:GetId() == editor:GetId() and ln == line then
            -- remove run-to breakpoint at this location
            debugger:breakpointToggle(ed, ln, false)
            debugger.runtocursor = nil
          end
        end
        local line = line - 1 -- editor line operations are zero-based
        editor:MarkerAdd(line, CURRENT_LINE_MARKER)
        editor:Refresh() -- needed for background markers that don't get refreshed (wx2.9.5)

        -- expand fold if the activated line is in a folded fragment
        if not editor:GetLineVisible(line) then editor:ToggleFold(editor:GetFoldParent(line)) end

        -- found and marked what we are looking for;
        -- don't need to activate with CHECKONLY (this assumes line is given)
        if activatehow == activate.CHECKONLY then return editor end

        local firstline = editor:DocLineFromVisible(editor:GetFirstVisibleLine())
        local lastline = math.min(editor:GetLineCount(),
          editor:DocLineFromVisible(editor:GetFirstVisibleLine() + editor:LinesOnScreen()))
        -- if the line is already on the screen, then don't enforce policy
        if line <= firstline or line >= lastline then
          editor:EnsureVisibleEnforcePolicy(line)
        end
      end

      local selection = document.index
      ide:RequestAttention()
      notebook:SetSelection(selection)
      SetEditorSelection(selection)

      if content then
        -- it's possible that the current editor tab already has
        -- breakpoints that have been set based on its filepath;
        -- if the content has been matched, then existing breakpoints
        -- need to be removed and new ones set, based on the content.
        if not debugger.editormap[editor] and filePath then
          local line = editor:MarkerNext(0, BREAKPOINT_MARKER_VALUE)
          while filePath and line ~= -1 do
            debugger:handle("delb " .. filePath .. " " .. (line+1))
            debugger:handle("setb " .. file .. " " .. (line+1))
            line = editor:MarkerNext(line + 1, BREAKPOINT_MARKER_VALUE)
          end
        end

        -- keep track of those editors that have been activated based on
        -- content rather than file names as their breakpoints have to be
        -- specified in a different way
        debugger.editormap[editor] = file
      end

      activated = editor
      break
    end
  end

  if not (activated or indebugger or debugger.loop or activatehow == activate.CHECKONLY)
  and (ide.config.editor.autoactivate or content and activatehow == activate.NOREPORT) then
    -- found file, but can't activate yet (because this part may be executed
    -- in a different coroutine), so schedule pending activation.
    if content or wx.wxFileName(file):FileExists() then
      debugger.activate = {file, line, content}
      return true -- report successful activation, even though it's pending
    end

    -- only report files once per session and if not asked to skip
    if not debugger.missing[file] and activatehow ~= activate.NOREPORT then
      debugger.missing[file] = true
      displayError(TR("Couldn't activate file '%s' for debugging; continuing without it.")
        :format(file))
    end
  end

  PackageEventHandle("onDebuggerActivate", debugger, file, line, activated)

  return activated
end

function debugger:reSetBreakpoints()
  local debugger = self
  -- remove all breakpoints that may still be present from the last session
  -- this only matters for those remote clients that reload scripts
  -- without resetting their breakpoints
  debugger:handle("delallb")

  -- go over all windows and find all breakpoints
  if (not debugger.scratchpad) then
    for _, document in pairs(ide.openDocuments) do
      local editor = document.editor
      local filePath = document.filePath
      local line = editor:MarkerNext(0, BREAKPOINT_MARKER_VALUE)
      while filePath and line ~= -1 do
        debugger:handle("setb " .. filePath .. " " .. (line+1))
        line = editor:MarkerNext(line + 1, BREAKPOINT_MARKER_VALUE)
      end
    end
  end
end

function debugger:shell(expression, isstatement)
  local debugger = self
  -- check if the debugger is running and may be waiting for a response.
  -- allow that request to finish, otherwise this function does nothing.
  if debugger.running then debugger:Update() end
  if debugger.server and not debugger.running
  and (not debugger.scratchpad or debugger.scratchpad.paused) then
    copas.addthread(function()
        local debugger = debugger
        -- exec command is not expected to return anything.
        -- eval command returns 0 or more results.
        -- 'values' has a list of serialized results returned.
        -- as it is not possible to distinguish between 0 results and one
        -- 'nil' value returned, 'nil' is always returned in this case.
        -- the first value returned by eval command is not used;
        -- this may need to be taken into account by other debuggers.
        local addedret, forceexpression = true, expression:match("^%s*=%s*")
        expression = expression:gsub("^%s*=%s*","")
        local _, values, err = debugger:evaluate(expression)
        if not forceexpression and err then
          local _, values2, err2 = debugger:execute(expression)
          -- since the remote execution may fail during compilation- and run-time,
          -- and some expressions may fail in both cases, try to report the "best" error.
          -- for example, `x[1]` fails as statement, and may also fail if `x` is `nil`.
          -- in this case, the first (expression) error is returned if it's not a
          -- statement and compiles as an expression without errors.
          -- the order of statement and expression checks can't be reversed as errors from
          -- code fragments that fail with both, will be always reported as expressions.
          if not (err2 and not isstatement and loadstring("return "..expression)) then
            addedret, values, err = false, values2, err2
          end
        end

        if err then
          if addedret then err = err:gsub('^%[string "return ', '[string "') end
          ide:GetConsole():Error(err)
        elseif addedret or #values > 0 then
          if forceexpression then -- display elements as multi-line
            for i,v in pairs(values) do -- stringify each of the returned values
              local func = loadstring('return '..v) -- deserialize the value first
              if func then -- if it's deserialized correctly
                values[i] = (forceexpression and i > 1 and '\n' or '') ..
                  serialize(func(), {nocode = true, comment = 0,
                    -- if '=' is used, then use multi-line serialized output
                    indent = forceexpression and '  ' or nil})
              end
            end
          end

          -- if empty table is returned, then show nil if this was an expression
          if #values == 0 and (forceexpression or not isstatement) then
            values = {'nil'}
          end
          ide:GetConsole():Print(unpack(values))
        end

        -- refresh Stack and Watch windows if executed a statement (and no err)
        if isstatement and not err and not addedret and #values == 0 then
          debugger:updateStackSync() debugger:updateWatchesSync()
        end
      end)
  elseif debugger.server then
    ide:GetConsole():Error(TR("Can't evaluate the expression while the application is running."))
  end
end

function debugger:stoppedAtBreakpoint(file, line)
  -- if this document can be activated and the current line has a breakpoint
  local editor = self:ActivateDocument(file, line, activate.CHECKONLY)
  if not editor then return false end

  local current = editor:MarkerNext(0, CURRENT_LINE_MARKER_VALUE)
  local breakpoint = editor:MarkerNext(current, BREAKPOINT_MARKER_VALUE)
  return breakpoint ~= wx.wxNOT_FOUND and breakpoint == current
end

function debugger:mapRemotePath(basedir, file, line, method)
  local debugger = self
  if not file then return end

  -- file is /foo/bar/my.lua; basedir is d:\local\path\
  -- check for d:\local\path\my.lua, d:\local\path\bar\my.lua, ...
  -- wxwidgets on Windows handles \\ and / as separators, but on OSX
  -- and Linux it only handles 'native' separator;
  -- need to translate for GetDirs to work.
  local file = file:gsub("\\", "/")
  local parts = wx.wxFileName(file):GetDirs()
  local name = wx.wxFileName(file):GetFullName()

  -- find the longest remote path that can be mapped locally
  local longestpath, remotedir
  while true do
    local mapped = GetFullPathIfExists(basedir, name)
    if mapped then
      longestpath = mapped
      remotedir = file:gsub(q(name):gsub("/", ".").."$", "")
    end
    if #parts == 0 then break end
    name = table.remove(parts, #parts) .. "/" .. name
  end
  -- if the mapped directory empty or the same as the basedir, nothing to do
  if not remotedir or remotedir == "" or wx.wxFileName(remotedir):SameAs(wx.wxFileName(debugger.basedir)) then return end

  -- if found a local mapping under basedir
  local activated = longestpath and (debugger:ActivateDocument(longestpath, line, method or activate.NOREPORT)
    -- local file may exist, but not activated when not (auto-)opened, still need to remap
    or wx.wxFileName(longestpath):FileExists())
  if activated then
    -- find remote basedir by removing the tail from remote file
    debugger:handle("basedir " .. debugger.basedir .. "\t" .. remotedir)
    -- reset breakpoints again as remote basedir has changed
    debugger:reSetBreakpoints()
    ide:Print(TR("Mapped remote request for '%s' to '%s'."):format(remotedir, debugger.basedir))

    return longestpath
  end

  return nil
end

function debugger:Listen(start)
  local debugger = ide:GetDebugger()
  if start == false then
    if debugger.listening then
      debugger:terminate() -- terminate if running
      copas.removeserver(debugger.listening)
      ide:Print(TR("Debugger server stopped at %s:%d.")
        :format(debugger.hostname, debugger.portnumber))
      debugger.listening = false
    else
      displayError(TR("Can't stop debugger server as it is not started."))
    end
    return
  end

  if debugger.listening then return end

  local server, err = socket.bind("*", debugger.portnumber)
  if not server then
    displayError(TR("Can't start debugger server at %s:%d: %s.")
      :format(debugger.hostname, debugger.portnumber, err or TR("unknown error")))
    return
  end
  ide:Print(TR("Debugger server started at %s:%d."):format(debugger.hostname, debugger.portnumber))

  copas.autoclose = false
  copas.addserver(server, function (skt)
      local debugger = ide:GetDebugger()
      local options = debugger.options or {}
      if options.refuseonconflict == nil then options.refuseonconflict = ide.config.debugger.refuseonconflict end

      -- pull any pending data not processed yet
      if debugger.running then debugger:Update() end
      if debugger.server and options.refuseonconflict then
        displayError(TR("Refused a request to start a new debugging session as there is one in progress already."))
        return
      end

      -- error handler is set per-copas-thread
      copas.setErrorHandler(function(error)
        -- ignore errors that happen because debugging session is
        -- terminated during handshake (server == nil in this case).
        if debugger.server then
          displayError(TR("Can't start debugging session due to internal error '%s'."):format(error))
        end
        debugger:terminate()
      end)

      -- this may be a remote call without using an interpreter and as such
      -- debugger.options may not be set, but runonstart is still configured.
      local runstart = options.runstart
      if runstart == nil then runstart = ide.config.debugger.runonstart end

      -- support allowediting as set in the interpreter or config
      if options.allowediting == nil then options.allowediting = ide.config.debugger.allowediting end

      if not debugger.scratchpad and not options.allowediting then
        SetAllEditorsReadOnly(true)
      end

      debugger = ide:SetDebugger(debugger:init({
          server = copas.wrap(skt),
          socket = skt,
          loop = false,
          scratchable = false,
          stats = {line = 0},
          missing = {},
          editormap = {},
          runtocursor = nil,
      }))

      if PackageEventHandle("onDebuggerPreLoad", debugger, options) == false then return end

      local wxfilepath = GetEditorFileAndCurInfo()
      local startfile = ide:GetProjectStartFile() or options.startwith
        or (wxfilepath and wxfilepath:GetFullPath())

      if not startfile then
        displayError(TR("Can't start debugging without an opened file or with the current file not being saved."))
        return debugger:terminate()
      end

      local startpath = wx.wxFileName(startfile):GetPath(wx.wxPATH_GET_VOLUME + wx.wxPATH_GET_SEPARATOR)
      local basedir = options.basedir or FileTreeGetDir() or startpath
      -- guarantee that the path has a trailing separator
      debugger.basedir = wx.wxFileName.DirName(basedir):GetFullPath()

      -- load the remote file into the debugger
      -- set basedir first, before loading to make sure that the path is correct
      debugger:handle("basedir " .. debugger.basedir)

      local init = options.init or ide.config.debugger.init
      if init then
        local _, _, err = debugger:execute(init)
        if err then displayError(TR("Ignored error in debugger initialization code: %s."):format(err)) end
      end

      debugger:reSetBreakpoints()

      local redirect = ide.config.debugger.redirect or options.redirect
      if redirect then
        debugger:handle("output stdout " .. redirect, nil,
          { handler = function(m)
              -- if it's an error returned, then handle the error
              if m and m:find("stack traceback:", 1, true) then
                -- this is an error message sent remotely
                local ok, res = LoadSafe("return "..m)
                if ok then
                  ide:Print(res)
                  return
                end
              end

              if ide.config.debugger.outputfilter then
                local ok, res = pcall(ide.config.debugger.outputfilter, m)
                if ok then
                  m = res
                else
                  displayError("Output filter failed: "..res)
                  return
                end
              end
              if m then ide:GetOutput():Write(m) end
            end})
      end

      if (options.startwith) then
        local file, line, err = debugger:loadfile(options.startwith)
        if err then
          displayError(TR("Can't run the entry point script ('%s').")
            :format(options.startwith)
            .." "..TR("Compilation error")
            ..":\n"..err)
          return debugger:terminate()
        elseif runstart and not debugger.scratchpad then
          if debugger:stoppedAtBreakpoint(file, line) then
            debugger:ActivateDocument(file, line)
            runstart = false
          end
        elseif file and line and not debugger:ActivateDocument(file, line) then
          displayError(TR("Debugging suspended at '%s:%s' (couldn't activate the file).")
            :format(file, line))
        end
      elseif not (options.run or debugger.scratchpad) then
        local file, line, err = debugger:loadfile(startfile)
        -- "load" can work in two ways: (1) it can load the requested file
        -- OR (2) it can "refuse" to load it if the client was started
        -- with start() method, which can't load new files
        -- if file and line are set, this indicates option #2
        if err then
          displayError(TR("Can't start debugging for '%s'."):format(startfile)
            .." "..TR("Compilation error")
            ..":\n"..err)
          return debugger:terminate()
        elseif runstart then
          local file = (debugger:mapRemotePath(basedir, file, line or 0, activate.CHECKONLY)
            or file or startfile)

          if debugger:stoppedAtBreakpoint(file, line or 0) then
            debugger:ActivateDocument(file, line or 0)
            runstart = false
          end
        elseif file and line then
          local activated = debugger:ActivateDocument(file, line, activate.NOREPORT)

          -- if not found, check using full file path and reset basedir
          if not activated and not wx.wxIsAbsolutePath(file) then
            activated = debugger:ActivateDocument(startpath..file, line, activate.NOREPORT)
            if activated then
              debugger.basedir = startpath
              debugger:handle("basedir " .. debugger.basedir)
              -- reset breakpoints again as basedir has changed
              debugger:reSetBreakpoints()
            end
          end

          -- if not found and the files doesn't exist, it may be
          -- a remote call; try to map it to the project folder.
          -- also check for absolute path as it may need to be remapped
          -- when autoactivation is disabled.
          if not activated and (not wx.wxFileName(file):FileExists()
                                or wx.wxIsAbsolutePath(file)) then
            if debugger:mapRemotePath(basedir, file, line, activate.NOREPORT) then
              activated = true
            end
          end

          if not activated then
            displayError(TR("Debugging suspended at '%s:%s' (couldn't activate the file).")
              :format(file, line))
          end

          -- debugger may still be available for scratchpad,
          -- if the interpreter signals scratchpad support, so enable it.
          debugger.scratchable = ide.interpreter.scratchextloop ~= nil
        else
          debugger.scratchable = true
          local activated = debugger:ActivateDocument(startfile, 0) -- find the appropriate line
          if not activated then
            displayError(TR("Debugging suspended at '%s:%s' (couldn't activate the file).")
              :format(startfile, '?'))
          end
        end
      end

      if (not options.noshell and not debugger.scratchpad) then
        ide:GetConsole():SetRemote(debugger:GetConsole())
      end

      debugger:toggleViews(true)
      debugger:updateStackSync()
      debugger:updateWatchesSync()

      ide:Print(TR("Debugging session started in '%s'."):format(debugger.basedir))

      if (debugger.scratchpad) then
        debugger.scratchpad.updated = true
      else
        if runstart then
          ClearAllCurrentLineMarkers()
          debugger:Run()
        end
        if (options.run) then
          local file, line = debugger:handle("run")
          debugger:ActivateDocument(file, line)
        end
      end

      -- request attention if the debugging is stopped
      if not debugger.running then ide:RequestAttention() end
      -- refresh toolbar and menus in case the main app is not active
      ide:GetMainFrame():UpdateWindowUI(wx.wxUPDATE_UI_FROMIDLE)
      ide:GetToolBar():UpdateWindowUI(wx.wxUPDATE_UI_FROMIDLE)

      PackageEventHandle("onDebuggerLoad", debugger, options)
    end)
  debugger.listening = server
end

local function nameOutputTab(name)
  local nbk = ide.frame.bottomnotebook
  local index = nbk:GetPageIndex(ide:GetOutput())
  if index ~= wx.wxNOT_FOUND then nbk:SetPageText(index, name) end
end

function debugger:handle(command, server, options)
  local debugger = self
  local verbose = ide.config.debugger.verbose
  options = options or {}
  options.verbose = verbose and (function(...) ide:Print(...) end) or false

  local ip, port = debugger.socket:getpeername()
  PackageEventHandle("onDebuggerCommand", debugger, command, server or debugger.server, options)
  debugger.running = true
  debugger:UpdateStatus("running")
  if verbose then ide:Print(("[%s:%s] Debugger sent (command):"):format(ip, port), command) end
  local file, line, err = mobdebug.handle(command, server or debugger.server, options)
  if verbose then ide:Print(("[%s:%s] Debugger received (file, line, err):"):format(ip, port), file, line, err) end
  debugger.running = false
  -- only set suspended if the debugging hasn't been terminated
  debugger:UpdateStatus(debugger.server and "suspended" or "stopped")

  return file, line, err
end

function debugger:exec(command, func)
  local debugger = self
  if debugger.server and not debugger.running then
    copas.addthread(function()
        local debugger = debugger
        -- execute a custom function (if any) in the context of this thread
        if type(func) == 'function' then func() end
        local out
        local attempts = 0
        while true do
          -- clear markers before running the command
          -- don't clear if running trace as the marker is then invisible,
          -- and it needs to be visible during tracing
          if not debugger.loop then ClearAllCurrentLineMarkers() end
          debugger.breaking = false
          local file, line, err = debugger:handle(out or command)
          if out then out = nil end
          if line == nil then
            if err then displayError(err) end
            debugger:teardown()
            return
          elseif not debugger.server then
            -- it is possible that while debugger.handle call was executing
            -- the debugging was terminated; simply return in this case.
            return
          else
            local activated = debugger:ActivateDocument(file, line)
            -- activation has been canceled; nothing else needs to be done
            if activated == nil then return end
            if activated then
              -- move cursor to the activated line if it's a breakpoint
              if ide.config.debugger.linetobreakpoint
              and command ~= "step" and debugger:stoppedAtBreakpoint(file, line)
              and not debugger.breaking and ide:IsValidCtrl(activated) then
                activated:GotoLine(line-1)
              end
              debugger.stats.line = debugger.stats.line + 1
              if debugger.loop then
                debugger:updateStackSync()
                debugger:updateWatchesSync()
              else
                debugger:updateStackAndWatches()
                return
              end
            else
              -- clear the marker as it wasn't cleared earlier
              if debugger.loop then ClearAllCurrentLineMarkers() end
              -- we may be in some unknown location at this point;
              -- If this happens, stop and report allowing users to set
              -- breakpoints and step through.
              if debugger.breaking then
                displayError(TR("Debugging suspended at '%s:%s' (couldn't activate the file).")
                  :format(file, line))
                debugger:updateStackAndWatches()
                return
              end
              -- redo now; if the call is from the debugger, then repeat
              -- the same command, except when it was "run" (switch to 'step');
              -- this is needed to "break" execution that happens in on() call.
              -- in all other cases get out of this file.
              -- don't get out of "mobdebug", because it may happen with
              -- start() or on() call, which will get us out of the current
              -- file, which is not what we want.
              -- Some engines (Corona SDK) report =?:0 as the current location.
              -- repeat the same command, but check if this has been tried
              -- too many times already; if so, get "out"
              out = ((tonumber(line) == 0 and attempts < 10) and command
                or (file:find('mobdebug%.lua$')
                  and (command == 'run' and 'step' or command) or "out"))
              attempts = attempts + 1
            end
          end
        end
      end)
  end
end

function debugger:handleAsync(command)
  local debugger = self
  if debugger.server and not debugger.running then
    copas.addthread(function()
        local debugger = debugger
        debugger:handle(command)
      end)
  end
end
function debugger:handleDirect(command)
  local debugger = self
  local sock = debugger.socket
  if debugger.server and sock then
    local running = debugger.running
    -- this needs to be short as it will block the UI
    sock:settimeout(0.25)
    debugger:handle(command, sock)
    sock:settimeout(0)
    -- restore running status
    debugger.running = running
  end
end

function debugger:loadfile(file)
  local debugger = self
  local f, l, err = debugger:handle("load " .. file)
  if not f and wx.wxFileExists(file) and err and err:find("Cannot open file") then
    local content = FileRead(file)
    if content then return debugger:loadstring(file, content) end
  end
  return f, l, err
end
function debugger:loadstring(file, string)
  local debugger = self
  return debugger:handle("loadstring '" .. file .. "' " .. string)
end

do
  local nextupdatedelta = 0.250
  local nextupdate = TimeGet() + nextupdatedelta
  local function forceUpdateOnWrap(editor)
    -- http://www.scintilla.org/ScintillaDoc.html#LineWrapping
    -- Scintilla doesn't perform wrapping immediately after a content change
    -- for performance reasons, so the activation calculations can be wrong
    -- if there is wrapping that pushes the current line out of the screen.
    -- force editor update that performs wrapping recalculation.
    if ide.config.editor.usewrap then editor:Update(); editor:Refresh() end
  end
  function debugger:Update()
    local debugger = self
    local smth = false
    if debugger.server or debugger.listening and TimeGet() > nextupdate then
      smth = copas.step(0)
      nextupdate = TimeGet() + nextupdatedelta
    end

    -- if there is any pending activation
    if debugger.activate then
      local file, line, content = unpack(debugger.activate)
      debugger.activate = nil
      if content then
        local editor = NewFile()
        editor:SetTextDyn(content)
        if not ide.config.debugger.allowediting
        and not (debugger.options or {}).allowediting then
          editor:SetReadOnly(true)
        end
        forceUpdateOnWrap(editor)
        debugger:ActivateDocument(file, line)
      else
        local editor = LoadFile(file)
        if editor then
          forceUpdateOnWrap(editor)
          debugger:ActivateDocument(file, line)
        end
      end
    end
    return smth
  end
end

function debugger:terminate()
  local debugger = self
  if debugger.server then
    if killProcess(ide:GetLaunchedProcess()) then -- if there is PID, try local kill
      ide:SetLaunchedProcess(nil)
    else -- otherwise, try graceful exit for the remote process
      debugger:detach("exit")
    end
    debugger:teardown()
  end
end
function debugger:Step() return self:exec("step") end
function debugger:trace()
  local debugger = self
  debugger.loop = true
  debugger:exec("step")
end
function debugger:RunTo(editor, line)
  local debugger = self

  -- check if the location is valid for a breakpoint
  if editor:IsLineEmpty(line-1) then return end

  local ed, ln = unpack(debugger.runtocursor or {})
  local same = ed and ln and ed:GetId() == editor:GetId() and ln == line

  -- check if there is already a breakpoint in the "run to" location;
  -- if so, don't mark the location as "run to" as it will stop there anyway
  if bit.band(editor:MarkerGet(line-1), BREAKPOINT_MARKER_VALUE) > 0
  and not same then
    debugger.runtocursor = nil
    debugger:Run()
    return
  end

  -- save the location of the breakpoint
  debugger.runtocursor = {editor, line}
  -- set breakpoint and execute run
  debugger:exec("run", function()
      -- if run-to-cursor location is already set, then remove the breakpoint,
      -- but only if this location is different
      if ed and ln and not same then
        debugger:breakpointToggle(ed, ln, false) -- remove earlier run-to breakpoint
        debugger:Wait()
      end
      if not same then
        debugger:breakpointToggle(editor, line, true) -- set new run-to breakpoint
        debugger:Wait()
      end
    end)
end
function debugger:Wait()
  local debugger = self
  -- wait for all results to come back
  while debugger.running do debugger:Update() end
end
function debugger:Over() return self:exec("over") end
function debugger:Out() return self:exec("out") end
function debugger:Run() return self:exec("run") end
function debugger:detach(cmd)
  local debugger = self
  if not debugger.server then return end
  if debugger.running then
    debugger:handleDirect(cmd or "done")
    debugger:teardown()
  else
    debugger:exec(cmd or "done")
  end
end
local function todeb(params) return params and " -- "..mobdebug.line(params, {comment = false}) or "" end
function debugger:evaluate(exp, params) return self:handle('eval ' .. exp .. todeb(params)) end
function debugger:execute(exp, params) return self:handle('exec '.. exp .. todeb(params)) end
function debugger:stack(params) return self:handle('stack' .. todeb(params)) end
function debugger:Break(command)
  local debugger = self
  -- stop if we're running a "trace" command
  debugger.loop = false

  -- force suspend command; don't use copas interface as it checks
  -- for the other side "reading" and the other side is not reading anything.
  -- use the "original" socket to send "suspend" command.
  -- this will only break on the next Lua command.
  if debugger.socket then
    local running = debugger.running
    -- this needs to be short as it will block the UI
    debugger.socket:settimeout(0.25)
    local file, line, err = debugger:handle(command or "suspend", debugger.socket)
    debugger.socket:settimeout(0)
    -- restore running status
    debugger.running = running
    debugger.breaking = true
    -- don't need to do anything else as the earlier call (run, step, etc.)
    -- will get the results (file, line) back and will update the UI
    return file, line, err
  end
end
function debugger:breakpoint(file, line, state)
  local debugger = self
  if debugger.running then
    return debugger:handleDirect((state and "asetb " or "adelb ") .. file .. " " .. line)
  end
  return debugger:handleAsync((state and "setb " or "delb ") .. file .. " " .. line)
end
function debugger:EvalAsync(var, callback, params)
  local debugger = self
  if debugger.server and not debugger.running and callback
  and not debugger.scratchpad and not (debugger.options or {}).noeval then
    copas.addthread(function()
      local debugger = debugger
      local _, values, err = debugger:evaluate(var, params)
      if err then
        callback(nil, (err:gsub("%[.-%]:%d+:%s*","error: ")))
      else
        callback(#values > 0 and values[1] or 'nil')
      end
    end)
  end
end

local width, height = 360, 200

local keyword = {}
for _,k in ipairs({'and', 'break', 'do', 'else', 'elseif', 'end', 'false',
  'for', 'function', 'goto', 'if', 'in', 'local', 'nil', 'not', 'or', 'repeat',
  'return', 'then', 'true', 'until', 'while'}) do keyword[k] = true end

local function stringifyKeyIntoPrefix(name, num)
  return (type(name) == "number"
    and (num and num == name and '' or ("[%s] = "):format(name))
    or type(name) == "string" and (name:match("^[%l%u_][%w_]*$") and not keyword[name]
      and ("%s = "):format(name)
      or ("[%q] = "):format(name))
    or ("[%s] = "):format(tostring(name)))
end

local function debuggerCreateStackWindow()
  local stackCtrl = ide:CreateTreeCtrl(ide.frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(width, height),
    wx.wxTR_LINES_AT_ROOT + wx.wxTR_HAS_BUTTONS + wx.wxTR_SINGLE
    + wx.wxTR_HIDE_ROOT + wx.wxNO_BORDER)

  local debugger = ide:GetDebugger()
  debugger.stackCtrl = stackCtrl

  stackCtrl:SetImageList(debugger.imglist)

  local names = {}
  function stackCtrl:SetItemName(item, name)
    local nametype = type(name)
    names[item:GetValue()] = (
      (nametype == 'string' or nametype == 'number' or nametype == 'boolean')
      and name or nil
    )
  end

  function stackCtrl:GetItemName(item)
    return names[item:GetValue()]
  end

  local expandable = {} -- special value
  local valuecache = {}
  function stackCtrl:SetItemValueIfExpandable(item, value, delayed)
    local maxlvl = tonumber(ide.config.debugger.maxdatalevel)
    -- don't make empty tables expandable if expansion is disabled (`maxdatalevel` is false)
    local isexpandable = type(value) == 'table' and (next(value) ~= nil or delayed and maxlvl ~= nil)
    if isexpandable then -- cache table value to expand when requested
      valuecache[item:GetValue()] = next(value) == nil and expandable or value
    elseif type(value) ~= 'table' then
      valuecache[item:GetValue()] = nil
    end
    self:SetItemHasChildren(item, isexpandable)
  end

  function stackCtrl:IsExpandable(item) return valuecache[item:GetValue()] == expandable end

  function stackCtrl:DeleteAll()
    self:DeleteAllItems()
    valuecache = {}
    names = {}
  end

  function stackCtrl:GetItemChildren(item)
    return valuecache[item:GetValue()] or {}
  end

  function stackCtrl:IsFrame(item)
    return (item and item:IsOk() and self:GetItemParent(item):IsOk()
      and self:GetItemParent(item):GetValue() == self:GetRootItem():GetValue())
  end

  function stackCtrl:GetItemFullExpression(item)
    local expr = ''
    while item:IsOk() and not self:IsFrame(item) do
      local name = self:GetItemName(item)
      -- check if it's a top item, as it needs to be used as is;
      -- convert `(*vararg num)` to `select(num, ...)`
      expr = (self:IsFrame(self:GetItemParent(item))
        and name:gsub("^%(%*vararg (%d+)%)$", "select(%1, ...)")
        or (type(name) == 'string' and '[%q]' or '[%s]'):format(tostring(name)))
      ..expr
      item = self:GetItemParent(item)
    end
    return expr, item:IsOk() and item or nil
  end

  function stackCtrl:GetItemPos(item)
    if not item:IsOk() then return end
    local pos = 0
    repeat
      pos = pos + 1
      item = self:GetPrevSibling(item)
    until not item:IsOk()
    return pos
  end

  function stackCtrl:ExpandItemValue(item)
    local expr, itemframe = self:GetItemFullExpression(item)
    local stack = self:GetItemPos(itemframe)

    local debugger = ide:GetDebugger()
    if debugger.running then debugger:Update() end
    if debugger.server and not debugger.running
    and (not debugger.scratchpad or debugger.scratchpad.paused) then
      copas.addthread(function()
        local debugger = debugger
        local value, _, err = debugger:evaluate(expr, {maxlevel = 1, stack = stack})
        if err then
          err = err:gsub("%[.-%]:%d+:%s+","")
          -- this may happen when attempting to expand a sub-element referenced by a key
          -- that can't be evaluated, like a table, function, or userdata
          if err ~= "attempt to index a nil value" then
            self:SetItemText(item, 'error: '..err)
          else
            local name = self:GetItemName(item)
            local text = stringifyKeyIntoPrefix(name, self:GetItemPos(item)).."{}"
            self:SetItemText(item, text)
            self:SetItemValueIfExpandable(item, {})
            self:Expand(item)
          end
        else
          local ok, res = LoadSafe("return "..tostring(value))
          if ok then
            self:SetItemValueIfExpandable(item, res)
            self:Expand(item)

            local name = self:GetItemName(item)
            if not name then
              -- this is an empty table, so replace MORE indicator with the empty table
              self:SetItemText(item, (self:GetItemText(item):gsub(q(MORE), "{}")))
              return
            end

            -- update cache in the parent
            local parent = self:GetItemParent(item)
            valuecache[parent:GetValue()][name] = res

            local params = debugger:GetDataOptions({maxlevel=false})

            -- now update all serialized values in the tree starting from the expanded item
            while item:IsOk() and not self:IsFrame(item) do
              local value = valuecache[item:GetValue()]
              local strval = fixUTF8(serialize(value, params))
              local name = self:GetItemName(item)
              local text = (self:IsFrame(self:GetItemParent(item))
                and name.." = "
                or stringifyKeyIntoPrefix(name, self:GetItemPos(item)))
              ..strval
              self:SetItemText(item, text)
              item = self:GetItemParent(item)
            end
          end
        end
      end)
    end
  end

  stackCtrl:Connect(wx.wxEVT_COMMAND_TREE_ITEM_EXPANDING,
    function (event)
      local item_id = event:GetItem()
      local count = stackCtrl:GetChildrenCount(item_id, false)
      if count > 0 then return true end

      if stackCtrl:IsExpandable(item_id) then return stackCtrl:ExpandItemValue(item_id) end

      local image = stackCtrl:GetItemImage(item_id)
      local num, maxnum = 1, ide.config.debugger.maxdatanum
      local params = debugger:GetDataOptions({maxlevel = false})

      stackCtrl:Freeze()
      for name,value in pairs(stackCtrl:GetItemChildren(item_id)) do
        local item = stackCtrl:AppendItem(item_id, "", image)
        stackCtrl:SetItemValueIfExpandable(item, value, true)

        local strval = stackCtrl:IsExpandable(item) and MORE or fixUTF8(serialize(value, params))
        stackCtrl:SetItemText(item, stringifyKeyIntoPrefix(name, num)..strval)
        stackCtrl:SetItemName(item, name)

        num = num + 1
        if num > maxnum then break end
      end
      stackCtrl:Thaw()
      return true
    end)

  stackCtrl:Connect(wx.wxEVT_SET_FOCUS, function(event)
      local debugger = ide:GetDebugger()
      if debugger.needrefresh.stack then
        debugger:updateStack()
        debugger.needrefresh.stack = false
      end
    end)

  -- register navigation callback
  stackCtrl:Connect(wx.wxEVT_LEFT_DCLICK, function (event)
    local item_id = stackCtrl:HitTest(event:GetPosition())
    if not item_id or not item_id:IsOk() then event:Skip() return end

    local coords = callData[item_id:GetValue()]
    if not coords then event:Skip() return end

    local file, line = coords[1], coords[2]
    if file:match("@") then file = string.sub(file, 2) end
    file = GetFullPathIfExists(ide:GetDebugger().basedir, file)
    if file then
      local editor = LoadFile(file,nil,true)
      editor:SetFocus()
      if line then
        editor:GotoLine(line-1)
        editor:EnsureVisibleEnforcePolicy(line-1) -- make sure the line is visible (unfolded)
      end
    end
  end)

  local layout = ide:GetSetting("/view", "uimgrlayout")
  if layout and not layout:find("stackpanel") then
    ide:AddPanelDocked(ide.frame.bottomnotebook, stackCtrl, "stackpanel", TR("Stack"))
  else
    ide:AddPanel(stackCtrl, "stackpanel", TR("Stack"))
  end
end

local function debuggerCreateWatchWindow()
  local watchCtrl = ide:CreateTreeCtrl(ide.frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(width, height),
    wx.wxTR_LINES_AT_ROOT + wx.wxTR_HAS_BUTTONS + wx.wxTR_SINGLE
    + wx.wxTR_HIDE_ROOT + wx.wxTR_EDIT_LABELS + wx.wxNO_BORDER)

  local debugger = ide:GetDebugger()
  debugger.watchCtrl = watchCtrl

  local root = watchCtrl:AddRoot("Watch")
  watchCtrl:SetImageList(debugger.imglist)

  local defaultExpr = "watch expression"
  local expressions = {} -- table to keep track of expressions

  function watchCtrl:SetItemExpression(item, expr, value)
    expressions[item:GetValue()] = expr
    self:SetItemText(item, expr .. ' = ' .. (value or '?'))
    self:SelectItem(item, true)
    local debugger = ide:GetDebugger()
    if not value then debugger:updateWatches(item) end
  end

  function watchCtrl:GetItemExpression(item)
    return expressions[item:GetValue()]
  end

  local names = {}
  function watchCtrl:SetItemName(item, name)
    local nametype = type(name)
    names[item:GetValue()] = (
      (nametype == 'string' or nametype == 'number' or nametype == 'boolean')
      and name or nil
    )
  end

  function watchCtrl:GetItemName(item)
    return names[item:GetValue()]
  end

  local expandable = {} -- special value
  local valuecache = {}
  function watchCtrl:SetItemValueIfExpandable(item, value, delayed)
    local maxlvl = tonumber(ide.config.debugger.maxdatalevel)
    -- don't make empty tables expandable if expansion is disabled (`maxdatalevel` is false)
    local isexpandable = type(value) == 'table' and (next(value) ~= nil or delayed and maxlvl ~= nil)
    if isexpandable then -- cache table value to expand when requested
      valuecache[item:GetValue()] = next(value) == nil and expandable or value
    elseif type(value) ~= 'table' then
      valuecache[item:GetValue()] = nil
    end
    self:SetItemHasChildren(item, isexpandable)
  end

  function watchCtrl:IsExpandable(item) return valuecache[item:GetValue()] == expandable end

  function watchCtrl:GetItemChildren(item)
    return valuecache[item:GetValue()] or {}
  end

  function watchCtrl:IsWatch(item)
    return (item and item:IsOk() and self:GetItemParent(item):IsOk()
      and self:GetItemParent(item):GetValue() == root:GetValue())
  end

  function watchCtrl:IsEditable(item)
    return (item and item:IsOk()
      and (self:IsWatch(item) or self:GetItemName(item) ~= nil))
  end

  function watchCtrl:GetItemFullExpression(item)
    local expr = ''
    while true do
      local name = self:GetItemName(item)
      expr = (self:IsWatch(item)
        and ('({%s})[1]'):format(self:GetItemExpression(item))
        or (type(name) == 'string' and '[%q]' or '[%s]'):format(tostring(name))
      )..expr
      if self:IsWatch(item) then break end
      item = self:GetItemParent(item)
      if not item:IsOk() then break end
    end
    return expr, item:IsOk() and item or nil
  end

  function watchCtrl:CopyItemValue(item)
    local expr = self:GetItemFullExpression(item)

    local debugger = ide:GetDebugger()
    if debugger.running then debugger:Update() end
    if debugger.server and not debugger.running
    and (not debugger.scratchpad or debugger.scratchpad.paused) then
      copas.addthread(function()
        local debugger = debugger
        local _, values, error = debugger:evaluate(expr)
        ide:CopyToClipboard(error and error:gsub("%[.-%]:%d+:%s+","")
          or (#values == 0 and 'nil' or fixUTF8(values[1])))
      end)
    end
  end

  function watchCtrl:UpdateItemValue(item, value)
    local expr, itemupd = self:GetItemFullExpression(item)

    local debugger = ide:GetDebugger()
    if debugger.running then debugger:Update() end
    if debugger.server and not debugger.running
    and (not debugger.scratchpad or debugger.scratchpad.paused) then
      copas.addthread(function()
        local debugger = debugger
        local _, _, err = debugger:execute(expr..'='..value)
        if err then
          watchCtrl:SetItemText(item, 'error: '..err:gsub("%[.-%]:%d+:%s+",""))
        elseif itemupd then
          debugger:updateWatchesSync(itemupd)
        end
        debugger:updateStackSync()
      end)
    end
  end

  function watchCtrl:GetItemPos(item)
    if not item:IsOk() then return end
    local pos = 0
    repeat
      pos = pos + 1
      item = self:GetPrevSibling(item)
    until not item:IsOk()
    return pos
  end

  function watchCtrl:ExpandItemValue(item)
    local expr = self:GetItemFullExpression(item)

    local debugger = ide:GetDebugger()
    if debugger.running then debugger:Update() end
    if debugger.server and not debugger.running
    and (not debugger.scratchpad or debugger.scratchpad.paused) then
      copas.addthread(function()
        local debugger = debugger
        local value, _, err = debugger:evaluate(expr, {maxlevel = 1})
        if err then
          self:SetItemText(item, 'error: '..err:gsub("%[.-%]:%d+:%s+",""))
        else
          local ok, res = LoadSafe("return "..tostring(value))
          if ok then
            self:SetItemValueIfExpandable(item, res)
            self:Expand(item)
            local name = self:GetItemName(item)
            if not name then
              self:SetItemText(item, (self:GetItemText(item):gsub(q(MORE), "{}")))
              return
            end

            -- update cache in the parent
            local parent = self:GetItemParent(item)
            valuecache[parent:GetValue()][name] = res

            local params = debugger:GetDataOptions({maxlevel=false})

            -- now update all serialized values in the tree starting from the expanded item
            while item:IsOk() do
              local value = valuecache[item:GetValue()]
              local strval = fixUTF8(serialize(value, params))
              local name = self:GetItemName(item)
              local text = (self:IsWatch(item)
                and self:GetItemExpression(item).." = "
                or stringifyKeyIntoPrefix(name, self:GetItemPos(item)))
              ..strval
              self:SetItemText(item, text)
              if self:IsWatch(item) then break end
              item = self:GetItemParent(item)
            end
          end
        end
      end)
    end
  end

  watchCtrl:Connect(wx.wxEVT_COMMAND_TREE_ITEM_EXPANDING,
    function (event)
      local item_id = event:GetItem()
      local count = watchCtrl:GetChildrenCount(item_id, false)
      if count > 0 then return true end

      if watchCtrl:IsExpandable(item_id) then return watchCtrl:ExpandItemValue(item_id) end

      local image = watchCtrl:GetItemImage(item_id)
      local num, maxnum = 1, ide.config.debugger.maxdatanum
      local params = debugger:GetDataOptions({maxlevel = false})

      watchCtrl:Freeze()
      for name,value in pairs(watchCtrl:GetItemChildren(item_id)) do
        local item = watchCtrl:AppendItem(item_id, "", image)
        watchCtrl:SetItemValueIfExpandable(item, value, true)

        local strval = watchCtrl:IsExpandable(item) and MORE or fixUTF8(serialize(value, params))
        watchCtrl:SetItemText(item, stringifyKeyIntoPrefix(name, num)..strval)
        watchCtrl:SetItemName(item, name)

        num = num + 1
        if num > maxnum then break end
      end
      watchCtrl:Thaw()
      return true
    end)

  watchCtrl:Connect(wx.wxEVT_COMMAND_TREE_DELETE_ITEM,
    function (event)
      local value = event:GetItem():GetValue()
      expressions[value] = nil
      valuecache[value] = nil
      names[value] = nil
    end)

  watchCtrl:Connect(wx.wxEVT_SET_FOCUS, function(event)
      local debugger = ide:GetDebugger()
      if debugger.needrefresh.watches then
        debugger:updateWatches()
        debugger.needrefresh.watches = false
      end
    end)

  local item
  -- wx.wxEVT_CONTEXT_MENU is only triggered over tree items on OSX,
  -- but it needs to be also triggered below any item to add a watch,
  -- so use RIGHT_DOWN instead
  watchCtrl:Connect(wx.wxEVT_RIGHT_DOWN,
    function (event)
      -- store the item to be used in edit/delete actions
      item = watchCtrl:HitTest(watchCtrl:ScreenToClient(wx.wxGetMousePosition()))
      local editlabel = watchCtrl:IsWatch(item) and TR("&Edit Watch") or TR("&Edit Value")
      local menu = ide:MakeMenu {
        { ID_ADDWATCH, TR("&Add Watch")..KSC(ID_ADDWATCH) },
        { ID_EDITWATCH, editlabel..KSC(ID_EDITWATCH) },
        { ID_DELETEWATCH, TR("&Delete Watch")..KSC(ID_DELETEWATCH) },
        { ID_COPYWATCHVALUE, TR("&Copy Value")..KSC(ID_COPYWATCHVALUE) },
      }
      PackageEventHandle("onMenuWatch", menu, watchCtrl, event)
      watchCtrl:PopupMenu(menu)
      item = nil
    end)

  watchCtrl:Connect(ID_ADDWATCH, wx.wxEVT_COMMAND_MENU_SELECTED,
    function (event) watchCtrl:EditLabel(watchCtrl:AppendItem(root, defaultExpr, image.LOCAL)) end)

  watchCtrl:Connect(ID_EDITWATCH, wx.wxEVT_COMMAND_MENU_SELECTED,
    function (event) watchCtrl:EditLabel(item or watchCtrl:GetSelection()) end)
  watchCtrl:Connect(ID_EDITWATCH, wx.wxEVT_UPDATE_UI,
    function (event) event:Enable(watchCtrl:IsEditable(item or watchCtrl:GetSelection())) end)

  watchCtrl:Connect(ID_DELETEWATCH, wx.wxEVT_COMMAND_MENU_SELECTED,
    function (event) watchCtrl:Delete(item or watchCtrl:GetSelection()) end)
  watchCtrl:Connect(ID_DELETEWATCH, wx.wxEVT_UPDATE_UI,
    function (event) event:Enable(watchCtrl:IsWatch(item or watchCtrl:GetSelection())) end)

  watchCtrl:Connect(ID_COPYWATCHVALUE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function (event) watchCtrl:CopyItemValue(item or watchCtrl:GetSelection()) end)
  watchCtrl:Connect(ID_COPYWATCHVALUE, wx.wxEVT_UPDATE_UI, function (event)
      -- allow copying only when the debugger is available
      local debugger = ide:GetDebugger()
      event:Enable(item:IsOk() and debugger.server and not debugger.running
        and (not debugger.scratchpad or debugger.scratchpad.paused))
    end)

  local label
  watchCtrl:Connect(wx.wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT,
    function (event)
      local item = event:GetItem()
      if not (item:IsOk() and watchCtrl:IsEditable(item)) then
        event:Veto()
        return
      end

      label = watchCtrl:GetItemText(item)

      if watchCtrl:IsWatch(item) then
        local expr = watchCtrl:GetItemExpression(item)
        if expr then watchCtrl:SetItemText(item, expr) end
      else
        local prefix = stringifyKeyIntoPrefix(watchCtrl:GetItemName(item))
        local val = watchCtrl:GetItemText(item):gsub(q(prefix),'')
        watchCtrl:SetItemText(item, val)
      end
    end)
  watchCtrl:Connect(wx.wxEVT_COMMAND_TREE_END_LABEL_EDIT,
    function (event)
      event:Veto()

      local item = event:GetItem()
      if event:IsEditCancelled() then
        if watchCtrl:GetItemText(item) == defaultExpr then
          -- when Delete is called from END_EDIT, it causes infinite loop
          -- on OSX (wxwidgets 2.9.5) as Delete calls END_EDIT again.
          -- disable handlers during Delete and then enable back.
          watchCtrl:SetEvtHandlerEnabled(false)
          watchCtrl:Delete(item)
          watchCtrl:SetEvtHandlerEnabled(true)
        else
          watchCtrl:SetItemText(item, label)
        end
      else
        if watchCtrl:IsWatch(item) then
          watchCtrl:SetItemExpression(item, event:GetLabel())
        else
          watchCtrl:UpdateItemValue(item, event:GetLabel())
        end
      end
      event:Skip()
    end)

  local layout = ide:GetSetting("/view", "uimgrlayout")
  if layout and not layout:find("watchpanel") then
    ide:AddPanelDocked(ide.frame.bottomnotebook, watchCtrl, "watchpanel", TR("Watch"))
  else
    ide:AddPanel(watchCtrl, "watchpanel", TR("Watch"))
  end
end

debuggerCreateStackWindow()
debuggerCreateWatchWindow()

----------------------------------------------
-- public api

function debugger:RefreshPanels() return self:updateStackAndWatches() end

function debugger:BreakpointSet(...) return self:breakpoint(...) end

local statuses = {
  running = TR("Output (running)"),
  suspended = TR("Output (suspended)"),
  stopped = TR("Output"),
}
function debugger:UpdateStatus(status)
  local debugger = self
  if not status then
    status = debugger.running and "running" or debugger.server and "suspended" or "stopped"
  end
  if PackageEventHandle("onDebuggerStatusUpdate", debugger, status) == false then return end
  nameOutputTab(statuses[status] or statuses.stopped)
end

function debugger:OutputSet(stream, mode, options)
  return self:handle(("output %s %s"):format(stream, mode), nil, options)
end

function DebuggerAttachDefault(options) ide:GetDebugger():SetOptions(options) end
function debugger:SetOptions(options) self.options = options end

function debugger:Stop()
  local debugger = self
  -- terminate the local session (if still active)
  if killProcess(ide:GetLaunchedProcess()) then ide:SetLaunchedProcess(nil) end
  debugger:terminate()
end

function debugger:Shutdown()
  self:Stop()
  PackageEventHandle("onDebuggerShutdown", self)
end

function debugger:teardown()
  local debugger = self
  if debugger.server then
    local lines = TR("traced %d instruction", debugger.stats.line):format(debugger.stats.line)
    ide:Print(TR("Debugging session completed (%s)."):format(lines))
    debugger:UpdateStatus(ide:GetLaunchedProcess() and "running" or "stopped")
    if debugger.runtocursor then
      local ed, ln = unpack(debugger.runtocursor)
      debugger:breakpointToggle(ed, ln, false) -- remove current run-to breakpoint
    end
    if PackageEventHandle("onDebuggerPreClose", debugger) ~= false then
      SetAllEditorsReadOnly(false)
      ide:GetConsole():SetRemote(nil)
      ClearAllCurrentLineMarkers()
      debugger:toggleViews(false)
      PackageEventHandle("onDebuggerClose", debugger)
    end
    debugger.server = nil
    debugger:ScratchpadOff()
  else
    -- it's possible that the application couldn't start, or that the
    -- debugger in the application didn't start, which means there is
    -- no debugger.server, but scratchpad may still be on. Turn it off.
    debugger:ScratchpadOff()
  end
end

local function debuggerMakeFileName(editor)
  return ide:GetDocument(editor):GetFilePath()
  or ide:GetDocument(editor):GetFileName()
  or ide:GetDefaultFileName()
end

function debugger:breakpointToggle(editor, line, value)
  local debugger = self
  local file = debugger.editormap and debugger.editormap[editor] or debuggerMakeFileName(editor)
  debugger:BreakpointSet(file, line, value)
end

-- scratchpad functions

function debugger:ScratchpadRefresh()
  local debugger = self
  if debugger.scratchpad and debugger.scratchpad.updated and not debugger.scratchpad.paused then
    local scratchpadEditor = debugger.scratchpad.editor
    if scratchpadEditor.spec.apitype
    and scratchpadEditor.spec.apitype == "lua"
    and not ide.interpreter.skipcompile
    and not CompileProgram(scratchpadEditor, { jumponerror = false, reportstats = false })
    then return end

    local code = StripShebang(scratchpadEditor:GetTextDyn())
    if debugger.scratchpad.running then
      -- break the current execution first
      -- don't try too frequently to avoid overwhelming the debugger
      local now = TimeGet()
      if now - debugger.scratchpad.running > 0.250 then
        debugger:Break()
        debugger.scratchpad.running = now
      end
    else
      local clear = ide:GetMenuBar():IsChecked(ID_CLEAROUTPUT)
      local filePath = debuggerMakeFileName(scratchpadEditor)

      -- wrap into a function call to make "return" to work with scratchpad
      code = "(function()"..code.."\nend)()"

      -- this is a special error message that is generated at the very end
      -- of each script to avoid exiting the (debugee) scratchpad process.
      -- these errors are handled and not reported to the user
      local errormsg = 'execution suspended at ' .. TimeGet()
      local stopper = "error('" .. errormsg .. "')"
      -- store if interpreter requires a special handling for external loop
      local extloop = ide.interpreter.scratchextloop

      local function reloadScratchpadCode()
        local debugger = debugger
        debugger.scratchpad.running = TimeGet()
        debugger.scratchpad.updated = false
        debugger.scratchpad.runs = (debugger.scratchpad.runs or 0) + 1

        if clear then ClearOutput(true) end

        -- the code can be running in two ways under scratchpad:
        -- 1. controlled by the application, requires stopper (most apps)
        -- 2. controlled by some external loop (for example, love2d).
        -- in the first case we need to reload the app after each change
        -- in the second case, we need to load the app once and then
        -- "execute" new code to reflect the changes (with some limitations).
        local _, _, err
        if extloop then -- if the execution is controlled by an external loop
          if debugger.scratchpad.runs == 1
          then _, _, err = debugger:loadstring(filePath, code)
          else _, _, err = debugger:execute(code) end
        else   _, _, err = debugger:loadstring(filePath, code .. stopper) end

        -- when execute() is used, it's not possible to distinguish between
        -- compilation and run-time error, so just report as "Scratchpad error"
        local prefix = extloop and TR("Scratchpad error") or TR("Compilation error")

        if not err then
          _, _, err = debugger:handle("run")
          prefix = TR("Execution error")
        end
        if err and not err:find(errormsg) then
          local fragment, line = err:match('.-%[string "([^\010\013]+)"%]:(%d+)%s*:')
          -- make the code shorter to better see the error message
          if prefix == TR("Scratchpad error") and fragment and #fragment > 30 then
            err = err:gsub(q(fragment), function(s) return s:sub(1,30)..'...' end)
          end
          displayError(prefix
            ..(line and (" "..TR("on line %d"):format(line)) or "")
            ..":\n"..err:gsub('stack traceback:.+', ''):gsub('\n+$', ''))
        end
        debugger.scratchpad.running = false
      end

      copas.addthread(reloadScratchpadCode)
    end
  end
end

function debugger:ScratchpadOn(editor)
  local debugger = self

  -- first check if there is already scratchpad editor.
  -- this may happen when more than one editor is being added...
  if debugger.scratchpad and debugger.scratchpad.editors then
    debugger.scratchpad.editors[editor] = true
  else
    debugger.scratchpad = {editor = editor, editors = {[editor] = true}}

    -- check if the debugger is already running; this happens when
    -- scratchpad is turned on after external script has connected
    if debugger.server then
      debugger.scratchpad.updated = true
      ClearAllCurrentLineMarkers()
      SetAllEditorsReadOnly(false)
      ide:GetConsole():SetRemote(nil) -- disable remote shell
      debugger:ScratchpadRefresh()
    elseif not ProjectDebug(true, "scratchpad") then
      debugger.scratchpad = nil
      return
    end
  end

  local scratchpadEditor = editor
  for _, numberStyle in ipairs(scratchpadEditor.spec.isnumber) do
    scratchpadEditor:StyleSetUnderline(numberStyle, true)
  end
  debugger.scratchpad.margin = scratchpadEditor:GetMarginWidth(0) +
    scratchpadEditor:GetMarginWidth(1) + scratchpadEditor:GetMarginWidth(2)

  scratchpadEditor:Connect(wxstc.wxEVT_STC_MODIFIED, function(event)
    local evtype = event:GetModificationType()
    if (bit.band(evtype,wxstc.wxSTC_MOD_INSERTTEXT) ~= 0 or
        bit.band(evtype,wxstc.wxSTC_MOD_DELETETEXT) ~= 0 or
        bit.band(evtype,wxstc.wxSTC_PERFORMED_UNDO) ~= 0 or
        bit.band(evtype,wxstc.wxSTC_PERFORMED_REDO) ~= 0) then
      debugger.scratchpad.updated = true
      debugger.scratchpad.editor = scratchpadEditor
    end
    event:Skip()
  end)

  scratchpadEditor:Connect(wx.wxEVT_LEFT_DOWN, function(event)
    local scratchpad = debugger.scratchpad

    local point = event:GetPosition()
    local pos = scratchpadEditor:PositionFromPoint(point)
    local isnumber = scratchpadEditor.spec.isnumber

    -- are we over a number in the scratchpad? if not, it's not our event
    if not (scratchpad and isnumber[bit.band(scratchpadEditor:GetStyleAt(pos),ide.STYLEMASK)]) then
      event:Skip()
      return
    end

    -- find start position and length of the number
    local text = scratchpadEditor:GetTextDyn()

    local nstart = pos
    while nstart >= 0 and isnumber[bit.band(scratchpadEditor:GetStyleAt(nstart),ide.STYLEMASK)] do
      nstart = nstart - 1
    end

    local nend = pos
    while nend < string.len(text) and isnumber[bit.band(scratchpadEditor:GetStyleAt(nend),ide.STYLEMASK)] do
      nend = nend + 1
    end

    -- check if there is minus sign right before the number and include it
    if nstart >= 0 and scratchpadEditor:GetTextRangeDyn(nstart,nstart+1) == '-' then 
      nstart = nstart - 1
    end
    scratchpad.start = nstart + 1
    scratchpad.length = nend - nstart - 1
    scratchpad.origin = scratchpadEditor:GetTextRangeDyn(nstart+1,nend)
    if tonumber(scratchpad.origin) then
      scratchpad.point = point
      scratchpadEditor:BeginUndoAction()
      scratchpadEditor:CaptureMouse()
    end
  end)

  scratchpadEditor:Connect(wx.wxEVT_LEFT_UP, function(event)
    if debugger.scratchpad and debugger.scratchpad.point then
      debugger.scratchpad.point = nil
      scratchpadEditor:EndUndoAction()
      scratchpadEditor:ReleaseMouse()
      wx.wxSetCursor(wx.wxNullCursor) -- restore cursor
    else event:Skip() end
  end)

  scratchpadEditor:Connect(wx.wxEVT_MOTION, function(event)
    local point = event:GetPosition()
    local pos = scratchpadEditor:PositionFromPoint(point)
    local scratchpad = debugger.scratchpad
    local ipoint = scratchpad and scratchpad.point

    -- record the fact that we are over a number or dragging slider
    scratchpad.over = scratchpad and
      (ipoint ~= nil or scratchpadEditor.spec.isnumber[bit.band(scratchpadEditor:GetStyleAt(pos),ide.STYLEMASK)])

    if ipoint then
      local startpos = scratchpad.start
      local endpos = scratchpad.start+scratchpad.length

      -- calculate difference in point position
      local dx = point.x - ipoint.x

      -- calculate the number of decimal digits after the decimal point
      local origin = scratchpad.origin
      local decdigits = #(origin:match('%.(%d+)') or '')

      -- calculate new value
      local value = tonumber(origin) + dx * 10^-decdigits

      -- convert new value back to string to check the number of decimal points
      -- this is needed because the rate of change is determined by the
      -- current value. For example, for number 1, the next value is 2,
      -- but for number 1.1, the next is 1.2 and for 1.01 it is 1.02.
      -- But if 1.01 becomes 1.00, the both zeros after the decimal point
      -- need to be preserved to keep the increment ratio the same when
      -- the user wants to release the slider and start again.
      origin = tostring(value)
      local newdigits = #(origin:match('%.(%d+)') or '')
      if decdigits ~= newdigits then
        origin = origin .. (origin:find('%.') and '' or '.') .. ("0"):rep(decdigits-newdigits)
      end

      -- update length
      scratchpad.length = #origin

      -- update the value in the document
      scratchpadEditor:SetTargetStart(startpos)
      scratchpadEditor:SetTargetEnd(endpos)
      scratchpadEditor:ReplaceTarget(origin)
    else event:Skip() end
  end)

  scratchpadEditor:Connect(wx.wxEVT_SET_CURSOR, function(event)
    if (debugger.scratchpad and debugger.scratchpad.over) then
      event:SetCursor(wx.wxCursor(wx.wxCURSOR_SIZEWE))
    elseif debugger.scratchpad and ide.osname == 'Unix' then
      -- restore the cursor manually on Linux since event:Skip() doesn't reset it
      local ibeam = event:GetX() > debugger.scratchpad.margin
      event:SetCursor(wx.wxCursor(ibeam and wx.wxCURSOR_IBEAM or wx.wxCURSOR_RIGHT_ARROW))
    else event:Skip() end
  end)

  return true
end

function debugger:ScratchpadOff()
  local debugger = self
  if not debugger.scratchpad then return end

  for scratchpadEditor in pairs(debugger.scratchpad.editors) do
    for _, numberStyle in ipairs(scratchpadEditor.spec.isnumber) do
      scratchpadEditor:StyleSetUnderline(numberStyle, false)
    end
    scratchpadEditor:Disconnect(wx.wxID_ANY, wx.wxID_ANY, wxstc.wxEVT_STC_MODIFIED)
    scratchpadEditor:Disconnect(wx.wxID_ANY, wx.wxID_ANY, wx.wxEVT_MOTION)
    scratchpadEditor:Disconnect(wx.wxID_ANY, wx.wxID_ANY, wx.wxEVT_LEFT_DOWN)
    scratchpadEditor:Disconnect(wx.wxID_ANY, wx.wxID_ANY, wx.wxEVT_LEFT_UP)
    scratchpadEditor:Disconnect(wx.wxID_ANY, wx.wxID_ANY, wx.wxEVT_SET_CURSOR)
  end

  wx.wxSetCursor(wx.wxNullCursor) -- restore cursor

  debugger.scratchpad = nil
  debugger:terminate()

  -- disable menu if it is still enabled
  -- (as this may be called when the debugger is being shut down)
  local menuBar = ide.frame.menuBar
  if menuBar:IsChecked(ID_RUNNOW) then menuBar:Check(ID_RUNNOW, false) end

  return true
end

debugger = ide:SetDebugger(setmetatable({}, {__index = protodeb}))

ide:AddPackage('core.debugger', {
    onEditorMarkerUpdate = function(self, editor, marker, line, value)
      if marker ~= BREAKPOINT_MARKER then return end

      local debugger = ide:GetDebugger()
      if value == false then
        -- if there is pending "run-to-cursor" call at this location, remove it
        local ed, ln = unpack(debugger.runtocursor or {})
        local same = ed and ln and ed:GetId() == editor:GetId() and ln == line
        if same then debugger.runtocursor = nil end
      elseif editor:IsLineEmpty(line-1) then
        return false -- don't set marker here
      end

      return debugger:breakpointToggle(editor, line, value)
    end,
  })
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local editorID = 100 -- window id to create editor pages with, incremented for new editors

local openDocuments = ide.openDocuments
local notebook = ide.frame.notebook
local edcfg = ide.config.editor
local styles = ide.config.styles
local unpack = table.unpack or unpack
local q = EscapeMagic

local margin = { LINENUMBER = 0, MARKER = 1, FOLD = 2 }
local linenumlen = 4 + 0.5
local foldtypes = {
  [0] = { wxstc.wxSTC_MARKNUM_FOLDEROPEN, wxstc.wxSTC_MARKNUM_FOLDER,
    wxstc.wxSTC_MARKNUM_FOLDERSUB, wxstc.wxSTC_MARKNUM_FOLDERTAIL, wxstc.wxSTC_MARKNUM_FOLDEREND,
    wxstc.wxSTC_MARKNUM_FOLDEROPENMID, wxstc.wxSTC_MARKNUM_FOLDERMIDTAIL,
  },
  box = { wxstc.wxSTC_MARK_BOXMINUS, wxstc.wxSTC_MARK_BOXPLUS,
    wxstc.wxSTC_MARK_VLINE, wxstc.wxSTC_MARK_LCORNER, wxstc.wxSTC_MARK_BOXPLUSCONNECTED,
    wxstc.wxSTC_MARK_BOXMINUSCONNECTED, wxstc.wxSTC_MARK_TCORNER,
  },
  circle = { wxstc.wxSTC_MARK_CIRCLEMINUS, wxstc.wxSTC_MARK_CIRCLEPLUS,
    wxstc.wxSTC_MARK_VLINE, wxstc.wxSTC_MARK_LCORNERCURVE, wxstc.wxSTC_MARK_CIRCLEPLUSCONNECTED,
    wxstc.wxSTC_MARK_CIRCLEMINUSCONNECTED, wxstc.wxSTC_MARK_TCORNERCURVE,
  },
  plus = { wxstc.wxSTC_MARK_MINUS, wxstc.wxSTC_MARK_PLUS },
  arrow = { wxstc.wxSTC_MARK_ARROWDOWN, wxstc.wxSTC_MARK_ARROW },
}

-- ----------------------------------------------------------------------------
-- Update the statusbar text of the frame using the given editor.
-- Only update if the text has changed.
local statusTextTable = { "OVR?", "R/O?", "Cursor Pos" }

local function updateStatusText(editor)
  local texts = { "", "", "" }
  if ide.frame and editor then
    local pos = editor:GetCurrentPos()
    local selected = #editor:GetSelectedText()
    local selections = ide.wxver >= "2.9.5" and editor:GetSelections() or 1

    texts = {
      iff(editor:GetOvertype(), TR("OVR"), TR("INS")),
      iff(editor:GetReadOnly(), TR("R/O"), TR("R/W")),
      table.concat({
        TR("Ln: %d"):format(editor:LineFromPosition(pos) + 1),
        TR("Col: %d"):format(editor:GetColumn(pos) + 1),
        selected > 0 and TR("Sel: %d/%d"):format(selected, selections) or "",
      }, ' ')}
  end

  if ide.frame then
    for n in ipairs(texts) do
      if (texts[n] ~= statusTextTable[n]) then
        ide:SetStatus(texts[n], n)
        statusTextTable[n] = texts[n]
      end
    end
  end
end

local function updateBraceMatch(editor)
  local pos = editor:GetCurrentPos()
  local posp = pos > 0 and pos-1
  local char = editor:GetCharAt(pos)
  local charp = posp and editor:GetCharAt(posp)
  local match = { [string.byte("<")] = true,
    [string.byte(">")] = true,
    [string.byte("(")] = true,
    [string.byte(")")] = true,
    [string.byte("{")] = true,
    [string.byte("}")] = true,
    [string.byte("[")] = true,
    [string.byte("]")] = true,
  }

  pos = (match[char] and pos) or (charp and match[charp] and posp)

  if (pos) then
    -- don't match brackets in markup comments
    local style = bit.band(editor:GetStyleAt(pos), ide.STYLEMASK)
    if (MarkupIsSpecial and MarkupIsSpecial(style)
      or editor.spec.iscomment[style]) then return end

    local pos2 = editor:BraceMatch(pos)
    if (pos2 == wxstc.wxSTC_INVALID_POSITION) then
      editor:BraceBadLight(pos)
    else
      editor:BraceHighlight(pos,pos2)
    end
    editor.matchon = true
  elseif(editor.matchon) then
    editor:BraceBadLight(wxstc.wxSTC_INVALID_POSITION)
    editor:BraceHighlight(wxstc.wxSTC_INVALID_POSITION,-1)
    editor.matchon = false
  end
end

-- Check if file is altered, show dialog to reload it
local function isFileAlteredOnDisk(editor)
  if not editor then return end

  local id = editor:GetId()
  if openDocuments[id] then
    local filePath = openDocuments[id].filePath
    local fileName = openDocuments[id].fileName
    local oldModTime = openDocuments[id].modTime

    if filePath and (string.len(filePath) > 0) and oldModTime and oldModTime:IsValid() then
      local modTime = GetFileModTime(filePath)
      if modTime == nil then
        openDocuments[id].modTime = nil
        wx.wxMessageBox(
          TR("File '%s' no longer exists."):format(fileName),
          ide:GetProperty("editormessage"),
          wx.wxOK + wx.wxCENTRE, ide.frame)
      elseif not editor:GetReadOnly() and modTime:IsValid() and oldModTime:IsEarlierThan(modTime) then
        local ret = (edcfg.autoreload and (not ide:GetDocument(editor):IsModified()) and wx.wxYES)
          or wx.wxMessageBox(
            TR("File '%s' has been modified on disk."):format(fileName)
            .."\n"..TR("Do you want to reload it?"),
            ide:GetProperty("editormessage"),
            wx.wxYES_NO + wx.wxCENTRE, ide.frame)

        if ret ~= wx.wxYES or ReLoadFile(filePath, editor, true) then
          openDocuments[id].modTime = GetFileModTime(filePath)
        end
      end
    end
  end
end

local function navigateToPosition(editor, fromPosition, toPosition, length)
  table.insert(editor.jumpstack, fromPosition)
  editor:GotoPosEnforcePolicy(toPosition)
  if length then
    editor:SetAnchor(toPosition + length)
  end
end

local function navigateBack(editor)
  if #editor.jumpstack == 0 then return end
  local pos = table.remove(editor.jumpstack)
  editor:GotoPosEnforcePolicy(pos)
  return true
end

-- ----------------------------------------------------------------------------
-- Get/Set notebook editor page, use nil for current page, returns nil if none
function GetEditor(selection)
  if selection == nil then
    selection = notebook:GetSelection()
  end
  local editor
  if (selection >= 0) and (selection < notebook:GetPageCount())
    and (notebook:GetPage(selection):GetClassInfo():GetClassName()=="wxStyledTextCtrl") then
    editor = notebook:GetPage(selection):DynamicCast("wxStyledTextCtrl")
  end
  return editor
end

-- init new notebook page selection, use nil for current page
function SetEditorSelection(selection)
  local editor = GetEditor(selection)
  updateStatusText(editor) -- update even if nil
  ide.frame:SetTitle(ExpandPlaceholders(ide.config.format.apptitle))

  if editor then
    editor:SetFocus()
    editor:SetSTCFocus(true)
    -- when the active editor is changed while the focus is away from the application
    -- (as happens on OSX when the editor is selected from the command bar)
    -- the focus stays on wxAuiToolBar component, so need to explicitly switch it.
    if ide.osname == "Macintosh" and ide.infocus then ide.infocus = editor end

    local id = editor:GetId()
    FileTreeMarkSelected(openDocuments[id] and openDocuments[id].filePath or '')
    AddToFileHistory(openDocuments[id] and openDocuments[id].filePath)
  else
    FileTreeMarkSelected('')
  end

  SetAutoRecoveryMark()
end

function GetEditorFileAndCurInfo(nochecksave)
  local editor = GetEditor()
  if (not (editor and (nochecksave or SaveIfModified(editor)))) then
    return
  end

  local id = editor:GetId()
  local filepath = openDocuments[id].filePath
  if not filepath then return end

  local fn = wx.wxFileName(filepath)
  fn:Normalize()

  local info = {}
  info.pos = editor:GetCurrentPos()
  info.line = editor:GetCurrentLine()
  info.sel = editor:GetSelectedText()
  info.sel = info.sel and info.sel:len() > 0 and info.sel or nil
  info.selword = info.sel and info.sel:match("([^a-zA-Z_0-9]+)") or info.sel

  return fn,info
end

function EditorAutoComplete(editor)
  if not (editor and editor.spec) then return end

  local pos = editor:GetCurrentPos()
  -- don't do auto-complete in comments or strings.
  -- the current position and the previous one have default style (0),
  -- so we need to check two positions back.
  local style = pos >= 2 and bit.band(editor:GetStyleAt(pos-2),ide.STYLEMASK) or 0
  if editor.spec.iscomment[style]
  or editor.spec.isstring[style]
  or (MarkupIsAny and MarkupIsAny(style)) -- markup in comments
  then return end

  -- retrieve the current line and get a string to the current cursor position in the line
  local line = editor:GetCurrentLine()
  local linetx = editor:GetLineDyn(line)
  local linestart = editor:PositionFromLine(line)
  local localpos = pos-linestart

  local lt = linetx:sub(1,localpos)
  lt = lt:gsub("%s*(["..editor.spec.sep.."])%s*", "%1")
  -- strip closed brace scopes
  lt = lt:gsub("%b()","")
  lt = lt:gsub("%b{}","")
  lt = lt:gsub("%b[]",".0")
  -- remove everything that can't be auto-completed
  lt = lt:match("[%w_"..q(editor.spec.sep).."]*$")

  -- if there is nothing to auto-complete for, then don't show the list
  if lt:find("^["..q(editor.spec.sep).."]*$") then return end

  -- know now which string is to be completed
  local userList = CreateAutoCompList(editor, lt, pos)

  -- don't show if what's typed so far matches one of the options
  local right = linetx:sub(localpos+1,#linetx):match("^([%a_]+[%w_]*)")
  local left = lt:match("[%w_]*$") -- extract one word on the left (without separators)
  local compmatch = {
    left = "( ?)%f[%w_]"..left.."%f[^%w_]( ?)",
    leftright = "( ?)%f[%w_]"..left..(right or "").."%f[^%w_]( ?)",
  }
  -- if the multiple selection is active, then remove the match from the `userList`,
  -- as it's going to match a (possibly earlier) copy of the same value
  local selections = ide.wxver >= "2.9.5" and editor:GetSelections() or 1
  if userList and selections > 1 then
    for _, m in pairs(compmatch) do
      -- replace with a space only when there are spaces on both sides
      userList = userList:gsub(m, function(s1, s2) return #(s1..s2) == 2 and " " or "" end)
    end
  end
  if userList and #userList > 0
  -- don't show autocomplete if there is a full match on the list of autocomplete options
  and not (userList:find(compmatch.left) or userList:find(compmatch.leftright)) then
    editor:UserListShow(1, userList)
  elseif editor:AutoCompActive() then
    editor:AutoCompCancel()
  end
end

local ident = "([a-zA-Z_][a-zA-Z_0-9%.%:]*)"
local function getValAtPosition(editor, pos)
  local line = editor:LineFromPosition(pos)
  local linetx = editor:GetLineDyn(line)
  local linestart = editor:PositionFromLine(line)
  local localpos = pos-linestart

  local selected = editor:GetSelectionStart() ~= editor:GetSelectionEnd()
    and pos >= editor:GetSelectionStart() and pos <= editor:GetSelectionEnd()

  -- check if we have a selected text or an identifier.
  -- for an identifier, check fragments on the left and on the right.
  -- this is to match 'io' in 'i^o.print' and 'io.print' in 'io.pr^int'.
  -- remove square brackets to make tbl[index].x show proper values.
  local start = linetx:sub(1,localpos)
    :gsub("%b[]", function(s) return ("."):rep(#s) end)
    :find(ident.."$")

  local right, funccall = linetx:sub(localpos+1,#linetx):match("^([a-zA-Z_0-9]*)%s*(['\"{%(]?)")
  local var = selected
    -- GetSelectedText() returns concatenated text when multiple instances
    -- are selected, so get the selected text based on start/end
    and editor:GetTextRangeDyn(editor:GetSelectionStart(), editor:GetSelectionEnd())
    or (start and linetx:sub(start,localpos):gsub(":",".")..right or nil)

  -- since this function can be called in different contexts, we need
  -- to detect function call of different types:
  -- 1. foo.b^ar(... -- the cursor (pos) is on the function name
  -- 2. foo.bar(..^. -- the cursor (pos) is on the parameter list
  -- "var" has value for #1 and the following fragment checks for #2

  -- check if the style is the right one; this is to ignore
  -- comments, strings, numbers (to avoid '1 = 1'), keywords, and such
  local goodpos = true
  if start and not selected then
    local style = bit.band(editor:GetStyleAt(linestart+start),ide.STYLEMASK)
    if (MarkupIsAny and MarkupIsAny(style)) -- markup in comments
    or editor.spec.iscomment[style]
    or editor.spec.isstring[style]
    or editor.spec.isnumber[style]
    or editor.spec.iskeyword[style] then
      goodpos = false
    end
  end

  local linetxtopos = linetx:sub(1,localpos)
  funccall = (#funccall > 0) and goodpos and var
    or (linetxtopos..")"):match(ident .. "%s*%b()$")
    or (linetxtopos.."}"):match(ident .. "%s*%b{}$")
    or (linetxtopos.."'"):match(ident .. "%s*'[^']*'$")
    or (linetxtopos..'"'):match(ident .. '%s*"[^"]*"$')
    or nil

  -- don't do anything for strings or comments or numbers
  if not goodpos then return nil, funccall end

  return var, funccall
end

local function formatUpToX(s)
  local x = math.max(20, ide.config.acandtip.width)
  local splitstr = "([ \t]*)(%S*)([ \t]*)(\n?)"
  local t = {""}
  for prefix, word, suffix, newline in s:gmatch(splitstr) do
    if #(t[#t]) + #prefix + #word > x and #t > 0 then
      table.insert(t, word..suffix)
    else
      t[#t] = t[#t]..prefix..word..suffix
    end
    if #newline > 0 then table.insert(t, "") end
  end
  return table.concat(t, "\n")
end

local function callTipFitAndShow(editor, pos, tip)
  local point = editor:PointFromPosition(pos)
  local sline = editor:LineFromPosition(pos)
  local height = editor:TextHeight(sline)
  local maxlines = math.max(1, math.floor(
    math.max(editor:GetSize():GetHeight()-point:GetY()-height, point:GetY())/height-1
  ))
  -- cut the tip to not exceed the number of maxlines.
  -- move the position to the left if needed to fit.
  -- find the longest line in terms of width in pixels.
  local maxwidth = 0
  local lines = {}
  for line in formatUpToX(tip):gmatch("[^\n]*\n?") do
    local width = editor:TextWidth(wxstc.wxSTC_STYLE_DEFAULT, line)
    if width > maxwidth then maxwidth = width end
    table.insert(lines, line)
    if #lines >= maxlines then
      lines[#lines] = lines[#lines]:gsub("%s*\n$","")..'...'
      break
    end
  end
  tip = table.concat(lines, '')

  local startpos = editor:PositionFromLine(sline)
  local afterwidth = editor:GetSize():GetWidth()-point:GetX()
  if maxwidth > afterwidth then
    local charwidth = editor:TextWidth(wxstc.wxSTC_STYLE_DEFAULT, 'A')
    pos = math.max(startpos, pos - math.floor((maxwidth - afterwidth) / charwidth))
  end

  editor:CallTipShow(pos, tip)
end

function EditorCallTip(editor, pos, x, y)
  -- don't show anything if the calltip/auto-complete is active;
  -- this may happen after typing function name, while the mouse is over
  -- a different function or when auto-complete is on for a parameter.
  if editor:CallTipActive() or editor:AutoCompActive() then return end

  -- don't activate if the window itself is not active (in the background)
  if not ide.frame:IsActive() then return end

  local var, funccall = editor:ValueFromPosition(pos)
  -- if this is a value type rather than a function/method call, then use
  -- full match to avoid calltip about coroutine.status for "status" vars
  local tip = GetTipInfo(editor, funccall or var, false, not funccall)
  local limit = ide.config.acandtip.maxlength
  local debugger = ide:GetDebugger()
  if debugger and debugger:IsConnected() then
    if var then
      debugger:EvalAsync(var, function(val, err)
        -- val == `nil` if there is any error
        val = val ~= nil and (var.." = "..val) or err
        if #val > limit then val = val:sub(1, limit-3).."..." end
        -- check if the mouse position is specified and the mouse has moved,
        -- then don't show the tooltip as it's already too late for it.
        if x and y then
          local mpos = wx.wxGetMousePosition()
          if mpos.x ~= x or mpos.y ~= y then return end
        end
        if PackageEventHandle("onEditorCallTip", editor, val, funccall or var, true) ~= false then
          callTipFitAndShow(editor, pos, val)
        end
      end, debugger:GetDataOptions({maxlevel = false}))
    end
  elseif tip then
    local oncalltip = PackageEventHandle("onEditorCallTip", editor, tip, funccall or var, false)
    -- only shorten if shown on mouse-over. Use shortcut to get full info.
    local showtooltip = ide.frame.menuBar:FindItem(ID_SHOWTOOLTIP)
    local suffix = "...\n"
        ..TR("Use '%s' to see full description."):format(showtooltip:GetLabel())
    if x and y and #tip > limit then
      tip = tip:sub(1, limit-#suffix):gsub("%W*%w*$","")..suffix
    end
    if oncalltip ~= false then callTipFitAndShow(editor, pos, tip) end
  end
end

-- Indicator handling for functions and local/global variables
local indicator = {
  FNCALL = ide:GetIndicator("core.fncall"),
  LOCAL = ide:GetIndicator("core.varlocal"),
  GLOBAL = ide:GetIndicator("core.varglobal"),
  SELF = ide:GetIndicator("core.varself"),
  MASKING = ide:GetIndicator("core.varmasking"),
  MASKED = ide:GetIndicator("core.varmasked"),
}

function IndicateFunctionsOnly(editor, lines, linee)
  local sindic = styles.indicator
  if not (edcfg.showfncall and editor.spec and editor.spec.isfncall)
  or not (sindic and sindic.fncall and sindic.fncall.st ~= wxstc.wxSTC_INDIC_HIDDEN) then return end

  local lines = lines or 0
  local linee = linee or editor:GetLineCount()-1

  if (lines < 0) then return end

  local isfncall = editor.spec.isfncall
  local isinvalid = {}
  for i,v in pairs(editor.spec.iscomment) do isinvalid[i] = v end
  for i,v in pairs(editor.spec.iskeyword) do isinvalid[i] = v end
  for i,v in pairs(editor.spec.isstring) do isinvalid[i] = v end

  editor:SetIndicatorCurrent(indicator.FNCALL)
  for line=lines,linee do
    local tx = editor:GetLineDyn(line)
    local ls = editor:PositionFromLine(line)
    editor:IndicatorClearRange(ls, #tx)

    local from = 1
    local off = -1
    while from do
      tx = from==1 and tx or string.sub(tx,from)

      local f,t,w = isfncall(tx)

      if (f) then
        local p = ls+f+off
        local s = bit.band(editor:GetStyleAt(p),ide.STYLEMASK)
        if not isinvalid[s] then editor:IndicatorFillRange(p, #w) end
        off = off + t
      end
      from = t and (t+1)
    end
  end
end

local delayed = {}

function IndicateIfNeeded()
  local editor = GetEditor()
  -- do the current one first
  if delayed[editor] then return IndicateAll(editor) end
  for ed in pairs(delayed) do return IndicateAll(ed) end
end

-- find all instances of a symbol at pos
-- return table with [0] as the definition position (if local)
local function indicateFindInstances(editor, name, pos)
  local tokens = editor:GetTokenList()
  local instances = {{[-1] = 1}}
  local this
  for _, token in ipairs(tokens) do
    local op = token[1]

    if op == 'EndScope' then -- EndScope has "new" level, so need +1
      if this and token.fpos > pos and this == token.at+1 then break end

      if #instances > 1 and instances[#instances][-1] == token.at+1 then
        table.remove(instances)
      end
    elseif token.name == name then
      if op == 'Id' then
        table.insert(instances[#instances], token.fpos)
      elseif op:find("^Var") then
        if this and this == token.at then break end

        -- if new Var is defined at the same level, replace the current frame;
        -- if not, add a new one; skip implicit definition of "self" variable.
        instances[#instances + (token.at > instances[#instances][-1] and 1 or 0)]
          = {[0] = (not token.self and token.fpos or nil), [-1] = token.at}
      end
      if token.fpos <= pos and pos <= token.fpos+#name then this = instances[#instances][-1] end
    end
  end
  instances[#instances][-1] = nil -- remove the current level
  -- only return the list if "this" instance has been found;
  -- this is to avoid reporting (improper) instances when checking for
  -- comments, strings, table fields, etc.
  return this and instances[#instances] or {}
end

function IndicateAll(editor, lines)
  if not ide.config.autoanalyzer then return end

  local d = delayed[editor]
  delayed[editor] = nil -- assume this can be finished for now

  -- this function can be called for an editor tab that is already closed
  -- when there are still some pending events for it, so handle it.
  if not ide:IsValidCtrl(editor) then return end

  -- if marksymbols is not set in the spec, nothing else to do
  if not (editor.spec and editor.spec.marksymbols) then return end

  local indic = styles.indicator or {}

  local pos, vars = d and d[1] or 1, d and d[2] or nil
  local start = lines and editor:PositionFromLine(lines)+1 or nil
  if d and start and pos >= start then
    -- ignore delayed processing as the change is earlier in the text
    pos, vars = 1, nil
  end

  local tokens = editor:GetTokenList()

  if start then -- if the range is specified
    local curindic = editor:GetIndicatorCurrent()
    editor:SetIndicatorCurrent(indicator.MASKED)
    for n = #tokens, 1, -1 do
      local token = tokens[n]
      -- find the last token before the range
      if not token.nobreak and token.name and token.fpos+#token.name < start then
        pos, vars = token.fpos+#token.name, token.context
        break
      end
      -- unmask all variables from the rest of the list
      if token[1] == 'Masked' then
        editor:IndicatorClearRange(token.fpos-1, #token.name)
      end
      -- trim the list as it will be re-generated
      table.remove(tokens, n)
    end

    -- Clear masked indicators from the current position to the end as these
    -- will be re-calculated and re-applied based on masking variables.
    -- This step is needed as some positions could have shifted after updates.
    editor:IndicatorClearRange(pos-1, editor:GetLength()-pos+1)

    editor:SetIndicatorCurrent(curindic)

    -- need to cleanup vars as they may include variables from later
    -- fragments (because the cut-point was arbitrary). Also need
    -- to clean variables in other scopes, hence getmetatable use.
    local vars = vars
    while vars do
      for name, var in pairs(vars) do
        -- remove all variables that are created later than the current pos
        -- skip all non-variable elements from the vars table
        if type(name) == 'string' then
          while type(var) == 'table' and var.fpos and (var.fpos > pos) do
            var = var.masked -- restored a masked var
            vars[name] = var
          end
        end
      end
      vars = getmetatable(vars) and getmetatable(vars).__index
    end
  else
    if pos == 1 then -- if not continuing, then trim the list
      tokens = editor:ResetTokenList()
    end
  end

  local cleared = {}
  for _, indic in ipairs {indicator.FNCALL, indicator.LOCAL, indicator.GLOBAL, indicator.MASKING, indicator.SELF} do
    cleared[indic] = pos
  end

  local function IndicateOne(indic, pos, length)
    editor:SetIndicatorCurrent(indic)
    editor:IndicatorClearRange(cleared[indic]-1, pos-cleared[indic])
    editor:IndicatorFillRange(pos-1, length)
    cleared[indic] = pos+length
  end

  local s = TimeGet()
  local canwork = start and 0.010 or 0.100 -- use shorter interval when typing
  local f = editor.spec.marksymbols(editor:GetTextDyn(), pos, vars)
  while true do
    local op, name, lineinfo, vars, at, nobreak = f()
    if not op then break end
    local var = vars and vars[name]
    local token = {op, name=name, fpos=lineinfo, at=at, context=vars,
      self = (op == 'VarSelf') or nil, nobreak=nobreak}
    if op == 'Function' then
      vars['function'] = (vars['function'] or 0) + 1
    end
    if op == 'FunctionCall' then
      if indic.fncall and edcfg.showfncall then
        IndicateOne(indicator.FNCALL, lineinfo, #name)
      end
    elseif op ~= 'VarNext' and op ~= 'VarInside' and op ~= 'Statement' and op ~= 'String' then
      table.insert(tokens, token)
    end

    -- indicate local/global variables
    if op == 'Id'
    and (var and indic.varlocal or not var and indic.varglobal) then
      IndicateOne(var and (var.self and indicator.SELF or indicator.LOCAL) or indicator.GLOBAL, lineinfo, #name)
    end

    -- indicate masked values at the same level
    if op == 'Var' and var and (var.masked and at == var.masked.at) then
      local fpos = var.masked.fpos
      -- indicate masked if it's not implicit self
      if indic.varmasked and not var.masked.self then
        editor:SetIndicatorCurrent(indicator.MASKED)
        editor:IndicatorFillRange(fpos-1, #name)
        table.insert(tokens, {"Masked", name=name, fpos=fpos, nobreak=nobreak})
      end

      if indic.varmasking then IndicateOne(indicator.MASKING, lineinfo, #name) end
    end
    -- in some rare cases `nobreak` may be a number indicating a desired
    -- position from which to start in case of a break
    if lineinfo and nobreak ~= true and (op == 'Statement' or op == 'String') and TimeGet()-s > canwork then
      delayed[editor] = {tonumber(nobreak) or lineinfo, vars}
      break
    end
  end

  -- clear indicators till the end of processed fragment
  pos = delayed[editor] and delayed[editor][1] or editor:GetLength()+1

  -- don't clear "masked" indicators as those can be set out of order (so
  -- last updated fragment is not always the last in terms of its position);
  -- these indicators should be up-to-date to the end of the code fragment.
  local funconly = ide.config.editor.showfncall and editor.spec.isfncall
  for _, indic in ipairs {indicator.FNCALL, indicator.LOCAL, indicator.GLOBAL, indicator.MASKING} do
    -- don't clear "funccall" indicators as those can be set based on
    -- IndicateFunctionsOnly processing, which is dealt with separately
    if indic ~= indicator.FNCALL or not funconly then IndicateOne(indic, pos, 0) end
  end

  local needmore = delayed[editor] ~= nil
  if ide.config.outlineinactivity then
    if needmore then ide.timers.outline:Stop()
    else ide.timers.outline:Start(ide.config.outlineinactivity*1000, wx.wxTIMER_ONE_SHOT)
    end
  end
  return needmore -- request more events if still need to work
end

-- ----------------------------------------------------------------------------
-- Create an editor
function CreateEditor(bare)
  local editor = ide:CreateStyledTextCtrl(notebook, editorID,
    wx.wxDefaultPosition, wx.wxSize(0, 0), wx.wxBORDER_NONE)

  editorID = editorID + 1 -- increment so they're always unique

  editor.matchon = false
  editor.assignscache = false
  editor.bom = false
  editor.updated = 0
  editor.jumpstack = {}
  editor.ctrlcache = {}
  editor.tokenlist = {}
  editor.onidle = {}
  -- populate cache with Ctrl-<letter> combinations for workaround on Linux
  -- http://wxwidgets.10942.n7.nabble.com/Menu-shortcuts-inconsistentcy-issue-td85065.html
  for id, shortcut in pairs(ide.config.keymap) do
    if shortcut:match('%f[%w]Ctrl[-+]') then
      local mask = (wx.wxMOD_CONTROL
        + (shortcut:match('%f[%w]Alt[-+]') and wx.wxMOD_ALT or 0)
        + (shortcut:match('%f[%w]Shift[-+]') and wx.wxMOD_SHIFT or 0)
      )
      local key = shortcut:match('[-+](.)$')
      if key then editor.ctrlcache[key:byte()..mask] = id end
    end
  end

  -- populate editor keymap with configured combinations
  for _, map in pairs(edcfg.keymap or {}) do
    local key, mod, cmd, osname = unpack(map)
    if not osname or osname == ide.osname then
      if cmd then
        editor:CmdKeyAssign(key, mod, cmd)
      else
        editor:CmdKeyClear(key, mod)
      end
    end
  end

  editor:SetBufferedDraw(not ide.config.hidpi and true or false)
  editor:StyleClearAll()

  editor:SetFont(ide.font.eNormal)
  editor:StyleSetFont(wxstc.wxSTC_STYLE_DEFAULT, ide.font.eNormal)

  editor:SetTabWidth(tonumber(edcfg.tabwidth) or 2)
  editor:SetIndent(tonumber(edcfg.tabwidth) or 2)
  editor:SetUseTabs(edcfg.usetabs and true or false)
  editor:SetIndentationGuides(tonumber(edcfg.indentguide) or (edcfg.indentguide and true or false))
  editor:SetViewWhiteSpace(tonumber(edcfg.whitespace) or (edcfg.whitespace and true or false))

  if (edcfg.usewrap) then
    editor:SetWrapMode(edcfg.wrapmode)
    editor:SetWrapStartIndent(0)
    if ide.wxver >= "2.9.5" then
      if edcfg.wrapflags then
        editor:SetWrapVisualFlags(tonumber(edcfg.wrapflags) or wxstc.wxSTC_WRAPVISUALFLAG_NONE)
      end
      if edcfg.wrapstartindent then
        editor:SetWrapStartIndent(tonumber(edcfg.wrapstartindent) or 0)
      end
      if edcfg.wrapindentmode then
        editor:SetWrapIndentMode(edcfg.wrapindentmode)
      end
    end
  else
    editor:SetScrollWidth(100) -- set default width
    editor:SetScrollWidthTracking(1) -- enable width auto-adjustment
  end

  if edcfg.defaulteol == wxstc.wxSTC_EOL_CRLF
  or edcfg.defaulteol == wxstc.wxSTC_EOL_LF then
    editor:SetEOLMode(edcfg.defaulteol)
  -- else: keep wxStyledTextCtrl default behavior (CRLF on Windows, LF on Unix)
  end

  editor:SetCaretLineVisible(edcfg.caretline and true or false)

  editor:SetVisiblePolicy(wxstc.wxSTC_VISIBLE_STRICT, 3)

  editor:SetMarginType(margin.LINENUMBER, wxstc.wxSTC_MARGIN_NUMBER)
  editor:SetMarginMask(margin.LINENUMBER, 0)
  editor:SetMarginWidth(margin.LINENUMBER,
    edcfg.linenumber and math.floor(linenumlen * editor:TextWidth(wxstc.wxSTC_STYLE_DEFAULT, "8")) or 0)

  editor:SetMarginWidth(margin.MARKER, 18)
  editor:SetMarginType(margin.MARKER, wxstc.wxSTC_MARGIN_SYMBOL)
  editor:SetMarginMask(margin.MARKER, 0xffffffff - wxstc.wxSTC_MASK_FOLDERS)
  editor:SetMarginSensitive(margin.MARKER, true)

  editor:MarkerDefine(StylesGetMarker("currentline"))
  editor:MarkerDefine(StylesGetMarker("breakpoint"))
  editor:MarkerDefine(StylesGetMarker("bookmark"))

  if edcfg.fold then
    editor:SetMarginWidth(margin.FOLD, 18)
    editor:SetMarginType(margin.FOLD, wxstc.wxSTC_MARGIN_SYMBOL)
    editor:SetMarginMask(margin.FOLD, wxstc.wxSTC_MASK_FOLDERS)
    editor:SetMarginSensitive(margin.FOLD, true)
  end

  editor:SetFoldFlags(tonumber(edcfg.foldflags) or wxstc.wxSTC_FOLDFLAG_LINEAFTER_CONTRACTED)
  editor:SetBackSpaceUnIndents(edcfg.backspaceunindent and 1 or 0)

  if ide.wxver >= "2.9.5" then
    -- allow multiple selection and multi-cursor editing if supported
    editor:SetMultipleSelection(1)
    editor:SetAdditionalCaretsBlink(1)
    editor:SetAdditionalSelectionTyping(1)
    -- allow extra ascent/descent
    editor:SetExtraAscent(tonumber(edcfg.extraascent) or 0)
    editor:SetExtraDescent(tonumber(edcfg.extradescent) or 0)
    -- set whitespace size
    editor:SetWhitespaceSize(tonumber(edcfg.whitespacesize) or 1)
  end

  do
    local fg, bg = wx.wxWHITE, wx.wxColour(128, 128, 128)
    local foldtype = foldtypes[edcfg.foldtype] or foldtypes.box
    local foldmarkers = foldtypes[0]
    for m = 1, #foldmarkers do
      editor:MarkerDefine(foldmarkers[m], foldtype[m] or wxstc.wxSTC_MARK_EMPTY, fg, bg)
    end
    bg:delete()
  end

  if edcfg.calltipdelay and edcfg.calltipdelay > 0 then
    editor:SetMouseDwellTime(edcfg.calltipdelay)
  end

  if edcfg.edgemode ~= wxstc.wxSTC_EDGE_NONE or edcfg.edge then
    editor:SetEdgeMode(edcfg.edgemode ~= wxstc.wxSTC_EDGE_NONE and edcfg.edgemode or wxstc.wxSTC_EDGE_LINE)
    editor:SetEdgeColumn(tonumber(edcfg.edge) or 80)
  end

  editor:AutoCompSetIgnoreCase(ide.config.acandtip.ignorecase)
  if (ide.config.acandtip.strategy > 0) then
    editor:AutoCompSetAutoHide(0)
    editor:AutoCompStops([[ \n\t=-+():.,;*/!"'$%&~'#°^@?´`<>][|}{]])
  end
  if ide.config.acandtip.fillups then
    editor:AutoCompSetFillUps(ide.config.acandtip.fillups)
  end

  if ide:IsValidProperty(editor, "SetMultiPaste") then editor:SetMultiPaste(wxstc.wxSTC_MULTIPASTE_EACH) end

  function editor:GetTokenList() return self.tokenlist end
  function editor:ResetTokenList() self.tokenlist = {}; return self.tokenlist end

  function editor:SetupKeywords(...) return SetupKeywords(self, ...) end
  function editor:ValueFromPosition(pos) return getValAtPosition(self, pos) end

  function editor:MarkerGotoNext(marker)
    local value = 2^marker
    local line = editor:MarkerNext(editor:GetCurrentLine()+1, value)
    if line == wx.wxNOT_FOUND then line = editor:MarkerNext(0, value) end
    if line == wx.wxNOT_FOUND then return end
    editor:GotoLine(line)
    editor:EnsureVisibleEnforcePolicy(line)
    return line
  end
  function editor:MarkerGotoPrev(marker)
    local value = 2^marker
    local line = editor:MarkerPrevious(editor:GetCurrentLine()-1, value)
    if line == wx.wxNOT_FOUND then line = editor:MarkerPrevious(editor:GetLineCount(), value) end
    if line == wx.wxNOT_FOUND then return end
    editor:GotoLine(line)
    editor:EnsureVisibleEnforcePolicy(line)
    return line
  end
  function editor:MarkerToggle(marker, line, value)
    if type(marker) == "string" then marker = StylesGetMarker(marker) end
    assert(marker ~= nil, "Marker update requires known marker type")
    line = line or editor:GetCurrentLine()
    local isset = bit.band(editor:MarkerGet(line), 2^marker) > 0
    if value ~= nil and isset == value then return end
    if PackageEventHandle("onEditorMarkerUpdate", editor, marker, line+1, not isset) == false then return end
    if isset then
      editor:MarkerDelete(line, marker)
    else
      editor:MarkerAdd(line, marker)
    end
  end

  function editor:BookmarkToggle(...) return self:MarkerToggle("bookmark", ...) end
  function editor:BreakpointToggle(...) return self:MarkerToggle("breakpoint", ...) end

  function editor:DoWhenIdle(func) table.insert(self.onidle, func) end

  -- GotoPos should work by itself, but it doesn't (wx 2.9.5).
  -- This is likely because the editor window hasn't been refreshed yet,
  -- so its LinesOnScreen method returns 0/-1, which skews the calculations.
  -- To avoid this, the caret line is made visible at the first opportunity.
  do
    local redolater
    function editor:GotoPosDelayed(pos)
      local badtime = self:LinesOnScreen() <= 0 -- -1 on OSX, 0 on Windows
      if pos then
        if badtime then
          redolater = pos
          -- without this GotoPos the content is not scrolled correctly on
          -- Windows, but with this it's not scrolled correctly on OSX.
          if ide.osname ~= 'Macintosh' then self:GotoPos(pos) end
        else
          redolater = nil
          self:GotoPosEnforcePolicy(pos)
        end
      elseif not badtime and redolater then
        -- reset the left margin first to make sure that the position
        -- is set "from the left" to get the best content displayed.
        self:SetXOffset(0)
        self:GotoPosEnforcePolicy(redolater)
        redolater = nil
      end
    end
  end

  if bare then return editor end -- bare editor doesn't have any event handlers

  editor.ev = {}
  editor:Connect(wxstc.wxEVT_STC_MARGINCLICK,
    function (event)
      local line = editor:LineFromPosition(event:GetPosition())
      local marginno = event:GetMargin()
      if marginno == margin.MARKER then
        editor:BreakpointToggle(line)
      elseif marginno == margin.FOLD then
        local header = bit.band(editor:GetFoldLevel(line),
          wxstc.wxSTC_FOLDLEVELHEADERFLAG) == wxstc.wxSTC_FOLDLEVELHEADERFLAG
        local shift, ctrl = wx.wxGetKeyState(wx.WXK_SHIFT), wx.wxGetKeyState(wx.WXK_CONTROL)
        if shift and ctrl then
          editor:FoldSome(line)
        elseif ctrl then -- select the scope that was clicked on
          local from = header and line or editor:GetFoldParent(line)
          if from > -1 then -- only select if there is a block to select
            local to = editor:GetLastChild(from, -1)
            editor:SetSelection(editor:PositionFromLine(from), editor:PositionFromLine(to+1))
          end
        elseif header or shift then
          editor:ToggleFold(line)
        end
      end
    end)

  editor:Connect(wxstc.wxEVT_STC_MODIFIED,
    function (event)
      if (editor.assignscache and editor:GetCurrentLine() ~= editor.assignscache.line) then
        editor.assignscache = false
      end
      local evtype = event:GetModificationType()
      if bit.band(evtype, wxstc.wxSTC_MOD_CHANGEMARKER) == 0 then
        -- this event is being called on OSX too frequently, so skip these notifications
        editor.updated = TimeGet()
      end
      local pos = event:GetPosition()
      local firstLine = editor:LineFromPosition(pos)
      local inserted = bit.band(evtype, wxstc.wxSTC_MOD_INSERTTEXT) ~= 0
      local deleted = bit.band(evtype, wxstc.wxSTC_MOD_DELETETEXT) ~= 0
      if (inserted or deleted) then
        SetAutoRecoveryMark()

        local linesChanged = inserted and event:GetLinesAdded() or 0
        -- collate events if they are for the same line
        local events = #editor.ev
        if events == 0 or editor.ev[events][1] ~= firstLine then
          editor.ev[events+1] = {firstLine, linesChanged}
        elseif events > 0 and editor.ev[events][1] == firstLine then
          editor.ev[events][2] = math.max(editor.ev[events][2], linesChanged)
        end
        DynamicWordsAdd(editor, nil, firstLine, linesChanged)
      end

      local beforeInserted = bit.band(evtype,wxstc.wxSTC_MOD_BEFOREINSERT) ~= 0
      local beforeDeleted = bit.band(evtype,wxstc.wxSTC_MOD_BEFOREDELETE) ~= 0

      if (beforeInserted or beforeDeleted) then
        -- unfold the current line being changed if folded, but only if one selection
        local lastLine = editor:LineFromPosition(pos+event:GetLength())
        local selections = ide.wxver >= "2.9.5" and editor:GetSelections() or 1
        if (not editor:GetFoldExpanded(firstLine)
          or not editor:GetLineVisible(firstLine)
          or not editor:GetLineVisible(lastLine))
        and selections == 1 then
          for line = firstLine, lastLine do
            if not editor:GetLineVisible(line) then editor:ToggleFold(editor:GetFoldParent(line)) end
          end
        end
      end

      -- hide calltip/auto-complete after undo/redo/delete
      local undodelete = (wxstc.wxSTC_MOD_DELETETEXT
        + wxstc.wxSTC_PERFORMED_UNDO + wxstc.wxSTC_PERFORMED_REDO)
      if bit.band(evtype, undodelete) ~= 0 then
        editor:DoWhenIdle(function(editor)
            if editor:CallTipActive() then editor:CallTipCancel() end
            if editor:AutoCompActive() then editor:AutoCompCancel() end
          end)
      end
      
      if ide.config.acandtip.nodynwords then return end
      -- only required to track changes

      if beforeDeleted then
        local text = editor:GetTextRangeDyn(pos, pos+event:GetLength())
        local _, numlines = text:gsub("\r?\n","%1")
        DynamicWordsRem(editor,nil,firstLine, numlines)
      end
      if beforeInserted then
        DynamicWordsRem(editor,nil,firstLine, 0)
      end
    end)

  editor:Connect(wxstc.wxEVT_STC_CHARADDED,
    function (event)
      local LF = string.byte("\n") -- `CHARADDED` gets `\n` code on all platforms
      local ch = event:GetKey()
      local pos = editor:GetCurrentPos()
      local line = editor:GetCurrentLine()
      local linetx = editor:GetLineDyn(line)
      local linestart = editor:PositionFromLine(line)
      local localpos = pos-linestart
      local linetxtopos = linetx:sub(1,localpos)

      if PackageEventHandle("onEditorCharAdded", editor, event) == false then
        -- this event has already been handled
      elseif (ch == LF) then
        -- auto-indent
        if (line > 0) then
          local indent = editor:GetLineIndentation(line - 1)
          local linedone = editor:GetLineDyn(line - 1)

          -- if the indentation is 0 and the current line is not empty,
          -- but the previous line is empty, then take indentation from the
          -- current line (instead of the previous one). This may happen when
          -- CR is hit at the beginning of a line (rather than at the end).
          if indent == 0 and not linetx:match("^[\010\013]*$")
          and linedone:match("^[\010\013]*$") then
            indent = editor:GetLineIndentation(line)
          end

          local ut = editor:GetUseTabs()
          local tw = ut and editor:GetTabWidth() or editor:GetIndent()
          local style = bit.band(editor:GetStyleAt(editor:PositionFromLine(line-1)), ide.STYLEMASK)

          if edcfg.smartindent
          -- don't apply smartindent to multi-line comments or strings
          and not (editor.spec.iscomment[style]
            or editor.spec.isstring[style]
            or (MarkupIsAny and MarkupIsAny(style)))
          and editor.spec.isdecindent and editor.spec.isincindent then
            local closed, blockend = editor.spec.isdecindent(linedone)
            local opened = editor.spec.isincindent(linedone)

            -- if the current block is already indented, skip reverse indenting
            if (line > 1) and (closed > 0 or blockend > 0)
            and editor:GetLineIndentation(line-2) > indent then
              -- adjust opened first; this is needed when use ENTER after })
              if blockend == 0 then opened = opened + closed end
              closed, blockend = 0, 0
            end
            editor:SetLineIndentation(line-1, indent - tw * closed)
            indent = indent + tw * (opened - blockend)
            if indent < 0 then indent = 0 end
          end
          editor:SetLineIndentation(line, indent)

          indent = ut and (indent / tw) or indent
          editor:GotoPos(editor:GetCurrentPos()+indent)
        end

      elseif ch == ("("):byte() or ch == (","):byte() then
        if ch == (","):byte() then
          -- comma requires special handling: either it's in a list of parameters
          -- and follows an opening bracket, or it does nothing
          if linetxtopos:gsub("%b()",""):find("%(") then
            linetxtopos = linetxtopos:gsub("%b()",""):gsub("%(.+,$", "(")
          else
            linetxtopos = nil
          end
        end
        local var, funccall = editor:ValueFromPosition(pos)
        local tip = GetTipInfo(editor, funccall or var, ide.config.acandtip.shorttip)
        if tip then
          if editor:CallTipActive() then editor:CallTipCancel() end
          if PackageEventHandle("onEditorCallTip", editor, tip) ~= false then
            editor:DoWhenIdle(function(editor) callTipFitAndShow(editor, pos, tip) end)
          end
        end

      elseif ide.config.autocomplete then -- code completion prompt
        local trigger = linetxtopos:match("["..editor.spec.sep.."%w_]+$")
        if trigger and (#trigger > 1 or trigger:match("["..editor.spec.sep.."]")) then
          editor:DoWhenIdle(function(editor) EditorAutoComplete(editor) end)
        end
      end
    end)

  editor:Connect(wxstc.wxEVT_STC_DWELLSTART,
    function (event)
      -- on Linux DWELLSTART event seems to be generated even for those
      -- editor windows that are not active. What's worse, when generated
      -- the event seems to report "old" position when retrieved using
      -- event:GetX and event:GetY, so instead we use wxGetMousePosition.
      local linux = ide.osname == 'Unix'
      if linux and editor ~= GetEditor() then return end

      -- check if this editor has focus; it may not when Stack/Watch window
      -- is on top, but DWELL events are still triggered in this case.
      -- Don't want to show calltip as it is still shown when the focus
      -- is switched to a different application.
      local focus = editor:FindFocus()
      if focus and focus:GetId() ~= editor:GetId() then return end

      -- event:GetX() and event:GetY() positions don't correspond to
      -- the correct positions calculated using ScreenToClient (at least
      -- on Windows and Linux), so use what's calculated.
      local mpos = wx.wxGetMousePosition()
      local cpos = editor:ScreenToClient(mpos)
      local position = editor:PositionFromPointClose(cpos.x, cpos.y)
      if position ~= wxstc.wxSTC_INVALID_POSITION then
        EditorCallTip(editor, position, mpos.x, mpos.y)
      end
      event:Skip()
    end)

  editor:Connect(wxstc.wxEVT_STC_DWELLEND,
    function (event)
      if editor:CallTipActive() then editor:CallTipCancel() end
      event:Skip()
    end)

  editor:Connect(wx.wxEVT_KILL_FOCUS,
    function (event)
      -- on OSX clicking on scrollbar in the popup is causing the editor to lose focus,
      -- which causes canceling of auto-complete, which later cause crash because
      -- the window is destroyed in wxwidgets after already being closed. Skip on OSX.
      if ide.osname ~= 'Macintosh' and editor:AutoCompActive() then editor:AutoCompCancel() end
      PackageEventHandle("onEditorFocusLost", editor)
      event:Skip()
    end)

  local eol = {
    [wxstc.wxSTC_EOL_CRLF] = "\r\n",
    [wxstc.wxSTC_EOL_LF] = "\n",
    [wxstc.wxSTC_EOL_CR] = "\r",
  }
  local function addOneLine(editor, adj)
    local pos = editor:GetLineEndPosition(editor:LineFromPosition(editor:GetCurrentPos())+(adj or 0))
    local added = eol[editor:GetEOLMode()] or "\n"
    editor:InsertTextDyn(pos, added)
    editor:SetCurrentPos(pos+#added)

    local ev = wxstc.wxStyledTextEvent(wxstc.wxEVT_STC_CHARADDED)
    ev:SetKey(string.byte("\n"))
    editor:AddPendingEvent(ev)
  end

  editor:Connect(wxstc.wxEVT_STC_USERLISTSELECTION,
    function (event)
      if PackageEventHandle("onEditorUserlistSelection", editor, event) == false then
        return
      end

      -- if used Shift-Enter, then skip auto complete and just do Enter.
      -- `lastkey` comparison can be replaced with checking `listCompletionMethod`,
      -- but it's not exposed in wxSTC (as of wxwidgets 3.1.1)
      if wx.wxGetKeyState(wx.WXK_SHIFT) and editor.lastkey == ("\r"):byte() then
        return addOneLine(editor)
      end

      if ide.wxver >= "2.9.5" and editor:GetSelections() > 1 then
        local text = event:GetText()
        -- capture all positions as the selection may change
        local positions = {}
        for s = 0, editor:GetSelections()-1 do
          table.insert(positions, editor:GetSelectionNCaret(s))
        end
        -- process all selections from last to first
        table.sort(positions)
        local mainpos = editor:GetSelectionNCaret(editor:GetMainSelection())

        editor:BeginUndoAction()
        for s = #positions, 1, -1 do
          local pos = positions[s]
          local startpos = editor:WordStartPosition(pos, true)
          editor:SetSelection(startpos, pos)
          editor:ReplaceSelection(text)
          -- if this is the main position, save new cursor position to restore
          if pos == mainpos then mainpos = editor:GetCurrentPos()
          elseif pos < mainpos then
            -- adjust main position as earlier changes may affect it
            mainpos = mainpos + #text - (pos - startpos)
          end
        end
        editor:EndUndoAction()

        editor:GotoPos(mainpos)
      else
        local pos = editor:GetCurrentPos()
        local startpos = editor:WordStartPosition(pos, true)
        local endpos = editor:WordEndPosition(pos, true)
        editor:SetSelection(startpos, ide.config.acandtip.droprest and endpos or pos)
        editor:ReplaceSelection(event:GetText())
      end
    end)

  local function updateModified()
    local update = function()
      local doc = ide:GetDocument(editor)
      if doc then doc:SetModified(editor:GetModify()) end
    end
    -- delay update on Unix/Linux as it seems to hang the application on ArchLinux;
    -- execute immediately on other platforms
    if ide.osname == "Unix" then editor:DoWhenIdle(update) else update() end
  end
  editor:Connect(wxstc.wxEVT_STC_SAVEPOINTREACHED, updateModified)
  editor:Connect(wxstc.wxEVT_STC_SAVEPOINTLEFT, updateModified)

  -- "updateStatusText" should be called in UPDATEUI event, but it creates
  -- several performance problems on Windows (using wx2.9.5+) when
  -- brackets or backspace is used (very slow screen repaint with 0.5s delay).
  -- Moving it to PAINTED event creates problems on OSX (using wx2.9.5+),
  -- where refresh of R/W and R/O status in the status bar is delayed.

  editor:Connect(wxstc.wxEVT_STC_PAINTED,
    function (event)
      PackageEventHandle("onEditorPainted", editor, event)

      if ide.osname == 'Windows' then
        -- STC_PAINTED is called on multiple editors when they point to
        -- the same document; only update status for the active one
        if notebook:GetSelection() == notebook:GetPageIndex(editor) then
          updateStatusText(editor)
        end

        if edcfg.usewrap ~= true and editor:AutoCompActive() then
          -- showing auto-complete list leaves artifacts on the screen,
          -- which can only be fixed by a forced refresh.
          -- shows with wxSTC 3.21 and both wxwidgets 2.9.5 and 3.1
          editor:Update()
          editor:Refresh()
        end
      end

      -- adjust line number margin, but only if it's already shown
      local linecount = #tostring(editor:GetLineCount()) + 0.5
      local mwidth = editor:GetMarginWidth(margin.LINENUMBER)
      if mwidth > 0 then
        local width = math.max(linecount, linenumlen) * editor:TextWidth(wxstc.wxSTC_STYLE_DEFAULT, "8")
        if mwidth ~= width then editor:SetMarginWidth(margin.LINENUMBER, math.floor(width)) end
      end
    end)

  editor.processedUpdateContent = 0
  editor:Connect(wxstc.wxEVT_STC_UPDATEUI,
    function (event)
      -- some of UPDATEUI events may be triggered as the result of editor updates
      -- from subsequent events (like PAINTED, which happens in documentmap plugin).
      -- the reason for the `processed` check is that it is not possible
      -- to completely skip all of these updates as this causes the issue
      -- of markup styling becoming visible after text deletion by Backspace.
      -- to avoid this, we allow the first update after any updates caused
      -- by real changes; the rest of UPDATEUI events are skipped.
      -- (use direct comparison, as need to skip events that just update content)
      if event:GetUpdated() == wxstc.wxSTC_UPDATE_CONTENT
      and not next(editor.ev) then
         if editor.processedUpdateContent > 1 then return end
      else
         editor.processedUpdateContent = 0
      end
      editor.processedUpdateContent = editor.processedUpdateContent + 1

      PackageEventHandle("onEditorUpdateUI", editor, event)

      if ide.osname ~= 'Windows' then updateStatusText(editor) end

      editor:GotoPosDelayed()
      updateBraceMatch(editor)
      local minupdated
      for _,iv in ipairs(editor.ev) do
        local line = iv[1]
        if not minupdated or line < minupdated then minupdated = line end
        IndicateFunctionsOnly(editor,line,line+iv[2])
      end
      if minupdated then
        local ok, res = pcall(IndicateAll, editor, minupdated)
        if not ok then ide:Print("Internal error: ",res,minupdated) end
      end
      local firstvisible = editor:GetFirstVisibleLine()
      local firstline = editor:DocLineFromVisible(firstvisible)
      local lastline = editor:DocLineFromVisible(firstvisible + editor:LinesOnScreen())
      -- cap last line at the number of lines in the document
      MarkupStyle(editor, minupdated or firstline, math.min(editor:GetLineCount(),lastline))
      editor.ev = {}
    end)

  editor:Connect(wx.wxEVT_IDLE,
    function (event)
      while #editor.onidle > 0 do table.remove(editor.onidle, 1)(editor) end
    end)

  editor:Connect(wx.wxEVT_LEFT_DOWN,
    function (event)
      if MarkupHotspotClick then
        local position = editor:PositionFromPointClose(event:GetX(),event:GetY())
        if position ~= wxstc.wxSTC_INVALID_POSITION then
          if MarkupHotspotClick(position, editor) then return end
        end
      end

      if event:ControlDown() and event:AltDown()
      -- ide.wxver >= "2.9.5"; fix after GetModifiers is added to wxMouseEvent in wxlua
      and not event:ShiftDown() and not event:MetaDown() then
        local point = event:GetPosition()
        local pos = editor:PositionFromPointClose(point.x, point.y)
        local value = pos ~= wxstc.wxSTC_INVALID_POSITION and editor:ValueFromPosition(pos) or nil
        local instances = value and indicateFindInstances(editor, value, pos+1)
        if instances and instances[0] then
          navigateToPosition(editor, pos, instances[0]-1, #value)
          return
        end
      end
      event:Skip()
    end)

  if edcfg.nomousezoom then
    -- disable zoom using mouse wheel as it triggers zooming when scrolling
    -- on OSX with kinetic scroll and then pressing CMD.
    editor:Connect(wx.wxEVT_MOUSEWHEEL,
      function (event)
        if wx.wxGetKeyState(wx.WXK_CONTROL) then return end
        event:Skip()
      end)
  end

  local inhandler = false
  editor:Connect(wx.wxEVT_SET_FOCUS,
    function (event)
      event:Skip()
      if inhandler or ide.exitingProgram then return end
      inhandler = true
      PackageEventHandle("onEditorFocusSet", editor)
      isFileAlteredOnDisk(editor)
      inhandler = false
    end)

  editor:Connect(wx.wxEVT_KEY_DOWN,
    function (event)
      local keycode = event:GetKeyCode()
      local mod = event:GetModifiers()
      if PackageEventHandle("onEditorKeyDown", editor, event) == false then
        -- this event has already been handled
      elseif keycode == wx.WXK_ESCAPE then
        if editor:CallTipActive() or editor:AutoCompActive() then
          event:Skip()
        elseif ide.findReplace:IsShown() then
          ide.findReplace:Hide()
        elseif ide:GetMainFrame():IsFullScreen() then
          ShowFullScreen(false)
        end
      -- Ctrl-Home and Ctrl-End don't work on OSX with 2.9.5+; fix it
      elseif ide.osname == 'Macintosh' and ide.wxver >= "2.9.5"
        and (mod == wx.wxMOD_RAW_CONTROL or mod == (wx.wxMOD_RAW_CONTROL + wx.wxMOD_SHIFT))
        and (keycode == wx.WXK_HOME or keycode == wx.WXK_END) then
        local pos = keycode == wx.WXK_HOME and 0 or editor:GetLength()
        if event:ShiftDown() -- mark selection and scroll to caret
        then editor:SetCurrentPos(pos) editor:EnsureCaretVisible()
        else editor:GotoPos(pos) end
      elseif (keycode == wx.WXK_DELETE or keycode == wx.WXK_BACK)
        and (mod == wx.wxMOD_NONE) then
        -- Delete and Backspace behave the same way for selected text
        if #(editor:GetSelectedText()) > 0 then
          editor:ClearAny()
        else
          local pos = editor:GetCurrentPos()
          if keycode == wx.WXK_BACK then
            pos = pos - 1
            if pos < 0 then return end
          end

          -- check if the modification is to one of "invisible" characters.
          -- if not, proceed with "normal" processing as there are other
          -- events that may depend on Backspace, for example, re-calculating
          -- auto-complete suggestions.
          local style = bit.band(editor:GetStyleAt(pos), ide.STYLEMASK)
          if not MarkupIsSpecial or not MarkupIsSpecial(style) then
            event:Skip()
            return
          end

          editor:SetTargetStart(pos)
          editor:SetTargetEnd(pos+1)
          editor:ReplaceTarget("")
        end
      elseif mod == wx.wxMOD_ALT and keycode == wx.WXK_LEFT then
        -- if no "jump back" is needed, then do normal processing as this
        -- combination can be mapped to some action
        if not navigateBack(editor) then event:Skip() end
      elseif (keycode == wx.WXK_RETURN or keycode == wx.WXK_NUMPAD_ENTER)
      and (mod == wx.wxMOD_CONTROL or mod == (wx.wxMOD_CONTROL + wx.wxMOD_SHIFT)) then
        addOneLine(editor, mod == (wx.wxMOD_CONTROL + wx.wxMOD_SHIFT) and -1 or 0)
      elseif ide.osname == "Unix" and ide.wxver >= "2.9.5"
      and editor.ctrlcache[keycode..mod] then
        ide.frame:AddPendingEvent(wx.wxCommandEvent(
          wx.wxEVT_COMMAND_MENU_SELECTED, editor.ctrlcache[keycode..mod]))
      else
        if ide.osname == 'Macintosh' and mod == wx.wxMOD_META then
          return -- ignore a key press if Command key is also pressed
        end
        event:Skip()
      end

      editor.lastkey = keycode
    end)

  local function selectAllInstances(instances, name, curpos)
    local this
    local idx = 0
    for _, pos in pairs(instances) do
      pos = pos - 1 -- positions are 0-based in Scintilla
      if idx == 0 then
        -- clear selections first as there seems to be a bug (Scintilla 3.2.3)
        -- that doesn't reset selection after right mouse click.
        editor:ClearSelections()
        editor:SetSelection(pos, pos+#name)
      else
        editor:AddSelection(pos+#name, pos)
      end

      -- check if this is the current selection
      if curpos >= pos and curpos <= pos+#name then this = idx end
      idx = idx + 1
    end
    if this then editor:SetMainSelection(this) end
    -- set the current name as the search value to make subsequence searches look for it
    ide.findReplace:SetFind(name)
  end

  editor:Connect(wxstc.wxEVT_STC_DOUBLECLICK,
    function(event)
      -- only activate selection of instances on Ctrl/Cmd-DoubleClick
      if event:GetModifiers() == wx.wxMOD_CONTROL then
        local pos = event:GetPosition()
        local value = pos ~= wxstc.wxSTC_INVALID_POSITION and editor:ValueFromPosition(pos) or nil
        local instances = value and indicateFindInstances(editor, value, pos+1)
        if instances and (instances[0] or #instances > 0) then
          selectAllInstances(instances, value, pos)
          return
        end
      end

      event:Skip()
    end)

  editor:Connect(wxstc.wxEVT_STC_ZOOM,
    function(event)
      -- if Shift+Zoom is used, then zoom all editors, not just the current one
      if wx.wxGetKeyState(wx.WXK_SHIFT) then
        local zoom = editor:GetZoom()
        for _, doc in pairs(openDocuments) do
          -- check the editor zoom level to avoid recursion
          if doc.editor:GetZoom() ~= zoom then doc.editor:SetZoom(zoom) end
        end
      end
      event:Skip()
    end)

  if ide.osname == "Windows" then
    editor:DragAcceptFiles(true)
    editor:Connect(wx.wxEVT_DROP_FILES, function(event)
        local files = event:GetFiles()
        if not files or #files == 0 then return end
        -- activate all files/directories one by one
        for _, filename in ipairs(files) do ide:ActivateFile(filename) end
      end)
  elseif ide.osname == "Unix" then
    editor:Connect(wxstc.wxEVT_STC_DO_DROP, function(event)
        local dropped = event:GetText()
        -- this event may get a list of files separated by \n (and the list ends in \n as well),
        -- so check if what's dropped looks like this list
        if dropped:find("^file://.+\n$") then
          for filename in dropped:gmatch("file://(.-)\n") do ide:ActivateFile(filename) end
          event:SetDragResult(wx.wxDragCancel) -- cancel the drag to not paste the text
        end
      end)
  end

  local pos
  local function getPositionValues()
    local p = pos or editor:GetCurrentPos()
    local value = p ~= wxstc.wxSTC_INVALID_POSITION and editor:ValueFromPosition(p) or nil
    local instances = value and indicateFindInstances(editor, value, p+1)
    return p, value, instances
  end
  editor:Connect(wx.wxEVT_CONTEXT_MENU,
    function (event)
      local point = editor:ScreenToClient(event:GetPosition())
      -- capture the position of the click to use in handlers later
      pos = editor:PositionFromPoint(point)

      local _, _, instances = getPositionValues()
      local occurrences = (not instances or #instances == 0) and ""
        or (" (%d)"):format(#instances+(instances[0] and 1 or 0))
      local line = instances and instances[0] and editor:LineFromPosition(instances[0]-1)+1
      local def =  line and " ("..TR("on line %d"):format(line)..")" or ""
      local menu = ide:MakeMenu {
        { ID_UNDO, TR("&Undo") },
        { ID_REDO, TR("&Redo") },
        { },
        { ID_CUT, TR("Cu&t") },
        { ID_COPY, TR("&Copy") },
        { ID_PASTE, TR("&Paste") },
        { ID_SELECTALL, TR("Select &All") },
        { },
        { ID_GOTODEFINITION, TR("Go To Definition")..def..KSC(ID_GOTODEFINITION) },
        { ID_RENAMEALLINSTANCES, TR("Rename All Instances")..occurrences..KSC(ID_RENAMEALLINSTANCES) },
        { ID_REPLACEALLSELECTIONS, TR("Replace All Selections")..KSC(ID_REPLACEALLSELECTIONS) },
        { },
        { ID_QUICKADDWATCH, TR("Add Watch Expression")..KSC(ID_QUICKADDWATCH) },
        { ID_QUICKEVAL, TR("Evaluate In Console")..KSC(ID_QUICKEVAL) },
        { ID_ADDTOSCRATCHPAD, TR("Add To Scratchpad")..KSC(ID_ADDTOSCRATCHPAD) },
        { ID_RUNTO, TR("Run To Cursor")..KSC(ID_RUNTO) },
      }
      -- disable calltips that could open over the menu
      local dwelltime = editor:GetMouseDwellTime()
      editor:SetMouseDwellTime(0) -- disable dwelling

      -- cancel calltip if it's already shown as it interferes with popup menu
      if editor:CallTipActive() then editor:CallTipCancel() end

      PackageEventHandle("onMenuEditor", menu, editor, event)

      -- popup statuses are not refreshed on Linux, so do it manually
      if ide.osname == "Unix" then UpdateMenuUI(menu, editor) end

      editor:PopupMenu(menu)
      editor:SetMouseDwellTime(dwelltime) -- restore dwelling
      pos = nil -- reset the position
    end)

  editor:Connect(ID_RUNTO, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      local pos = getPositionValues()
      if pos and pos ~= wxstc.wxSTC_INVALID_POSITION then
        ide:GetDebugger():RunTo(editor, editor:LineFromPosition(pos)+1)
      end
    end)

  editor:Connect(ID_GOTODEFINITION, wx.wxEVT_UPDATE_UI, function(event)
      local _, _, instances = getPositionValues()
      event:Enable(instances and instances[0])
    end)
  editor:Connect(ID_GOTODEFINITION, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event)
      local _, value, instances = getPositionValues()
      if value and instances[0] then
        navigateToPosition(editor, editor:GetCurrentPos(), instances[0]-1, #value)
      end
    end)

  editor:Connect(ID_RENAMEALLINSTANCES, wx.wxEVT_UPDATE_UI, function(event)
      local _, _, instances = getPositionValues()
      event:Enable(instances and (instances[0] or #instances > 0)
        or editor:GetSelectionStart() ~= editor:GetSelectionEnd())
    end)
  editor:Connect(ID_RENAMEALLINSTANCES, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event)
      local pos, value, instances = getPositionValues()
      if value and pos then
        if not (instances and (instances[0] or #instances > 0)) then
          -- if multiple instances (of a variable) are not detected,
          -- then simply find all instances of (selected) `value`
          instances = {}
          local length, pos = editor:GetLength(), 0
          while true do
            editor:SetTargetStart(pos)
            editor:SetTargetEnd(length)
            pos = editor:SearchInTarget(value)
            if pos == wx.wxNOT_FOUND then break end
            table.insert(instances, pos+1)
            pos = pos + #value
          end
        end
        selectAllInstances(instances, value, pos)
      end
    end)

  editor:Connect(ID_REPLACEALLSELECTIONS, wx.wxEVT_UPDATE_UI, function(event)
      event:Enable((ide.wxver >= "2.9.5" and editor:GetSelections() or 1) > 1)
    end)
  editor:Connect(ID_REPLACEALLSELECTIONS, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event)
      local main = editor:GetMainSelection()
      local text = wx.wxGetTextFromUser(
        TR("Enter replacement text"),
        TR("Replace All Selections"),
        editor:GetTextRangeDyn(editor:GetSelectionNStart(main), editor:GetSelectionNEnd(main))
      )
      if not text or text == "" then return end

      editor:BeginUndoAction()
      for s = 0, editor:GetSelections()-1 do
        local selst, selend = editor:GetSelectionNStart(s), editor:GetSelectionNEnd(s)
        editor:SetTargetStart(selst)
        editor:SetTargetEnd(selend)
        editor:ReplaceTarget(text)
        editor:SetSelectionNStart(s, selst)
        editor:SetSelectionNEnd(s, selst+#text)
      end
      editor:EndUndoAction()
      editor:SetMainSelection(main)
    end)

  editor:Connect(ID_QUICKADDWATCH, wx.wxEVT_UPDATE_UI, function(event)
      local _, value = getPositionValues()
      event:Enable(value ~= nil)
    end)
  editor:Connect(ID_QUICKADDWATCH, wx.wxEVT_COMMAND_MENU_SELECTED, function(event)
      local _, value = getPositionValues()
      ide:AddWatch(value)
    end)

  editor:Connect(ID_QUICKEVAL, wx.wxEVT_UPDATE_UI, function(event)
      local _, value = getPositionValues()
      event:Enable(value ~= nil)
    end)
  editor:Connect(ID_QUICKEVAL, wx.wxEVT_COMMAND_MENU_SELECTED, function(event)
      local _, value = getPositionValues()
      ShellExecuteCode(value)
    end)

  editor:Connect(ID_ADDTOSCRATCHPAD, wx.wxEVT_UPDATE_UI, function(event)
      local debugger = ide:GetDebugger()
      event:Enable(debugger.scratchpad
        and debugger.scratchpad.editors and not debugger.scratchpad.editors[editor])
    end)
  editor:Connect(ID_ADDTOSCRATCHPAD, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event) ide:GetDebugger():ScratchpadOn(editor) end)

  return editor
end

-- ----------------------------------------------------------------------------
-- Add an editor to the notebook
function AddEditor(editor, name)
  assert(notebook:GetPageIndex(editor) == wx.wxNOT_FOUND, "Editor being added is not in the notebook: failed")

  -- set the document properties
  local id = editor:GetId()
  local document = setmetatable({}, ide.proto.Document)
  document.editor = editor
  document.fileName = name
  document.filePath = nil
  document.modTime = nil
  document.isModified = false
  openDocuments[id] = document

  -- add page only after document is created as there may be handlers
  -- that expect the document (for example, onEditorFocusSet)
  if not notebook:AddPage(editor, name, true) then
    openDocuments[id] = nil
    return
  else
    document.index = notebook:GetPageIndex(editor)
    return document
  end
end

local lexlpegmap = {
  text = {"identifier"},
  lexerdef = {"nothing"},
  comment = {"comment"},
  stringtxt = {"string","longstring"},
  preprocessor= {"preprocessor","embedded"},
  operator = {"operator"},
  number = {"number"},
  keywords0 = {"keyword"},
  keywords1 = {"constant","variable"},
  keywords2 = {"function","regex"},
  keywords3 = {"library","class","type"},
}
local function cleanup(paths)
  for _, path in ipairs(paths) do
    if not FileRemove(path) then wx.wxRmdir(path) end
  end
end
local function setLexLPegLexer(editor, lexername)
  local lexer = lexername:gsub("^lexlpeg%.","")

  local ppath = package.path
  local lpath = ide:GetRootPath("lualibs/lexers")

  package.path = MergeFullPath(lpath, "?.lua") -- update `package.path` to reference `lexers/`
  local ok, lex = pcall(require, "lexer")
  package.path = ppath -- restore the original `package.path`
  if not ok then return nil, "Can't load LexLPeg lexer components: "..lex end

  -- if the requested lexer is a dynamically registered one, then need to create a file for it,
  -- as LexLPeg lexers are loaded in a separate Lua state, which this process has no contol over.
  local dynlexer, dynfile = ide:GetLexer(lexername), nil
  local tmppath = MergeFullPath(wx.wxStandardPaths.Get():GetTempDir(),
    "lexer-"..wx.wxGetLocalTimeMillis():ToString())
  if dynlexer then
    local ok, err = CreateFullPath(tmppath)
    if not ok then return nil, err end
    -- update lex.LEXERPATH to search there
    lex.LEXERPATH = MergeFullPath(tmppath, "?.lua")
    dynfile = MergeFullPath(tmppath, lexer..".lua")
    -- save the file to the temp folder
    ok, err = FileWrite(dynfile, dynlexer)
    if not ok then cleanup({tmppath}); return nil, err end
  end
  local lexmod, err = lex.load(lexer)
  if dynlexer then cleanup({dynfile, tmppath}) end
  if not lexmod then return nil, err end

  local lexpath = package.searchpath("lexlpeg", ide.osclibs)
  if not lexpath then return nil, "Can't find LexLPeg lexer." end

  do
    local err = wx.wxSysErrorCode()
    local _ = wx.wxLogNull() -- disable error reporting; will report as needed
    local loaded = pcall(function() editor:LoadLexerLibrary(lexpath) end)
    if not loaded then return nil, "Can't load LexLPeg library." end
    -- the error code may be non-zero, but still needs to be different from the previous one
    -- as it may report non-zero values on Windows (for example, 1447) when no error is generated
    local newerr = wx.wxSysErrorCode()
    if newerr > 0 and newerr ~= err then return nil, wx.wxSysErrorMsg() end
  end

  if dynlexer then
    local ok, err = CreateFullPath(tmppath)
    if not ok then return nil, err end
    -- copy lexer.lua to the temp folder
    ok, err = FileCopy(MergeFullPath(lpath, "lexer.lua"), MergeFullPath(tmppath, "lexer.lua"))
    if not ok then return nil, err end
    -- save the file to the temp folder
    ok, err = FileWrite(dynfile, dynlexer)
    if not ok then FileRemove(MergeFullPath(tmppath, "lexer.lua")); return nil, err end
    -- update lpath to point to the temp folder
    lpath = tmppath
  end

  -- temporarily set the enviornment variable to load the new lua state with proper paths
  -- do here as the Lua state in LexLPeg parser is initialized furing `SetLexerLanguage` call
  local ok, cpath = wx.wxGetEnv("LUA_CPATH")
  if ok then wx.wxSetEnv("LUA_CPATH", ide.osclibs) end
  editor:SetLexerLanguage("lpeg")
  editor:SetProperty("lexer.lpeg.home", lpath)
  editor:SetProperty("fold", edcfg.fold and "1" or "0")
  editor:PrivateLexerCall(wxstc.wxSTC_SETLEXERLANGUAGE, lexer) --[[ SetLexerLanguage for LexLPeg ]]
  if ok then wx.wxSetEnv("LUA_CPATH", cpath) end

  if dynlexer then cleanup({dynfile, MergeFullPath(tmppath, "lexer.lua"), tmppath}) end

  local styleconvert = {}
  for name, map in pairs(lexlpegmap) do
    styleconvert[name] = {}
    for _, stylename in ipairs(map) do
      if lexmod._TOKENSTYLES[stylename] then
        table.insert(styleconvert[name], lexmod._TOKENSTYLES[stylename])
      end
    end
  end
  return true, styleconvert
end

function SetupKeywords(editor, ext, forcespec, styles, font, fontitalic)
  local lexerstyleconvert = nil
  local spec = forcespec or ide:FindSpec(ext)
  -- found a spec setup lexers and keywords
  if spec then
    if type(spec.lexer) == "string" then
      local ok, res = setLexLPegLexer(editor, spec.lexer)
      if ok then
        spec.lexerstyleconvert = res
      else
        spec.lexerstyleconvert = {}
        ide:Print(("Can't load LexLPeg '%s' lexer: %s"):format(spec.lexer, res))
        editor:SetLexer(wxstc.wxSTC_LEX_NULL)
      end
      UpdateSpecs(spec)
    else
      editor:SetLexer(spec.lexer or wxstc.wxSTC_LEX_NULL)
    end
    lexerstyleconvert = spec.lexerstyleconvert

    if (spec.keywords) then
      for i,words in ipairs(spec.keywords) do
        editor:SetKeyWords(i-1,words)
      end
    end

    editor.api = GetApi(spec.apitype or "none")
    editor.spec = spec
  else
    editor:SetLexer(wxstc.wxSTC_LEX_NULL)
    editor:SetKeyWords(0, "")

    editor.api = GetApi("none")
    editor.spec = ide.specs.none
  end

  -- need to set folding property after lexer is set, otherwise
  -- the folds are not shown (wxwidgets 2.9.5)
  if edcfg.fold then
    editor:SetProperty("fold", "1")
    editor:SetProperty("fold.html", "1")
    editor:SetProperty("fold.compact", edcfg.foldcompact and "1" or "0")
    editor:SetProperty("fold.comment", "1")
  end
  
  -- quickfix to prevent weird looks, otherwise need to update styling mechanism for cpp
  -- cpp "greyed out" styles are  styleid + 64
  editor:SetProperty("lexer.cpp.track.preprocessor", "0")
  editor:SetProperty("lexer.cpp.update.preprocessor", "0")

  -- create italic font if only main font is provided
  if font and not fontitalic then
    fontitalic = wx.wxFont(font)
    fontitalic:SetStyle(wx.wxFONTSTYLE_ITALIC)
  end

  StylesApplyToEditor(styles or ide.config.styles, editor,
    font or ide.font.eNormal,fontitalic or ide.font.eItalic,lexerstyleconvert)
end
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide

ide.filetree = {
  projdir = "",
  projdirlist = {},
  projdirpartmap = {},
  projtreeCtrl = nil,
  imglist = ide:CreateImageList("PROJECT",
    "FOLDER", "FOLDER-MAPPED", "FILE-NORMAL", "FILE-NORMAL-START",
    -- "file known" needs to be the last one as dynamic types can be added after it
    "FILE-KNOWN"),
  settings = {extensionignore = {}, startfile = {}, mapped = {}},
  extmap = {},
}
local filetree = ide.filetree
local iscaseinsensitive = wx.wxFileName("A"):SameAs(wx.wxFileName("a"))
local pathsep = GetPathSeparator()
local q = EscapeMagic
local image = { -- the order of ids has to match the order in the ImageList
  DIRECTORY = 0, DIRECTORYMAPPED = 1, FILEOTHER = 2, FILEOTHERSTART = 3,
  FILEKNOWN = 4,
}

local clearbmp = ide:GetBitmap("FILE-NORMAL-CLR", "PROJECT", wx.wxSize(16,16))
local function str2rgb(str)
  local a = ('a'):byte()
  -- `red`/`blue` are more prominent colors; use them for the first two letters; suppress `green`
  local r = (((str:sub(1,1):lower():byte() or a)-a) % 27)/27
  local b = (((str:sub(2,2):lower():byte() or a)-a) % 27)/27
  local g = (((str:sub(3,3):lower():byte() or a)-a) % 27)/27/3
  local ratio = 256/(r + g + b + 1e-6)
  return {math.floor(r*ratio), math.floor(g*ratio), math.floor(b*ratio)}
end
local function createImg(ext)
  local iconmap = ide.config.filetree.iconmap
  local color = type(iconmap)=="table" and type(iconmap[ext])=="table" and iconmap[ext].fg
  local bitmap = wx.wxBitmap(16, 16)
  local font = wx.wxFont(ide.font.eNormal)
  font:SetPointSize(ide.osname == "Macintosh" and 6 or 5)
  local mdc = wx.wxMemoryDC()
  mdc:SelectObject(bitmap)
  mdc:SetFont(font)
  mdc:DrawBitmap(clearbmp, 0, 0, true)
  mdc:SetTextForeground(wx.wxColour(unpack(type(color)=="table" and color or str2rgb(ext))))
  mdc:DrawText(ext:sub(1,3), 2, 5) -- take first three letters only
  mdc:SelectObject(wx.wxNullBitmap)
  return bitmap
end

local function getIcon(name, isdir)
  local startfile = GetFullPathIfExists(FileTreeGetDir(),
    filetree.settings.startfile[FileTreeGetDir()])
  local ext = GetFileExt(name)
  local extmap = ide.filetree.extmap
  local known = extmap[ext] or ide:FindSpec(ext)
  if known and not extmap[ext] then
    local iconmap = ide.config.filetree.iconmap
    extmap[ext] = iconmap and ide.filetree.imglist:Add(createImg(ext)) or image.FILEKNOWN
  end
  local icon = isdir and image.DIRECTORY or known and extmap[ext] or image.FILEOTHER
  if startfile and startfile == name then icon = image.FILEOTHERSTART end
  return icon
end

local function treeAddDir(tree,parent_id,rootdir)
  local items = {}
  local item, cookie = tree:GetFirstChild(parent_id)
  while item:IsOk() do
    items[tree:GetItemText(item) .. tree:GetItemImage(item)] = item
    item, cookie = tree:GetNextChild(parent_id, cookie)
  end

  local cache = {}
  local curr
  local files = FileSysGetRecursive(rootdir)
  local dirmapped = {}
  if tree:IsRoot(parent_id) then
    local mapped = filetree.settings.mapped[FileTreeGetDir()] or {}
    table.sort(mapped)
    -- insert into files at the sorted order
    for i, v in ipairs(mapped) do
      table.insert(files, i, v)
      dirmapped[v] = true
    end
  end

  for _, file in ipairs(files) do
    local name, dir = file:match("([^"..pathsep.."]+)("..pathsep.."?)$")
    local isdir = #dir>0
    if isdir or not filetree.settings.extensionignore[GetFileExt(name)] then
      local icon = getIcon(file, isdir)

      -- keep full name for the mapped directories
      if dirmapped[file] then name, icon = file, image.DIRECTORYMAPPED end

      local item = items[name .. icon]
      if item then -- existing item
        -- keep deleting items until we find item
        while true do
          local nextitem = (curr
            and tree:GetNextSibling(curr)
            or tree:GetFirstChild(parent_id))
          if not nextitem:IsOk() or name == tree:GetItemText(nextitem) then
            curr = nextitem
            break
          end
          PackageEventHandle("onFiletreeFileRemove", tree, nextitem, tree:GetItemFullName(nextitem))
          tree:Delete(nextitem)
        end
      else -- new item
        curr = (curr
          and tree:InsertItem(parent_id, curr, name, icon)
          or tree:PrependItem(parent_id, name, icon))
        if isdir then tree:SetItemHasChildren(curr, FileDirHasContent(file)) end
        PackageEventHandle("onFiletreeFileAdd", tree, curr, file)
      end
      if curr:IsOk() then cache[iscaseinsensitive and name:lower() or name] = curr end
    end
  end

  -- delete any leftovers (something that exists in the tree, but not on disk)
  while true do
    local nextitem = (curr
      and tree:GetNextSibling(curr)
      or tree:GetFirstChild(parent_id))
    if not nextitem:IsOk() then break end
    PackageEventHandle("onFiletreeFileRemove", tree, nextitem, tree:GetItemFullName(nextitem))
    tree:Delete(nextitem)
  end

  -- cache the mapping from names to tree items
  if ide.wxver >= "2.9.5" then
    local data = wx.wxLuaTreeItemData()
    data:SetData(cache)
    tree:SetItemData(parent_id, data)
  end

  tree:SetItemHasChildren(parent_id,
    tree:GetChildrenCount(parent_id, false) > 0)

  PackageEventHandle("onFiletreeFileRefresh", tree, parent_id, tree:GetItemFullName(parent_id))
end

local function treeSetRoot(tree,rootdir)
  if not ide:IsValidCtrl(tree) then return end
  tree:DeleteAllItems()
  if not wx.wxDirExists(rootdir) then return end

  local root_id = tree:AddRoot(rootdir, image.DIRECTORY)
  tree:SetItemHasChildren(root_id, true) -- make sure that the item can expand
  tree:Expand(root_id) -- this will also populate the tree
end

local function findItem(tree, match)
  local node = tree:GetRootItem()
  local label = tree:GetItemText(node)

  local s, e
  if iscaseinsensitive then
    s, e = string.find(match:lower(), label:lower(), 1, true)
  else
    s, e = string.find(match, label, 1, true)
  end
  if not s or s ~= 1 then return end

  for token in string.gmatch(string.sub(match,e+1), "[^%"..pathsep.."]+") do
    local data = tree:GetItemData(node)
    local cache = data and data:GetData()
    if cache and cache[iscaseinsensitive and token:lower() or token] then
      node = cache[iscaseinsensitive and token:lower() or token]
    else
      -- token is missing; may need to re-scan the folder; maybe new file
      local dir = tree:GetItemFullName(node)
      treeAddDir(tree,node,dir)

      local item, cookie = tree:GetFirstChild(node)
      while true do
        if not item:IsOk() then return end -- not found
        if tree:GetItemText(item) == token then
          node = item
          break
        end
        item, cookie = tree:GetNextChild(node, cookie)
      end
    end
  end

  -- this loop exits only when a match is found
  return node
end

local function treeSetConnectorsAndIcons(tree)
  tree:AssignImageList(filetree.imglist)

  local function isIt(item, imgtype) return tree:GetItemImage(item) == imgtype end

  function tree:IsDirectory(item_id) return isIt(item_id, image.DIRECTORY) end
  function tree:IsDirMapped(item_id) return isIt(item_id, image.DIRECTORYMAPPED) end
  -- "file known" is a special case as it includes file types registered dynamically
  function tree:IsFileKnown(item_id) return tree:GetItemImage(item_id) >= image.FILEKNOWN end
  function tree:IsFileOther(item_id) return isIt(item_id, image.FILEOTHER) end
  function tree:IsFileStart(item_id) return isIt(item_id, image.FILEOTHERSTART) end
  function tree:IsRoot(item_id) return not tree:GetItemParent(item_id):IsOk() end

  function tree:FindItem(match)
    return findItem(self, (wx.wxIsAbsolutePath(match) or match == '') and match
      or MergeFullPath(ide:GetProject(), match))
  end

  function tree:GetItemFullName(item_id)
    local tree = self
    local str = tree:GetItemText(item_id)
    local cur = str

    while (#cur > 0) do
      item_id = tree:GetItemParent(item_id)
      if not item_id:IsOk() then break end
      cur = tree:GetItemText(item_id)
      if cur and #cur > 0 then str = MergeFullPath(cur, str) end
    end
    -- as root may already include path separator, normalize the path
    local fullPath = wx.wxFileName(str)
    fullPath:Normalize()
    return fullPath:GetFullPath()
  end

  function tree:RefreshChildren(node)
    node = node or tree:GetRootItem()
    treeAddDir(tree,node,tree:GetItemFullName(node))
    local item, cookie = tree:GetFirstChild(node)
    while true do
      if not item:IsOk() then return end
      if tree:IsExpanded(item) then tree:RefreshChildren(item) end
      item, cookie = tree:GetNextChild(node, cookie)
    end
  end

  local function refreshAncestors(node)
    -- when this method is called from END_EDIT, it causes infinite loop
    -- on OSX (wxwidgets 2.9.5) as Delete in treeAddDir calls END_EDIT again.
    -- disable handlers while the tree is populated and then enable back.
    tree:SetEvtHandlerEnabled(false)
    while node:IsOk() do
      local dir = tree:GetItemFullName(node)
      treeAddDir(tree,node,dir)
      node = tree:GetItemParent(node)
    end
    tree:SetEvtHandlerEnabled(true)
  end

  function tree:ActivateItem(item_id)
    local name = tree:GetItemFullName(item_id)

    local event = wx.wxTreeEvent(wx.wxEVT_COMMAND_TREE_ITEM_ACTIVATED, item_id:GetValue())
    if PackageEventHandle("onFiletreeActivate", tree, event, item_id) == false then
      return
    end

    -- refresh the folder
    if (tree:IsDirectory(item_id) or tree:IsDirMapped(item_id)) then
      if wx.wxDirExists(name) then
        treeAddDir(tree,item_id,name)
        tree:Toggle(item_id)
      else refreshAncestors(tree:GetItemParent(item_id)) end -- stale content
    else -- open file
      if wx.wxFileExists(name) then LoadFile(name,nil,true)
      else refreshAncestors(tree:GetItemParent(item_id)) end -- stale content
    end
  end

  local function unMapDir(dir)
    local project = FileTreeGetDir()
    if not project then return end

    local mapped = filetree.settings.mapped[project] or {}
    for k, m in ipairs(mapped) do
      if m == dir then table.remove(mapped, k) end
    end
    filetree.settings.mapped[project] = #mapped > 0 and mapped or nil
    refreshAncestors(tree:GetRootItem())
  end
  local function mapDir()
    local project = FileTreeGetDir()
    if not project then return end

    local dirPicker = wx.wxDirDialog(ide.frame, TR("Choose a directory to map"),
      project ~= "" and project or wx.wxGetCwd(), wx.wxDIRP_DIR_MUST_EXIST)
    if dirPicker:ShowModal(true) ~= wx.wxID_OK then return end
    local dir = wx.wxFileName.DirName(FixDir(dirPicker:GetPath()))
    local path = dir:GetFullPath()

    -- don't remap the project directory
    if dir:SameAs(wx.wxFileName(project)) then return end

    local mapped = filetree.settings.mapped[project] or {}
    for _, m in ipairs(mapped) do
      if m == path then return end -- already on the list
    end
    table.insert(mapped, path)
    filetree.settings.mapped[project] = mapped
    refreshAncestors(tree:GetRootItem())
  end

  local empty = ""
  local function renameItem(itemsrc, target)
    local cache = type(itemsrc) == 'table' and itemsrc or nil
    local isdir = not cache and tree:IsDirectory(itemsrc) or cache and cache.isdir or false
    local isnew = not cache and tree:GetItemText(itemsrc) == empty or cache and cache.isnew or false
    local source = cache and cache.fullname or tree:GetItemFullName(itemsrc)
    local fn = wx.wxFileName(target)

    -- check if the target is the same as the source;
    -- SameAs check is not used here as "Test" and "test" are the same
    -- on case insensitive systems, but need to be allowed in renaming.
    if source == target then return end

    local docs = {}
    if not isnew then -- find if source is already opened in the editor
      docs = (isdir
        and ide:FindDocumentsByPartialPath(source)
        or {ide:FindDocument(source)})
      for _, doc in ipairs(docs) do
        if not isdir and PackageEventHandle("onEditorPreSave", doc.editor, source) == false then
          return false
        end
        if SaveModifiedDialog(doc.editor, true) == wx.wxID_CANCEL then return end
      end
    end

    -- check if existing file/dir is going to be overwritten
    local overwrite = ((wx.wxFileExists(target) or wx.wxDirExists(target))
      and not wx.wxFileName(source):SameAs(fn))
    if overwrite and not ApproveFileOverwrite() then return false end

    if not fn:Mkdir(tonumber(755,8), wx.wxPATH_MKDIR_FULL) then
      ReportError(TR("Unable to create directory '%s'."):format(target))
      return false
    end

    if isnew then -- new directory or file; create manually
      if (isdir and not wx.wxFileName.DirName(target):Mkdir(tonumber(755,8), wx.wxPATH_MKDIR_FULL))
      or (not isdir and not FileWrite(target, "")) then
        ReportError(TR("Unable to create file '%s'."):format(target))
        return false
      end
    else -- existing directory or file; rename/move it
      local ok, err = FileRename(source, target)
      if not ok then
        ReportError(TR("Unable to rename file '%s'."):format(source)
          .."\nError: "..err)
        return false
      end
    end

    refreshAncestors(cache and cache.parent or tree:GetItemParent(itemsrc))
    -- load file(s) into the same editor (if any); will also refresh the tree
    if #docs > 0 then
      for _, doc in ipairs(docs) do
        local fullpath = doc.filePath
        doc.filePath = nil -- remove path to avoid "file no longer exists" message
        -- when moving folders, /foo/bar/file.lua can be replaced with
        -- /foo/baz/bar/file.lua, so change /foo/bar to /foo/baz/bar
        local path = (not iscaseinsensitive and fullpath:gsub(q(source), target)
          or fullpath:lower():gsub(q(source:lower()), target))
        local editor = LoadFile(path)
        -- check if the file was loaded into another editor;
        -- this is possible if "foo" is renamed to "bar" and both are opened;
        -- if this happens, then "bar" is refreshed and "foo" can be closed.
        if doc.editor:GetId() ~= editor:GetId() then ClosePage(doc.index) end
        if not isdir and editor then PackageEventHandle("onEditorSave", editor) end
      end
    else -- refresh the tree and select the new item
      local itemdst = tree:FindItem(target)
      if itemdst then
        refreshAncestors(tree:GetItemParent(itemdst))
        tree:SelectItem(itemdst)
        tree:EnsureVisible(itemdst)
        tree:SetScrollPos(wx.wxHORIZONTAL, 0, true)
      end
    end

    -- refresh the target if it's open and has been overwritten
    if overwrite and not isdir then
      local doc = ide:FindDocument(target)
      if doc then LoadFile(doc:GetFilePath(), doc:GetEditor()) end
    end

    return true
  end
  local function deleteItem(item_id)
    -- if delete is for mapped directory, unmap it instead
    if tree:IsDirMapped(item_id) then
      unMapDir(tree:GetItemText(item_id))
      return
    end

    local isdir = tree:IsDirectory(item_id)
    local source = tree:GetItemFullName(item_id)

    if isdir and FileDirHasContent(source..pathsep) then return false end
    if wx.wxMessageBox(
      TR("Do you want to delete '%s'?"):format(source),
      ide:GetProperty("editormessage"),
      wx.wxYES_NO + wx.wxCENTRE, ide.frame) ~= wx.wxYES then return false end

    if isdir then
      if not wx.wxRmdir(source) then
        ReportError(TR("Unable to delete directory '%s': %s")
          :format(source, wx.wxSysErrorMsg()))
      end
    else
      local doc = ide:FindDocument(source)
      if doc then ClosePage(doc.index) end
      if not wx.wxRemoveFile(source) then
        ReportError(TR("Unable to delete file '%s': %s")
          :format(source, wx.wxSysErrorMsg()))
      end
    end
    refreshAncestors(tree:GetItemParent(item_id))
    return true
  end

  tree:Connect(wx.wxEVT_COMMAND_TREE_ITEM_COLLAPSED,
    function (event)
      PackageEventHandle("onFiletreeCollapse", tree, event, event:GetItem())
    end)
  tree:Connect(wx.wxEVT_COMMAND_TREE_ITEM_EXPANDED,
    function (event)
      PackageEventHandle("onFiletreeExpand", tree, event, event:GetItem())
    end)
  tree:Connect(wx.wxEVT_COMMAND_TREE_ITEM_COLLAPSING,
    function (event)
      if PackageEventHandle("onFiletreePreCollapse", tree, event, event:GetItem()) == false then
        return
      end
    end)
  tree:Connect(wx.wxEVT_COMMAND_TREE_ITEM_EXPANDING,
    function (event)
      local item_id = event:GetItem()
      if PackageEventHandle("onFiletreePreExpand", tree, event, item_id) == false then
        return
      end

      local dir = tree:GetItemFullName(item_id)
      if wx.wxDirExists(dir) then treeAddDir(tree,item_id,dir) -- refresh folder
      else refreshAncestors(tree:GetItemParent(item_id)) end -- stale content
      return true
    end)
  tree:Connect(wx.wxEVT_COMMAND_TREE_ITEM_ACTIVATED,
    function (event)
      tree:ActivateItem(event:GetItem())
    end)

  local function saveSettings()
    ide:AddPackage('core.filetree', {}):SetSettings(filetree.settings)
  end

  -- refresh the tree
  local function refreshChildren()
    tree:RefreshChildren()
    -- now mark the current file (if it was previously disabled)
    local editor = ide:GetEditor()
    if editor then FileTreeMarkSelected(ide:GetDocument(editor):GetFilePath()) end
  end

  -- handle context menu
  local function addItem(item_id, name, img)
    local isdir = tree:IsDirectory(item_id)
    local parent = isdir and item_id or tree:GetItemParent(item_id)
    if isdir then tree:Expand(item_id) end -- expand to populate if needed

    local item = tree:PrependItem(parent, name, img)
    tree:SetItemHasChildren(parent, true)
    -- temporarily disable expand as we don't need this node populated
    tree:SetEvtHandlerEnabled(false)
    tree:EnsureVisible(item)
    tree:SetEvtHandlerEnabled(true)
    return item
  end

  local function unsetStartFile()
    local project = FileTreeGetDir()
    if not project then return end

    local startfile = filetree.settings.startfile[project]
    filetree.settings.startfile[project] = nil
    if startfile then
      local item_id = tree:FindItem(startfile)
      if item_id and item_id:IsOk() then
        tree:SetItemImage(item_id, getIcon(tree:GetItemFullName(item_id)))
      end
    end
    return startfile
  end

  local function setStartFile(item_id)
    local project = FileTreeGetDir()
    if not project then return end

    local startfile = tree:GetItemFullName(item_id):gsub(project, "")
    filetree.settings.startfile[project] = startfile
    tree:SetItemImage(item_id, getIcon(tree:GetItemFullName(item_id)))
    return startfile
  end

  function tree:GetStartFile()
    local project = FileTreeGetDir()
    return project and filetree.settings.startfile[project]
  end

  function tree:SetStartFile(path)
    local item_id
    local project = FileTreeGetDir()
    if project and type(path) == "string" then
      local startfile = path:gsub(project, "")
      item_id = self:FindItem(startfile)
    end
    -- unset if explicitly requested or the replacement has been found
    if not path or item_id then unsetStartFile() end
    if item_id then setStartFile(item_id) end

    saveSettings()
    return item_id
  end

  tree:Connect(ID_NEWFILE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      tree:EditLabel(addItem(tree:GetSelection(), empty, image.FILEOTHER))
    end)
  tree:Connect(ID_NEWDIRECTORY, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      tree:EditLabel(addItem(tree:GetSelection(), empty, image.DIRECTORY))
    end)
  tree:Connect(ID_RENAMEFILE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() tree:EditLabel(tree:GetSelection()) end)
  tree:Connect(ID_DELETEFILE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() deleteItem(tree:GetSelection()) end)
  tree:Connect(ID_COPYFULLPATH, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() ide:CopyToClipboard(tree:GetItemFullName(tree:GetSelection())) end)
  tree:Connect(ID_OPENEXTENSION, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      local fname = tree:GetItemFullName(tree:GetSelection())
      local ext = '.'..wx.wxFileName(fname):GetExt()
      local ft = wx.wxTheMimeTypesManager:GetFileTypeFromExtension(ext)
      if ft then
        local cmd = ft:GetOpenCommand(fname:gsub('"','\\"'))
        -- some programs on Windows, when started by rundll32.exe (for example, PhotoViewer)
        -- accept files with spaces in names ONLY if they are not in quotes.
        if ide.osname == "Windows" and cmd:find("rundll32%.exe") then
          cmd = ft:GetOpenCommand(""):gsub('""%s*$', '')..fname
        end
        wx.wxExecute(cmd, wx.wxEXEC_ASYNC)
      end
    end)
  tree:Connect(ID_REFRESH, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() refreshChildren() end)
  tree:Connect(ID_SHOWLOCATION, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() ShowLocation(tree:GetItemFullName(tree:GetSelection())) end)
  tree:Connect(ID_HIDEEXTENSION, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      local ext = GetFileExt(tree:GetItemText(tree:GetSelection()))
      filetree.settings.extensionignore[ext] = true
      saveSettings()
      refreshChildren()
    end)
  tree:Connect(ID_SHOWEXTENSIONALL, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      filetree.settings.extensionignore = {}
      saveSettings()
      refreshChildren()
    end)
  tree:Connect(ID_SETSTARTFILE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      unsetStartFile()
      setStartFile(tree:GetSelection())
      saveSettings()
    end)
  tree:Connect(ID_UNSETSTARTFILE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      unsetStartFile()
      saveSettings()
    end)
  tree:Connect(ID_MAPDIRECTORY, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      mapDir()
      saveSettings()
    end)
  tree:Connect(ID_UNMAPDIRECTORY, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      unMapDir(tree:GetItemText(tree:GetSelection()))
      saveSettings()
    end)
  tree:Connect(ID_PROJECTDIRFROMDIR, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      ide:SetProject(tree:GetItemFullName(tree:GetSelection()))
    end)

  tree:Connect(wx.wxEVT_COMMAND_TREE_ITEM_MENU,
    function (event)
      local item_id = event:GetItem()
      tree:SelectItem(item_id)

      local renamelabel = (tree:IsRoot(item_id)
        and TR("&Edit Project Directory")
        or TR("&Rename"))
      local fname = tree:GetItemText(item_id)
      local ext = GetFileExt(fname)
      local project = FileTreeGetDir()
      local startfile = project and filetree.settings.startfile[project]
      local menu = ide:MakeMenu {
        { ID_NEWFILE, TR("New &File") },
        { ID_NEWDIRECTORY, TR("&New Directory") },
        { },
        { ID_RENAMEFILE, renamelabel..KSC(ID_RENAMEFILE) },
        { ID_DELETEFILE, TR("&Delete")..KSC(ID_DELETEFILE) },
        { ID_REFRESH, TR("Refresh") },
        { },
        { ID_HIDEEXTENSION, TR("Hide '.%s' Files"):format(ext) },
        { },
        { ID_SETSTARTFILE, TR("Set As Start File") },
        { ID_UNSETSTARTFILE, TR("Unset '%s' As Start File"):format(startfile or "<none>") },
        { },
        { ID_MAPDIRECTORY, TR("Map Directory...") },
        { ID_UNMAPDIRECTORY, TR("Unmap Directory") },
        { ID_OPENEXTENSION, TR("Open With Default Program") },
        { ID_COPYFULLPATH, TR("Copy Full Path") },
        { ID_SHOWLOCATION, TR("Show Location") },
      }
      local extlist = {
        {},
        { ID_SHOWEXTENSIONALL, TR("Show All Files"), TR("Show all files") },
      }
      for extignore in pairs(filetree.settings.extensionignore) do
        local id = ID("filetree.showextension."..extignore)
        table.insert(extlist, 1, {id, '.'..extignore})
        menu:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, function()
          filetree.settings.extensionignore[extignore] = nil
          saveSettings()
          refreshChildren()
        end)
      end
      local _, _, hideextpos = ide:FindMenuItem(ID_HIDEEXTENSION, menu)
      assert(hideextpos, "Can't find HideExtension menu item")
      menu:Insert(hideextpos+1, wx.wxMenuItem(menu, ID_SHOWEXTENSION,
        TR("Show Hidden Files"), TR("Show files previously hidden"),
        wx.wxITEM_NORMAL, ide:MakeMenu(extlist)))

      local projectdirectorymenu = ide:MakeMenu {
        { },
        {ID_PROJECTDIRCHOOSE, TR("Choose...")..KSC(ID_PROJECTDIRCHOOSE), TR("Choose a project directory")},
        {ID_PROJECTDIRFROMDIR, TR("Set To Selected Directory")..KSC(ID_PROJECTDIRFROMDIR), TR("Set project directory to the selected one")},
      }
      local projectdirectory = wx.wxMenuItem(menu, ID_PROJECTDIR,
        TR("Project Directory"), TR("Set the project directory to be used"),
        wx.wxITEM_NORMAL, projectdirectorymenu)
      local _, _, unmapdirpos = ide:FindMenuItem(ID_UNMAPDIRECTORY, menu)
      assert(unmapdirpos, "Can't find UnMapDirectory menu item")
      menu:Insert(unmapdirpos+1, projectdirectory)
      FileTreeProjectListUpdate(projectdirectorymenu, 0)

      -- disable Delete on non-empty directories
      local isdir = tree:IsDirectory(item_id)
      local ismapped = tree:IsDirMapped(item_id)
      menu:Destroy(ismapped and ID_MAPDIRECTORY or ID_UNMAPDIRECTORY)
      if not startfile then menu:Destroy(ID_UNSETSTARTFILE) end
      if ismapped then menu:Enable(ID_RENAMEFILE, false) end
      if isdir then
        local source = tree:GetItemFullName(item_id)
        menu:Enable(ID_DELETEFILE, not FileDirHasContent(source..pathsep))
        menu:Enable(ID_OPENEXTENSION, false)
        menu:Enable(ID_HIDEEXTENSION, false)
      else
        local ft = wx.wxTheMimeTypesManager:GetFileTypeFromExtension('.'..ext)
        menu:Enable(ID_OPENEXTENSION, ft and #ft:GetOpenCommand("") > 0)
        menu:Enable(ID_HIDEEXTENSION, not filetree.settings.extensionignore[ext])
        menu:Enable(ID_PROJECTDIRFROMDIR, false)
      end
      menu:Enable(ID_SETSTARTFILE, tree:IsFileOther(item_id) or tree:IsFileKnown(item_id))
      menu:Enable(ID_SHOWEXTENSION, next(filetree.settings.extensionignore) ~= nil)

      PackageEventHandle("onMenuFiletree", menu, tree, event)

      -- stopping/restarting garbage collection is generally not needed,
      -- but on Linux not stopping is causing crashes (wxwidgets 2.9.5 and 3.1.0)
      -- when symbol indexing is done while popup menu is open (with gc methods in the trace).
      -- this only happens when EVT_IDLE is called when popup menu is open.
      collectgarbage("stop")

      -- stopping UI updates is generally not needed as well,
      -- but it's causing a crash on OSX (wxwidgets 2.9.5 and 3.1.0)
      -- when symbol indexing is done while popup menu is open, so it's disabled
      local interval = wx.wxUpdateUIEvent.GetUpdateInterval()
      wx.wxUpdateUIEvent.SetUpdateInterval(-1) -- don't update

      tree:PopupMenu(menu)
      wx.wxUpdateUIEvent.SetUpdateInterval(interval)
      collectgarbage("restart")
    end)

  tree:Connect(wx.wxEVT_RIGHT_DOWN,
    function (event)
      local item_id = tree:HitTest(event:GetPosition())
      if PackageEventHandle("onFiletreeRDown", tree, event, item_id and item_id:IsOk() and item_id or nil) == false then
        return
      end
      event:Skip()
    end)

  -- toggle a folder on a single click
  tree:Connect(wx.wxEVT_LEFT_DOWN,
    function (event)
      -- only toggle if this is a folder and the click is on the item line
      -- (exclude the label as it's used for renaming and dragging)
      local mask = (wx.wxTREE_HITTEST_ONITEMINDENT
        + wx.wxTREE_HITTEST_ONITEMICON + wx.wxTREE_HITTEST_ONITEMRIGHT)
      local item_id, flags = tree:HitTest(event:GetPosition())

      if PackageEventHandle("onFiletreeLDown", tree, event, item_id and item_id:IsOk() and item_id or nil) == false then
        return
      end

      if item_id and item_id:IsOk() and bit.band(flags, mask) > 0 then
        if tree:IsDirectory(item_id) then
          tree:Toggle(item_id)
          tree:SelectItem(item_id)
        else
          local name = tree:GetItemFullName(item_id)
          if wx.wxFileExists(name) then LoadFile(name,nil,true) end
        end
      else
        event:Skip()
      end
      return true
    end)
  local parent
  tree:Connect(wx.wxEVT_COMMAND_TREE_BEGIN_LABEL_EDIT,
    function (event)
      local itemsrc = event:GetItem()
      parent = tree:GetItemParent(itemsrc)
      if not itemsrc:IsOk() or tree:IsDirMapped(itemsrc) then event:Veto() end
    end)
  tree:Connect(wx.wxEVT_COMMAND_TREE_END_LABEL_EDIT,
    function (event)
      -- veto the event to keep the original label intact as the tree
      -- is going to be refreshed with the correct names.
      event:Veto()

      local itemsrc = event:GetItem()
      if not itemsrc:IsOk() then return end

      local label = event:GetLabel():gsub("^%s+$","") -- clean all spaces

      -- edited the root element; set the new project directory if needed
      local cancelled = event:IsEditCancelled()
      if tree:IsRoot(itemsrc) then
        if not cancelled and wx.wxDirExists(label) then
          ide:SetProject(label)
        end
        return
      end

      if not parent or not parent:IsOk() then return end
      local target = MergeFullPath(tree:GetItemFullName(parent), label)
      if cancelled or label == empty then refreshAncestors(parent)
      elseif target then
        -- normally, none of this caching would be needed as `renameItem`
        -- would be called to check if the item can be renamed;
        -- however, as it may open a dialog box, on Linux it's causing a crash
        -- (caused by the same END_LABEL_EDIT even triggered one more time),
        -- so to protect from that, `renameItem` is called from IDLE event.
        -- Unfortunately, by that time, the filetree item (`itemsrc`) may
        -- already have incorrect state (as it's removed from the tree),
        -- so its properties need to be cached to be used from IDLE event.
        local cache = {
          isdir = tree:IsDirectory(itemsrc),
          isnew = tree:GetItemText(itemsrc) == empty,
          fullname = tree:GetItemFullName(itemsrc),
          parent = parent,
        }
        ide:DoWhenIdle(function()
            if not renameItem(cache, target) then refreshAncestors(parent) end
          end)
      end
    end)

  local itemsrc
  tree:Connect(wx.wxEVT_COMMAND_TREE_BEGIN_DRAG,
    function (event)
      if ide.config.filetree.mousemove and tree:GetItemParent(event:GetItem()):IsOk() then
        itemsrc = event:GetItem()
        event:Allow()
      end
    end)
  tree:Connect(wx.wxEVT_COMMAND_TREE_END_DRAG,
    function (event)
      local itemdst = event:GetItem()
      if not itemdst:IsOk() or not itemsrc:IsOk() then return end

      -- check if itemdst is a folder
      local target = tree:GetItemFullName(itemdst)
      if wx.wxDirExists(target) then
        local source = tree:GetItemFullName(itemsrc)
        -- check if moving the directory and target is a subfolder of source
        if (target..pathsep):find("^"..q(source)..pathsep) then return end
        renameItem(itemsrc, MergeFullPath(target, tree:GetItemText(itemsrc)))
      end
    end)
end

-- project
local projtree = ide:CreateTreeCtrl(ide.frame, wx.wxID_ANY,
  wx.wxDefaultPosition, wx.wxDefaultSize,
  wx.wxTR_HAS_BUTTONS + wx.wxTR_SINGLE + wx.wxTR_LINES_AT_ROOT
  + wx.wxTR_EDIT_LABELS + wx.wxNO_BORDER)
projtree:SetFont(ide.font.fNormal)
filetree.projtreeCtrl = projtree

ide:GetProjectNotebook():AddPage(projtree, TR("Project"), true)

-- proj connectors
-- ---------------

treeSetConnectorsAndIcons(projtree)

-- proj functions
-- ---------------

local function appendPathSep(dir)
  return (dir and #dir > 0 and wx.wxFileName.DirName(dir):GetFullPath() or nil)
end

function filetree:updateProjectDir(newdir)
  if (not newdir) or not wx.wxDirExists(newdir) then return nil, "Directory doesn't exist" end
  local dirname = wx.wxFileName.DirName(newdir)

  if filetree.projdir and #filetree.projdir > 0
  and dirname:SameAs(wx.wxFileName.DirName(filetree.projdir)) then return false end

  -- strip the last path separator if any
  local newdir = dirname:GetPath(wx.wxPATH_GET_VOLUME)

  -- save the current interpreter as it may be reset in onProjectClose
  -- when the project event handlers manipulates interpreters
  local intfname = ide.interpreter and ide.interpreter.fname

  if filetree.projdir and #filetree.projdir > 0 then
    PackageEventHandle("onProjectClose", appendPathSep(filetree.projdir))
  end

  PackageEventHandle("onProjectPreLoad", appendPathSep(newdir))

  if ide.config.projectautoopen and filetree.projdir then
    StoreRestoreProjectTabs(filetree.projdir, newdir, intfname)
  end

  filetree.projdir = newdir
  filetree.projdirpartmap = {}

  PrependStringToArray(
    filetree.projdirlist,
    newdir,
    ide.config.projecthistorylength,
    function(s1, s2) return dirname:SameAs(wx.wxFileName.DirName(s2)) end)

  ide:SetProject(newdir,true)
  treeSetRoot(projtree,newdir)

  -- sync with the current editor window and activate selected file
  local editor = GetEditor()
  if editor then FileTreeMarkSelected(ide:GetDocument(editor):GetFilePath()) end

  -- refresh Recent Projects menu item
  ide.frame:AddPendingEvent(wx.wxUpdateUIEvent(ID_RECENTPROJECTS))

  PackageEventHandle("onProjectLoad", appendPathSep(newdir))

  return true
end

function FileTreeGetDir() return appendPathSep(filetree.projdir) end

function FileTreeSetProjects(tab)
  filetree.projdirlist = tab
  if (tab and tab[1]) then
    filetree:updateProjectDir(tab[1])
  end
end

function FileTreeGetProjects() return filetree.projdirlist end

local function getProjectLabels()
  local labels = {}
  local fmt = ide.config.format.menurecentprojects or '%f'
  for _, proj in ipairs(FileTreeGetProjects()) do
    local config = ide.session.projects[proj]
    local intfname = config and config[2] and config[2].interpreter or ide.interpreter:GetFileName()
    local interpreter = intfname and ide.interpreters[intfname]
    local parts = wx.wxFileName(proj..pathsep):GetDirs()
    table.insert(labels, ExpandPlaceholders(fmt, {
          f = proj,
          i = interpreter and interpreter:GetName() or (intfname or '')..'?',
          s = parts[#parts] or '',
        }))
  end
  return labels
end

function FileTreeProjectListClear()
  -- remove all items from the list except the current one
  filetree.projdirlist = {FileTreeGetDir()}
end

function FileTreeProjectListUpdate(menu, items)
  -- protect against recent project menu not being present
  if not ide:FindMenuItem(ID_RECENTPROJECTS) then return end

  local list = getProjectLabels()
  for i=#list, 1, -1 do
    local id = ID("file.recentprojects."..i)
    local label = list[i]
    if i <= items then -- this is an existing item; update the label
      menu:FindItem(id):SetItemLabel(label)
    else -- need to add an item
      local item = wx.wxMenuItem(menu, id, label, "")
      menu:Insert(items, item)
      ide.frame:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, function()
          wx.wxSafeYield() -- let the menu on screen (if any) disappear
          ide:SetProject(FileTreeGetProjects()[i])
        end)
    end
    -- disable the currently selected project
    if i == 1 then menu:Enable(id, false) end
  end
  for i=items, #list+1, -1 do -- delete the rest if the list got shorter
    menu:Delete(menu:FindItemByPosition(i-1))
  end
  return #list
end

local curr_file
function FileTreeMarkSelected(file)
  if not file or not filetree.projdir or #filetree.projdir == 0
  or not ide:IsValidCtrl(projtree) then return end

  local item_id = wx.wxIsAbsolutePath(file) and projtree:FindItem(file)

  -- if the select item is different from the current one
  -- or the current one is the same, but not bold (which may happen when
  -- the project is changed to one that includes the current item)
  if curr_file ~= file
  or item_id and not projtree:IsBold(item_id) then
    if curr_file then
      local curr_id = wx.wxIsAbsolutePath(curr_file) and projtree:FindItem(curr_file)
      if curr_id and projtree:IsBold(curr_id) then
        projtree:SetItemBold(curr_id, false)
      end
    end
    if item_id then
      projtree:EnsureVisible(item_id)
      projtree:SetScrollPos(wx.wxHORIZONTAL, 0, true)
      projtree:SetItemBold(item_id, true)
    end
    curr_file = file
    if ide.wxver < "2.9.5" and ide.osname == 'Macintosh' then
      projtree:Refresh()
    end
  end
end

function FileTreeFindByPartialName(name)
  -- check if it's already cached
  if filetree.projdirpartmap[name] then return filetree.projdirpartmap[name] end

  -- this function may get a partial name that starts with ... and has
  -- an abbreviated path (as generated by stack traces);
  -- remove starting "..." if any and escape
  local pattern = q(name:gsub("^%.%.%.","")):gsub("[\\/]", "[\\/]").."$"
  local lpattern = pattern:lower()

  for _, file in ipairs(FileSysGetRecursive(filetree.projdir, true)) do
    if file:find(pattern) or iscaseinsensitive and file:lower():find(lpattern) then
      filetree.projdirpartmap[name] = file
      return file
    end
  end
  return
end

local watchers, blacklist, watcher = {}, {}
local function watchDir(path)
  if watcher and not watchers[path] and not blacklist[path] then
    local _ = wx.wxLogNull() -- disable error reporting; will report as needed
    local ok  = watcher:Add(wx.wxFileName.DirName(path),
      wx.wxFSW_EVENT_CREATE + wx.wxFSW_EVENT_DELETE + wx.wxFSW_EVENT_RENAME)
    if not ok then
      blacklist[path] = true
      ide:GetOutput():Error(("Can't set watch for '%s': %s"):format(path, wx.wxSysErrorMsg()))
      return nil, wx.wxSysErrorMsg()
    end
  end
  -- keep track of watchers even if `watcher` is not yet set to set them later
  watchers[path] = watcher ~= nil
  return watchers[path]
end
local function unWatchDir(path)
  if watcher and watchers[path] then watcher:Remove(wx.wxFileName.DirName(path)) end
  watchers[path] = nil
end
local package = ide:AddPackage('core.filetree', {
    onProjectClose = function(plugin, project)
      if watcher then watcher:RemoveAll() end
      watchers = {}
    end,

    -- watcher can only be properly setup when MainLoop is already running, so use first idle event
    onIdleOnce = function(plugin)
      if not ide.config.filetree.showchanges or not wx.wxFileSystemWatcher then return end
      if not watcher then
        watcher = wx.wxFileSystemWatcher()
        watcher:SetOwner(ide:GetMainFrame())

        local needrefresh = {}
        ide:GetMainFrame():Connect(wx.wxEVT_FSWATCHER, function(event)
            -- using `GetNewPath` to make it work with rename operations
            needrefresh[event:GetNewPath():GetFullPath()] = event:GetChangeType()
            ide:DoWhenIdle(function()
                for file, kind in pairs(needrefresh) do
                  -- if the file is removed, try to find a non-existing file in the same folder
                  -- as this will trigger a refresh of that folder
                  local path = MergeFullPath(file, kind == wx.wxFSW_EVENT_DELETE and "../\1"  or "")
                  local tree = ide:GetProjectTree() -- project tree may be hidden/disabled
                  if ide:IsValidCtrl(tree) then tree:FindItem(path) end
                end
                needrefresh = {}
              end)
          end)
      end
      -- start watching cached paths
      for path, active in pairs(watchers) do
        if not active then watchDir(path) end
      end
    end,

    -- check on PreExpand when expanding (as the folder may not expand if it's empty)
    onFiletreePreExpand = function(plugin, tree, event, item_id)
      watchDir(tree:GetItemFullName(item_id))
    end,

    -- check on Collapse when collapsing to make sure it's unwatched only when collapsed
    onFiletreeCollapse = function(plugin, tree, event, item_id)
      -- only unwatch if the directory is not empty;
      -- otherwise it's collapsed without ability to expand
      if tree:GetChildrenCount(item_id, false) > 0 then unWatchDir(tree:GetItemFullName(item_id)) end
    end,
  })
MergeSettings(filetree.settings, package:GetSettings())
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local searchpanel = 'searchpanel'
local q = EscapeMagic
local unpack = table.unpack or unpack
ide.findReplace = {
  panel = nil, -- the control for find/replace
  replace = false, -- is it a find or replace
  infiles = false,
  backfocus = nil, -- editor and position to return focus to
  cureditor = nil, -- the editor being searched
  reseditor = nil, -- the editor for search results
  oveditor = nil, -- the editor is used for search during find-in-files
  findCtrl = nil, -- the control that has the search text
  replaceCtrl = nil, -- the control that has the replace text
  scopeText = nil,
  foundString = false, -- was the string found for the last search
  curfilename = "", -- for search in files
  inselection = false,
  occurrences = 0,
  files = 0,

  settings = {
    flags = {
      WholeWord = false, -- match whole words
      MatchCase = false, -- case sensitive
      RegularExpr = false, -- use regex
      Wrap = true, -- search wraps around
      Down = true, -- search downwards in doc
      Context = true, -- include context in search results
      SubDirs = true, -- search in subdirectories
      MultiResults = false, -- show multiple result tabs
    },
    flist = {},
    rlist = {},
    slist = {},
  },

  -- HasText() is there a string to search for
  -- GetSelection() get currently selected string if it's on one line
  -- Find(reverse) find the text
  -- Show(replace) create the dialog
  -- GetEditor() which editor to use
}
local findReplace = ide.findReplace
local replaceHintText = '<replace with>'
local sep = ';'

function findReplace:GetEditor(reset)
  if reset or not ide:IsValidCtrl(self.cureditor) then self.cureditor = nil end
  self.cureditor = ide:GetEditorWithLastFocus() or self.cureditor
  return self.oveditor or self.cureditor or GetEditor()
end

-------------------- Find replace dialog

local function setSearchFlags(editor)
  local flags = wxstc.wxSTC_FIND_POSIX
  local f = findReplace.settings.flags
  if f.WholeWord then flags = flags + wxstc.wxSTC_FIND_WHOLEWORD end
  if f.MatchCase then flags = flags + wxstc.wxSTC_FIND_MATCHCASE end
  if f.RegularExpr then flags = flags + wxstc.wxSTC_FIND_REGEXP end
  editor:SetSearchFlags(flags)
end

local function setTarget(editor, flags)
  flags = flags or {}
  local fDown, fAll, fWrap = flags.Down, flags.All, flags.Wrap
  local len = editor:GetLength()
  local selStart, selEnd = editor:GetSelectionStart(), editor:GetSelectionEnd()
  local s, e
  if fDown then
    e = flags.EndPos or len
    s = math.min(e, math.max(flags.StartPos or 0, iff(fAll, selStart, selEnd)))
  else -- reverse the range for the backward search
    e = flags.StartPos or 0
    s = math.max(e, math.min(flags.EndPos or len, iff(fAll, selEnd, selStart)))
  end
  -- if wrap around and search all requested, then search the entire document
  if fAll and fWrap then s, e = 0, len end
  editor:SetTargetStart(s)
  editor:SetTargetEnd(e)
  return e
end

function findReplace:IsPreview(editor)
  local ok, ispreview = pcall(function() return editor and editor.searchpreview end)
  return ok and ispreview and true or false
end

function findReplace:CanSave(editor)
  return editor and editor:GetModify() and self:IsPreview(editor) and editor or nil
end

function findReplace:HasText()
  if not self.panel then self:createPanel() end
  local findText = self.findCtrl:GetValue()
  return findText ~= nil and #findText > 0 and findText or nil
end

function findReplace:SetStatus(msg)
  if self.status then self.status:SetLabel(msg) end
end

function findReplace:SetFind(text)
  if not self.panel then self:createPanel() end
  local ctrl = self.findCtrl
  if text and ctrl then
    if ctrl:GetValue() ~= text then ctrl:ChangeValue(text) end
    return text
  end
  return
end

function findReplace:GetFind(...) return self:HasText() end

function findReplace:GetFlags() return self.settings.flags end

function findReplace:SetReplace(text)
  if not self.panel then self:createPanel() end
  local ctrl = self.replaceCtrl
  if text and ctrl then
    if ctrl:GetValue() ~= text then ctrl:ChangeValue(text) end
    return text
  end
  return
end

function findReplace:GetScope()
  local scopeval = self.scope:GetValue()
  local dir, mask = scopeval:match(('([^%s]*)%s%%s*(.+)'):format(sep,sep))
  if not dir then dir = scopeval end
  -- strip leading/trailing spaces from the directory
  dir = dir:gsub("^%s+",""):gsub("%s+$","")
  -- if the directory doesn't exist, treat it as the extension(s)
  if not mask and not wx.wxDirExists(dir) and dir:find('%*') then
    dir, mask = ide:GetProject() or wx.wxGetCwd(), (#dir > 0 and dir or nil)
  end
  return dir, mask
end

function findReplace:SetScope(dir, mask)
  return dir .. (mask and (sep..' '..mask) or "")
end

function findReplace:GetScopeMRU(head)
  local patt, match = "^"..q(head)
  for _, v in ipairs(findReplace.settings.slist) do
    if v:find(patt) then match = v; break end
  end
  return match
end

function findReplace:GetWordAtCaret()
  local editor = self:GetEditor()
  if editor then
    local pos = editor:GetCurrentPos()
    local text = editor:GetTextRangeDyn( -- try to select a word under caret
      editor:WordStartPosition(pos, true), editor:WordEndPosition(pos, true))
    if #text == 0 then
      editor:GetTextRangeDyn( -- try to select a non-word under caret
        editor:WordStartPosition(pos, false), editor:WordEndPosition(pos, false))
    end
    return #text > 0 and text or nil
  end
  return
end

function findReplace:GetSelection()
  local editor = self:GetEditor()
  if editor then
    local startSel = editor:GetSelectionStart()
    local endSel = editor:GetSelectionEnd()
    if (startSel ~= endSel)
    and (editor:LineFromPosition(startSel) == editor:LineFromPosition(endSel)) then
      return editor:GetTextRangeDyn(startSel, endSel)
    end
  end
  return
end

function findReplace:Find(reverse)
  if not self.panel then self:createPanel() end
  local findText = self.findCtrl:GetValue()

  local msg = ""
  local editor = self:GetEditor()
  if editor and self:HasText() then
    local fDown = iff(reverse, not self:GetFlags().Down, self:GetFlags().Down)
    local bf = self.inselection and self.backfocus or {}
    setSearchFlags(editor)
    setTarget(editor, {Down = fDown, StartPos = bf.spos, EndPos = bf.epos})
    local posFind = editor:SearchInTarget(findText)
    if (posFind == wx.wxNOT_FOUND) and self:GetFlags().Wrap then
      editor:SetTargetStart(iff(fDown, bf.spos or 0, bf.epos or editor:GetLength()))
      editor:SetTargetEnd(iff(fDown, bf.epos or editor:GetLength(), bf.spos or 0))
      posFind = editor:SearchInTarget(findText)
      msg = (self.inselection
        and TR("Reached end of selection and wrapped around.")
        or TR("Reached end of text and wrapped around.")
      )
    end
    if posFind == wx.wxNOT_FOUND then
      self.foundString = false
      msg = TR("Text not found.")
    else
      self.foundString = true
      local start = editor:GetTargetStart()
      local finish = editor:GetTargetEnd()
      editor:SetSelection(start, finish)
      editor:ShowRange(finish, start)
    end
  end
  self:SetStatus(msg)
  return self.foundString
end

-- returns true if something was found
-- [inFileRegister(pos)] passing function will
-- register every position item was found

function findReplace:FindAll(inFileRegister)
  if not self.panel then self:createPanel() end
  local findText = self.findCtrl:GetValue()

  local found = false
  local editor = self:GetEditor()
  if editor and self:HasText() then
    local e = setTarget(editor, {All = true, Wrap = true})

    setSearchFlags(editor)
    while true do
      local posFind = editor:SearchInTarget(findText)
      if posFind == wx.wxNOT_FOUND then break end
      inFileRegister(posFind, editor:GetTargetEnd()-posFind)
      editor:SetTargetStart(editor:GetTargetEnd())
      editor:SetTargetEnd(e)
      found = true
    end
    if inFileRegister and found then inFileRegister() end
  end

  return found
end

local indicator = {
  SEARCHMATCH = ide:GetIndicator("core.searchmatch"),
}

-- returns true if replacements were done
function findReplace:Replace(fReplaceAll, resultsEditor)
  if not self.panel then self:createPanel() end

  local findText = self.findCtrl:GetValue()
  local replaceText = self.replaceCtrl:GetValue()
  if replaceText == replaceHintText then replaceText = "" end

  local replaced = false
  local editor = resultsEditor or self:GetEditor()
  if editor and self:HasText() then
    -- don't replace in read-only editors
    if editor:GetReadOnly() then
      self:SetStatus(TR("Can't replace in read-only text."))
      return false
    end

    -- in the preview results always replace in the entire file
    local bf = self.inselection and self.backfocus
    local endTarget = (resultsEditor and setTarget(editor, {All = true, Wrap = true})
      -- when selection is marked, only replace in the selection
      or (bf and setTarget(editor, {Down = self:GetFlags().Down, All = fReplaceAll, StartPos = bf.spos, EndPos = bf.epos}))
      -- in all other cases, replace as selected
      or setTarget(editor, {Down = self:GetFlags().Down, All = fReplaceAll, Wrap = self:GetFlags().Wrap})
    )

    if fReplaceAll then
      if resultsEditor then editor:SetIndicatorCurrent(indicator.SEARCHMATCH) end

      setSearchFlags(editor)
      local occurrences = 0
      local posFind = editor:SearchInTarget(findText)
      if posFind ~= wx.wxNOT_FOUND then
        editor:BeginUndoAction()
        while posFind ~= wx.wxNOT_FOUND do
          local length = editor:GetLength()
          -- if replace-in-files (resultsEditor) is being done,
          -- then check that the match starts with %d+:
          local match = true
          if resultsEditor then
            local line = editor:LineFromPosition(posFind)
            local _, _, prefix = editor:GetLineDyn(line):find("^(%s*%d+: )")
            match = prefix and posFind >= editor:PositionFromLine(line)+#prefix
          end
          if match then
            local replaced = self:GetFlags().RegularExpr
              and editor:ReplaceTargetRE(replaceText)
              or editor:ReplaceTarget(replaceText)

            -- mark replaced text
            if resultsEditor then editor:IndicatorFillRange(posFind, replaced) end
            occurrences = occurrences + 1
          end

          editor:SetTargetStart(editor:GetTargetEnd())
          -- adjust the endTarget as the position could have changed;
          -- can't simply subtract text length as it could be a regexp
          local adjusted = editor:GetLength() - length
          endTarget = endTarget + adjusted
          -- also adjust the selection as the end marker can move after replacement
          if bf and bf.epos then bf.epos = bf.epos + adjusted end
          editor:SetTargetEnd(endTarget)
          posFind = editor:SearchInTarget(findText)
        end
        editor:EndUndoAction()
        replaced = true
      end
      self:SetStatus(
        TR("Replaced %d instance.", occurrences):format(occurrences))
    else
      editor:TargetFromSelection()
      -- check if there is anything selected as well as the user can
      -- move the cursor after successful search
      if editor:GetSelectionStart() ~= editor:GetSelectionEnd()
      -- check that the current selection matches what's being searched for
      and editor:SearchInTarget(findText) ~= wx.wxNOT_FOUND then
        local length = editor:GetLength()
        local start = editor:GetSelectionStart()
        local replaced = self:GetFlags().RegularExpr
          and editor:ReplaceTargetRE(replaceText)
          or editor:ReplaceTarget(replaceText)
        local adjusted = editor:GetLength() - length
        if bf and bf.epos then bf.epos = bf.epos + adjusted end

        editor:SetSelection(start, start + replaced)
        self.foundString = false

        replaced = true
      end
      self:Find()
    end
  end

  return replaced
end

local oldline
local FILE_MARKER = ide:GetMarker("searchmatchfile")
local FILE_MARKER_VALUE = 2^FILE_MARKER
local function getRawLine(ed, line) return (ed:GetLineDyn(line):gsub("[\n\r]+$","")) end
local function onFileRegister(pos, length)
  local editor = findReplace.oveditor
  local reseditor = findReplace.reseditor
  local posline = pos and editor:LineFromPosition(pos) + 1
  local text = ""
  local cfg = ide.config.search
  local contextb = findReplace:GetFlags().Context and cfg.contextlinesbefore or 0
  local contexta = findReplace:GetFlags().Context and cfg.contextlinesafter or 0
  local lines = reseditor:GetLineCount() -- current number of lines

  -- check if there is another match on the same line; do not add anything
  if oldline ~= posline then
    if posline and not oldline then
      -- show file name and a bookmark marker
      reseditor:AppendTextDyn(findReplace.curfilename.."\n")
      reseditor:MarkerAdd(lines-1, FILE_MARKER)
      reseditor:SetFoldLevel(lines-1, reseditor:GetFoldLevel(lines-1)
        + wxstc.wxSTC_FOLDLEVELHEADERFLAG)
      findReplace:SetStatus(GetFileName(findReplace.curfilename))

      lines = lines + 1

      -- show context lines before posline
      for line = math.max(1, posline-contextb), posline-1 do
        text = text .. ("%5d  %s\n"):format(line, getRawLine(editor, line-1))
      end
    end
    if posline and oldline then
      -- show context lines between oldposline and posline
      for line = oldline+1, math.min(posline-1, oldline+contexta) do
        text = text .. ("%5d  %s\n"):format(line, getRawLine(editor, line-1))
      end
      if contextb + contexta > 0 and posline-oldline > contextb + contexta + 1 then
        text = text .. ("%5s\n"):format(("."):rep(#tostring(posline)))
      end
      for line = math.max(oldline+contexta+1, posline-contextb), posline-1 do
        text = text .. ("%5d  %s\n"):format(line, getRawLine(editor, line-1))
      end
    end
    if posline then
      text = text .. ("%5d: %s\n"):format(posline, getRawLine(editor, posline-1))
      findReplace.lines = findReplace.lines + 1
    elseif oldline then
      -- show context lines after posline
      for line = oldline+1, math.min(editor:GetLineCount(), oldline+contexta) do
        text = text .. ("%5d  %s\n"):format(line, getRawLine(editor, line-1))
      end
      text = text .. "\n"
    end
    oldline = posline

    reseditor:AppendTextDyn(text)

    for line = lines-1, reseditor:GetLineCount()-2 do
      reseditor:SetFoldLevel(line, wxstc.wxSTC_FOLDLEVELBASE + 1)
    end
  end

  if posline then
    findReplace.occurrences = findReplace.occurrences + 1

    -- get the added line
    local markline = reseditor:GetLineCount()-2
    -- get the match position in the file relative to the beginning of the line
    local localpos = pos - editor:PositionFromLine(posline-1)
    -- recalculate position in the search results relative to the line
    local newpos = reseditor:PositionFromLine(markline)+localpos+7 -- add indent
    reseditor:SetIndicatorCurrent(indicator.SEARCHMATCH)
    reseditor:IndicatorFillRange(newpos, length)
  end
end

local firstReadSize = 2048
local knownBinary = {}
local function checkBinary(ext, content)
  if not content then return knownBinary[ext] end
  if ext == "" then return IsBinary(content) end
  if knownBinary[ext] == nil then knownBinary[ext] = IsBinary(content) end
  return knownBinary[ext]
end

function findReplace:ProcInFiles(startdir,mask,subdirs)
  if not self.panel then self:createPanel() end

  local text = not self:GetFlags().RegularExpr and q(self.findCtrl:GetValue()) or nil
  if text and not self:GetFlags().MatchCase then
    text = text:gsub("%w",function(s) return "["..s:lower()..s:upper().."]" end)
  end

  local files = coroutine.wrap(function()
      FileSysGetRecursive(startdir, subdirs, mask, {yield = true, folder = false, skipbinary = true})
    end)
  while true do
    local file = files()
    if not file then break end

    if checkBinary(GetFileExt(file)) ~= true then
      self.curfilename = file
      local filetext, err = FileRead(file, firstReadSize)
      if not filetext then
        ide:Print(TR("Can't open file '%s': %s"):format(file, err))
      elseif not checkBinary(GetFileExt(file), filetext) then
        -- read the rest if there is more to read in the file
        if #filetext == firstReadSize then filetext = FileRead(file) end
        if filetext and (not text or filetext:find(text)) then
          self.oveditor:SetTextDyn(filetext)

          if self:FindAll(onFileRegister) then self.files = self.files + 1 end

          -- give time to the UI to refresh
          ide:Yield()
          -- the IDE may be quitting after Yield or the tab may be closed,
          local ok, mgr = pcall(function() return ide:GetUIManager() end)
          -- so check to make sure the manager is still active
          if not (ok and mgr:GetPane(searchpanel):IsShown())
          -- and check that the search results tab is still open
          or not ide:IsValidCtrl(self.reseditor) then
            return false
          end
        end
      end
    end
  end
  return true
end

local function makePlural(word, counter) return word..(counter == 1 and '' or 's') end

function findReplace:RunInFiles(replace)
  if not self.panel then self:createPanel() end
  if not self:HasText() or self.oveditor then return end

  self.oveditor = ide:CreateStyledTextCtrl(self.panel, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(0,0), wx.wxBORDER_NONE)
  self.occurrences = 0
  self.lines = 0
  self.files = 0
  self.toolbar:UpdateWindowUI(wx.wxUPDATE_UI_FROMIDLE)
  ide:Yield() -- let the update of the UI happen

  -- save focus to restore after adding a page with search results
  local ctrl = ide:GetMainFrame():FindFocus()
  local findText = self.findCtrl:GetValue()
  local flags = self:GetFlags()
  local showaseditor = ide.config.search.showaseditor
  local nb = ide:GetOutputNotebook()
  local reseditor = self.reseditor
  local resultsText = "Search Results"
  local previewText = resultsText..": "
  local valid = self:IsPreview(reseditor)
  -- open new tab if the current one is not valid
  -- or if multiple tabs are requested, but when searching for different text
  if not valid or (flags.MultiResults and reseditor.searchpreview ~= findText) then
    -- enable folds in the preview even if disabled in the editor
    local fold = ide.config.editor.fold
    ide.config.editor.fold = true
    if showaseditor then
      reseditor = NewFile(resultsText)
    else
      reseditor = ide:CreateBareEditor()
      reseditor:SetupKeywords("")

      local modpref = ide.MODPREF
      local function setModified(modified)
        local index = nb:GetPageIndex(reseditor)
        local text = nb:GetPageText(index):gsub("^"..q(modpref), "")
        nb:SetPageText(index, (modified and modpref or '')..text)
      end
      reseditor:Connect(wxstc.wxEVT_STC_SAVEPOINTREACHED,
        function () setModified(false) end)
      reseditor:Connect(wxstc.wxEVT_STC_SAVEPOINTLEFT,
        function () setModified(true) end)
      reseditor:Connect(wxstc.wxEVT_STC_MARGINCLICK,
        function (event)
          local editor = event:GetEventObject():DynamicCast('wxStyledTextCtrl')
          local line = editor:LineFromPosition(event:GetPosition())
          local header = bit.band(editor:GetFoldLevel(line),
            wxstc.wxSTC_FOLDLEVELHEADERFLAG) == wxstc.wxSTC_FOLDLEVELHEADERFLAG
          local shift, ctrl = wx.wxGetKeyState(wx.WXK_SHIFT), wx.wxGetKeyState(wx.WXK_CONTROL)
          if shift and ctrl then
            editor:FoldSome(line)
          elseif ctrl then -- select the scope that was clicked on
            local from = header and line or editor:GetFoldParent(line)
            if from > -1 then -- only select if there is a block to select
              local to = editor:GetLastChild(from, -1)
              editor:SetSelection(editor:PositionFromLine(from), editor:PositionFromLine(to+1))
            end
          elseif header or shift then
            editor:ToggleFold(line)
          end
        end)

      -- mark as searchpreview to allow AddPage to add "close" button
      reseditor.searchpreview = findText
      nb:AddPage(reseditor, previewText, true)
    end
    reseditor:SetWrapMode(wxstc.wxSTC_WRAP_NONE)
    reseditor:SetIndentationGuides(false)
    if tonumber(ide.config.search.zoom) then
      reseditor:SetZoom(tonumber(ide.config.search.zoom))
    end
    for m = 0, ide.MAXMARGIN do -- hide all margins except folding
      if reseditor:GetMarginWidth(m) > 0
      and reseditor:GetMarginMask(m) ~= wxstc.wxSTC_MASK_FOLDERS then
        reseditor:SetMarginWidth(m, 0)
      end
    end
    reseditor:MarkerDefine(ide:GetMarker("searchmatchfile"))
    reseditor:Connect(wx.wxEVT_LEFT_DCLICK, function(event)
        if not wx.wxGetKeyState(wx.WXK_SHIFT)
        and not wx.wxGetKeyState(wx.WXK_CONTROL)
        and not wx.wxGetKeyState(wx.WXK_ALT) then
          local point = event:GetPosition()
          local margin = 0
          for m = 0, ide.MAXMARGIN do margin = margin + reseditor:GetMarginWidth(m) end
          if point:GetX() <= margin then return end

          local pos = reseditor:PositionFromPoint(point)
          local line = reseditor:LineFromPosition(pos)
          local text = reseditor:GetLineDyn(line):gsub("[\n\r]+$","")
          -- get line with the line number
          local jumpline = text:match("^%s*(%d+)")
          local file
          if jumpline then
            -- search back to find the file name
            for curline = line-1, 0, -1 do
              local text = reseditor:GetLineDyn(curline):gsub("[\n\r]+$","")
              if not text:find("^%s") and wx.wxFileExists(text) then
                file = text
                break
              end
            end
          else
            file = text
            jumpline = 1
          end

          -- activate the file and the line number
          local editor = file and LoadFile(file,nil,true)
          if editor then
            editor:GotoLine(jumpline-1)
            editor:EnsureVisibleEnforcePolicy(jumpline-1)
            editor:SetFocus()
          end
          return
        end

        event:Skip()
      end)

    ide.config.editor.fold = fold
    self.reseditor = reseditor
  else
    if showaseditor then
      ide:GetDocument(reseditor):SetActive()
    else
      local index = nb:GetPageIndex(reseditor)
      if nb:GetSelection() ~= index then nb:SetSelection(index) end
    end
  end
  reseditor.replace = replace -- keep track of the current status
  reseditor:ShowLines(0, reseditor:GetLineCount()-1)
  reseditor:SetReadOnly(false)
  reseditor:SetTextDyn('')
  do -- update the preview name
    local nb = showaseditor and ide:GetEditorNotebook() or nb
    nb:SetPageText(nb:GetPageIndex(reseditor), previewText .. findText)
  end
  if not showaseditor and nb then -- show the bottom notebook if hidden
    local uimgr = ide:GetUIManager()
    if not uimgr:GetPane(nb):IsShown() then
      uimgr:GetPane(nb):Show(true)
      uimgr:Update()
    end
  end

  self:SetStatus(TR("Searching for '%s'."):format(findText))
  wx.wxSafeYield() -- allow the status to update

  local startdir, mask = self:GetScope()
  local completed = self:ProcInFiles(startdir, mask or "*", flags.SubDirs)

  -- reseditor may already be closed, so check if it's valid first
  if ide:IsValidCtrl(reseditor) then
    reseditor:GotoPos(reseditor:GetLength())
    reseditor:AppendTextDyn(("Searched for '%s'. "):format(findText))
    if not completed then reseditor:AppendTextDyn("Cancelled by the user. ") end
    reseditor:AppendTextDyn(("Found %d %s on %d %s in %d %s.")
      :format(
        self.occurrences, makePlural("instance", self.occurrences),
        self.lines, makePlural("line", self.lines),
        self.files, makePlural("file", self.files)))
    reseditor:EmptyUndoBuffer() -- don't undo the changes in the results
    reseditor:SetSavePoint() -- set unmodified status

    if completed and replace and self.occurrences > 0 then
      reseditor:AppendTextDyn("\n\n"
        .."Review the changes and save this preview to apply them.\n"
        .."You can also make other changes; only lines with : will be updated.\n"
        .."Context lines (if any) are used as safety checks during the update.")
      self:Replace(true, reseditor)
    else
      reseditor:SetReadOnly(true)
    end
    reseditor:EnsureVisibleEnforcePolicy(reseditor:GetLineCount()-1)
    reseditor.searchpreview = findText
  end

  self:SetStatus(not completed and TR("Cancelled by the user.")
    or TR("Found %d instance.", self.occurrences):format(self.occurrences))
  self.oveditor:Destroy()
  self.oveditor = nil
  self.toolbar:UpdateWindowUI(wx.wxUPDATE_UI_FROMIDLE)

  -- return focus to the control that had it if it's on the search panel
  -- (as it could be changed by added results tab)
  if ctrl and (ctrl:GetParent():GetId() == self.panel:GetId() or not showaseditor) then
    -- set the focus temporarily on the search results tab as this provides a workaround
    -- for the cursor disappearing in Search/Replace controls after results shown
    -- in the same tab (somehow caused by `oveditor:Destroy()` call).
    if ide:IsValidCtrl(reseditor) then reseditor:SetFocus() end
    ctrl:SetFocus()
  end

  if completed and ide.config.search.autohide then self:Hide() end
end

local icons = {
  find = {
    internal = {
      ID.FINDNEXT, ID.SEPARATOR,
      ID.FINDOPTDIRECTION, ID.FINDOPTWRAPWROUND, ID.FINDOPTSELECTION,
      ID.FINDOPTWORD, ID.FINDOPTCASE, ID.FINDOPTREGEX,
      ID.SEPARATOR, ID.FINDOPTSTATUS,
    },
    infiles = {
      ID.FIND, ID_SEPARATOR,
      ID.FINDOPTCONTEXT, ID.FINDOPTMULTIRESULTS, ID.FINDOPTWORD,
      ID.FINDOPTCASE, ID.FINDOPTREGEX, ID.FINDOPTSUBDIR,
      ID.FINDOPTSCOPE, ID.FINDSETDIR,
      ID.SEPARATOR, ID.FINDOPTSTATUS,
    },
  },
  replace = {
    internal = {
      ID.FINDNEXT, ID.FINDREPLACENEXT, ID.FINDREPLACEALL, ID.SEPARATOR,
      ID.FINDOPTDIRECTION, ID.FINDOPTWRAPWROUND, ID.FINDOPTSELECTION,
      ID.FINDOPTWORD, ID.FINDOPTCASE, ID.FINDOPTREGEX,
      ID.SEPARATOR, ID.FINDOPTSTATUS,
    },
    infiles = {
      ID.FIND, ID.FINDREPLACEALL, ID.SEPARATOR,
      ID.FINDOPTCONTEXT, ID.FINDOPTMULTIRESULTS, ID.FINDOPTWORD,
      ID.FINDOPTCASE, ID.FINDOPTREGEX, ID.FINDOPTSUBDIR,
      ID.FINDOPTSCOPE, ID.FINDSETDIR,
      ID.SEPARATOR, ID.FINDOPTSTATUS,
    },
  },
}

function findReplace:createToolbar()
  local ctrl, tb, scope, status =
    self.panel, self.toolbar, self.scope, self.status
  local icons = icons[self.replace and "replace" or "find"][self.infiles and "infiles" or "internal"]

  local toolBmpSize = wx.wxSize(16, 16)
  tb:Freeze()
  tb:Clear()
  for _, id in ipairs(icons) do
    if id == ID.SEPARATOR then
      tb:AddSeparator()
    elseif id == ID.FINDOPTSCOPE then
      tb:AddControl(scope)
    elseif id == ID.FINDOPTSTATUS then
      tb:AddControl(status)
    else
      local iconmap = ide.config.toolbar.iconmap[id]
      if iconmap then
        local icon, description = unpack(iconmap)
        local isbitmap = type(icon) == "userdata" and icon:GetClassInfo():GetClassName() == "wxBitmap"
        local bitmap = isbitmap and icon or ide:GetBitmap(icon, "TOOLBAR", toolBmpSize)
        tb:AddTool(id, "", bitmap, (TR)(description))
      end
    end
  end

  local options = {
    [ID.FINDOPTDIRECTION] = 'Down',
    [ID.FINDOPTWRAPWROUND] = 'Wrap',
    [ID.FINDOPTWORD] = 'WholeWord',
    [ID.FINDOPTCASE] = 'MatchCase',
    [ID.FINDOPTREGEX] = 'RegularExpr',
    [ID.FINDOPTSUBDIR] = 'SubDirs',
    [ID.FINDOPTCONTEXT] = 'Context',
    [ID.FINDOPTMULTIRESULTS] = 'MultiResults',
  }

  for id, var in pairs(options) do
    local tool = tb:FindTool(id)
    if tool then
      local flags = self:GetFlags()
      tool:SetSticky(flags[var])
      ctrl:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED,
        function ()
          flags[var] = not flags[var]
          self:SaveSettings()

          tb:FindTool(id):SetSticky(flags[var])
          tb:Refresh()
        end)
    end
  end

  local optseltool = tb:FindTool(ID.FINDOPTSELECTION)
  if optseltool then
    optseltool:SetSticky(self.inselection)
    tb:EnableTool(ID.FINDOPTSELECTION, self.inselection)
    ctrl:Connect(ID.FINDOPTSELECTION, wx.wxEVT_COMMAND_MENU_SELECTED,
      function (event)
        self.inselection = not self.inselection
        tb:FindTool(event:GetId()):SetSticky(self.inselection)
        tb:Refresh()
      end)
  end

  tb:SetToolDropDown(ID.FINDSETDIR, true)
  tb:Connect(ID.FINDSETDIR, wxaui.wxEVT_COMMAND_AUITOOLBAR_TOOL_DROPDOWN, function(event)
      if event:IsDropDownClicked() then
        local menu = wx.wxMenu({})
        local pos = tb:GetToolRect(event:GetId()):GetBottomLeft()
        menu:Append(ID.FINDSETDIR, TR("Choose..."))
        menu:Append(ID.FINDSETTOPROJDIR, TR("Set To Project Directory"))
        menu:Enable(ID.FINDSETTOPROJDIR, ide:GetProject() ~= nil)
        menu:Connect(ID.FINDSETTOPROJDIR, wx.wxEVT_COMMAND_MENU_SELECTED,
          function()
            local _, mask = self:GetScope()
            self:refreshToolbar(self:SetScope(ide:GetProject(), mask))
          end)
        if #self.settings.slist > 0 then menu:AppendSeparator() end
        for i, text in ipairs(self.settings.slist) do
          local id = ID("findreplace.scope."..i)
          menu:Append(id, text)
          menu:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED,
            function() self:refreshToolbar(text) end)
        end
        menu:AppendSeparator()
        menu:Append(ID.RECENTSCOPECLEAR, TR("Clear Items"))
        menu:Enable(ID.RECENTSCOPECLEAR, #self.settings.slist > 0)
        menu:Connect(ID.RECENTSCOPECLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
          function()
            self.settings.slist = {}
            self:SaveSettings()
          end)
        tb:PopupMenu(menu, pos)
      else
        event:Skip()
      end
    end)

  tb:Realize()
  tb:Thaw()

  local sizer = ctrl:GetSizer()
  if sizer then sizer:Layout() end
end

function findReplace:refreshToolbar(value)
  local scope = self.scope
  value = value or self.scope:GetValue()
  self.scope:SetMinSize(wx.wxSize(scope:GetTextExtent(value..'AZ'), -1))
  self:createToolbar()
  self.scope:SetValue(value)
end

function findReplace:createPanel()
  local ctrl = wx.wxPanel(ide:GetMainFrame(), wx.wxID_ANY, wx.wxDefaultPosition,
      wx.wxDefaultSize, wx.wxFULL_REPAINT_ON_RESIZE)
  local mgr = ide:GetUIManager()
  mgr:AddPane(ctrl, wxaui.wxAuiPaneInfo()
    :Name(searchpanel):CaptionVisible(false):PaneBorder(false):Hide())
  mgr:Update()

  local tb = wxaui.wxAuiToolBar(ctrl, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize, wxaui.wxAUI_TB_PLAIN_BACKGROUND)
  local status = wx.wxStaticText(tb, wx.wxID_ANY, "")
  local scope = wx.wxTextCtrl(tb, wx.wxID_ANY, "",
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wx.wxTE_PROCESS_ENTER + wx.wxTE_PROCESS_TAB + wx.wxBORDER_STATIC)
  -- limit the scope control height as it gets too large on Linux
  scope:SetMaxSize(wx.wxSize(-1, 22))

  self.panel = ctrl
  self.status = status
  self.toolbar = tb
  self.scope = scope

  self:createToolbar()

  local style, styledef = ide.config.styles, StylesGetDefault()
  local textcolor = wx.wxColour(unpack(style.text.fg or styledef.text.fg))
  local backcolor = wx.wxColour(unpack(style.text.bg or styledef.text.bg))
  local pancolor = tb:GetBackgroundColour()
  local borcolor = ide:GetUIManager():GetArtProvider():GetColour(wxaui.wxAUI_DOCKART_BORDER_COLOUR)
  local bpen = wx.wxPen(borcolor, 1, wx.wxSOLID)
  local bbrush = wx.wxBrush(pancolor, wx.wxSOLID)
  local tempctrl = ide:IsValidCtrl(ide:GetProjectTree()) and ide:GetProjectTree() or wx.wxTreeCtrl()
  local tfont = tempctrl:GetFont()
  -- don't increase font size on Linux as it gets too large
  tfont:SetPointSize(tfont:GetPointSize() + (ide.osname == 'Unix' and 0 or 1))

  local findCtrl = wx.wxTextCtrl(ctrl, wx.wxID_ANY, "",
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wx.wxTE_PROCESS_ENTER + wx.wxTE_PROCESS_TAB + wx.wxBORDER_STATIC)
  local replaceCtrl = wx.wxTextCtrl(ctrl, wx.wxID_ANY, replaceHintText,
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wx.wxTE_PROCESS_ENTER + wx.wxTE_PROCESS_TAB + wx.wxBORDER_STATIC)
  self.ac = {[findCtrl:GetId()] = {}, [replaceCtrl:GetId()] = {}, [scope:GetId()] = {}}

  local findSizer = wx.wxBoxSizer(wx.wxHORIZONTAL)
  findSizer:Add(findCtrl, 1, wx.wxLEFT + wx.wxRIGHT + wx.wxALIGN_LEFT + wx.wxEXPAND + wx.wxFIXED_MINSIZE, 1)
  findSizer:Add(replaceCtrl, 1, wx.wxLEFT + wx.wxRIGHT + wx.wxALIGN_LEFT + wx.wxEXPAND + wx.wxFIXED_MINSIZE, 1)
  findSizer:Hide(1)

  local mainSizer = wx.wxBoxSizer(wx.wxVERTICAL)
  mainSizer:Add(tb, 0, wx.wxTOP + wx.wxLEFT + wx.wxRIGHT + wx.wxALIGN_LEFT + wx.wxEXPAND, 2)
  mainSizer:Add(findSizer, 0, wx.wxALL + wx.wxALIGN_LEFT + wx.wxEXPAND, 2)

  ctrl:SetSizer(mainSizer)
  ctrl:GetSizer():Fit(ctrl)

  for _, control in ipairs({findCtrl, replaceCtrl}) do
    control:SetBackgroundColour(backcolor)
    control:SetForegroundColour(textcolor)
    control:SetFont(tfont)
  end
  scope:SetBackgroundColour(pancolor) -- set toolbar background
  scope:SetFont(tfont)
  status:SetFont(tfont)

  local function updateLists()
    PrependStringToArray(self.settings.flist, findCtrl:GetValue())
    if self.replace then
      local replaceText = replaceCtrl:GetValue()
      if replaceText == replaceHintText then replaceText = "" end
      PrependStringToArray(self.settings.rlist, replaceText)
    end
    if self.infiles then
      PrependStringToArray(self.settings.slist, self.scope:GetValue())
    end
    self:SaveSettings()
    return true
  end

  local function findNext()
    updateLists()
    if findReplace.infiles then
      findReplace:RunInFiles(false)
    else
      local reverse = (wx.wxGetKeyState(wx.WXK_SHIFT)
          and not wx.wxGetKeyState(wx.WXK_ALT) and not wx.wxGetKeyState(wx.WXK_CONTROL))
      findReplace:Find(reverse)
    end
  end

  local function autoComplete(event)
    if not ide.config.search.autocomplete then return end

    local obj = event:GetEventObject():DynamicCast('wxTextCtrl')
    local ac = self.ac[obj:GetId()]
    if not ac then return end

    local keycode, needac = ac.lastkeycode, ac.needautocomplete
    if needac then ac.needautocomplete = false end
    if not needac or not keycode then return end

    -- if the last key was Delete or Backspace, don't autocomplete
    if keycode == wx.WXK_DELETE or keycode == wx.WXK_BACK then return end

    -- find match for the current text and add it to the control
    local value = obj:GetValue()
    if not value or #value == 0 then return end

    local patt, match = "^"..q(value)
    for _, v in ipairs(
      obj:GetId() == self.findCtrl:GetId() and self.settings.flist or
      obj:GetId() == self.replaceCtrl:GetId() and self.settings.rlist or
      {}
    ) do
      if v:find(patt) then match = v; break end
    end
    if match then
      obj:ChangeValue(match)
      obj:SetSelection(#value, #match)
    end
  end

  local function findIncremental(event)
    -- don't do any incremental search when search in selection
    if self.inselection then return end

    if not self.infiles and self.backfocus and self.backfocus.position then
      self:GetEditor():SetSelection(self.backfocus.position, self.backfocus.position)
    end
    -- don't search when used with "infiles", but still trigger autocomplete
    if self.infiles or self:Find() then
      self.ac[event:GetEventObject():DynamicCast('wxTextCtrl'):GetId()].needautocomplete = true
    end
  end

  local function findReplaceNext()
    updateLists()
    if findReplace.replace then
      if findReplace.infiles then
        findReplace:RunInFiles(true)
      else
        local replaceAll = (wx.wxGetKeyState(wx.WXK_ALT)
          and not wx.wxGetKeyState(wx.WXK_SHIFT) and not wx.wxGetKeyState(wx.WXK_CONTROL))
        findReplace:Replace(replaceAll)
      end
    end
  end

  local function findReplaceAll()
    updateLists()
    if findReplace.replace then
      if findReplace.infiles then
        findReplace:RunInFiles(true)
      else
        findReplace:Replace(true)
      end
    end
  end

  local function onPanelPaint()
    local dc = wx.wxBufferedPaintDC(ctrl)
    local psize = ctrl:GetClientSize()
    dc:SetBrush(bbrush)
    dc:SetPen(bpen)
    dc:DrawRectangle(0, 0, psize:GetWidth(), psize:GetHeight())
    dc:SetPen(wx.wxNullPen)
    dc:SetBrush(wx.wxNullBrush)
    dc:delete()
  end

  ctrl:Connect(wx.wxEVT_PAINT, onPanelPaint)
  ctrl:Connect(wx.wxEVT_ERASE_BACKGROUND, function() end)

  local taborder = {findCtrl, replaceCtrl, scope}
  local function keyHandle(event)
    local keycode = event:GetKeyCode()
    self.ac[event:GetEventObject():DynamicCast('wxTextCtrl'):GetId()].lastkeycode = keycode
    if keycode == wx.WXK_ESCAPE then
      self:Hide(event:ShiftDown())
    elseif keycode == wx.WXK_TAB then
      local id = event:GetId()
      local order, pos = {}
      for _, v in ipairs(taborder) do
        if v:IsEnabled() and v:IsShown() then table.insert(order, v) end
        if v:GetId() == id then pos = #order end
      end
      if not pos then return end
      pos = pos + (event:ShiftDown() and -1 or 1)
      if pos == 0 then pos = #order
      elseif pos > #order then pos = 1
      end
      order[pos]:SetFocus()
      if order[pos] ~= scope then order[pos]:SetSelection(-1, -1) end
    else
      event:Skip()
    end
  end

  -- remember the current position in the editor when setting focus on find
  local function refreshEditorInfo()
    local ed = self:GetEditor()
    if ed and ed ~= self.oveditor then
      local spos, epos = ed:GetSelectionStart(), ed:GetSelectionEnd()
      if not self.backfocus or self.backfocus.editor ~= ed then
        self.backfocus = { editor = ed, spos = spos, epos = epos }
      end
      local bf = self.backfocus
      bf.position = spos == epos and ed:GetCurrentPos() or spos
      local inselection = ed:LineFromPosition(spos) ~= ed:LineFromPosition(epos)

      -- when the focus is changed, don't remove current "inselection" status as the
      -- selection may change to highlight the match; not doing this makes it difficult
      -- to switch between searching and replacing without losing the current match
      if inselection and (not self.inselection or bf.spos ~= spos or bf.epos ~= epos) then
        bf.spos = spos
        bf.epos = epos
        self.inselection = inselection
        self:refreshToolbar()
      end
    end
  end
  findCtrl:Connect(wx.wxEVT_SET_FOCUS,
    function(event)
      event:Skip()
      refreshEditorInfo()
    end)
  findCtrl:Connect(wx.wxEVT_COMMAND_TEXT_ENTER, findNext)
  findCtrl:Connect(wx.wxEVT_COMMAND_TEXT_UPDATED, findIncremental)
  findCtrl:Connect(wx.wxEVT_KEY_DOWN, keyHandle)
  replaceCtrl:Connect(wx.wxEVT_SET_FOCUS, function(event)
      event:Skip()
      refreshEditorInfo()
      -- hide the replace hint; should be done with SetHint method,
      -- but it's not yet available in wxlua 2.8.12
      if replaceCtrl:GetValue() == replaceHintText then replaceCtrl:ChangeValue('') end
    end)
  replaceCtrl:Connect(wx.wxEVT_COMMAND_TEXT_ENTER, findReplaceNext)
  replaceCtrl:Connect(wx.wxEVT_COMMAND_TEXT_UPDATED, function(event)
      self.ac[event:GetEventObject():DynamicCast('wxTextCtrl'):GetId()].needautocomplete = true
    end)
  replaceCtrl:Connect(wx.wxEVT_KEY_DOWN, keyHandle)

  -- autocomplete for find/replace can be done from TEXT_UPDATED event,
  -- but SetSelection doesn't work from TEXT_UPDATED event on Linux,
  -- which makes it impossible to select the suggested part.
  -- IDLE event is used instead to provide autocomplete suggestions.
  findCtrl:Connect(wx.wxEVT_IDLE, autoComplete)
  replaceCtrl:Connect(wx.wxEVT_IDLE, autoComplete)

  scope:Connect(wx.wxEVT_COMMAND_TEXT_ENTER, findNext)
  scope:Connect(wx.wxEVT_KEY_DOWN, keyHandle)

  local function notSearching(event) event:Enable(not self.oveditor) end
  ctrl:Connect(ID.FIND, wx.wxEVT_UPDATE_UI, notSearching)
  ctrl:Connect(ID.FINDNEXT, wx.wxEVT_UPDATE_UI, notSearching)
  ctrl:Connect(ID.FINDREPLACENEXT, wx.wxEVT_UPDATE_UI, notSearching)
  ctrl:Connect(ID.FINDREPLACEALL, wx.wxEVT_UPDATE_UI, notSearching)

  ctrl:Connect(ID.FIND, wx.wxEVT_COMMAND_MENU_SELECTED, findNext)
  ctrl:Connect(ID.FINDNEXT, wx.wxEVT_COMMAND_MENU_SELECTED, findNext)
  ctrl:Connect(ID.FINDREPLACENEXT, wx.wxEVT_COMMAND_MENU_SELECTED, findReplaceNext)
  ctrl:Connect(ID.FINDREPLACEALL, wx.wxEVT_COMMAND_MENU_SELECTED, findReplaceAll)

  ctrl:Connect(ID.FINDSETDIR, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      local dir, mask = self:GetScope()
      local filePicker = wx.wxDirDialog(ctrl, TR("Choose a search directory"),
        dir or wx.wxGetCwd(), wx.wxFLP_USE_TEXTCTRL)
      if filePicker:ShowModal(true) == wx.wxID_OK then
        self:refreshToolbar(self:SetScope(FixDir(filePicker:GetPath()), mask))
      end
    end)

  self.findCtrl = findCtrl
  self.replaceCtrl = replaceCtrl
  self.findSizer = findSizer
end

function findReplace:refreshPanel(replace, infiles)
  if not self.panel then self:createPanel() end

  self:GetEditor(true) -- remember the current editor

  local ctrl = self.panel

  -- check if a proper pane is already populated
  if self.replace ~= replace or self.infiles ~= infiles then
    self.replace = replace
    self.infiles = infiles

    if replace then
      self.findSizer:Show(1)
      if self.replaceCtrl:GetValue() == '' then
        self.replaceCtrl:ChangeValue(replaceHintText)
      end
    else
      self.findSizer:Hide(1)
    end
    self.findSizer:Layout()

    self.scope:Show(infiles)
  end

  local value = self.scope:GetValue()
  local ed = ide:GetEditor()
  if not value or #value == 0 then
    local doc = ed and ide:GetDocument(ed)
    local ext = doc and doc:GetFileExt() or ""
    local proj = ide:GetProject()
    value = (proj and self:GetScopeMRU(proj..sep) or
      self:SetScope(proj or wx.wxGetCwd(), '*.'..(#ext > 0 and ext or '*')))
  end
  if ed then -- check if there is any selection
    self.backfocus = nil
    self.inselection = ed:LineFromPosition(ed:GetSelectionStart()) ~=
      ed:LineFromPosition(ed:GetSelectionEnd())
  end
  self:refreshToolbar(value)

  local mgr = ide:GetUIManager()
  local pane = mgr:GetPane(searchpanel)
  if not pane:IsShown() then
    local size = ctrl:GetSize()
    pane:Dock():Bottom():BestSize(size):MinSize(size):Layer(0):Row(1):Show()
    mgr:Update()

    self:SetStatus(TR("Use %s to close."):format("`Escape`"))
  end

  -- set value from the current selection (if any)
  self.findCtrl:ChangeValue(self:GetSelection() or self.findCtrl:GetValue())

  -- reset search when re-creating dialog to avoid modifying selected
  -- fragment after successful search and updated replacement
  self.foundString = false
  self.findCtrl:SetFocus()
  self.findCtrl:SetSelection(-1, -1) -- select the content
end

function findReplace:RefreshResults(editor)
  if not ide:IsValidCtrl(editor) or not editor.searchpreview then return end

  self:Show(false, true) -- always show "Find" when refreshing
  self:SetFind(editor.searchpreview)
  editor:SetFocus() -- set the focus on the editor as it may be in an inactive tab
  self.reseditor = editor:DynamicCast("wxStyledTextCtrl") -- show the results in the same tab
  self:RunInFiles(false) -- only refresh search results, no replace
end

function findReplace:Show(replace,infiles)
  self:refreshPanel(replace,infiles)
end

function findReplace:IsShown()
  local pane = ide:GetUIManager():GetPane(searchpanel)
  return pane:IsOk() and pane:IsShown()
end

function findReplace:Hide(restorepos)
  local ctrl = self.panel:FindFocus()
  if not ctrl or ctrl:GetParent():GetId() ~= self.panel:GetId() then
    -- if focus outside of the search panel, do nothing
  elseif self.backfocus and ide:IsValidCtrl(self.backfocus.editor) then
    local editor = self.backfocus.editor
    -- restore original position for Shift-Esc or failed search
    if restorepos or self.foundString == false then
      editor:SetSelection(self.backfocus.spos, self.backfocus.epos)
    end
    editor:SetFocus()
  elseif self:IsPreview(self.reseditor) then -- there is a preview, go there
    self.reseditor:SetFocus()
  end

  local mgr = ide:GetUIManager()
  mgr:GetPane(searchpanel):Hide()
  mgr:Update()
end

local package = ide:AddPackage('core.findreplace', {
    onProjectLoad = function()
      if not findReplace.panel then return end -- not set yet
      local _, mask = findReplace:GetScope()
      local proj = ide:GetProject()
      -- find the last used scope for the same project on the scope history
      findReplace:refreshToolbar(findReplace:GetScopeMRU(proj..sep)
        or findReplace:SetScope(proj, mask))
    end,

    onEditorPreSave = function(self, editor, filePath)
      if not findReplace:IsPreview(editor) then return end

      local isModified = editor:GetModify()
      if editor.replace and isModified then
        findReplace:SetStatus("")

        local line = wx.wxNOT_FOUND
        local oveditor = ide:CreateStyledTextCtrl(findReplace.panel, wx.wxID_ANY,
          wx.wxDefaultPosition, wx.wxSize(0,0), wx.wxBORDER_NONE)
        local files, lines = 0, 0
        local report
        while true do
          -- for each marker that marks a file (MarkerNext)
          line = editor:MarkerNext(line + 1, FILE_MARKER_VALUE)
          if line == wx.wxNOT_FOUND then break end

          local fname = getRawLine(editor, line) -- get the file name
          local filetext, err = FileRead(fname)
          local mismatch = false
          if filetext then
            findReplace:SetStatus(GetFileName(fname))
            wx.wxSafeYield()

            oveditor:SetTextDyn(filetext)
            while true do -- for each line following the file name
              line = line + 1
              local text = getRawLine(editor, line)
              local lnum, lmark, ltext = text:match("^%s*(%d+)([ :]) (.*)")
              if lnum then
                lnum = tonumber(lnum)
                if lmark == ':' then -- if the change line, then apply the change
                  local pos = oveditor:PositionFromLine(lnum-1)
                  if pos == wx.wxNOT_FOUND then
                    mismatch = lnum
                    break
                  end
                  oveditor:SetTargetStart(pos)
                  oveditor:SetTargetEnd(pos+#getRawLine(oveditor, lnum-1))
                  oveditor:ReplaceTarget(ltext)
                  lines = lines + 1
                -- if the context line, then check the context
                elseif getRawLine(oveditor, lnum-1) ~= ltext then
                  mismatch = lnum
                  break
                end
              -- if not placeholder line " ...", then abort
              elseif not text:find("^%s*%.+$") then
                break
              end
            end
            if lines > 0 and not mismatch then -- save the file
              local ok
              ok, err = FileWrite(fname, oveditor:GetTextDyn())
              if ok then files = files + 1 end
            end
          end
          if err or mismatch then
            report = (report or "") .. (("\n%s: %s")
              :format(fname, mismatch and "mismatch on line "..mismatch or err))
          end
        end
        oveditor:Destroy() -- destroy the editor to release its memory
        if report then editor:AppendTextDyn("\n"..report) end
        editor:AppendTextDyn(("\n\nUpdated %d %s in %d %s.")
          :format(
            lines, makePlural("line", lines),
            files, makePlural("file", files)))
        editor:EnsureVisibleEnforcePolicy(editor:GetLineCount()-1)
        editor:SetSavePoint() -- set unmodified status when done
        findReplace:SetStatus(TR("Updated %d file.", files):format(files))
        return false

      -- don't offer to save file if called from SaveFile;
      -- can still be used with explicit SaveFileAs
      elseif not filePath and not isModified then
        return false
      end
    end
  })

function findReplace:SaveSettings() package:SetSettings(self.settings) end
MergeSettings(findReplace.settings, package:GetSettings())
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
-- Lomtik Software (J. Winwood & John Labenski)
---------------------------------------------------------

local ide = ide
local unpack = table.unpack or unpack

-- Pick some reasonable fixed width fonts to use for the editor
local function setFont(style, config)
  return wx.wxFont(config.fontsize or 10, wx.wxFONTFAMILY_MODERN, style,
    wx.wxFONTWEIGHT_NORMAL, false, config.fontname or "",
    config.fontencoding or wx.wxFONTENCODING_DEFAULT)
end
ide.font.eNormal = setFont(wx.wxFONTSTYLE_NORMAL, ide.config.editor)
ide.font.eItalic = setFont(wx.wxFONTSTYLE_ITALIC, ide.config.editor)

ide.font.oNormal = setFont(wx.wxFONTSTYLE_NORMAL, ide.config.outputshell)
ide.font.oItalic = setFont(wx.wxFONTSTYLE_ITALIC, ide.config.outputshell)

-- treeCtrl font requires slightly different handling
do local gui, config = wx.wxTreeCtrl():GetFont(), ide.config.filetree
  if config.fontsize then gui:SetPointSize(config.fontsize) end
  if config.fontname then gui:SetFaceName(config.fontname) end
  ide.font.fNormal = gui
end

-- ----------------------------------------------------------------------------
-- Create the wxFrame
-- ----------------------------------------------------------------------------
local function createFrame()
  local frame = ide:GetMainFrame() -- retrieve or create as needed
  frame:Center()

  -- update best size of the toolbar after resizing
  frame:Connect(wx.wxEVT_SIZE, function(event)
      local mgr = ide:GetUIManager()
      local toolbar = mgr:GetPane("toolbar")
      if toolbar and toolbar:IsOk() then
        toolbar:BestSize(event:GetSize():GetWidth(), ide:GetToolBar():GetClientSize():GetHeight())
        mgr:Update()
      end
    end)

  local menuBar = wx.wxMenuBar()
  local statusBar = frame:CreateStatusBar(5)
  local section_width = statusBar:GetTextExtent("OVRW")
  statusBar:SetStatusStyles({wx.wxSB_FLAT, wx.wxSB_FLAT, wx.wxSB_FLAT, wx.wxSB_FLAT, wx.wxSB_FLAT})
  statusBar:SetStatusWidths({-1, section_width, section_width, section_width*5, section_width*5})
  statusBar:SetStatusText(ide:GetProperty("statuswelcome", ""))
  statusBar:Connect(wx.wxEVT_LEFT_DOWN, function (event)
      local rect = wx.wxRect()
      statusBar:GetFieldRect(4, rect)
      if rect:Contains(event:GetPosition()) then -- click on the interpreter section
        local interpreter = ide:GetInterpreter()
        if interpreter and interpreter.takeparameters and interpreter:GetCommandLineArg() then
          rect:SetWidth(statusBar:GetTextExtent(ide:GetInterpreter():GetName()..": "))
          if not rect:Contains(event:GetPosition()) then
            local menuitem = ide:FindMenuItem(ID.COMMANDLINEPARAMETERS)
            if menuitem then
              local menu = ide:MakeMenu {
                { ID_COMMANDLINEPARAMETERS, TR("Command Line Parameters...")..KSC(ID_COMMANDLINEPARAMETERS) },
              }
              local cmdargs = ide:GetPackage("core.project"):GetCmdLines()
              local curargs = interpreter:GetCommandLineArg()
              if #cmdargs > 1 or cmdargs[1] ~= curargs then menu:PrependSeparator() end
              local function setParams(ev) ide:SetCommandLineParameters(menu:GetLabel(ev:GetId())) end
              -- do in the reverse order as `Prepend` is used;
              -- skip the currently set parameters
              for i = #cmdargs, 1, -1 do
                if cmdargs[i] ~= curargs then
                  local id = ID("status.commandparameters."..i)
                  menu:Prepend(id, cmdargs[i])
                  menu:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, setParams)
                end
              end
              statusBar:PopupMenu(menu)
            end
            return
          end
        end
        local menuitem = ide:FindMenuItem(ID.INTERPRETER)
        if menuitem then
          local menu = ide:CloneMenu(menuitem:GetSubMenu())
          if menu then statusBar:PopupMenu(menu) end
        end
      end
    end)

  local mgr = wxaui.wxAuiManager()
  mgr:SetManagedWindow(frame)
  -- allow the panes to be larger than the defalt 1/3 of the main window size
  mgr:SetDockSizeConstraint(0.8,0.8)

  frame.menuBar = menuBar
  frame.statusBar = statusBar
  frame.uimgr = mgr

  return frame
end

local function SCinB(id) -- shortcut in brackets
  local osx = ide.osname == "Macintosh"
  local shortcut = KSC(id):gsub("\t","")
  return shortcut and #shortcut > 0 and (" ("..shortcut:gsub("%f[%w]Ctrl", osx and "Cmd" or "Ctrl")..")") or ""
end

local function menuDropDownPosition(event)
  local tb = event:GetEventObject():DynamicCast('wxAuiToolBar')
  local rect = tb:GetToolRect(event:GetId())
  return ide.frame:ScreenToClient(tb:ClientToScreen(rect:GetBottomLeft()))
end

local function tbIconSize()
  -- use large icons by default on OSX and on large screens
  local iconsize = tonumber(ide.config.toolbar and ide.config.toolbar.iconsize)
  return (iconsize and (iconsize % 8) == 0 and iconsize
    or ((ide.osname == 'Macintosh' or wx.wxGetClientDisplayRect():GetWidth() >= 1500) and 24 or 16))
end

local function createToolBar(frame)
  local toolBar = wxaui.wxAuiToolBar(frame, wx.wxID_ANY, wx.wxDefaultPosition, wx.wxDefaultSize,
    wxaui.wxAUI_TB_PLAIN_BACKGROUND)

  -- there are two sets of icons: use 24 on OSX and 16 on others.
  local iconsize = tbIconSize()
  local toolBmpSize = wx.wxSize(iconsize, iconsize)
  local icons = ide.config.toolbar.icons
  local needseparator = false
  for _, id in ipairs(icons) do
    if icons[id] ~= false then -- skip explicitly disabled icons
      if id == ID.SEPARATOR and toolBar:GetToolCount() > 0 then
        needseparator = true
      else
        local iconmap = ide.config.toolbar.iconmap[id]
        if iconmap then
          if needseparator then
            toolBar:AddSeparator()
            needseparator = false
          end
          local icon, description = unpack(iconmap)
          local isbitmap = type(icon) == "userdata" and icon:GetClassInfo():GetClassName() == "wxBitmap"
          local bitmap = isbitmap and icon or ide:GetBitmap(icon, "TOOLBAR", toolBmpSize)
          toolBar:AddTool(id, "", bitmap, (TR)(description)..SCinB(id))
        end
      end
    end
  end

  toolBar:SetToolDropDown(ID_OPEN, true)
  toolBar:Connect(ID_OPEN, wxaui.wxEVT_COMMAND_AUITOOLBAR_TOOL_DROPDOWN, function(event)
      if event:IsDropDownClicked() then
        local menu = wx.wxMenu({})
        FileRecentListUpdate(menu)
        toolBar:PopupMenu(menu, menuDropDownPosition(event))
      else
        event:Skip()
      end
    end)

  toolBar:SetToolDropDown(ID_PROJECTDIRCHOOSE, true)
  toolBar:Connect(ID_PROJECTDIRCHOOSE, wxaui.wxEVT_COMMAND_AUITOOLBAR_TOOL_DROPDOWN, function(event)
      if event:IsDropDownClicked() then
        local menu = wx.wxMenu({})
        FileTreeProjectListUpdate(menu, 0)
        toolBar:PopupMenu(menu, menuDropDownPosition(event))
      else
        event:Skip()
      end
    end)

  toolBar:GetArtProvider():SetElementSize(wxaui.wxAUI_TBART_GRIPPER_SIZE, 0)
  toolBar:Realize()

  frame.toolBar = toolBar
  return toolBar
end

local function getTabWindow(event, nb)
  local tabctrl = event:GetEventObject():DynamicCast("wxAuiTabCtrl")
  local idx = event:GetSelection() -- index within the current tab ctrl
  return idx ~= wx.wxNOT_FOUND and nb:GetPageIndex(tabctrl:GetPage(idx).window) or wx.wxNOT_FOUND, tabctrl
end

local function isPreview(win)
  return ide.findReplace ~= nil and ide.findReplace:IsPreview(win)
end

local function createNotebook(frame)
  -- notebook for editors
  local notebook = wxaui.wxAuiNotebook(frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wxaui.wxAUI_NB_DEFAULT_STYLE + wxaui.wxAUI_NB_TAB_EXTERNAL_MOVE
    + wxaui.wxAUI_NB_WINDOWLIST_BUTTON + wx.wxNO_BORDER)

  -- wxEVT_SET_FOCUS could be used, but it only works on Windows with wx2.9.5+
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_PAGE_CHANGED,
    function (event)
      local ed = GetEditor(notebook:GetSelection())
      local doc = ed and ide:GetDocument(ed)

      -- skip activation when any of the following is true:
      -- (1) there is no document yet, the editor tab was just added,
      -- so no changes needed as there will be a proper later call;
      -- (2) the page change event was triggered after a tab is closed;
      -- (3) on OSX from AddPage event when changing from the last tab
      -- (this is to work around a duplicate event generated in this case
      -- that first activates the added tab and then some other tab (2.9.5)).

      local double = (ide.osname == 'Macintosh'
        and event:GetOldSelection() == notebook:GetPageCount()
        and debug:traceback():find("'AddPage'"))

      if doc and event:GetOldSelection() ~= wx.wxNOT_FOUND and not double then
        SetEditorSelection(notebook:GetSelection())
      end
    end)

  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSE,
    function (event)
      local idx = event:GetSelection()
      if idx ~= wx.wxNOT_FOUND then ClosePage(idx) end
      event:Veto() -- don't propagate the event as the page is already closed
    end)

  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_BG_DCLICK,
    function (event)
      -- as this event can be on different tab controls,
      -- need to find the control to add the page to
      local tabctrl = event:GetEventObject():DynamicCast("wxAuiTabCtrl")
      -- check if the active page is in the current control
      local active = tabctrl:GetActivePage()
      if (active >= 0 and tabctrl:GetPage(active).window
        ~= notebook:GetPage(notebook:GetSelection())) then
        -- if not, need to activate the control that was clicked on;
        -- find the last window and switch to it (assuming there is always one)
        assert(tabctrl:GetPageCount() >= 1, "Expected at least one page in a notebook tab control.")
        local lastwin = tabctrl:GetPage(tabctrl:GetPageCount()-1).window
        notebook:SetSelection(notebook:GetPageIndex(lastwin))
      end
      NewFile()
    end)

  -- tabs can be dragged around which may change their indexes;
  -- when this happens stored indexes need to be updated to reflect the change.
  -- there is DRAG_DONE event that I'd prefer to use, but it
  -- doesn't fire for some reason using wxwidgets 2.9.5 (tested on Windows).
  if ide.wxver >= "2.9.5" then
    notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_END_DRAG,
      function (event)
        for page = 0, notebook:GetPageCount()-1 do
          local editor = GetEditor(page)
          if editor then ide.openDocuments[editor:GetId()].index = page end
        end

        local selection = getTabWindow(event, notebook)
        if selection == wx.wxNOT_FOUND then return end
        -- set the selection on the dragged tab to reset its state
        -- workaround for wxwidgets issue http://trac.wxwidgets.org/ticket/15071
        notebook:SetSelection(selection)
        -- select the content of the tab after drag is done
        SetEditorSelection(selection)
        event:Skip()
      end)
  end

  local selection
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_TAB_RIGHT_UP,
    function (event)
      -- event:GetSelection() returns the index *inside the current tab*;
      -- for split notebooks, this may not be the same as the index
      -- in the notebook we are interested in here
      local idx = event:GetSelection()
      if idx == wx.wxNOT_FOUND then return end
      local tabctrl = event:GetEventObject():DynamicCast("wxAuiTabCtrl")

      -- save tab index the event is for
      selection = notebook:GetPageIndex(tabctrl:GetPage(idx).window)
      local tree = ide:GetProjectTree()
      local startfile = tree:GetStartFile()

      local menu = ide:MakeMenu {
        { ID_CLOSE, TR("&Close Page") },
        { ID_CLOSEALL, TR("Close A&ll Pages") },
        { ID_CLOSEOTHER, TR("Close &Other Pages") },
        { ID_CLOSESEARCHRESULTS, TR("Close Search Results Pages") },
        { },
        { ID_SAVE, TR("&Save") },
        { ID_SAVEAS, TR("Save &As...") },
        { },
        { ID_SETSTARTFILE, TR("Set As Start File") },
        { ID_UNSETSTARTFILE, TR("Unset '%s' As Start File"):format(startfile or "<none>") },
        { },
        { ID_COPYFULLPATH, TR("Copy Full Path") },
        { ID_SHOWLOCATION, TR("Show Location") },
        { ID_REFRESHSEARCHRESULTS, TR("Refresh Search Results") },
      }

      local fpath = ide:GetDocument(ide:GetEditor(selection)):GetFilePath()
      if not fpath or not tree:FindItem(fpath) then menu:Enable(ID_SETSTARTFILE, false) end
      if not startfile then menu:Destroy(ID_UNSETSTARTFILE) end

      PackageEventHandle("onMenuEditorTab", menu, notebook, event, selection)

      -- popup statuses are not refreshed on Linux, so do it manually
      if ide.osname == "Unix" then UpdateMenuUI(menu, notebook) end
      notebook:PopupMenu(menu)
    end)

  local function IfAtLeastOneTab(event) event:Enable(notebook:GetPageCount() > 0) end

  notebook:Connect(ID_SETSTARTFILE, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      local fpath = ide:GetDocument(ide:GetEditor(selection)):GetFilePath()
      if fpath then ide:GetProjectTree():SetStartFile(fpath) end
    end)
  notebook:Connect(ID_UNSETSTARTFILE, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:GetProjectTree():SetStartFile()
    end)
  notebook:Connect(ID_SAVE, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:GetDocument(ide:GetEditor(selection)):Save()
    end)
  notebook:Connect(ID_SAVE, wx.wxEVT_UPDATE_UI, function(event)
      local doc = ide:GetDocument(ide:GetEditor(selection))
      event:Enable(doc:IsModified() or doc:IsNew())
    end)
  notebook:Connect(ID_SAVEAS, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      SaveFileAs(ide:GetEditor(selection))
    end)
  notebook:Connect(ID_SAVEAS, wx.wxEVT_UPDATE_UI, IfAtLeastOneTab)

  -- the following three methods require handling of closing in the idle event,
  -- because of wxwidgets issue that causes crash on OSX when the last page is closed
  -- (http://trac.wxwidgets.org/ticket/15417)
  notebook:Connect(ID_CLOSE, wx.wxEVT_UPDATE_UI, IfAtLeastOneTab)
  notebook:Connect(ID_CLOSE, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:DoWhenIdle(function() ClosePage(selection) end)
    end)

  notebook:Connect(ID_CLOSEALL, wx.wxEVT_UPDATE_UI, IfAtLeastOneTab)
  notebook:Connect(ID_CLOSEALL, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:DoWhenIdle(function() CloseAllPagesExcept(nil) end)
    end)

  notebook:Connect(ID_CLOSEOTHER, wx.wxEVT_UPDATE_UI, function(event)
      event:Enable(notebook:GetPageCount() > 1)
    end)
  notebook:Connect(ID_CLOSEOTHER, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:DoWhenIdle(function() CloseAllPagesExcept(selection) end)
    end)

  notebook:Connect(ID_CLOSESEARCHRESULTS, wx.wxEVT_UPDATE_UI, function(event)
      local ispreview = false
      for p = 0, notebook:GetPageCount()-1 do
        ispreview = ispreview or isPreview(notebook:GetPage(p))
      end
      event:Enable(ispreview)
    end)
  notebook:Connect(ID_CLOSESEARCHRESULTS, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:DoWhenIdle(function()
          for p = notebook:GetPageCount()-1, 0, -1 do
            if isPreview(notebook:GetPage(p)) then ClosePage(p) end
          end
        end)
    end)

  notebook:Connect(ID_REFRESHSEARCHRESULTS, wx.wxEVT_UPDATE_UI, function(event)
      event:Enable(isPreview(notebook:GetPage(selection)))
    end)
  notebook:Connect(ID_REFRESHSEARCHRESULTS, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide.findReplace:RefreshResults(notebook:GetPage(selection))
    end)

  notebook:Connect(ID_SHOWLOCATION, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ShowLocation(ide:GetDocument(GetEditor(selection)):GetFilePath())
    end)
  notebook:Connect(ID_SHOWLOCATION, wx.wxEVT_UPDATE_UI, IfAtLeastOneTab)

  notebook:Connect(ID_COPYFULLPATH, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:CopyToClipboard(ide:GetDocument(GetEditor(selection)):GetFilePath())
    end)

  frame.notebook = notebook
  return notebook
end

local function addDND(notebook)
  -- this handler allows dragging tabs into this notebook
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_ALLOW_DND,
    function (event)
      local notebookfrom = event:GetDragSource()
      if notebookfrom ~= ide.frame.notebook then
        -- disable cross-notebook movement of specific tabs
        local idx = event:GetSelection()
        if idx == wx.wxNOT_FOUND then return end
        local win = notebookfrom:GetPage(idx)
        if not win then return end
        local winid = win:GetId()
        if (ide:IsValidCtrl(ide:GetOutput()) and winid == ide:GetOutput():GetId())
        or (ide:IsValidCtrl(ide:GetConsole()) and winid == ide:GetConsole():GetId())
        or (ide:IsValidCtrl(ide:GetProjectTree()) and winid == ide:GetProjectTree():GetId())
        or isPreview(win) -- search results preview
        then return end

        local mgr = ide.frame.uimgr
        local pane = mgr:GetPane(notebookfrom)
        if not pane:IsOk() then return end -- not a managed window
        if pane:IsFloating() then
          notebookfrom:GetParent():Hide()
        else
          pane:Hide()
          mgr:Update()
        end
        mgr:DetachPane(notebookfrom)

        -- this is a workaround for wxwidgets bug (2.9.5+) that combines
        -- content from two windows when tab is dragged over an active tab.
        local mouse = wx.wxGetMouseState()
        local mouseatpoint = wx.wxPoint(mouse:GetX(), mouse:GetY())
        local ok, tabs = pcall(function() return wx.wxFindWindowAtPoint(mouseatpoint):DynamicCast("wxAuiTabCtrl") end)
        if ok then tabs:SetNoneActive() end

        event:Allow()
      end
    end)

  -- these handlers allow dragging tabs out of this notebook.
  -- I couldn't find a good way to stop dragging event as it's not known
  -- where the event is going to end when it's started, so we manipulate
  -- the flag that allows splits and disable it when needed.
  -- It is then enabled in BEGIN_DRAG event.
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_BEGIN_DRAG,
    function (event)
      event:Skip()

      -- allow dragging if it was disabled earlier
      local flags = notebook:GetWindowStyleFlag()
      if bit.band(flags, wxaui.wxAUI_NB_TAB_SPLIT) == 0 then
        notebook:SetWindowStyleFlag(flags + wxaui.wxAUI_NB_TAB_SPLIT)
      end
    end)

  -- there is currently no support in wxAuiNotebook for dragging tabs out.
  -- This is implemented as removing a tab that was dragged out and
  -- recreating it with the right control. This is complicated by the fact
  -- that tabs can be split, so if the destination is withing the area where
  -- splits happen, the tab is not removed.
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_END_DRAG,
    function (event)
      event:Skip()

      local mgr = ide.frame.uimgr
      local win = mgr:GetPane(notebook).window
      local x = win:GetScreenPosition():GetX()
      local y = win:GetScreenPosition():GetY()
      local w, h = win:GetSize():GetWidth(), win:GetSize():GetHeight()

      local selection, tabctrl = getTabWindow(event, notebook)
      if selection == wx.wxNOT_FOUND then return end

      local mouse = wx.wxGetMouseState()
      local mx, my = mouse:GetX(), mouse:GetY()
      if mx < x or mx > x + w or my < y or my > y + h then
        -- disallow split as the target is outside the notebook
        local flags = notebook:GetWindowStyleFlag()
        if bit.band(flags, wxaui.wxAUI_NB_TAB_SPLIT) ~= 0 then
          notebook:SetWindowStyleFlag(flags - wxaui.wxAUI_NB_TAB_SPLIT)
        end

        if ide.wxver < "3.0" then
          -- don't allow dragging out single tabs from tab ctrl
          -- as wxwidgets doesn't like removing pages from split notebooks.
          if tabctrl:GetPageCount() == 1 then return end
        end

        -- don't allow last pages to be dragged out from Project and Output notebooks
        if (notebook == ide:GetProjectNotebook() or notebook == ide:GetOutputNotebook())
        and notebook:GetPageCount() == 1 then
          return
        end

        local label = notebook:GetPageText(selection)
        local pane = ide:RestorePanelByLabel(label)
        if pane then
          pane:FloatingPosition(mx-10, my-10)
          pane:Show()
          notebook:RemovePage(selection)
          mgr:Update()
          return
        end
      end

      -- set the selection on the dragged tab to reset its state
      -- workaround for wxwidgets issue http://trac.wxwidgets.org/ticket/15071
      notebook:SetSelection(selection)
      -- set focus on the content of the selected tab
      notebook:GetPage(selection):SetFocus()
    end)
end

local function createBottomNotebook(frame)
  -- bottomnotebook (errorlog,shellbox)
  local bottomnotebook = wxaui.wxAuiNotebook(frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wxaui.wxAUI_NB_DEFAULT_STYLE + wxaui.wxAUI_NB_TAB_EXTERNAL_MOVE
    - wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB + wx.wxNO_BORDER)

  addDND(bottomnotebook)

  bottomnotebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_PAGE_CHANGED,
    function (event)
      local nb = event:GetEventObject():DynamicCast("wxAuiNotebook")
      -- set focus on the new page
      local idx = event:GetSelection()
      if idx == wx.wxNOT_FOUND then return end
      nb:GetPage(idx):SetFocus()

      local preview = isPreview(nb:GetPage(nb:GetSelection()))
      local flags = nb:GetWindowStyleFlag()
      if preview and bit.band(flags, wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB) == 0 then
        nb:SetWindowStyleFlag(flags + wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
      elseif not preview and bit.band(flags, wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB) ~= 0 then
        nb:SetWindowStyleFlag(flags - wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
      end
    end)

  -- disallow tabs closing
  bottomnotebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSE,
    function (event)
      local nb = event:GetEventObject():DynamicCast("wxAuiNotebook")
      if isPreview(nb:GetPage(nb:GetSelection())) then
        event:Skip()
      else
        event:Veto()
      end
    end)

  local selection
  bottomnotebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_TAB_RIGHT_UP,
    function (event)
      -- event:GetSelection() returns the index *inside the current tab*;
      -- for split notebooks, this may not be the same as the index
      -- in the notebook we are interested in here
      local idx = event:GetSelection()
      if idx == wx.wxNOT_FOUND then return end
      local tabctrl = event:GetEventObject():DynamicCast("wxAuiTabCtrl")

      -- save tab index the event is for
      selection = bottomnotebook:GetPageIndex(tabctrl:GetPage(idx).window)

      local menu = ide:MakeMenu {
        { ID_CLOSE, TR("&Close Page") },
        { ID_CLOSESEARCHRESULTS, TR("Close Search Results Pages") },
        { },
        { ID_REFRESHSEARCHRESULTS, TR("Refresh Search Results") },
      }

      PackageEventHandle("onMenuOutputTab", menu, bottomnotebook, event, selection)

      -- popup statuses are not refreshed on Linux, so do it manually
      if ide.osname == "Unix" then UpdateMenuUI(menu, bottomnotebook) end
      bottomnotebook:PopupMenu(menu)
    end)

  bottomnotebook:Connect(ID_CLOSE, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:DoWhenIdle(function() bottomnotebook:DeletePage(selection) end)
    end)
  bottomnotebook:Connect(ID_CLOSE, wx.wxEVT_UPDATE_UI, function(event)
      event:Enable(isPreview(bottomnotebook:GetPage(selection)))
    end)

  bottomnotebook:Connect(ID_CLOSESEARCHRESULTS, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide:DoWhenIdle(function()
          for p = bottomnotebook:GetPageCount()-1, 0, -1 do
            if isPreview(bottomnotebook:GetPage(p)) then bottomnotebook:DeletePage(p) end
          end
        end)
    end)
  bottomnotebook:Connect(ID_CLOSESEARCHRESULTS, wx.wxEVT_UPDATE_UI, function(event)
      local ispreview = false
      for p = 0, bottomnotebook:GetPageCount()-1 do
        ispreview = ispreview or isPreview(bottomnotebook:GetPage(p))
      end
      event:Enable(ispreview)
    end)

  bottomnotebook:Connect(ID_REFRESHSEARCHRESULTS, wx.wxEVT_UPDATE_UI, function(event)
      event:Enable(isPreview(bottomnotebook:GetPage(selection)))
    end)
  bottomnotebook:Connect(ID_REFRESHSEARCHRESULTS, wx.wxEVT_COMMAND_MENU_SELECTED, function()
      ide.findReplace:RefreshResults(bottomnotebook:GetPage(selection))
    end)

  local errorlog = ide:CreateStyledTextCtrl(bottomnotebook, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize, wx.wxBORDER_NONE)

  errorlog:Connect(wx.wxEVT_CONTEXT_MENU,
    function (event)
      local menu = ide:MakeMenu {
          { ID_UNDO, TR("&Undo") },
          { ID_REDO, TR("&Redo") },
          { },
          { ID_CUT, TR("Cu&t") },
          { ID_COPY, TR("&Copy") },
          { ID_PASTE, TR("&Paste") },
          { ID_SELECTALL, TR("Select &All") },
          { },
          { ID_CLEAROUTPUT, TR("C&lear Output Window") },
        }
      PackageEventHandle("onMenuOutput", menu, errorlog, event)

      -- popup statuses are not refreshed on Linux, so do it manually
      if ide.osname == "Unix" then UpdateMenuUI(menu, errorlog) end
      errorlog:PopupMenu(menu)
    end)

  errorlog:Connect(ID_CLEAROUTPUT, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event) ClearOutput(true) end)

  local shellbox = ide:CreateStyledTextCtrl(bottomnotebook, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize, wx.wxBORDER_NONE)

  local menupos
  shellbox:Connect(wx.wxEVT_CONTEXT_MENU,
    function (event)
      local menu = ide:MakeMenu {
          { ID_UNDO, TR("&Undo") },
          { ID_REDO, TR("&Redo") },
          { },
          { ID_CUT, TR("Cu&t") },
          { ID_COPY, TR("&Copy") },
          { ID_PASTE, TR("&Paste") },
          { ID_SELECTALL, TR("Select &All") },
          { },
          { ID_SELECTCONSOLECOMMAND, TR("&Select Command") },
          { ID_CLEARCONSOLE, TR("C&lear Console Window") },
        }
      menupos = event:GetPosition()
      PackageEventHandle("onMenuConsole", menu, shellbox, event)

      -- popup statuses are not refreshed on Linux, so do it manually
      if ide.osname == "Unix" then UpdateMenuUI(menu, shellbox) end
      shellbox:PopupMenu(menu)
    end)

  shellbox:Connect(ID_SELECTCONSOLECOMMAND, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event) ConsoleSelectCommand(menupos) end)
  shellbox:Connect(ID_CLEARCONSOLE, wx.wxEVT_COMMAND_MENU_SELECTED,
    function(event) ide:GetConsole():Erase() end)

  bottomnotebook:AddPage(errorlog, TR("Output"), true)
  bottomnotebook:AddPage(shellbox, TR("Local console"), false)

  bottomnotebook.errorlog = errorlog
  bottomnotebook.shellbox = shellbox

  frame.bottomnotebook = bottomnotebook
  return bottomnotebook
end

local function createProjNotebook(frame)
  local projnotebook = wxaui.wxAuiNotebook(frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wxaui.wxAUI_NB_DEFAULT_STYLE + wxaui.wxAUI_NB_TAB_EXTERNAL_MOVE
    - wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB + wx.wxNO_BORDER)

  addDND(projnotebook)

  -- disallow tabs closing
  projnotebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSE,
    function (event) event:Veto() end)

  frame.projnotebook = projnotebook
  return projnotebook
end

-- ----------------------------------------------------------------------------
-- Add the child windows to the frame

local frame = createFrame()
createToolBar(frame)
createNotebook(frame)
createProjNotebook(frame)
createBottomNotebook(frame)

do
  local mgr = frame.uimgr

  mgr:AddPane(frame.toolBar, wxaui.wxAuiPaneInfo():
    Name("toolbar"):Caption("Toolbar"):
    ToolbarPane():Top():CloseButton(false):PaneBorder(false):
    LeftDockable(false):RightDockable(false))
  mgr:AddPane(frame.notebook, wxaui.wxAuiPaneInfo():
    Name("notebook"):
    CenterPane():PaneBorder(false))
  mgr:AddPane(frame.projnotebook, wxaui.wxAuiPaneInfo():
    Name("projpanel"):CaptionVisible(false):
    MinSize(200,200):FloatingSize(200,400):
    Left():Layer(1):Position(1):PaneBorder(false):
    CloseButton(true):MaximizeButton(false):PinButton(true))
  mgr:AddPane(frame.bottomnotebook, wxaui.wxAuiPaneInfo():
    Name("bottomnotebook"):CaptionVisible(false):
    MinSize(100,100):BestSize(200,200):FloatingSize(400,200):
    Bottom():Layer(1):Position(1):PaneBorder(false):
    CloseButton(true):MaximizeButton(false):PinButton(true))

  if type(ide.config.bordersize) == 'number' then
    for _, uimgr in pairs {mgr, frame.notebook:GetAuiManager(),
      frame.bottomnotebook:GetAuiManager(), frame.projnotebook:GetAuiManager()} do
      uimgr:GetArtProvider():SetMetric(wxaui.wxAUI_DOCKART_SASH_SIZE,
        ide.config.bordersize)
    end
  end

  for _, nb in pairs {frame.bottomnotebook, frame.projnotebook} do
    nb:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_BG_DCLICK,
      function() PaneFloatToggle(nb) end)
  end

  mgr.defaultPerspective = mgr:SavePerspective()
end
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
--          Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

-- Generate a unique new wxWindowID
local ID_IDCOUNTER = wx.wxID_HIGHEST + 1
function NewID()
  ID_IDCOUNTER = ID_IDCOUNTER + 1
  return ID_IDCOUNTER
end

-- some Ubuntu versions (Ubuntu 13.10) ignore labels on stock menu IDs,
-- so don't use stock IDs on Linux (http://trac.wxwidgets.org/ticket/15958)
local linux = ide.osname == 'Unix'

ID_SEPARATOR        = NewID()
-- File menu
ID_NEW              = linux and NewID() or wx.wxID_NEW
ID_OPEN             = linux and NewID() or wx.wxID_OPEN
ID_CLOSE            = NewID()
ID_CLOSEALL         = NewID()
ID_CLOSEOTHER       = NewID()
ID_CLOSESEARCHRESULTS = NewID()
ID_REFRESHSEARCHRESULTS = NewID()
ID_NEWFILE          = NewID()
ID_NEWDIRECTORY     = NewID()
ID_RENAMEFILE       = NewID()
ID_DELETEFILE       = NewID()
ID_SAVE             = linux and NewID() or wx.wxID_SAVE
ID_SAVEAS           = linux and NewID() or wx.wxID_SAVEAS
ID_SAVEALL          = NewID()
ID_RECENTFILES      = NewID()
ID_RECENTFILESCLEAR = NewID()
ID_RECENTFILESPREV  = NewID()
ID_RECENTFILESNEXT  = NewID()
ID_RECENTPROJECTS   = NewID()
ID_RECENTPROJECTSCLEAR = NewID()
ID_RECENTPROJECTSPREV = NewID()
ID_PRINT            = NewID()
ID_PAGESETUP        = NewID()
ID_EXIT             = linux and NewID() or wx.wxID_EXIT
-- Edit menu
ID_CUT              = linux and NewID() or wx.wxID_CUT
ID_COPY             = linux and NewID() or wx.wxID_COPY
ID_PASTE            = linux and NewID() or wx.wxID_PASTE
ID_SELECTALL        = linux and NewID() or wx.wxID_SELECTALL
ID_UNDO             = linux and NewID() or wx.wxID_UNDO
ID_REDO             = linux and NewID() or wx.wxID_REDO
ID_SHOWTOOLTIP      = NewID()
ID_AUTOCOMPLETE     = NewID()
ID_AUTOCOMPLETEENABLE = NewID()
ID_COMMENT          = NewID()
ID_FOLD             = NewID()
ID_FOLDLINE         = NewID()
ID_CLEARDYNAMICWORDS = NewID()
ID_SOURCE           = NewID()
ID_REINDENT         = NewID()
ID_BOOKMARK         = NewID()
ID_BOOKMARKTOGGLE   = NewID()
ID_BOOKMARKNEXT     = NewID()
ID_BOOKMARKPREV     = NewID()
ID_BOOKMARKFILECLEAR = NewID()
ID_BOOKMARKPROJECTCLEAR = NewID()
ID_NAVIGATE         = NewID()
ID_NAVIGATETOFILE   = NewID()
ID_NAVIGATETOLINE   = NewID()
ID_NAVIGATETOSYMBOL = NewID()
ID_NAVIGATETOMETHOD = NewID()
-- don't use wx.wxID_PREFERENCES to avoid merging with OSX app menu, because
-- Apple guidelines describe Preferences as a "normal" item without submenus.
ID_PREFERENCES      = NewID()
ID_PREFERENCESSYSTEM = NewID()
ID_PREFERENCESUSER  = NewID()
-- Search menu
ID_FIND             = linux and NewID() or wx.wxID_FIND
ID_FINDNEXT         = NewID()
ID_FINDPREV         = NewID()
ID_FINDSELECTNEXT   = NewID()
ID_FINDSELECTPREV   = NewID()
ID_REPLACE          = NewID()
ID_FINDINFILES      = NewID()
ID_REPLACEINFILES   = NewID()
ID_SORT             = NewID()
-- View menu
ID_VIEWFILETREE     = NewID()
ID_VIEWOUTPUT       = NewID()
ID_VIEWCALLSTACK    = NewID()
ID_VIEWWATCHWINDOW  = NewID()
ID_VIEWOUTLINE      = NewID()
ID_VIEWMARKERS      = NewID()
ID_VIEWTOOLBAR      = NewID()
ID_VIEWSTATUSBAR    = NewID()
ID_VIEWDEFAULTLAYOUT = NewID()
ID_VIEWFULLSCREEN   = NewID()
ID_VIEWMINIMIZE     = NewID()
ID_ZOOM             = NewID()
ID_ZOOMRESET        = NewID()
ID_ZOOMIN           = NewID()
ID_ZOOMOUT          = NewID()
-- Project menu
ID_BREAKPOINT       = NewID()
ID_BREAKPOINTTOGGLE = NewID()
ID_BREAKPOINTNEXT   = NewID()
ID_BREAKPOINTPREV   = NewID()
ID_BREAKPOINTFILECLEAR = NewID()
ID_BREAKPOINTPROJECTCLEAR = NewID()
ID_COMPILE          = NewID()
ID_ANALYZE          = NewID()
ID_RUN              = NewID()
ID_RUNNOW           = NewID()
ID_ATTACHDEBUG      = NewID()
ID_STARTDEBUG       = NewID()
ID_STOPDEBUG        = NewID()
ID_DETACHDEBUG      = NewID()
ID_STEP             = NewID()
ID_STEPOVER         = NewID()
ID_STEPOUT          = NewID()
ID_RUNTO            = NewID()
ID_BREAK            = NewID()
ID_TRACE            = NewID()
ID_CLEAROUTPUT      = NewID()
ID_CLEARCONSOLE     = NewID()
ID_COMMANDLINEPARAMETERS = NewID()
ID_INTERPRETER      = NewID()
ID_PROJECTDIR       = NewID()
ID_PROJECTDIRFROMFILE = NewID()
ID_PROJECTDIRFROMDIR = NewID()
ID_PROJECTDIRCHOOSE = NewID()
-- Help menu
ID_ABOUT            = linux and NewID() or wx.wxID_ABOUT
ID_HELPPROJECT      = NewID()
ID_HELPDOCUMENTATION = NewID()
ID_HELPGETTINGSTARTED = NewID()
ID_HELPTUTORIALS    = NewID()
ID_HELPFAQ          = NewID()
ID_HELPCOMMUNITY    = NewID()
-- Watch window menu items
ID_ADDWATCH         = NewID()
ID_EDITWATCH        = NewID()
ID_DELETEWATCH      = NewID()
ID_COPYWATCHVALUE   = NewID()
-- Editor popup menu items
ID_GOTODEFINITION   = NewID()
ID_RENAMEALLINSTANCES = NewID()
ID_REPLACEALLSELECTIONS = NewID()
ID_QUICKADDWATCH    = NewID()
ID_QUICKEVAL        = NewID()
ID_ADDTOSCRATCHPAD  = NewID()
-- filetree menu
ID_HIDEEXTENSION    = NewID()
ID_SETSTARTFILE     = NewID()
ID_UNSETSTARTFILE   = NewID()
ID_SHOWEXTENSION    = NewID()
ID_SHOWEXTENSIONALL = NewID()
ID_MAPDIRECTORY     = NewID()
ID_UNMAPDIRECTORY   = NewID()
ID_OPENEXTENSION    = NewID()
ID_COPYFULLPATH     = NewID()
ID_SHOWLOCATION     = NewID()
ID_REFRESH          = NewID()
ID_SYMBOLDIRREFRESH = NewID()
ID_SYMBOLDIRINDEX   = NewID()
ID_SYMBOLDIRDISABLE = NewID()
ID_SYMBOLDIRENABLE  = NewID()
-- outline menu
ID_OUTLINESORT      = NewID()
-- console menu
ID_SELECTCONSOLECOMMAND = NewID()
-- search toolbar
ID_FINDALL          = NewID()
ID_FINDREPLACENEXT  = NewID()
ID_FINDREPLACEALL   = NewID()
ID_FINDSETDIR       = NewID()
ID_FINDSETTOPROJDIR = NewID()
ID_FINDOPTSCOPE     = NewID()
ID_FINDOPTSTATUS    = NewID()
ID_FINDOPTDIRECTION = NewID()
ID_FINDOPTWRAPWROUND = NewID()
ID_FINDOPTSELECTION = NewID()
ID_FINDOPTWORD      = NewID()
ID_FINDOPTCASE      = NewID()
ID_FINDOPTREGEX     = NewID()
ID_FINDOPTCONTEXT   = NewID()
ID_FINDOPTSUBDIR    = NewID()
ID_FINDOPTMULTIRESULTS = NewID()
ID_RECENTSCOPECLEAR = NewID()
-- global shortcuts
ID_NOTEBOOKTABNEXT  = NewID()
ID_NOTEBOOKTABPREV  = NewID()

local ids = {}
function IDgen (name)
  ids[name] = ids[name] or NewID()
  return ids[name]
end
function IDget (name) return ids[name] end

ID = setmetatable({}, ide.proto.ID)
-- Copyright 2012-17 Paul Kulchenko, ZeroBrane LLC
-- Integration with LuaInspect or LuaCheck
---------------------------------------------------------

local warnings_from_string

if ide.config.staticanalyzer.luacheck then
  local config = type(ide.config.staticanalyzer.luacheck) == "table" and ide.config.staticanalyzer.luacheck or {}

  local luacheck = require("luacheck")

  -- globals only need to be generated once the API has changed.
  -- maybe this can be a module instead?

  local function build_env_from_api(tbl, out)
    out = out or {}
    for k, v in pairs(tbl) do
      if v.type ~= "keyword" then
        out[k] = {fields = v.childs and build_env_from_api(v.childs)}
      end
    end
    return out
  end

  local function build_env()
    local globals = {}

    for _, api in pairs(ide:GetInterpreter():GetAPI()) do
      -- not sure if this is how you're supposed to get an api
      local ok, tbl = pcall(require, "api/lua/" .. api)
      if ok then
        build_env_from_api(tbl, globals)
      end
    end

    return globals
  end

  warnings_from_string = function(src, file)
    local data = luacheck.check_strings({src}, config.options or {
      max_line_length = false,
      std = {
        globals = build_env(),
      },
      -- http://luacheck.readthedocs.io/en/stable/warnings.html
      ignore = config.ignore or {
        "11.", -- setting, accessing and mutating globals
        "6..", -- whitespace and style warnings
      },
    })

    -- I think luacheck can support showing multiple errors
    -- but warnings_from_string is meant to only show one
    if data.errors > 0 or data.fatals > 0 then
      local report = data[1][1]
      return nil, luacheck.get_message(report), report.line, report.column
    end

    local warnings = {}

    for _, report in ipairs(data[1]) do
      local str = luacheck.get_message(report)

      if config.reportcode then
        str = str .. "(" .. report.code .. ")"
      end

      table.insert(warnings, ("%s:%d:%d: %s"):format(
          file,
          report.line,
          report.column, -- not standard when using luainspect
          str
      ))
    end

    return warnings
  end
else
  local LA, LI, T

  local current_ast
  local current_src
  local current_file

  local function init()
    if LA then return end

    -- metalua is using 'checks', which noticeably slows the execution
    -- stab it with out own
    package.loaded.checks = {}
    checks = function() end

    LA = require "luainspect.ast"
    LI = require "luainspect.init"
    T = require "luainspect.types"
  end

  local function pos2line(pos)
    return pos and 1 + select(2, current_src:sub(1,pos):gsub(".-\n[^\n]*", ""))
  end

  local function show_warnings(top_ast, globinit)
    local warnings = {}
    local function warn(msg, linenum, path)
      warnings[#warnings+1] = (path or current_file or "?") .. ":" .. (linenum or pos2line(current_ast.pos) or 0) .. ": " .. msg
    end
    local function known(o) return not T.istype[o] end
    local function index(f) -- build abc.def.xyz name recursively
      if not f or f.tag ~= 'Index' or not f[1] or not f[2] then return end
      local main = f[1].tag == 'Id' and f[1][1] or index(f[1])
      return main and type(f[2][1]) == "string" and (main .. '.' .. f[2][1]) or nil
    end
    local globseen, isseen, fieldseen = globinit or {}, {}, {}
    LA.walk(top_ast, function(ast)
      current_ast = ast
      local path, line = tostring(ast.lineinfo):gsub('<C|','<'):match('<([^|]+)|L(%d+)')
      local name = ast[1]
      -- check if we're masking a variable in the same scope
      if ast.localmasking and name ~= '_' and
         ast.level == ast.localmasking.level then
        local linenum = ast.localmasking.lineinfo
          and tostring(ast.localmasking.lineinfo.first):match('|L(%d+)')
          or pos2line(ast.localmasking.pos)
        local parent = ast.parent and ast.parent.parent
        local func = parent and parent.tag == 'Localrec'
        warn("local " .. (func and 'function' or 'variable') .. " '" ..
          name .. "' masks earlier declaration " ..
          (linenum and "on line " .. linenum or "in the same scope"),
          line, path)
      end
      if ast.localdefinition == ast and not ast.isused and
         not ast.isignore then
        local parent = ast.parent and ast.parent.parent
        local isparam = parent and parent.tag == 'Function'
        if isparam then
          if name ~= 'self' then
            local func = parent.parent and parent.parent.parent
            local assignment = not func.tag or func.tag == 'Set' or func.tag == 'Localrec'
            -- anonymous functions can also be defined in expressions,
            -- for example, 'Op' or 'Return' tags
            local expression = not assignment and func.tag
            local func1 = func[1][1]
            local fname = assignment and func1 and type(func1[1]) == 'string'
              and func1[1] or (func1 and func1.tag == 'Index' and index(func1))
            -- "function foo(bar)" => func.tag == 'Set'
            --   `Set{{`Id{"foo"}},{`Function{{`Id{"bar"}},{}}}}
            -- "local function foo(bar)" => func.tag == 'Localrec'
            -- "local _, foo = 1, function(bar)" => func.tag == 'Local'
            -- "print(function(bar) end)" => func.tag == nil
            -- "a = a or function(bar) end" => func.tag == nil
            -- "return(function(bar) end)" => func.tag == 'Return'
            -- "function tbl:foo(bar)" => func.tag == 'Set'
            --   `Set{{`Index{`Id{"tbl"},`String{"foo"}}},{`Function{{`Id{"self"},`Id{"bar"}},{}}}}
            -- "function tbl.abc:foo(bar)" => func.tag == 'Set'
            --   `Set{{`Index{`Index{`Id{"tbl"},`String{"abc"}},`String{"foo"}}},{`Function{{`Id{"self"},`Id{"bar"}},{}}}},
            warn("unused parameter '" .. name .. "'" ..
                 (func and (assignment or expression)
                       and (fname and func.tag
                                 and (" in function '" .. fname .. "'")
                                 or " in anonymous function")
                       or ""),
                 line, path)
          end
        else
          if parent and parent.tag == 'Localrec' then -- local function foo...
            warn("unused local function '" .. name .. "'", line, path)
          else
            warn("unused local variable '" .. name .. "'; "..
                 "consider removing or replacing with '_'", line, path)
          end
        end
      end
      -- added check for "fast" mode as ast.seevalue relies on value evaluation,
      -- which is very slow even on simple and short scripts
      if ide.config.staticanalyzer.infervalue and ast.isfield
      and not(known(ast.seevalue.value) and ast.seevalue.value ~= nil) then
        local var = index(ast.parent)
        local parent = ast.parent and var
          and (" in '"..var:gsub("%."..name.."$","").."'")
          or ""
        if not fieldseen[name..parent] then
          fieldseen[name..parent] = true
          local tblref = ast.parent and ast.parent[1]
          local localparam = (tblref and tblref.localdefinition
            and tblref.localdefinition.isparam)
          if not localparam then
            warn("first use of unknown field '" .. name .."'"..parent,
              ast.lineinfo and tostring(ast.lineinfo.first):match('|L(%d+)'), path)
          end
        end
      elseif ast.tag == 'Id' and not ast.localdefinition and not ast.definedglobal then
        if not globseen[name] then
          globseen[name] = true
          local parent = ast.parent
          -- if being called and not one of the parameters
          if parent and parent.tag == 'Call' and parent[1] == ast then
            warn("first use of unknown global function '" .. name .. "'", line, path)
          else
            warn("first use of unknown global variable '" .. name .. "'", line, path)
          end
        end
      elseif ast.tag == 'Id' and not ast.localdefinition and ast.definedglobal then
        local parent = ast.parent and ast.parent.parent
        if parent and parent.tag == 'Set' and not globseen[name] -- report assignments to global
          -- only report if it is on the left side of the assignment
          -- this is a bit tricky as it can be assigned as part of a, b = c, d
          -- `Set{ {lhs+} {expr+} } -- lhs1, lhs2... = e1, e2...
          and parent[1] == ast.parent
          and parent[2][1].tag ~= "Function" then -- but ignore global functions
          warn("first assignment to global variable '" .. name .. "'", line, path)
          globseen[name] = true
        end
      elseif (ast.tag == 'Set' or ast.tag == 'Local') and #(ast[2]) > #(ast[1]) then
        warn(("value discarded in multiple assignment: %d values assigned to %d variable%s")
          :format(#(ast[2]), #(ast[1]), #(ast[1]) > 1 and 's' or ''), line, path)
      end
      local vast = ast.seevalue or ast
      local note = vast.parent
               and (vast.parent.tag == 'Call' or vast.parent.tag == 'Invoke')
               and vast.parent.note
      if note and not isseen[vast.parent] and type(name) == "string" then
        isseen[vast.parent] = true
        warn("function '" .. name .. "': " .. note, line, path)
      end
    end)
    return warnings
  end

  local function cleanError(err)
    return err and err:gsub(".-:%d+: file%s+",""):gsub(", line (%d+), char %d+", ":%1")
  end

  warnings_from_string = function(src, file)
    init()

    local ast, err, linenum, colnum = LA.ast_from_string(src, file)
    if not ast and err then return nil, cleanError(err), linenum, colnum end

    LI.uninspect(ast)
    if ide.config.staticanalyzer.infervalue then
      local tokenlist = LA.ast_to_tokenlist(ast, src)
      LI.clear_cache()
      LI.inspect(ast, tokenlist, src)
      LI.mark_related_keywords(ast, tokenlist, src)
    else
      -- stub out LI functions that depend on tokenlist,
      -- which is not built in the "fast" mode
      local ec, iv = LI.eval_comments, LI.infer_values
      LI.eval_comments, LI.infer_values = function() end, function() end

      LI.inspect(ast, nil, src)
      LA.ensure_parents_marked(ast)

      LI.eval_comments, LI.infer_values = ec, iv
    end

    local globinit = {arg = true} -- skip `arg` global variable
    local spec = ide:FindSpec(wx.wxFileName(file):GetExt())
    for k in pairs(spec and GetApi(spec.apitype or "none").ac.childs or {}) do
      globinit[k] = true
    end

    current_src = src
    current_file = file
    return show_warnings(ast, globinit)
  end
end

function AnalyzeFile(file)
  local src, err = FileRead(file)
  if not src and err then return nil, TR("Can't open file '%s': %s"):format(file, err) end

  return warnings_from_string(src, file)
end

function AnalyzeString(src, file)
  return warnings_from_string(src, file or "<string>")
end

local frame = ide.frame

-- insert after "Compile" item
local _, menu, compilepos = ide:FindMenuItem(ID_COMPILE)
if compilepos then
  menu:Insert(compilepos+1, ID_ANALYZE, TR("Analyze")..KSC(ID_ANALYZE), TR("Analyze the source code"))
end

local function analyzeProgram(editor)
  -- save all files (if requested) for "infervalue" analysis to keep the changes on disk
  if ide.config.editor.saveallonrun and ide.config.staticanalyzer.infervalue then SaveAll(true) end
  if ide:GetLaunchedProcess() == nil and not ide:GetDebugger():IsConnected() then ClearOutput() end
  ide:GetOutput():Write("Analyzing the source code")
  frame:Update()

  local editorText = editor:GetTextDyn()
  local doc = ide:GetDocument(editor)
  local filePath = doc:GetFilePath() or doc:GetFileName()
  local warn, err = warnings_from_string(editorText, filePath)
  if err then -- report compilation error
    ide:Print((": not completed.\n%s"):format(err))
    return false
  end

  ide:Print((": %s warning%s.")
    :format(#warn > 0 and #warn or 'no', #warn == 1 and '' or 's'))
  ide:GetOutput():Write(table.concat(warn, "\n") .. (#warn > 0 and "\n" or ""))

  return true -- analyzed ok
end

frame:Connect(ID_ANALYZE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    ide:GetOutput():Activate()
    local editor = GetEditor()
    if not analyzeProgram(editor) then
      CompileProgram(editor, { reportstats = false, keepoutput = true })
    end
  end)
frame:Connect(ID_ANALYZE, wx.wxEVT_UPDATE_UI,
  function (event) event:Enable(GetEditor() ~= nil) end)
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide

ide.iofilters["0d0d0aFix"] = {
  -- this function converts 0d0d0a line ending to 0d0a
  input = function(fpath, content)
    return content:gsub("\013\013\010","\013\010")
  end,
}

-- which: "input" or "output"
function GetConfigIOFilter(which)
  local filtername = ide.config.editor.iofilter
  return (filtername and ide.iofilters[filtername] and ide.iofilters[filtername][which])
end
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC

local ide = ide

--[[
Accelerator general syntax is any combination of "CTRL", "ALT", "RAWCTRL" and
"SHIFT" strings (case doesn't matter) separated by either '-' or '+' characters
and followed by the accelerator itself. The accelerator may be any alphanumeric
character, any function key (from F1 to F12) or one of the special characters
listed below (again, case doesn't matter):

  DEL/DELETE   Delete key
  INS/INSERT   Insert key
  ENTER/RETURN Enter key
  PGUP         PageUp key
  PGDN         PageDown key
  LEFT         Left cursor arrow key
  RIGHT        Right cursor arrow key
  UP           Up cursor arrow key
  DOWN         Down cursor arrow key
  HOME         Home key
  END          End key
  SPACE        Space
  TAB          Tab key
  ESC/ESCAPE   Escape key (Windows only)

"CTRL" accelerator is mapped to "Cmd" key on OSX and to "Ctrl" key on other platforms.
"RAWCTRL" accelerator is mapped to "Ctrl" key on all platforms. For example, to specify
a combination of "Ctrl" with "PGUP" use "RawCtrl-PgUp".
--]]

ide.config.keymap = {
-- File menu
  [ID.NEW]              = "Ctrl-N",
  [ID.OPEN]             = "Ctrl-O",
  [ID.CLOSE]            = "Ctrl-W",
  [ID.SAVE]             = "Ctrl-S",
  [ID.SAVEAS]           = "Alt-Shift-S",
  [ID.SAVEALL]          = "",
  [ID.RECENTFILES]      = "",
  [ID.RECENTFILESPREV]  = "Ctrl-,",
  [ID.RECENTFILESNEXT]  = "Ctrl-.",
  [ID.EXIT]             = "Ctrl-Q",
  [ID.RECENTPROJECTSPREV] = "Ctrl-Shift-<",
-- Edit menu
  [ID.CUT]              = "Ctrl-X",
  [ID.COPY]             = "Ctrl-C",
  [ID.PASTE]            = "Ctrl-V",
  [ID.SELECTALL]        = "Ctrl-A",
  [ID.UNDO]             = "Ctrl-Z",
  [ID.REDO]             = "Ctrl-Y",
  [ID.SHOWTOOLTIP]      = "Ctrl-T",
  [ID.AUTOCOMPLETE]     = "Ctrl-K",
  [ID.AUTOCOMPLETEENABLE] = "",
  [ID.COMMENT]          = "Ctrl-U",
  [ID.FOLD]             = "F12",
  [ID.FOLDLINE]         = "Shift-F12",
  [ID.CLEARDYNAMICWORDS] = "",
  [ID.REINDENT]         = "Ctrl-I",
  [ID.BOOKMARKTOGGLE]   = "Ctrl-F2",
  [ID.BOOKMARKNEXT]     = "F2",
  [ID.BOOKMARKPREV]     = "Shift-F2",
  [ID.NAVIGATETOFILE]   = "Ctrl-P",
  [ID.NAVIGATETOLINE]   = "Ctrl-G",
  [ID.NAVIGATETOSYMBOL] = "Ctrl-B",
  [ID.NAVIGATETOMETHOD] = "Ctrl-;",
-- Search menu
  [ID.FIND]             = "Ctrl-F",
  [ID.FINDNEXT]         = "F3",
  [ID.FINDPREV]         = "Shift-F3",
  [ID.FINDSELECTNEXT]   = "Ctrl-F3",
  [ID.FINDSELECTPREV]   = "Ctrl-Shift-F3",
  [ID.REPLACE]          = "Ctrl-R",
  [ID.FINDINFILES]      = "Ctrl-Shift-F",
  [ID.REPLACEINFILES]   = "Ctrl-Shift-R",
  [ID.SORT]             = "",
-- View menu
  [ID.VIEWFILETREE]     = "Ctrl-Shift-P",
  [ID.VIEWOUTPUT]       = "Ctrl-Shift-O",
  [ID.VIEWWATCHWINDOW]  = "Ctrl-Shift-W",
  [ID.VIEWCALLSTACK]    = "Ctrl-Shift-S",
  [ID.VIEWDEFAULTLAYOUT] = "",
  [ID.VIEWFULLSCREEN]   = "Ctrl-Shift-A",
  [ID.ZOOMRESET]        = "Ctrl-0",
  [ID.ZOOMIN]           = "Ctrl-+",
  [ID.ZOOMOUT]          = "Ctrl--",
-- Project menu
  [ID.RUN]              = "F6",
  [ID.RUNNOW]           = "Ctrl-F6",
  [ID.COMPILE]          = "F7",
  [ID.ANALYZE]          = "Shift-F7",
  [ID.STARTDEBUG]       = "F5",
  [ID.ATTACHDEBUG]      = "",
  [ID.DETACHDEBUG]      = "",
  [ID.STOPDEBUG]        = "Shift-F5",
  [ID.STEP]             = "F10",
  [ID.STEPOVER]         = "Shift-F10",
  [ID.STEPOUT]          = "Ctrl-F10",
  [ID.RUNTO]            = "Ctrl-Shift-F10",
  [ID.TRACE]            = "",
  [ID.BREAK]            = "",
  [ID.BREAKPOINTTOGGLE] = "Ctrl-F9",
  [ID.BREAKPOINTNEXT]   = "F9",
  [ID.BREAKPOINTPREV]   = "Shift-F9",
  [ID.CLEAROUTPUT]      = "",
  [ID.INTERPRETER]      = "",
  [ID.PROJECTDIR]       = "",
-- Help menu
  [ID.ABOUT]            = "F1",
-- Watch window menu items
  [ID.ADDWATCH]         = "Ins",
  [ID.EDITWATCH]        = "F2",
  [ID.DELETEWATCH]      = "Del",
-- Editor popup menu items
  [ID.QUICKADDWATCH]    = "",
  [ID.QUICKEVAL]        = "",
-- Filetree popup menu items
  [ID.RENAMEFILE]       = "F2",
  [ID.DELETEFILE]       = "Del",
-- Special global accelerators
  [ID.NOTEBOOKTABNEXT]  = "RawCtrl-PgDn",
  [ID.NOTEBOOKTABPREV]  = "RawCtrl-PgUp",
}

function KSC(id, default)
  -- this is only for the rare case of someone assigning a complete list
  -- to ide.config.keymap.
  local keymap = ide.config.keymap
  return (keymap[id] and "\t"..keymap[id]) or (default and "\t"..default) or ""
end

ide.config.editor.keymap = {
  -- key, modifier, command, os: http://www.scintilla.org/ScintillaDoc.html#KeyboardCommands
  -- Cmd+Left/Right moves to start/end of line
  ["Ctrl-Left"] = {wxstc.wxSTC_KEY_LEFT, wxstc.wxSTC_SCMOD_CTRL, wxstc.wxSTC_CMD_HOME, "Macintosh"},
  ["Ctrl-Right"] = {wxstc.wxSTC_KEY_RIGHT, wxstc.wxSTC_SCMOD_CTRL, wxstc.wxSTC_CMD_LINEEND, "Macintosh"},
  -- Cmd+Shift+Left/Right selects to the beginning/end of the line
  ["Ctrl-Shift-Left"] = {wxstc.wxSTC_KEY_LEFT, wxstc.wxSTC_SCMOD_CTRL+wxstc.wxSTC_SCMOD_SHIFT, wxstc.wxSTC_CMD_HOMEEXTEND, "Macintosh"},
  ["Ctrl-Shift-Right"] = {wxstc.wxSTC_KEY_RIGHT, wxstc.wxSTC_SCMOD_CTRL+wxstc.wxSTC_SCMOD_SHIFT, wxstc.wxSTC_CMD_LINEENDEXTEND, "Macintosh"},
  -- Cmd+Shift+Up/Down selects to the beginning/end of the text
  ["Ctrl-Shift-Up"] = {wxstc.wxSTC_KEY_UP, wxstc.wxSTC_SCMOD_CTRL+wxstc.wxSTC_SCMOD_SHIFT, wxstc.wxSTC_CMD_LINEUPEXTEND, "Macintosh"},
  ["Ctrl-Shift-Down"] = {wxstc.wxSTC_KEY_DOWN, wxstc.wxSTC_SCMOD_CTRL+wxstc.wxSTC_SCMOD_SHIFT, wxstc.wxSTC_CMD_LINEDOWNEXTEND, "Macintosh"},
  -- Opt+Left/Right moves one word left (to the beginning)/right (to the end)
  ["Alt-Left"] = {wxstc.wxSTC_KEY_LEFT, wxstc.wxSTC_SCMOD_ALT, wxstc.wxSTC_CMD_WORDLEFT, "Macintosh"},
  ["Alt-Right"] = {wxstc.wxSTC_KEY_RIGHT, wxstc.wxSTC_SCMOD_ALT, wxstc.wxSTC_CMD_WORDRIGHTEND, "Macintosh"},
}
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

-- put bin/ and lualibs/ first to avoid conflicts with included modules
-- that may have other versions present somewhere else in path/cpath.
local function isproc()
  local file = io.open("/proc")
  if file then file:close() end
  return file ~= nil
end
local iswindows = os.getenv('WINDIR') or (os.getenv('OS') or ''):match('[Ww]indows')
local islinux = not iswindows and isproc()
local arch = "x86" -- use 32bit by default
local unpack = table.unpack or unpack

if islinux then
  local file = io.popen("uname -m")
  if file then
    local machine=file:read("*l")
    local archtype= {
      x86_64  = "x64",
      armv7l  = "armhf",
      aarch64 = "aarch64",
    }
    arch = archtype[machine] or arch
    file:close()
  end
end

package.cpath = (
  iswindows and 'bin/clibs/?.dll;' or
  islinux and ('bin/linux/%s/clibs/lib?.so;bin/linux/%s/clibs/?.so;'):format(arch,arch) or
  --[[isosx]] 'bin/clibs/lib?.dylib;bin/clibs/?.dylib;')
    .. package.cpath
package.path  = 'lualibs/?.lua;lualibs/?/?.lua;lualibs/?/init.lua;lualibs/?/?/?.lua;lualibs/?/?/init.lua;'
              .. package.path

require("wx")
require("bit")
require("mobdebug")
if jit and jit.on then jit.on() end -- turn jit "on" as "mobdebug" may turn it off for LuaJIT

dofile "src/util.lua"

-----------
-- IDE
--
local pendingOutput = {}
local config = dofile("src/config.lua")
config.path = {
  projectdir = "",
  app = nil,
}
ide = {
  MODPREF = "* ",
  MAXMARGIN = wxstc.wxSTC_MAX_MARGIN or 4,
  ANYMARKERMASK = 2^24-1,
  config = config,
  specs = {
    none = {
      sep = "\1",
    }
  },
  messages = {},
  tools = {},
  iofilters = {},
  interpreters = {},
  packages = {},
  apis = {},
  timers = {},
  onidle = {},

  proto = {}, -- prototypes for various classes

  app = nil, -- application engine
  interpreter = nil, -- current Lua interpreter
  frame = nil, -- gui related
  debugger = {}, -- debugger related info
  filetree = nil, -- filetree
  findReplace = nil, -- find & replace handling
  settings = nil, -- user settings (window pos, last files..)
  session = {
    projects = {}, -- project configuration for the current session
    lastupdated = nil, -- timestamp of the last modification in any of the editors
    lastsaved = nil, -- timestamp of the last recovery information saved
  },

  -- misc
  exitingProgram = false, -- are we currently exiting, ID_EXIT
  infocus = nil, -- last component with a focus
  editorApp = wx.wxGetApp(),
  editorFilename = nil,
  openDocuments = {},-- open notebook editor documents[winId] = {
  -- editor = wxStyledTextCtrl,
  -- index = wxNotebook page index,
  -- filePath = full filepath, nil if not saved,
  -- fileName = just the filename,
  -- modTime = wxDateTime of disk file or nil,
  -- isModified = bool is the document modified? }
  ignoredFilesList = {},
  font = {
    eNormal = nil,
    eItalic = nil,
    oNormal = nil,
    oItalic = nil,
    fNormal = nil,
  },

  osname = wx.wxPlatformInfo.Get():GetOperatingSystemFamilyName(),
  osarch = arch,
  oshome = os.getenv("HOME") or (iswindows and os.getenv('HOMEDRIVE') and os.getenv('HOMEPATH')
    and (os.getenv('HOMEDRIVE')..os.getenv('HOMEPATH'))),
  wxver = string.match(wx.wxVERSION_STRING, "[%d%.]+"),

  startedat = TimeGet(),
  test = {}, -- local functions used for testing

  Print = function(self, ...)
    if DisplayOutputLn then
      -- flush any pending output
      while #pendingOutput > 0 do DisplayOutputLn(unpack(table.remove(pendingOutput, 1))) end
      -- print without parameters can be used for flushing, so skip the printing
      if select('#', ...) > 0 then DisplayOutputLn(...) end
      return
    end
    pendingOutput[#pendingOutput + 1] = {...}
  end,
}
-- Scintilla switched to using full byte for style numbers from using only first 5 bits
ide.STYLEMASK = ide.wxver <= "2.9.5" and 31 or 255

-- add wx.wxMOD_RAW_CONTROL as it's missing in wxlua 2.8.12.3;
-- provide default for wx.wxMOD_CONTROL as it's missing in wxlua 2.8 that
-- is available through Linux package managers
if not wx.wxMOD_CONTROL then wx.wxMOD_CONTROL = 0x02 end
if not wx.wxMOD_RAW_CONTROL then
  wx.wxMOD_RAW_CONTROL = ide.osname == 'Macintosh' and 0x10 or wx.wxMOD_CONTROL
end
if not wx.WXK_RAW_CONTROL then
  wx.WXK_RAW_CONTROL = ide.osname == 'Macintosh' and 396 or wx.WXK_CONTROL
end
-- ArchLinux running 2.8.12.2 doesn't have wx.wxMOD_SHIFT defined
if not wx.wxMOD_SHIFT then wx.wxMOD_SHIFT = 0x04 end
-- wxDIR_NO_FOLLOW is missing in wxlua 2.8.12 as well
if not wx.wxDIR_NO_FOLLOW then wx.wxDIR_NO_FOLLOW = 0x10 end
if not wxaui.wxAUI_TB_PLAIN_BACKGROUND then wxaui.wxAUI_TB_PLAIN_BACKGROUND = 2^8 end
if not wx.wxNOT_FOUND then wx.wxNOT_FOUND = -1 end
if not wx.wxEXEC_NOEVENTS then wx.wxEXEC_NOEVENTS = 16 end
if not wx.wxEXEC_HIDE_CONSOLE then wx.wxEXEC_HIDE_CONSOLE = 32 end
if not wx.wxEXEC_BLOCK then wx.wxEXEC_BLOCK = wx.wxEXEC_SYNC + wx.wxEXEC_NOEVENTS end

-- it's an interface constant and is not public in wxlua, so add it
if not wxstc.wxSTC_SETLEXERLANGUAGE then wxstc.wxSTC_SETLEXERLANGUAGE = 4006 end

if not setfenv then -- Lua 5.2
  -- based on http://lua-users.org/lists/lua-l/2010-06/msg00314.html
  -- this assumes f is a function
  local function findenv(f)
    local level = 1
    repeat
      local name, value = debug.getupvalue(f, level)
      if name == '_ENV' then return level, value end
      level = level + 1
    until name == nil
    return nil end
  getfenv = function (f) return(select(2, findenv(f)) or _G) end
  setfenv = function (f, t)
    local level = findenv(f)
    if level then debug.setupvalue(f, level, t) end
    return f end
end

if not package.searchpath then
  -- from Scintillua by Mitchell (mitchell.att.foicica.com).
  -- Searches for the given *name* in the given *path*.
  -- This is an implementation of Lua 5.2's `package.searchpath()` function for Lua 5.1.
  function package.searchpath(name, path)
    local tried = {}
    for part in path:gmatch('[^;]+') do
      local filename = part:gsub('%?', name)
      local f = io.open(filename, 'r')
      if f then f:close() return filename end
      tried[#tried + 1] = ("no file '%s'"):format(filename)
    end
    return nil, table.concat(tried, '\n')
  end
end

local function loadToTab(folder, tab, recursive, proto)
  local files = (wx.wxFileExists(folder) and {folder}
    or wx.wxDirExists(folder) and FileSysGetRecursive(folder, recursive, "*.lua")
    or {})
  for _, file in ipairs(files) do LoadLuaFileExt(tab, file, proto) end
  return tab
end

function ide:LoadSpec(path)
  loadToTab(path or "spec", ide.specs, true)
  UpdateSpecs()
  -- force reload of all APIs as some of them may depend on the specs
  if ReloadAPIs then ReloadAPIs("*") end
end

function ide:LoadTool(path)
  local tools = {}
  for name,tool in pairs(loadToTab(path or "tools", {}, false)) do
    if tool.fninit then
      local ok, err = pcall(tool.fninit, ide:GetMainFrame(), ide:GetMenuBar())
      if not ok then ide:Print(("Error when initializing tool %s: %s"):format(name, err)) end
    end
    if tool.exec and tool.exec.name then table.insert(tools,tool) end
  end

  -- sort tools
  table.sort(tools,function(a,b) return a.exec.name < b.exec.name end)

  for _, tool in ipairs(tools) do
    -- add menus for each
    local id, menu = ide:AddTool(tool.exec.name, tool.exec.fn)
    -- add descriptions
    if id and tool.exec.description then menu:SetHelpString(id, tool.exec.description) end
  end

  return #tools
end

function ide:LoadInterpreter(path)
  loadToTab(path or "interpreters", ide.interpreters, false, ide.proto.Interpreter)
end

function ide:LoadAPI(path)
  local folder = path or "api"
  local files = (wx.wxFileExists(folder) and {folder}
    or wx.wxDirExists(folder) and FileSysGetRecursive(folder, true, "*.lua")
    or {})
  for _, file in ipairs(files) do
    if not IsDirectory(file) then
      local ftype, fname = file:match("api[/\\]([^/\\]+)[/\\](.*)%.")
      if not ftype or not fname then
        ide:Print(TR("The API file must be located in a subdirectory of the API directory."))
      else
        ide.apis[ftype] = ide.apis[ftype] or {}
        -- make sure the path is absolute to access it if the current directory changes
        ide.apis[ftype][fname] = MergeFullPath("", file)
      end
    end
  end
  if ReloadAPIs then ReloadAPIs("*") end
end

dofile "src/version.lua"

for _, file in ipairs({"proto", "ids", "style", "keymap", "toolbar", "package"}) do
  dofile("src/editor/"..file..".lua")
end

ide.config.styles = StylesGetDefault()
ide.config.stylesoutshell = StylesGetDefault()

local function setLuaPaths(mainpath, osname)
  -- use LUA_DEV to setup paths for Lua for Windows modules if installed
  local luadev = osname == "Windows" and os.getenv('LUA_DEV')
  if luadev and not wx.wxDirExists(luadev) then luadev = nil end
  local luadev_path = (luadev
    and ('LUA_DEV/?.lua;LUA_DEV/?/init.lua;LUA_DEV/lua/?.lua;LUA_DEV/lua/?/init.lua')
      :gsub('LUA_DEV', (luadev:gsub('[\\/]$','')))
    or nil)
  local luadev_cpath = (luadev
    and ('LUA_DEV/?.dll;LUA_DEV/?51.dll;LUA_DEV/clibs/?.dll;LUA_DEV/clibs/?51.dll')
      :gsub('LUA_DEV', (luadev:gsub('[\\/]$','')))
    or nil)

  if luadev then
    local path, clibs = os.getenv('PATH'), luadev:gsub('[\\/]$','')..'\\clibs'
    if not path:find(clibs, 1, true) then wx.wxSetEnv('PATH', path..';'..clibs) end
  end

  -- (luaconf.h) in Windows, any exclamation mark ('!') in the path is replaced
  -- by the path of the directory of the executable file of the current process.
  -- this effectively prevents any path with an exclamation mark from working.
  -- if the path has an excamation mark, allow Lua to expand it as this
  -- expansion happens only once.
  if osname == "Windows" and mainpath:find('%!') then mainpath = "!/../" end

  -- if LUA_PATH or LUA_CPATH is not specified, then add ;;
  -- ;; will be replaced with the default (c)path by the Lua interpreter
  wx.wxSetEnv("LUA_PATH",
    (os.getenv("LUA_PATH") or ';') .. ';'
    .. "./?.lua;./?/init.lua;./lua/?.lua;./lua/?/init.lua" .. ';'
    .. mainpath.."lualibs/?/?.lua;"..mainpath.."lualibs/?.lua;"
    .. mainpath.."lualibs/?/?/init.lua;"..mainpath.."lualibs/?/init.lua"
    .. (luadev_path and (';' .. luadev_path) or ''))

  ide.osclibs = -- keep the list to use for various Lua versions
    osname == "Windows" and table.concat({
        mainpath.."bin/clibs/?.dll",
      },";") or
    osname == "Macintosh" and table.concat({
        mainpath.."bin/clibs/?.dylib",
        mainpath.."bin/clibs/lib?.dylib",
      },";") or
    osname == "Unix" and table.concat({
        mainpath..("bin/linux/%s/clibs/?.so"):format(arch),
        mainpath..("bin/linux/%s/clibs/lib?.so"):format(arch),
      },";") or
    assert(false, "Unexpected OS name")

  wx.wxSetEnv("LUA_CPATH",
    (os.getenv("LUA_CPATH") or ';') .. ';' .. ide.osclibs
    .. (luadev_cpath and (';' .. luadev_cpath) or ''))

  -- on some OSX versions, PATH is sanitized to not include even /usr/local/bin; add it
  if osname == "Macintosh" then
    local ok, path = wx.wxGetEnv("PATH")
    if ok then wx.wxSetEnv("PATH", (#path > 0 and path..":" or "").."/usr/local/bin") end
  end
end

ide.test.setLuaPaths = setLuaPaths

---------------
-- process args
local filenames = {}
local configs = {}
do
  local arg = {...}
  -- application name is expected as the first argument
  local fullPath = arg[1] or "zbstudio"

  ide.arg = arg

  -- on Windows use GetExecutablePath, which is Unicode friendly,
  -- whereas wxGetCwd() is not (at least in wxlua 2.8.12.2).
  -- some wxlua version on windows report wx.dll instead of *.exe.
  local exepath = wx.wxStandardPaths.Get():GetExecutablePath()
  if ide.osname == "Windows" and exepath:find("%.exe$") then
    fullPath = exepath
  -- path handling only works correctly on UTF8-valid strings, so check for that.
  -- This may be caused by the launcher on Windows using ANSI methods for command line
  -- processing. Keep the path as is for UTF-8 invalid strings as it's still good enough
  elseif not wx.wxIsAbsolutePath(fullPath) and wx.wxString().FromUTF8(fullPath) == fullPath then
    fullPath = MergeFullPath(wx.wxGetCwd(), fullPath)
  end

  ide.editorFilename = fullPath
  ide.appname = fullPath:match("([%w_-%.]+)$"):gsub("%.[^%.]*$","")
  assert(ide.appname, "no application path defined")

  for index = 2, #arg do
    if (arg[index] == "-cfg" and index+1 <= #arg) then
      table.insert(configs,arg[index+1])
    elseif arg[index-1] ~= "-cfg"
    -- on OSX command line includes -psn... parameter, don't include these
    and (ide.osname ~= 'Macintosh' or not arg[index]:find("^-psn")) then
      table.insert(filenames,arg[index])
    end
  end

  setLuaPaths(GetPathWithSep(ide.editorFilename), ide.osname)
end

----------------------
-- process application

ide.app = dofile(ide.appname.."/app.lua")
local app = assert(ide.app)

-- load packages
local function processPackages(packages)
  -- check dependencies and assign file names to each package
  local skip = {}
  for fname, package in pairs(packages) do
    if type(package.dependencies) == 'table'
    and package.dependencies.osname
    and not package.dependencies.osname:find(ide.osname, 1, true) then
      ide:Print(("Package '%s' not loaded: requires %s platform, but you are running %s.")
        :format(fname, package.dependencies.osname, ide.osname))
      skip[fname] = true
    end

    local needsversion = tonumber(package.dependencies)
      or type(package.dependencies) == 'table' and tonumber(package.dependencies[1])
      or -1
    local isversion = tonumber(ide.VERSION)
    if isversion and needsversion > isversion then
      ide:Print(("Package '%s' not loaded: requires version %s, but you are running version %s.")
        :format(fname, needsversion, ide.VERSION))
      skip[fname] = true
    end
    package.fname = fname
  end

  for fname, package in pairs(packages) do
    if not skip[fname] then ide.packages[fname] = package end
  end
end

function UpdateSpecs(spec)
  for _, spec in pairs(spec and {spec} or ide.specs) do
    spec.sep = spec.sep or "\1" -- default separator doesn't match anything
    spec.iscomment = {}
    spec.iskeyword = {}
    spec.isstring = {}
    spec.isnumber = {}
    if spec.lexerstyleconvert then
      for _, s in pairs(spec.lexerstyleconvert.comment or {}) do spec.iscomment[s] = true end
      for _, s in pairs(spec.lexerstyleconvert.keywords0 or {}) do spec.iskeyword[s] = true end
      for _, s in pairs(spec.lexerstyleconvert.stringtxt or {}) do spec.isstring[s] = true end
      for _, s in pairs(spec.lexerstyleconvert.number or {}) do spec.isnumber[s] = true end
    end
  end
end

----------------------
-- process config

-- set ide.config environment
do
  ide.configs = {
    system = MergeFullPath("cfg", "user.lua"),
    user = ide.oshome and MergeFullPath(ide.oshome, "."..ide.appname.."/user.lua"),
  }
  ide.configqueue = {}

  local num = 0
  local package = setmetatable({}, {
      __index = function(_,k) return package[k] end,
      __newindex = function(_,k,v) package[k] = v end,
      __call = function(_,p)
        -- package can be defined inline, like "package {...}"
        if type(p) == 'table' then
          num = num + 1
          return ide:AddPackage('config'..num..'package', p)
        -- package can be included as "package 'file.lua'" or "package 'folder/'"
        elseif type(p) == 'string' then
          local config = ide.configqueue[#ide.configqueue]
          local pkg
          for _, packagepath in ipairs({
              '.', 'packages/', '../packages/',
              ide.oshome and MergeFullPath(ide.oshome, "."..ide.appname.."/packages")}) do
            local p = MergeFullPath(config and MergeFullPath(config, packagepath) or packagepath, p)
            pkg = wx.wxDirExists(p) and loadToTab(p, {}, false, ide.proto.Plugin)
              or wx.wxFileExists(p) and LoadLuaFileExt({}, p, ide.proto.Plugin)
              or wx.wxFileExists(p..".lua") and LoadLuaFileExt({}, p..".lua", ide.proto.Plugin)
            if pkg then
              processPackages(pkg)
              break
            end
          end
          if not pkg then ide:Print(("Can't find '%s' to load package from."):format(p)) end
        else
          ide:Print(("Can't load package based on parameter of type '%s'."):format(type(p)))
        end
      end,
    })

  local includes = {}
  local include = function(c)
    if c then
      for _, config in ipairs({ide.configqueue[#ide.configqueue], ide.configs.user, ide.configs.system}) do
        local p = config and MergeFullPath(config.."/../", c)
        includes[p] = (includes[p] or 0) + 1
        if includes[p] > 1 or LoadLuaConfig(p) or LoadLuaConfig(p..".lua") then return end
        includes[p] = includes[p] - 1
      end
      ide:Print(("Can't find configuration file '%s' to process."):format(c))
    end
  end

  setmetatable(ide.config, {
    __index = setmetatable({
        -- these are provided for compatibility only to avoid breaking configs using `load.*`
        load = {
          interpreters = function() ide:Print("Warning: using `load.interpreters()` in config is deprecated.") end,
          specs = function() ide:Print("Warning: using `load.specs()` in config is deprecated.") end,
          tools = function() ide:Print("Warning: using `load.tools()` in config is deprecated.") end,
        },
        package = package,
        include = include,
    }, {__index = _G or _ENV})
  })
end

LoadLuaConfig(ide.appname.."/config.lua")

ide.editorApp:SetAppName(ide:GetProperty("settingsapp"))

-- check if the .ini file needs to be migrated on Windows
if ide.osname == 'Windows' and ide.wxver >= "2.9.5" then
  -- Windows used to have local ini file kept in wx.wxGetHomeDir (before 2.9),
  -- but since 2.9 it's in GetUserConfigDir(), so migrate it.
  local ini = ide.editorApp:GetAppName() .. ".ini"
  local old = wx.wxFileName(wx.wxGetHomeDir(), ini)
  local new = wx.wxFileName(wx.wxStandardPaths.Get():GetUserConfigDir(), ini)
  if old:FileExists() and not new:FileExists() then
    FileCopy(old:GetFullPath(), new:GetFullPath())
    ide:Print(("Migrated configuration file from '%s' to '%s'.")
      :format(old:GetFullPath(), new:GetFullPath()))
  end
end

----------------------
-- process plugins

if app.preinit then app.preinit() end

ide:LoadInterpreter()
ide:LoadSpec()

do
  -- process configs
  LoadLuaConfig(ide.configs.system)
  LoadLuaConfig(ide.configs.user)

  -- process all other configs (if any)
  for _, v in ipairs(configs) do LoadLuaConfig(v, true) end
  configs = nil

  -- check and apply default styles in case a user resets styles in the config
  for _, styles in ipairs({"styles", "stylesoutshell"}) do
    if not ide.config[styles] then
      ide:Print(("Ignored incorrect value of '%s' setting in the configuration file")
        :format(styles))
      ide.config[styles] = StylesGetDefault()
    end
  end

  local sep = GetPathSeparator()
  if ide.config.language then
    LoadLuaFileExt(ide.messages, "cfg"..sep.."i18n"..sep..ide.config.language..".lua")
  end
  -- always load 'en' as it's required as a fallback for pluralization
  if ide.config.language ~= 'en' then
    LoadLuaFileExt(ide.messages, "cfg"..sep.."i18n"..sep.."en.lua")
  end
end

processPackages(loadToTab("packages", {}, false, ide.proto.Plugin))
if ide.oshome then
  local userpackages = MergeFullPath(ide.oshome, "."..ide.appname.."/packages")
  if wx.wxDirExists(userpackages) then
    processPackages(loadToTab(userpackages, {}, false, ide.proto.Plugin))
  end
end

---------------
-- Load App

for _, file in ipairs({
    "settings", "singleinstance", "iofilters", "markup",
    "gui", "filetree", "output", "debugger", "outline", "commandbar",
    "editor", "findreplace", "commands", "autocomplete", "shellbox", "markers",
    "menu_file", "menu_edit", "menu_search", "menu_view", "menu_project", "menu_help",
    "print", "inspect" }) do
  dofile("src/editor/"..file..".lua")
end

-- delay loading tools until everything is loaded as it modifies the menus
ide:LoadTool()
-- delay loading APIs until auto-complete is loaded
ide:LoadAPI()

-- register all the plugins
PackageEventHandle("onRegister")

-- initialization that was delayed until configs processed and packages loaded
ProjectUpdateInterpreters()

-- load rest of settings
SettingsRestoreFramePosition(ide.frame, "MainFrame")
SettingsRestoreFileHistory(SetFileHistory)
SettingsRestoreEditorSettings()
SettingsRestoreProjectSession(FileTreeSetProjects)
SettingsRestoreFileSession(function(tabs, params)
  if params and params.recovery
  then return SetOpenTabs(params)
  else return SetOpenFiles(tabs, params) end
end)
SettingsRestoreView()

-- ---------------------------------------------------------------------------
-- Load the filenames

do
  for _, filename in ipairs(filenames) do
    if filename ~= "--" then ide:ActivateFile(filename) end
  end
  if ide:GetEditorNotebook():GetPageCount() == 0 then NewFile() end
end

if app.postinit then app.postinit() end

-- this is a workaround for a conflict between global shortcuts and local
-- shortcuts (like F2) used in the file tree or a watch panel.
-- because of several issues on OSX (as described in details in this thread:
-- https://groups.google.com/d/msg/wx-dev/juJj_nxn-_Y/JErF1h24UFsJ),
-- the workaround installs a global event handler that manually re-routes
-- conflicting events when the current focus is on a proper object.
-- non-conflicting shortcuts are handled through key-down events.
local remap = {
  [ID_ADDWATCH]    = ide:GetWatch(),
  [ID_EDITWATCH]   = ide:GetWatch(),
  [ID_DELETEWATCH] = ide:GetWatch(),
  [ID_RENAMEFILE]  = ide:GetProjectTree(),
  [ID_DELETEFILE]  = ide:GetProjectTree(),
}

local function rerouteMenuCommand(obj, id)
  -- check if the conflicting shortcut is enabled:
  -- (1) SetEnabled wasn't called or (2) Enabled was set to `true`.
  local uievent = wx.wxUpdateUIEvent(id)
  obj:ProcessEvent(uievent)
  if not uievent:GetSetEnabled() or uievent:GetEnabled() then
    obj:AddPendingEvent(wx.wxCommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED, id))
  end
end

local function remapkey(event)
  local keycode = event:GetKeyCode()
  local mod = event:GetModifiers()
  for id, obj in pairs(remap) do
    local focus = obj:FindFocus()
    if focus and focus:GetId() == obj:GetId() then
      local ae = wx.wxAcceleratorEntry(); ae:FromString(KSC(id))
      if ae:GetFlags() == mod and ae:GetKeyCode() == keycode then
        rerouteMenuCommand(obj, id)
        return
      end
    end
  end
  event:Skip()
end
ide:GetWatch():Connect(wx.wxEVT_KEY_DOWN, remapkey)
ide:GetProjectTree():Connect(wx.wxEVT_KEY_DOWN, remapkey)

local function resolveConflict(localid, globalid)
  return function(event)
    local shortcut = ide.config.keymap[localid]
    for id, obj in pairs(remap) do
      if ide.config.keymap[id]:lower() == shortcut:lower() then
        local focus = obj:FindFocus()
        if focus and focus:GetId() == obj:GetId() then
          obj:AddPendingEvent(wx.wxCommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED, id))
          return
        -- also need to check for children of objects
        -- to avoid re-triggering events when labels are being edited
        elseif focus and focus:GetParent():GetId() == obj:GetId() then
          return
        end
      end
    end
    rerouteMenuCommand(ide.frame, globalid)
  end
end

for lid in pairs(remap) do
  local shortcut = ide.config.keymap[lid]
  -- find a (potential) conflict for this shortcut (if any)
  for gid, ksc in pairs(ide.config.keymap) do
    -- if the same shortcut is used elsewhere (not one of IDs being checked)
    if shortcut:lower() == ksc:lower() and not remap[gid] then
      local fakeid = NewID()
      ide.frame:Connect(fakeid, wx.wxEVT_COMMAND_MENU_SELECTED, resolveConflict(lid, gid))
      ide:SetAccelerator(fakeid, ksc)
    end
  end
end

if ide.osname == 'Macintosh' then ide:SetAccelerator(ID_VIEWMINIMIZE, "Ctrl-M") end

-- these shortcuts need accelerators handling as they are not present anywhere in the menu
for _, id in ipairs({ ID_GOTODEFINITION, ID_RENAMEALLINSTANCES,
    ID_REPLACEALLSELECTIONS, ID_QUICKADDWATCH, ID_QUICKEVAL, ID_ADDTOSCRATCHPAD}) do
  local ksc = ide.config.keymap[id]
  if ksc and ksc > "" then
    local fakeid = NewID()
    ide.frame:Connect(fakeid, wx.wxEVT_COMMAND_MENU_SELECTED, function()
        local editor = ide:GetEditorWithFocus(ide:GetEditor())
        if editor then rerouteMenuCommand(editor, id) end
      end)
    ide:SetAccelerator(fakeid, ksc)
  end
end

for _, id in ipairs({ ID_NOTEBOOKTABNEXT, ID_NOTEBOOKTABPREV }) do
  local ksc = ide.config.keymap[id]
  if ksc and ksc > "" then
    local nbc = "wxAuiNotebook"
    ide.frame:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, function(event)
        local win = ide.frame:FindFocus()
        if not win then return end

        local notebook = win:GetClassInfo():GetClassName() == nbc and win:DynamicCast(nbc)
        or win:GetParent():GetClassInfo():GetClassName() == nbc and win:GetParent():DynamicCast(nbc)
        or nil
        if not notebook then return end

        local first, last = 0, notebook:GetPageCount()-1
        local fwd = event:GetId() == ID_NOTEBOOKTABNEXT
        if fwd and notebook:GetSelection() == last then
          notebook:SetSelection(first)
        elseif not fwd and notebook:GetSelection() == first then
          notebook:SetSelection(last)
        else
          notebook:AdvanceSelection(fwd)
        end
      end)
    ide:SetAccelerator(id, ksc)
  end
end

-- only set menu bar *after* postinit handler as it may include adding
-- app-specific menus (Help/About), which are not recognized by MacOS
-- as special items unless SetMenuBar is done after menus are populated.
ide.frame:SetMenuBar(ide.frame.menuBar)

ide:Print() -- flush pending output (if any)

PackageEventHandle("onAppLoad")

-- this provides a workaround for Ctrl-(Shift-)Tab not navigating over tabs on OSX
-- http://trac.wxwidgets.org/ticket/17064
if ide.osname == 'Macintosh' then
  local frame = ide.frame
  local focus
  ide.timers.ctrltab = ide:AddTimer(frame, function(event)
      local mouse = wx.wxGetMouseState()
      -- if anything other that Ctrl (along with Shift) is pressed, then cancel the timer
      if not ide:IsValidCtrl(focus)
      or not wx.wxGetKeyState(wx.WXK_RAW_CONTROL)
      or wx.wxGetKeyState(wx.WXK_ALT) or wx.wxGetKeyState(wx.WXK_CONTROL)
      or mouse:LeftDown() or mouse:RightDown() or mouse:MiddleDown() then
        ide.timers.ctrltab:Stop()
        return
      end
      local ctrl = frame:FindFocus()
      if not ctrl then return end
      local nb = focus:GetParent():DynamicCast("wxAuiNotebook")
      -- when moving backward from the very first tab, the focus moves
      -- to wxAuiTabCtrl on OSX, so need to take that into account
      if nb:GetId() ~= ctrl:GetParent():GetId()
      or ctrl:GetClassInfo():GetClassName() == "wxAuiTabCtrl" then
        local frwd = not wx.wxGetKeyState(wx.WXK_SHIFT)
        if nb:GetId() ~= ctrl:GetParent():GetId()
        or not frwd and nb:GetSelection() == 0
        or frwd and nb:GetSelection() == nb:GetPageCount()-1 then
          nb:AdvanceSelection(frwd)
          focus = nb:GetPage(nb:GetSelection())
          focus:SetFocus()
        end
        -- don't cancel the timer as the user may be cycling through tabs
      end
    end)

  frame:Connect(wx.wxEVT_CHAR_HOOK, function(event)
      local key = event:GetKeyCode()
      if key == wx.WXK_RAW_CONTROL then
        local ctrl = frame:FindFocus()
        local parent = ctrl and ctrl:GetParent()
        if parent and parent:GetClassInfo():GetClassName() == "wxAuiNotebook" then
          local nb = parent:DynamicCast("wxAuiNotebook")
          focus = nb:GetPage(nb:GetSelection())
          focus:SetFocus()
          ide.timers.ctrltab:Start(20) -- check periodically
        end
      elseif key == wx.WXK_SHIFT then -- Shift
        -- timer is started when `Ctrl` is pressed; even when `Shift` is pressed first,
        -- the Ctrl will still be pressed eventually, which will start the timer
      else
        ide.timers.ctrltab:Stop()
      end
      event:Skip()
    end)
end

-- add Ctrl-Tab and Ctrl-Shift-Tab processing on Linux as there is a similar issue
-- to the one on OSX: http://trac.wxwidgets.org/ticket/17064,
-- but at least on Linux the handling of Tab from CHAR_HOOK works.
if ide.osname == 'Unix' then
  ide.frame:Connect(wx.wxEVT_CHAR_HOOK, function(event)
      local key = event:GetKeyCode()
      if key == wx.WXK_TAB and wx.wxGetKeyState(wx.WXK_CONTROL)
      and not wx.wxGetKeyState(wx.WXK_ALT) then
        ide.frame:AddPendingEvent(wx.wxCommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED,
            wx.wxGetKeyState(wx.WXK_SHIFT) and ID.NOTEBOOKTABPREV or ID.NOTEBOOKTABNEXT
        ))
      else
        event:Skip()
      end
    end)
end

-- The status bar content is drawn incorrectly if it is shown
-- after being initially hidden.
-- Show the statusbar and hide it after showing the frame, which fixes the issue.
local statusbarfix = ide.osname == 'Windows' and not ide.frame:GetStatusBar():IsShown()
if statusbarfix then ide.frame:GetStatusBar():Show(true) end

ide.frame:Show(true)

if statusbarfix then ide.frame:GetStatusBar():Show(false) end

-- somehow having wxAuiToolbar "steals" the focus from the editor on OSX;
-- have to set the focus implicitly on the current editor (if any)
if ide.osname == 'Macintosh' then
  local editor = GetEditor()
  if editor then editor:SetFocus() end
end

-- enable full screen view if supported (for example, on OSX)
if ide:IsValidProperty(ide:GetMainFrame(), "EnableFullScreenView") then
  ide:GetMainFrame():EnableFullScreenView()
end

do
  local args = {}
  for _, a in ipairs(arg or {}) do args[a] = true end

  wx.wxGetApp().MacOpenFiles = function(files)
    for _, filename in ipairs(files) do
      -- in some cases, OSX sends the last command line parameter that looks like a filename
      -- to OpenFile callback, which gets reported to MacOpenFiles.
      -- I've tried to trace why this happens, but the only reference I could find
      -- is this one: http://lists.apple.com/archives/cocoa-dev/2009/May/msg00480.html
      -- To avoid this issue, the filename is skipped if it's present in `arg`.
      -- Also see http://trac.wxwidgets.org/ticket/14558 for related discussion.
      if not args[filename] then ide:ActivateFile(filename) end
    end
    args = {} -- reset the argument cache as it only needs to be checked on the initial launch
  end
end

wx.wxGetApp():MainLoop()

-- There are several reasons for this call:
-- (1) to fix a crash on OSX when closing with debugging in progress.
-- (2) to fix a crash on Linux 32/64bit during GC cleanup in wxlua
-- after an external process has been started from the IDE.
-- (3) to fix exit on Windows when started as "bin\lua src\main.lua".
os.exit()
-- Copyright 2015-17 Paul Kulchenko, ZeroBrane LLC

local ide = ide
ide.markers = {
  markersCtrl = nil,
  imglist = ide:CreateImageList("MARKERS", "FILE-NORMAL", "DEBUG-BREAKPOINT-TOGGLE", "BOOKMARK-TOGGLE"),
  needrefresh = {},
  settings = {markers = {}},
}

local unpack = table.unpack or unpack
local markers = ide.markers
local caches = {}
local image = { FILE = 0, BREAKPOINT = 1, BOOKMARK = 2 }
local markertypes = {breakpoint = 0, bookmark = 0}
local maskall = 0
for markertype in pairs(markertypes) do
  markertypes[markertype] = 2^ide:GetMarker(markertype)
  maskall = maskall + markertypes[markertype]
end

local function resetMarkersTimer()
  if ide.config.markersinactivity then
    ide.timers.markers:Start(ide.config.markersinactivity*1000, wx.wxTIMER_ONE_SHOT)
  end
end

local function needRefresh(editor)
  ide.markers.needrefresh[editor] = true
  resetMarkersTimer()
end

local function getMarkers(editor, mtype)
  local edmarkers = {}
  local line = editor:MarkerNext(0, maskall)
  while line ~= wx.wxNOT_FOUND do
    local markerval = editor:MarkerGet(line)
    for markertype, val in pairs(markertypes) do
      if bit.band(markerval, val) > 0 and (not mtype or markertype == mtype) then
        table.insert(edmarkers, {line, markertype})
      end
    end
    line = editor:MarkerNext(line + 1, maskall)
  end
  return edmarkers
end

local function markersRefresh()
  local ctrl = ide.markers.markersCtrl
  local win = ide:GetMainFrame():FindFocus()
  ctrl:Freeze()

  for editor in pairs(ide.markers.needrefresh) do
    local cache = caches[editor]
    if cache then
      local fileitem = cache.fileitem
      if not fileitem then
        local filename = ide:GetDocument(editor):GetTabText()
        local root = ctrl:GetRootItem()
        if not root or not root:IsOk() then return end
        fileitem = ctrl:AppendItem(root, filename, image.FILE)
        ctrl:SortChildren(root)
        cache.fileitem = fileitem
      end

      -- disabling event handlers is not strictly necessary, but it's expected
      -- to fix a crash on Windows that had DeleteChildren in the trace (#442).
      ctrl:SetEvtHandlerEnabled(false)
      ctrl:DeleteChildren(fileitem)
      ctrl:SetEvtHandlerEnabled(true)

      for _, edmarker in ipairs(getMarkers(editor)) do
        local line, markertype = unpack(edmarker)
        local text = ("%d: %s"):format(line+1,
          FixUTF8(editor:GetLineDyn(line), function(s) return '\\'..string.byte(s) end))
        ctrl:AppendItem(fileitem, text:gsub("[\r\n]+$",""), image[markertype:upper()])
      end

      -- if no markers added, then remove the file from the markers list
      ctrl:Expand(fileitem)
      if not ctrl:ItemHasChildren(fileitem) then
        ctrl:Delete(fileitem)
        cache.fileitem = nil
      end
    end
  end

  ctrl:Thaw()
  if win and win ~= ide:GetMainFrame():FindFocus() then win:SetFocus() end
end

local function item2editor(item_id)
  for editor, cache in pairs(caches) do
    if cache.fileitem and cache.fileitem:GetValue() == item_id:GetValue() then return editor end
  end
end

local function clearAllEditorMarkers(mtype, editor)
  for _, edmarker in ipairs(getMarkers(editor, mtype)) do
    local line = unpack(edmarker)
    editor:MarkerToggle(mtype, line, false)
  end
end

local function clearAllProjectMarkers(mtype)
  for filepath, markers in pairs(markers.settings.markers) do
    if ide:IsProjectSubDirectory(filepath) then
      local doc = ide:FindDocument(filepath)
      local editor = doc and doc:GetEditor()
      for m = #markers, 1, -1 do
        local line, markertype = unpack(markers[m])
        if markertype == mtype then
          if editor then
            editor:MarkerToggle(markertype, line, false)
          else
            table.remove(markers, m)
          end
        end
      end
    end
  end
end

local function createMarkersWindow()
  local width, height = 360, 200
  local ctrl = ide:CreateTreeCtrl(ide.frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(width, height),
    wx.wxTR_LINES_AT_ROOT + wx.wxTR_HAS_BUTTONS + wx.wxTR_HIDE_ROOT + wx.wxNO_BORDER)

  markers.markersCtrl = ctrl
  ide.timers.markers = ide:AddTimer(ctrl, function() markersRefresh() end)

  ctrl:AddRoot("Markers")
  ctrl:SetImageList(markers.imglist)
  ctrl:SetFont(ide.font.fNormal)

  function ctrl:ActivateItem(item_id, marker)
    local itemimage = ctrl:GetItemImage(item_id)
    if itemimage == image.FILE then
      -- activate editor tab
      local editor = item2editor(item_id)
      if editor then ide:GetDocument(editor):SetActive() end
    else -- clicked on the marker item
      local parent = ctrl:GetItemParent(item_id)
      if parent:IsOk() and ctrl:GetItemImage(parent) == image.FILE then
        local editor = item2editor(parent)
        if editor then
          local line = tonumber(ctrl:GetItemText(item_id):match("^(%d+):"))
          if line then
            if marker then
              editor:MarkerToggle(marker, line-1, false)
              ctrl:Delete(item_id)
              return -- don't activate the editor when the breakpoint is toggled
            end
            editor:GotoLine(line-1)
            editor:EnsureVisibleEnforcePolicy(line-1)
          end
          ide:GetDocument(editor):SetActive()
        end
      end
    end
  end

  local function activateByPosition(event)
    local mask = (wx.wxTREE_HITTEST_ONITEMINDENT + wx.wxTREE_HITTEST_ONITEMLABEL
      + wx.wxTREE_HITTEST_ONITEMICON + wx.wxTREE_HITTEST_ONITEMRIGHT)
    local item_id, flags = ctrl:HitTest(event:GetPosition())

    if item_id and item_id:IsOk() and bit.band(flags, mask) > 0 then
      local marker
      local itemimage = ctrl:GetItemImage(item_id)
      if bit.band(flags, wx.wxTREE_HITTEST_ONITEMICON) > 0 then
        for iname, itype in pairs(image) do
          if itemimage == itype then marker = iname:lower() end
        end
      end
      ctrl:ActivateItem(item_id, marker)
    else
      event:Skip()
    end
    return true
  end

  local function clearMarkersInFile(item_id, marker)
    local editor = item2editor(item_id)
    local itemimage = ctrl:GetItemImage(item_id)
    if itemimage ~= image.FILE then
      local parent = ctrl:GetItemParent(item_id)
      if parent:IsOk() and ctrl:GetItemImage(parent) == image.FILE then
        editor = item2editor(parent)
      end
    end
    if editor then clearAllEditorMarkers(marker, editor) end
  end

  ctrl:Connect(wx.wxEVT_LEFT_DOWN, activateByPosition)
  ctrl:Connect(wx.wxEVT_LEFT_DCLICK, activateByPosition)
  ctrl:Connect(wx.wxEVT_COMMAND_TREE_ITEM_ACTIVATED, function(event)
      ctrl:ActivateItem(event:GetItem())
    end)

  ctrl:Connect(wx.wxEVT_COMMAND_TREE_ITEM_MENU,
    function (event)
      local item_id = event:GetItem()
      local ID_BOOKMARKTOGGLE = ID("markers.bookmarktoggle")
      local ID_BREAKPOINTTOGGLE = ID("markers.breakpointtoggle")
      local menu = ide:MakeMenu {
        { ID_BOOKMARKTOGGLE, TR("Toggle Bookmark"), TR("Toggle bookmark") },
        { ID_BREAKPOINTTOGGLE, TR("Toggle Breakpoint"), TR("Toggle breakpoint") },
        { },
        { ID_BOOKMARKFILECLEAR, TR("Clear Bookmarks In File")..KSC(ID_BOOKMARKFILECLEAR) },
        { ID_BREAKPOINTFILECLEAR, TR("Clear Breakpoints In File")..KSC(ID_BREAKPOINTFILECLEAR) },
        { },
        { ID_BOOKMARKPROJECTCLEAR, TR("Clear Bookmarks In Project")..KSC(ID_BOOKMARKPROJECTCLEAR) },
        { ID_BREAKPOINTPROJECTCLEAR, TR("Clear Breakpoints In Project")..KSC(ID_BREAKPOINTPROJECTCLEAR) },
      }
      local itemimage = ctrl:GetItemImage(item_id)

      menu:Enable(ID_BOOKMARKTOGGLE, itemimage == image.BOOKMARK)
      menu:Connect(ID_BOOKMARKTOGGLE, wx.wxEVT_COMMAND_MENU_SELECTED,
        function() ctrl:ActivateItem(item_id, "bookmark") end)

      menu:Enable(ID_BREAKPOINTTOGGLE, itemimage == image.BREAKPOINT)
      menu:Connect(ID_BREAKPOINTTOGGLE, wx.wxEVT_COMMAND_MENU_SELECTED,
        function() ctrl:ActivateItem(item_id, "breakpoint") end)

      menu:Enable(ID_BOOKMARKFILECLEAR, itemimage == image.BOOKMARK or itemimage == image.FILE)
      menu:Connect(ID_BOOKMARKFILECLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
        function() clearMarkersInFile(item_id, "bookmark") end)

      menu:Enable(ID_BREAKPOINTFILECLEAR, itemimage == image.BREAKPOINT or itemimage == image.FILE)
      menu:Connect(ID_BREAKPOINTFILECLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
        function() clearMarkersInFile(item_id, "breakpoint") end)

      PackageEventHandle("onMenuMarkers", menu, ctrl, event)

      ctrl:PopupMenu(menu)
    end)

  local function reconfigure(pane)
    pane:TopDockable(false):BottomDockable(false)
        :MinSize(150,-1):BestSize(300,-1):FloatingSize(200,300)
  end

  local layout = ide:GetSetting("/view", "uimgrlayout")
  if not layout or not layout:find("markerspanel") then
    ide:AddPanelDocked(ide:GetOutputNotebook(), ctrl, "markerspanel", TR("Markers"), reconfigure, false)
  else
    ide:AddPanel(ctrl, "markerspanel", TR("Markers"), reconfigure)
  end
end

local package = ide:AddPackage('core.markers', {
    onRegister = function(self)
      if not ide.config.markersinactivity then return end

      createMarkersWindow()

      local bmmenu = ide:FindMenuItem(ID_BOOKMARK):GetSubMenu()
      bmmenu:AppendSeparator()
      bmmenu:Append(ID_BOOKMARKFILECLEAR, TR("Clear Bookmarks In File")..KSC(ID_BOOKMARKFILECLEAR))
      bmmenu:Append(ID_BOOKMARKPROJECTCLEAR, TR("Clear Bookmarks In Project")..KSC(ID_BOOKMARKPROJECTCLEAR))

      local bpmenu = ide:FindMenuItem(ID_BREAKPOINT):GetSubMenu()
      bpmenu:AppendSeparator()
      bpmenu:Append(ID_BREAKPOINTFILECLEAR, TR("Clear Breakpoints In File")..KSC(ID_BREAKPOINTFILECLEAR))
      bpmenu:Append(ID_BREAKPOINTPROJECTCLEAR, TR("Clear Breakpoints In Project")..KSC(ID_BREAKPOINTPROJECTCLEAR))

      ide:GetMainFrame():Connect(ID_BOOKMARKFILECLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
        function()
          local editor = ide:GetEditor()
          if editor then clearAllEditorMarkers("bookmark", editor) end
        end)
      ide:GetMainFrame():Connect(ID_BOOKMARKPROJECTCLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
        function() clearAllProjectMarkers("bookmark") end)

      ide:GetMainFrame():Connect(ID_BREAKPOINTFILECLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
        function()
          local editor = ide:GetEditor()
          if editor then clearAllEditorMarkers("breakpoint", editor) end
        end)
      ide:GetMainFrame():Connect(ID_BREAKPOINTPROJECTCLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
        function() clearAllProjectMarkers("breakpoint") end)
    end,

    -- save markers; remove tab from the list
    onEditorClose = function(self, editor)
      local cache = caches[editor]
      if not cache then return end
      if cache.fileitem then markers.markersCtrl:Delete(cache.fileitem) end
      caches[editor] = nil
    end,

    -- schedule marker update if the change is for one of the editors with markers
    onEditorUpdateUI = function(self, editor, event)
      if not caches[editor] then return end
      if bit.band(event:GetUpdated(), wxstc.wxSTC_UPDATE_CONTENT) == 0 then return end
      needRefresh(editor)
    end,

    onEditorMarkerUpdate = function(self, editor)
      -- if no marker, then all markers in a file need to be refreshed
      if not caches[editor] then caches[editor] = {} end
      needRefresh(editor)
      -- delay saving markers as other EditorMarkerUpdate handlers may still modify them,
      -- but check to make sure that the editor is still valid
      ide:DoWhenIdle(function()
          if ide:IsValidCtrl(editor) then markers:SaveMarkers(editor) end
        end)
    end,

    onEditorSave = function(self, editor) markers:SaveMarkers(editor) end,
    onEditorLoad = function(self, editor) markers:LoadMarkers(editor) end,
  })

function markers:SaveSettings() package:SetSettings(self.settings) end

function markers:SaveMarkers(editor, force)
  -- if the file has the name and has not been modified, save the breakpoints
  -- this also works when the file is saved as the modified flag is already set to `false`
  local doc = ide:GetDocument(editor)
  local filepath = doc:GetFilePath()
  if filepath and (force or not doc:IsModified()) then
    -- remove it from the list if it has no breakpoints
    local edmarkers = getMarkers(editor)
    self.settings.markers[filepath] = #edmarkers > 0 and edmarkers or nil
    self:SaveSettings()
  end
end

function markers:LoadMarkers(editor)
  local doc = ide:GetDocument(editor)
  local filepath = doc:GetFilePath()
  if filepath then
    for _, edmarker in ipairs(self.settings.markers[filepath] or {}) do
      local line, markertype = unpack(edmarker)
      editor:MarkerToggle(markertype, line, true)
    end
  end
end

MergeSettings(markers.settings, package:GetSettings())
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- styles for comment markup
---------------------------------------------------------

local MD_MARK_ITAL = '_' -- italic
local MD_MARK_BOLD = '**' -- bold
local MD_MARK_LINK = '[' -- link description start
local MD_MARK_LINZ = ']' -- link description end
local MD_MARK_LINA = '(' -- link URL start
local MD_MARK_LINT = ')' -- link URL end
local MD_MARK_HEAD = '#' -- header
local MD_MARK_CODE = '`' -- code
local MD_MARK_BOXD = '|' -- highlight
local MD_MARK_MARK = ' ' -- separator
local MD_LINK_NEWWINDOW = '+' -- indicator to open a new window for links
-- old versions of Scintilla had only 5-bit styles, so assign styles manually in those cases
local markup = {
  [MD_MARK_BOXD] = {st=ide:AddStyle("markup.boxd", ide.STYLEMASK == 31 and 25 or nil), fg={127,0,127}, b=true},
  [MD_MARK_CODE] = {st=ide:AddStyle("markup.code", ide.STYLEMASK == 31 and 26 or nil), fg={127,127,127}, fs=10},
  [MD_MARK_HEAD] = {st=ide:AddStyle("markup.head", ide.STYLEMASK == 31 and 27 or nil), fn="Lucida Console", b=true},
  [MD_MARK_LINK] = {st=ide:AddStyle("markup.link", ide.STYLEMASK == 31 and 28 or nil), u=true, hs={32,32,127}},
  [MD_MARK_BOLD] = {st=ide:AddStyle("markup.bold", ide.STYLEMASK == 31 and 29 or nil), b=true},
  [MD_MARK_ITAL] = {st=ide:AddStyle("markup.ital", ide.STYLEMASK == 31 and 30 or nil), i=true},
  [MD_MARK_MARK] = {st=ide:AddStyle("markup.mark", ide.STYLEMASK == 31 and 31 or nil), v=false},
}

-- allow other editor features to recognize this special markup
function MarkupIsSpecial(style) return style == markup[MD_MARK_MARK].st end
function MarkupIsAny(style)
  for _, mark in pairs(markup) do
    if style == mark.st then return true end
  end
  return false
end
function MarkupAddStyles(styles)
  local comment = styles.comment or {}
  for key,value in pairs(markup) do
    local style = styles[key] or {}
    -- copy all style features by value
    for feature in pairs(value) do
      style[feature] = style[feature] or value[feature]
    end
    style.fg = style.fg or comment.fg
    style.bg = style.bg or comment.bg
    styles[key] = style
  end
end

local q = EscapeMagic

local MD_MARK_PTRN = ''  -- combination of all markup marks that can start styling
for key in pairs(markup) do
  if key ~= MD_MARK_MARK then MD_MARK_PTRN = MD_MARK_PTRN .. q(key) end
end
MarkupAddStyles(ide.config.styles)

function MarkupHotspotClick(pos, editor)
  -- check if this is "our" hotspot event
  if bit.band(editor:GetStyleAt(pos),ide.STYLEMASK) ~= markup[MD_MARK_LINK].st then
    -- not "our" style, so nothing to do for us here
    return
  end
  local line = editor:LineFromPosition(pos)
  local tx = editor:GetLineDyn(line)
  pos = pos + #MD_MARK_LINK - editor:PositionFromLine(line) -- turn into relative position

  -- extract the URL/command on the right side of the separator
  local _,_,text = string.find(tx, q(MD_MARK_LINZ).."(%b"..MD_MARK_LINA..MD_MARK_LINT..")", pos)
  if text then
    text = text:gsub("^"..q(MD_MARK_LINA), ""):gsub(q(MD_MARK_LINT).."$", "")
    local doc = ide:GetDocument(editor)
    local filepath = doc and doc.filePath or FileTreeGetDir()
    local _,_,http = string.find(text, [[^(https?:%S+)$]])
    local _,_,command,code = string.find(text, [[^macro:(%w+)%((.*%S)%)$]])
    if not command then _,_,command = string.find(text, [[^macro:(%w+)$]]) end

    if command == 'shell' then
      ShellExecuteCode(code)
    elseif command == 'inline' then
      ShellExecuteInline(code)
    elseif command == 'run' then -- run the current file
      ProjectRun()
    elseif command == 'debug' then -- debug the current file
      ProjectDebug()
    elseif http then -- open the URL in a new browser window
      wx.wxLaunchDefaultBrowser(http, 0)
    elseif filepath then -- only check for saved files
      -- check if requested to open in a new window
      local newwindow = not doc or string.find(text, MD_LINK_NEWWINDOW, 1, true)
      if newwindow then text = string.gsub(text, "^%" .. MD_LINK_NEWWINDOW, "") end
      local filename = GetFullPathIfExists(
        wx.wxFileName(filepath):GetPath(wx.wxPATH_GET_VOLUME), text)
      if filename and
        (newwindow or SaveModifiedDialog(editor, true) ~= wx.wxID_CANCEL) then
        if not newwindow and ide.osname == 'Macintosh' then editor:GotoPos(0) end
        LoadFile(filename,not newwindow and editor or nil,true)
      end
    end
  end
  return true
end

local function ismarkup (tx)
  local start = 1
  local marksep = "[%s!%?%.,;:%(%)]"
  while true do
    -- find a separator first
    local st,_,sep,more = string.find(tx, "(["..MD_MARK_PTRN.."])(.)", start)
    if not st then return end

    -- check if this is a first character of a multi-character separator
    if not markup[sep] then sep = sep .. (more or '') end

    local s,e,cap
    local qsep = q(sep)
    local nonspace = "[^%s]"
    if sep == MD_MARK_HEAD then
      -- always search from the start of the line
      -- [%w%p] set is needed to avoid continuing this markup to the next line
      s,e,cap = string.find(tx,"^("..q(MD_MARK_HEAD)..".+[%w%p])")
    elseif sep == MD_MARK_LINK then
      -- allow everything based on balanced link separators
      s,e,cap = string.find(tx,
        "^(%b"..MD_MARK_LINK..MD_MARK_LINZ
        .."%b"..MD_MARK_LINA..MD_MARK_LINT..")", st)
    elseif markup[sep] then
      -- try a single character first, then 2+ characters between separators;
      -- this is to handle "`5` `6`" as two sequences, not one.
      s,e,cap = string.find(tx,"^("..qsep..nonspace..qsep..")".."%f"..marksep, st)
      if not s then s,e,cap = string.find(tx,"^("..qsep..nonspace..".-"..nonspace..qsep..")".."%f"..marksep, st) end
    end
    if s and -- selected markup is surrounded by spaces or punctuation marks
      (s == 1   or tx:sub(s-1, s-1):match(marksep)) and
      (e == #tx or tx:sub(e+1, e+1):match(marksep))
      then return s,e,cap,sep end
    start = st+1
  end
end

function MarkupStyle(editor, lines, linee)
  local lines = lines or 0
  if (lines < 0) then return end

  -- if the current spec doesn't have any comments, nothing to style
  if not next(editor.spec.iscomment) then return end

  -- always style to the end as there may be comments that need re-styling
  -- technically, this should be GetLineCount()-1, but we want to style
  -- beyond the last line to make sure it is styled correctly
  local linec = editor:GetLineCount()
  local linee = linee or linec

  local linecomment = editor.spec.linecomment
  local iscomment = {}
  for i,v in pairs(editor.spec.iscomment) do
    iscomment[i] = v
  end

  local es = editor:GetEndStyled()
  local needfix = false

  for line=lines,linee do
    local tx = editor:GetLineDyn(line)
    local ls = editor:PositionFromLine(line)

    local from = 1
    local off = -1

    -- doing WrapCount(line) when line == linec (which may be beyond
    -- the last line) occasionally crashes the application on OSX.
    local wrapped = line < linec and editor:WrapCount(line) or 0

    while from do
      tx = string.sub(tx,from)
      local f,t,w,mark = ismarkup(tx)

      if (f) then
        local p = ls+f+off
        local s = bit.band(editor:GetStyleAt(p), ide.STYLEMASK)
        -- only style comments and only those that are not at the beginning
        -- of the file to avoid styling shebang (#!) lines
        -- also ignore matches for line comments (as defined in the spec)
        if iscomment[s] and p > 0 and mark ~= linecomment then
          local smark = #mark
          local emark = #mark -- assumes end mark is the same length as start mark
          if mark == MD_MARK_HEAD then
            -- grab multiple MD_MARK_HEAD if present
            local _,_,full = string.find(w,"^("..q(MD_MARK_HEAD).."+)")
            smark,emark = #full,0
          elseif mark == MD_MARK_LINK then
            local lsep = w:find(q(MD_MARK_LINZ)..q(MD_MARK_LINA))
            if lsep then emark = #w-lsep+#MD_MARK_LINT end
          end
          editor:StartStyling(p, ide.STYLEMASK)
          editor:SetStyling(smark, markup[MD_MARK_MARK].st)
          editor:SetStyling(t-f+1-smark-emark, markup[mark].st or markup[MD_MARK_MARK].st)
          editor:SetStyling(emark, markup[MD_MARK_MARK].st)
        end

        off = off + t
      end
      from = t and (t+1)
    end

    -- has this line changed its wrapping because of invisible styling?
    if wrapped > 1 and editor:WrapCount(line) < wrapped then needfix = true end
  end
  editor:StartStyling(es, ide.STYLEMASK)

  -- if any wrapped lines have changed, then reset WrapMode to fix the drawing
  if needfix then
    -- this fixes an issue with duplicate lines in Scintilla when
    -- invisible styles hide some of the content that would be wrapped.
    local wrapmode = editor:GetWrapMode()
    if wrapmode ~= wxstc.wxSTC_WRAP_NONE then
      -- change the wrap mode to force recalculation
      editor:SetWrapMode(wxstc.wxSTC_WRAP_NONE)
      editor:SetWrapMode(wrapmode)
    end
    -- if some of the lines have folded, this can make not styled lines visible
    MarkupStyle(editor, linee+1) -- style to the end in this case
  end
end
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide

-- ---------------------------------------------------------------------------
-- Create the Edit menu and attach the callback functions

local frame = ide.frame
local menuBar = frame.menuBar

local editMenu = ide:MakeMenu {
  { ID_CUT, TR("Cu&t")..KSC(ID_CUT), TR("Cut selected text to clipboard") },
  { ID_COPY, TR("&Copy")..KSC(ID_COPY), TR("Copy selected text to clipboard") },
  { ID_PASTE, TR("&Paste")..KSC(ID_PASTE), TR("Paste text from the clipboard") },
  { ID_SELECTALL, TR("Select &All")..KSC(ID_SELECTALL), TR("Select all text in the editor") },
  { },
  { ID_UNDO, TR("&Undo")..KSC(ID_UNDO), TR("Undo last edit") },
  { ID_REDO, TR("&Redo")..KSC(ID_REDO), TR("Redo last edit undone") },
  { },
  { ID_SHOWTOOLTIP, TR("Show &Tooltip")..KSC(ID_SHOWTOOLTIP), TR("Show tooltip for current position; place cursor after opening bracket of function") },
  { ID_AUTOCOMPLETE, TR("Complete &Identifier")..KSC(ID_AUTOCOMPLETE), TR("Complete the current identifier") },
  { ID_AUTOCOMPLETEENABLE, TR("Auto Complete Identifiers")..KSC(ID_AUTOCOMPLETEENABLE), TR("Auto complete while typing"), wx.wxITEM_CHECK },
  { },
  { ID_SOURCE, TR("Source"), "", {
    { ID_COMMENT, TR("C&omment/Uncomment")..KSC(ID_COMMENT), TR("Comment or uncomment current or selected lines") },
    { ID_REINDENT, TR("Correct &Indentation")..KSC(ID_REINDENT), TR("Re-indent selected lines") },
    { ID_FOLD, TR("&Fold/Unfold All")..KSC(ID_FOLD), TR("Fold or unfold all code folds") },
    { ID_FOLDLINE, TR("Fold/Unfold Current &Line")..KSC(ID_FOLDLINE), TR("Fold or unfold current line") },
    { ID_SORT, TR("&Sort")..KSC(ID_SORT), TR("Sort selected lines") },
  } },
  { ID_BOOKMARK, TR("Bookmark"), "", {
    { ID_BOOKMARKTOGGLE, TR("Toggle Bookmark")..KSC(ID_BOOKMARKTOGGLE), TR("Toggle bookmark") },
    { ID_BOOKMARKNEXT, TR("Go To Next Bookmark")..KSC(ID_BOOKMARKNEXT) },
    { ID_BOOKMARKPREV, TR("Go To Previous Bookmark")..KSC(ID_BOOKMARKPREV) },
  } },
  { },
  { ID_PREFERENCES, TR("Preferences"), "", {
    { ID_PREFERENCESSYSTEM, TR("Settings: System")..KSC(ID_PREFERENCESSYSTEM) },
    { ID_PREFERENCESUSER, TR("Settings: User")..KSC(ID_PREFERENCESUSER) },
  } },
}
menuBar:Append(editMenu, TR("&Edit"))

editMenu:Check(ID_AUTOCOMPLETEENABLE, ide.config.autocomplete)

local function getCtrlWithFocus(edType)
  local ctrl = ide:GetMainFrame():FindFocus()
  return ctrl and ctrl:GetClassInfo():GetClassName() == edType and ctrl:DynamicCast(edType) or nil
end

local function onUpdateUIEditorInFocus(event)
  event:Enable(GetEditorWithFocus(GetEditor()) ~= nil)
end

local function onUpdateUIEditMenu(event)
  local menu_id = event:GetId()
  local editor = GetEditorWithFocus()
  if editor == nil then
    local editor = getCtrlWithFocus("wxTextCtrl")
    event:Enable(editor and (
        menu_id == ID_PASTE and editor:CanPaste() or
        menu_id == ID_UNDO and editor:CanUndo() or
        menu_id == ID_REDO and editor:CanRedo() or
        menu_id == ID_CUT and editor:CanCut() or
        menu_id == ID_COPY and editor:CanCopy() or
        menu_id == ID_SELECTALL and true
      ) or false)
    return
  end

  local alwaysOn = {
    [ID_SELECTALL] = true,
    -- allow Cut and Copy commands as these work on a line if no selection
    [ID_COPY] = true, [ID_CUT] = true,
  }
  local enable =
    -- pasting is allowed when the document is not read-only and the selection
    -- (if any) has no protected text; since pasting handles protected text,
    -- use GetReadOnly() instead of CanPaste()
    menu_id == ID_PASTE and (not editor:GetReadOnly()) or
    menu_id == ID_UNDO and editor:CanUndo() or
    menu_id == ID_REDO and editor:CanRedo() or
    alwaysOn[menu_id]
  event:Enable(enable)
end

local function onEditMenu(event)
  local menu_id = event:GetId()
  local editor = GetEditorWithFocus()
  if editor == nil then
    local editor = getCtrlWithFocus("wxTextCtrl")
    if not editor or not (
      menu_id == ID_PASTE and editor:Paste() or
      menu_id == ID_UNDO and editor:Undo() or
      menu_id == ID_REDO and editor:Redo() or
      menu_id == ID_CUT and editor:Cut() or
      menu_id == ID_COPY and editor:Copy() or
      menu_id == ID_SELECTALL and editor:SetSelection(-1, -1) or
      true
    ) then event:Skip() end
    return
  end

  if PackageEventHandle("onEditorAction", editor, event) == false then
    return
  end

  local copytext
  if (menu_id == ID_CUT or menu_id == ID_COPY)
  and ide.wxver >= "2.9.5" and editor:GetSelections() > 1 then
    local main = editor:GetMainSelection()
    copytext = editor:GetTextRangeDyn(editor:GetSelectionNStart(main), editor:GetSelectionNEnd(main))
    for s = 0, editor:GetSelections()-1 do
      if copytext ~= editor:GetTextRangeDyn(editor:GetSelectionNStart(s), editor:GetSelectionNEnd(s)) then
        copytext = nil
        break
      end
    end
  end

  local spos, epos = editor:GetSelectionStart(), editor:GetSelectionEnd()
  if menu_id == ID_CUT then
    if spos == epos then editor:LineCopy() else editor:CopyDyn() end
    if spos == epos then
      local line = editor:LineFromPosition(spos)
      spos, epos = editor:PositionFromLine(line), editor:PositionFromLine(line+1)
      editor:SetSelectionStart(spos)
      editor:SetSelectionEnd(epos)
    end
    if spos ~= epos then editor:ClearAny() end
  elseif menu_id == ID_COPY then
    if spos == epos then editor:LineCopy() else editor:CopyDyn() end
  elseif menu_id == ID_PASTE then
    -- first clear the text in case there is any hidden markup
    if spos ~= epos then editor:ClearAny() end
    editor:PasteDyn()
  elseif menu_id == ID_SELECTALL then editor:SelectAll()
  elseif menu_id == ID_UNDO then editor:Undo()
  elseif menu_id == ID_REDO then editor:Redo()
  end

  if copytext then editor:CopyText(#copytext, copytext) end
end

for _, event in pairs({ID_CUT, ID_COPY, ID_PASTE, ID_SELECTALL, ID_UNDO, ID_REDO}) do
  frame:Connect(event, wx.wxEVT_COMMAND_MENU_SELECTED, onEditMenu)
  frame:Connect(event, wx.wxEVT_UPDATE_UI, onUpdateUIEditMenu)
end

for _, event in pairs({
    ID_BOOKMARKTOGGLE, ID_BOOKMARKNEXT, ID_BOOKMARKPREV,
    ID_AUTOCOMPLETE, ID_SORT, ID_REINDENT, ID_SHOWTOOLTIP,
}) do
  frame:Connect(event, wx.wxEVT_UPDATE_UI, onUpdateUIEditorInFocus)
end

frame:Connect(ID_COMMENT, wx.wxEVT_UPDATE_UI,
  function(event)
    local editor = GetEditorWithFocus(GetEditor())
    event:Enable(editor ~= nil
      and ide:IsValidProperty(editor, "spec") and editor.spec
      and editor.spec.linecomment and true or false)
  end)

local function generateConfigMessage(type)
  return ([==[--[[--
  Use this file to specify **%s** preferences.
  Review [examples](+%s) or check [online documentation](%s) for details.
--]]--
]==])
    :format(type, MergeFullPath(ide.editorFilename, "../cfg/user-sample.lua"),
      "http://studio.zerobrane.com/documentation.html")
end

frame:Connect(ID_PREFERENCESSYSTEM, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    local editor = LoadFile(ide.configs.system)
    if editor and editor:GetLength() == 0 then
      editor:AddTextDyn(generateConfigMessage("System")) end
  end)

frame:Connect(ID_PREFERENCESUSER, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    local editor = LoadFile(ide.configs.user)
    if editor and editor:GetLength() == 0 then
      editor:AddTextDyn(generateConfigMessage("User")) end
  end)
frame:Connect(ID_PREFERENCESUSER, wx.wxEVT_UPDATE_UI,
  function (event) event:Enable(ide.configs.user ~= nil) end)

frame:Connect(ID_CLEARDYNAMICWORDS, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () DynamicWordsReset() end)

frame:Connect(ID_SHOWTOOLTIP, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditor()

    if (editor:CallTipActive()) then
      editor:CallTipCancel()
      return
    end

    EditorCallTip(editor, editor:GetCurrentPos())
  end)

frame:Connect(ID_AUTOCOMPLETE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) EditorAutoComplete(GetEditor()) end)

frame:Connect(ID_AUTOCOMPLETEENABLE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) ide.config.autocomplete = event:IsChecked() end)

frame:Connect(ID_COMMENT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditor()
    local lc = editor.spec.linecomment
    if not lc then return end

    -- for multi-line selection, always start the first line at the beginning
    local ssel, esel = editor:GetSelectionStart(), editor:GetSelectionEnd()
    local sline = editor:LineFromPosition(ssel)
    local eline = editor:LineFromPosition(esel)
    local sel = ssel ~= esel
    local rect = editor:SelectionIsRectangle()
    local qlc = lc:gsub(".", "%%%1")

    -- figure out how to toggle comments; if there is at least one non-empty
    -- line that doesn't start with a comment, need to comment
    local comment = false
    for line = sline, eline do
      local pos = sel and (sline == eline or rect)
        and ssel-editor:PositionFromLine(sline)+1 or 1
      local text = editor:GetLineDyn(line)
      local _, cpos = text:find("^%s*"..qlc, pos)
      if not cpos and text:find("%S")
      -- ignore last line when the end of selection is at the first position
      and (line == sline or line < eline or esel-editor:PositionFromLine(line) > 0) then
        comment = true
        break
      end
    end

    local linetoggle = ide.config.editor.commentlinetoggle
    editor:BeginUndoAction()
    -- go last to first as selection positions we captured may be affected
    -- by text changes
    for line = eline, sline, -1 do
      local pos = sel and (sline == eline or rect) and ssel-editor:PositionFromLine(sline)+1 or 1
      local text = editor:GetLineDyn(line)
      local validline = (line == sline or line < eline or esel-editor:PositionFromLine(line) > 0)
      local _, cpos = text:find("^%s*"..qlc, pos)
      if (linetoggle or not comment) and cpos and validline then
        editor:DeleteRange(cpos-#lc+editor:PositionFromLine(line), #lc)
      elseif (linetoggle or comment) and text:find("%S") and validline then
        editor:SetTargetStart(pos+editor:PositionFromLine(line)-1)
        editor:SetTargetEnd(editor:GetTargetStart())
        editor:ReplaceTarget(lc)
      end
    end
    editor:EndUndoAction()
  end)

local function processSelection(editor, func)
  local text = editor:GetSelectedText()
  local line = editor:GetCurrentLine()
  local posinline = editor:GetCurrentPos() - editor:PositionFromLine(line)
  if #text == 0 then
    editor:SelectAll()
    text = editor:GetSelectedText()
  end
  local wholeline = text:find("\n$")
  local buf = {}
  for ln in string.gmatch(text..(wholeline and "" or "\n"), "(.-\r?\n)") do
    table.insert(buf, ln)
  end
  if #buf > 0 then
    if func then func(buf) end
    -- add new line at the end if it was there
    local newtext = table.concat(buf, ""):gsub("(\r?\n)$", wholeline and "%1" or "")
    -- straightforward editor:ReplaceSelection() doesn't work reliably as
    -- it sometimes doubles the context when the entire file is selected.
    -- this seems like Scintilla issue, so use ReplaceTarget instead.
    -- Since this doesn't work with rectangular selection, which
    -- ReplaceSelection should handle (after wxwidgets 3.x upgrade), this
    -- will need to be revisited when ReplaceSelection is updated.
    if newtext ~= text then
      editor:BeginUndoAction()
      -- if there is at least one marker, then use a different mechanism to preserve them
      -- simply saving markers, replacing text, and reapplying markers doesn't work as
      -- they get reset after `undo/redo` operations.
      local ssel, esel = editor:GetSelectionStart(), editor:GetSelectionEnd()
      local sline = editor:LineFromPosition(ssel)
      local eline = editor:LineFromPosition(esel)
      if #editor:MarkerGetAll(nil, sline, eline) > 0 then
        for line = #buf, 1, -1 do
          editor:SetTargetStart(line == 1 and ssel or editor:PositionFromLine(sline+line-1))
          editor:SetTargetEnd(line == eline-sline+1 and esel or editor:GetLineEndPosition(sline+line-1))
          editor:ReplaceTarget((buf[line]:gsub("\r?\n$", "")))
        end
      else
        editor:TargetFromSelection()
        editor:ReplaceTarget(newtext)
      end
      editor:EndUndoAction()
    end
  end
  editor:GotoPosEnforcePolicy(math.min(
      editor:PositionFromLine(line)+posinline, editor:GetLineEndPosition(line)))
end

frame:Connect(ID_SORT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) processSelection(GetEditor(), table.sort) end)

local function reIndent(editor, buf)
  local decindent, incindent = editor.spec.isdecindent, editor.spec.isincindent
  if not (decindent and incindent) then return end

  local edline = editor:LineFromPosition(editor:GetSelectionStart())
  local indent = 0
  local text = ""
  -- find the last non-empty line in the previous block (if any)
  for n = edline-1, 1, -1 do
    indent = editor:GetLineIndentation(n)
    text = editor:GetLineDyn(n)
    if text:match("[^\r\n]") then break end
  end

  local ut = editor:GetUseTabs()
  local tw = ut and editor:GetTabWidth() or editor:GetIndent()

  local indents = {}
  local isstatic = {}
  for line = 1, #buf+1 do
    local ls = editor:PositionFromLine(edline+line-1)
    local style = bit.band(editor:GetStyleAt(ls), ide.STYLEMASK)
    -- don't reformat multi-line comments or strings
    isstatic[line] = (editor.spec.iscomment[style]
      or editor.spec.isstring[style]
      or (MarkupIsAny and MarkupIsAny(style)))
    if not isstatic[line] or line == 1 or not isstatic[line-1] then
      local closed, blockend = decindent(text)
      local opened = incindent(text)

      -- ignore impact from initial block endings as they are already indented
      if line == 1 then blockend = 0 end

      -- this only needs to be done for 2, #buf+1; do it and get out when done
      if line > 1 then indents[line-1] = indents[line-1] - tw * closed end
      if line > #buf then break end

      indent = indent + tw * (opened - blockend)
      if indent < 0 then indent = 0 end
    end

    indents[line] = indent
    text = buf[line]
  end

  for line = 1, #buf do
    if not isstatic[line] then
      buf[line] = buf[line]:gsub("^[ \t]*",
        not buf[line]:match("%S") and ""
        or ut and ("\t"):rep(indents[line] / tw) or (" "):rep(indents[line]))
    end
  end
end

frame:Connect(ID_REINDENT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditor()
    processSelection(editor, function(buf) reIndent(editor, buf) end)
  end)

local function canfold(event)
  local editor = GetEditorWithFocus()
  event:Enable(editor and editor:CanFold() or false)
end

frame:Connect(ID_FOLD, wx.wxEVT_UPDATE_UI, canfold)
frame:Connect(ID_FOLD, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) GetEditorWithFocus():FoldSome() end)

frame:Connect(ID_FOLDLINE, wx.wxEVT_UPDATE_UI, canfold)
frame:Connect(ID_FOLDLINE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditorWithFocus()
    local current = editor:GetCurrentLine()
    editor:ToggleFold(current)
    -- move up to the parent line if the current one is not visible
    local visible = editor:GetLineVisible(current)
    if not visible and editor:GetFoldParent(current) ~= wx.wxNOT_FOUND then editor:LineUp() end
  end)

local BOOKMARK_MARKER = StylesGetMarker("bookmark")

frame:Connect(ID_BOOKMARKTOGGLE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() GetEditor():BookmarkToggle() end)
frame:Connect(ID_BOOKMARKNEXT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() GetEditor():MarkerGotoNext(BOOKMARK_MARKER) end)
frame:Connect(ID_BOOKMARKPREV, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() GetEditor():MarkerGotoPrev(BOOKMARK_MARKER) end)
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local frame = ide.frame
local menuBar = frame.menuBar
local openDocuments = ide.openDocuments

local filehistorymenu = ide:MakeMenu {
  { },
  { ID_RECENTFILESCLEAR, TR("Clear Items")..KSC(ID_RECENTFILESCLEAR), TR("Clear items from this list") },
}
local projecthistorymenu = ide:MakeMenu {
  { },
  { ID_RECENTPROJECTSCLEAR, TR("Clear Items")..KSC(ID_RECENTPROJECTSCLEAR), TR("Clear items from this list") },
}
local fileMenu = ide:MakeMenu {
  { ID_NEW, TR("&New")..KSC(ID_NEW), TR("Create an empty document") },
  { ID_OPEN, TR("&Open...")..KSC(ID_OPEN), TR("Open an existing document") },
  { ID_CLOSE, TR("&Close Page")..KSC(ID_CLOSE), TR("Close the current editor window") },
  { },
  { ID_SAVE, TR("&Save")..KSC(ID_SAVE), TR("Save the current document") },
  { ID_SAVEAS, TR("Save &As...")..KSC(ID_SAVEAS), TR("Save the current document to a file with a new name") },
  { ID_SAVEALL, TR("Save A&ll")..KSC(ID_SAVEALL), TR("Save all open documents") },
  { },
  { ID_RECENTFILES, TR("Recent Files")..KSC(ID_RECENTFILES), TR("File history"), filehistorymenu },
  { ID_RECENTPROJECTS, TR("Recent Projects")..KSC(ID_RECENTPROJECTS), TR("Project history"), projecthistorymenu },
  { },
  { ID_EXIT, TR("E&xit")..KSC(ID_EXIT), TR("Exit program") },
}
menuBar:Append(fileMenu, TR("&File"))

do -- recent file history
  local iscaseinsensitive = wx.wxFileName("A"):SameAs(wx.wxFileName("a"))
  local function isSameAs(f1, f2)
    return f1 == f2 or iscaseinsensitive and f1:lower() == f2:lower()
  end

  local filehistory = {[0] = 1}

  -- add file to the file history removing duplicates
  local function addFileHistory(filename)
    -- a new (empty) tab is opened; don't change the history
    if not filename then return end

    local fn = wx.wxFileName(filename)
    if fn:Normalize() then filename = fn:GetFullPath() end

    local index = filehistory[0]

    -- special case: selecting the current file (or moving through the history)
    if filehistory[index] and isSameAs(filename, filehistory[index].filename) then return end

    -- something else is selected
    -- (1) flip the history from 1 to the current index
    for i = 1, math.floor(index/2) do
      filehistory[i], filehistory[index-i+1] = filehistory[index-i+1], filehistory[i]
    end

    -- (2) if the file is in the history, remove it
    for i = #filehistory, 1, -1 do
      if isSameAs(filename, filehistory[i].filename) then
        table.remove(filehistory, i)
      end
    end

    -- (3) add the file to the top and update the index
    table.insert(filehistory, 1, {filename=filename})
    filehistory[0] = 1

    -- (4) remove all entries that are no longer needed
    while #filehistory>ide.config.filehistorylength do table.remove(filehistory) end
  end

  local function remFileHistory(filename)
    if not filename then return end

    local fn = wx.wxFileName(filename)
    if fn:Normalize() then filename = fn:GetFullPath() end

    local index = filehistory[0]

    -- special case: removing the current file
    if filehistory[index] and isSameAs(filename, filehistory[index].filename) then
      -- (1) flip the history from 1 to the current index
      for i = 1, math.floor(index/2) do
        filehistory[i], filehistory[index-i+1] = filehistory[index-i+1], filehistory[i]
      end
    end

    -- (2) if the file is in the history, remove it
    for i = #filehistory, 1, -1 do
      if isSameAs(filename, filehistory[i].filename) then
        table.remove(filehistory, i)
      end
    end

    -- (3) update index
    filehistory[0] = 1
  end

  local updateRecentFiles -- need forward declaration because of recursive refs

  local function loadRecent(event)
    local id = event:GetId()
    local item = filehistorymenu:FindItem(id)
    local filename = item:GetLabel()
    local index = filehistory[0]
    filehistory[0] = (
      (index > 1 and id == ID("file.recentfiles."..(index-1)) and index-1) or
      (index < #filehistory) and id == ID("file.recentfiles."..(index+1)) and index+1 or
      1)
    if not LoadFile(filename, nil, true) then
      wx.wxMessageBox(
        TR("File '%s' no longer exists."):format(filename),
        ide:GetProperty("editormessage"),
        wx.wxOK + wx.wxCENTRE, ide.frame)
      remFileHistory(filename)
      updateRecentFiles(filehistory)
    end
  end

  local items = 0
  updateRecentFiles = function (list)
    -- protect against recent files menu not being present
    if not ide:FindMenuItem(ID_RECENTFILES) then return end

    for i=1, #list do
      local file = list[i].filename
      local id = ID("file.recentfiles."..i)
      local label = file..(
        i == list[0]-1 and KSC(ID_RECENTFILESNEXT) or
        i == list[0]+1 and KSC(ID_RECENTFILESPREV) or
        "")
      if i <= items then -- this is an existing item; update the label
        filehistorymenu:FindItem(id):SetItemLabel(label)
      else -- need to add an item
        local item = wx.wxMenuItem(filehistorymenu, id, label, "")
        filehistorymenu:Insert(i-1, item)
        frame:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, loadRecent)
      end
    end
    for i=items, #list+1, -1 do -- delete the rest if the list got shorter
      filehistorymenu:Delete(filehistorymenu:FindItemByPosition(i-1))
    end
    items = #list -- update the number of items for the next refresh

    -- enable if there are any recent files
    fileMenu:Enable(ID_RECENTFILES, #list > 0)
  end

  -- public methods
  function GetFileHistory() return filehistory end
  function SetFileHistory(fh)
    filehistory = fh
    filehistory[0] = 1
    updateRecentFiles(filehistory)
  end
  function AddToFileHistory(filename)
    addFileHistory(filename)
    updateRecentFiles(filehistory)
  end

  function FileRecentListUpdate(menu)
    local list = filehistory
    for i=#list, 1, -1 do
      local id = ID("file.recentfiles."..i)
      local label = list[i].filename
      local item = wx.wxMenuItem(menu, id, label, "")
      menu:Insert(0, item)
      ide.frame:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, loadRecent)
    end
  end
end

frame:Connect(ID_NEW, wx.wxEVT_COMMAND_MENU_SELECTED, function() return NewFile() end)
frame:Connect(ID_OPEN, wx.wxEVT_COMMAND_MENU_SELECTED, OpenFile)
frame:Connect(ID_SAVE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    local editor = ide.findReplace:CanSave(GetEditorWithFocus()) or GetEditor()
    local doc = ide:GetDocument(editor)
    SaveFile(editor, doc and doc:GetFilePath() or nil)
  end)
frame:Connect(ID_SAVE, wx.wxEVT_UPDATE_UI,
  function (event)
    local doc = ide:GetDocument(GetEditor())
    event:Enable(ide.findReplace:CanSave(GetEditorWithFocus()) and true
      or doc and (doc:IsModified() or doc:IsNew()) or false)
  end)

frame:Connect(ID_SAVEAS, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    SaveFileAs(GetEditor())
  end)
frame:Connect(ID_SAVEAS, wx.wxEVT_UPDATE_UI,
  function (event)
    event:Enable(GetEditor() ~= nil)
  end)

frame:Connect(ID_SAVEALL, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    SaveAll()
  end)
frame:Connect(ID_SAVEALL, wx.wxEVT_UPDATE_UI,
  function (event)
    local atLeastOneModifiedDocument = false
    for _, document in pairs(openDocuments) do
      if document.isModified or not document.filePath then
        atLeastOneModifiedDocument = true
        break
      end
    end
    event:Enable(atLeastOneModifiedDocument)
  end)

frame:Connect(ID_CLOSE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditorWithFocus()
    local nb = ide:GetOutputNotebook()
    local index = editor and nb:GetPageIndex(editor)
    if index and ide.findReplace:IsPreview(editor) and index >= 0 then
      nb:DeletePage(index) -- close preview tab
    else
      ClosePage() -- this will find the current editor tab
    end
  end)
frame:Connect(ID_CLOSE, wx.wxEVT_UPDATE_UI,
  function (event)
    event:Enable(ide.findReplace:IsPreview(GetEditorWithFocus()) or GetEditor() ~= nil)
  end)

frame:Connect(ID_EXIT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    frame:Close() -- this will trigger wxEVT_CLOSE_WINDOW
  end)

frame:Connect(ID_RECENTPROJECTSCLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) FileTreeProjectListClear() end)

frame:Connect(ID_RECENTFILESCLEAR, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    SetFileHistory({})
    local ed = ide:GetEditor()
    if ed then AddToFileHistory(ide:GetDocument(ed):GetFilePath()) end
  end)

local recentprojects = 0
frame:Connect(ID_RECENTPROJECTS, wx.wxEVT_UPDATE_UI,
  function (event)
    recentprojects = FileTreeProjectListUpdate(projecthistorymenu, recentprojects)
    if not recentprojects then return end
    local pos = 1 -- add shortcut for the previous project (if any)
    if recentprojects > pos then
      local item = projecthistorymenu:FindItemByPosition(pos)
      item:SetItemLabel(item:GetItemLabelText()..KSC(ID_RECENTPROJECTSPREV))
    end
    event:Enable(recentprojects > 0)
  end)
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
---------------------------------------------------------

local ide = ide
-- ---------------------------------------------------------------------------
-- Create the Help menu and attach the callback functions

local frame = ide.frame
local menuBar = frame.menuBar
local mobdebug = require "mobdebug"

local product = ide:GetProperty("help", "zerobranestudio")
local url = "http://zerobrane.com/r/"..product.."-"
local urls = {
  [ID_HELPPROJECT] = "main",
  [ID_HELPDOCUMENTATION] =  "documentation",
  [ID_HELPGETTINGSTARTED] = "gettingstarted",
  [ID_HELPTUTORIALS] = "tutorials",
  [ID_HELPFAQ] = "faq",
  [ID_HELPCOMMUNITY] = "community",
}

local helpMenu = ide:MakeMenu {
  { ID_ABOUT, TR("&About")..KSC(ID_ABOUT), TR("About %s"):format(ide:GetProperty("editor")) },
  { ID_HELPPROJECT, TR("&Project Page")..KSC(ID_HELPPROJECT) },
  { ID_HELPDOCUMENTATION, TR("&Documentation")..KSC(ID_HELPDOCUMENTATION) },
  { ID_HELPGETTINGSTARTED, TR("&Getting Started Guide")..KSC(ID_HELPGETTINGSTARTED) },
  { ID_HELPTUTORIALS, TR("&Tutorials")..KSC(ID_HELPTUTORIALS) },
  { ID_HELPFAQ, TR("&Frequently Asked Questions")..KSC(ID_HELPFAQ) },
  { ID_HELPCOMMUNITY, TR("&Community")..KSC(ID_HELPCOMMUNITY) },
}
-- do not translate Help menu on Mac as it won't merge with "standard" menus
menuBar:Append(helpMenu, ide.osname == 'Macintosh' and "&Help" or TR("&Help"))

local function displayAbout(event)
  local logo = ide:GetAppName().."/"..ide:GetProperty("logo")
  local logoimg = wx.wxFileName(logo):FileExists() and
    ([[<tr><td><img src="%s"></td></tr>]]):format(logo) or ""
  local page = ([[
    <html>
      <body text="#777777">
	<table border="0" width="100%%">
	  %s
	  <tr><td>
	<table cellspacing="3" cellpadding="3" width="100%%">
	  <tr>
		<td>
		<b>ZeroBrane Studio (%s; MobDebug %s)</b><br>
		<b>Copyright &copy; 2011-2017 ZeroBrane LLC</b><br>
		Paul Kulchenko<br>
		Licensed under the MIT License.
		</td>
	  </tr>
	  <tr>
		<td>
		<b>Based on Estrela Editor</b><br>
		<b>Copyright &copy; 2008-2011 Luxinia DevTeam</b><br>
		Christoph Kubisch, Eike Decker<br>
		Licensed under the MIT License.
		</td>
		<td><img align="right" src="%s/res/estrela.png"></td>
	  </tr>
	  <tr>
		<td>
		<b>Based on wxLua editor</b><br>
		<b>Copyright &copy; 2002-2005 Lomtick Software</b><br>
		J. Winwood, John Labenski<br>
		Licensed under wxWindows Library License, v3.
		</td>
	  </tr>
	  <tr>
		<td>
                <b>Built with %s, %s</b>
		</td>
	  </tr>
	</table>
	</td></tr></table>
      </body>
    </html>]])
  :format(logoimg, ide.VERSION, mobdebug._VERSION, ide:GetAppName(),
    wx.wxVERSION_STRING, wxlua.wxLUA_VERSION_STRING)

  local dlg = wx.wxDialog(frame, wx.wxID_ANY, TR("About %s"):format(ide:GetProperty("editor")))

  -- this is needed because wxLuaHtmlWindow only seems to take into account
  -- the initial size, but not the one set with SetSize using
  -- wxlua 2.8.12.2 and wxwidgets 2.9.5+.
  local tmp = wx.wxLuaHtmlWindow(dlg, wx.wxID_ANY, wx.wxDefaultPosition, wx.wxSize(450, 260))
  tmp:SetPage(page)
  local w = tmp:GetInternalRepresentation():GetWidth()
  local h = tmp:GetInternalRepresentation():GetHeight()
  tmp:Destroy()

  local html = wx.wxLuaHtmlWindow(dlg, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(w, h), wx.wxHW_SCROLLBAR_NEVER)

  html:SetBorders(0)
  html:SetPage(page)

  local line = wx.wxStaticLine(dlg, wx.wxID_ANY)
  local button = wx.wxButton(dlg, wx.wxID_OK, "OK")
  button:SetDefault()

  local topsizer = wx.wxBoxSizer(wx.wxVERTICAL)
  topsizer:Add(html, 1, wx.wxEXPAND + wx.wxALL, 10)
  topsizer:Add(line, 0, wx.wxEXPAND + wx.wxLEFT + wx.wxRIGHT, 10)
  topsizer:Add(button, 0, wx.wxALL + wx.wxALIGN_RIGHT, 10)

  dlg:SetSizerAndFit(topsizer)
  dlg:ShowModal()
  dlg:Destroy()
end

frame:Connect(ID_ABOUT, wx.wxEVT_COMMAND_MENU_SELECTED, displayAbout)
for item, page in pairs(urls) do
  frame:Connect(item, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() wx.wxLaunchDefaultBrowser(url..page, 0) end)
end
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local frame = ide.frame
local menuBar = frame.menuBar
local openDocuments = ide.openDocuments

------------------------
-- Interpreters and Menu

local targetDirMenu = ide:MakeMenu {
  {ID_PROJECTDIRCHOOSE, TR("Choose...")..KSC(ID_PROJECTDIRCHOOSE), TR("Choose a project directory")},
  {ID_PROJECTDIRFROMFILE, TR("Set From Current File")..KSC(ID_PROJECTDIRFROMFILE), TR("Set project directory from current file")},
}
local targetMenu = ide:MakeMenu {}
local debugMenu = ide:MakeMenu {
  { ID_RUN, TR("&Run")..KSC(ID_RUN), TR("Execute the current project/file") },
  { ID_RUNNOW, TR("Run As Scratchpad")..KSC(ID_RUNNOW), TR("Execute the current project/file and keep updating the code to see immediate results"), wx.wxITEM_CHECK },
  { ID_COMPILE, TR("&Compile")..KSC(ID_COMPILE), TR("Compile the current file") },
  { ID_STARTDEBUG, TR("Start &Debugging")..KSC(ID_STARTDEBUG), TR("Start or continue debugging") },
  { ID_ATTACHDEBUG, TR("&Start Debugger Server")..KSC(ID_ATTACHDEBUG), TR("Allow external process to start debugging"), wx.wxITEM_CHECK },
  { },
  { ID_STOPDEBUG, TR("S&top Process")..KSC(ID_STOPDEBUG), TR("Stop the currently running process") },
  { ID_DETACHDEBUG, TR("Detach &Process")..KSC(ID_DETACHDEBUG), TR("Stop debugging and continue running the process") },
  { ID_STEP, TR("Step &Into")..KSC(ID_STEP), TR("Step into") },
  { ID_STEPOVER, TR("Step &Over")..KSC(ID_STEPOVER), TR("Step over") },
  { ID_STEPOUT, TR("Step O&ut")..KSC(ID_STEPOUT), TR("Step out of the current function") },
  { ID_RUNTO, TR("Run To Cursor")..KSC(ID_RUNTO), TR("Run to cursor") },
  { ID_TRACE, TR("Tr&ace")..KSC(ID_TRACE), TR("Trace execution showing each executed line") },
  { ID_BREAK, TR("&Break")..KSC(ID_BREAK), TR("Break execution at the next executed line of code") },
  { },
  { ID_BREAKPOINT, TR("Breakpoint")..KSC(ID_BREAKPOINT), "", {
    { ID_BREAKPOINTTOGGLE, TR("Toggle Breakpoint")..KSC(ID_BREAKPOINTTOGGLE) },
    { ID_BREAKPOINTNEXT, TR("Go To Next Breakpoint")..KSC(ID_BREAKPOINTNEXT) },
    { ID_BREAKPOINTPREV, TR("Go To Previous Breakpoint")..KSC(ID_BREAKPOINTPREV) },
  } },
  { },
  { ID_CLEAROUTPUT, TR("C&lear Output Window")..KSC(ID_CLEAROUTPUT), TR("Clear the output window before compiling or debugging"), wx.wxITEM_CHECK },
  { ID_COMMANDLINEPARAMETERS, TR("Command Line Parameters...")..KSC(ID_COMMANDLINEPARAMETERS), TR("Provide command line parameters") },
  { ID_PROJECTDIR, TR("Project Directory"), TR("Set the project directory to be used"), targetDirMenu },
  { ID_INTERPRETER, TR("Lua &Interpreter"), TR("Set the interpreter to be used"), targetMenu },
}
menuBar:Append(debugMenu, TR("&Project"))

-- older (<3.x) versions of wxwidgets may not have `GetLabelText`, so provide alternative
if not pcall(function() return debugMenu.GetLabelText end) then
  debugMenu.GetLabelText = function(self, ...) return wx.wxMenuItem.GetLabelText(self.GetLabel(self, ...)) end
end
local debugMenuRunLabel = { [false]=debugMenu:GetLabelText(ID_STARTDEBUG), [true]=TR("Co&ntinue") }
local debugMenuStopLabel = { [false]=debugMenu:GetLabelText(ID_STOPDEBUG), [true]=TR("S&top Debugging") }

local interpreters
local function selectInterpreter(id)
  for id in pairs(interpreters) do
    menuBar:Check(id, false)
    menuBar:Enable(id, true)
  end
  menuBar:Check(id, true)
  menuBar:Enable(id, false)

  local changed = ide.interpreter ~= interpreters[id]
  if changed then
    if ide.interpreter then PackageEventHandle("onInterpreterClose", ide.interpreter) end
    if interpreters[id] then PackageEventHandle("onInterpreterLoad", interpreters[id]) end
  end

  ide.interpreter = interpreters[id]

  ide:GetDebugger():Shutdown()

  if ide.interpreter then
    ide.interpreter:UpdateStatus()
  else
    ide:SetStatus("", 4)
  end
  if changed then ReloadAPIs() end
end

function ProjectSetInterpreter(name)
  local id = IDget("debug.interpreter."..name)
  if id and interpreters[id] then
    selectInterpreter(id)
    return true
  else
    ide:Print(("Can't load interpreter '%s'; using the default interpreter instead."):format(name))
    local id = (
      -- interpreter is set and is (still) on the list of known interpreters
      IDget("debug.interpreter."..(ide.config.interpreter or ide.config.default.interpreter)) or
      -- otherwise use default interpreter
      ID("debug.interpreter."..ide.config.default.interpreter)
    )
    selectInterpreter(id)
  end
end

local function evSelectInterpreter(event)
  selectInterpreter(event:GetId())
end

function ProjectUpdateInterpreters()
  assert(ide.interpreters, "no interpreters defined")

  -- delete all existing items (if any)
  local items = targetMenu:GetMenuItemCount()
  for i = items, 1, -1 do
    targetMenu:Delete(targetMenu:FindItemByPosition(i-1))
  end

  local names = {}
  for file in pairs(ide.interpreters) do table.insert(names, file) end
  table.sort(names)

  interpreters = {}
  for _, file in ipairs(names) do
    local inter = ide.interpreters[file]
    local id = ID("debug.interpreter."..file)
    inter.fname = file
    interpreters[id] = inter
    targetMenu:Append(
      wx.wxMenuItem(targetMenu, id, inter.name, inter.description, wx.wxITEM_CHECK))
    frame:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, evSelectInterpreter)
  end

  local id = (
    -- interpreter is set and is (still) on the list of known interpreters
    IDget("debug.interpreter."
      ..(ide.interpreter and ide.interpreters[ide.interpreter.fname] and ide.interpreter.fname
         or ide.config.interpreter or ide.config.default.interpreter)) or
    -- otherwise use default interpreter
    ID("debug.interpreter."..ide.config.default.interpreter)
  )
  selectInterpreter(id)
end

-----------------------------
-- Project directory handling

local function projChoose(event)
  local editor = GetEditor()
  local fn = wx.wxFileName(
    editor and openDocuments[editor:GetId()].filePath or "")
  fn:Normalize() -- want absolute path for dialog

  local projectdir = ide:GetProject()
  local filePicker = wx.wxDirDialog(frame, TR("Choose a project directory"),
    projectdir ~= "" and projectdir or wx.wxGetCwd(), wx.wxDIRP_DIR_MUST_EXIST)
  if filePicker:ShowModal(true) == wx.wxID_OK then
    return ide:SetProject(filePicker:GetPath())
  end
  return false
end

frame:Connect(ID_PROJECTDIRCHOOSE, wx.wxEVT_COMMAND_MENU_SELECTED, projChoose)

local function projFromFile(event)
  local editor = GetEditor()
  if not editor then return end
  local id = editor:GetId()
  local filepath = openDocuments[id].filePath
  if not filepath then return end
  local fn = wx.wxFileName(filepath)
  fn:Normalize() -- want absolute path for dialog

  if ide.interpreter then
    ide:SetProject(ide.interpreter:fprojdir(fn))
  end
end
frame:Connect(ID_PROJECTDIRFROMFILE, wx.wxEVT_COMMAND_MENU_SELECTED, projFromFile)
frame:Connect(ID_PROJECTDIRFROMFILE, wx.wxEVT_UPDATE_UI,
  function (event)
    local editor = GetEditor()
    event:Enable(editor ~= nil and ide:GetDocument(editor):GetFilePath() ~= nil)
  end)

----------------------
-- Interpreter Running

local function getNameToRun(skipcheck)
  local editor = GetEditor()
  if not editor then return end

  -- test compile it before we run it, if successful then ask to save
  -- only compile if lua api
  if editor.spec.apitype and
    editor.spec.apitype == "lua" and
    (not skipcheck) and
    (not ide.interpreter.skipcompile) and
    (not CompileProgram(editor, { reportstats = false })) then
    return
  end

  local doc = ide:GetDocument(editor)
  local name = ide:GetProjectStartFile() or doc:GetFilePath()
  if not name then doc:SetModified(true) end
  if not SaveIfModified(editor) then return end
  if ide.config.editor.saveallonrun then SaveAll(true) end

  return wx.wxFileName(name or doc:GetFilePath())
end

local function runInterpreter(wfilename, withdebugger)
  ClearOutput()
  ide:GetOutput():Activate()

  ClearAllCurrentLineMarkers()
  if not wfilename or not ide.interpreter.frun then return end
  local pid = ide.interpreter:frun(wfilename, withdebugger)
  if pid then OutputEnableInput() end
  ide:SetLaunchedProcess(pid)
  return pid
end

function ProjectRun(skipcheck)
  local fname = getNameToRun(skipcheck)
  if not fname then return end
  return runInterpreter(fname)
end

local debuggers = {
  debug = "require('mobdebug').loop('%s',%d)",
  scratchpad = "require('mobdebug').scratchpad('%s',%d)"
}

function ProjectDebug(skipcheck, debtype)
  local debugger = ide:GetDebugger()
  if (debugger:IsConnected()) then
    if (debugger.scratchpad and debugger.scratchpad.paused) then
      debugger.scratchpad.paused = nil
      debugger.scratchpad.updated = true
      ide:GetConsole():SetRemote(nil) -- disable remote while Scratchpad running
    elseif (not debugger:IsRunning()) then
      debugger:Run()
    end
  else
    if not debugger:IsListening() then debugger:Listen() end
    local debcall = (debuggers[debtype or "debug"]):
      format(debugger:GetHostName(), debugger:GetPortNumber())
    local fname = getNameToRun(skipcheck)
    if not fname then return end
    return runInterpreter(fname, debcall) -- this may be pid or nil
  end
  return true
end

-----------------------
-- Actions

local BREAKPOINT_MARKER = StylesGetMarker("breakpoint")

frame:Connect(ID_BREAKPOINTTOGGLE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() GetEditor():BreakpointToggle() end)
frame:Connect(ID_BREAKPOINTTOGGLE, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    local editor = GetEditorWithFocus(GetEditor())
    event:Enable(ide.interpreter and ide.interpreter.hasdebugger and (not debugger.scratchpad)
      and (editor ~= nil) and (not editor:IsLineEmpty()))
  end)

frame:Connect(ID_BREAKPOINTNEXT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function()
    local BPNSC = KSC(ID_BREAKPOINTNEXT):gsub("\t","")
    if not GetEditor():MarkerGotoNext(BREAKPOINT_MARKER) and BPNSC == "F9" then
      local osx = ide.osname == "Macintosh"
      ide:Print(("You used '%s' shortcut that has been changed from toggling a breakpoint to navigating to the next breakpoint in the document.")
        :format(BPNSC))
      -- replace Ctrl with Cmd, but not in RawCtrl
      ide:Print(("To toggle a breakpoint, use '%s' or click in the editor margin.")
        :format(KSC(ID_BREAKPOINTTOGGLE):gsub("\t",""):gsub("%f[%w]Ctrl", osx and "Cmd" or "Ctrl")))
    end
  end)
frame:Connect(ID_BREAKPOINTPREV, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() GetEditor():MarkerGotoPrev(BREAKPOINT_MARKER) end)

frame:Connect(ID_BREAKPOINTNEXT, wx.wxEVT_UPDATE_UI,
  function (event) event:Enable(GetEditor() ~= nil) end)
frame:Connect(ID_BREAKPOINTPREV, wx.wxEVT_UPDATE_UI,
  function (event) event:Enable(GetEditor() ~= nil) end)

frame:Connect(ID_COMPILE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    ide:GetOutput():Activate()
    CompileProgram(GetEditor(), {
        keepoutput = ide:GetLaunchedProcess() ~= nil or ide:GetDebugger():IsConnected()
    })
  end)
frame:Connect(ID_COMPILE, wx.wxEVT_UPDATE_UI,
  function (event) event:Enable(GetEditor() ~= nil) end)

frame:Connect(ID_RUN, wx.wxEVT_COMMAND_MENU_SELECTED, function () ProjectRun() end)
frame:Connect(ID_RUN, wx.wxEVT_UPDATE_UI,
  function (event)
    event:Enable(ide:GetDebugger():IsConnected() == nil and
                 ide:GetLaunchedProcess() == nil and
                 (ide.interpreter.frun ~= nil) and -- nil == no running from this interpreter
                 ide:GetEditor() ~= nil)
  end)

frame:Connect(ID_RUNNOW, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local debugger = ide:GetDebugger()
    if debugger.scratchpad then
      debugger:ScratchpadOff()
    else
      debugger:ScratchpadOn(ide:GetEditor())
    end
  end)
frame:Connect(ID_RUNNOW, wx.wxEVT_UPDATE_UI,
  function (event)
    local editor = GetEditor()
    local debugger = ide:GetDebugger()
    -- allow scratchpad if there is no server or (there is a server and it is
    -- allowed to turn it into a scratchpad) and we are not debugging anything
    event:Enable((ide.interpreter) and (ide.interpreter.hasdebugger) and
                 (ide.interpreter.frun ~= nil) and -- nil == no running from this interpreter
                 (ide.interpreter.scratchextloop ~= nil) and -- nil == no scratchpad support
                 (editor ~= nil) and ((debugger:IsConnected() == nil or debugger.scratchable)
                 and ide:GetLaunchedProcess() == nil or debugger.scratchpad ~= nil))
    local isscratchpad = debugger.scratchpad ~= nil
    menuBar:Check(ID_RUNNOW, isscratchpad)
    local tool = ide:GetToolBar():FindTool(ID_RUNNOW)
    if tool and tool:IsSticky() ~= isscratchpad then
      tool:SetSticky(isscratchpad)
      ide:GetToolBar():Refresh()
    end
  end)

frame:Connect(ID_ATTACHDEBUG, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    ide:GetDebugger():Listen(event:IsChecked()) -- start/stop listening
    if event:IsChecked() and ide.interpreter.fattachdebug then ide.interpreter:fattachdebug() end
  end)
frame:Connect(ID_ATTACHDEBUG, wx.wxEVT_UPDATE_UI,
  function (event)
    event:Enable(ide.interpreter and ide.interpreter.fattachdebug and true or false)
    ide.frame.menuBar:Check(event:GetId(), ide:GetDebugger():IsListening() and true or false)
  end)

frame:Connect(ID_STARTDEBUG, wx.wxEVT_COMMAND_MENU_SELECTED, function () ProjectDebug() end)
frame:Connect(ID_STARTDEBUG, wx.wxEVT_UPDATE_UI,
  function (event)
    local editor = GetEditor()
    local debugger = ide:GetDebugger()
    event:Enable((ide.interpreter) and (ide.interpreter.hasdebugger) and
                 (ide.interpreter.frun ~= nil) and -- nil == no running from this interpreter
      ((debugger:IsConnected() == nil and ide:GetLaunchedProcess() == nil and editor ~= nil) or
       (debugger:IsConnected() ~= nil and not debugger:IsRunning())) and
      (not debugger.scratchpad or debugger.scratchpad.paused))
    local isconnected = debugger:IsConnected() ~= nil
    local label, other = debugMenuRunLabel[isconnected], debugMenuRunLabel[not isconnected]
    if debugMenu:GetLabelText(ID_STARTDEBUG) == wx.wxMenuItem.GetLabelText(other) then
      debugMenu:SetLabel(ID_STARTDEBUG, label..KSC(ID_STARTDEBUG))
    end
  end)

frame:Connect(ID_STOPDEBUG, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () ide:GetDebugger():Stop() end)
frame:Connect(ID_STOPDEBUG, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable(debugger:IsConnected() ~= nil or ide:GetLaunchedProcess() ~= nil)
    local isdebugging = debugger:IsConnected() ~= nil
    local label, other = debugMenuStopLabel[isdebugging], debugMenuStopLabel[not isdebugging]
    if debugMenu:GetLabelText(ID_STOPDEBUG) == wx.wxMenuItem.GetLabelText(other) then
      debugMenu:SetLabel(ID_STOPDEBUG, label..KSC(ID_STOPDEBUG))
    end
  end)

frame:Connect(ID_DETACHDEBUG, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () ide:GetDebugger():detach() end)
frame:Connect(ID_DETACHDEBUG, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable(debugger:IsConnected() ~= nil and (not debugger.scratchpad))
  end)

frame:Connect(ID_RUNTO, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    local editor = GetEditor()
    ide:GetDebugger():RunTo(editor, editor:GetCurrentLine()+1)
  end)
frame:Connect(ID_RUNTO, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable((debugger:IsConnected() ~= nil) and (not debugger:IsRunning())
      and (ide:GetEditor() ~= nil) and (not debugger.scratchpad))
  end)

frame:Connect(ID_STEP, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () ide:GetDebugger():Step() end)
frame:Connect(ID_STEP, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable((debugger:IsConnected() ~= nil) and (not debugger:IsRunning())
      and (ide:GetEditor() ~= nil) and (not debugger.scratchpad))
  end)

frame:Connect(ID_STEPOVER, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () ide:GetDebugger():Over() end)
frame:Connect(ID_STEPOVER, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable((debugger:IsConnected() ~= nil) and (not debugger:IsRunning())
      and (ide:GetEditor() ~= nil) and (not debugger.scratchpad))
  end)

frame:Connect(ID_STEPOUT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () ide:GetDebugger():Out() end)
frame:Connect(ID_STEPOUT, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable((debugger:IsConnected() ~= nil) and (not debugger:IsRunning())
      and (ide:GetEditor() ~= nil) and (not debugger.scratchpad))
  end)

frame:Connect(ID_TRACE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function () ide:GetDebugger():trace() end)
frame:Connect(ID_TRACE, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable((debugger:IsConnected() ~= nil) and (not debugger:IsRunning())
      and (ide:GetEditor() ~= nil) and (not debugger.scratchpad))
  end)

frame:Connect(ID_BREAK, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    local debugger = ide:GetDebugger()
    if debugger.server then
      debugger:Break()
      if debugger.scratchpad then
        debugger.scratchpad.paused = true
        ide:GetConsole():SetRemote(debugger:GetConsole())
      end
    end
  end)
frame:Connect(ID_BREAK, wx.wxEVT_UPDATE_UI,
  function (event)
    local debugger = ide:GetDebugger()
    event:Enable(debugger:IsConnected() ~= nil
      and (debugger:IsRunning()
           or (debugger.scratchpad and not debugger.scratchpad.paused)))
  end)

frame:Connect(ID_COMMANDLINEPARAMETERS, wx.wxEVT_COMMAND_MENU_SELECTED,
  function ()
    local params = ide:GetTextFromUser(TR("Enter command line parameters"),
      TR("Command line parameters"), ide.config.arg.any or "")
    -- params is `nil` when the dialog is canceled
    if params then ide:SetCommandLineParameters(params) end
  end)
frame:Connect(ID_COMMANDLINEPARAMETERS, wx.wxEVT_UPDATE_UI,
  function (event)
    local interpreter = ide:GetInterpreter()
    event:Enable(interpreter and interpreter.takeparameters and true or false)
  end)

-- save and restore command line parameters
ide:AddPackage("core.project", {
    AddCmdLine = function(self, params)
      local settings = self:GetSettings()
      local arglist = settings.arglist or {}
      PrependStringToArray(arglist, params, ide.config.commandlinehistorylength)
      settings.arglist = arglist
      self:SetSettings(settings)
    end,
    GetCmdLines = function(self) return self:GetSettings().arglist or {} end,

    onProjectLoad = function(self, project)
      local settings = self:GetSettings()
      if type(settings.arg) == "table" then
        ide:SetConfig("arg.any", settings.arg[project], project)
      end
      local interpreter = ide:GetInterpreter()
      if interpreter then interpreter:UpdateStatus() end
    end,
    onProjectClose = function(self, project)
      local settings = self:GetSettings()
      if type(settings.arg) ~= "table" then settings.arg = {} end
      if settings.arg[project] ~= ide.config.arg.any then
        settings.arg[project] = ide.config.arg.any
        self:SetSettings(settings)
      end
    end,
})
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local frame = ide.frame
local menuBar = frame.menuBar
local findReplace = ide.findReplace

local findMenu = ide:MakeMenu {
  { ID_FIND, TR("&Find")..KSC(ID_FIND), TR("Find text") },
  { ID_FINDNEXT, TR("Find &Next")..KSC(ID_FINDNEXT), TR("Find the next text occurrence") },
  { ID_FINDPREV, TR("Find &Previous")..KSC(ID_FINDPREV), TR("Find the earlier text occurence") },
  { ID_FINDSELECTNEXT, TR("Select And Find Next")..KSC(ID_FINDSELECTNEXT), TR("Select the word under cursor and find its next occurrence") },
  { ID_FINDSELECTPREV, TR("Select And Find Previous")..KSC(ID_FINDSELECTPREV), TR("Select the word under cursor and find its previous occurrence") },
  { ID_REPLACE, TR("&Replace")..KSC(ID_REPLACE), TR("Find and replace text") },
  { },
  { ID_FINDINFILES, TR("Find &In Files")..KSC(ID_FINDINFILES), TR("Find text in files") },
  { ID_REPLACEINFILES, TR("Re&place In Files")..KSC(ID_REPLACEINFILES), TR("Find and replace text in files") },
  { },
  { ID_NAVIGATE, TR("Navigate"), "", {
    { ID_NAVIGATETOFILE, TR("Go To File...")..KSC(ID_NAVIGATETOFILE), TR("Go to file") },
    { ID_NAVIGATETOLINE, TR("Go To Line...")..KSC(ID_NAVIGATETOLINE), TR("Go to line") },
    { ID_NAVIGATETOSYMBOL, TR("Go To Symbol...")..KSC(ID_NAVIGATETOSYMBOL), TR("Go to symbol") },
    { ID_NAVIGATETOMETHOD, TR("Insert Library Function...")..KSC(ID_NAVIGATETOMETHOD), TR("Find and insert library function") },
  } },
}
menuBar:Append(findMenu, TR("&Search"))

-- allow search functions for either Editor with focus (which includes editor-like panels)
-- or editor tabs (even when the current editor is not in focus)
local function onUpdateUISearchMenu(event) event:Enable((GetEditorWithFocus() or GetEditor()) ~= nil) end

frame:Connect(ID_FIND, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    findReplace:Show(false)
  end)
frame:Connect(ID_FIND, wx.wxEVT_UPDATE_UI, onUpdateUISearchMenu)

frame:Connect(ID_REPLACE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    findReplace:Show(true)
  end)
frame:Connect(ID_REPLACE, wx.wxEVT_UPDATE_UI, onUpdateUISearchMenu)

frame:Connect(ID_FINDINFILES, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    findReplace:Show(false,true)
  end)
frame:Connect(ID_REPLACEINFILES, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    findReplace:Show(true,true)
  end)

frame:Connect(ID_FINDNEXT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditor()
    if editor and ide.wxver >= "2.9.5" and editor:GetSelections() > 1 then
      local selection = editor:GetMainSelection() + 1
      if selection >= editor:GetSelections() then selection = 0 end
      editor:SetMainSelection(selection)
      editor:ShowPosEnforcePolicy(editor:GetCurrentPos())
    else
      if findReplace:SetFind(findReplace:GetFind() or findReplace:GetSelection()) then
        findReplace:Find()
      else
        findReplace:Show(false)
      end
    end
  end)
frame:Connect(ID_FINDNEXT, wx.wxEVT_UPDATE_UI, onUpdateUISearchMenu)

frame:Connect(ID_FINDPREV, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local editor = GetEditor()
    if editor and ide.wxver >= "2.9.5" and editor:GetSelections() > 1 then
      local selection = editor:GetMainSelection() - 1
      if selection < 0 then selection = editor:GetSelections() - 1 end
      editor:SetMainSelection(selection)
      editor:ShowPosEnforcePolicy(editor:GetCurrentPos())
    else
      if findReplace:SetFind(findReplace:GetFind() or findReplace:GetSelection()) then
        findReplace:Find(true) -- search up
      else
        findReplace:Show(false)
      end
    end
  end)
frame:Connect(ID_FINDPREV, wx.wxEVT_UPDATE_UI, onUpdateUISearchMenu)

-- Select and Find behaves like Find if there is a current selection;
-- if not, it selects a word under cursor (if any) and does find.

frame:Connect(ID_FINDSELECTNEXT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    if findReplace:SetFind(findReplace:GetSelection() or findReplace:GetWordAtCaret()) then
      findReplace:Find()
    end
  end)
frame:Connect(ID_FINDSELECTNEXT, wx.wxEVT_UPDATE_UI, onUpdateUISearchMenu)

frame:Connect(ID_FINDSELECTPREV, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    if findReplace:SetFind(findReplace:GetSelection() or findReplace:GetWordAtCaret()) then
      findReplace:Find(true)
    end
  end)
frame:Connect(ID_FINDSELECTPREV, wx.wxEVT_UPDATE_UI, onUpdateUISearchMenu)

local special = {SYMBOL = '@', LINE = ':', METHOD = ';'}
frame:Connect(ID_NAVIGATETOFILE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() ide:ShowCommandBar("") end)
frame:Connect(ID_NAVIGATETOLINE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() ide:ShowCommandBar(special.LINE) end)
frame:Connect(ID_NAVIGATETOMETHOD, wx.wxEVT_COMMAND_MENU_SELECTED,
  function() ide:ShowCommandBar(special.METHOD) end)
frame:Connect(ID_NAVIGATETOSYMBOL, wx.wxEVT_COMMAND_MENU_SELECTED,
  function()
    local ed = GetEditor()
    ide:ShowCommandBar(special.SYMBOL, ed and ed:ValueFromPosition(ed:GetCurrentPos()))
  end)
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local frame = ide.frame
local menuBar = frame.menuBar
local uimgr = frame.uimgr

local viewMenu = ide:MakeMenu {
  { ID_VIEWFILETREE, TR("Project/&FileTree Window")..KSC(ID_VIEWFILETREE), TR("View the project/filetree window"), wx.wxITEM_CHECK },
  { ID_VIEWOUTPUT, TR("&Output/Console Window")..KSC(ID_VIEWOUTPUT), TR("View the output/console window"), wx.wxITEM_CHECK },
  { ID_VIEWWATCHWINDOW, TR("&Watch Window")..KSC(ID_VIEWWATCHWINDOW), TR("View the watch window"), wx.wxITEM_CHECK },
  { ID_VIEWCALLSTACK, TR("&Stack Window")..KSC(ID_VIEWCALLSTACK), TR("View the stack window"), wx.wxITEM_CHECK },
  { ID_VIEWOUTLINE, TR("Outline Window")..KSC(ID_VIEWOUTLINE), TR("View the outline window"), wx.wxITEM_CHECK },
  { ID_VIEWMARKERS, TR("Markers Window")..KSC(ID_VIEWMARKERS), TR("View the markers window"), wx.wxITEM_CHECK },
  { },
  { ID_VIEWTOOLBAR, TR("&Tool Bar")..KSC(ID_VIEWTOOLBAR), TR("Show/Hide the toolbar"), wx.wxITEM_CHECK },
  { ID_VIEWSTATUSBAR, TR("&Status Bar")..KSC(ID_VIEWSTATUSBAR), TR("Show/Hide the status bar"), wx.wxITEM_CHECK },
  { },
  { ID_VIEWDEFAULTLAYOUT, TR("&Default Layout")..KSC(ID_VIEWDEFAULTLAYOUT), TR("Reset to default layout") },
  { ID_VIEWFULLSCREEN, TR("Full &Screen")..KSC(ID_VIEWFULLSCREEN), TR("Switch to or from full screen mode") },
}

do -- Add zoom submenu
  local zoomMenu = ide:MakeMenu {
    {ID_ZOOMRESET, TR("Zoom to 100%")..KSC(ID_ZOOMRESET)},
    {ID_ZOOMIN, TR("Zoom In")..KSC(ID_ZOOMIN)},
    {ID_ZOOMOUT, TR("Zoom Out")..KSC(ID_ZOOMOUT)},
  }

  frame:Connect(ID_ZOOMRESET, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() local editor = GetEditorWithFocus()
      if editor then editor:SetZoom(0) end end)
  frame:Connect(ID_ZOOMIN, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() local editor = GetEditorWithFocus()
      if editor then editor:SetZoom(editor:GetZoom()+1) end end)
  frame:Connect(ID_ZOOMOUT, wx.wxEVT_COMMAND_MENU_SELECTED,
    function() local editor = GetEditorWithFocus()
      if editor then editor:SetZoom(editor:GetZoom()-1) end end)

  -- only enable if there is an editor
  local iseditor = function (event) event:Enable(GetEditorWithFocus() ~= nil) end
  for _, id in ipairs({ID_ZOOMRESET, ID_ZOOMIN, ID_ZOOMOUT}) do
    frame:Connect(id, wx.wxEVT_UPDATE_UI, iseditor)
  end

  viewMenu:Append(ID_ZOOM, TR("Zoom"), zoomMenu)
end

menuBar:Append(viewMenu, TR("&View"))

local panels = {
  [ID_VIEWOUTPUT] = "bottomnotebook",
  [ID_VIEWFILETREE] = "projpanel",
  [ID_VIEWWATCHWINDOW] = "watchpanel",
  [ID_VIEWCALLSTACK] = "stackpanel",
  [ID_VIEWOUTLINE] = "outlinepanel",
  [ID_VIEWMARKERS] = "markerspanel",
  [ID_VIEWTOOLBAR] = "toolbar",
}

local function togglePanel(event)
  local panel = panels[event:GetId()]
  local pane = uimgr:GetPane(panel)
  local shown = not pane:IsShown()
  if not shown then pane:BestSize(pane.window:GetSize()) end
  pane:Show(shown)
  uimgr:Update()

  return shown
end

local function checkPanel(event)
  local pane = uimgr:GetPane(panels[event:GetId()])
  event:Enable(pane:IsOk()) -- disable if doesn't exist
  menuBar:Check(event:GetId(), pane:IsOk() and pane:IsShown())
end

frame:Connect(ID_VIEWDEFAULTLAYOUT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    uimgr:LoadPerspective(uimgr.defaultPerspective, true)
  end)
  
frame:Connect(ID_VIEWMINIMIZE, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) ide.frame:Iconize(true) end)

frame:Connect(ID_VIEWFULLSCREEN, wx.wxEVT_COMMAND_MENU_SELECTED, function ()
    ShowFullScreen(not frame:IsFullScreen())
  end)
frame:Connect(ID_VIEWFULLSCREEN, wx.wxEVT_UPDATE_UI,
  function (event) event:Enable(GetEditor() ~= nil) end)

frame:Connect(ID_VIEWOUTPUT, wx.wxEVT_COMMAND_MENU_SELECTED, togglePanel)
frame:Connect(ID_VIEWFILETREE, wx.wxEVT_COMMAND_MENU_SELECTED, togglePanel)
frame:Connect(ID_VIEWTOOLBAR, wx.wxEVT_COMMAND_MENU_SELECTED, togglePanel)
frame:Connect(ID_VIEWOUTLINE, wx.wxEVT_COMMAND_MENU_SELECTED, togglePanel)
frame:Connect(ID_VIEWMARKERS, wx.wxEVT_COMMAND_MENU_SELECTED, togglePanel)
frame:Connect(ID_VIEWWATCHWINDOW, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) if togglePanel(event) then ide:GetDebugger():RefreshPanels() end end)
frame:Connect(ID_VIEWCALLSTACK, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event) if togglePanel(event) then ide:GetDebugger():RefreshPanels() end end)

frame:Connect(ID_VIEWSTATUSBAR, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    frame:GetStatusBar():Show(menuBar:IsChecked(event:GetId()))
    uimgr:Update()
  end)
frame:Connect(ID_VIEWSTATUSBAR, wx.wxEVT_UPDATE_UI,
  function (event) menuBar:Check(event:GetId(), frame:GetStatusBar():IsShown()) end)

for id in pairs(panels) do frame:Connect(id, wx.wxEVT_UPDATE_UI, checkPanel) end
-- Copyright 2014-17 Paul Kulchenko, ZeroBrane LLC

local ide = ide
ide.outline = {
  outlineCtrl = nil,
  imglist = ide:CreateImageList("OUTLINE", "FILE-NORMAL", "VALUE-LCALL",
    "VALUE-GCALL", "VALUE-ACALL", "VALUE-SCALL", "VALUE-MCALL"),
  settings = {
    symbols = {},
    ignoredirs = {},
  },
  needsaving = false,
  needrefresh = nil,
  indexqueue = {[0] = {}},
  indexpurged = false, -- flag that the index has been purged from old records; once per session
}

local outline = ide.outline
local image = { FILE = 0, LFUNCTION = 1, GFUNCTION = 2, AFUNCTION = 3,
  SMETHOD = 4, METHOD = 5,
}
local q = EscapeMagic
local caches = {}

local function setData(ctrl, item, value)
  if ide.wxver >= "2.9.5" then
    local data = wx.wxLuaTreeItemData()
    data:SetData(value)
    ctrl:SetItemData(item, data)
  end
end

local function resetOutlineTimer()
  if ide.config.outlineinactivity then
    ide.timers.outline:Start(ide.config.outlineinactivity*1000, wx.wxTIMER_ONE_SHOT)
  end
end

local function resetIndexTimer(interval)
  if ide.timers.symbolindex and ide.config.symbolindexinactivity and not ide.timers.symbolindex:IsRunning() then
    ide.timers.symbolindex:Start(interval or ide.config.symbolindexinactivity*1000, wx.wxTIMER_ONE_SHOT)
  end
end

local function outlineRefresh(editor, force)
  if not editor then return end
  local tokens = editor:GetTokenList()
  local sep = editor.spec.sep
  local varname = "([%w_][%w_"..q(sep:sub(1,1)).."]*)"
  local funcs = {updated = TimeGet()}
  local var = {}
  local outcfg = ide.config.outline or {}
  local scopes = {}
  local funcnum = 0
  local SCOPENUM, FUNCNUM = 1, 2
  local text
  for _, token in ipairs(tokens) do
    local op = token[1]
    if op == 'Var' or op == 'Id' then
      var = {name = token.name, fpos = token.fpos, global = token.context[token.name] == nil}
    elseif outcfg.showcurrentfunction and op == 'Scope' then
      local fundepth = #scopes
      if token.name == '(' then -- a function starts a new scope
        funcnum = funcnum + 1 -- increment function count
        local nested = fundepth == 0 or scopes[fundepth][SCOPENUM] > 0
        scopes[fundepth + (nested and 1 or 0)] = {1, funcnum}
      elseif fundepth > 0 then
        scopes[fundepth][SCOPENUM] = scopes[fundepth][SCOPENUM] + 1
      end
    elseif outcfg.showcurrentfunction and op == 'EndScope' then
      local fundepth = #scopes
      if fundepth > 0 and scopes[fundepth][SCOPENUM] > 0 then
        scopes[fundepth][SCOPENUM] = scopes[fundepth][SCOPENUM] - 1
        if scopes[fundepth][SCOPENUM] == 0 then
          local funcnum = scopes[fundepth][FUNCNUM]
          if funcs[funcnum] then
            funcs[funcnum].poe = token.fpos + (token.name and #token.name or 0)
          end
          table.remove(scopes)
        end
      end
    elseif op == 'Function' then
      local depth = token.context['function'] or 1
      local name, pos = token.name, token.fpos
      text = text or editor:GetTextDyn()
      local _, _, rname, params = text:find('([^(]*)(%b())', pos)
      if rname then rname = rname:gsub("%s+$","") end
      -- if something else got captured, then don't show any parameters
      if name and rname and name ~= rname then params = "" end
      if not name then
        local s = editor:PositionFromLine(editor:LineFromPosition(pos-1))
        local rest
        rest, pos, name = text:sub(s+1, pos-1):match('%s*(.-)()'..varname..'%s*=%s*function%s*$')
        if rest then
          pos = s + pos
          -- guard against "foo, bar = function() end" as it would get "bar"
          if #rest>0 and rest:find(',') then name = nil end
        end
      end
      local ftype = image.LFUNCTION
      if not name then
        ftype = image.AFUNCTION
      elseif outcfg.showmethodindicator and name:find('['..q(sep)..']') then
        ftype = name:find(q(sep:sub(1,1))) and image.SMETHOD or image.METHOD
      elseif var.name == name and var.fpos == pos
      or var.name and name:find('^'..var.name..'['..q(sep)..']') then
        ftype = var.global and image.GFUNCTION or image.LFUNCTION
      end
      name = name or outcfg.showanonymous
      funcs[#funcs+1] = {
        name = ((name or '~')..(params or "")):gsub("%s+", " "),
        skip = (not name) and true or nil,
        depth = depth,
        image = ftype,
        pos = name and pos or token.fpos,
      }
    end
  end

  if force == nil then return funcs end

  local ctrl = outline.outlineCtrl
  if not ctrl then return end -- outline can be completely removed/disabled

  local cache = caches[editor] or {}
  caches[editor] = cache

  -- add file
  local filename = ide:GetDocument(editor):GetTabText()
  local fileitem = cache.fileitem
  if not fileitem or not fileitem:IsOk() then
    local root = ctrl:GetRootItem()
    if not root or not root:IsOk() then return end

    if outcfg.showonefile then
      fileitem = root
    else
      fileitem = ctrl:AppendItem(root, filename, image.FILE)
      setData(ctrl, fileitem, editor)
      ctrl:SetItemBold(fileitem, true)
      ctrl:SortChildren(root)
    end
    cache.fileitem = fileitem
  end

  do -- check if any changes in the cached function list
    local prevfuncs = cache.funcs or {}
    local nochange = #funcs == #prevfuncs
    local resort = {} -- items that need to be re-sorted
    if nochange then
      for n, func in ipairs(funcs) do
        func.item = prevfuncs[n].item -- carry over cached items
        if func.depth ~= prevfuncs[n].depth then
          nochange = false
        elseif nochange and prevfuncs[n].item then
          if func.name ~= prevfuncs[n].name then
            ctrl:SetItemText(prevfuncs[n].item, func.name)
            if outcfg.sort then resort[ctrl:GetItemParent(prevfuncs[n].item)] = true end
          end
          if func.image ~= prevfuncs[n].image then
            ctrl:SetItemImage(prevfuncs[n].item, func.image)
          end
        end
      end
    end
    cache.funcs = funcs -- set new cache as positions may change
    if nochange and not force then -- return if no visible changes
      if outcfg.sort then -- resort items for all parents that have been modified
        for item in pairs(resort) do ctrl:SortChildren(item) end
      end
      return
    end
  end

  -- refresh the tree
  -- refreshing shouldn't change the focus of the current element,
  -- but it appears that DeleteChildren (wxwidgets 2.9.5 on Windows)
  -- moves the focus from the current element to wxTreeCtrl.
  -- need to save the window having focus and restore after the refresh.
  local win = ide:GetMainFrame():FindFocus()

  ctrl:Freeze()

  -- disabling event handlers is not strictly necessary, but it's expected
  -- to fix a crash on Windows that had DeleteChildren in the trace (#442).
  ctrl:SetEvtHandlerEnabled(false)
  ctrl:DeleteChildren(fileitem)
  ctrl:SetEvtHandlerEnabled(true)

  local edpos = editor:GetCurrentPos()+1
  local stack = {fileitem}
  local resort = {} -- items that need to be re-sorted
  for n, func in ipairs(funcs) do
    local depth = outcfg.showflat and 1 or func.depth
    local parent = stack[depth]
    while not parent do depth = depth - 1; parent = stack[depth] end
    if not func.skip then
      local item = ctrl:AppendItem(parent, func.name, func.image)
      if ide.config.outline.showcurrentfunction
      and edpos >= func.pos and func.poe and edpos <= func.poe then
        ctrl:SetItemBold(item, true)
      end
      if outcfg.sort then resort[parent] = true end
      setData(ctrl, item, n)
      func.item = item
      stack[func.depth+1] = item
    end
    func.skip = nil
  end
  if outcfg.sort then -- resort items for all parents that have been modified
    for item in pairs(resort) do ctrl:SortChildren(item) end
  end
  if outcfg.showcompact then ctrl:Expand(fileitem) else ctrl:ExpandAllChildren(fileitem) end

  -- scroll to the fileitem, but only if it's not a root item (as it's hidden)
  if fileitem:GetValue() ~= ctrl:GetRootItem():GetValue() then
    ctrl:ScrollTo(fileitem)
    ctrl:SetScrollPos(wx.wxHORIZONTAL, 0, true)
  else -- otherwise, scroll to the top
    ctrl:SetScrollPos(wx.wxVERTICAL, 0, true)
  end
  ctrl:Thaw()

  if win and win ~= ide:GetMainFrame():FindFocus() then win:SetFocus() end
end

local function indexFromQueue()
  if #outline.indexqueue == 0 then return end

  local editor = ide:GetEditor()
  local inactivity = ide.config.symbolindexinactivity
  if editor and inactivity and editor.updated > TimeGet()-inactivity then
    -- reschedule timer for later time
    resetIndexTimer()
  else
    local fname = table.remove(outline.indexqueue, 1)
    outline.indexqueue[0][fname] = nil
    -- check if fname is already loaded
    ide:SetStatusFor(TR("Indexing %d files: '%s'..."):format(#outline.indexqueue+1, fname))
    local content, err = FileRead(fname)
    if content then
      local editor = ide:CreateBareEditor()
      editor:SetupKeywords(GetFileExt(fname))
      editor:SetTextDyn(content)
      editor:Colourise(0, -1)
      editor:ResetTokenList()
      while IndicateAll(editor) do end

      outline:UpdateSymbols(fname, outlineRefresh(editor))
      editor:Destroy()
    else
      ide:Print(TR("Can't open file '%s': %s"):format(fname, err))
    end
    if #outline.indexqueue == 0 then
      outline:SaveSettings()
      ide:SetStatusFor(TR("Indexing completed."))
    end
    ide:DoWhenIdle(indexFromQueue)
  end
  return
end

local function createOutlineWindow()
  local width, height = 360, 200
  local ctrl = ide:CreateTreeCtrl(ide.frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxSize(width, height),
    wx.wxTR_LINES_AT_ROOT + wx.wxTR_HAS_BUTTONS
    + wx.wxTR_HIDE_ROOT + wx.wxNO_BORDER)

  outline.outlineCtrl = ctrl
  ide.timers.outline = ide:AddTimer(ctrl, function() outlineRefresh(GetEditor(), false) end)
  ide.timers.symbolindex = ide:AddTimer(ctrl, function() ide:DoWhenIdle(indexFromQueue) end)

  ctrl:AddRoot("Outline")
  ctrl:SetImageList(outline.imglist)
  ctrl:SetFont(ide.font.fNormal)

  function ctrl:ActivateItem(item_id)
    local data = ctrl:GetItemData(item_id)
    if ctrl:GetItemImage(item_id) == image.FILE then
      -- activate editor tab
      local editor = data:GetData()
      if not ide:GetEditorWithFocus(editor) then ide:GetDocument(editor):SetActive() end
    else
      -- activate tab and move cursor based on stored pos
      -- get file parent
      local onefile = (ide.config.outline or {}).showonefile
      local parent = ctrl:GetItemParent(item_id)
      if not onefile then -- find the proper parent
        while parent:IsOk() and ctrl:GetItemImage(parent) ~= image.FILE do
          parent = ctrl:GetItemParent(parent)
        end
        if not parent:IsOk() then return end
      end
      -- activate editor tab
      local editor = onefile and ide:GetEditor() or ctrl:GetItemData(parent):GetData()
      local cache = caches[editor]
      if editor and cache then
        -- move to position in the file
        editor:GotoPosEnforcePolicy(cache.funcs[data:GetData()].pos-1)
        -- only set editor active after positioning as this may change focus,
        -- which may regenerate the outline, which may invalidate `data` value
        if not ide:GetEditorWithFocus(editor) then ide:GetDocument(editor):SetActive() end
      end
    end
  end

  local function activateByPosition(event)
    local mask = (wx.wxTREE_HITTEST_ONITEMINDENT + wx.wxTREE_HITTEST_ONITEMLABEL
      + wx.wxTREE_HITTEST_ONITEMICON + wx.wxTREE_HITTEST_ONITEMRIGHT)
    local item_id, flags = ctrl:HitTest(event:GetPosition())

    if item_id and item_id:IsOk() and bit.band(flags, mask) > 0 then
      ctrl:ActivateItem(item_id)
    else
      event:Skip()
    end
    return true
  end

  if (ide.config.outline or {}).activateonclick then
    ctrl:Connect(wx.wxEVT_LEFT_DOWN, activateByPosition)
  end
  ctrl:Connect(wx.wxEVT_LEFT_DCLICK, activateByPosition)
  ctrl:Connect(wx.wxEVT_COMMAND_TREE_ITEM_ACTIVATED, function(event)
      ctrl:ActivateItem(event:GetItem())
    end)

  ctrl:Connect(ID_OUTLINESORT, wx.wxEVT_COMMAND_MENU_SELECTED,
    function()
      ide.config.outline.sort = not ide.config.outline.sort
      local ed = ide:GetEditor()
      if not ed then return end
      -- when showing one file only refresh outline for the current editor
      for editor, cache in pairs((ide.config.outline or {}).showonefile and {[ed] = caches[ed]} or caches) do
        ide:SetStatus(("Refreshing '%s'..."):format(ide:GetDocument(editor):GetFileName()))
        local isexpanded = ctrl:IsExpanded(cache.fileitem)
        outlineRefresh(editor, true)
        if not isexpanded then ctrl:Collapse(cache.fileitem) end
      end
      ide:SetStatus('')
    end)

  ctrl:Connect(wx.wxEVT_COMMAND_TREE_ITEM_MENU,
    function (event)
      local menu = ide:MakeMenu {
        { ID_OUTLINESORT, TR("Sort By Name"), "", wx.wxITEM_CHECK },
      }
      menu:Check(ID_OUTLINESORT, ide.config.outline.sort)

      PackageEventHandle("onMenuOutline", menu, ctrl, event)

      ctrl:PopupMenu(menu)
    end)


  local function reconfigure(pane)
    pane:TopDockable(false):BottomDockable(false)
        :MinSize(150,-1):BestSize(300,-1):FloatingSize(200,300)
  end

  local layout = ide:GetSetting("/view", "uimgrlayout")
  if not layout or not layout:find("outlinepanel") then
    ide:AddPanelDocked(ide:GetProjectNotebook(), ctrl, "outlinepanel", TR("Outline"), reconfigure, false)
  else
    ide:AddPanel(ctrl, "outlinepanel", TR("Outline"), reconfigure)
  end
end

local function eachNode(eachFunc, root, recursive)
  local ctrl = outline.outlineCtrl
  if not ctrl then return end
  root = root or ctrl:GetRootItem()
  if not (root and root:IsOk()) then return end
  local item = ctrl:GetFirstChild(root)
  while true do
    if not item:IsOk() then break end
    if eachFunc and eachFunc(ctrl, item) then break end
    if recursive and ctrl:ItemHasChildren(item) then eachNode(eachFunc, item, recursive) end
    item = ctrl:GetNextSibling(item)
  end
end

local pathsep = GetPathSeparator()
local function isInSubDir(name, path)
  return #name > #path and path..pathsep == name:sub(1, #path+#pathsep)
end

local function isIgnoredInIndex(name)
  local ignoredirs = outline.settings.ignoredirs
  if ignoredirs[name] then return true end

  -- check through ignored dirs to see if any of them match the file;
  -- skip those that are outside of the current project tree to allow
  -- scanning of the projects that may be inside ignored directories.
  local proj = ide:GetProject() -- `nil` when not set
  for path in pairs(ignoredirs) do
    if (not proj or isInSubDir(path, proj)) and isInSubDir(name, path) then return true end
  end

  return false
end

local function purgeIndex(path)
  local symbols = outline.settings.symbols
  for name in pairs(symbols) do
    if isInSubDir(name, path) then outline:UpdateSymbols(name, nil) end
  end
end

local function purgeQueue(path)
  local curqueue = outline.indexqueue
  local newqueue = {[0] = {}}
  for _, name in ipairs(curqueue) do
    if not isInSubDir(name, path) then
      table.insert(newqueue, name)
      newqueue[0][name] = true
    end
  end
  outline.indexqueue = newqueue
end

local function disableIndex(path)
  outline.settings.ignoredirs[path] = true
  outline:SaveSettings(true)

  -- purge the path from the index and the (current) queue
  purgeIndex(path)
  purgeQueue(path)
end

local function enableIndex(path)
  outline.settings.ignoredirs[path] = nil
  outline:SaveSettings(true)
  outline:RefreshSymbols(path)
end

local lastfocus
local package = ide:AddPackage('core.outline', {
    onRegister = function(self)
      if not ide.config.outlineinactivity then return end

      createOutlineWindow()
    end,

    -- remove the editor from the list
    onEditorClose = function(self, editor)
      local cache = caches[editor]
      local fileitem = cache and cache.fileitem
      caches[editor] = nil -- remove from cache

      if fileitem and fileitem:IsOk() then
        local ctrl = outline.outlineCtrl
        if (ide.config.outline or {}).showonefile then
          ctrl:DeleteChildren(fileitem)
        else
          ctrl:Delete(fileitem)
        end
      end
    end,

    -- handle rename of the file in the current editor
    onEditorSave = function(self, editor)
      if (ide.config.outline or {}).showonefile then return end
      local cache = caches[editor]
      local fileitem = cache and cache.fileitem
      local doc = ide:GetDocument(editor)
      local ctrl = outline.outlineCtrl
      if doc and fileitem and ctrl:GetItemText(fileitem) ~= doc:GetTabText() then
        ctrl:SetItemText(fileitem, doc:GetTabText())
      end
      local path = doc and doc:GetFilePath()
      if path and cache and cache.funcs then
        outline:UpdateSymbols(path, cache.funcs.updated > editor.updated and cache.funcs or nil)
        outline:SaveSettings()
      end
    end,

    -- go over the file items to turn bold on/off or collapse/expand
    onEditorFocusSet = function(self, editor)
      local cache = caches[editor]

      -- if the editor is not in the cache, which may happen if the user
      -- quickly switches between tabs that don't have outline generated,
      -- regenerate it manually
      if not cache then resetOutlineTimer() end
      resetIndexTimer()

      if (ide.config.outline or {}).showonefile and ide.config.outlineinactivity then
        -- this needs to be done when editor gets focus, but during active auto-complete
        -- the focus shifts between the editor and the popup after each character;
        -- the refresh is not necessary in this case, so only refresh when the editor changes
        if not lastfocus or editor:GetId() ~= lastfocus then
          outlineRefresh(editor, true)
          lastfocus = editor:GetId()
        end
        return
      end

      local fileitem = cache and cache.fileitem
      local ctrl = outline.outlineCtrl
      local itemname = ide:GetDocument(editor):GetTabText()

      -- update file name if it changed in the editor
      if fileitem and ctrl:GetItemText(fileitem) ~= itemname then
        ctrl:SetItemText(fileitem, itemname)
      end

      eachNode(function(ctrl, item)
          local found = fileitem and item:GetValue() == fileitem:GetValue()
          if not found and ctrl:IsBold(item) then
            ctrl:SetItemBold(item, false)
            ctrl:CollapseAllChildren(item)
          end
        end)

      if fileitem and not ctrl:IsBold(fileitem) then
        -- run the following changes on idle as doing them inline is causing a strange
        -- issue on OSX when clicking on a tab may skip several tabs (#546);
        -- this is somehow caused by `ExpandAllChildren` triggered from `SetFocus` inside
        -- `PAGE_CHANGED` handler for the notebook.
        ide:DoWhenIdle(function()
            -- check if this editor is still in the cache,
            -- as it may be closed before this handler is executed
            if not caches[editor] then return end
            ctrl:SetItemBold(fileitem, true)
            if (ide.config.outline or {}).showcompact then
              ctrl:Expand(fileitem)
            else
              ctrl:ExpandAllChildren(fileitem)
            end
            ctrl:ScrollTo(fileitem)
            ctrl:SetScrollPos(wx.wxHORIZONTAL, 0, true)
          end)
      end
    end,

    onMenuFiletree = function(self, menu, tree, event)
      local item_id = event:GetItem()
      local name = tree:GetItemFullName(item_id)
      local symboldirmenu = ide:MakeMenu {
        {ID_SYMBOLDIRREFRESH, TR("Refresh Index"), TR("Refresh indexed symbols from files in the selected directory")},
        {ID_SYMBOLDIRDISABLE, TR("Disable Indexing For '%s'"):format(name), TR("Ignore and don't index symbols from files in the selected directory")},
      }
      local _, _, projdirpos = ide:FindMenuItem(ID_PROJECTDIR, menu)
      if projdirpos then
        local ignored = isIgnoredInIndex(name)
        local enabledirmenu = ide:MakeMenu {}
        local paths = {}
        for path in pairs(outline.settings.ignoredirs) do table.insert(paths, path) end
        table.sort(paths)
        for i, path in ipairs(paths) do
          local id = ID("file.enablesymboldir."..i)
          enabledirmenu:Append(id, path, "")
          tree:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED, function() enableIndex(path) end)
        end

        symboldirmenu:Append(wx.wxMenuItem(symboldirmenu, ID_SYMBOLDIRENABLE,
          TR("Enable Indexing"), "", wx.wxITEM_NORMAL, enabledirmenu))
        menu:Insert(projdirpos+1, wx.wxMenuItem(menu, ID_SYMBOLDIRINDEX,
          TR("Symbol Index"), "", wx.wxITEM_NORMAL, symboldirmenu))

        -- disable "enable" if it's empty
        menu:Enable(ID_SYMBOLDIRENABLE, #paths > 0)
        -- disable "refresh" and "disable" if the directory is ignored
        -- or if any of the directories above it are ignored
        menu:Enable(ID_SYMBOLDIRREFRESH, tree:IsDirectory(item_id) and not ignored)
        menu:Enable(ID_SYMBOLDIRDISABLE, tree:IsDirectory(item_id) and not ignored)

        tree:Connect(ID_SYMBOLDIRREFRESH, wx.wxEVT_COMMAND_MENU_SELECTED, function()
            -- purge files in this directory as some might have been removed;
            -- files will be purged based on time, but this is a good time to clean.
            purgeIndex(name)
            outline:RefreshSymbols(name)
            resetIndexTimer(1) -- start after 1ms
          end)
        tree:Connect(ID_SYMBOLDIRDISABLE, wx.wxEVT_COMMAND_MENU_SELECTED, function()
            disableIndex(name)
          end)
       end
    end,

    onEditorUpdateUI = function(self, editor, event)
      -- only update when content or selection changes; ignore scrolling events
      if bit.band(event:GetUpdated(), wxstc.wxSTC_UPDATE_CONTENT + wxstc.wxSTC_UPDATE_SELECTION) > 0 then
        ide.outline.needrefresh = editor
      end
    end,

    onIdle = function(self)
      local editor = ide.outline.needrefresh
      if not editor then return end

      ide.outline.needrefresh = nil

      local ctrl = ide.outline.outlineCtrl
      if not ide:IsWindowShown(ctrl) then return end

      local cache = ide:IsValidCtrl(editor) and caches[editor]
      if not cache or not ide.config.outline.showcurrentfunction then return end

      local edpos = editor:GetCurrentPos()+1
      local edline = editor:LineFromPosition(edpos-1)+1
      if cache.pos and cache.pos == edpos then return end
      if cache.line and cache.line == edline then return end

      cache.pos = edpos
      cache.line = edline

      local n = 0
      local MIN, MAX = 1, 2
      local visible = {[MIN] = math.huge, [MAX] = 0}
      local needshown = {[MIN] = math.huge, [MAX] = 0}

      ctrl:Unselect()
      -- scan all items recursively starting from the current file
      eachNode(function(ctrl, item)
          local func = cache.funcs[ctrl:GetItemData(item):GetData()]
          if not func then return end
          local val = edpos >= func.pos and func.poe and edpos <= func.poe
          if edline == editor:LineFromPosition(func.pos)+1
          or (func.poe and edline == editor:LineFromPosition(func.poe)+1) then
            cache.line = nil
          end
          ctrl:SetItemBold(item, val)
          if val then ctrl:SelectItem(item, val) end

          if not ide.config.outline.jumptocurrentfunction then return end
          n = n + 1
          -- check that this and the items around it are all visible;
          -- this is to avoid the situation when the current item is only partially visible
          local isvisible = ctrl:IsVisible(item) and ctrl:GetNextVisible(item):IsOk() and ctrl:GetPrevVisible(item):IsOk()
          if val and not isvisible then
            needshown[MIN] = math.min(needshown[MIN], n)
            needshown[MAX] = math.max(needshown[MAX], n)
          elseif isvisible then
            visible[MIN] = math.min(visible[MIN], n)
            visible[MAX] = math.max(visible[MAX], n)
          end
        end, cache.fileitem, true)

      if not ide.config.outline.jumptocurrentfunction then return end
      if needshown[MAX] > visible[MAX] then
        ctrl:ScrollLines(needshown[MAX]-visible[MAX]) -- scroll forward to the last hidden line
      elseif needshown[MIN] < visible[MIN] then
        ctrl:ScrollLines(needshown[MIN]-visible[MIN]) -- scroll backward to the first hidden line
      end
    end,
  })

local function queuePath(path)
  -- only queue if symbols inactivity is set, so files will be indexed
  if ide.config.symbolindexinactivity and not outline.indexqueue[0][path] then
    outline.indexqueue[0][path] = true
    table.insert(outline.indexqueue, 1, path)
  end
end

function outline:GetFileSymbols(path)
  local symbols = self.settings.symbols[path]
  -- queue path to process when appropriate
  if not symbols then queuePath(path) end
  return symbols
end

function outline:GetEditorSymbols(editor)
  -- force token refresh (as these may be not updated yet)
  if #editor:GetTokenList() == 0 then
    while IndicateAll(editor) do end
  end

  -- only refresh the functions when none is present
  if not caches[editor] or #(caches[editor].funcs or {}) == 0 then outlineRefresh(editor, true) end
  return caches[editor] and caches[editor].funcs or {}
end

function outline:RefreshSymbols(path, callback)
  if isIgnoredInIndex(path) then return end

  local exts = {}
  for _, ext in pairs(ide:GetKnownExtensions()) do
    local spec = ide:FindSpec(ext)
    if spec and spec.marksymbols then table.insert(exts, ext) end
  end

  local opts = {sort = false, folder = false, skipbinary = true, yield = true,
    -- skip those directories that are on the "ignore" list
    ondirectory = function(name) return outline.settings.ignoredirs[name] == nil end
  }
  local nextfile = coroutine.wrap(function() FileSysGetRecursive(path, true, table.concat(exts, ";"), opts) end)
  while true do
    local file = nextfile()
    if not file then break end
    if not isIgnoredInIndex(file) then (callback or queuePath)(file) end
  end
end

function outline:UpdateSymbols(fname, symb)
  local symbols = self.settings.symbols
  symbols[fname] = symb

  -- purge outdated records
  local threshold = TimeGet() - 60*60*24*7 -- cache for 7 days
  if not self.indexpurged then
    for k, v in pairs(symbols) do
      if v.updated < threshold then symbols[k] = nil end
    end
    self.indexpurged = true
  end

  self.needsaving = true
end

function outline:SaveSettings(force)
  if self.needsaving or force then
    ide:PushStatus(TR("Updating symbol index and settings..."))
    package:SetSettings(self.settings, {keyignore = {depth = true, image = true, poe = true, item = true, skip = true}})
    ide:PopStatus()
    self.needsaving = false
  end
end

MergeSettings(outline.settings, package:GetSettings())
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local frame = ide.frame
local bottomnotebook = frame.bottomnotebook
local out = bottomnotebook.errorlog

local MESSAGE_MARKER = StylesGetMarker("message")
local ERROR_MARKER = StylesGetMarker("error")
local PROMPT_MARKER = StylesGetMarker("prompt")
local PROMPT_MARKER_VALUE = 2^PROMPT_MARKER

out:Show(true)
out:SetFont(ide.font.oNormal)
out:StyleSetFont(wxstc.wxSTC_STYLE_DEFAULT, ide.font.oNormal)
out:SetBufferedDraw(not ide.config.hidpi and true or false)
out:StyleClearAll()
out:SetMarginWidth(1, 16) -- marker margin
out:SetMarginType(1, wxstc.wxSTC_MARGIN_SYMBOL)
out:MarkerDefine(StylesGetMarker("message"))
out:MarkerDefine(StylesGetMarker("error"))
out:MarkerDefine(StylesGetMarker("prompt"))
out:SetReadOnly(true)
if (ide.config.outputshell.usewrap) then
  out:SetWrapMode(wxstc.wxSTC_WRAP_WORD)
  out:SetWrapStartIndent(0)
  out:SetWrapVisualFlags(wxstc.wxSTC_WRAPVISUALFLAG_END)
  out:SetWrapVisualFlagsLocation(wxstc.wxSTC_WRAPVISUALFLAGLOC_END_BY_TEXT)
end

StylesApplyToEditor(ide.config.stylesoutshell,out,ide.font.oNormal,ide.font.oItalic)

function ClearOutput(force)
  if not (force or ide:GetMenuBar():IsChecked(ID_CLEAROUTPUT)) then return end
  out:SetReadOnly(false)
  out:ClearAll()
  out:SetReadOnly(true)
end

function out:Erase() ClearOutput(true) end

local inputBound = 0 -- to track where partial output ends for input editing purposes
local function getInputLine()
  return out:MarkerPrevious(out:GetLineCount()+1, PROMPT_MARKER_VALUE)
end
local function getInputText(bound)
  return out:GetTextRangeDyn(
    out:PositionFromLine(getInputLine())+(bound or 0), out:GetLength())
end
local function updateInputMarker()
  local lastline = out:GetLineCount()-1
  out:MarkerDeleteAll(PROMPT_MARKER)
  out:MarkerAdd(lastline, PROMPT_MARKER)
  inputBound = #getInputText()
end
function OutputEnableInput() updateInputMarker() end

function DisplayOutputNoMarker(...)
  local message = ""
  local cnt = select('#',...)
  for i=1,cnt do
    local v = select(i,...)
    message = message..tostring(v)..(i<cnt and "\t" or "")
  end

  local promptLine = getInputLine()
  local insertedAt = promptLine == wx.wxNOT_FOUND and out:GetLength() or out:PositionFromLine(promptLine) + inputBound
  local current = out:GetReadOnly()
  out:SetReadOnly(false)
  out:InsertTextDyn(insertedAt, out.useraw and message or FixUTF8(message, "\022"))
  out:EmptyUndoBuffer()
  out:SetReadOnly(current)
  out:GotoPos(out:GetLength())
  out:EnsureVisibleEnforcePolicy(out:GetLineCount()-1)
  if promptLine ~= wx.wxNOT_FOUND then updateInputMarker() end
end
function DisplayOutput(...)
  local line = out:GetLineCount()-1
  DisplayOutputNoMarker(...)
  out:MarkerAdd(line, MESSAGE_MARKER)
end
function DisplayOutputLn(...)
  DisplayOutput(...)
  DisplayOutputNoMarker("\n")
end

function out:Print(...) return ide:Print(...) end
function out:Write(...) return DisplayOutputNoMarker(...) end
function out:Error(...)
  local line = out:GetLineCount()-1
  DisplayOutputNoMarker(...)
  DisplayOutputNoMarker("\n")
  out:MarkerAdd(line, ERROR_MARKER)
end

local streamins = {}
local streamerrs = {}
local streamouts = {}
local customprocs = {}
local textout = '' -- this is a buffer for any text sent to external scripts

function DetachChildProcess()
  for pid, custom in pairs(customprocs) do
    -- since processes are detached, their END_PROCESS event is not going
    -- to be called; call endcallback() manually if registered.
    if custom.endcallback then custom.endcallback(pid) end
    if custom.proc then custom.proc:Detach() end
  end
end

function CommandLineRunning(uid)
  for pid, custom in pairs(customprocs) do
    if (custom.uid == uid and custom.proc and custom.proc.Exists(tonumber(pid))) then
      return pid, custom.proc
    end
  end

  return
end

function CommandLineToShell(uid,state)
  for pid, custom in pairs(customprocs) do
    if (pid == uid or custom.uid == uid) and custom.proc and custom.proc.Exists(tonumber(pid)) then
      if (streamins[pid]) then streamins[pid].toshell = state end
      if (streamerrs[pid]) then streamerrs[pid].toshell = state end
      return true
    end
  end
end

-- logic to "unhide" wxwidget window using winapi
pcall(require, 'winapi')
local checkstart, checknext, checkperiod
local pid = nil
local function unHideWindow(pidAssign)
  -- skip if not configured to do anything
  if not ide.config.unhidewindow then return end
  if pidAssign then
    pid = pidAssign > 0 and pidAssign or nil
  end
  if pid and winapi then
    local now = TimeGet()
    if pidAssign and pidAssign > 0 then
      checkstart, checknext, checkperiod = now, now, 0.02
    end
    if now - checkstart > 1 and checkperiod < 0.5 then
      checkperiod = checkperiod * 2
    end
    if now >= checknext then
      checknext = now + checkperiod
    else
      return
    end
    local wins = winapi.find_all_windows(function(w)
      return w:get_process():get_pid() == pid
    end)
    local any = ide.interpreter.unhideanywindow
    local show, hide, ignore = 1, 2, 0
    for _,win in pairs(wins) do
      -- win:get_class_name() can return nil if the window is already gone
      -- between getting the list and this check.
      local action = ide.config.unhidewindow[win:get_class_name()]
        or (any and show or ignore)
      if action == show and not win:is_visible()
      or action == hide and win:is_visible() then
        -- use show_async call (ShowWindowAsync) to avoid blocking the IDE
        -- if the app is busy or is being debugged
        win:show_async(action == show and winapi.SW_SHOW or winapi.SW_HIDE)
        pid = nil -- indicate that unhiding is done
      end
    end
  end
end

local function nameTab(tab, name)
  local index = bottomnotebook:GetPageIndex(tab)
  if index ~= wx.wxNOT_FOUND then bottomnotebook:SetPageText(index, name) end
end

function OutputSetCallbacks(pid, proc, callback, endcallback)
  local streamin = proc and proc:GetInputStream()
  local streamerr = proc and proc:GetErrorStream()
  if streamin then
    streamins[pid] = {stream=streamin, callback=callback,
      proc=proc, check=proc and proc.IsInputAvailable}
  end
  if streamerr then
    streamerrs[pid] = {stream=streamerr, callback=callback,
      proc=proc, check=proc and proc.IsErrorAvailable}
  end
  customprocs[pid] = {proc=proc, endcallback=endcallback}
end

function CommandLineRun(cmd,wdir,tooutput,nohide,stringcallback,uid,endcallback)
  if (not cmd) then return end

  -- expand ~ at the beginning of the command
  if ide.oshome and cmd:find('~') then
    cmd = cmd:gsub([[^(['"]?)~]], '%1'..ide.oshome:gsub('[\\/]$',''), 1)
  end

  -- try to extract the name of the executable from the command
  -- the executable may not have the extension and may be in quotes
  local exename = string.gsub(cmd, "\\", "/")
  local _,_,fullname = string.find(exename,'^[\'"]([^\'"]+)[\'"]')
  exename = fullname and string.match(fullname,'/?([^/]+)$')
    or string.match(exename,'/?([^/]-)%s') or exename

  uid = uid or exename

  if (CommandLineRunning(uid)) then
    DisplayOutputLn(TR("Program can't start because conflicting process is running as '%s'.")
      :format(cmd))
    return
  end

  DisplayOutputLn(TR("Program starting as '%s'."):format(cmd))

  local proc = wx.wxProcess(out)
  if (tooutput) then proc:Redirect() end -- redirect the output if requested

  -- set working directory if specified
  local oldcwd
  if (wdir and #wdir > 0) then -- directory can be empty; ignore in this case
    oldcwd = wx.wxFileName.GetCwd()
    oldcwd = wx.wxFileName.SetCwd(wdir) and oldcwd
  end

  -- launch process
  local params = wx.wxEXEC_ASYNC + wx.wxEXEC_MAKE_GROUP_LEADER + (nohide and wx.wxEXEC_NOHIDE or 0)
  local pid = wx.wxExecute(cmd, params, proc)

  if oldcwd then wx.wxFileName.SetCwd(oldcwd) end

  -- For asynchronous execution, the return value is the process id and
  -- zero value indicates that the command could not be executed.
  -- The return value of -1 in this case indicates that we didn't launch
  -- a new process, but connected to the running one (e.g. DDE under Windows).
  if not pid or pid == -1 or pid == 0 then
    DisplayOutputLn(TR("Program unable to run as '%s'."):format(cmd))
    return
  end

  DisplayOutputLn(TR("Program '%s' started in '%s' (pid: %d).")
    :format(uid, (wdir and wdir or wx.wxFileName.GetCwd()), pid))

  OutputSetCallbacks(pid, proc, stringcallback, endcallback)
  customprocs[pid].uid=uid
  customprocs[pid].started = TimeGet()

  local streamout = proc and proc:GetOutputStream()
  if streamout then streamouts[pid] = {stream=streamout, callback=stringcallback, out=true} end

  unHideWindow(pid)
  nameTab(out, TR("Output (running)"))

  return pid
end

local readonce = 4096
local maxread = readonce * 10 -- maximum number of bytes to read before pausing
local function getStreams()
  local function readStream(tab)
    for _,v in pairs(tab) do
      -- periodically stop reading to get a chance to process other events
      local processed = 0
      while (v.check(v.proc) and processed <= maxread) do
        local str = v.stream:Read(readonce)
        -- the buffer has readonce bytes, so cut it to the actual size
        str = str:sub(1, v.stream:LastRead())
        processed = processed + #str

        local pfn
        if (v.callback) then
          str,pfn = v.callback(str)
        end
        if not str then
          -- skip if nothing to display
        elseif (v.toshell) then
          ide:GetConsole():Print(str)
        else
          DisplayOutputNoMarker(str)
          if str and (getInputLine() ~= wx.wxNOT_FOUND or out:GetReadOnly()) then
            ide:GetOutput():Activate()
            updateInputMarker()
          end
        end
        pfn = pfn and pfn()
      end
    end
  end
  local function sendStream(tab)
    local str = textout
    if not str then return end
    textout = nil
    str = str .. "\n"
    for _,v in pairs(tab) do
      local pfn
      if (v.callback) then
        str,pfn = v.callback(str)
      end
      v.stream:Write(str, #str)
      updateInputMarker()
      pfn = pfn and pfn()
    end
  end

  readStream(streamins)
  readStream(streamerrs)
  sendStream(streamouts)
end

function out:ProcessStreams()
  if (#streamins or #streamerrs) then getStreams() end
end

out:Connect(wx.wxEVT_END_PROCESS, function(event)
    local pid = event:GetPid()
    if (pid ~= -1) then
      getStreams()
      streamins[pid] = nil
      streamerrs[pid] = nil
      streamouts[pid] = nil

      if not customprocs[pid] then return end
      if customprocs[pid].endcallback then
        local ok, err = pcall(customprocs[pid].endcallback, pid, event:GetExitCode())
        if not ok then ide:GetOutput():Error(("Post processing execution failed: %s"):format(err)) end
      end

      -- if this was started with uid (`CommandLineRun`), then it needs additional processing
      if customprocs[pid].uid then
        -- delete markers and set focus to the editor if there is an input marker
        if out:MarkerPrevious(out:GetLineCount(), PROMPT_MARKER_VALUE) > wx.wxNOT_FOUND then
          out:MarkerDeleteAll(PROMPT_MARKER)
          local editor = GetEditor()
          -- check if editor still exists; it may not if the window is closed
          if editor then editor:SetFocus() end
        end
        unHideWindow(0)
        ide:SetLaunchedProcess(nil)
        nameTab(out, TR("Output"))
        DisplayOutputLn(TR("Program completed in %.2f seconds (pid: %d).")
          :format(TimeGet() - customprocs[pid].started, pid))
      end
      customprocs[pid] = nil
    end
  end)

out:Connect(wx.wxEVT_IDLE, function()
    out:ProcessStreams()
    if ide.osname == 'Windows' then unHideWindow() end
  end)

local function activateByPartialName(fname, jumpline, jumplinepos)
  -- fname may include name of executable, as in "path/to/lua: file.lua";
  -- strip it and try to find match again if needed.
  -- try the stripped name first as if it doesn't match, the longer
  -- name may have parts that may be interpreted as a network path and
  -- may take few seconds to check.
  local name
  local fixedname = fname:match(":%s+(.+)")
  if fixedname then
    name = GetFullPathIfExists(FileTreeGetDir(), fixedname)
      or FileTreeFindByPartialName(fixedname)
  end
  name = name
    or GetFullPathIfExists(FileTreeGetDir(), fname)
    or FileTreeFindByPartialName(fname)

  local editor = LoadFile(name or fname,nil,true)
  if not editor then
    local ed = GetEditor()
    if ed and ide:GetDocument(ed):GetFileName() == (name or fname) then
      editor = ed
    end
  end
  if not editor then return false end

  jumpline = tonumber(jumpline)
  jumplinepos = tonumber(jumplinepos)

  editor:GotoPos(editor:PositionFromLine(math.max(0,jumpline-1))
    + (jumplinepos and (math.max(0,jumplinepos-1)) or 0))
  editor:EnsureVisibleEnforcePolicy(jumpline)
  editor:SetFocus()
  return true
end

local jumptopatterns = { -- ["pattern"] = true/false for multiple/single
  -- <filename>(line,linepos):
  ["%s*(.-)%((%d+),(%d+)%)%s*:"] = false,
  -- <filename>(line):
  ["%s*(.-)%((%d+).*%)%s*:"] = false,
  --[string "<filename>"]:line:
  ['.-%[string "([^"]+)"%]:(%d+)%s*:'] = false,
  -- <filename>:line:linepos
  ["%s*(.-):(%d+):(%d+):"] = false,
  -- <filename>:line:
  ["%s*(.-):(%d+)%s*:"] = true,
}

out:Connect(wxstc.wxEVT_STC_DOUBLECLICK,
  function(event)
    local line = out:GetCurrentLine()
    local linetx = out:GetLineDyn(line)

    -- try to detect a filename and line in linetx
    for pattern, multiple in pairs(jumptopatterns) do
      local results = {}
      for fname, jumpline, jumplinepos in linetx:gmatch(pattern) do
        -- insert matches in reverse order (if any)
        table.insert(results, 1, {fname, jumpline, jumplinepos})
        if not multiple then break end -- one match is enough if no multiple is requested
      end
      for _, result in ipairs(results) do
        if activateByPartialName(unpack(result)) then
          -- doubleclick can set selection, so reset it
          local pos = event:GetPosition()
          if pos == wx.wxNOT_FOUND then pos = out:GetLineEndPosition(event:GetLine()) end
          out:SetSelection(pos, pos)
          return
        end
      end
    end
    event:Skip()
  end)

local function positionInLine(line)
  return out:GetCurrentPos() - out:PositionFromLine(line)
end
local function caretOnInputLine(disallowLeftmost)
  local inputLine = getInputLine()
  local boundary = inputBound + (disallowLeftmost and 0 or -1)
  return (out:GetCurrentLine() > inputLine
    or out:GetCurrentLine() == inputLine
   and positionInLine(inputLine) > boundary)
end

out:Connect(wx.wxEVT_KEY_DOWN,
  function (event)
    local key = event:GetKeyCode()
    if out:GetReadOnly() then
      -- no special processing if it's readonly
    elseif key == wx.WXK_UP or key == wx.WXK_NUMPAD_UP then
      if out:GetCurrentLine() <= getInputLine() then return end
    elseif key == wx.WXK_DOWN or key == wx.WXK_NUMPAD_DOWN then
      -- can go down
    elseif key == wx.WXK_LEFT or key == wx.WXK_NUMPAD_LEFT then
      if not caretOnInputLine(true) then return end
    elseif key == wx.WXK_BACK then
      if not caretOnInputLine(true) then return end
    elseif key == wx.WXK_DELETE or key == wx.WXK_NUMPAD_DELETE then
      if not caretOnInputLine()
      or out:LineFromPosition(out:GetSelectionStart()) < getInputLine() then
        return
      end
    elseif key == wx.WXK_PAGEUP or key == wx.WXK_NUMPAD_PAGEUP
        or key == wx.WXK_PAGEDOWN or key == wx.WXK_NUMPAD_PAGEDOWN
        or key == wx.WXK_END or key == wx.WXK_NUMPAD_END
        or key == wx.WXK_HOME or key == wx.WXK_NUMPAD_HOME
        or key == wx.WXK_RIGHT or key == wx.WXK_NUMPAD_RIGHT
        or key == wx.WXK_SHIFT or key == wx.WXK_CONTROL
        or key == wx.WXK_ALT then
      -- fall through
    elseif key == wx.WXK_RETURN or key == wx.WXK_NUMPAD_ENTER then
      if not caretOnInputLine()
      or out:LineFromPosition(out:GetSelectionStart()) < getInputLine() then
        return
      end
      out:GotoPos(out:GetLength()) -- move to the end
      textout = (textout or '') .. getInputText(inputBound)
      -- remove selection if any, otherwise the text gets replaced
      out:SetSelection(out:GetSelectionEnd()+1,out:GetSelectionEnd())
      -- don't need to do anything else with return
    else
      -- move cursor to end if not already there
      if not caretOnInputLine() then
        out:GotoPos(out:GetLength())
      -- check if the selection starts before the input line and reset it
      elseif out:LineFromPosition(out:GetSelectionStart()) < getInputLine(-1) then
        out:GotoPos(out:GetLength())
        out:SetSelection(out:GetSelectionEnd()+1,out:GetSelectionEnd())
      end
    end
    event:Skip()
  end)

local function inputEditable(line)
  local inputLine = getInputLine()
  local currentLine = line or out:GetCurrentLine()
  return inputLine ~= wx.wxNOT_FOUND and
    (currentLine > inputLine or
     currentLine == inputLine and positionInLine(inputLine) >= inputBound) and
    not (out:LineFromPosition(out:GetSelectionStart()) < getInputLine())
end

out:Connect(wxstc.wxEVT_STC_UPDATEUI,
  function () out:SetReadOnly(not inputEditable()) end)

-- only allow copy/move text by dropping to the input line
out:Connect(wxstc.wxEVT_STC_DO_DROP,
  function (event)
    if not inputEditable(out:LineFromPosition(event:GetPosition())) then
      event:SetDragResult(wx.wxDragNone)
    end
  end)

if ide.config.outputshell.nomousezoom then
  -- disable zoom using mouse wheel as it triggers zooming when scrolling
  -- on OSX with kinetic scroll and then pressing CMD.
  out:Connect(wx.wxEVT_MOUSEWHEEL,
    function (event)
      if wx.wxGetKeyState(wx.WXK_CONTROL) then return end
      event:Skip()
    end)
end
-- Copyright 2013-17 Paul Kulchenko, ZeroBrane LLC
---------------------------------------------------------

local ide = ide
local iscaseinsensitive = wx.wxFileName("A"):SameAs(wx.wxFileName("a"))
local unpack = table.unpack or unpack
local q = EscapeMagic

local function eventHandle(handlers, event, ...)
  local success
  for package, handler in pairs(handlers) do
    local ok, res = pcall(handler, package, ...)
    if ok then
      if res == false then success = false end
    else
      ide:GetOutput():Error(TR("%s event failed: %s"):format(event, res))
    end
  end
  return success
end

local function getEventHandlers(packages, event)
  local handlers = {}
  for _, package in pairs(packages) do
    if package[event] then handlers[package] = package[event] end
  end
  return handlers
end

function PackageEventHandle(event, ...)
  return eventHandle(getEventHandlers(ide.packages, event), event, ...)
end

function PackageEventHandleOnce(event, ...)
  -- copy packages as the event that is handled only once needs to be removed
  local handlers = getEventHandlers(ide.packages, event)
  -- remove all handlers as they need to be called only once
  -- this allows them to be re-installed if needed
  for _, package in pairs(ide.packages) do package[event] = nil end
  return eventHandle(handlers, event, ...)
end

local function PackageEventHandleOne(file, event, ...)
  local package = ide.packages[file]
  if package and type(package[event]) == 'function' then
    local ok, res = pcall(package[event], package, ...)
    if ok then
      if res == false then return false end
    else
      ide:GetOutput():Error(TR("%s event failed: %s"):format(event, res))
    end
  end
end

function PackageUnRegister(file, ...)
  PackageEventHandleOne(file, "onUnRegister", ...)
  -- remove from the list of installed packages
  local package = ide.packages[file]
  ide.packages[file] = nil
  return package
end

function PackageRegister(file, ...)
  if not ide.packages[file] then
    local packages = {}
    local package = MergeFullPath(
      GetPathWithSep(ide.editorFilename), "packages/"..file..".lua")
    LoadLuaFileExt(packages, package, ide.proto.Plugin)
    packages[file].fname = file
    ide.packages[file] = packages[file]
  end
  return PackageEventHandleOne(file, "onRegister", ...)
end

function ide:GetProperty(keyword, default)
  return self.app.stringtable[keyword] or default
end
function ide:GetRootPath(path)
  return MergeFullPath(GetPathWithSep(self.editorFilename), path or '')
end
function ide:GetPackagePath(packname)
  return MergeFullPath(
    self.oshome and MergeFullPath(self.oshome, '.'..self:GetAppName()..'/') or self:GetRootPath(),
    MergeFullPath('packages', packname or '')
  )
end
function ide:GetLaunchPath(addparams)
  local path = ide.editorFilename
  if ide.osname == "Macintosh" then
    -- find .app folder in the path; there are two options:
    -- 1. `/Applications/ZeroBraneStudio.app/Contents/ZeroBraneStudio/zbstudio`(installed path)
    -- 2. `...ZeroBraneStudio/zbstudio` (cloned repository path)
    local app = path:match("(.+%.app)/")
    if app then -- check if the application is already in the path
      path = app
    else
      local apps = FileSysGetRecursive(path, true, "Info.plist", {ondirectory = function(dir)
            -- don't recurse for more than necessary
            return dir:find("%.app/Contents/.+") == nil
          end}
      )
      if #apps == 0 then return nil, "Can't find application path." end

      local fn = wx.wxFileName(apps[1])
      fn:RemoveLastDir()
      path = fn:GetPath(wx.wxPATH_GET_VOLUME)
    end
    -- generate command with `-n` (start a new copy of the application)
    path = ([[open -n -a "%s" --args]]):format(path)
  elseif ide.osname == "Unix" then
    path = ([["%s.sh"]]):format(path)
  else
    path = ([["%s"]]):format(path)
  end
  if addparams then
    for n, val in ipairs(ide.arg) do
      if val == "-cfg" and #ide.arg > n then
        path = path .. ([[ %s "%s"]]):format(ide.arg[n], ide.arg[n+1])
      end
    end
  end
  return path
end
function ide:Exit(hotexit)
  if hotexit then self.config.hotexit = true end
  self:GetMainFrame():Close()
end
function ide:GetApp() return self.editorApp end
function ide:GetAppName() return self.appname end
function ide:GetDefaultFileName()
  local default = self.config.default
  local ext = default.extension
  local ed = self:GetEditor()
  if ed and default.usecurrentextension then ext = self:GetDocument(ed):GetFileExt() end
  return default.name..(ext and ext > "" and "."..ext or "")
end
function ide:GetEditor(index) return GetEditor(index) end
function ide:GetEditorWithFocus(...) return GetEditorWithFocus(...) end
function ide:GetEditorWithLastFocus()
  -- make sure ide.infocus is still a valid component and not "some" userdata
  return (self:IsValidCtrl(self.infocus)
    and self.infocus:GetClassInfo():GetClassName() == "wxStyledTextCtrl"
    and self.infocus:DynamicCast("wxStyledTextCtrl") or nil)
end
function ide:GetMenuBar() return self.frame and self.frame.menuBar end
function ide:GetStatusBar() return self.frame and self.frame.statusBar end
function ide:GetToolBar() return self.frame and self.frame.toolBar end
function ide:GetDebugger() return self.debugger end
function ide:SetDebugger(deb)
  self.debugger = deb
  -- if the remote console is already assigned, then assign it based on the new debugger
  local console = ide:GetConsole()
  -- `SetDebugger` may be called before console is set, so need to check if it's available
  if ide:IsValidProperty(console, 'GetRemote') and console:GetRemote() then console:SetRemote(deb:GetConsole()) end
  return deb
end
function ide:GetMainFrame()
  if not self.frame then
    self.frame = wx.wxFrame(wx.NULL, wx.wxID_ANY, self:GetProperty("editor"),
      wx.wxDefaultPosition, wx.wxSize(1100, 700))
  end
  return self.frame
end
function ide:GetUIManager() return self.frame.uimgr end
function ide:GetDocument(ed) return ed and self.openDocuments[ed:GetId()] end
function ide:GetDocuments() return self.openDocuments end
function ide:GetKnownExtensions(ext)
  local knownexts, extmatch = {}, ext and ext:lower()
  for _, spec in pairs(self.specs) do
    for _, ext in ipairs(spec.exts or {}) do
      if not extmatch or extmatch == ext:lower() then
        table.insert(knownexts, ext)
      end
    end
  end
  table.sort(knownexts)
  return knownexts
end

function ide:DoWhenIdle(func) table.insert(self.onidle, func) end

function ide:FindTopMenu(item)
  local index = self:GetMenuBar():FindMenu((TR)(item))
  return self:GetMenuBar():GetMenu(index), index
end
function ide:FindMenuItem(itemid, menu)
  local menubar = self:GetMenuBar()
  if not menubar then return end -- no associated menu
  local item, imenu = menubar:FindItem(itemid, menu)
  if menu and not item then item = menu:FindItem(itemid) end
  if not item then return end
  menu = menu or imenu

  for pos = 0, menu:GetMenuItemCount()-1 do
    if menu:FindItemByPosition(pos):GetId() == itemid then
      return item, menu, pos
    end
  end
  return
end
function ide:AttachMenu(...)
  -- AttachMenu([targetmenu,] id, submenu)
  -- `targetmenu` is only needed for menus not attached to the main menubar
  local menu, id, submenu = ...
  if select('#', ...) == 2 then menu, id, submenu = nil, ... end
  local item, menu, pos = self:FindMenuItem(id, menu)
  if not item then return end

  menu:Remove(item)
  item:SetSubMenu(submenu)
  return menu:Insert(pos, item), pos
end
function ide:CloneMenu(menu)
  if not menu then return end
  local newmenu = wx.wxMenu({})
  local ok, node = pcall(function() return menu:GetMenuItems():GetFirst() end)
  -- some wxwidgets versions may not have GetFirst, so return an empty menu in this case
  if not ok then return newmenu end
  while node do
    local item = node:GetData():DynamicCast("wxMenuItem")
    newmenu:Append(item:GetId(), item:GetItemLabel(), item:GetHelp(), item:GetKind())
    node = node:GetNext()
  end
  return newmenu
end
function ide:MakeMenu(t)
  local menu = wx.wxMenu({})
  local menuicon = self.config.menuicon -- menu items need to have icons
  local iconmap = self.config.toolbar.iconmap
  for p = 1, #(t or {}) do
    if type(t[p]) == "table" then
      if #t[p] == 0 then -- empty table signals a separator
        menu:AppendSeparator()
      else
        local id, label, help, kind = unpack(t[p])
        local submenu
        if type(kind) == "table" then
          submenu, kind = self:MakeMenu(kind)
        elseif type(kind) == "userdata" then
          submenu, kind = kind
        end
        if submenu then
          menu:Append(id, label, submenu, help or "")
        else
          local item = wx.wxMenuItem(menu, id, label, help or "", kind or wx.wxITEM_NORMAL)
          if menuicon and type(iconmap[id]) == "table"
          -- only add icons to "normal" items (OSX can take them on checkbox items too),
          -- otherwise this causes asert on Linux (http://trac.wxwidgets.org/ticket/17123)
          and (ide.osname == "Macintosh" or item:GetKind() == wx.wxITEM_NORMAL) then
            local bitmap = ide:GetBitmap(iconmap[id][1], "TOOLBAR", wx.wxSize(16,16))
            item:SetBitmap(bitmap)
          end
          menu:Append(item)
        end
      end
    end
  end
  return menu
end

function ide:FindDocument(path)
  local fileName = wx.wxFileName(path)
  for _, doc in pairs(self:GetDocuments()) do
    if doc.filePath and fileName:SameAs(wx.wxFileName(doc.filePath)) then
      return doc
    end
  end
  return
end
function ide:FindDocumentsByPartialPath(path)
  local seps = "[\\/]"
  -- add trailing path separator to make sure full directory match
  if not path:find(seps.."$") then path = path .. GetPathSeparator() end
  local pattern = "^"..q(path):gsub(seps, seps)
  local lpattern = pattern:lower()

  local docs = {}
  for _, doc in pairs(self:GetDocuments()) do
    if doc.filePath
    and (doc.filePath:find(pattern)
         or iscaseinsensitive and doc.filePath:lower():find(lpattern)) then
      table.insert(docs, doc)
    end
  end
  return docs
end
function ide:SetInterpreter(name) return ProjectSetInterpreter(name) end
function ide:GetInterpreter(name) return name == nil and self.interpreter or name and self.interpreters[name] or nil end
function ide:GetInterpreters() return self.interpreters end
function ide:GetConfig() return self.config end
function ide:GetOutput() return self.frame.bottomnotebook.errorlog end
function ide:GetConsole() return self.frame.bottomnotebook.shellbox end
function ide:GetEditorNotebook() return self.frame.notebook end
function ide:GetOutputNotebook() return self.frame.bottomnotebook end
function ide:GetOutline() return self.outline end
function ide:GetProjectNotebook() return self.frame.projnotebook end
function ide:GetProject() return FileTreeGetDir and FileTreeGetDir() or nil end
function ide:SetProject(projdir,skiptree)
  -- strip trailing spaces as this may create issues with "path/ " on Windows
  projdir = projdir:gsub("%s+$","")
  local dir = wx.wxFileName.DirName(FixDir(projdir))
  dir:Normalize() -- turn into absolute path if needed
  if not wx.wxDirExists(dir:GetFullPath()) then return ide.filetree:updateProjectDir(projdir) end

  projdir = dir:GetPath(wx.wxPATH_GET_VOLUME) -- no trailing slash

  ide.config.path.projectdir = projdir ~= "" and projdir or nil
  ide:SetStatus(projdir)
  self.frame:SetTitle(ExpandPlaceholders(ide.config.format.apptitle))

  if skiptree then return true end
  return ide.filetree:updateProjectDir(projdir)
end
function ide:GetProjectStartFile()
  local projectdir = FileTreeGetDir()
  local startfile = self.filetree.settings.startfile[projectdir]
  return MergeFullPath(projectdir, startfile), startfile
end
function ide:GetLaunchedProcess() return self.debugger and self.debugger.pid end
function ide:SetLaunchedProcess(pid) if self.debugger then self.debugger.pid = pid; return pid end end
function ide:GetProjectTree() return self.filetree.projtreeCtrl end
function ide:GetOutlineTree() return self.outline.outlineCtrl end
function ide:GetWatch() return self.debugger and self.debugger.watchCtrl end
function ide:GetStack() return self.debugger and self.debugger.stackCtrl end

function ide:GetTextFromUser(message, caption, value)
  local dlg = wx.wxTextEntryDialog(self.frame, message, caption, value)
  local res = dlg:ShowModal()
  return res == wx.wxID_OK and dlg:GetValue() or nil, res
end

local statusreset
function ide:SetStatusFor(text, interval, field)
  field = field or 0
  interval = interval or 2
  local statusbar = self:GetStatusBar()
  if not ide.timers.status then
    ide.timers.status = ide:AddTimer(statusbar, function(event) if statusreset then statusreset() end end)
  end
  statusreset = function()
    if statusbar:GetStatusText(field) == text then statusbar:SetStatusText("", field) end
  end
  ide.timers.status:Start(interval*1000, wx.wxTIMER_ONE_SHOT)
  statusbar:SetStatusText(text, field)
end
function ide:SetStatus(text, field) self:GetStatusBar():SetStatusText(text, field or 0) end
function ide:GetStatus(field) return self:GetStatusBar():GetStatusText(field or 0) end
function ide:PushStatus(text, field) self:GetStatusBar():PushStatusText(text, field or 0) end
function ide:PopStatus(field) self:GetStatusBar():PopStatusText(field or 0) end
function ide:Yield() wx.wxYield() end
function ide:CreateBareEditor() return CreateEditor(true) end
function ide:ShowCommandBar(...) return ShowCommandBar(...) end

function ide:RequestAttention()
  local ide = self
  local frame = ide.frame
  if not frame:IsActive() then
    frame:RequestUserAttention()
    if ide.osname == "Macintosh" then
      local cmd = [[osascript -e 'tell application "%s" to activate']]
      wx.wxExecute(cmd:format(ide.editorApp:GetAppName()), wx.wxEXEC_ASYNC)
    elseif ide.osname == "Unix" then
      if frame:IsIconized() then frame:Iconize(false) end
    elseif ide.osname == "Windows" then
      if frame:IsIconized() then frame:Iconize(false) end
      frame:Raise() -- raise the window

      local winapi = require 'winapi'
      if winapi then
        local pid = winapi.get_current_pid()
        local wins = winapi.find_all_windows(function(w)
          return w:get_process():get_pid() == pid
             and w:get_class_name() == 'wxWindowNR'
        end)
        if wins and #wins > 0 then
          -- found the window, now need to activate it:
          -- send some input to the window and then
          -- bring our window to foreground (doesn't work without some input)
          -- send Attn key twice (down and up)
          winapi.send_to_window(0xF6, false)
          winapi.send_to_window(0xF6, true)
          for _, w in ipairs(wins) do w:set_foreground() end
        end
      end
    end
  end
end

local rawMethods = {"AddTextDyn", "InsertTextDyn", "AppendTextDyn", "SetTextDyn",
  "GetTextDyn", "GetLineDyn", "GetSelectedTextDyn", "GetTextRangeDyn"}
local useraw = nil

local invalidUTF8, invalidLength
local suffix = "\1\0"
local DF_TEXT = wx.wxDataFormat(wx.wxDF_TEXT)

function ide:CreateStyledTextCtrl(...)
  local editor = wxstc.wxStyledTextCtrl(...)
  if not editor then return end

  if useraw == nil then
    useraw = true
    for _, m in ipairs(rawMethods) do
      if not pcall(function() return editor[m:gsub("Dyn", "Raw")] end) then useraw = false; break end
    end
  end

  -- map all `GetTextDyn` to `GetText` or `GetTextRaw` if `*Raw` methods are present
  editor.useraw = useraw
  for _, m in ipairs(rawMethods) do
    -- some `*Raw` methods return `nil` instead of `""` as their "normal" calls do
    -- (for example, `GetLineRaw` and `GetTextRangeRaw` for parameters outside of text)
    local def = m:find("^Get") and "" or nil
    editor[m] = function(...) return editor[m:gsub("Dyn", useraw and "Raw" or "")](...) or def end
  end

  function editor:CopyDyn()
    invalidUTF8 = nil
    if not self.useraw then return self:Copy() end
    -- check if selected fragment is a valid UTF-8 sequence
    local text = self:GetSelectedTextRaw()
    if text == "" or wx.wxString.FromUTF8(text) ~= "" then return self:Copy() end
    local tdo = wx.wxTextDataObject()
    -- append suffix as wxwidgets (3.1+ on Windows) truncate last char for odd-length strings
    local workaround = ide.osname == "Windows" and (#text % 2 > 0) and suffix or ""
    tdo:SetData(DF_TEXT, text..workaround)
    invalidUTF8, invalidLength = text, tdo:GetDataSize()

    local clip = wx.wxClipboard.Get()
    clip:Open()
    clip:SetData(tdo)
    clip:Close()
  end

  function editor:PasteDyn()
    if not self.useraw then return self:Paste() end
    local tdo = wx.wxTextDataObject()
    local clip = wx.wxClipboard.Get()
    clip:Open()
    clip:GetData(tdo)
    clip:Close()
    local ok, text = tdo:GetDataHere(DF_TEXT)
    -- check if the fragment being pasted is a valid UTF-8 sequence
    if ide.osname == "Windows" then text = text and text:gsub(suffix.."+$","") end
    if not ok or wx.wxString.FromUTF8(text) ~= ""
    or not invalidUTF8 or invalidLength ~= tdo:GetDataSize() then return self:Paste() end

    self:AddTextRaw(ide.osname ~= "Windows" and invalidUTF8 or text)
    self:GotoPos(self:GetCurrentPos())
  end

  function editor:GotoPosEnforcePolicy(pos)
    self:GotoPos(pos)
    self:EnsureVisibleEnforcePolicy(self:LineFromPosition(pos))
  end

  function editor:CanFold()
    local foldable = false
    for m = 0, ide.MAXMARGIN do
      if editor:GetMarginWidth(m) > 0
      and editor:GetMarginMask(m) == wxstc.wxSTC_MASK_FOLDERS then
        foldable = true
      end
    end
    return foldable
  end

  -- circle through "fold all" => "hide base lines" => "unfold all"
  function editor:FoldSome(line)
    local foldall = false -- at least on header unfolded => fold all
    local hidebase = false -- at least one base is visible => hide all

    local header = line and bit.band(editor:GetFoldLevel(line),
      wxstc.wxSTC_FOLDLEVELHEADERFLAG) == wxstc.wxSTC_FOLDLEVELHEADERFLAG
    local from = line and (header and line or editor:GetFoldParent(line)) or 0
    local to = line and from > -1 and editor:GetLastChild(from, -1) or editor:GetLineCount()-1

    for ln = from, to do
      local foldRaw = editor:GetFoldLevel(ln)
      local foldLvl = foldRaw % 4096
      local foldHdr = (math.floor(foldRaw / 8192) % 2) == 1

      -- at least one header is expanded
      foldall = foldall or (foldHdr and editor:GetFoldExpanded(ln))

      -- at least one base can be hidden
      hidebase = hidebase or (
        not foldHdr
        and ln > 1 -- first line can't be hidden, so ignore it
        and foldLvl == wxstc.wxSTC_FOLDLEVELBASE
        and bit.band(foldRaw, wxstc.wxSTC_FOLDLEVELWHITEFLAG) == 0
        and editor:GetLineVisible(ln))
    end

    -- shows lines; this doesn't change fold status for folded lines
    if not foldall and not hidebase then editor:ShowLines(from, to) end

    for ln = from, to do
      local foldRaw = editor:GetFoldLevel(ln)
      local foldLvl = foldRaw % 4096
      local foldHdr = (math.floor(foldRaw / 8192) % 2) == 1

      if foldall then
        if foldHdr and editor:GetFoldExpanded(ln) then
          editor:ToggleFold(ln)
        end
      elseif hidebase then
        if not foldHdr and (foldLvl == wxstc.wxSTC_FOLDLEVELBASE) then
          editor:HideLines(ln, ln)
        end
      else -- unfold all
        if foldHdr and not editor:GetFoldExpanded(ln) then
          editor:ToggleFold(ln)
        end
      end
    end
    -- if the entire file is being un/folded, make sure the cursor is on the screen
    -- (although it may be inside a folded fragment)
    if not line then editor:EnsureCaretVisible() end
  end

  local function getMarginWidth(editor)
    local width = 0
    for m = 0, ide.MAXMARGIN do width = width + editor:GetMarginWidth(m) end
    return width
  end

  function editor:ShowPosEnforcePolicy(pos)
    local line = self:LineFromPosition(pos)
    self:EnsureVisibleEnforcePolicy(line)
    -- skip the rest if line wrapping is on
    if editor:GetWrapMode() ~= wxstc.wxSTC_WRAP_NONE then return end
    local xwidth = self:GetClientSize():GetWidth() - getMarginWidth(self)
    local xoffset = self:GetTextExtent(self:GetLineDyn(line):sub(1, pos-self:PositionFromLine(line)+1))
    self:SetXOffset(xoffset > xwidth and xoffset-xwidth or 0)
  end

  -- wxSTC included with wxlua didn't have ScrollRange defined, so substitute if not present
  if not ide:IsValidProperty(editor, "ScrollRange") then
    function editor:ScrollRange() end
  end

  -- ScrollRange moves to the correct position, but doesn't unfold folded region
  function editor:ShowRange(secondary, primary)
    self:ShowPosEnforcePolicy(primary)
    self:ScrollRange(secondary, primary)
  end

  function editor:ClearAny()
    local length = self:GetLength()
    local selections = ide.wxver >= "2.9.5" and self:GetSelections() or 1
    self:Clear() -- remove selected fragments

    -- check if the modification has failed, which may happen
    -- if there is "invisible" text in the selected fragment.
    -- if there is only one selection, then delete manually.
    if length == self:GetLength() and selections == 1 then
      self:SetTargetStart(self:GetSelectionStart())
      self:SetTargetEnd(self:GetSelectionEnd())
      self:ReplaceTarget("")
    end
  end

  function editor:MarkerGetAll(mask, from, to)
    mask = mask or ide.ANYMARKERMASK
    local markers = {}
    local line = editor:MarkerNext(from or 0, mask)
    while line ~= wx.wxNOT_FOUND do
      table.insert(markers, {line, editor:MarkerGet(line)})
      if to and line > to then break end
      line = editor:MarkerNext(line + 1, mask)
    end
    return markers
  end

  function editor:IsLineEmpty(line)
    local text = self:GetLineDyn(line or editor:GetCurrentLine())
    local lc = self.spec and self.spec.linecomment
    return not text:find("%S") or (lc and text:find("^%s*"..q(lc)) ~= nil)
  end

  function editor:Activate(force)
    -- check for `activateoutput` if the current component is the same as `Output`
    if self == ide:GetOutput() and not ide.config.activateoutput and not force then return end

    local nb = self:GetParent()
    -- check that the parent is of the correct type
    if nb:GetClassInfo():GetClassName() ~= "wxAuiNotebook" then return end
    nb = nb:DynamicCast("wxAuiNotebook")

    local uimgr = ide:GetUIManager()
    local pane = uimgr:GetPane(nb)
    if pane:IsOk() and not pane:IsShown() then
      pane:Show(true)
      uimgr:Update()
    end
    -- activate output/errorlog window
    local index = nb:GetPageIndex(self)
    if nb:GetSelection() == index then return false end
    nb:SetSelection(index)
    return true
  end

  editor:Connect(wx.wxEVT_KEY_DOWN,
    function (event)
      local keycode = event:GetKeyCode()
      local mod = event:GetModifiers()
      if (keycode == wx.WXK_DELETE and mod == wx.wxMOD_SHIFT)
      or (keycode == wx.WXK_INSERT and mod == wx.wxMOD_CONTROL)
      or (keycode == wx.WXK_INSERT and mod == wx.wxMOD_SHIFT) then
        local id = keycode == wx.WXK_DELETE and ID.CUT or mod == wx.wxMOD_SHIFT and ID.PASTE or ID.COPY
        ide.frame:AddPendingEvent(wx.wxCommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED, id))
      elseif keycode == wx.WXK_CAPITAL and mod == wx.wxMOD_CONTROL then
        -- ignore Ctrl+CapsLock
      else
        event:Skip()
      end
    end)
  return editor
end

function ide:CreateTreeCtrl(...)
  local ctrl = wx.wxTreeCtrl(...)
  if not ctrl then return end

  -- LeftArrow on Linux doesn't collapse expanded nodes as it does on Windows/OSX; do it manually
  if ide.osname == "Unix" and ide:IsValidProperty(ctrl, "GetFocusedItem") then
    ctrl:Connect(wx.wxEVT_KEY_DOWN, function (event)
        local keycode = event:GetKeyCode()
        local mod = event:GetModifiers()
        local item = ctrl:GetFocusedItem()
        if keycode == wx.WXK_LEFT and mod == wx.wxMOD_NONE and item:IsOk() and ctrl:IsExpanded(item) then
          ctrl:Collapse(item)
        else
          event:Skip()
        end
      end)
  end
  return ctrl
end

function ide:LoadFile(...) return LoadFile(...) end

function ide:CopyToClipboard(text)
  if wx.wxClipboard:Get():Open() then
    wx.wxClipboard:Get():SetData(wx.wxTextDataObject(text))
    wx.wxClipboard:Get():Close()
    return true
  end
  return false
end

function ide:GetSetting(path, setting)
  local settings = self.settings
  local curpath = settings:GetPath()
  settings:SetPath(path)
  local ok, value = settings:Read(setting)
  settings:SetPath(curpath)
  return ok and value or nil
end

function ide:RemoveMenuItem(id, menu)
  local _, menu, pos = self:FindMenuItem(id, menu)
  if menu then
    self:GetMainFrame():Disconnect(id, wx.wxID_ANY, wx.wxEVT_COMMAND_MENU_SELECTED)
    self:GetMainFrame():Disconnect(id, wx.wxID_ANY, wx.wxEVT_UPDATE_UI)
    menu:Disconnect(id, wx.wxID_ANY, wx.wxEVT_COMMAND_MENU_SELECTED)
    menu:Disconnect(id, wx.wxID_ANY, wx.wxEVT_UPDATE_UI)
    menu:Remove(id)

    local positem = menu:FindItemByPosition(pos)
    if (not positem or positem:GetKind() == wx.wxITEM_SEPARATOR)
    and pos > 0 and (menu:FindItemByPosition(pos-1):GetKind() == wx.wxITEM_SEPARATOR) then
      menu:Destroy(menu:FindItemByPosition(pos-1)) -- remove last or double separator
    elseif positem and pos == 0 and positem:GetKind() == wx.wxITEM_SEPARATOR then
      menu:Destroy(menu:FindItemByPosition(pos)) -- remove first separator
    end
    return true
  end
  return false
end

function ide:ExecuteCommand(cmd, wdir, callback, endcallback)
  local proc = wx.wxProcess(self:GetOutput())
  proc:Redirect()

  local cwd
  if (wdir and #wdir > 0) then -- ignore empty directory
    cwd = wx.wxFileName.GetCwd()
    cwd = wx.wxFileName.SetCwd(wdir) and cwd
  end

  local _ = wx.wxLogNull() -- disable error reporting; will report as needed
  local pid = wx.wxExecute(cmd, wx.wxEXEC_ASYNC, proc)
  pid = pid ~= -1 and pid ~= 0 and pid or nil
  if cwd then wx.wxFileName.SetCwd(cwd) end -- restore workdir
  if not pid then return pid, wx.wxSysErrorMsg() end

  OutputSetCallbacks(pid, proc, callback or function() end, endcallback)
  return pid
end

function ide:CreateImageList(group, ...)
  local _ = wx.wxLogNull() -- disable error reporting in popup
  local size = wx.wxSize(16,16)
  local imglist = wx.wxImageList(16,16)
  for i = 1, select('#', ...) do
    local icon, file = self:GetBitmap(select(i, ...), group, size)
    if imglist:Add(icon) == -1 then
      ide:Print(("Failed to add image '%s' to the image list."):format(file or select(i, ...)))
    end
  end
  return imglist
end

local tintdef = 100
local function iconFilter(bitmap, tint)
  if type(tint) == 'function' then return tint(bitmap) end
  if type(tint) ~= 'table' or #tint ~= 3 then return bitmap end

  local tr, tg, tb = tint[1]/255, tint[2]/255, tint[3]/255
  local pi = 0.299*tr + 0.587*tg + 0.114*tb -- pixel intensity
  local perc = (tint[0] or tintdef)/tintdef
  tr, tg, tb = tr*perc, tg*perc, tb*perc

  local img = bitmap:ConvertToImage()
  for x = 0, img:GetWidth()-1 do
    for y = 0, img:GetHeight()-1 do
      if not img:IsTransparent(x, y) then
        local r, g, b = img:GetRed(x, y)/255, img:GetGreen(x, y)/255, img:GetBlue(x, y)/255
        local gs = (r + g + b) / 3
        local weight = 1-4*(gs-0.5)*(gs-0.5)
        r = math.max(0, math.min(255, math.floor(255 * (gs + (tr-pi) * weight))))
        g = math.max(0, math.min(255, math.floor(255 * (gs + (tg-pi) * weight))))
        b = math.max(0, math.min(255, math.floor(255 * (gs + (tb-pi) * weight))))
        img:SetRGB(x, y, r, g, b)
      end
    end
  end
  return wx.wxBitmap(img)
end

function ide:GetTintedColor(color, tint)
  if type(tint) == 'function' then return tint(color) end
  if type(tint) ~= 'table' or #tint ~= 3 then return color end
  if type(color) ~= 'table' then return color end

  local tr, tg, tb = tint[1]/255, tint[2]/255, tint[3]/255
  local pi = 0.299*tr + 0.587*tg + 0.114*tb -- pixel intensity
  local perc = (tint[0] or tintdef)/tintdef
  tr, tg, tb = tr*perc, tg*perc, tb*perc

  local r, g, b = color[1]/255, color[2]/255, color[3]/255
  local gs = (r + g + b) / 3
  local weight = 1-4*(gs-0.5)*(gs-0.5)
  r = math.max(0, math.min(255, math.floor(255 * (gs + (tr-pi) * weight))))
  g = math.max(0, math.min(255, math.floor(255 * (gs + (tg-pi) * weight))))
  b = math.max(0, math.min(255, math.floor(255 * (gs + (tb-pi) * weight))))
  return {r, g, b}
end

local icons = {} -- icon cache to avoid reloading the same icons
function ide:GetBitmap(id, client, size)
  local im = self.config.imagemap
  local width = size:GetWidth()
  local key = width.."/"..id
  local keyclient = key.."-"..client
  local mapped = im[keyclient] or im[id.."-"..client] or im[key] or im[id]
  -- mapped may be a file name/path or wxImage object; take that into account
  if type(im[id.."-"..client]) == 'string' then keyclient = width.."/"..im[id.."-"..client]
  elseif type(im[keyclient]) == 'string' then keyclient = im[keyclient]
  elseif type(im[id]) == 'string' then
    id = im[id]
    key = width.."/"..id
    keyclient = key.."-"..client
  end

  local fileClient = self:GetAppName() .. "/res/" .. keyclient .. ".png"
  local fileKey = self:GetAppName() .. "/res/" .. key .. ".png"
  local isImage = type(mapped) == 'userdata' and mapped:GetClassInfo():GetClassName() == 'wxImage'
  local file
  if mapped and (isImage or wx.wxFileName(mapped):FileExists()) then file = mapped
  elseif wx.wxFileName(fileClient):FileExists() then file = fileClient
  elseif wx.wxFileName(fileKey):FileExists() then file = fileKey
  else return wx.wxArtProvider.GetBitmap(id, client, size) end
  local icon = icons[file] or iconFilter(wx.wxBitmap(file), self.config.imagetint)
  icons[file] = icon
  return icon, file
end

function ide:AddPackage(name, package)
  self.packages[name] = setmetatable(package, self.proto.Plugin)
  self.packages[name].fname = name
  return self.packages[name]
end
function ide:RemovePackage(name) self.packages[name] = nil end
function ide:GetPackage(name) return self.packages[name] end

function ide:AddWatch(watch, value)
  local mgr = self.frame.uimgr
  local pane = mgr:GetPane("watchpanel")
  if (pane:IsOk() and not pane:IsShown()) then
    pane:Show()
    mgr:Update()
  end

  local watchCtrl = self.debugger.watchCtrl
  if not watchCtrl then return end

  local root = watchCtrl:GetRootItem()
  if not root or not root:IsOk() then return end

  local item = watchCtrl:GetFirstChild(root)
  while true do
    if not item:IsOk() then break end
    if watchCtrl:GetItemExpression(item) == watch then
      if value then watchCtrl:SetItemText(item, watch .. ' = ' .. tostring(value)) end
      return item
    end
    item = watchCtrl:GetNextSibling(item)
  end

  item = watchCtrl:AppendItem(root, watch, 1)
  watchCtrl:SetItemExpression(item, watch, value)
  return item
end

function ide:AddInterpreter(name, interpreter)
  self.interpreters[name] = setmetatable(interpreter, self.proto.Interpreter)
  ProjectUpdateInterpreters()
end
function ide:RemoveInterpreter(name)
  self.interpreters[name] = nil
  ProjectUpdateInterpreters()
end

function ide:AddSpec(name, spec)
  self.specs[name] = spec
  UpdateSpecs()
  if spec.apitype then ReloadAPIs(spec.apitype) end
end
function ide:RemoveSpec(name) self.specs[name] = nil end

function ide:FindSpec(ext)
  if not ext then return end
  for _,curspec in pairs(self.specs) do
    for _,curext in ipairs(curspec.exts or {}) do
      if curext == ext then return curspec end
    end
  end
  -- check for extension to spec mapping and create the spec on the fly if present
  local edcfg = self.config.editor
  if type(edcfg.specmap) == "table" and edcfg.specmap[ext] then
    local name = edcfg.specmap[ext]
    -- check if there is already spec with this name, but doesn't have this extension registered
    if self.specs[name] then
      table.insert(self.specs[name].exts or {}, ext)
      return self.specs[name]
    end
    local spec = { exts = {ext}, lexer = "lexlpeg."..name }
    self:AddSpec(name, spec)
    return spec
  end
end

function ide:AddAPI(type, name, api)
  self.apis[type] = self.apis[type] or {}
  self.apis[type][name] = api
  ReloadAPIs(type)
end
function ide:RemoveAPI(type, name) self.apis[type][name] = nil end

function ide:AddConsoleAlias(alias, table) return ShellSetAlias(alias, table) end
function ide:RemoveConsoleAlias(alias) return ShellSetAlias(alias, nil) end

function ide:AddMarker(...) return StylesAddMarker(...) end
function ide:GetMarker(marker) return StylesGetMarker(marker) end
function ide:RemoveMarker(marker) StylesRemoveMarker(marker) end

local styles = {}
function ide:AddStyle(style, num)
  num = num or styles[style]
  if not num then -- new style; find the smallest available number
    local nums = {}
    for _, stylenum in pairs(styles) do nums[stylenum] = true end
    num = wxstc.wxSTC_STYLE_MAX
    while nums[num] and num > wxstc.wxSTC_STYLE_LASTPREDEFINED do num = num - 1 end
    if num <= wxstc.wxSTC_STYLE_LASTPREDEFINED then return end
  end
  styles[style] = num
  return num
end
function ide:GetStyle(style) return styles[style] end
function ide:GetStyles() return styles end
function ide:RemoveStyle(style) styles[style] = nil end

local indicators = {}
function ide:AddIndicator(indic, num)
  num = num or indicators[indic]
  if not num then -- new indicator; find the smallest available number
    local nums = {}
    for _, indicator in pairs(indicators) do
      -- wxstc.wxSTC_INDIC_CONTAINER is the first available style
      if indicator >= wxstc.wxSTC_INDIC_CONTAINER then
        nums[indicator-wxstc.wxSTC_INDIC_CONTAINER+1] = true
      end
    end
    -- can't do `#nums + wxstc.wxSTC_INDIC_CONTAINER` as #nums can be calculated incorrectly
    -- on tables that have gaps before 2^n values (`1,2,nil,4`)
    num = wxstc.wxSTC_INDIC_CONTAINER
    for _ in ipairs(nums) do num = num + 1 end
    if num > wxstc.wxSTC_INDIC_MAX then return end
  end
  indicators[indic] = num
  return num
end
function ide:GetIndicator(indic) return indicators[indic] end
function ide:GetIndicators() return indicators end
function ide:RemoveIndicator(indic) indicators[indic] = nil end

-- this provides a simple stack for saving/restoring current configuration
local configcache = {}
function ide:AddConfig(name, files)
  if not name or configcache[name] then return end -- don't overwrite existing slots
  if type(files) ~= "table" then files = {files} end -- allow to pass one value
  configcache[name] = {
    config = require('mobdebug').dump(self.config, {nocode = true}),
    configmeta = getmetatable(self.config),
    packages = {},
    overrides = {},
  }
  -- build a list of existing packages
  local packages = {}
  for package in pairs(self.packages) do packages[package] = true end
  -- load config file(s)
  for _, file in pairs(files) do LoadLuaConfig(MergeFullPath(name, file)) end
  -- register newly added packages (if any)
  for package in pairs(self.packages) do
    if not packages[package] then -- this is a newly added package
      PackageEventHandleOne(package, "onRegister")
      configcache[name].packages[package] = true
    end
  end
  ReApplySpecAndStyles() -- apply current config to the UI
end
local function setLongKey(tbl, key, value)
  local paths = {}
  for path in key:gmatch("([^%.]+)") do table.insert(paths, path) end
  while #paths > 0 do
    local lastkey = table.remove(paths, 1)
    if #paths > 0 then
      if tbl[lastkey] == nil then tbl[lastkey] = {} end
      tbl = tbl[lastkey]
      if type(tbl) ~= "table" then return end
    else
      tbl[lastkey] = value
    end
  end
end
function ide:RemoveConfig(name)
  if not name or not configcache[name] then return end
  -- unregister cached packages
  for package in pairs(configcache[name].packages) do PackageUnRegister(package) end
  -- load original config
  local ok, res = LoadSafe(configcache[name].config)
  if ok then
    self.config = res
    -- restore overrides
    for key, value in pairs(configcache[name].overrides) do setLongKey(self.config, key, value) end
    if configcache[name].configmeta then setmetatable(self.config, configcache[name].configmeta) end
  else
    ide:Print(("Error while restoring configuration: '%s'."):format(res))
  end
  configcache[name] = nil -- clear the slot after use
  ReApplySpecAndStyles() -- apply current config to the UI
end
function ide:SetConfig(key, value, name)
  setLongKey(self.config, key, value) -- set config["foo.bar"] as config.foo.bar
  if not name or not configcache[name] then return end
  configcache[name].overrides[key] = value
end

local panels = {}
function ide:AddPanel(ctrl, panel, name, conf)
  local width, height = 360, 200
  local notebook = wxaui.wxAuiNotebook(self.frame, wx.wxID_ANY,
    wx.wxDefaultPosition, wx.wxDefaultSize,
    wxaui.wxAUI_NB_DEFAULT_STYLE + wxaui.wxAUI_NB_TAB_EXTERNAL_MOVE
    - wxaui.wxAUI_NB_CLOSE_ON_ACTIVE_TAB + wx.wxNO_BORDER)
  notebook:AddPage(ctrl, name, true)
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_BG_DCLICK,
    function() PaneFloatToggle(notebook) end)
  notebook:Connect(wxaui.wxEVT_COMMAND_AUINOTEBOOK_PAGE_CLOSE,
    function(event) event:Veto() end)

  local mgr = self.frame.uimgr
  mgr:AddPane(notebook, wxaui.wxAuiPaneInfo():
              Name(panel):Float():CaptionVisible(false):PaneBorder(false):
              MinSize(width/2,height/2):
              BestSize(width,height):FloatingSize(width,height):
              PinButton(true):Hide())
  if type(conf) == "function" then conf(mgr:GetPane(panel)) end
  mgr.defaultPerspective = mgr:SavePerspective() -- resave default perspective

  panels[name] = {ctrl, panel, name, conf}
  return mgr:GetPane(panel), notebook
end

function ide:RemovePanel(panel)
  local mgr = self.frame.uimgr
  local pane = mgr:GetPane(panel)
  if pane:IsOk() then
    local win = pane.window
    mgr:DetachPane(win)
    win:Destroy()
    mgr:Update()
  end
end

function ide:IsPanelDocked(panel)
  local layout = self:GetSetting("/view", "uimgrlayout")
  return layout and not layout:find(panel)
end
function ide:AddPanelDocked(notebook, ctrl, panel, name, conf, activate)
  notebook:AddPage(ctrl, name, activate ~= false)
  panels[name] = {ctrl, panel, name, conf}
  return notebook
end
function ide:AddPanelFlex(notebook, ctrl, panel, name, conf)
  local nb
  if self:IsPanelDocked(panel) then
    nb = self:AddPanelDocked(notebook, ctrl, panel, name, conf, false)
  else
    self:AddPanel(ctrl, panel, name, conf)
  end
  return nb
end

function ide:IsValidCtrl(ctrl)
  return ctrl and pcall(function() ctrl:GetId() end)
end

function ide:IsValidProperty(ctrl, prop)
  -- some control may return `nil` values for non-existing properties, so check for that
  return pcall(function() return ctrl[prop] end) and ctrl[prop] ~= nil
end

function ide:IsValidHotKey(ksc)
  return wx.wxAcceleratorEntry():FromString(ksc)
end

function ide:IsWindowShown(win)
  while win do
    if not win:IsShown() then return false end
    win = win:GetParent()
  end
  return true
end

function ide:RestorePanelByLabel(name)
  if not panels[name] then return end
  return self:AddPanel(unpack(panels[name]))
end

local function tool2id(name) return ID("tools.exec."..name) end

function ide:AddTool(name, command, updateui)
  local toolMenu = self:FindTopMenu('&Tools')
  if not toolMenu then
    local helpMenu, helpindex = self:FindTopMenu('&Help')
    if not helpMenu then helpindex = self:GetMenuBar():GetMenuCount() end

    toolMenu = ide:MakeMenu {}
    self:GetMenuBar():Insert(helpindex, toolMenu, "&Tools")
  end
  local id = tool2id(name)
  toolMenu:Append(id, name)
  if command then
    toolMenu:Connect(id, wx.wxEVT_COMMAND_MENU_SELECTED,
      function (event)
        local editor = ide:GetEditor()
        if not editor then return end

        command(ide:GetDocument(editor):GetFilePath(), ide:GetProject())
        return true
      end)
    toolMenu:Connect(id, wx.wxEVT_UPDATE_UI,
      updateui or function(event) event:Enable(ide:GetEditor() ~= nil) end)
  end
  return id, toolMenu
end

function ide:RemoveTool(name)
  self:RemoveMenuItem(tool2id(name))
  local toolMenu, toolindex = self:FindTopMenu('&Tools')
  if toolMenu and toolMenu:GetMenuItemCount() == 0 then self:GetMenuBar():Remove(toolindex) end
end

local lexers = {}
function ide:AddLexer(name, lexer)
  lexers[name] = lexer
end
function ide:RemoveLexer(name)
  lexers[name] = nil
end
function ide:GetLexer(name)
  return lexers[name]
end

local timers = {}
local function evhandler(event)
  local callback = timers[event:GetId()]
  if callback then callback() end
end
function ide:AddTimer(ctrl, callback)
  table.insert(timers, callback or function() end)
  ctrl:Connect(wx.wxEVT_TIMER, evhandler)
  return wx.wxTimer(ctrl, #timers)
end

local function setAcceleratorTable(accelerators)
  local at = {}
  for id, ksc in pairs(accelerators) do
    local ae = wx.wxAcceleratorEntry(); ae:FromString(ksc)
    table.insert(at, wx.wxAcceleratorEntry(ae:GetFlags(), ae:GetKeyCode(), id))
  end
  ide:GetMainFrame():SetAcceleratorTable(#at > 0 and wx.wxAcceleratorTable(at) or wx.wxNullAcceleratorTable)
end
local at = {}
function ide:SetAccelerator(id, ksc) at[id] = ksc; setAcceleratorTable(at) end
function ide:GetAccelerator(id) return at[id] end
function ide:GetAccelerators() return at end

function ide:SetHotKey(id, ksc)
  if not ide:IsValidHotKey(ksc) then
    ide:Print(("Can't set invalid hotkey value: %s."):format(ksc))
    return
  end

  -- this function handles several cases
  -- 1. shortcut is assigned to an ID listed in keymap
  -- 2. shortcut is assigned to an ID used in a menu item
  -- 3. shortcut is assigned to an ID linked to an item (but not present in keymap or menu)
  -- 4. shortcut is assigned to a function (passed instead of ID)
  local keymap = ide.config.keymap

  -- remove any potential conflict with this hotkey
  -- since the hotkey can be written as `Ctrl+A` and `Ctrl-A`, account for both
  -- this doesn't take into account different order in `Ctrl-Shift-F1` and `Shift-Ctrl-F1`.
  local kscpat = "^"..(ksc:gsub("[+-]", "[+-]"):lower()).."$"
  for gid, ksc in pairs(keymap) do
    -- if the same hotkey is used elsewhere (not one of IDs being checked)
    if ksc:lower():find(kscpat) then
      keymap[gid] = ""
      -- try to find a menu item with this ID (if any) to remove the hotkey
      local item = ide:FindMenuItem(gid)
      if item then item:SetText(item:GetText():gsub("\t.+","").."") end
    end
    -- continue with the loop as there may be multiple associations with the same hotkey
  end

  -- if the hotkey is associated with a function, handle it first
  if type(id) == "function" then
    local fakeid = NewID()
    ide:GetMainFrame():Connect(fakeid, wx.wxEVT_COMMAND_MENU_SELECTED, function() id() end)
    ide:SetAccelerator(fakeid, ksc)
    return
  end

  -- if the keymap is already asigned, then reassign it
  -- if not, then it may need an accelerator, which will be set later
  if keymap[id] then keymap[id] = ksc end

  local item = ide:FindMenuItem(id)
  if item then
    -- get the item text and replace the shortcut
    -- since it also needs to keep the accelerator (if any), so can't use `GetLabel`
    item:SetText(item:GetText():gsub("\t.+","")..KSC(nil, ksc))
  end

  -- if there is no keymap or menu item, then use the accelerator
  if not keymap[id] and not item then ide:SetAccelerator(id, ksc) end
end

function ide:IsProjectSubDirectory(dir)
  local projdir = self:GetProject()
  if not projdir then return end
  -- normalize and check if directory when cut is the same as the project directory;
  -- this relies on the project directory ending in a path separator.
  local path = wx.wxFileName(dir:sub(1, #projdir))
  path:Normalize()
  return path:SameAs(wx.wxFileName(projdir))
end

function ide:SetCommandLineParameters(params)
  if not params then return end
  ide:SetConfig("arg.any", #params > 0 and params or nil, ide:GetProject())
  if #params > 0 then ide:GetPackage("core.project"):AddCmdLine(params) end
  local interpreter = ide:GetInterpreter()
  if interpreter then interpreter:UpdateStatus() end
end

function ide:ActivateFile(filename)
  if wx.wxDirExists(filename) then
    ide:SetProject(filename)
    return true
  end

  local name, suffix, value = filename:match('(.+):([lLpP]?)(%d+)$')
  if name and not wx.wxFileExists(filename) then filename = name end

  -- check if non-existing file can be loaded from the project folder;
  -- this is to handle: "project file" used on the command line
  if not wx.wxFileExists(filename) and not wx.wxIsAbsolutePath(filename) then
    filename = GetFullPathIfExists(ide:GetProject(), filename) or filename
  end

  local opened = LoadFile(filename, nil, true)
  if opened and value then
    if suffix:upper() == 'P' then opened:GotoPosDelayed(tonumber(value))
    else opened:GotoPosDelayed(opened:PositionFromLine(value-1))
    end
  end

  if not opened then
    ide:Print(TR("Can't open file '%s': %s"):format(filename, wx.wxSysErrorMsg()))
  end
  return opened
end
-- Copyright 2015 Paul Kulchenko, ZeroBrane LLC
---------------------------------------------------------

local frame = ide:GetMainFrame()
local margin = {top = 10, left = 10, bottom = 10, right = 10}
local function printScaling(dc, printOut)
  local pageSizeMM_x, pageSizeMM_y = printOut:GetPageSizeMM()

  local ppiScr_x, ppiScr_y = printOut:GetPPIScreen()
  local ppiPrn_x, ppiPrn_y = printOut:GetPPIPrinter()

  local ppi_scale_x = ppiPrn_x/ppiScr_x
  local ppi_scale_y = ppiPrn_y/ppiScr_y

  -- get the size of DC in pixels and the number of pixels in the page
  local dcSize_x, dcSize_y = dc:GetSize()
  local pagePixSize_x, pagePixSize_y = printOut:GetPageSizePixels()

  local dc_pagepix_scale_x = dcSize_x/pagePixSize_x
  local dc_pagepix_scale_y = dcSize_y/pagePixSize_y

  local dc_scale_x = ppi_scale_x * dc_pagepix_scale_x
  local dc_scale_y = ppi_scale_y * dc_pagepix_scale_y

  -- calculate the pixels / mm (25.4 mm = 1 inch)
  local ppmm_x = ppiScr_x / 25.4
  local ppmm_y = ppiScr_y / 25.4

  -- adjust the page size for the pixels / mm scaling factor
  local page_x    = math.floor(pageSizeMM_x * ppmm_x)
  local page_y    = math.floor(pageSizeMM_y * ppmm_y)
  local pageRect  = wx.wxRect(0, 0, page_x, page_y)

  -- get margins informations and convert to printer pixels
  local top    = math.floor(margin.top    * ppmm_y)
  local bottom = math.floor(margin.bottom * ppmm_y)
  local left   = math.floor(margin.left   * ppmm_x)
  local right  = math.floor(margin.right  * ppmm_x)

  dc:SetUserScale(dc_scale_x, dc_scale_y)

  local printRect = wx.wxRect(left, top, page_x-(left+right), page_y-(top+bottom))
  return printRect, pageRect
end

local function connectPrintEvents(printer, printOut)
  local editor = ide:GetEditorWithFocus()
  local cfg = ide.config.print
  local pages

  function printOut:OnPrintPage(pageNum)
    local dc = self:GetDC()
    local printRect, pageRect = printScaling(dc, printOut)

    -- print to an area smaller by the height of the header/footer
    dc:SetFont(editor:GetFont())
    local _, headerHeight = dc:GetTextExtent("qH")
    local textRect = wx.wxRect(printRect)
    if cfg.header then
      textRect:SetY(textRect:GetY() + headerHeight*1.5)
      textRect:SetHeight(textRect:GetHeight() - headerHeight*1.5)
    end
    if cfg.footer then
      textRect:SetHeight(textRect:GetHeight() - headerHeight*1.5)
    end

    local selection = printer:GetPrintDialogData():GetSelection()
    local spos = selection and editor:GetSelectionStart() or 1
    local epos = selection and editor:GetSelectionEnd() or editor:GetLength()
    if pageNum == nil then
      pages = {}
      ide:PushStatus("")
      printOut.startTime = wx.wxNow()
      local pos = spos
      while pos < epos do
        table.insert(pages, pos)
        pos = editor:FormatRange(false, pos, epos, dc, dc, textRect, pageRect)
        ide:PopStatus()
        ide:PushStatus(TR("%s%% formatted..."):format(math.floor((pos-spos)*100.0/(epos-spos))))
      end
      if #pages == 0 then pages = {0} end
      ide:PopStatus()
    else
      ide:SetStatusFor(TR("Formatting page %d..."):format(pageNum))
      editor:FormatRange(true, pages[pageNum], epos, dc, dc, textRect, pageRect)

      local c = wx.wxColour(127, 127, 127)
      dc:SetPen(wx.wxPen(c, 1, wx.wxSOLID))
      dc:SetTextForeground(c)

      local doc = ide:GetDocument(editor)
      local format = "([^\t]*)\t?([^\t]*)\t?([^\t]*)"
      local placeholders = {
        D = printOut.startTime,
        p = pageNum,
        P = #pages,
        S = doc and doc:GetFileName() or "",
      }
      dc:SetFont(editor:GetFont())
      if cfg.header then
        local left, center, right = ExpandPlaceholders(cfg.header, placeholders):match(format)
        dc:DrawText(left, printRect.X, printRect.Y)
        dc:DrawText(center, printRect.Left + (printRect.Left + printRect.Width - dc:GetTextExtentSize(center).Width)/2, printRect.Y)
        dc:DrawText(right, printRect.Left + printRect.Width - dc:GetTextExtentSize(right).Width,  printRect.Y)
        dc:DrawLine(printRect.X, printRect.Y + headerHeight, printRect.Left + printRect.Width, printRect.Y + headerHeight)
      end
      if cfg.footer then
        local footerY = printRect.Y + printRect.Height - headerHeight
        local left, center, right = ExpandPlaceholders(cfg.footer, placeholders):match(format)
        dc:DrawText(left, printRect.X, footerY)
        dc:DrawText(center, printRect.Left + (printRect.Left + printRect.Width - dc:GetTextExtentSize(center).Width)/2, footerY)
        dc:DrawText(right, printRect.Left + printRect.Width - dc:GetTextExtentSize(right).Width,  footerY)
        dc:DrawLine(printRect.X, footerY, printRect.Left + printRect.Width, footerY)
      end
    end
    return true
  end
  function printOut:HasPage(pageNum) return pages[pageNum] ~= nil end
  function printOut:GetPageInfo()
    -- on Linux `GetPageInfo` is called before the canvas is initialized, which prevents
    -- proper calculation of the number of pages (wx2.9.5).
    -- Return defaults here as it's going to be called once more in the right place.
    if ide.osname == "Unix" and not pages then return 1, 9999, 1, 9999 end
    local printDD = printer:GetPrintDialogData()
    -- due to wxwidgets bug (http://trac.wxwidgets.org/ticket/17200), if `to` page is not set explicitly,
    -- only one page is being printed when `selection` option is selected in the print dialog.
    if printDD:GetSelection() then printDD:SetToPage(#pages) end -- set the page as a workaround
    local tofrom = not printDD:GetSelection() and not printDD:GetAllPages()
    return 1, #pages, tofrom and printDD:GetFromPage() or 1, tofrom and printDD:GetToPage() or #pages
  end
  function printOut:OnPreparePrinting() self:OnPrintPage() end
end

frame:Connect(ID_PAGESETUP, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local pageSetupDD = wx.wxPageSetupDialogData()
    pageSetupDD.MarginTopLeft     = wx.wxPoint(margin.left, margin.top)
    pageSetupDD.MarginBottomRight = wx.wxPoint(margin.right, margin.bottom)
    pageSetupDD:EnableOrientation(false)
    pageSetupDD:EnablePaper(false)

    local pageSetupDialog = wx.wxPageSetupDialog(frame, pageSetupDD)
    pageSetupDialog:ShowModal()
    pageSetupDD = pageSetupDialog:GetPageSetupDialogData()
    margin.top, margin.left = pageSetupDD.MarginTopLeft.y, pageSetupDD.MarginTopLeft.x
    margin.bottom, margin.right = pageSetupDD.MarginBottomRight.y, pageSetupDD.MarginBottomRight.x
  end)

frame:Connect(ID_PRINT, wx.wxEVT_COMMAND_MENU_SELECTED,
  function (event)
    local cfg = ide.config.print
    local editor = ide:GetEditorWithFocus()
    editor:SetPrintMagnification(cfg.magnification)
    editor:SetPrintColourMode(cfg.colourmode)
    editor:SetPrintWrapMode(cfg.wrapmode)

    -- only enable selection if there is something selected in the editor (ignore multiple selections)
    local printDD = wx.wxPrintDialogData()
    printDD:EnableSelection(editor:GetSelectionStart() ~= editor:GetSelectionEnd())

    local printer  = wx.wxPrinter(printDD)
    local luaPrintout = wx.wxLuaPrintout()
    connectPrintEvents(printer, luaPrintout)

    -- save and hide indicators
    local indics = {}
    for _, num in pairs(ide:GetIndicators()) do
      indics[num] = editor:IndicatorGetStyle(num)
      editor:IndicatorSetStyle(num, wxstc.wxSTC_INDIC_HIDDEN)
    end
    -- bold keywords
    local keywords = {}
    for _, num in ipairs(ide:IsValidProperty(editor, 'spec') and editor.spec.lexerstyleconvert and editor.spec.lexerstyleconvert.keywords0 or {}) do
      keywords[num] = editor:StyleGetBold(num)
      editor:StyleSetBold(num, true)
    end
    local ok = printer:Print(frame, luaPrintout, true)
    -- restore indicators
    for n, style in pairs(indics) do editor:IndicatorSetStyle(n, style) end
    for n, style in pairs(keywords) do editor:StyleSetBold(n, style) end
    if not ok and printer:GetLastError() == wx.wxPRINTER_ERROR then
      ReportError("There was a problem while printing.\nCheck if your current printer is set correctly.")
    end
  end)

frame:Connect(ID_PRINT, wx.wxEVT_UPDATE_UI, function(event) event:Enable(ide:GetEditorWithFocus() ~= nil) end)

local _, menu, epos = ide:FindMenuItem(ID.EXIT)
-- disable printing on Unix/Linux as it generates incorrect layout (wx2.9.5, wx3.1)
if ide.osname ~= "Unix" and menu and epos then
  -- insert Print-repated menu items (going in the opposite order)
  menu:Insert(epos-1, ID_PAGESETUP, TR("Page Setup..."), "")
  menu:Insert(epos-1, ID_PRINT, TR("&Print..."), TR("Print the current document"))
  menu:InsertSeparator(epos-1)
end
-- Copyright 2013-17 Paul Kulchenko, ZeroBrane LLC
---------------------------------------------------------

local q = EscapeMagic
local modpref = ide.MODPREF

ide.proto.Document = {__index = {
  GetFileName = function(self) return self.fileName end,
  GetFilePath = function(self) return self.filePath end,
  GetFileExt = function(self) return GetFileExt(self.fileName) end,
  GetModTime = function(self) return self.modTime end,
  GetEditor = function(self) return self.editor end,
  GetTabIndex = function(self) return self.index end,
  IsModified = function(self) return self.isModified end,
  IsNew = function(self) return self.filePath == nil end,
  SetFilePath = function(self, path) self.filePath = path end,
  SetModTime = function(self, modtime) self.modTime = modtime end,
  SetModified = function(self, modified)
    self.isModified = modified
    self:SetTabText()
  end,
  SetTabText = function(self, text)
    ide:GetEditorNotebook():SetPageText(self.index,
      (self.isModified and modpref or '')..(text or self:GetTabText()))
  end,
  GetTabText = function(self)
    if self.index == nil then return self.fileName end
    return ide:GetEditorNotebook():GetPageText(self.index):gsub("^"..q(modpref), "")
  end,
  SetActive = function(self) SetEditorSelection(self.index) end,
  Save = function(self) return SaveFile(self.editor, self.filePath) end,
  Close = function(self) return ClosePage(self.index) end,
  CloseAll = function(self) return CloseAllPagesExcept(-1) end,
  CloseAllExcept = function(self) return CloseAllPagesExcept(self.index) end,
}}

ide.proto.Plugin = {__index = {
  GetName = function(self) return self.name end,
  GetFileName = function(self) return self.fname end,
  GetFilePath = function(self) return MergeFullPath(GetPathWithSep(ide.editorFilename), self.fpath) end,
  GetConfig = function(self) return ide.config[self.fname] or {} end,
  GetSettings = function(self) return SettingsRestorePackage(self.fname) end,
  SetSettings = function(self, settings, opts) SettingsSavePackage(self.fname, settings, opts) end,
}}

ide.proto.Interpreter = {__index = {
  GetName = function(self) return self.name end,
  GetFileName = function(self) return self.fname end,
  GetExePath = function(self, ...) return self:fexepath(...) end,
  GetAPI = function(self) return self.api end,
  GetCommandLineArg = function(self, name)
    return ide.config.arg and (ide.config.arg.any or ide.config.arg[name or self.fname])
  end,
  UpdateStatus = function(self)
    local cla = self.takeparameters and self:GetCommandLineArg()
    ide:SetStatus(self.name..(cla and #cla > 0 and ": "..cla or ""), 4)
  end,
  fprojdir = function(self,wfilename)
    return wfilename:GetPath(wx.wxPATH_GET_VOLUME)
  end,
  fworkdir = function(self,wfilename)
    local proj = ide:GetProject()
    return proj and proj:gsub("[\\/]$","") or wfilename:GetPath(wx.wxPATH_GET_VOLUME)
  end,
  fattachdebug = function(self) ide:GetDebugger():SetOptions() end,
}}

ide.proto.Debugger = {__index = {
  IsRunning = function(self) return self.running end,
  IsConnected = function(self) return self.server end,
  IsListening = function(self) return self.listening end,
  GetHostName = function(self) return self.hostname end,
  GetPortNumber = function(self) return self.portnumber end,
  GetConsole = function(self)
    local debugger = self
    return function(...) return debugger:shell(...) end
  end,
  GetDataOptions = function(self, options)
    local cfg = ide.config.debugger
    local params = {
      comment = false, nocode = true, numformat = cfg.numformat,
      maxlevel = cfg.maxdatalevel, maxnum = cfg.maxdatanum, maxlength = cfg.maxdatalength,
    }
    for k, v in pairs(options or {}) do params[k] = v end
    return params
  end,
}}

ide.proto.ID = {
  __index = function(_, id) return _G['ID_'..id] end,
  __call = function(_, id) return IDgen(id) end,
}
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local layoutlabel = {
  UIMANAGER = "uimgrlayout",
  NOTEBOOK = "nblayout",
  NOTEBOOKOUTPUT = "nbbtmlayout",
  NOTEBOOKPROJECT = "nbprojlayout",
  DOCKNOTEBOOK = "nbdocklayout",
  DOCKNOTEBOOKOUTPUT = "nbbtmdocklayout",
  DOCKNOTEBOOKPROJECT = "nbprojdocklayout",
  STATUSBAR = "statusbar",
}

-- ----------------------------------------------------------------------------
-- Initialize the wxConfig for loading/saving the preferences

local ini = ide.config.ini
-- if ini path is relative and includes a directory name, make it relative to the IDE location
ini = ini and (not wx.wxIsAbsolutePath(ini) and wx.wxFileName(ini):GetDirCount() > 0
  and MergeFullPath(GetPathWithSep(ide.editorFilename), ini) or ini)
-- check that the ini file doesn't point to a directory
if ini and (wx.wxFileName(ini):IsDir() or wx.wxIsAbsolutePath(ini) and wx.wxDirExists(ini)) then
  ide:Print(("Can't use 'ini' configuration setting '%s' that points to a directory instead of a file; ignored.")
    :format(ini))
  ini = nil
end
-- check that the directory is writable
if ini and wx.wxIsAbsolutePath(ini) and not wx.wxFileName(ini):IsDirWritable() then
  ide:Print(("Can't use 'ini' configuration setting '%s' that points to a non-writable directory; ignored.")
    :format(ini))
  ini = nil
end

local settings = wx.wxFileConfig(
  ide:GetProperty("settingsapp"), ide:GetProperty("settingsvendor"), ini or "")
ide.settings = settings

local function settingsReadSafe(settings,what,default)
  local cr,out = settings:Read(what,default)
  return cr and out or default
end

-- ----------------------------------------------------------------------------
-- wxConfig load/save preferences functions

function SettingsRestoreFramePosition(window, windowName)
  local path = settings:GetPath()
  settings:SetPath("/"..windowName)

  local s = tonumber(select(2,settings:Read("s", -1)))
  local x = tonumber(select(2,settings:Read("x", 0)))
  local y = tonumber(select(2,settings:Read("y", 0)))
  local w = tonumber(select(2,settings:Read("w", 1100)))
  local h = tonumber(select(2,settings:Read("h", 700)))

  if (s ~= -1) then
    local clientX, clientY, clientWidth, clientHeight = wx.wxClientDisplayRect()

    -- if left-top corner outside of the left-top side, reset it to the screen side
    if x < clientX then x = clientX end
    if y < clientY then y = clientY end

    -- if the window is too wide for the screen, reset it to the screen size
    if w > clientWidth then w = clientWidth end
    if h > clientHeight then h = clientHeight end

    -- if the right-bottom corner is still outside and there is only one display,
    -- then reposition left-top corner, keeping the window centered
    if wx.wxDisplay():GetCount() == 1 then
      local outx = (x + w) - (clientX + clientWidth)
      local outy = (y + h) - (clientY + clientHeight)
      if outx > 0 then x = math.floor(0.5+(x - outx)/2) end
      if outy > 0 then y = math.floor(0.5+(y - outy)/2) end
    end

    window:SetSize(x, y, w, h)
  end

  -- maximize after setting window position to make sure it's maximized on the correct monitor
  if s == 1 then window:Maximize(true) end

  settings:SetPath(path)
end

function SettingsSaveFramePosition(window, windowName)
  local path = settings:GetPath()
  settings:SetPath("/"..windowName)

  local s = 0
  local w, h = window:GetSizeWH()
  local x, y = window:GetPositionXY()

  if window:IsMaximized() then
    s = 1
  elseif window:IsIconized() then
    s = 2
  end

  settings:Write("s", s==2 and 0 or s) -- iconized maybe - but that shouldnt be saved
  settings:Write("x", x)
  settings:Write("y", y)
  settings:Write("w", w)
  settings:Write("h", h)

  settings:SetPath(path)
end

---
-- (table) SettingsRestoreFileHistory (function)
-- restores a list of recently loaded documents from the settings table
-- a table is returned which contains tables each with a filename key, pointing to
-- the filename
function SettingsRestoreFileHistory(fntab)
  local path = settings:GetPath()
  local listname = "/filehistory"
  settings:SetPath(listname)

  local outtab = {}
  local inlist = {}
  for id=1,ide.config.filehistorylength do
    local couldread, name = settings:Read(tostring(id), "")
    if not couldread or name == "" then break end
    if not inlist[name] then
      inlist[name] = true
      table.insert(outtab,{filename = name})
    end
  end

  if fntab then fntab(outtab) end

  settings:SetPath(path)

  return outtab
end

function SettingsSaveFileHistory (filehistory)
  local listname = "/filehistory"
  local path = settings:GetPath()
  settings:DeleteGroup(listname)
  settings:SetPath(listname)

  for i,doc in ipairs(filehistory) do
    settings:Write(tostring(i), doc.filename)
  end

  settings:SetPath(path)
end

---
-- () SettingsRestoreFileSession (function [, string section])
-- restores a list of opened files from the file settings
-- calls the given function with the restored table, a list
-- of tables containing tables like
-- {filename = "filename", cursorpos = <cursor position>}
function SettingsRestoreFileSession(fntab, section)
  local listname = section or "/session"
  local path = settings:GetPath()
  settings:SetPath(listname)
  local outtab = {}
  local params = {}
  local ismore, key, index = settings:GetFirstEntry("", 0)
  while (ismore) do
    local couldread, value = settings:Read(key, "")
    if tonumber(key) then
      local fname,cursorpos = value:match("^(.+);(.-)$")
      if (couldread and value ~= "") then
        outtab[tonumber(key)] =
          {filename = fname or value, cursorpos = tonumber(cursorpos) or 0}
      end
    else
      params[key] = tonumber(value) or value
    end
    ismore, key, index = settings:GetNextEntry(index)
  end

  if fntab then fntab(outtab, params) end

  settings:SetPath(path)

  return outtab
end

---
-- () SettingsSaveFileSession (table opendocs, table params [, string section])
-- saves the list of currently opened documents (passed in the opendocs table)
-- in the settings.
function SettingsSaveFileSession(opendocs, params, section)
  local listname = section or "/session"
  local path = settings:GetPath()
  settings:DeleteGroup(listname)
  settings:SetPath(listname)

  for i,doc in ipairs(opendocs) do
    settings:Write(tostring(i), doc.filename..";"..doc.cursorpos)
  end

  -- save all other parameters
  for k,v in pairs(params) do settings:Write(k, v) end

  settings:SetPath(path)
end

---
-- () SettingsRestoreProjectSession (function)
function SettingsRestoreProjectSession(fntab)
  local listname = "/projectsession"
  local path = settings:GetPath()
  settings:SetPath(listname)
  local outtab = {}
  local couldread = true
  local id = 1
  local name
  while (couldread) do
    couldread, name = settings:Read(tostring(id), "")
    couldread = couldread and name ~= ""
    if (couldread) then
      if (wx.wxDirExists(name)) then
        table.insert(outtab,name)

        local function projsession(...) ProjectConfig(name, {...}) end
        SettingsRestoreFileSession(projsession, listname .. "/" .. tostring(id))
      end
      id = id + 1
    end
  end

  if fntab then fntab(outtab) end

  settings:SetPath(path)

  return outtab
end

---
-- () SettingsSaveProjectSession (table projdirs)
-- saves the list of currently active projects
-- in the settings.
function SettingsSaveProjectSession(projdirs)
  local listname = "/projectsession"
  local path = settings:GetPath()
  settings:DeleteGroup(listname)
  settings:SetPath(listname)

  for i,dir in ipairs(projdirs) do
    settings:Write(tostring(i), dir)

    local opendocs, params = ProjectConfig(dir)
    if opendocs then
      SettingsSaveFileSession(opendocs, params, listname .. "/" .. tostring(i))
    end
  end

  settings:SetPath(path)
end

function SettingsRestorePackage(package)
  local packagename = "/package/"..package
  local path = settings:GetPath()
  settings:SetPath(packagename)
  local outtab = {}
  local ismore, key, index = settings:GetFirstEntry("", 0)
  while (ismore) do
    local couldread, value = settings:Read(key, "")
    if couldread then
      local ok, res = LoadSafe("return "..value)
      if ok then outtab[key] = res
      else
        outtab[key] = nil
        ide:Print(("Couldn't load and ignored '%s' settings for package '%s': %s")
          :format(key, package, res))
      end
    end
    ismore, key, index = settings:GetNextEntry(index)
  end
  settings:SetPath(path)
  return outtab
end

function SettingsSavePackage(package, values, opts)
  local packagename = "/package/"..package
  local path = settings:GetPath()

  settings:DeleteGroup(packagename)
  settings:SetPath(packagename)
  for k,v in pairs(values or {}) do settings:Write(k, DumpPlain(v, opts)) end
  settings:SetPath(path)
end

-----------------------------------

local function saveNotebook(nb)
  local cnt = nb:GetPageCount()
  
  local function addTo(tab,key,value)
    local out = tab[key] or {}
    table.insert(out,value)
    tab[key] = out
  end
  
  local pagesX = {}
  local pagesY = {}
  
  local str = "nblayout|"
  
  for i=1,cnt do
    local id = nb:GetPageText(i-1)
    local pg = nb:GetPage(i-1)
    local x,y = pg:GetPosition():GetXY()
    addTo(pagesX,x,id)
    addTo(pagesY,y,id)
  end
  
  local function sortedPages(tab)
    local t = {}
    for i in pairs(tab) do
      table.insert(t,i)
    end
    table.sort(t)
    return t
  end
  
  local sortedX = sortedPages(pagesX)
  local sortedY = sortedPages(pagesY)
  
  -- for now only support "1D" splits and prefer
  -- dimension which has more, anything else
  -- requires a more complex algorithm, yet to do
  
  local pagesUse
  local sortedUse
  local split
  
  if ( #sortedX >= #sortedY) then
    pagesUse  = pagesX
    sortedUse = sortedX
    split = "<X>"
  else
    pagesUse  = pagesY
    sortedUse = sortedY
    split = "<Y>"
  end
  
  for _, v in ipairs(sortedUse) do
    local pages = pagesUse[v]
    for _, id in ipairs(pages) do
      str = str..id.."|"
    end
    str = str..split.."|"
  end
  
  return str
end

local function loadNotebook(nb,str,fnIdConvert)
  str = str:match("nblayout|(.+)")
  if (not str) then return end
  local cnt = nb:GetPageCount()
  local sel = nb:GetSelection()

  -- store old pages
  local currentpages, order = {}, {}
  for i=1,cnt do
    local id = nb:GetPageText(i-1)
    local newid = fnIdConvert and fnIdConvert(id) or id
    currentpages[newid] = currentpages[newid] or {}
    table.insert(currentpages[newid], {page = nb:GetPage(i-1), text = id, index = i-1})
    order[i] = newid
  end

  -- remove them
  for i=cnt,1,-1 do nb:RemovePage(i-1) end

  -- read them and perform splits
  local t = 0
  local newsel
  local function finishPage(page)
    if (page.index == sel) then
      newsel = t
    end
    t = t + 1
  end

  local direction
  local splits = { X = wx.wxRIGHT, Y = wx.wxBOTTOM }
  for cmd in str:gmatch("([^|]+)") do
    local instr = cmd:match("<(%w)>")
    if (not instr) then
      local id = fnIdConvert and fnIdConvert(cmd) or cmd
      local pageind = next(currentpages[id] or {})
      if (pageind) then
        local page = currentpages[id][pageind]
        currentpages[id][pageind] = nil

        nb:AddPage(page.page, page.text)
        if (direction) then nb:Split(t, direction) end
        finishPage(page)
      end
    end
    direction = instr and splits[instr]
  end
  
  -- add anything we forgot; make sure page groups are in the order specified
  for i=1,cnt do
    local pagelist = currentpages[order[i]]
    for _,page in pairs(pagelist) do
      nb:AddPage(page.page, page.text)
      finishPage(page)
    end
  end
  
  -- set the active page as it was before
  if (newsel) then nb:SetSelection(newsel) end
end

function SettingsRestoreView()
  local listname = "/view"
  local path = settings:GetPath()
  settings:SetPath(listname)

  local frame = ide.frame
  local uimgr = frame.uimgr
  
  local layoutcur = uimgr:SavePerspective()
  local layout = settingsReadSafe(settings,layoutlabel.UIMANAGER,"")
  if (layout ~= layoutcur) then
    -- save the current toolbar besth and re-apply after perspective is loaded
    -- bestw and besth has two separate issues:
    -- (1) layout includes bestw that is only as wide as the toolbar size,
    -- this leaves default background on the right side of the toolbar;
    -- fix it by explicitly replacing with the screen width.
    -- (2) besth may be wrong after icon size changes.
    local toolbar = frame.uimgr:GetPane("toolbar")
    local besth = toolbar:IsOk() and tonumber(uimgr:SavePaneInfo(toolbar):match("besth=([^;]+)"))

    -- reload the perspective if the saved one is not empty as it's different from the default
    if #layout > 0 then uimgr:LoadPerspective(layout, false) end

    local screenw = frame:GetClientSize():GetWidth()
    if toolbar:IsOk() and screenw > 0 then toolbar:BestSize(screenw, besth or -1) end

    -- check if debugging panes are not mentioned and float them
    for _, name in pairs({"stackpanel", "watchpanel"}) do
      local pane = frame.uimgr:GetPane(name)
      if pane:IsOk() and not layout:find(name) then pane:Float() end
    end

    -- check if the toolbar is not mentioned in the layout and show it
    for _, name in pairs({"toolbar"}) do
      local pane = frame.uimgr:GetPane(name)
      if pane:IsOk() and not layout:find(name) then pane:Show() end
    end

    -- remove captions from all panes
    local panes = frame.uimgr:GetAllPanes()
    for index = 0, panes:GetCount()-1 do
      uimgr:GetPane(panes:Item(index).name):CaptionVisible(false)
    end
  end

  frame:GetStatusBar():Show(settingsReadSafe(settings,layoutlabel.STATUSBAR,true))

  uimgr:Update()
  
  layoutcur = saveNotebook(ide:GetOutputNotebook())
  layout = settingsReadSafe(settings,layoutlabel.NOTEBOOKOUTPUT,layoutcur)
  if (layout ~= layoutcur) then
    loadNotebook(ide:GetOutputNotebook(),layout,
      -- treat "Output (running)" same as "Output"
      function(name) return
        name:match(TR("Output")) or name:match("Output") or name end)
  end

  layoutcur = saveNotebook(ide:GetProjectNotebook())
  layout = settingsReadSafe(settings,layoutlabel.NOTEBOOKPROJECT,layoutcur)
  if (layout ~= layoutcur) then
    loadNotebook(ide:GetProjectNotebook(),layout)
  end

  -- always select Output tab
  local bottomnotebook = ide:GetOutputNotebook()
  local index = bottomnotebook:GetPageIndex(bottomnotebook.errorlog)
  if index >= 0 then bottomnotebook:SetSelection(index) end

  layoutcur = saveNotebook(frame.notebook)
  layout = settingsReadSafe(settings,layoutlabel.NOTEBOOK,layoutcur)
  if (layout ~= layoutcur) then
    loadNotebook(ide.frame.notebook,layout)
    local openDocuments = ide.openDocuments
    local nb = frame.notebook
    local cnt = nb:GetPageCount()
    for i=0,cnt-1 do
      openDocuments[nb:GetPage(i):GetId()].index = i
    end
  end

  -- restore configuration for notebook pages that have been split;
  -- load saved dock_size values and update current values with saved ones
  -- where dock_size configuration matches
  for l, m in pairs({
    [layoutlabel.DOCKNOTEBOOK] = ide:GetEditorNotebook():GetAuiManager(),
    [layoutlabel.DOCKNOTEBOOKOUTPUT] = ide:GetOutputNotebook():GetAuiManager(),
    [layoutlabel.DOCKNOTEBOOKPROJECT] = ide:GetProjectNotebook():GetAuiManager(),
  }) do
    -- ...|dock_size(5,0,0)=20|dock_size(2,1,0)=200|...
    local prevlayout = settingsReadSafe(settings, l, "")
    local curlayout = m:SavePerspective()
    local newlayout = curlayout:gsub('(dock_size[^=]+=)(%d+)', function(t,v)
        local val = prevlayout:match(EscapeMagic(t)..'(%d+)')
        return t..(val or v)
      end)
    if newlayout ~= curlayout then m:LoadPerspective(newlayout) end
  end

  local editor = GetEditor()
  if editor then editor:SetFocus() end

  settings:SetPath(path)
end

function SettingsSaveView()
  local listname = "/view"
  local path = settings:GetPath()
  settings:DeleteGroup(listname)
  settings:SetPath(listname)

  local frame = ide.frame
  local uimgr = frame.uimgr
  
  settings:Write(layoutlabel.UIMANAGER, uimgr:SavePerspective())
  settings:Write(layoutlabel.NOTEBOOK, saveNotebook(ide:GetEditorNotebook()))
  settings:Write(layoutlabel.NOTEBOOKOUTPUT, saveNotebook(ide:GetOutputNotebook()))
  settings:Write(layoutlabel.NOTEBOOKPROJECT, saveNotebook(ide:GetProjectNotebook()))
  settings:Write(layoutlabel.DOCKNOTEBOOK, ide:GetEditorNotebook():GetAuiManager():SavePerspective())
  settings:Write(layoutlabel.DOCKNOTEBOOKOUTPUT, ide:GetOutputNotebook():GetAuiManager():SavePerspective())
  settings:Write(layoutlabel.DOCKNOTEBOOKPROJECT, ide:GetProjectNotebook():GetAuiManager():SavePerspective())
  settings:Write(layoutlabel.STATUSBAR, frame:GetStatusBar():IsShown())

  settings:SetPath(path)
end

function SettingsRestoreEditorSettings()
  local listname = "/editor"
  local path = settings:GetPath()
  settings:SetPath(listname)

  local interpreter = settingsReadSafe(settings, "interpreter",
    ide.config.interpreter or ide.config.default.interpreter)
  ProjectSetInterpreter(interpreter)

  settings:SetPath(path)
end

function SettingsSaveEditorSettings()
  local listname = "/editor"
  local path = settings:GetPath()
  settings:DeleteGroup(listname)
  settings:SetPath(listname)

  settings:Write("interpreter", ide.interpreter and ide.interpreter.fname or ide.config.default.interpreter)

  settings:SetPath(path)
end

function SettingsSaveAll()
  SettingsSaveFileSession(GetOpenFiles())
  SettingsSaveEditorSettings()
  SettingsSaveProjectSession(FileTreeGetProjects())
  SettingsSaveFileHistory(GetFileHistory())
  SettingsSaveView()
  SettingsSaveFramePosition(ide.frame, "MainFrame")
end
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
local unpack = table.unpack or unpack

local bottomnotebook = ide.frame.bottomnotebook
local console = bottomnotebook.shellbox
local remotesend

local PROMPT_MARKER = StylesGetMarker("prompt")
local PROMPT_MARKER_VALUE = 2^PROMPT_MARKER
local ERROR_MARKER = StylesGetMarker("error")
local OUTPUT_MARKER = StylesGetMarker("output")
local MESSAGE_MARKER = StylesGetMarker("message")

console:SetFont(ide.font.oNormal)
console:StyleSetFont(wxstc.wxSTC_STYLE_DEFAULT, ide.font.oNormal)
console:SetBufferedDraw(not ide.config.hidpi and true or false)
console:StyleClearAll()

console:SetTabWidth(ide.config.editor.tabwidth or 2)
console:SetIndent(ide.config.editor.tabwidth or 2)
console:SetUseTabs(ide.config.editor.usetabs and true or false)
console:SetViewWhiteSpace(ide.config.editor.whitespace and true or false)
console:SetIndentationGuides(true)

console:SetWrapMode(wxstc.wxSTC_WRAP_WORD)
console:SetWrapStartIndent(0)
console:SetWrapVisualFlagsLocation(wxstc.wxSTC_WRAPVISUALFLAGLOC_END_BY_TEXT)
console:SetWrapVisualFlags(wxstc.wxSTC_WRAPVISUALFLAG_END)

console:MarkerDefine(StylesGetMarker("prompt"))
console:MarkerDefine(StylesGetMarker("error"))
console:MarkerDefine(StylesGetMarker("output"))
console:MarkerDefine(StylesGetMarker("message"))
console:SetReadOnly(false)

SetupKeywords(console,"lua",nil,ide.config.stylesoutshell,ide.font.oNormal,ide.font.oItalic)

local function getPromptLine()
  local totalLines = console:GetLineCount()
  return console:MarkerPrevious(totalLines+1, PROMPT_MARKER_VALUE)
end

local function getPromptText()
  local prompt = getPromptLine()
  return console:GetTextRangeDyn(console:PositionFromLine(prompt), console:GetLength())
end

local function setPromptText(text)
  local length = console:GetLength()
  console:SetSelectionStart(length - string.len(getPromptText()))
  console:SetSelectionEnd(length)
  console:ClearAny()
  console:AddTextDyn(text)
  -- refresh the output window to force recalculation of wrapped lines;
  -- otherwise a wrapped part of the last line may not be visible.
  console:Update(); console:Refresh()
  console:GotoPos(console:GetLength())
end

local function positionInLine(line)
  return console:GetCurrentPos() - console:PositionFromLine(line)
end

local function caretOnPromptLine(disallowLeftmost, line)
  local promptLine = getPromptLine()
  local currentLine = line or console:GetCurrentLine()
  local boundary = disallowLeftmost and 0 or -1
  return (currentLine > promptLine
    or currentLine == promptLine and positionInLine(promptLine) > boundary)
end

local function chomp(line) return (line:gsub("%s+$", "")) end

local function getInput(line)
  local nextMarker = line
  local count = console:GetLineCount()

  repeat -- check until we find at least some marker
    nextMarker = nextMarker+1
  until console:MarkerGet(nextMarker) > 0 or nextMarker > count-1
  return chomp(console:GetTextRangeDyn(
    console:PositionFromLine(line), console:PositionFromLine(nextMarker)))
end

function ConsoleSelectCommand(point)
  local cpos = console:ScreenToClient(point or wx.wxGetMousePosition())
  local position = console:PositionFromPoint(cpos)
  if position == wxstc.wxSTC_INVALID_POSITION then return end

  local promptline = console:MarkerPrevious(console:LineFromPosition(position), PROMPT_MARKER_VALUE)
  if promptline == wxstc.wxSTC_INVALID_POSITION then return end
  local nextline = console:MarkerNext(promptline+1, ide.ANYMARKERMASK)
  local epos = nextline ~= wxstc.wxSTC_INVALID_POSITION and console:PositionFromLine(nextline) or console:GetLength()
  console:SetSelection(console:PositionFromLine(promptline), epos)
  return true
end

local currentHistory
local lastCommand = ""
local function getNextHistoryLine(forward, promptText)
  local count = console:GetLineCount()
  if currentHistory == nil then currentHistory = count end

  if forward then
    currentHistory = console:MarkerNext(currentHistory+1, PROMPT_MARKER_VALUE)
    if currentHistory == wx.wxNOT_FOUND then
      currentHistory = count
      return ""
    end
  else
    currentHistory = console:MarkerPrevious(currentHistory-1, PROMPT_MARKER_VALUE)
    if currentHistory == wx.wxNOT_FOUND then
      return lastCommand
    end
  end
  -- need to skip the current prompt line
  -- or skip repeated commands
  if currentHistory == getPromptLine()
  or getInput(currentHistory) == promptText then
    return getNextHistoryLine(forward, promptText)
  end
  return getInput(currentHistory)
end

local function getNextHistoryMatch(promptText)
  local count = console:GetLineCount()
  if currentHistory == nil then currentHistory = count end

  local current = currentHistory
  while true do
    currentHistory = console:MarkerPrevious(currentHistory-1, PROMPT_MARKER_VALUE)
    if currentHistory == wx.wxNOT_FOUND then -- restart search from the last item
      currentHistory = count
    elseif currentHistory ~= getPromptLine() then -- skip current prompt
      local input = getInput(currentHistory)
      if input:find(promptText, 1, true) == 1 then return input end
    end
    -- couldn't find anything and made a loop; get out
    if currentHistory == current then return end
  end

  assert(false, "getNextHistoryMatch coudn't find a proper match")
end

local function concat(sep, ...)
  local text = ""
  for i=1, select('#',...) do
    text = text .. (i > 1 and sep or "") .. tostring(select(i,...))
  end

  -- split the text into smaller chunks as one large line
  -- is difficult to handle for the editor
  local prev, maxlength = 0, ide.config.debugger.maxdatalength
  if #text > maxlength and not text:find("\n.") then
    text = text:gsub("()(%s+)", function(p, s)
        if p-prev >= maxlength then
          prev = p
          return "\n"
        else
          return s
        end
      end)
  end
  return text
end

local partial = false
local function shellPrint(marker, text, newline)
  if not text or text == "" then return end -- return if nothing to print
  if newline then text = text:gsub("\n+$", "").."\n" end
  local isPrompt = marker and (getPromptLine() ~= wx.wxNOT_FOUND)
  local lines = console:GetLineCount()
  local promptLine = isPrompt and getPromptLine() or nil
  local insertLineAt = isPrompt and not partial and getPromptLine() or console:GetLineCount()-1
  local insertAt = isPrompt and not partial and console:PositionFromLine(getPromptLine()) or console:GetLength()
  console:InsertTextDyn(insertAt, console.useraw and text or FixUTF8(text, function (s) return '\\'..string.byte(s) end))
  local linesAdded = console:GetLineCount() - lines

  partial = text:find("\n$") == nil

  if marker then
    if promptLine then console:MarkerDelete(promptLine, PROMPT_MARKER) end
    for line = insertLineAt, insertLineAt + linesAdded - 1 do
      console:MarkerAdd(line, marker)
    end
    if promptLine then console:MarkerAdd(promptLine+linesAdded, PROMPT_MARKER) end
  end

  console:EmptyUndoBuffer() -- don't allow the user to undo shell text
  console:GotoPos(console:GetLength())
  console:EnsureVisibleEnforcePolicy(console:GetLineCount()-1)
end

displayShellDirect = function (...) shellPrint(nil, concat("\t", ...), true) end
DisplayShell = function (...) shellPrint(OUTPUT_MARKER, concat("\t", ...), true) end
DisplayShellErr = function (...) shellPrint(ERROR_MARKER, concat("\t", ...), true) end
DisplayShellMsg = function (...) shellPrint(MESSAGE_MARKER, concat("\t", ...), true) end
  -- don't print anything; just mark the line with a prompt mark
DisplayShellPrompt = function (...) console:MarkerAdd(console:GetLineCount()-1, PROMPT_MARKER) end

function console:Print(...) return DisplayShell(...) end
function console:Write(...) return shellPrint(OUTPUT_MARKER, concat("", ...), false) end
function console:Error(...) return DisplayShellErr(...) end

local function filterTraceError(err, addedret)
  local err = err:match("(.-:%d+:.-)\n[^\n]*\n[^\n]*\n[^\n]*src/editor/shellbox.lua:.*in function 'executeShellCode'")
              or err
        err = err:gsub("stack traceback:.-\n[^\n]+\n?","")
        if addedret then err = err:gsub('^%[string "return ', '[string "') end
        err = err:match("(.*)\n[^\n]*%(tail call%): %?$") or err
  return err
end

local function createenv()
  local env = {}
  setmetatable(env,{__index = _G})

  local function luafilename(level)
    level = level and level + 1 or 2
    local src
    while (true) do
      src = debug.getinfo(level)
      if (src == nil) then return nil,level end
      if (string.byte(src.source) == string.byte("@")) then
        return string.sub(src.source,2),level
      end
      level = level + 1
    end
  end

  local function luafilepath(level)
    local src,level = luafilename(level)
    if (src == nil) then return src,level end
    src = string.gsub(src,"[\\/][^\\//]*$","")
    return src,level
  end

  local function relativeFilename(file)
    assert(type(file)=='string',"String as filename expected")
    local name = file
    local level = 3
    while (name) do
      if (wx.wxFileName(name):FileExists()) then return name end
      name,level = luafilepath(level)
      if (name == nil) then break end
      name = name .. "/" .. file
    end

    return file
  end

  local function relativeFilepath(file)
    local name = luafilepath(3)
    return (file and name) and name.."/"..file or file or name
  end

  local _loadfile = loadfile
  local function loadfile(file)
    assert(type(file)=='string',"String as filename expected")
    local name = relativeFilename(file)

    return _loadfile(name)
  end

  local function dofile(file, ...)
    assert(type(file) == 'string',"String as filename expected")
    local fn,err = loadfile(file)
    local args = {...}
    if not fn then
      DisplayShellErr(err)
    else
      setfenv(fn,env)
      return fn(unpack(args))
    end
  end

  local os = {
    exit = function()
      ide.frame:AddPendingEvent(wx.wxCommandEvent(wx.wxEVT_COMMAND_MENU_SELECTED, ID_EXIT))
    end,
  }
  env.os = setmetatable(os, {__index = _G.os})
  env.io = setmetatable({write = function(...) console:Write(...) end}, {__index = _G.io})
  env.print = function(...) console:Print(...) end
  env.dofile = dofile
  env.loadfile = loadfile
  env.RELFILE = relativeFilename
  env.RELPATH = relativeFilepath

  return env
end

local env = createenv()

function ShellSetAlias(alias, table)
  local value = env[alias]
  env[alias] = table
  return value
end

local function packResults(status, ...) return status, {...} end

local function executeShellCode(tx)
  if tx == nil or tx == '' then return end

  local forcelocalprefix = '^!'
  local forcelocal = tx:find(forcelocalprefix)
  tx = tx:gsub(forcelocalprefix, '')

  DisplayShellPrompt('')

  -- try to compile as statement
  local _, err = loadstring(tx)
  local isstatement = not err

  if remotesend and not forcelocal then remotesend(tx, isstatement); return end

  local addedret, forceexpression = true, tx:match("^%s*=%s*")
  tx = tx:gsub("^%s*=%s*","")
  local fn
  fn, err = loadstring("return "..tx)
  if not forceexpression and err then
    fn, err = loadstring(tx)
    addedret = false
  end
  
  if fn == nil and err then
    DisplayShellErr(filterTraceError(err, addedret))
  elseif fn then
    setfenv(fn,env)

    -- set the project dir as the current dir to allow "require" calls
    -- to work from shell
    local projectDir, cwd = FileTreeGetDir(), nil
    if projectDir and #projectDir > 0 then
      cwd = wx.wxFileName.GetCwd()
      wx.wxFileName.SetCwd(projectDir)
    end

    local ok, res = packResults(xpcall(fn,
      function(err)
        DisplayShellErr(filterTraceError(debug.traceback(err), addedret))
      end))

    -- restore the current dir
    if cwd then wx.wxFileName.SetCwd(cwd) end
    
    if ok and (addedret or #res > 0) then
      if addedret then
        local mobdebug = require "mobdebug"
        for i,v in pairs(res) do -- stringify each of the returned values
          res[i] = (forceexpression and i > 1 and '\n' or '') ..
            mobdebug.line(v, {nocode = true, comment = 1,
              -- if '=' is used, then use multi-line serialized output
              indent = forceexpression and '  ' or nil})
        end
        -- add nil only if we are forced (using =) or if this is not a statement
        -- this is needed to print 'nil' when asked for 'foo',
        -- and don't print it when asked for 'print(1)'
        if #res == 0 and (forceexpression or not isstatement) then
          res = {'nil'}
        end
      end
      DisplayShell(unpack(res))
    end
  end
end

function console:GetRemote() return remotesend end
function console:SetRemote(client)
  remotesend = client

  local index = bottomnotebook:GetPageIndex(console)
  if index then
    bottomnotebook:SetPageText(index,
      client and TR("Remote console") or TR("Local console"))
  end
end

function ShellExecuteFile(wfilename)
  if (not wfilename) then return end
  local cmd = 'dofile([['..wfilename:GetFullPath()..']])'
  ShellExecuteCode(cmd)
end

ShellExecuteInline = executeShellCode
function ShellExecuteCode(code)
  local index = bottomnotebook:GetPageIndex(bottomnotebook.shellbox)
  if ide.config.activateoutput and bottomnotebook:GetSelection() ~= index then
    bottomnotebook:SetSelection(index)
  end

  displayShellDirect(code)
  executeShellCode(code)
end

local function displayShellIntro()
  DisplayShellMsg(TR("Welcome to the interactive Lua interpreter.").." "
    ..TR("Enter Lua code and press Enter to run it.").."\n"
    ..TR("Use Shift-Enter for multiline code.").."  "
    ..TR("Use 'clear' to clear the shell output and the history.").."\n"
    ..TR("Prepend '=' to show complex values on multiple lines.").." "
    ..TR("Prepend '!' to force local execution."))
  DisplayShellPrompt('')
end

console:Connect(wx.wxEVT_KEY_DOWN,
  function (event)
    -- this loop is only needed to allow to get to the end of function easily
    -- "return" aborts the processing and ignores the key
    -- "break" aborts the processing and processes the key normally
    while true do
      local key = event:GetKeyCode()
      if key == wx.WXK_UP or key == wx.WXK_NUMPAD_UP then
        -- if we are below the prompt line, then allow to go up
        -- through multiline entry
        if console:GetCurrentLine() > getPromptLine() then break end

        -- if we are not on the caret line, move normally
        if not caretOnPromptLine() then break end

        local promptText = getPromptText()
        setPromptText(getNextHistoryLine(false, promptText))
        return
      elseif key == wx.WXK_DOWN or key == wx.WXK_NUMPAD_DOWN then
        -- if we are above the last line, then allow to go down
        -- through multiline entry
        local totalLines = console:GetLineCount()-1
        if console:GetCurrentLine() < totalLines then break end

        -- if we are not on the caret line, move normally
        if not caretOnPromptLine() then break end

        local promptText = getPromptText()
        setPromptText(getNextHistoryLine(true, promptText))
        return
      elseif key == wx.WXK_TAB then
        -- if we are above the prompt line, then don't move
        local promptline = getPromptLine()
        if console:GetCurrentLine() < promptline then return end

        local promptText = getPromptText()
        -- save the position in the prompt text to restore
        local pos = console:GetCurrentPos()
        local text = promptText:sub(1, positionInLine(promptline))
        if #text == 0 then return end

        -- find the next match and set the prompt text
        local match = getNextHistoryMatch(text)
        if match then
          setPromptText(match)
          -- restore the position to make it easier to find the next match
          console:GotoPos(pos)
        end
        return
      elseif key == wx.WXK_ESCAPE then
        setPromptText("")
        return
      elseif key == wx.WXK_BACK then
        if not caretOnPromptLine(true) then return end
      elseif key == wx.WXK_DELETE or key == wx.WXK_NUMPAD_DELETE then
        if not caretOnPromptLine()
        or console:LineFromPosition(console:GetSelectionStart()) < getPromptLine() then
          return
        end
      elseif key == wx.WXK_PAGEUP or key == wx.WXK_NUMPAD_PAGEUP
          or key == wx.WXK_PAGEDOWN or key == wx.WXK_NUMPAD_PAGEDOWN
          or key == wx.WXK_END or key == wx.WXK_NUMPAD_END
          or key == wx.WXK_HOME or key == wx.WXK_NUMPAD_HOME
          or key == wx.WXK_LEFT or key == wx.WXK_NUMPAD_LEFT
          or key == wx.WXK_RIGHT or key == wx.WXK_NUMPAD_RIGHT
          or key == wx.WXK_SHIFT or key == wx.WXK_CONTROL
          or key == wx.WXK_ALT then
        break
      elseif key == wx.WXK_RETURN or key == wx.WXK_NUMPAD_ENTER then
        if not caretOnPromptLine()
        or console:LineFromPosition(console:GetSelectionStart()) < getPromptLine() then
          return
        end

        -- allow multiline entry for shift+enter
        if caretOnPromptLine(true) and event:ShiftDown() then break end

        local promptText = getPromptText()
        if #promptText == 0 then return end -- nothing to execute, exit
        if promptText == 'clear' then
          console:Erase()
        else
          displayShellDirect('\n')
          executeShellCode(promptText)
        end
        currentHistory = getPromptLine() -- reset history
        return -- don't need to do anything else with return
      elseif event:GetModifiers() == wx.wxMOD_NONE or console:GetSelectedText() == "" then
        -- move cursor to end if not already there
        if not caretOnPromptLine() then
          console:GotoPos(console:GetLength())
          console:SetReadOnly(false) -- allow the current character to appear at the new location
        -- check if the selection starts before the prompt line and reset it
        elseif console:LineFromPosition(console:GetSelectionStart()) < getPromptLine() then
          console:GotoPos(console:GetLength())
          console:SetSelection(console:GetSelectionEnd()+1,console:GetSelectionEnd())
        end
      end
      break
    end
    event:Skip()
  end)

local function inputEditable(line)
  return caretOnPromptLine(false, line) and
    not (console:LineFromPosition(console:GetSelectionStart()) < getPromptLine())
end

-- Scintilla 3.2.1+ changed the way markers move when the text is updated
-- ticket: http://sourceforge.net/p/scintilla/bugs/939/
-- discussion: https://groups.google.com/forum/?hl=en&fromgroups#!topic/scintilla-interest/4giFiKG4VXo
if ide.wxver >= "2.9.5" then
  -- this is a workaround that stores a position of the last prompt marker
  -- before insert and restores the same position after as the marker
  -- could have moved if the text is added at the beginning of the line.
  local promptAt
  console:Connect(wxstc.wxEVT_STC_MODIFIED,
    function (event)
      local evtype = event:GetModificationType()
      if bit.band(evtype, wxstc.wxSTC_MOD_BEFOREINSERT) ~= 0 then
        local promptLine = getPromptLine()
        if promptLine and event:GetPosition() == console:PositionFromLine(promptLine)
        then promptAt = promptLine end
      end
      if bit.band(evtype, wxstc.wxSTC_MOD_INSERTTEXT) ~= 0 then
        local promptLine = getPromptLine()
        if promptLine and promptAt then
          console:MarkerDelete(promptLine, PROMPT_MARKER)
          console:MarkerAdd(promptAt, PROMPT_MARKER)
          promptAt = nil
        end
      end
    end)
end

console:Connect(wxstc.wxEVT_STC_UPDATEUI,
  function (event) console:SetReadOnly(not inputEditable()) end)

-- only allow copy/move text by dropping to the input line
console:Connect(wxstc.wxEVT_STC_DO_DROP,
  function (event)
    if not inputEditable(console:LineFromPosition(event:GetPosition())) then
      event:SetDragResult(wx.wxDragNone)
    end
  end)

if ide.config.outputshell.nomousezoom then
  -- disable zoom using mouse wheel as it triggers zooming when scrolling
  -- on OSX with kinetic scroll and then pressing CMD.
  console:Connect(wx.wxEVT_MOUSEWHEEL,
    function (event)
      if wx.wxGetKeyState(wx.WXK_CONTROL) then return end
      event:Skip()
    end)
end

displayShellIntro()

function console:Erase()
  -- save the last command to keep when the history is cleared
  currentHistory = getPromptLine()
  lastCommand = getNextHistoryLine(false, "")
  -- allow writing as the editor may be read-only depending on current cursor position
  self:SetReadOnly(false)
  self:ClearAll()
  displayShellIntro()
end
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------

local ide = ide
--[[ single instance
open an UDP port - if it fails it is either because
- IDE is running already
- an application is already blocking that port
if it fails it tries to contact the running application
- if it confirms being the IDE we let that instance open it, finish our application
- otherwise we throw an error message on the user and start like normal

probably a pitfal: an instance is running but is not visible
  (because it was finished though the UDP thing still runs)
]]

local socket = require "socket"
local svr = socket.udp()

function ide:Restart(hotexit)
  self:AddPackage("core.restart", {
      onAppShutdown = function() wx.wxExecute(ide:GetLaunchPath(true), wx.wxEXEC_ASYNC) end
    })
  if self.timers.idle then self.timers.idle:Stop() end
  if svr then svr:close() end
  self:Exit(hotexit)
end

if not ide.config.singleinstance then return end

local port = ide.config.singleinstanceport
local delay = tonumber(ide.config.singleinstance) or 1000 -- in ms
local success = svr:setsockname("127.0.0.1",port) -- bind on local host
local protocol = {client = {}, server = {}}

protocol.client.greeting = "Is this you, my master? It's me, a new instance."
protocol.server.greeting = "Yes it is me, running as: %s"
protocol.client.requestloading = "Could you please load this file for me: %s"
protocol.client.show = "Show yourself, my master."
protocol.server.answerok = "Sure. You may now leave."

if success then -- ok, server was started, we are solo
  svr:settimeout(0) -- don't block
  ide.timers.idle = ide:AddTimer(wx.wxGetApp(), function()
      local msg, ip, port = svr:receivefrom()
      if msg then
        if msg == protocol.client.greeting then
          svr:sendto(protocol.server.greeting:format(wx.wxGetUserName()),ip,port)
        elseif msg == protocol.client.show then
          svr:sendto(protocol.server.answerok,ip,port)
          ide:RequestAttention()
        elseif msg:match(protocol.client.requestloading:gsub("%%s",".+$")) then -- ok we need to open something
          svr:sendto(protocol.server.answerok,ip,port)
          local filename = msg:match(protocol.client.requestloading:gsub("%%s","(.+)$"))
          if filename then
            ide:RequestAttention()
            ide:ActivateFile(filename)
          end
        end
      end
    end)
  ide.timers.idle:Start(delay,false)
else -- something different is running on our port
  local cln = socket.udp()
  cln:setpeername("127.0.0.1",port)
  cln:settimeout(2)
  cln:send(protocol.client.greeting)

  local msg = cln:receive()
  local arg = ide.arg
  if msg and msg:match(protocol.server.greeting:gsub("%%s",".+$")) then
    local username = msg:match(protocol.server.greeting:gsub("%%s","(.+)$"))
    if username ~= wx.wxGetUserName() then
      ide:Print(("Another instance is running under user '%s' and can't be activated. This instance will continue running, which may cause interference with the debugger."):format(username))
    else
      local failed = false
      for index = 2, #arg do
        local fileName = arg[index]
        if fileName ~= "--"
        -- on OSX, the command line includes -psn parameter, so ignore it
        and (ide.osname ~= 'Macintosh' or not fileName:find("^-psn")) then
          cln:send(protocol.client.requestloading:format(fileName))

          local msg, err = cln:receive()
          if msg ~= protocol.server.answerok then
            failed = true
            ide:Print(err,msg)
          end
        end
      end
      if #arg == 1 then -- no files are being loaded; just active the IDE
        cln:send(protocol.client.show)
        if cln:receive() ~= protocol.server.answerok then failed = true end
      end
      if failed then
        ide:Print("Communication with the running instance failed, this instance will continue running.")
      else -- done
        os.exit(0)
      end
    end
  else
    ide:Print("The single instance communication has failed; there may be another instance running, which may cause interference with the debugger.")
  end
end
-- Copyright 2011-16 Paul Kulchenko, ZeroBrane LLC
-- authors: Luxinia Dev (Eike Decker & Christoph Kubisch)
---------------------------------------------------------
----------
-- Style
--
-- common style attributes
-- ---------------------------
-- fg foreground - {r,g,b} 0-255
-- bg background - {r,g,b} 0-255
-- alpha translucency - 0-255 (0 - transparent, 255 - opaque, 256 - opaque/faster)
-- sel color of the selected block - {r,g,b} 0-255 (only applies to folds)
-- u underline - boolean
-- b bold - boolean
-- i italic - boolean
-- fill fill to end - boolean
-- fn font Face Name - string ("Lucida Console")
-- fs font size - number (11)
-- hs turn hotspot on - true or {r,g,b} 0-255
-- v visibility for symbols of the current style - boolean

local unpack = table.unpack or unpack

function StylesGetDefault()
  return {
    -- lexer specific (inherit fg/bg from text)
    lexerdef = {fg = {160, 160, 160}},
    comment = {fg = {128, 128, 128}},
    stringtxt = {fg = {128, 32, 16}},
    stringeol = {fg = {128, 32, 16}, bg = {224, 192, 224}, fill = true},
    preprocessor = {fg = {128, 128, 0}},
    operator = {fg = {64, 64, 64}},
    number = {fg = {80, 112, 255}},

    keywords0 = {fg = {32, 32, 192}},
    keywords1 = {fg = {127, 32, 96}},
    keywords2 = {fg = {32, 127, 96}},
    keywords3 = {fg = {64, 32, 96}},
    keywords4 = {fg = {127, 0, 95}},
    keywords5 = {fg = {35, 95, 175}},
    keywords6 = {fg = {0, 127, 127}},
    keywords7 = {fg = {240, 255, 255}},

    -- common (inherit fg/bg from text)
    text = {fg = {64, 64, 64}, bg = {250, 250, 250}},
    linenumber = {fg = {128, 128, 128}, bg = {250, 250, 250}},
    bracematch = {fg = {32, 128, 255}, b = true},
    bracemiss = {fg = {255, 128, 32}, b = true},
    ctrlchar = {},
    indent = {fg = {192, 192, 230}, bg = {255, 255, 255}},
    calltip = {},

    -- common special (need custom fg & bg)
    sel = {bg = {208, 208, 208}},
    caret = {fg = {0, 0, 0}},
    caretlinebg = {bg = {240, 240, 230}},
    fold = {fg = {192, 192, 192}, bg = {250, 250, 250}, sel = {160, 128, 224}},
    whitespace = {},
    edge = {},

    -- deprecated; allowed for backward compatibility in case someone does
    -- fncall.fg = {...}
    fncall = {},

    -- markup
    ['|'] = {fg = {127, 0, 127}},
    ['`'] = {fg = {64, 128, 64}},
    ['['] = {hs = {32, 32, 127}},

    -- markers
    marker = {
      currentline = {},
      breakpoint = {},
      message = {},
      output = {},
      prompt = {},
      error = {},
      searchmatchfile = {},
    },

    -- indicators
    indicator = {
      fncall = {},
      varlocal = {},
      varglobal = {},
      varmasking = {},
      varmasked = {},
      varself = {},
      searchmatch = {},
    },
  }
end

local markers = {
  breakpoint = {0, wxstc.wxSTC_MARK_CIRCLE, {196, 64, 64}, {220, 64, 64}},
  bookmark = {1, wxstc.wxSTC_MARK_BOOKMARK or wxstc.wxSTC_MARK_SHORTARROW, {16, 96, 128}, {96, 160, 220}},
  currentline = {2, wxstc.wxSTC_MARK_ARROW, {16, 128, 16}, {64, 220, 64}},
  message = {3, wxstc.wxSTC_MARK_CHARACTER+(' '):byte(), {0, 0, 0}, {220, 220, 220}},
  output = {4, wxstc.wxSTC_MARK_BACKGROUND, {0, 0, 0}, {240, 240, 240}},
  prompt = {5, wxstc.wxSTC_MARK_ARROWS, {0, 0, 0}, {220, 220, 220}},
  error = {6, wxstc.wxSTC_MARK_BACKGROUND, {0, 0, 0}, {255, 220, 220}},
  searchmatchfile = {7, wxstc.wxSTC_MARK_EMPTY, {0, 0, 0}, {196, 0, 0}},
}

local function tint(c)
  return ide.config.markertint and ide:GetTintedColor(c, ide.config.imagetint) or c
end

function StylesGetMarker(marker)
  local id, ch, fg, bg = unpack(markers[marker] or {})
  return id, ch, fg and wx.wxColour(unpack(tint(fg))), bg and wx.wxColour(unpack(tint(bg)))
end
function StylesRemoveMarker(marker) markers[marker] = nil end
function StylesAddMarker(marker, ch, fg, bg)
  if type(fg) ~= "table" or type(bg) ~= "table" then return end
  local num = (markers[marker] or {})[1]
  if not num then -- new marker; find the smallest available marker number
    local nums = {}
    for _, mark in pairs(markers) do nums[mark[1]] = true end
    num = #nums + 1
    if num > 24 then return end -- 24 markers with no pre-defined functions
  end
  markers[marker] = {num, ch, fg, bg}
  return num
end

local function iscolor(c) return type(c) == "table" and #c == 3 end
local function applymarker(editor,marker,clrfg,clrbg,clrsel)
  if (clrfg) then editor:MarkerSetForeground(marker,clrfg) end
  if (clrbg) then editor:MarkerSetBackground(marker,clrbg) end
  if (ide.wxver >= "2.9.5" and clrsel) then editor:MarkerSetBackgroundSelected(marker,clrsel) end
end
local specialmapping = {
  sel = function(editor,style)
    if iscolor(style.fg) then
      editor:SetSelForeground(1,wx.wxColour(unpack(style.fg)))
    else
      editor:SetSelForeground(0,wx.wxWHITE)
    end
    if iscolor(style.bg) then
      editor:SetSelBackground(1,wx.wxColour(unpack(style.bg)))
    else
      editor:SetSelBackground(0,wx.wxWHITE)
    end
    if (style.alpha and ide.wxver >= "2.9.5") then
      editor:SetSelAlpha(style.alpha)
    end

    -- set alpha for additional selecton: 0 - transparent, 255 - opaque
    if ide.wxver >= "2.9.5" then editor:SetAdditionalSelAlpha(127) end
  end,

  seladd = function(editor,style)
    if ide.wxver >= "2.9.5" then
      if iscolor(style.fg) then
        editor:SetAdditionalSelForeground(wx.wxColour(unpack(style.fg)))
      end
      if iscolor(style.bg) then
        editor:SetAdditionalSelBackground(wx.wxColour(unpack(style.bg)))
      end
      if (style.alpha) then
        editor:SetAdditionalSelAlpha(style.alpha)
      end
    end
  end,

  caret = function(editor,style)
    if iscolor(style.fg) then
      editor:SetCaretForeground(wx.wxColour(unpack(style.fg)))
    end
  end,

  caretlinebg = function(editor,style)
    if iscolor(style.bg) then
      editor:SetCaretLineBackground(wx.wxColour(unpack(style.bg)))
    end
    if (style.alpha and ide.wxver >= "2.9.5") then
      editor:SetCaretLineBackAlpha(style.alpha)
    end
  end,

  whitespace = function(editor,style)
    if iscolor(style.fg) then
      editor:SetWhitespaceForeground(1,wx.wxColour(unpack(style.fg)))
    else
      editor:SetWhitespaceForeground(0,wx.wxBLACK) -- color is not used, but needs to be provided
    end
    if iscolor(style.bg) then
      editor:SetWhitespaceBackground(1,wx.wxColour(unpack(style.bg)))
    else
      editor:SetWhitespaceBackground(0,wx.wxBLACK) -- color is not used, but needs to be provided
    end
  end,

  fold = function(editor,style)
    local clrfg = iscolor(style.fg) and wx.wxColour(unpack(style.fg))
    local clrbg = iscolor(style.bg) and wx.wxColour(unpack(style.bg))
    local clrhi = iscolor(style.hi) and wx.wxColour(unpack(style.hi))
    local clrsel = iscolor(style.sel) and wx.wxColour(unpack(style.sel))

    -- if selected background is set then enable support for it
    if ide.wxver >= "2.9.5" and clrsel then editor:MarkerEnableHighlight(true) end

    if (clrfg or clrbg or clrsel) then
      -- foreground and background are defined as opposite to what I'd expect
      -- for fold markers in Scintilla's terminilogy:
      -- background is the color of fold lines/boxes and foreground is the color
      -- of everything around fold lines or inside fold boxes.
      -- in the following code fg and bg are simply reversed
      local clrfg, clrbg = clrbg, clrfg
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDEROPEN, clrfg, clrbg, clrsel)
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDER, clrfg, clrbg, clrsel)
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDERSUB, clrfg, clrbg, clrsel)
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDERTAIL, clrfg, clrbg, clrsel)
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDEREND, clrfg, clrbg, clrsel)
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDEROPENMID, clrfg, clrbg, clrsel)
      applymarker(editor,wxstc.wxSTC_MARKNUM_FOLDERMIDTAIL, clrfg, clrbg, clrsel)
    end
    if clrbg then
      -- the earlier calls only color the actual markers, but not the
      -- overall fold background; SetFoldMargin calls below do this.
      -- http://community.activestate.com/forum-topic/fold-margin-colors
      -- http://www.scintilla.org/ScintillaDoc.html#SCI_SETFOLDMARGINCOLOUR
      editor:SetFoldMarginColour(true, clrbg)
      editor:SetFoldMarginHiColour(true, clrbg)
    end
    if clrhi then
      editor:SetFoldMarginHiColour(true, clrhi)
    end
  end,

  edge = function(editor,style)
    if iscolor(style.fg) then
      editor:SetEdgeColour(wx.wxColour(unpack(style.fg)))
    end
  end,

  marker = function(editor,markers)
    for m, style in pairs(markers) do
      local id, ch, fg, bg = StylesGetMarker(m)
      if style.ch then ch = style.ch end
      if iscolor(style.fg) then fg = wx.wxColour(unpack(tint(style.fg))) end
      if iscolor(style.bg) then bg = wx.wxColour(unpack(tint(style.bg))) end
      editor:MarkerDefine(id, ch, fg, bg)
    end
  end,

  auxwindow = function(editor,style)
    if not style then return end

    -- don't color toolbars as they have their own color/style
    local skipcolor = {wxAuiToolBar = true, wxToolBar = true}
    local default = wxstc.wxSTC_STYLE_DEFAULT
    local bg = iscolor(style.bg) and wx.wxColour(unpack(style.bg)) or editor:StyleGetBackground(default)
    local fg = iscolor(style.fg) and wx.wxColour(unpack(style.fg)) or editor:StyleGetForeground(default)

    local uimgr = ide.frame.uimgr
    local panes = uimgr:GetAllPanes()
    for index = 0, panes:GetCount()-1 do
      local wind = uimgr:GetPane(panes:Item(index).name).window

      -- wxlua compiled with STL doesn't provide GetChildren() method
      -- as per http://sourceforge.net/p/wxlua/mailman/message/32500995/
      local ok, children = pcall(function() return wind:GetChildren() end)
      if not ok then break end

      for child = 0, children:GetCount()-1 do
        local data = children:Item(child):GetData()
        local _, window = pcall(function() return data:DynamicCast("wxWindow") end)
        if window and not skipcolor[window:GetClassInfo():GetClassName()] then
          window:SetBackgroundColour(bg)
          window:SetForegroundColour(fg)
          window:Refresh()
        end
      end
    end
  end,
}

local defaultmapping = {
  text = wxstc.wxSTC_STYLE_DEFAULT,
  linenumber = wxstc.wxSTC_STYLE_LINENUMBER,
  bracematch = wxstc.wxSTC_STYLE_BRACELIGHT,
  bracemiss = wxstc.wxSTC_STYLE_BRACEBAD,
  ctrlchar = wxstc.wxSTC_STYLE_CONTROLCHAR,
  indent = wxstc.wxSTC_STYLE_INDENTGUIDE,
  calltip = wxstc.wxSTC_STYLE_CALLTIP,
}

function StylesApplyToEditor(styles,editor,font,fontitalic,lexerconvert)
  local defaultfg = styles.text and iscolor(styles.text.fg) and wx.wxColour(unpack(styles.text.fg)) or nil
  local defaultbg = styles.text and iscolor(styles.text.bg) and wx.wxColour(unpack(styles.text.bg)) or nil

  local function applystyle(style,id)
    editor:StyleSetFont(id, style.i and fontitalic or font)
    editor:StyleSetBold(id, style.b or false)
    editor:StyleSetUnderline(id, style.u or false)
    editor:StyleSetEOLFilled(id, style.fill or false)

    if style.fn then editor:StyleSetFaceName(id, style.fn) end
    if style.fs then editor:StyleSetSize(id, style.fs) end
    if style.v ~= nil then editor:StyleSetVisible(id, style.v) end

    if style.hs then
      editor:StyleSetHotSpot(id, 1)
      -- if passed a color (table) as value, set it as foreground
      if iscolor(style.hs) then
        local color = wx.wxColour(unpack(style.hs))
        editor:SetHotspotActiveForeground(1, color)
      end
      editor:SetHotspotActiveUnderline(1)
      editor:SetHotspotSingleLine(1)
    end

    if iscolor(style.fg) or defaultfg then
      editor:StyleSetForeground(id, style.fg and wx.wxColour(unpack(style.fg)) or defaultfg)
    end
    if iscolor(style.bg) or defaultbg then
      editor:StyleSetBackground(id, style.bg and wx.wxColour(unpack(style.bg)) or defaultbg)
    end
  end

  editor:StyleResetDefault()
  editor:SetFont(font)
  if (styles.text) then
    applystyle(styles.text,defaultmapping["text"])
  else
    applystyle({},defaultmapping["text"])
  end
  editor:StyleClearAll()

  -- set the default linenumber font size based on the editor font size
  if styles.linenumber and not styles.linenumber.fs then
    styles.linenumber.fs = ide.config.editor.fontsize and (ide.config.editor.fontsize - 1) or nil
  end

  for name,style in pairs(styles) do
    if (specialmapping[name]) then
      specialmapping[name](editor,style)
    elseif (defaultmapping[name]) then
      applystyle(style,defaultmapping[name])
    end

    if (lexerconvert and lexerconvert[name]) then
      local targets = lexerconvert[name]
      for _, outid in pairs(targets) do
        applystyle(style,outid)
      end
    elseif style.st then
      applystyle(style,style.st)
    end
  end

  -- additional selection (seladd) attributes can only be set after
  -- normal selection (sel) attributes are set, so handle them again
  if styles.seladd then specialmapping.seladd(editor, styles.seladd) end

  -- calltip has a special style that needs to be enabled
  if styles.calltip then editor:CallTipUseStyle(2) end

  do
    local defaultfg = {127,127,127}
    local indic = styles.indicator or {}

    -- use styles.fncall if not empty and if indic.fncall is empty
    -- for backward compatibility
    if type(styles.fncall) == 'table' and next(styles.fncall)
    and not (type(indic.fncall) == 'table' and next(indic.fncall)) then indic.fncall = styles.fncall end

    local fncall = ide:AddIndicator("core.fncall")
    local varlocal = ide:AddIndicator("core.varlocal")
    local varself = ide:AddIndicator("core.varself")
    local varglobal = ide:AddIndicator("core.varglobal")
    local varmasking = ide:AddIndicator("core.varmasking")
    local varmasked = ide:AddIndicator("core.varmasked")
    local searchmatch = ide:AddIndicator("core.searchmatch")

    editor:IndicatorSetStyle(fncall, type(indic.fncall) == type{} and indic.fncall.st or ide.wxver >= "2.9.5" and wxstc.wxSTC_INDIC_ROUNDBOX or wxstc.wxSTC_INDIC_TT)
    editor:IndicatorSetForeground(fncall, wx.wxColour(unpack(type(indic.fncall) == type{} and indic.fncall.fg or {128, 128, 255})))
    editor:IndicatorSetStyle(varlocal, type(indic.varlocal) == type{} and indic.varlocal.st or wxstc.wxSTC_INDIC_DOTS or wxstc.wxSTC_INDIC_TT)
    editor:IndicatorSetForeground(varlocal, wx.wxColour(unpack(type(indic.varlocal) == type{} and indic.varlocal.fg or defaultfg)))
    editor:IndicatorSetStyle(varself, type(indic.varself) == type{} and indic.varself.st or wxstc.wxSTC_INDIC_DOTS)
    editor:IndicatorSetForeground(varself, wx.wxColour(unpack(type(indic.varself) == type{} and indic.varself.fg or defaultfg)))
    editor:IndicatorSetStyle(varglobal, type(indic.varglobal) == type{} and indic.varglobal.st or wxstc.wxSTC_INDIC_PLAIN)
    editor:IndicatorSetForeground(varglobal, wx.wxColour(unpack(type(indic.varglobal) == type{} and indic.varglobal.fg or defaultfg)))
    editor:IndicatorSetStyle(varmasking, type(indic.varmasking) == type{} and indic.varmasking.st or wxstc.wxSTC_INDIC_DASH or wxstc.wxSTC_INDIC_DIAGONAL)
    editor:IndicatorSetForeground(varmasking, wx.wxColour(unpack(type(indic.varmasking) == type{} and indic.varmasking.fg or defaultfg)))
    editor:IndicatorSetStyle(varmasked, type(indic.varmasked) == type{} and indic.varmasked.st or wxstc.wxSTC_INDIC_STRIKE)
    editor:IndicatorSetForeground(varmasked, wx.wxColour(unpack(type(indic.varmasked) == type{} and indic.varmasked.fg or defaultfg)))
    editor:IndicatorSetStyle(searchmatch, type(indic.searchmatch) == type{} and indic.searchmatch.st or wxstc.wxSTC_INDIC_BOX)
    editor:IndicatorSetForeground(searchmatch, wx.wxColour(unpack(type(indic.searchmatch) == type{} and indic.searchmatch.fg or {196, 0, 0})))
  end
end

function ReApplySpecAndStyles()
  -- re-register markup styles as they are special:
  -- these styles need to be updated as they are based on comment styles
  if MarkupAddStyles then MarkupAddStyles(ide.config.styles) end

  local errorlog = ide.frame.bottomnotebook.errorlog
  local shellbox = ide.frame.bottomnotebook.shellbox
  SetupKeywords(shellbox,"lua",nil,ide.config.stylesoutshell,ide.font.oNormal,ide.font.oItalic)
  StylesApplyToEditor(ide.config.stylesoutshell,errorlog,ide.font.oNormal,ide.font.oItalic)

  for _, doc in pairs(ide:GetDocuments()) do
    if doc.editor.spec then doc.editor:SetupKeywords(nil, doc.editor.spec) end
  end
end

function ApplyStyleConfig(config, style)
  if not wx.wxIsAbsolutePath(config)
    then config = MergeFullPath(GetPathWithSep(ide.editorFilename), config) end

  local cfg = {wxstc = wxstc, math = math, print = function(...) ide:Print(...) end,
    path = {}, editor = {}, view ={}, acandtip = {}, outputshell = {}, debugger={}}
  local cfgfn, err = loadfile(config)
  if not cfgfn then
    ide:Print(TR("Error while loading configuration file: %s"):format(err))
    return
  end

  setfenv(cfgfn,cfg)
  cfgfn, err = pcall(cfgfn,style)
  if not cfgfn then
    ide:Print(TR("Error while processing configuration file: %s"):format(err))
    return
  end

  -- if no style assigned explicitly, but a table is returned, use it
  if not (cfg.styles or cfg.stylesoutshell) and type(err) == 'table' then
    cfg.styles = err
  end

  if cfg.styles or cfg.stylesoutshell then
    if (cfg.styles) then
      ide.config.styles = StylesGetDefault()
      -- copy
      for i,s in pairs(cfg.styles) do
        ide.config.styles[i] = s
      end
    end
    if (cfg.stylesoutshell) then
      ide.config.stylesoutshell = StylesGetDefault()
      -- copy
      for i,s in pairs(cfg.stylesoutshell) do
        ide.config.stylesoutshell[i] = s
      end
    end
    ReApplySpecAndStyles()
  end
end
-- Copyright 2014-17 Paul Kulchenko, ZeroBrane LLC

local TR = function(...) return ... end

ide.config.toolbar = ide.config.toolbar or {}

ide.config.toolbar.icons = {
  ID.NEW, ID.OPEN, ID.SAVE, ID.SAVEALL, ID.PROJECTDIRFROMFILE, ID.PROJECTDIRCHOOSE,
  ID.SEPARATOR,
  ID.FIND, ID.REPLACE, ID.FINDINFILES,
  ID.SEPARATOR,
  ID.RUN, ID.STARTDEBUG, ID.RUNNOW, ID.STOPDEBUG, ID.DETACHDEBUG, ID.BREAK,
  ID.COMPILE, ID.STEP, ID.STEPOVER, ID.STEPOUT, ID.RUNTO,
  ID.SEPARATOR,
  ID.BREAKPOINTTOGGLE, ID.BOOKMARKTOGGLE, ID.VIEWCALLSTACK, ID.VIEWWATCHWINDOW,
  [ID.FINDINFILES] = false,
  [ID.COMPILE] = false,
}

ide.config.toolbar.iconmap = {
  [ID.NEW] = {"FILE-NEW", TR("Create an empty document")},
  [ID.OPEN] = {"FILE-OPEN", TR("Open an existing document")},
  [ID.SAVE] = {"FILE-SAVE", TR("Save the current document")},
  [ID.SAVEALL] = {"FILE-SAVE-ALL", TR("Save all open documents")},
  [ID.PROJECTDIRFROMFILE]= {"DIR-SETUP-FILE", TR("Set project directory from current file")},
  [ID.PROJECTDIRCHOOSE] = {"DIR-SETUP", TR("Choose a project directory")},
  [ID.FIND] = {"FIND", TR("Find text")},
  [ID.REPLACE] = {"FIND-AND-REPLACE", TR("Find and replace text")},
  [ID.FINDINFILES] = {"FIND-IN-FILES", TR("Find in files")},
  [ID.COMPILE] = {"COMPILE", TR("Compile the current file")},
  [ID.RUN] = {"RUN", TR("Execute the current project/file")},
  [ID.RUNNOW] = {"RUN-NOW", TR("Run as Scratchpad")},
  [ID.STARTDEBUG] = {"DEBUG-START", TR("Start or continue debugging")},
  [ID.STOPDEBUG] = {"DEBUG-STOP", TR("Stop the currently running process")},
  [ID.DETACHDEBUG]= {"DEBUG-DETACH", TR("Stop debugging and continue running the process")},
  [ID.BREAK] = {"DEBUG-BREAK", TR("Break execution at the next executed line of code")},
  [ID.RUNTO] = {"DEBUG-RUN-TO", TR("Run to cursor")},
  [ID.STEP] = {"DEBUG-STEP-INTO", TR("Step into")},
  [ID.STEPOVER] = {"DEBUG-STEP-OVER", TR("Step over")},
  [ID.STEPOUT] = {"DEBUG-STEP-OUT", TR("Step out of the current function")},
  [ID.BREAKPOINTTOGGLE] = {"DEBUG-BREAKPOINT-TOGGLE", TR("Toggle breakpoint")},
  [ID.BOOKMARKTOGGLE] = {"BOOKMARK-TOGGLE", TR("Toggle bookmark")},
  [ID.VIEWCALLSTACK] = {"DEBUG-CALLSTACK", TR("View the stack window")},
  [ID.VIEWWATCHWINDOW] = {"DEBUG-WATCH", TR("View the watch window")},
  -- search toolbar
  [ID.FINDNEXT] = {"FIND-NEXT", TR("Find next")},
  [ID.FINDREPLACENEXT] = {"FIND-REPLACE-NEXT", TR("Replace next instance")},
  [ID.FINDREPLACEALL] = {"FIND-AND-REPLACE", TR("Replace all")},
  [ID.FINDSETDIR] = {"FIND-OPT-SETDIR", TR("Set search directory")},
  [ID.FINDOPTDIRECTION] = {"FIND-OPT-DOWN", TR("Search direction")},
  [ID.FINDOPTWRAPWROUND] = {"FIND-OPT-WRAP-AROUND", TR("Wrap around")},
  [ID.FINDOPTSELECTION] = {"FIND-OPT-SELECTION", TR("Search in selection")},
  [ID.FINDOPTWORD] = {"FIND-OPT-WORD", TR("Match whole word")},
  [ID.FINDOPTCASE] = {"FIND-OPT-CASE-SENSITIVE", TR("Match case")},
  [ID.FINDOPTREGEX] = {"FIND-OPT-REGEX", TR("Regular expression")},
  [ID.FINDOPTCONTEXT] = {"FIND-OPT-CONTEXT", TR("Show context")},
  [ID.FINDOPTSUBDIR] = {"FIND-OPT-SUBDIR", TR("Search in subdirectories")},
  [ID.FINDOPTMULTIRESULTS] = {"FIND-OPT-MULTI-RESULTS", TR("Show multiple result windows")},
}
-- Copyright 2011-17 Paul Kulchenko, ZeroBrane LLC
-- authors: Lomtik Software (J. Winwood & John Labenski)
-- Luxinia Dev (Eike Decker & Christoph Kubisch)
-- David Manura
---------------------------------------------------------

-- Equivalent to C's "cond ? a : b", all terms will be evaluated
function iff(cond, a, b) if cond then return a else return b end end

function EscapeMagic(s) return s:gsub('([%(%)%.%%%+%-%*%?%[%^%$%]])','%%%1') end

function GetPathSeparator()
  return string.char(wx.wxFileName.GetPathSeparator())
end

do
  local sep = GetPathSeparator()
  function IsDirectory(dir) return dir:find(sep.."$") end
end

function PrependStringToArray(t, s, maxstrings, issame)
  if string.len(s) == 0 then return end
  for i = #t, 1, -1 do
    local v = t[i]
    if v == s or issame and issame(s, v) then
      table.remove(t, i) -- remove old copy
      -- don't break here in case there are multiple copies to remove
    end
  end
  table.insert(t, 1, s)
  if #t > (maxstrings or 15) then table.remove(t, #t) end -- keep reasonable length
end

function GetFileModTime(filePath)
  if filePath and #filePath > 0 then
    local fn = wx.wxFileName(filePath)
    if fn:FileExists() then
      return fn:GetModificationTime()
    end
  end

  return nil
end

function GetFileExt(filePath)
  local match = filePath and filePath:gsub("%s+$",""):match("%.([^./\\]*)$")
  return match and match:lower() or ''
end

function GetFileName(filePath)
  return filePath and filePath:gsub("%s+$",""):match("([^/\\]*)$") or ''
end

function IsLuaFile(filePath)
  return filePath and (string.len(filePath) > 4) and
  (string.lower(string.sub(filePath, -4)) == ".lua")
end

function GetPathWithSep(wxfn)
  if type(wxfn) == 'string' then wxfn = wx.wxFileName(wxfn) end
  return wxfn:GetPath(bit.bor(wx.wxPATH_GET_VOLUME, wx.wxPATH_GET_SEPARATOR))
end

function FileDirHasContent(dir)
  local f = wx.wxFindFirstFile(dir, wx.wxFILE + wx.wxDIR)
  return #f>0
end

function FileSysGetRecursive(path, recursive, spec, opts)
  local content = {}
  local showhidden = ide.config and ide.config.showhiddenfiles
  local sep = GetPathSeparator()
  -- trip trailing separator and adjust the separator in the path
  path = path:gsub("[\\/]$",""):gsub("[\\/]", sep)
  local queue = {path}
  local pathpatt = "^"..EscapeMagic(path)..sep.."?"
  local optyield = (opts or {}).yield
  local optfolder = (opts or {}).folder ~= false
  local optsort = (opts or {}).sort ~= false
  local optpath = (opts or {}).path ~= false
  local optskipbinary = (opts or {}).skipbinary
  local optondirectory = (opts or {}).ondirectory

  local function spec2list(spect, list)
    -- return empty list if no spec is provided
    if spect == nil or spect == "*" or spect == "*.*" then return {}, 0 end
    -- accept "*.lua" and "*.txt,*.wlua" combinations
    local masknum, list = 0, list or {}
    for spec, specopt in pairs(type(spect) == 'table' and spect or {spect}) do
      -- specs can be kept as `{[spec] = true}` or `{spec}`, so handle both cases
      if type(spec) == "number" then spec = specopt end
      if specopt == false then spec = "" end -- skip keys with `false` values
      for m in spec:gmatch("[^%s;,]+") do
        m = m:gsub("[\\/]", sep)
        if m:find("^%*%.%w+"..sep.."?$") then
          list[m:sub(2)] = true
        else
          -- escape all special characters
          table.insert(list, EscapeMagic(m)
            :gsub("%%%*%%%*", ".*") -- replace (escaped) ** with .*
            :gsub("%%%*", "[^/\\]*") -- replace (escaped) * with [^\//]*
            :gsub("^"..sep, ide:GetProject() or "") -- replace leading separator with project directory (if set)
            .."$")
        end
        masknum = masknum + 1
      end
    end
    return list, masknum
  end

  local inmasks, masknum = spec2list(spec)
  if masknum >= 2 then spec = nil end

  local exmasks = spec2list(ide.config.excludelist or {})
  if optskipbinary then -- add any binary files to the list to skip
    exmasks = spec2list(type(optskipbinary) == 'table' and optskipbinary
      or ide.config.binarylist or {}, exmasks)
  end

  local function ismatch(file, inmasks, exmasks)
    -- convert extension 'foo' to '.foo', as need to distinguish file
    -- from extension with the same name
    local ext = '.'..GetFileExt(file)
    -- check exclusions if needed
    if exmasks[file] or exmasks[ext] then return false end
    for _, mask in ipairs(exmasks) do
      if file:find(mask) then return false end
    end

    -- return true if none of the exclusions match and no inclusion list
    if not inmasks or not next(inmasks) then return true end

    -- now check inclusions
    if inmasks[file] or inmasks[ext] then return true end
    for _, mask in ipairs(inmasks) do
      if file:find(mask) then return true end
    end
    return false
  end

  local function report(fname)
    if optyield then return coroutine.yield(fname) end
    table.insert(content, fname)
  end

  local dir = wx.wxDir()
  local function getDir(path)
    dir:Open(path)
    if not dir:IsOpened() then
      if TR then ide:Print(TR("Can't open '%s': %s"):format(path, wx.wxSysErrorMsg())) end
      return
    end

    -- recursion is done in all folders if requested,
    -- but only those folders that match the spec are returned
    local _ = wx.wxLogNull() -- disable error reporting; will report as needed
    local found, file = dir:GetFirst("*",
      wx.wxDIR_DIRS + ((showhidden == true or showhidden == wx.wxDIR_DIRS) and wx.wxDIR_HIDDEN or 0))
    while found do
      local fname = path..sep..file
      if optfolder and ismatch(fname..sep, inmasks, exmasks) then
        report((optpath and fname or fname:gsub(pathpatt, ""))..sep)
      end

      if recursive and ismatch(fname..sep, nil, exmasks)
      and (not optondirectory or optondirectory(fname) ~= false)
      -- check if this name already appears in the path earlier;
      -- Skip the processing if it does as it could lead to infinite
      -- recursion with circular references created by symlinks.
      and select(2, fname:gsub(EscapeMagic(file..sep),'')) <= 2 then
        table.insert(queue, fname)
      end
      found, file = dir:GetNext()
    end
    found, file = dir:GetFirst(spec or "*",
      wx.wxDIR_FILES + ((showhidden == true or showhidden == wx.wxDIR_FILES) and wx.wxDIR_HIDDEN or 0))
    while found do
      local fname = path..sep..file
      if ismatch(fname, inmasks, exmasks) then
        report(optpath and fname or fname:gsub(pathpatt, ""))
      end
      found, file = dir:GetNext()
    end
    -- wxlua < 3.1 doesn't provide Close method for the directory, so check for it
    if ide:IsValidProperty(dir, "Close") then dir:Close() end
  end
  while #queue > 0 do getDir(table.remove(queue)) end

  if optyield then return end

  if optsort then
    local prefix = '\001' -- prefix to sort directories first
    local shadow = {}
    for _, v in ipairs(content) do
      shadow[v] = (v:sub(-1) == sep and prefix or '')..v:lower()
    end
    table.sort(content, function(a,b) return shadow[a] < shadow[b] end)
  end

  return content
end

local normalflags = wx.wxPATH_NORM_ABSOLUTE + wx.wxPATH_NORM_DOTS + wx.wxPATH_NORM_TILDE
function MergeFullPath(p, f)
  if not p or not f then return end
  local file = wx.wxFileName(f)
  -- Normalize call is needed to make the case of p = '/abc/def' and
  -- f = 'xyz/main.lua' work correctly. Normalize() returns true if done.
  -- Normalization with PATH_NORM_DOTS removes leading dots, which need to be added back.
  -- This allows things like `-cfg ../myconfig.lua` to work.
  local rel, rest = p:match("^(%.[/\\.]*[/\\])(.*)")
  if rel and rest then p = rest end
  return (file:Normalize(normalflags, p)
    and (rel or ""):gsub("[/\\]", GetPathSeparator())..file:GetFullPath()
    or nil)
end

function FileNormalizePath(path)
  local filePath = wx.wxFileName(path)
  filePath:Normalize()
  filePath:SetVolume(filePath:GetVolume():upper())
  return filePath:GetFullPath()
end

function FileGetLongPath(path)
  local fn = wx.wxFileName(path)
  local vol = fn:GetVolume():upper()
  local volsep = vol and vol:byte() and wx.wxFileName.GetVolumeSeparator(vol:byte()-("A"):byte()+1)
  local dir = wx.wxDir()
  local dirs = fn:GetDirs()
  table.insert(dirs, fn:GetFullName())
  local normalized = vol and volsep and vol..volsep or (path:match("^[/\\]") or ".")
  local hasclose = ide:IsValidProperty(dir, "Close")
  while #dirs > 0 do
    dir:Open(normalized)
    if not dir:IsOpened() then return path end
    local p = table.remove(dirs, 1)
    local ok, segment = dir:GetFirst(p)
    if not ok then return path end
    normalized = MergeFullPath(normalized,segment)
    if hasclose then dir:Close() end
  end
  local file = wx.wxFileName(normalized)
  file:Normalize(wx.wxPATH_NORM_DOTS) -- remove leading dots, if any
  return file:GetFullPath()
end

function CreateFullPath(path)
  local ok = wx.wxFileName.Mkdir(path, tonumber(755,8), wx.wxPATH_MKDIR_FULL)
  return ok, not ok and wx.wxSysErrorMsg() or nil
end
function GetFullPathIfExists(p, f)
  local path = MergeFullPath(p, f)
  return path and wx.wxFileExists(path) and path or nil
end

function FileWrite(file, content)
  local _ = wx.wxLogNull() -- disable error reporting; will report as needed

  if not wx.wxFileExists(file)
  and not wx.wxFileName(file):Mkdir(tonumber(755,8), wx.wxPATH_MKDIR_FULL) then
    return nil, wx.wxSysErrorMsg()
  end

  local file = wx.wxFile(file, wx.wxFile.write)
  if not file:IsOpened() then return nil, wx.wxSysErrorMsg() end

  local ok = file:Write(content, #content) == #content
  file:Close()
  return ok, not ok and wx.wxSysErrorMsg() or nil
end

function FileSize(fname)
  if not wx.wxFileExists(fname) then return end
  local size = wx.wxFileSize(fname)
  -- size can be returned as 0 for symlinks, so check with wxFile:Length();
  -- can't use wxFile:Length() as it's reported incorrectly for some non-seekable files
  -- (see https://github.com/pkulchenko/ZeroBraneStudio/issues/458);
  -- the combination of wxFileSize and wxFile:Length() should do the right thing.
  if size == 0 then size = wx.wxFile(fname, wx.wxFile.read):Length() end
  return size
end

function FileRead(fname, length, callback)
  -- on OSX "Open" dialog allows to open applications, which are folders
  if wx.wxDirExists(fname) then return nil, "Can't read directory as file." end

  local _ = wx.wxLogNull() -- disable error reporting; will report as needed
  local file = wx.wxFile(fname, wx.wxFile.read)
  if not file:IsOpened() then return nil, wx.wxSysErrorMsg() end

  if type(callback) == 'function' then
    length = length or 8192
    local pos = 0
    while true do
      local len, content = file:Read(length)
      local res, msg = callback(content) -- may return `false` to signal to stop
      if res == false then
        file:Close()
        return false, msg or "Unknown error"
      end
      if len < length then break end
      pos = pos + len
      file:Seek(pos)
    end
    file:Close()
    return true, wx.wxSysErrorMsg()
  end

  local _, content = file:Read(length or FileSize(fname))
  file:Close()
  return content, wx.wxSysErrorMsg()
end

function FileRemove(file)
  local _ = wx.wxLogNull() -- disable error reporting; will report as needed
  return wx.wxRemoveFile(file), wx.wxSysErrorMsg()
end

function FileRename(file1, file2)
  local _ = wx.wxLogNull() -- disable error reporting; will report as needed
  return wx.wxRenameFile(file1, file2), wx.wxSysErrorMsg()
end

function FileCopy(file1, file2)
  local _ = wx.wxLogNull() -- disable error reporting; will report as needed
  return wx.wxCopyFile(file1, file2), wx.wxSysErrorMsg()
end

local ok, socket = pcall(require, "socket")
TimeGet = ok and socket.gettime or os.clock

function IsBinary(text) return text:find("[^\7\8\9\10\12\13\27\32-\255]") and true or false end

function pairsSorted(t, f)
  local a = {}
  for n in pairs(t) do table.insert(a, n) end
  table.sort(a, f)
  local i = 0 -- iterator variable
  local iter = function () -- iterator function
    i = i + 1
    if a[i] == nil then return nil
    else return a[i], t[a[i]]
    end
  end
  return iter
end

function FixUTF8(s, repl)
  local p, len, invalid = 1, #s, {}
  while p <= len do
    if     s:find("^[%z\1-\127]", p) then p = p + 1
    elseif s:find("^[\194-\223][\128-\191]", p) then p = p + 2
    elseif s:find(       "^\224[\160-\191][\128-\191]", p)
        or s:find("^[\225-\236][\128-\191][\128-\191]", p)
        or s:find(       "^\237[\128-\159][\128-\191]", p)
        or s:find("^[\238-\239][\128-\191][\128-\191]", p) then p = p + 3
    elseif s:find(       "^\240[\144-\191][\128-\191][\128-\191]", p)
        or s:find("^[\241-\243][\128-\191][\128-\191][\128-\191]", p)
        or s:find(       "^\244[\128-\143][\128-\191][\128-\191]", p) then p = p + 4
    else
      if not repl then return end -- just signal invalid UTF8 string
      local repl = type(repl) == 'function' and repl(s:sub(p,p)) or repl
      s = s:sub(1, p-1)..repl..s:sub(p+1)
      table.insert(invalid, p)
      -- adjust position/length as the replacement may be longer than one char
      p = p + #repl
      len = len + #repl - 1
    end
  end
  return s, invalid
end

function TR(msg, count)
  local messages = ide.messages
  local lang = ide.config.language
  local counter = messages[lang] and messages[lang][0]
  local message = messages[lang] and messages[lang][msg]
  -- if there is count and no corresponding message, then
  -- get the message from the (default) english language,
  -- otherwise the message is not going to be pluralized properly
  if count and (not message or type(message) == 'table' and not next(message)) then
    message, counter = messages.en[msg], messages.en[0]
  end
  return count and counter and message and type(message) == 'table'
    and message[counter(count)] or (type(message) == 'string' and message or msg)
end

-- wxwidgets 2.9.x may report the last folder twice (depending on how the
-- user selects the folder), which makes the selected folder incorrect.
-- check if the last segment is repeated and drop it.
function FixDir(path)
  if wx.wxDirExists(path) then return path end

  local dir = wx.wxFileName.DirName(path)
  local dirs = dir:GetDirs()
  if #dirs > 1 and dirs[#dirs] == dirs[#dirs-1] then dir:RemoveLastDir() end
  return dir:GetFullPath()
end

function ShowLocation(fname)
  local osxcmd = [[osascript -e 'tell application "Finder" to reveal POSIX file "%s"']]
    .. [[ -e 'tell application "Finder" to activate']]
  local wincmd = [[explorer /select,"%s"]]
  local lnxcmd = [[xdg-open "%s"]] -- takes path, not a filename
  local cmd =
    ide.osname == "Windows" and wincmd:format(fname) or
    ide.osname == "Macintosh" and osxcmd:format(fname) or
    ide.osname == "Unix" and lnxcmd:format(wx.wxFileName(fname):GetPath())
  if cmd then wx.wxExecute(cmd, wx.wxEXEC_ASYNC) end
end

function LoadLuaFileExt(tab, file, proto)
  local cfgfn,err = loadfile(file)
  if not cfgfn then
    ide:Print(("Error while loading file: '%s'."):format(err))
  else
    local name = file:match("([a-zA-Z_0-9%-]+)%.lua$")
    if not name then return end

    -- check if os/arch matches to allow packages for different systems
    local osvals = {windows = true, unix = true, macintosh = true}
    local archvals = {x64 = true, x86 = true}
    local os, arch = name:match("-(%w+)-?(%w*)")
    if os and os:lower() ~= ide.osname:lower() and osvals[os:lower()]
    or arch and #arch > 0 and arch:lower() ~= ide.osarch:lower() and archvals[arch:lower()]
    then return end
    if os and osvals[os:lower()] then name = name:gsub("-.*","") end

    local success, result = pcall(function()return cfgfn(assert(_G or _ENV))end)
    if not success then
      ide:Print(("Error while processing file: '%s'."):format(result))
    else
      if (tab[name]) then
        local out = tab[name]
        for i,v in pairs(result) do
          out[i] = v
        end
      else
        tab[name] = proto and result and setmetatable(result, proto) or result
        if tab[name] then tab[name].fpath = file end
      end
    end
  end
  return tab
end

function LoadLuaConfig(filename,isstring)
  if not filename then return end
  -- skip those files that don't exist
  if not isstring and not wx.wxFileExists(filename) then return end
  -- if it's marked as command, but exists as a file, load it as a file
  if isstring and wx.wxFileExists(filename) then isstring = false end

  local cfgfn, err, msg
  if isstring
  then msg, cfgfn, err = "string", loadstring(filename)
  else msg, cfgfn, err = "file", loadfile(filename) end

  if not cfgfn then
    ide:Print(("Error while loading configuration %s: '%s'."):format(msg, err))
  else
    setfenv(cfgfn,ide.config)
    table.insert(ide.configqueue, (wx.wxFileName().SplitPath(filename)))
    local _, err = pcall(function()cfgfn(assert(_G or _ENV))end)
    table.remove(ide.configqueue)
    if err then
      ide:Print(("Error while processing configuration %s: '%s'."):format(msg, err))
    end
  end
  return true
end

function LoadSafe(data)
  local f, res = loadstring(data)
  if not f then return f, res end

  local count = 0
  debug.sethook(function ()
    count = count + 1
    if count >= 3 then error("cannot call functions") end
  end, "c")
  local ok, res = pcall(f)
  count = 0
  debug.sethook()
  return ok, res
end

local function isCtrlFocused(e)
  local ctrl = e and e:FindFocus()
  return ctrl and
    (ctrl:GetId() == e:GetId()
     or ide.osname == 'Macintosh' and
       ctrl:GetParent():GetId() == e:GetId()) and ctrl or nil
end

function GetEditorWithFocus(...)
  -- need to distinguish GetEditorWithFocus() and GetEditorWithFocus(nil)
  -- as the latter may happen when GetEditor() is passed and returns `nil`
  if select('#', ...) > 0 then
    local ed = ...
    return isCtrlFocused(ed) and ed or nil
  end

  local editor = GetEditor()
  if isCtrlFocused(editor) then return editor end

  local nb = ide:GetOutputNotebook()
  for p = 0, nb:GetPageCount()-1 do
    local ctrl = nb:GetPage(p)
    if ctrl:GetClassInfo():GetClassName() == "wxStyledTextCtrl"
    and isCtrlFocused(ctrl) then
      return ctrl:DynamicCast("wxStyledTextCtrl")
    end
  end
  return nil
end

function GenerateProgramFilesPath(exec, sep)
  local env = os.getenv('ProgramFiles')
  return
    (env and env..'\\'..exec..sep or '')..
    [[C:\Program Files\]]..exec..sep..
    [[D:\Program Files\]]..exec..sep..
    [[C:\Program Files (x86)\]]..exec..sep..
    [[D:\Program Files (x86)\]]..exec
end

--[[ format placeholders
    - %f -- full project name (project path)
    - %s -- short project name (directory name)
    - %i -- interpreter name
    - %S -- file name
    - %F -- file path
    - %n -- line number
    - %c -- line content
    - %T -- application title
    - %v -- application version
    - %t -- current tab name
--]]
function ExpandPlaceholders(msg, ph)
  ph = ph or {}
  if type(msg) == 'function' then return msg(ph) end
  local editor = ide:GetEditor()
  local proj = ide:GetProject() or ""
  local dirs = wx.wxFileName(proj):GetDirs()
  local doc = editor and ide:GetDocument(editor)
  local nb = ide:GetEditorNotebook()
  local def = {
    f = proj,
    s = dirs[#dirs] or "",
    i = ide:GetInterpreter():GetName() or "",
    S = doc and doc:GetFileName() or "",
    F = doc and doc:GetFilePath() or "",
    n = editor and editor:GetCurrentLine()+1 or 0,
    c = editor and editor:GetLineDyn(editor:GetCurrentLine()) or "",
    T = ide:GetProperty("editor") or "",
    v = ide.VERSION,
    t = editor and nb:GetPageText(nb:GetPageIndex(editor)) or "",
  }
  return(msg:gsub('%%(%w)', function(p) return ph[p] or def[p] or '?' end))
end

function MergeSettings(localSettings, savedSettings)
  for name in pairs(localSettings) do
    if savedSettings[name] ~= nil
    and type(savedSettings[name]) == type(localSettings[name]) then
      if type(localSettings[name]) == 'table'
      and next(localSettings[name]) ~= nil then
        -- check every value in the table to make sure that it's possible
        -- to add new keys to the table and they get correct default values
        -- (even though that are absent in savedSettings)
        for setting in pairs(localSettings[name]) do
          if savedSettings[name][setting] ~= nil then
            localSettings[name][setting] = savedSettings[name][setting]
           end
        end
      else
        localSettings[name] = savedSettings[name]
      end
    end
  end
end

function UpdateMenuUI(menu, obj)
  if not menu or not obj then return end
  for pos = 0, menu:GetMenuItemCount()-1 do
    local id = menu:FindItemByPosition(pos):GetId()
    local uievent = wx.wxUpdateUIEvent(id)
    obj:ProcessEvent(uievent)
    menu:Enable(id, not uievent:GetSetEnabled() or uievent:GetEnabled())
  end
end

local function plaindump(val, opts, done)
  local keyignore = opts and opts.keyignore or {}
  local final = done == nil
  opts, done = opts or {}, done or {}
  local t = type(val)
  if t == "table" then
    done[#done+1] = '{'
    done[#done+1] = ''
    for key, value in pairs (val) do
      if not keyignore[key] then
        done[#done+1] = '['
        plaindump(key, opts, done)
        done[#done+1] = ']='
        plaindump(value, opts, done)
        done[#done+1] = ","
      end
    end
    done[#done] = '}'
  elseif t == "string" then
    done[#done+1] = ("%q"):format(val):gsub("\010","n"):gsub("\026","\\026")
  elseif t == "number" then
    done[#done+1] = ("%.17g"):format(val)
  else
    done[#done+1] = tostring(val)
  end
  return final and table.concat(done, '')
end

DumpPlain = plaindump
ide.VERSION = [[devel]]