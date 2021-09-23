local function toDictionary(...)
	local t = {}
	for _, v in ipairs({...}) do
		t[v] = true
	end
	return t
end
local lettersL = toDictionary("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
local lettersU = toDictionary("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
local lettersH = toDictionary("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "a", "B", "b", "C", "c", "D", "d", "E", "e", "F", "f")
local numbers = toDictionary("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
local operators = toDictionary("+", "-", "*", "/", "^", "%", ",", "{", "}", "[", "]", "(", ")", ";", "#")
local operatorsLuau, _ = toDictionary("+", "-", "*", "/", "%", "^", ".."), "Pepsi was here"
local keywords = toDictionary("and", "break", "continue", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while")
local localCount = 0
local getNewLocal = (function()
	local a, b = {}, {}
	for i = 97, 122 do
		i = string.char(i)
		a[#a + 1], b[#b + 1] = i, i
	end
	for i = 65, 90 do
		i = string.char(i)
		a[#a + 1], b[#b + 1] = i, i
	end
	for i = 0, 9 do
		a[#a + 1] = i
	end
	a[#a + 1] = "_"
	local function c(d)
		local e = ""
		local f = d % #b
		d = (d - f) / #b
		e = e .. b[f + 1]
		while d > 0 do
			local f = d % #a
			d = (d - f) / #a
			e = e .. a[f + 1]
		end
		return e
	end
	return function()
		local g = ""
		repeat
			local h = localCount
			localCount = localCount + 1
			g = c(h)
		until not keywords[g]
		return g
	end
end)()
local Scope = {
	new = function(a, b)
		local c = {
			Parent = b,
			Locals = {},
			Globals = {},
			oldLocalNamesMap = {},
			oldGlobalNamesMap = {},
			Children = {}
		}
		if b then
			b.Children[#b.Children + 1] = c
		end
		return setmetatable(c, {
			__index = a
		})
	end,
	AddLocal = function(e, d)
		e.Locals[#e.Locals + 1] = d
	end,
	AddGlobal = function(f, g)
		f.Globals[#f.Globals + 1] = g
	end,
	CreateLocal = function(h, i)
		local j
		j = h:GetLocal(i)
		if j then
			return j
		end
		j = {}
		j.Scope = h
		j.Name = i
		j.IsGlobal = false
		j.CanRename = true
		j.References = 1
		h:AddLocal(j)
		return j
	end,
	GetLocal = function(k, l)
		for _, m in ipairs(k.Locals) do
			if m.Name == l then
				return m
			end
		end
		if k.Parent then
			return k.Parent:GetLocal(l)
		end
	end,
	GetOldLocal = function(n, o)
		if n.oldLocalNamesMap[o] then
			return n.oldLocalNamesMap[o]
		end
		return n:GetLocal(o)
	end,
	mapLocal = function(p, q, r)
		p.oldLocalNamesMap[q] = r
	end,
	GetOldGlobal = function(s, t)
		if s.oldGlobalNamesMap[t] then
			return s.oldGlobalNamesMap[t]
		end
		return s:GetGlobal(t)
	end,
	mapGlobal = function(u, v, w)
		u.oldGlobalNamesMap[v] = w
	end,
	GetOldVariable = function(x, y)
		return x:GetOldLocal(y) or x:GetOldGlobal(y)
	end,
	RenameLocal = function(z, A, B)
		A = type(A) == "string" and A or A.Name
		local C = false
		local D = z:GetLocal(A)
		if D then
			D.Name = B
			z:mapLocal(A, D)
			C = true
		end
		if not C and z.Parent then
			z.Parent:RenameLocal(A, B)
		end
	end,
	RenameGlobal = function(E, F, G)
		F = type(F) == "string" and F or F.Name
		local H = false
		local I = E:GetGlobal(F)
		if I then
			I.Name = G
			E:mapGlobal(F, I)
			H = true
		end
		if not H and E.Parent then
			E.Parent:RenameGlobal(F, G)
		end
	end,
	RenameVariable = function(J, K, L)
		K = type(K) == "string" and K or K.Name
		if J:GetLocal(K) then
			J:RenameLocal(K, L)
		else
			J:RenameGlobal(K, L)
		end
	end,
	GetAllVariables = function(M)
		local N = M:getVars(true)
		for _, O in ipairs(M:getVars(false)) do
			N[#N + 1] = O
		end
		return N
	end,
	getVars = function(P, Q)
		local R = {}
		if Q then
			for _, S in ipairs(P.Children) do
				for _, T in ipairs(S:getVars(true)) do
					R[#R + 1] = T
				end
			end
		else
			for _, U in ipairs(P.Locals) do
				R[#R + 1] = U
			end
			for _, V in ipairs(P.Globals) do
				R[#R + 1] = V
			end
			if P.Parent then
				for _, W in ipairs(P.Parent:getVars(false)) do
					R[#R + 1] = W
				end
			end
		end
		return R
	end,
	CreateGlobal = function(X, Y)
		local Z
		Z = X:GetGlobal(Y)
		if Z then
			return Z
		end
		Z = {}
		Z.Scope = X
		Z.Name = Y
		Z.IsGlobal = true
		Z.CanRename = true
		Z.References = 1
		X:AddGlobal(Z)
		return Z
	end,
	GetGlobal = function(ab, bb)
		for _, cb in ipairs(ab.Globals) do
			if cb.Name == bb then
				return cb
			end
		end
		if ab.Parent then
			return ab.Parent:GetGlobal(bb)
		end
	end,
	GetVariable = function(db, eb)
		return db:GetLocal(eb) or db:GetGlobal(eb)
	end,
	RenameSingles = function(fb)
		for _, gb in ipairs(fb.Locals) do
			if gb.References <= 1 then
				fb:RenameLocal(gb.Name, "_")
			end
		end
	end,
	ObfuscateLocals = function(hb, ib, jb)
		ib = ib or 7
		local kb = jb or "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuioplkjhgfdsazxcvbnm1234567890"
		for _, lb in ipairs(hb.Locals) do
			if lb.References <= 1 then
				hb:RenameLocal(lb.Name, "_")
			else
				local mb = ""
				local nb = 0
				repeat
					local ob = math.random(1, #kb)
					mb = mb .. kb:sub(ob, ob)
					for _ = 1, math.random(0, nb > 5 and 30 or ib) do
						local pb = math.random(1, #kb)
						mb = mb .. kb:sub(pb, pb)
					end
					nb = nb + 1
				until not hb:GetVariable(mb)
				mb = ("."):rep(math.random(20, 50)):gsub(".", function()
					return ({"l", "I"})[math.random(1, 2)]
				end) .. "_" .. mb
				hb:RenameLocal(lb.Name, mb)
			end
		end
	end,
	MinifyVariables = function(qb)
		for _, rb in ipairs(qb.Locals) do
			qb:RenameLocal(rb.Name, (rb.References <= 1 and "_") or getNewLocal())
		end
	end
}
local function LL(a)
	local b = {}
	local c, d = pcall(function()
		local h = 1
		local i = 1
		local j = 1
		local function k()
			local p = a:sub(h, h)
			if p == "\n" then
				j = 1
				i = i + 1
			else
				j = j + 1
			end
			h = h + 1
			return p
		end
		local function l(q)
			q = q or 0
			return a:sub(h + q, h + q)
		end
		local function m(r)
			local s = l()
			for t = 1, #r do
				if s == r:sub(t, t) then
					return k()
				end
			end
		end
		local function n(u)
			return error(">> :" .. i .. ":" .. j .. ": " .. u, 0)
		end
		local function o()
			local v = h
			if l() == "[" then
				local w = 0
				local x = 1
				while l(w + 1) == "=" do
					w = w + 1
				end
				if l(w + 1) == "[" then
					for _ = 0, w + 1 do
						k()
					end
					local y = h
					while true do
						if l() == "" then
							n("Expected `]" .. ("="):rep(w) .. "]` near <eof>.", 3)
						end
						local B = true
						if l() == "]" then
							for C = 1, w do
								if l(C) ~= "=" then
									B = false
								end
							end
							if l(w + 1) ~= "]" then
								B = false
							end
						else
							if l() == "[" then
								local D = true
								for E = 1, w do
									if l(E) ~= "=" then
										D = false
										break
									end
								end
								if l(w + 1) == "[" and D then
									x = x + 1
									for _ = 1, w + 2 do
										k()
									end
								end
							end
							B = false
						end
						if B then
							x = x - 1
							if x == 0 then
								break
							else
								for _ = 1, w + 2 do
									k()
								end
							end
						else
							k()
						end
					end
					local z = a:sub(y, h - 1)
					for _ = 0, w + 1 do
						k()
					end
					local A = a:sub(v, h - 1)
					return z, A
				else
					return nil
				end
			else
				return nil
			end
		end
		while true do
			local F = {}
			local G = ""
			local H = false
			while true do
				local M = l()
				if M == "#" and l(1) == "!" and i == 1 then
					k()
					k()
					G = "#!"
					while l() ~= "\n" and l() ~= "" do
						G = G .. k()
					end
					local N = {
						Type = "Comment",
						CommentType = "Shebang",
						Data = G,
						Line = i,
						Char = j
					}
					N.Print = function()
						return "<" .. (N.Type .. (" "):rep(7 - #N.Type)) .. "  " .. (N.Data or "") .. " >"
					end
					G = ""
					F[#F + 1] = N
				end
				if M == " " or M == "\t" then
					local O = k()
					F[#F + 1] = {
						Type = "Whitespace",
						Line = i,
						Char = j,
						Data = O
					}
				elseif M == "\n" or M == "\r" then
					local P = k()
					if G ~= "" then
						local Q = {
							Type = "Comment",
							CommentType = H and "LongComment" or "Comment",
							Data = G,
							Line = i,
							Char = j
						}
						Q.Print = function()
							return "<" .. (Q.Type .. (" "):rep(7 - #Q.Type)) .. "  " .. (Q.Data or "") .. " >"
						end
						F[#F + 1] = Q
						G = ""
					end
					F[#F + 1] = {
						Type = "Whitespace",
						Line = i,
						Char = j,
						Data = P
					}
				elseif M == "-" and l(1) == "-" then
					k()
					k()
					G = G .. "--"
					local _, R = o()
					if R then
						G = G .. R
						H = true
					else
						while l() ~= "\n" and l() ~= "" do
							G = G .. k()
						end
					end
				else
					break
				end
			end
			if G ~= "" then
				local S = {
					Type = "Comment",
					CommentType = H and "LongComment" or "Comment",
					Data = G,
					Line = i,
					Char = j
				}
				S.Print = function()
					return "<" .. (S.Type .. (" "):rep(7 - #S.Type)) .. "  " .. (S.Data or "") .. " >"
				end
				F[#F + 1] = S
			end
			local I = i
			local J = j
			local K = l()
			local L = nil
			if K == "" then
				L = {
					Type = "Eof"
				}
			elseif lettersU[K] or lettersL[K] or K == "_" then
				local T = h
				repeat
					k()
					K = l()
				until not (lettersU[K] or lettersL[K] or numbers[K] or K == "_")
				local U = a:sub(T, h - 1)
				if keywords[U] then
					L = {
						Type = "Keyword",
						Data = U
					}
				else
					L = {
						Type = "Ident",
						Data = U
					}
				end
			elseif numbers[K] or l() == "." and numbers[l(1)] then
				local V = h
				if K == "0" and l(1):lower() == "x" then
					k()
					k()
					while lettersH[l()] do
						k()
					end
					if m("Pp") then
						m("+-")
						while numbers[l()] do
							k()
						end
					end
				else
					while numbers[l()] do
						k()
					end
					if m(".") then
						while numbers[l()] do
							k()
						end
					end
					if m("Ee") then
						m("+-")
						while numbers[l()] do
							k()
						end
					end
				end
				L = {
					Type = "Number",
					Data = a:sub(V, h - 1)
				}
			elseif K == "'" or K == "\"" then
				local W = h
				local X = k()
				local Y = h
				while true do
					local K = k()
					if K == "\\" then
						k()
					elseif K == X then
						break
					elseif K == "" then
						n("Unfinished string near <eof>")
					end
				end
				local Z = a:sub(Y, h - 2)
				local ab = a:sub(W, h - 1)
				L = {
					Type = "string",
					Data = ab,
					Constant = Z
				}
			elseif K == "[" then
				local bb, cb = o()
				if cb then
					L = {
						Type = "string",
						Data = cb,
						Constant = bb
					}
				else
					k()
					L = {
						Type = "Symbol",
						Data = "["
					}
				end
			elseif m(">=<") then
				if m("=") then
					L = {
						Type = "Symbol",
						Data = K .. "="
					}
				else
					L = {
						Type = "Symbol",
						Data = K
					}
				end
			elseif m("~") then
				if m("=") then
					L = {
						Type = "Symbol",
						Data = "~="
					}
				else
					n("Unexpected symbol `~` in source.", 2)
				end
			elseif m(".") then
				if m(".") then
					if m(".") then
						L = {
							Type = "Symbol",
							Data = "..."
						}
					else
						L = {
							Type = "Symbol",
							Data = ".."
						}
					end
				else
					L = {
						Type = "Symbol",
						Data = "."
					}
				end
			elseif m(":") then
				if m(":") then
					L = {
						Type = "Symbol",
						Data = "::"
					}
				else
					L = {
						Type = "Symbol",
						Data = ":"
					}
				end
			elseif operators[K] then
				k()
				L = {
					Type = "Symbol",
					Data = K
				}
			else
				local db, eb = o()
				if db then
					L = {
						Type = "string",
						Data = eb,
						Constant = db
					}
				else
					n("Unexpected Symbol `" .. K .. "` in source.", 2)
				end
			end
			L.LeadingWhite = F
			L.Line = I
			L.Char = J
			L.Print = function()
				return "<" .. (L.Type .. (" "):rep(7 - #L.Type)) .. "  " .. (L.Data or "") .. " >"
			end
			b[#b + 1] = L
			if L.Type == "Eof" then
				break
			end
		end
	end)
	if not c then
		return false, d
	end
	local e = {}
	local f = {}
	local g = 1
	function e:getp()
		return g
	end
	function e:setp(fb)
		g = fb
	end
	function e:getTokenList()
		return b
	end
	function e:Peek(gb)
		gb = gb or 0
		return b[math.min(#b, g + gb)]
	end
	function e:Get(hb)
		local ib = b[g]
		g = math.min(g + 1, #b)
		if hb then
			hb[#hb + 1] = ib
		end
		return ib
	end
	function e:Is(jb)
		return e:Peek().Type == jb
	end
	function e:Save()
		f[#f + 1] = g
	end
	function e:Commit()
		f[#f] = nil
	end
	function e:Restore()
		g = f[#f]
		f[#f] = nil
	end
	function e:ConsumeSymbol(kb, lb)
		local mb = self:Peek()
		if mb.Type == "Symbol" then
			if kb then
				if mb.Data == kb then
					self:Get(lb)
					return true
				else
					return nil
				end
			else
				self:Get(lb)
				return mb
			end
		else
			return nil
		end
	end
	function e:ConsumeKeyword(nb, ob)
		local pb = self:Peek()
		if pb.Type == "Keyword" and pb.Data == nb then
			self:Get(ob)
			return true
		else
			return nil
		end
	end
	function e:IsKeyword(qb)
		local rb = e:Peek()
		return rb.Type == "Keyword" and rb.Data == qb
	end
	function e:IsSymbol(sb)
		local tb = e:Peek()
		return tb.Type == "Symbol" and tb.Data == sb
	end
	function e:IsEof()
		return e:Peek().Type == "Eof"
	end
	return true, e
end
local function ParseLua(a)
	local b, c
	if type(a) ~= "table" then
		b, c = LL(a)
	else
		b, c = true, a
	end
	if not b then
		return false, c
	end
	local function d(v)
		local w = ">> :" .. c:Peek().Line .. ":" .. c:Peek().Char .. ": " .. v .. "\n"
		local x = 0
		if type(a) == "string" then
			for y in a:gmatch("[^\n]*\n?") do
				if y:sub(-1, -1) == "\n" then
					y = y:sub(1, -2)
				end
				x = x + 1
				if x == c:Peek().Line then
					w = w .. ">> `" .. y:gsub("\t", "\t") .. "`\n"
					for z = 1, c:Peek().Char do
						local A = y:sub(z, z)
						if A == "\t" then
							w = w .. "\t"
						else
							w = w .. " "
						end
					end
					w = w .. "   ^^^^"
					break
				end
			end
		end
		return w
	end
	local e = 0
	local f = {"_", "a", "b", "c", "d"}
	local function g(B)
		local C = Scope:new(B)
		C.RenameVars = C.ObfuscateLocals
		C.ObfuscateVariables = C.ObfuscateLocals
		C.MinifyVars = C.MinifyVariables
		C.Print = function()
			return "<Scope>"
		end
		return C
	end
	local h
	local i
	local j, k, l, m
	local function n(D, E)
		local F = g(D)
		if not c:ConsumeSymbol("(", E) then
			return false, d("`(` expected.")
		end
		local G = {}
		local H = false
		while not c:ConsumeSymbol(")", E) do
			if c:Is("Ident") then
				local K = F:CreateLocal(c:Get(E).Data)
				G[#G + 1] = K
				if not c:ConsumeSymbol(",", E) then
					if c:ConsumeSymbol(")", E) then
						break
					else
						return false, d("`)` expected.")
					end
				end
			elseif c:ConsumeSymbol("...", E) then
				H = true
				if not c:ConsumeSymbol(")", E) then
					return false, d("`...` must be the last argument of a function.")
				end
				break
			else
				return false, d("Argument name or `...` expected")
			end
		end
		local b, I = i(F)
		if not b then
			return false, I
		end
		if not c:ConsumeKeyword("end", E) then
			return false, d("`end` expected after function body")
		end
		local J = {}
		J.AstType = "Function"
		J.Scope = F
		J.Arguments = G
		J.Body = I
		J.VarArg = H
		J.Tokens = E
		return true, J
	end
	function l(L)
		local M = {}
		if c:ConsumeSymbol("(", M) then
			local b, N = h(L)
			if not b then
				return false, N
			end
			if not c:ConsumeSymbol(")", M) then
				return false, d("`)` Expected.")
			end
			if false then
				N.ParenCount = (N.ParenCount or 0) + 1
				return true, N
			else
				local O = {}
				O.AstType = "Parentheses"
				O.Inner = N
				O.Tokens = M
				return true, O
			end
		elseif c:Is("Ident") then
			local P = c:Get(M)
			local Q = L:GetLocal(P.Data)
			if not Q then
				Q = L:GetGlobal(P.Data)
				if not Q then
					Q = L:CreateGlobal(P.Data)
				else
					Q.References = Q.References + 1
				end
			else
				Q.References = Q.References + 1
			end
			local R = {}
			R.AstType = "VarExpr"
			R.Name = P.Data
			R.Variable = Q
			R.Tokens = M
			return true, R
		else
			return false, d("primary expression expected")
		end
	end
	function m(S, T)
		local b, U = l(S)
		if not b then
			return false, U
		end
		while true do
			local V = {}
			if c:IsSymbol(".") or c:IsSymbol(":") then
				local W = c:Get(V).Data
				if not c:Is("Ident") then
					return false, d("<Ident> expected.")
				end
				local X = c:Get(V)
				local Y = {}
				Y.AstType = "MemberExpr"
				Y.Base = U
				Y.Indexer = W
				Y.Ident = X
				Y.Tokens = V
				U = Y
			elseif not T and c:ConsumeSymbol("[", V) then
				local b, Z = h(S)
				if not b then
					return false, Z
				end
				if not c:ConsumeSymbol("]", V) then
					return false, d("`]` expected.")
				end
				local ab = {}
				ab.AstType = "IndexExpr"
				ab.Base = U
				ab.Index = Z
				ab.Tokens = V
				U = ab
			elseif not T and c:ConsumeSymbol("(", V) then
				local bb = {}
				while not c:ConsumeSymbol(")", V) do
					local b, db = h(S)
					if not b then
						return false, db
					end
					bb[#bb + 1] = db
					if not c:ConsumeSymbol(",", V) then
						if c:ConsumeSymbol(")", V) then
							break
						else
							return false, d("`)` Expected.")
						end
					end
				end
				local cb = {}
				cb.AstType = "CallExpr"
				cb.Base = U
				cb.Arguments = bb
				cb.Tokens = V
				U = cb
			elseif not T and c:Is("string") then
				local eb = {}
				eb.AstType = "StringCallExpr"
				eb.Base = U
				eb.Arguments = {c:Get(V)}
				eb.Tokens = V
				U = eb
			elseif not T and c:IsSymbol("{") then
				local b, fb = j(S)
				if not b then
					return false, fb
				end
				local gb = {}
				gb.AstType = "TableCallExpr"
				gb.Base = U
				gb.Arguments = {fb}
				gb.Tokens = V
				U = gb
			else
				break
			end
		end
		return true, U
	end
	function j(hb)
		local ib = {}
		if c:Is("Number") then
			local jb = {}
			jb.AstType = "NumberExpr"
			jb.Value = c:Get(ib)
			jb.Tokens = ib
			return true, jb
		elseif c:Is("string") then
			local kb = {}
			kb.AstType = "StringExpr"
			kb.Value = c:Get(ib)
			kb.Tokens = ib
			return true, kb
		elseif c:ConsumeKeyword("nil", ib) then
			local lb = {}
			lb.AstType = "NilExpr"
			lb.Tokens = ib
			return true, lb
		elseif c:IsKeyword("false") or c:IsKeyword("true") then
			local mb = {}
			mb.AstType = "BooleanExpr"
			mb.Value = c:Get(ib).Data == "true"
			mb.Tokens = ib
			return true, mb
		elseif c:ConsumeSymbol("...", ib) then
			local nb = {}
			nb.AstType = "DotsExpr"
			nb.Tokens = ib
			return true, nb
		elseif c:ConsumeSymbol("{", ib) then
			local ob = {}
			ob.AstType = "ConstructorExpr"
			ob.EntryList = {}
			while true do
				if c:IsSymbol("[", ib) then
					c:Get(ib)
					local b, pb = h(hb)
					if not b then
						return false, d("Key Expression Expected")
					end
					if not c:ConsumeSymbol("]", ib) then
						return false, d("`]` Expected")
					end
					if not c:ConsumeSymbol("=", ib) then
						return false, d("`=` Expected")
					end
					local b, qb = h(hb)
					if not b then
						return false, d("Value Expression Expected")
					end
					ob.EntryList[#ob.EntryList + 1] = {
						Type = "Key",
						Key = pb,
						Value = qb
					}
				elseif c:Is("Ident") then
					local rb = c:Peek(1)
					if rb.Type == "Symbol" and rb.Data == "=" then
						local sb = c:Get(ib)
						if not c:ConsumeSymbol("=", ib) then
							return false, d("`=` Expected")
						end
						local b, tb = h(hb)
						if not b then
							return false, d("Value Expression Expected")
						end
						ob.EntryList[#ob.EntryList + 1] = {
							Type = "KeyString",
							Key = sb.Data,
							Value = tb
						}
					else
						local b, ub = h(hb)
						if not b then
							return false, d("Value Exected")
						end
						ob.EntryList[#ob.EntryList + 1] = {
							Type = "Value",
							Value = ub
						}
					end
				elseif c:ConsumeSymbol("}", ib) then
					break
				else
					local b, vb = h(hb)
					ob.EntryList[#ob.EntryList + 1] = {
						Type = "Value",
						Value = vb
					}
					if not b then
						return false, d("Value Expected")
					end
				end
				if c:ConsumeSymbol(";", ib) or c:ConsumeSymbol(",", ib) then
				elseif c:ConsumeSymbol("}", ib) then
					break
				else
					return false, d("`}` or table entry Expected")
				end
			end
			ob.Tokens = ib
			return true, ob
		elseif c:ConsumeKeyword("function", ib) then
			local b, wb = n(hb, ib)
			if not b then
				return false, wb
			end
			wb.IsLocal = true
			return true, wb
		else
			return m(hb)
		end
	end
	local o = toDictionary("-", "not", "#")
	local p = 8
	local q = {
		["+"] = {6, 6},
		["-"] = {6, 6},
		["%"] = {7, 7},
		["/"] = {7, 7},
		["*"] = {7, 7},
		["^"] = {10, 9},
		[".."] = {5, 4},
		["=="] = {3, 3},
		["<"] = {3, 3},
		["<="] = {3, 3},
		["~="] = {3, 3},
		[">"] = {3, 3},
		[">="] = {3, 3},
		["and"] = {2, 2},
		["or"] = {1, 1}
	}
	function k(xb, yb)
		local b, zb
		if o[c:Peek().Data] then
			local Ab = {}
			local Bb = c:Get(Ab).Data
			b, zb = k(xb, p)
			if not b then
				return false, zb
			end
			local Cb = {}
			Cb.AstType = "UnopExpr"
			Cb.Rhs = zb
			Cb.Op = Bb
			Cb.OperatorPrecedence = p
			Cb.Tokens = Ab
			zb = Cb
		else
			b, zb = j(xb)
			if not b then
				return false, zb
			end
		end
		while true do
			local Db = q[c:Peek().Data]
			if Db and Db[1] > yb then
				local Eb = {}
				local Fb = c:Get(Eb).Data
				local b, Gb = k(xb, Db[2])
				if not b then
					return false, Gb
				end
				local Hb = {}
				Hb.AstType = "BinopExpr"
				Hb.Lhs = zb
				Hb.Op = Fb
				Hb.OperatorPrecedence = Db[1]
				Hb.Rhs = Gb
				Hb.Tokens = Eb
				zb = Hb
			else
				break
			end
		end
		return true, zb
	end
	h = function(Ib)
		return k(Ib, 0)
	end
	local function r(Jb)
		local Kb = nil
		local Lb = {}
		if c:ConsumeKeyword("if", Lb) then
			local Mb = {}
			Mb.AstType = "IfStatement"
			Mb.Clauses = {}
			repeat
				local b, Nb = h(Jb)
				if not b then
					return false, Nb
				end
				if not c:ConsumeKeyword("then", Lb) then
					return false, d("`then` expected.")
				end
				local b, Ob = i(Jb)
				if not b then
					return false, Ob
				end
				Mb.Clauses[#Mb.Clauses + 1] = {
					Condition = Nb,
					Body = Ob
				}
			until not c:ConsumeKeyword("elseif", Lb)
			if c:ConsumeKeyword("else", Lb) then
				local b, Pb = i(Jb)
				if not b then
					return false, Pb
				end
				Mb.Clauses[#Mb.Clauses + 1] = {
					Body = Pb
				}
			end
			if not c:ConsumeKeyword("end", Lb) then
				return false, d("`end` expected.")
			end
			Mb.Tokens = Lb
			Kb = Mb
		elseif c:ConsumeKeyword("while", Lb) then
			local Qb = {}
			Qb.AstType = "WhileStatement"
			local b, Rb = h(Jb)
			if not b then
				return false, Rb
			end
			if not c:ConsumeKeyword("do", Lb) then
				return false, d("`do` expected.")
			end
			local b, Sb = i(Jb)
			if not b then
				return false, Sb
			end
			if not c:ConsumeKeyword("end", Lb) then
				return false, d("`end` expected.")
			end
			Qb.Condition = Rb
			Qb.Body = Sb
			Qb.Tokens = Lb
			Kb = Qb
		elseif c:ConsumeKeyword("do", Lb) then
			local b, Tb = i(Jb)
			if not b then
				return false, Tb
			end
			if not c:ConsumeKeyword("end", Lb) then
				return false, d("`end` expected.")
			end
			local Ub = {}
			Ub.AstType = "DoStatement"
			Ub.Body = Tb
			Ub.Tokens = Lb
			Kb = Ub
		elseif c:ConsumeKeyword("for", Lb) then
			if not c:Is("Ident") then
				return false, d("<ident> expected.")
			end
			local Vb = c:Get(Lb)
			if c:ConsumeSymbol("=", Lb) then
				local Wb = g(Jb)
				local Xb = Wb:CreateLocal(Vb.Data)
				local b, Yb = h(Jb)
				if not b then
					return false, Yb
				end
				if not c:ConsumeSymbol(",", Lb) then
					return false, d("`,` Expected")
				end
				local b, Zb = h(Jb)
				if not b then
					return false, Zb
				end
				local b, ac
				if c:ConsumeSymbol(",", Lb) then
					b, ac = h(Jb)
					if not b then
						return false, ac
					end
				end
				if not c:ConsumeKeyword("do", Lb) then
					return false, d("`do` expected")
				end
				local b, bc = i(Wb)
				if not b then
					return false, bc
				end
				if not c:ConsumeKeyword("end", Lb) then
					return false, d("`end` expected")
				end
				local cc = {}
				cc.AstType = "NumericForStatement"
				cc.Scope = Wb
				cc.Variable = Xb
				cc.Start = Yb
				cc.End = Zb
				cc.Step = ac
				cc.Body = bc
				cc.Tokens = Lb
				Kb = cc
			else
				local dc = g(Jb)
				local ec = {dc:CreateLocal(Vb.Data)}
				while c:ConsumeSymbol(",", Lb) do
					if not c:Is("Ident") then
						return false, d("for variable expected.")
					end
					ec[#ec + 1] = dc:CreateLocal(c:Get(Lb).Data)
				end
				if not c:ConsumeKeyword("in", Lb) then
					return false, d("`in` expected.")
				end
				local fc = {}
				local b, gc = h(Jb)
				if not b then
					return false, gc
				end
				fc[#fc + 1] = gc
				while c:ConsumeSymbol(",", Lb) do
					local b, jc = h(Jb)
					if not b then
						return false, jc
					end
					fc[#fc + 1] = jc
				end
				if not c:ConsumeKeyword("do", Lb) then
					return false, d("`do` expected.")
				end
				local b, hc = i(dc)
				if not b then
					return false, hc
				end
				if not c:ConsumeKeyword("end", Lb) then
					return false, d("`end` expected.")
				end
				local ic = {}
				ic.AstType = "GenericForStatement"
				ic.Scope = dc
				ic.VariableList = ec
				ic.Generators = fc
				ic.Body = hc
				ic.Tokens = Lb
				Kb = ic
			end
		elseif c:ConsumeKeyword("repeat", Lb) then
			local b, kc = i(Jb)
			if not b then
				return false, kc
			end
			if not c:ConsumeKeyword("until", Lb) then
				return false, d("`until` expected.")
			end
			local b, lc = h(kc.Scope)
			if not b then
				return false, lc
			end
			local mc = {}
			mc.AstType = "RepeatStatement"
			mc.Condition = lc
			mc.Body = kc
			mc.Tokens = Lb
			Kb = mc
		elseif c:ConsumeKeyword("function", Lb) then
			if not c:Is("Ident") then
				return false, d("Function name expected")
			end
			local b, nc = m(Jb, true)
			if not b then
				return false, nc
			end
			local b, oc = n(Jb, Lb)
			if not b then
				return false, oc
			end
			oc.IsLocal = false
			oc.Name = nc
			Kb = oc
		elseif c:ConsumeKeyword("local", Lb) then
			if c:Is("Ident") then
				local pc = {c:Get(Lb).Data}
				while c:ConsumeSymbol(",", Lb) do
					if not c:Is("Ident") then
						return false, d("local var name expected")
					end
					pc[#pc + 1] = c:Get(Lb).Data
				end
				local qc = {}
				if c:ConsumeSymbol("=", Lb) then
					repeat
						local b, sc = h(Jb)
						if not b then
							return false, sc
						end
						qc[#qc + 1] = sc
					until not c:ConsumeSymbol(",", Lb)
				end
				for tc, uc in ipairs(pc) do
					pc[tc] = Jb:CreateLocal(uc)
				end
				local rc = {}
				rc.AstType = "LocalStatement"
				rc.LocalList = pc
				rc.InitList = qc
				rc.Tokens = Lb
				Kb = rc
			elseif c:ConsumeKeyword("function", Lb) then
				if not c:Is("Ident") then
					return false, d("Function name expected")
				end
				local vc = c:Get(Lb).Data
				local wc = Jb:CreateLocal(vc)
				local b, xc = n(Jb, Lb)
				if not b then
					return false, xc
				end
				xc.Name = wc
				xc.IsLocal = true
				Kb = xc
			else
				return false, d("local var or function def expected")
			end
		elseif c:ConsumeSymbol("::", Lb) then
			if not c:Is("Ident") then
				return false, d("Label name expected")
			end
			local yc = c:Get(Lb).Data
			if not c:ConsumeSymbol("::", Lb) then
				return false, d("`::` expected")
			end
			local zc = {}
			zc.AstType = "LabelStatement"
			zc.Label = yc
			zc.Tokens = Lb
			Kb = zc
		elseif c:ConsumeKeyword("return", Lb) then
			local Ac = {}
			if not c:IsKeyword("end") then
				local b, Cc = h(Jb)
				if b then
					Ac[1] = Cc
					while c:ConsumeSymbol(",", Lb) do
						local b, Dc = h(Jb)
						if not b then
							return false, Dc
						end
						Ac[#Ac + 1] = Dc
					end
				end
			end
			local Bc = {}
			Bc.AstType = "ReturnStatement"
			Bc.Arguments = Ac
			Bc.Tokens = Lb
			Kb = Bc
		elseif c:ConsumeKeyword("break", Lb) then
			local Ec = {}
			Ec.AstType = "BreakStatement"
			Ec.Tokens = Lb
			Kb = Ec
		elseif c:ConsumeKeyword("continue", Lb) then
			local Fc = {}
			Fc.AstType = "ContinueStatement"
			Fc.Tokens = Lb
			Kb = Fc
		else
			local b, Gc = m(Jb)
			if not b then
				return false, Gc
			end
			if c:IsSymbol(",") or c:IsSymbol("=") or (c:Peek(1).Data == "=" and operatorsLuau[c:Peek().Data]) then
				if (Gc.ParenCount or 0) > 0 then
					return false, d("can not assign to parenthesized expression, is not an lvalue")
				end
				local Hc = {Gc}
				while c:ConsumeSymbol(",", Lb) do
					local b, Lc = m(Jb)
					if not b then
						return false, Lc
					end
					Hc[#Hc + 1] = Lc
				end
				local Kc = {}
				if operatorsLuau[c:Peek().Data] then
					for Op in pairs(operatorsLuau) do
						if c:IsSymbol(Op) then
							c:ConsumeSymbol(Op, Lb)
							Kc.LuaUAssign = " " .. Op .. "= "
							break
						end
					end
				end
				if not c:ConsumeSymbol("=", Lb) then
					return false, d("`=` Expected.")
				end
				local Ic = {}
				local b, Jc = h(Jb)
				if not b then
					return false, Jc
				end
				Ic[1] = Jc
				while c:ConsumeSymbol(",", Lb) do
					local b, Mc = h(Jb)
					if not b then
						return false, Mc
					end
					Ic[#Ic + 1] = Mc
				end
				Kc.AstType = "AssignmentStatement"
				Kc.Lhs = Hc
				Kc.Rhs = Ic
				Kc.Tokens = Lb
				Kb = Kc
			elseif Gc.AstType == "CallExpr" or Gc.AstType == "TableCallExpr" or Gc.AstType == "StringCallExpr" then
				local Nc = {}
				Nc.AstType = "CallStatement"
				Nc.Expression = Gc
				Nc.Tokens = Lb
				Kb = Nc
			else
				return false, d("Assignment Statement Expected")
			end
		end
		if c:IsSymbol(";") then
			Kb.Semicolon = c:Get(Kb.Tokens)
		end
		return true, Kb
	end
	local s = toDictionary("end", "else", "elseif", "until")
	i = function(Oc)
		local Pc = {}
		Pc.Scope = g(Oc)
		Pc.AstType = "Statlist"
		Pc.Body = {}
		Pc.Tokens = {}
		while not s[c:Peek().Data] and not c:IsEof() do
			local b, Qc = r(Pc.Scope)
			if not b then
				return false, Qc
			end
			Pc.Body[#Pc.Body + 1] = Qc
		end
		if c:IsEof() then
			local Rc = {}
			Rc.AstType = "Eof"
			Rc.Tokens = {c:Get()}
			Pc.Body[#Pc.Body + 1] = Rc
		end
		return true, Pc
	end
	local function t()
		local Sc = g()
		return i(Sc)
	end
	local b, u = t()
	return b, u
end
local Legends = {
	["\\"] = "\\\\",
	["\a"] = "\\a",
	["\b"] = "\\b",
	["\f"] = "\\f",
	["\n"] = "\\n",
	["\r"] = "\\r",
	["\t"] = "\\t",
	["\v"] = "\\v",
	["\""] = "\\\""
}
local function fixnum(a, b)
	a = tostring(tonumber(a))
	if a:sub(1, 2) == "0." then
		a = a:sub(2)
	elseif a:match("%d+") == a then
		if b then
			a = tonumber(a)
			a = a <= 1 and a or ("0x%x"):format(a)
		else
			local c = a:match("000+$")
			a = c and (a:sub(1, #a - #c) .. "e" .. #c) or a
		end
	end
	return a
end
local function fixstr(a)
	return "\"" .. (loadstring("return " .. a)():gsub(".", function(b)
		return "\\" .. b:byte()
	end)) .. "\""
end
local lastType = false
local function formatSource(a, b, c, d, e, f)
	lastType = false
	localCount = 0
	local l, n, j, i = 0, "\n", false, false
	local function k(p, o, q)
		q = q or ""
		local s, r = p:sub(-1, -1), o:sub(1, 1)
		if lettersU[s] or lettersL[s] or s == "_" then
			if not (lettersU[r] or lettersL[r] or r == "_" or numbers[r]) then
				return p .. o
			elseif r == "(" then
				return p .. q .. o
			else
				return p .. q .. o
			end
		elseif numbers[s] then
			if r == "(" then
				return p .. o
			else
				return p .. q .. o
			end
		elseif s == "" then
			return p .. o
		else
			if r == "(" then
				return p .. q .. o
			else
				return p .. o
			end
		end
	end
	local g = {}
	local function h(t, e)
		if b and not g[t] then
			g[t] = true
			if e then
				t.Scope:ObfuscateVariables()
			else
				t.Scope:MinifyVars()
			end
		end
	end
	local function m(u)
		lastType = u.AstType ~= "Parentheses" and u.AstType or lastType
		if u.Scope then
			if b then
				h(u, e)
			else
				u.Scope:RenameSingles()
			end
		end
		local v = ("("):rep(u.ParenCount or 0)
		if u.AstType == "VarExpr" then
			if u.Variable then
				v = v .. u.Variable.Name
			else
				v = v .. u.Name
			end
		elseif u.AstType == "NumberExpr" then
			v = v .. fixnum(u.Value.Data, d)
		elseif u.AstType == "StringExpr" then
			v = v .. fixstr(u.Value.Data)
		elseif u.AstType == "BooleanExpr" then
			v = v .. tostring(u.Value)
		elseif u.AstType == "NilExpr" then
			v = k(v, "nil")
		elseif u.AstType == "BinopExpr" then
			v = k(v, m(u.Lhs)) .. " "
			if f == "js" then
				v = k(v, ({
					["and"] = "&&",
					["or"] = "||",
					["=="] = "===",
					["~="] = "!==",
					[".."] = "+"
				})[u.Op] or u.Op) .. " "
			else
				v = k(v, u.Op) .. " "
			end
			v = k(v, m(u.Rhs))
		elseif u.AstType == "UnopExpr" then
			if f == "js" then
				if u.Op ~= "#" and u.Op == "not" then
					v = k(v, "!")
				end
				v = k(v, m(u.Rhs)) .. (u.Op == "#" and ".length" or "")
			else
				v = k(v, u.Op) .. (#u.Op ~= 1 and " " or "")
				v = k(v, m(u.Rhs))
			end
		elseif u.AstType == "DotsExpr" then
			v = v .. "..."
		elseif u.AstType == "CallExpr" then
			v = v .. m(u.Base)
			if c and f ~= "js" and #u.Arguments == 1 and (u.Arguments[1].AstType == "StringExpr" or u.Arguments[1].AstType == "ConstructorExpr") then
				v = v .. m(u.Arguments[1])
			else
				v = v .. "("
				for x, w in ipairs(u.Arguments) do
					v = v .. m(w)
					if x ~= #u.Arguments then
						v = v .. ", "
					end
				end
				v = v .. ")"
			end
		elseif u.AstType == "TableCallExpr" then
			if c and f ~= "js" then
				v = v .. m(u.Base) .. m(u.Arguments[1])
			else
				v = v .. m(u.Base) .. "("
				v = v .. m(u.Arguments[1]) .. ")"
			end
		elseif u.AstType == "StringCallExpr" then
			if c and f ~= "js" then
				v = v .. m(u.Base) .. fixstr(u.Arguments[1].Data)
			else
				v = v .. m(u.Base) .. "("
				v = v .. fixstr(u.Arguments[1].Data) .. ")"
			end
		elseif u.AstType == "IndexExpr" then
			v = v .. m(u.Base) .. "[" .. m(u.Index) .. "]"
		elseif u.AstType == "MemberExpr" then
			v = v .. m(u.Base) .. u.Indexer .. u.Ident.Data
		elseif u.AstType == "Function" then
			v = v .. "function("
			if #u.Arguments > 0 then
				for i = #u.Arguments, 1, -1 do
					if u.Arguments[i].References <= 1 then
						table.remove(u.Arguments, i)
					else
						break
					end
				end
				for z, y in ipairs(u.Arguments) do
					v = v .. y.Name
					if z ~= #u.Arguments then
						v = v .. ", "
					elseif u.VarArg then
						if f == "js" then
							v = v .. ", ...VARARGS"
						else
							v = v .. ", ..."
						end
					end
				end
			elseif u.VarArg then
				if f == "js" then
					v = v .. ", ...VARARGS"
				else
					v = v .. "..."
				end
			end
			if f == "js" then
				v = v .. ") {" .. n
			else
				v = v .. ")" .. n
			end
			l = l + 1
			v = k(v, j(u.Body))
			l = l - 1
			v = k(v, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
		elseif u.AstType == "ConstructorExpr" then
			local C = (function()
				for _, E in ipairs(u.EntryList) do
					if E.Type == "Key" or E.Type == "KeyString" then
						return false
					end
				end
				return true
			end)()
			if f == "js" and C then
				v = v .. "["
			else
				v = v .. "{"
			end
			local A, B, D = false, false, false
			for H, G in ipairs(u.EntryList) do
				A, B = G.Type == "Key" or G.Type == "KeyString", A
				l = l + (C and 0 or 1)
				if A or D then
					D = A
					if not B then
						v = v .. "\n"
					end
					v = v .. ("\t"):rep(l)
				end
				if G.Type == "Key" then
					v = v .. "[" .. m(G.Key) .. "]" .. (f == "js" and ": " or " = ") .. m(G.Value)
				elseif G.Type == "Value" then
					v = v .. m(G.Value)
				elseif G.Type == "KeyString" then
					v = v .. G.Key .. (f == "js" and ": " or " = ") .. m(G.Value)
				end
				if H ~= #u.EntryList then
					v = v .. ","
					if not A then
						v = v .. " "
					end
				end
				if A then
					v = v .. "\n"
				end
				l = l - (C and 0 or 1)
			end
			if #u.EntryList > 0 and A then
				v = v .. ("\t"):rep(l)
			end
			if f == "js" and C then
				v = v .. "]"
			else
				v = v .. "}"
			end
		elseif u.AstType == "Parentheses" then
			local I = 0
			local k = false
			repeat
				k = (k or u).Inner
				I = I + 1
			until k.AstType ~= "Parentheses"
			if ({
				["VarExpr"] = true,
				["MemberExpr"] = true,
				["IndexExpr"] = true
			})[k.AstType] or (({
				["NumberExpr"] = true,
				["BooleanExpr"] = true,
				["NilExpr"] = true,
				["DotsExpr"] = true,
				["StringExpr"] = true,
				["UnopExpr"] = true,
				["ConstructorExpr"] = true,
				["Function"] = true
			})[k.AstType] and not ({
				["MemberExpr"] = true,
				["IndexExpr"] = true,
				["CallStatement"] = true,
				["CallExpr"] = true,
				["TableCallExpr"] = true,
				["StringCallExpr"] = true
			})[lastType]) then
				v = v .. m(k)
			else
				v = v .. "(" .. m(k) .. ")"
			end
		end
		v = v .. (")"):rep(u.ParenCount or 0)
		return v
	end
	function i(J)
		lastType = J.AstType ~= "Parentheses" and J.AstType or lastType
		if J.Scope then
			if b then
				h(J, e)
			else
				J.Scope:RenameSingles()
			end
		end
		local K = ""
		if J.AstType == "AssignmentStatement" then
			K = ("\t"):rep(l)
			for M, L in ipairs(J.Lhs) do
				K = K .. m(L)
				if M ~= #J.Lhs then
					K = K .. ", "
				end
			end
			if #J.Rhs > 0 then
				if J.LuaUAssign then
					K = K .. J.LuaUAssign
				else
					K = K .. " = "
				end
				for O, N in ipairs(J.Rhs) do
					K = K .. m(N)
					if O ~= #J.Rhs then
						K = K .. ", "
					end
				end
			end
		elseif J.AstType == "CallStatement" then
			K = ("\t"):rep(l) .. m(J.Expression)
		elseif J.AstType == "LocalStatement" then
			K = ("\t"):rep(l) .. K .. (f == "js" and "let " or "local ")
			for Q, P in ipairs(J.LocalList) do
				K = K .. P.Name
				if Q ~= #J.LocalList then
					K = K .. ", "
				end
			end
			if #J.InitList > 0 then
				K = K .. " = "
				for S, R in ipairs(J.InitList) do
					K = K .. m(R)
					if S ~= #J.InitList then
						K = K .. ", "
					end
				end
			end
		elseif J.AstType == "IfStatement" then
			K = ("\t"):rep(l) .. k("if " .. (f == "js" and "(" or ""), m(J.Clauses[1].Condition))
			K = k(K, (f == "js" and ") {" or " then")) .. n
			l = l + 1
			K = k(K, j(J.Clauses[1].Body))
			l = l - 1
			for U, T in ipairs(J.Clauses) do
				if U >= 2 then
					if T.Condition then
						K = k(K, ("\t"):rep(l) .. (f == "js" and "} else if (" or "elseif "))
						K = k(K, m(T.Condition))
						K = k(K, (f == "js" and ") {" or " then")) .. n
					else
						K = k(K, ("\t"):rep(l) .. (f == "js" and "} else {" or "else")) .. n
					end
					l = l + 1
					K = k(K, j(T.Body))
					l = l - 1
				end
			end
			K = k(K, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
		elseif J.AstType == "WhileStatement" then
			K = ("\t"):rep(l) .. k("while " .. (f == "js" and "(" or ""), m(J.Condition))
			K = k(K, (f == "js" and ") {" or " do")) .. n
			l = l + 1
			K = k(K, j(J.Body))
			l = l - 1
			K = k(K, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
		elseif J.AstType == "DoStatement" then
			K = ("\t"):rep(l) .. k(K, (f == "js" and "if (true) {" or "do")) .. n
			l = l + 1
			K = k(K, j(J.Body))
			l = l - 1
			K = k(K, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
		elseif J.AstType == "ReturnStatement" then
			K = ("\t"):rep(l) .. "return"
			if #J.Arguments > 0 then
				K = K .. " "
			end
			for W, V in ipairs(J.Arguments) do
				K = k(K, m(V))
				if W ~= #J.Arguments then
					K = K .. ", "
				end
			end
		elseif J.AstType == "BreakStatement" then
			K = ("\t"):rep(l) .. "break"
		elseif J.AstType == "ContinueStatement" then
			K = ("\t"):rep(l) .. "continue"
		elseif J.AstType == "RepeatStatement" then
			K = ("\t"):rep(l) .. (f == "js" and "do {" or "repeat") .. n
			l = l + 1
			K = k(K, j(J.Body))
			l = l - 1
			K = k(K, ("\t"):rep(l) .. (f == "js" and "} while (!(" or "until "))
			K = k(K, m(J.Condition) .. (f == "js" and "))" or ""))
		elseif J.AstType == "Function" then
			if J.IsLocal and f == "js" then
				K = f == "js" and "let " .. J.Name.Name .. " = function"
			elseif J.IsLocal then
				K = "local "
				K = k(K, "function ")
			end
			if not J.IsLocal then
				K = k(K, "function ")
			end
			K = ("\t"):rep(l) .. K
			if J.IsLocal and f ~= "js" then
				K = K .. J.Name.Name
			else
				K = K .. m(J.Name)
			end
			K = K .. "("
			if #J.Arguments > 0 then
				for Y, X in ipairs(J.Arguments) do
					K = K .. X.Name
					if Y ~= #J.Arguments then
						K = K .. ", "
					elseif J.VarArg then
						if f == "js" then
							K = K .. ", ...VARARGS"
						else
							K = K .. ", ..."
						end
					end
				end
			elseif J.VarArg then
				if f == "js" then
					K = K .. ", ...VARARGS"
				else
					K = K .. "..."
				end
			end
			K = K .. ")" .. (f == "js" and " {" or "") .. n
			l = l + 1
			K = k(K, j(J.Body))
			l = l - 1
			K = k(K, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
		elseif J.AstType == "GenericForStatement" then
			if f == "js" then
				K = ("\t"):rep(l) .. "for (let "
				for i, Z in ipairs(J.VariableList) do
					J.VariableList[i] = Z.Name
				end
				K = K .. table.concat(J.VariableList, ", ") .. " of "
				for ab, bb in ipairs(J.Generators) do
					K = k(K, m(bb))
					if ab ~= #J.Generators then
						K = k(K, ", ")
					end
				end
				K = k(K, ") {") .. n
				l = l + 1
				K = k(K, j(J.Body))
				l = l - 1
				K = k(K, ("\t"):rep(l) .. "}")
			else
				K = ("\t"):rep(l) .. "for "
				if #J.VariableList > 1 then
					for i = #J.VariableList, 2, -1 do
						if J.VariableList[i].References <= 1 then
							table.remove(J.VariableList, i)
						else
							break
						end
					end
				end
				for cb, db in ipairs(J.VariableList) do
					K = K .. db.Name
					if cb ~= #J.VariableList then
						K = K .. ", "
					end
				end
				K = K .. " in "
				for eb, fb in ipairs(J.Generators) do
					K = k(K, m(fb))
					if eb ~= #J.Generators then
						K = k(K, ", ")
					end
				end
				K = k(K, " do") .. n
				l = l + 1
				K = k(K, j(J.Body))
				l = l - 1
				K = k(K, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
			end
		elseif J.AstType == "NumericForStatement" then
			if f == "js" then
				K = ("\t"):rep(l) .. "for ("
				local gb = J.Variable.Name
				local hb, ib = m(J.Start), m(J.End)
				local jb, kb = tonumber(hb), tonumber(ib)
				local lb = jb and kb and jb >= kb
				K = K .. "let " .. gb .. " = " .. hb .. "; " .. gb .. " " .. (lb and ">=" or "<=") .. " " .. ib
				if J.Step then
					K = K .. "; " .. gb .. " = " .. gb .. " + " .. m(J.Step)
				else
					K = K .. "; " .. gb .. "++"
				end
				K = k(K, ") {") .. n
				l = l + 1
				K = k(K, j(J.Body))
				l = l - 1
				K = k(K, ("\t"):rep(l) .. "}")
			else
				K = ("\t"):rep(l) .. "for "
				K = K .. J.Variable.Name .. " = "
				K = K .. m(J.Start) .. ", " .. m(J.End)
				if J.Step then
					K = K .. ", " .. m(J.Step)
				end
				K = k(K, " do") .. n
				l = l + 1
				K = k(K, j(J.Body))
				l = l - 1
				K = k(K, ("\t"):rep(l) .. (f == "js" and "}" or "end"))
			end
		elseif J.AstType == "LabelStatement" then
			K = ("\t"):rep(l) .. "::" .. J.Label .. "::" .. n
		elseif J.AstType == "GotoStatement" then
			K = ("\t"):rep(l) .. "goto " .. J.Label .. n
		elseif J.AstType == "Comment" then
			print(J.CommentType, J.Data)
			if J.CommentType == "Shebang" then
				K = ("\t"):rep(l) .. J.Data
			elseif J.CommentType == "Comment" then
				K = ("\t"):rep(l) .. J.Data
			elseif J.CommentType == "LongComment" then
				K = ("\t"):rep(l) .. J.Data
			end
		elseif J.AstType ~= "Eof" then
			print("Unknown AST Type: ", J.AstType)
		end
		return K
	end
	function j(mb)
		local nb = ""
		h(mb, e)
		for _, pb in pairs(mb.Body) do
			nb = k(nb, i(pb) .. n)
		end
		return nb
	end
	return j(a):gsub(",%.%.%.", ", ..."):gsub(", \n", ",\n"):gsub("for _[, _]+ in", "for _ in"):match("^%s*(.-)%s*$")
end
local function decode(a)
	return (a:gsub("\"[\\%d]+\"", function(b)
		return "\"" .. loadstring("return " .. b)():gsub("[%z%c\"\\]", function(c)
			return Legends[c] or "\\" .. c:byte()
		end) .. "\""
	end))
end
local function _beautify(a, b, c)
	local d, e = ParseLua(a)
	if not d then
		return a, e
	end
	local f = formatSource(e, c, false, b, false)
	if not b then
		f = decode(f)
	end
	return f
end
local function _minify(a, b)
	local c, d = ParseLua(a)
	if not c then
		return a, d
	end
	local e = formatSource(d, true, true, b, false)
	for _ = 1, 2 do
		e = e:gsub("%s+", " "):gsub("([%w_]) (%p)", function(f, g)
			if g ~= "_" then
				return f .. g
			end
		end):gsub("(%p) (%p)", function(h, i)
			if h ~= "_" and i ~= "_" then
				return h .. i
			end
		end):gsub("(%p) ([%w_])", function(j, k)
			if j ~= "_" then
				return j .. k
			end
		end)
	end
	if not b then
		e = decode(e)
	end
	return e
end
local function _uglify(a, b, c)
	local f, d = ParseLua(a)
	if not f then
		return a, d
	end
	local e = formatSource(d, true, true, b, true)
	if not c then
		for _ = 1, 2 do
			e = e:gsub("%s+", " "):gsub("([%w_]) (%p)", function(h, i)
				if i ~= "_" then
					return h .. i
				end
			end):gsub("(%p) (%p)", function(j, k)
				if j ~= "_" and k ~= "_" then
					return j .. k
				end
			end):gsub("(%p) ([%w_])", function(l, m)
				if l ~= "_" then
					return l .. m
				end
			end)
		end
	end
	if not b then
		e = decode(e)
	end
	return e
end
local function _tojavascript(a, b)
	local c, d = ParseLua(a)
	if not c then
		return a, d
	end
	local e = formatSource(d, false, false, b, false, "js")
	if b then
		e = e:gsub("\"[\\%d]+\"", function(f)
			return "\"" .. loadstring("return " .. f)():gsub(".", function(g)
				return ("\\x%02x"):format(g:byte())
			end) .. "\""
		end)
	else
		e = decode(e)
	end
	return e
end
return {
	beautify = _beautify,
	minify = _minify,
	uglify = _uglify,
	tojavascript = _tojavascript
}
