local function Dictionary(...)
	local t = {}
	for _, v in ipairs({...}) do
		t[v] = true
	end
	return t
end
local LowerLetters = Dictionary("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
local UpperLetters = Dictionary("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
local HexLetters = Dictionary("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "a", "B", "b", "C", "c", "D", "d", "E", "e", "F", "f")
local Numbers = Dictionary("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
local Operators = Dictionary("+", "-", "*", "/", "^", "%", ",", "{", "}", "[", "]", "(", ")", ";", "#")
local OperatorsU = Dictionary("+", "-", "*", "/", "%", "^", ".."), "Pepsi was here"
local Keywords = Dictionary("and", "break", "continue", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while")
local Locals = 0
local GenerateLocal = (function()
	local a, b = {}, {}
	for d = 97, 122 do
		d = string.char(d)
		a[#a + 1], b[#b + 1] = d, d
	end
	for e = 65, 90 do
		e = string.char(e)
		a[#a + 1], b[#b + 1] = e, e
	end
	for f = 0, 9 do
		a[#a + 1] = f
	end
	a[#a + 1] = "_"
	local function c(g)
		local h = ""
		local i = g % #b
		g = (g - i) / #b
		h = h .. b[i + 1]
		while g > 0 do
			local i = g % #a
			g = (g - i) / #a
			h = h .. a[i + 1]
		end
		return h
	end
	return function(tab)
		local j = ""
		repeat
			local k = Locals
			Locals = Locals + 1
			j = c(k)
		until not Keywords[j] or (function()
			for _, v in ipairs(tab) do
				if j == v.Name then
					return false
				end
			end
			return true
		end)()
		return j
	end
end)()
local Scope = {
	new = function(a, b)
		local c = {
			Parent = b,
			Locals = {},
			Globals = {},
			OldLocalNamesMap = {},
			OldGlobalNamesMap = {},
			Children = {}
		}
		if b then
			b.Children[#b.Children + 1] = c
		end
		return setmetatable(c, {
			__index = a
		})
	end,
	AddLocal = function(d, e)
		d.Locals[#d.Locals + 1] = e
	end,
	AddGlobal = function(f, g)
		f.Globals[#f.Globals + 1] = g
	end,
	CreateLocal = function(h, i)
		local j = h:GetLocal(i)
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
		if n.OldLocalNamesMap[o] then
			return n.OldLocalNamesMap[o]
		end
		return n:GetLocal(o)
	end,
	MapLocal = function(p, q, r)
		p.OldLocalNamesMap[q] = r
	end,
	GetOldGlobal = function(s, t)
		if s.OldGlobalNamesMap[t] then
			return s.OldGlobalNamesMap[t]
		end
		return s:GetGlobal(t)
	end,
	MapGlobal = function(u, v, w)
		u.OldGlobalNamesMap[v] = w
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
			z:MapLocal(A, D)
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
			E:MapGlobal(F, I)
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
		local N = M:GetVars(true)
		for _, O in ipairs(M:GetVars(false)) do
			N[#N + 1] = O
		end
		return N
	end,
	GetVars = function(P, Q)
		local R = {}
		if Q then
			for _, S in ipairs(P.Children) do
				for _, T in ipairs(S:GetVars(true)) do
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
				for _, W in ipairs(P.Parent:GetVars(false)) do
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
			qb:RenameLocal(rb.Name, (rb.References <= 1 and "_") or GenerateLocal(qb.Locals))
		end
	end
}
local function Tokenize(a)
	local b = {}
	local c, d = pcall(function()
		local h, i, j = 1, 1, 1
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
			elseif UpperLetters[K] or LowerLetters[K] or K == "_" then
				local T = h
				repeat
					k()
					K = l()
				until not (UpperLetters[K] or LowerLetters[K] or Numbers[K] or K == "_")
				local U = a:sub(T, h - 1)
				if Keywords[U] then
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
			elseif Numbers[K] or l() == "." and Numbers[l(1)] then
				local V = h
				if K == "0" and l(1):lower() == "x" then
					k()
					k()
					while HexLetters[l()] do
						k()
					end
					if m("Pp") then
						m("+-")
						while Numbers[l()] do
							k()
						end
					end
				else
					while Numbers[l()] do
						k()
					end
					if m(".") then
						while Numbers[l()] do
							k()
						end
					end
					if m("Ee") then
						m("+-")
						while Numbers[l()] do
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
			elseif Operators[K] then
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
local function Parse(a)
	local b, c
	if type(a) ~= "table" then
		b, c = Tokenize(a)
	else
		b, c = true, a
	end
	if not b then
		return false, c
	end
	local function d(t)
		local u = ">> :" .. c:Peek().Line .. ":" .. c:Peek().Char .. ": " .. t .. "\n"
		local v = 0
		if type(a) == "string" then
			for w in a:gmatch("[^\n]*\n?") do
				if w:sub(-1, -1) == "\n" then
					w = w:sub(1, -2)
				end
				v = v + 1
				if v == c:Peek().Line then
					u = u .. ">> `" .. w:gsub("\t", "\t") .. "`\n"
					for x = 1, c:Peek().Char do
						local y = w:sub(x, x)
						if y == "\t" then
							u = u .. "\t"
						else
							u = u .. " "
						end
					end
					u = u .. "   ^^^^"
					break
				end
			end
		end
		return u
	end
	local _ = 0
	local _ = {"_", "a", "b", "c", "d"}
	local function e(z)
		local A = Scope:new(z)
		A.RenameVars = A.ObfuscateLocals
		A.ObfuscateVariables = A.ObfuscateLocals
		A.MinifyVars = A.MinifyVariables
		A.Print = function()
			return "<Scope>"
		end
		return A
	end
	local f
	local g
	local h, i, j, k
	local function l(B, C)
		local D = e(B)
		if not c:ConsumeSymbol("(", C) then
			return false, d("`(` expected.")
		end
		local E = {}
		local F = false
		while not c:ConsumeSymbol(")", C) do
			if c:Is("Ident") then
				local I = D:CreateLocal(c:Get(C).Data)
				E[#E + 1] = I
				if not c:ConsumeSymbol(",", C) then
					if c:ConsumeSymbol(")", C) then
						break
					else
						return false, d("`)` expected.")
					end
				end
			elseif c:ConsumeSymbol("...", C) then
				F = true
				if not c:ConsumeSymbol(")", C) then
					return false, d("`...` must be the last argument of a function.")
				end
				break
			else
				return false, d("Argument name or `...` expected")
			end
		end
		local b, G = g(D)
		if not b then
			return false, G
		end
		if not c:ConsumeKeyword("end", C) then
			return false, d("`end` expected after function body")
		end
		local H = {}
		H.AstType = "Function"
		H.Scope = D
		H.Arguments = E
		H.Body = G
		H.VarArg = F
		H.Tokens = C
		return true, H
	end
	function j(J)
		local K = {}
		if c:ConsumeSymbol("(", K) then
			local b, L = f(J)
			if not b then
				return false, L
			end
			if not c:ConsumeSymbol(")", K) then
				return false, d("`)` Expected.")
			end
			if false then
				L.ParenCount = (L.ParenCount or 0) + 1
				return true, L
			else
				local M = {}
				M.AstType = "Parentheses"
				M.Inner = L
				M.Tokens = K
				return true, M
			end
		elseif c:Is("Ident") then
			local N = c:Get(K)
			local O = J:GetLocal(N.Data)
			if not O then
				O = J:GetGlobal(N.Data)
				if not O then
					O = J:CreateGlobal(N.Data)
				else
					O.References = O.References + 1
				end
			else
				O.References = O.References + 1
			end
			local P = {}
			P.AstType = "VarExpr"
			P.Name = N.Data
			P.Variable = O
			P.Tokens = K
			return true, P
		else
			return false, d("primary expression expected")
		end
	end
	function k(Q, R)
		local b, S = j(Q)
		if not b then
			return false, S
		end
		while true do
			local T = {}
			if c:IsSymbol(".") or c:IsSymbol(":") then
				local U = c:Get(T).Data
				if not c:Is("Ident") then
					return false, d("<Ident> expected.")
				end
				local V = c:Get(T)
				local W = {}
				W.AstType = "MemberExpr"
				W.Base = S
				W.Indexer = U
				W.Ident = V
				W.Tokens = T
				S = W
			elseif not R and c:ConsumeSymbol("[", T) then
				local b, X = f(Q)
				if not b then
					return false, X
				end
				if not c:ConsumeSymbol("]", T) then
					return false, d("`]` expected.")
				end
				local Y = {}
				Y.AstType = "IndexExpr"
				Y.Base = S
				Y.Index = X
				Y.Tokens = T
				S = Y
			elseif not R and c:ConsumeSymbol("(", T) then
				local Z = {}
				while not c:ConsumeSymbol(")", T) do
					local b, bb = f(Q)
					if not b then
						return false, bb
					end
					Z[#Z + 1] = bb
					if not c:ConsumeSymbol(",", T) then
						if c:ConsumeSymbol(")", T) then
							break
						else
							return false, d("`)` Expected.")
						end
					end
				end
				local ab = {}
				ab.AstType = "CallExpr"
				ab.Base = S
				ab.Arguments = Z
				ab.Tokens = T
				S = ab
			elseif not R and c:Is("string") then
				local cb = {}
				cb.AstType = "StringCallExpr"
				cb.Base = S
				cb.Arguments = {c:Get(T)}
				cb.Tokens = T
				S = cb
			elseif not R and c:IsSymbol("{") then
				local b, db = h(Q)
				if not b then
					return false, db
				end
				local eb = {}
				eb.AstType = "TableCallExpr"
				eb.Base = S
				eb.Arguments = {db}
				eb.Tokens = T
				S = eb
			else
				break
			end
		end
		return true, S
	end
	function h(fb)
		local gb = {}
		if c:Is("Number") then
			local hb = {}
			hb.AstType = "NumberExpr"
			hb.Value = c:Get(gb)
			hb.Tokens = gb
			return true, hb
		elseif c:Is("string") then
			local ib = {}
			ib.AstType = "StringExpr"
			ib.Value = c:Get(gb)
			ib.Tokens = gb
			return true, ib
		elseif c:ConsumeKeyword("nil", gb) then
			local jb = {}
			jb.AstType = "NilExpr"
			jb.Tokens = gb
			return true, jb
		elseif c:IsKeyword("false") or c:IsKeyword("true") then
			local kb = {}
			kb.AstType = "BooleanExpr"
			kb.Value = c:Get(gb).Data == "true"
			kb.Tokens = gb
			return true, kb
		elseif c:ConsumeSymbol("...", gb) then
			local lb = {}
			lb.AstType = "DotsExpr"
			lb.Tokens = gb
			return true, lb
		elseif c:ConsumeSymbol("{", gb) then
			local mb = {}
			mb.AstType = "ConstructorExpr"
			mb.EntryList = {}
			while true do
				if c:IsSymbol("[", gb) then
					c:Get(gb)
					local b, nb = f(fb)
					if not b then
						return false, d("Key Expression Expected")
					end
					if not c:ConsumeSymbol("]", gb) then
						return false, d("`]` Expected")
					end
					if not c:ConsumeSymbol("=", gb) then
						return false, d("`=` Expected")
					end
					local b, ob = f(fb)
					if not b then
						return false, d("Value Expression Expected")
					end
					mb.EntryList[#mb.EntryList + 1] = {
						Type = "Key",
						Key = nb,
						Value = ob
					}
				elseif c:Is("Ident") then
					local pb = c:Peek(1)
					if pb.Type == "Symbol" and pb.Data == "=" then
						local qb = c:Get(gb)
						if not c:ConsumeSymbol("=", gb) then
							return false, d("`=` Expected")
						end
						local b, rb = f(fb)
						if not b then
							return false, d("Value Expression Expected")
						end
						mb.EntryList[#mb.EntryList + 1] = {
							Type = "KeyString",
							Key = qb.Data,
							Value = rb
						}
					else
						local b, sb = f(fb)
						if not b then
							return false, d("Value Exected")
						end
						mb.EntryList[#mb.EntryList + 1] = {
							Type = "Value",
							Value = sb
						}
					end
				elseif c:ConsumeSymbol("}", gb) then
					break
				else
					local b, tb = f(fb)
					mb.EntryList[#mb.EntryList + 1] = {
						Type = "Value",
						Value = tb
					}
					if not b then
						return false, d("Value Expected")
					end
				end
				if c:ConsumeSymbol(";", gb) or c:ConsumeSymbol(",", gb) then
				elseif c:ConsumeSymbol("}", gb) then
					break
				else
					return false, d("`}` or table entry Expected")
				end
			end
			mb.Tokens = gb
			return true, mb
		elseif c:ConsumeKeyword("function", gb) then
			local b, ub = l(fb, gb)
			if not b then
				return false, ub
			end
			ub.IsLocal = true
			return true, ub
		else
			return k(fb)
		end
	end
	local m = {
		["-"] = true,
		["not"] = true,
		["#"] = true
	}
	local n = 8
	local o = {
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
	function i(vb, wb)
		local b, xb
		if m[c:Peek().Data] then
			local yb = {}
			local zb = c:Get(yb).Data
			b, xb = i(vb, n)
			if not b then
				return false, xb
			end
			local Ab = {}
			Ab.AstType = "UnopExpr"
			Ab.Rhs = xb
			Ab.Op = zb
			Ab.OperatorPrecedence = n
			Ab.Tokens = yb
			xb = Ab
		else
			b, xb = h(vb)
			if not b then
				return false, xb
			end
		end
		while true do
			local Bb = o[c:Peek().Data]
			if Bb and Bb[1] > wb then
				local Cb = {}
				local Db = c:Get(Cb).Data
				local b, Eb = i(vb, Bb[2])
				if not b then
					return false, Eb
				end
				local Fb = {}
				Fb.AstType = "BinopExpr"
				Fb.Lhs = xb
				Fb.Op = Db
				Fb.OperatorPrecedence = Bb[1]
				Fb.Rhs = Eb
				Fb.Tokens = Cb
				xb = Fb
			else
				break
			end
		end
		return true, xb
	end
	f = function(Gb)
		return i(Gb, 0)
	end
	local function p(Hb)
		local Ib = nil
		local Jb = {}
		if c:ConsumeKeyword("if", Jb) then
			local Kb = {}
			Kb.AstType = "IfStatement"
			Kb.Clauses = {}
			repeat
				local b, Lb = f(Hb)
				if not b then
					return false, Lb
				end
				if not c:ConsumeKeyword("then", Jb) then
					return false, d("`then` expected.")
				end
				local b, Mb = g(Hb)
				if not b then
					return false, Mb
				end
				Kb.Clauses[#Kb.Clauses + 1] = {
					Condition = Lb,
					Body = Mb
				}
			until not c:ConsumeKeyword("elseif", Jb)
			if c:ConsumeKeyword("else", Jb) then
				local b, Nb = g(Hb)
				if not b then
					return false, Nb
				end
				Kb.Clauses[#Kb.Clauses + 1] = {
					Body = Nb
				}
			end
			if not c:ConsumeKeyword("end", Jb) then
				return false, d("`end` expected.")
			end
			Kb.Tokens = Jb
			Ib = Kb
		elseif c:ConsumeKeyword("while", Jb) then
			local Ob = {}
			Ob.AstType = "WhileStatement"
			local b, Pb = f(Hb)
			if not b then
				return false, Pb
			end
			if not c:ConsumeKeyword("do", Jb) then
				return false, d("`do` expected.")
			end
			local b, Qb = g(Hb)
			if not b then
				return false, Qb
			end
			if not c:ConsumeKeyword("end", Jb) then
				return false, d("`end` expected.")
			end
			Ob.Condition = Pb
			Ob.Body = Qb
			Ob.Tokens = Jb
			Ib = Ob
		elseif c:ConsumeKeyword("do", Jb) then
			local b, Rb = g(Hb)
			if not b then
				return false, Rb
			end
			if not c:ConsumeKeyword("end", Jb) then
				return false, d("`end` expected.")
			end
			local Sb = {}
			Sb.AstType = "DoStatement"
			Sb.Body = Rb
			Sb.Tokens = Jb
			Ib = Sb
		elseif c:ConsumeKeyword("for", Jb) then
			if not c:Is("Ident") then
				return false, d("<ident> expected.")
			end
			local Tb = c:Get(Jb)
			if c:ConsumeSymbol("=", Jb) then
				local Ub = e(Hb)
				local Vb = Ub:CreateLocal(Tb.Data)
				local b, Wb = f(Hb)
				if not b then
					return false, Wb
				end
				if not c:ConsumeSymbol(",", Jb) then
					return false, d("`,` Expected")
				end
				local b, Xb = f(Hb)
				if not b then
					return false, Xb
				end
				local b, Yb
				if c:ConsumeSymbol(",", Jb) then
					b, Yb = f(Hb)
					if not b then
						return false, Yb
					end
				end
				if not c:ConsumeKeyword("do", Jb) then
					return false, d("`do` expected")
				end
				local b, Zb = g(Ub)
				if not b then
					return false, Zb
				end
				if not c:ConsumeKeyword("end", Jb) then
					return false, d("`end` expected")
				end
				local ac = {}
				ac.AstType = "NumericForStatement"
				ac.Scope = Ub
				ac.Variable = Vb
				ac.Start = Wb
				ac.End = Xb
				ac.Step = Yb
				ac.Body = Zb
				ac.Tokens = Jb
				Ib = ac
			else
				local bc = e(Hb)
				local cc = {bc:CreateLocal(Tb.Data)}
				while c:ConsumeSymbol(",", Jb) do
					if not c:Is("Ident") then
						return false, d("for variable expected.")
					end
					cc[#cc + 1] = bc:CreateLocal(c:Get(Jb).Data)
				end
				if not c:ConsumeKeyword("in", Jb) then
					return false, d("`in` expected.")
				end
				local dc = {}
				local b, ec = f(Hb)
				if not b then
					return false, ec
				end
				dc[#dc + 1] = ec
				while c:ConsumeSymbol(",", Jb) do
					local b, hc = f(Hb)
					if not b then
						return false, hc
					end
					dc[#dc + 1] = hc
				end
				if not c:ConsumeKeyword("do", Jb) then
					return false, d("`do` expected.")
				end
				local b, fc = g(bc)
				if not b then
					return false, fc
				end
				if not c:ConsumeKeyword("end", Jb) then
					return false, d("`end` expected.")
				end
				local gc = {}
				gc.AstType = "GenericForStatement"
				gc.Scope = bc
				gc.VariableList = cc
				gc.Generators = dc
				gc.Body = fc
				gc.Tokens = Jb
				Ib = gc
			end
		elseif c:ConsumeKeyword("repeat", Jb) then
			local b, ic = g(Hb)
			if not b then
				return false, ic
			end
			if not c:ConsumeKeyword("until", Jb) then
				return false, d("`until` expected.")
			end
			local b, jc = f(ic.Scope)
			if not b then
				return false, jc
			end
			local kc = {}
			kc.AstType = "RepeatStatement"
			kc.Condition = jc
			kc.Body = ic
			kc.Tokens = Jb
			Ib = kc
		elseif c:ConsumeKeyword("function", Jb) then
			if not c:Is("Ident") then
				return false, d("Function name expected")
			end
			local b, lc = k(Hb, true)
			if not b then
				return false, lc
			end
			local b, mc = l(Hb, Jb)
			if not b then
				return false, mc
			end
			mc.IsLocal = false
			mc.Name = lc
			Ib = mc
		elseif c:ConsumeKeyword("local", Jb) then
			if c:Is("Ident") then
				local nc = {c:Get(Jb).Data}
				while c:ConsumeSymbol(",", Jb) do
					if not c:Is("Ident") then
						return false, d("local var name expected")
					end
					nc[#nc + 1] = c:Get(Jb).Data
				end
				local oc = {}
				if c:ConsumeSymbol("=", Jb) then
					repeat
						local b, qc = f(Hb)
						if not b then
							return false, qc
						end
						oc[#oc + 1] = qc
					until not c:ConsumeSymbol(",", Jb)
				end
				for rc, sc in ipairs(nc) do
					nc[rc] = Hb:CreateLocal(sc)
				end
				local pc = {}
				pc.AstType = "LocalStatement"
				pc.LocalList = nc
				pc.InitList = oc
				pc.Tokens = Jb
				Ib = pc
			elseif c:ConsumeKeyword("function", Jb) then
				if not c:Is("Ident") then
					return false, d("Function name expected")
				end
				local tc = c:Get(Jb).Data
				local uc = Hb:CreateLocal(tc)
				local b, vc = l(Hb, Jb)
				if not b then
					return false, vc
				end
				vc.Name = uc
				vc.IsLocal = true
				Ib = vc
			else
				return false, d("local var or function def expected")
			end
		elseif c:ConsumeSymbol("::", Jb) then
			if not c:Is("Ident") then
				return false, d("Label name expected")
			end
			local wc = c:Get(Jb).Data
			if not c:ConsumeSymbol("::", Jb) then
				return false, d("`::` expected")
			end
			local xc = {}
			xc.AstType = "LabelStatement"
			xc.Label = wc
			xc.Tokens = Jb
			Ib = xc
		elseif c:ConsumeKeyword("return", Jb) then
			local yc = {}
			if not c:IsKeyword("end") then
				local b, Ac = f(Hb)
				if b then
					yc[1] = Ac
					while c:ConsumeSymbol(",", Jb) do
						local b, Bc = f(Hb)
						if not b then
							return false, Bc
						end
						yc[#yc + 1] = Bc
					end
				end
			end
			local zc = {}
			zc.AstType = "ReturnStatement"
			zc.Arguments = yc
			zc.Tokens = Jb
			Ib = zc
		elseif c:ConsumeKeyword("break", Jb) then
			local Cc = {}
			Cc.AstType = "BreakStatement"
			Cc.Tokens = Jb
			Ib = Cc
		elseif c:ConsumeKeyword("continue", Jb) then
			local Dc = {}
			Dc.AstType = "ContinueStatement"
			Dc.Tokens = Jb
			Ib = Dc
		else
			local b, Ec = k(Hb)
			if not b then
				return false, Ec
			end
			if c:IsSymbol(",") or c:IsSymbol("=") or (c:Peek(1).Data == "=" and OperatorsU[c:Peek().Data]) then
				if (Ec.ParenCount or 0) > 0 then
					return false, d("can not assign to parenthesized expression, is not an lvalue")
				end
				local Fc = {Ec}
				while c:ConsumeSymbol(",", Jb) do
					local b, Jc = k(Hb)
					if not b then
						return false, Jc
					end
					Fc[#Fc + 1] = Jc
				end
				local Gc = {}
				if OperatorsU[c:Peek().Data] then
					for Kc in pairs(OperatorsU) do
						if c:IsSymbol(Kc) then
							c:ConsumeSymbol(Kc, Jb)
							Gc.LuaUAssign = " " .. Kc .. "= "
							break
						end
					end
				end
				if not c:ConsumeSymbol("=", Jb) then
					return false, d("`=` Expected.")
				end
				local Hc = {}
				local b, Ic = f(Hb)
				if not b then
					return false, Ic
				end
				Hc[1] = Ic
				while c:ConsumeSymbol(",", Jb) do
					local b, Lc = f(Hb)
					if not b then
						return false, Lc
					end
					Hc[#Hc + 1] = Lc
				end
				Gc.AstType = "AssignmentStatement"
				Gc.Lhs = Fc
				Gc.Rhs = Hc
				Gc.Tokens = Jb
				Ib = Gc
			elseif Ec.AstType == "CallExpr" or Ec.AstType == "TableCallExpr" or Ec.AstType == "StringCallExpr" then
				local Mc = {}
				Mc.AstType = "CallStatement"
				Mc.Expression = Ec
				Mc.Tokens = Jb
				Ib = Mc
			else
				return false, d("Assignment Statement Expected")
			end
		end
		if c:IsSymbol(";") then
			Ib.Semicolon = c:Get(Ib.Tokens)
		end
		return true, Ib
	end
	local q = {
		["end"] = true,
		["else"] = true,
		["elseif"] = true,
		["until"] = true
	}
	g = function(Nc)
		local Oc = {}
		Oc.Scope = e(Nc)
		Oc.AstType = "Statlist"
		Oc.Body = {}
		Oc.Tokens = {}
		while not q[c:Peek().Data] and not c:IsEof() do
			local b, Pc = p(Oc.Scope)
			if not b then
				return false, Pc
			end
			Oc.Body[#Oc.Body + 1] = Pc
		end
		if c:IsEof() then
			local Qc = {}
			Qc.AstType = "Eof"
			Qc.Tokens = {c:Get()}
			Oc.Body[#Oc.Body + 1] = Qc
		end
		return true, Oc
	end
	local function r()
		local Rc = e()
		return g(Rc)
	end
	local b, s = r()
	return b, s
end
local function ParseString(a)
	return "\"" .. (loadstring("return " .. a)():gsub(".", function(b)
		return "\\" .. b:byte()
	end)) .. "\""
end
local lastType = false
local function GenerateSource(a, b, c, d, e, f)
	lastType = false
	Locals = 0
	local l, n, j, i = 0, "\n", false, false
	local function k(p, o, q)
		q = q or ""
		local s, r = p:sub(-1, -1), o:sub(1, 1)
		if UpperLetters[s] or LowerLetters[s] or s == "_" then
			if not (UpperLetters[r] or LowerLetters[r] or r == "_" or Numbers[r]) then
				return p .. o
			elseif r == "(" then
				return p .. q .. o
			else
				return p .. q .. o
			end
		elseif Numbers[s] then
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
			local NUMBER = tostring(tonumber(u.Value.Data))
			if NUMBER:sub(1, 2) == "0." then
				NUMBER = NUMBER:sub(2)
			elseif NUMBER:match("%d+") == NUMBER then
				if d then
					NUMBER = tonumber(NUMBER)
					NUMBER = NUMBER <= 1 and NUMBER or ("0x%x"):format(NUMBER)
				else
					local c = NUMBER:match("000+$")
					NUMBER = c and (NUMBER:sub(1, #NUMBER - #c) .. "e" .. #c) or NUMBER
				end
			end
			v = v .. NUMBER
		elseif u.AstType == "StringExpr" then
			v = v .. ParseString(u.Value.Data)
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
				v = v .. m(u.Base) .. ParseString(u.Arguments[1].Data)
			else
				v = v .. m(u.Base) .. "("
				v = v .. ParseString(u.Arguments[1].Data) .. ")"
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
local function DecryptString(a)
	return (a:gsub("\"[\\%d]+\"", function(b)
		return "\"" .. loadstring("return " .. b)():gsub("[%z%c\"\\]", function(c)
			return ({
				["\\"] = "\\\\",
				["\a"] = "\\a",
				["\b"] = "\\b",
				["\f"] = "\\f",
				["\n"] = "\\n",
				["\r"] = "\\r",
				["\t"] = "\\t",
				["\v"] = "\\v",
				["\""] = "\\\""
			})[c] or "\\" .. c:byte()
		end) .. "\""
	end))
end
local function _beautify(a, b, c)
	local d, e = Parse(a)
	if not d then
		return a, e
	end
	local f = GenerateSource(e, c, false, b, false)
	if not b then
		f = DecryptString(f)
	end
	return f
end
local function _minify(a, b)
	local c, d = Parse(a)
	if not c then
		return a, d
	end
	local e = GenerateSource(d, true, true, b, false)
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
		e = DecryptString(e)
	end
	return e
end
local function _uglify(a, b, c)
	local f, d = Parse(a)
	if not f then
		return a, d
	end
	local e = GenerateSource(d, true, true, b, true)
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
		e = DecryptString(e)
	end
	return e
end
local function _tojavascript(a, b)
	local c, d = Parse(a)
	if not c then
		return a, d
	end
	local e = GenerateSource(d, false, false, b, false, "js")
	if b then
		e = e:gsub("\"[\\%d]+\"", function(f)
			return "\"" .. loadstring("return " .. f)():gsub(".", function(g)
				return ("\\x%02x"):format(g:byte())
			end) .. "\""
		end)
	else
		e = DecryptString(e)
	end
	return e
end
return {
	beautify = _beautify,
	minify = _minify,
	uglify = _uglify,
	tojavascript = _tojavascript
}
