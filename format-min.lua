local function a(...)local v={}for _,w in next,{...}do v[w]=true end return v end local b=a("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")local c=a("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")local d=a("0","1","2","3","4","5","6","7","8","9","A","a","B","b","C","c","D","d","E","e","F","f")local e=a("0","1","2","3","4","5","6","7","8","9")local f=a("+","-","*","/","^","%",",","{","}","[","]","(",")",";","#")local g=a("+","-","*","/","%","^",".."),"Pepsi was here"local h=a("and","break","continue","do","else","elseif","end","false","for","function","if","in","local","nil","not","or","repeat","return","then","true","until","while")local i=0 local j=(function()local x,y={},{}for A=97,122 do A=string.char(A)x[#x+1],y[#y+1]=A,A end for B=65,90 do B=string.char(B)x[#x+1],y[#y+1]=B,B end for C=0,9 do x[#x+1]=C end x[#x+1]="_"local function z(D)local E=""local F=D%#y D=(D-F)/#y E=E..y[F+1]while D>0 do local F=D%#x D=(D-F)/#x E=E..x[F+1]end return E end return function(G)local H=""repeat local I=i i=i+1 H=z(I)until not h[H]and(function()for _,J in next,G do if H==J.Name then return false end end return true end)()return H end end)()local k={new=function(K,L)local M={Parent=L,Locals={},Globals={},OldLocalNamesMap={},OldGlobalNamesMap={},Children={}}if L then L.Children[#L.Children+1]=M end return setmetatable(M,{__index=K})end,AddLocal=function(N,O)N.Locals[#N.Locals+1]=O end,AddGlobal=function(P,Q)P.Globals[#P.Globals+1]=Q end,CreateLocal=function(R,S)local T=R:GetLocal(S)if T then return T end T={}T.Scope=R T.Name=S T.IsGlobal=false T.CanRename=true T.References=1 R:AddLocal(T)return T end,GetLocal=function(U,V)for _,W in next,U.Locals do if W.Name==V then return W end end if U.Parent then return U.Parent:GetLocal(V)end end,GetOldLocal=function(X,Y)if X.OldLocalNamesMap[Y]then return X.OldLocalNamesMap[Y]end return X:GetLocal(Y)end,MapLocal=function(Z,ab,bb)Z.OldLocalNamesMap[ab]=bb end,GetOldGlobal=function(cb,db)if cb.OldGlobalNamesMap[db]then return cb.OldGlobalNamesMap[db]end return cb:GetGlobal(db)end,MapGlobal=function(eb,fb,gb)eb.OldGlobalNamesMap[fb]=gb end,GetOldVariable=function(hb,ib)return hb:GetOldLocal(ib)or hb:GetOldGlobal(ib)end,RenameLocal=function(jb,kb,lb)kb=type(kb)=="string"and kb or kb.Name local mb=false local nb=jb:GetLocal(kb)if nb then nb.Name=lb jb:MapLocal(kb,nb)mb=true end if not mb and jb.Parent then jb.Parent:RenameLocal(kb,lb)end end,RenameGlobal=function(ob,pb,qb)pb=type(pb)=="string"and pb or pb.Name local rb=false local sb=ob:GetGlobal(pb)if sb then sb.Name=qb ob:MapGlobal(pb,sb)rb=true end if not rb and ob.Parent then ob.Parent:RenameGlobal(pb,qb)end end,RenameVariable=function(tb,ub,vb)ub=type(ub)=="string"and ub or ub.Name if tb:GetLocal(ub)then tb:RenameLocal(ub,vb)else tb:RenameGlobal(ub,vb)end end,GetAllVariables=function(wb)local xb=wb:GetVars(true)for _,yb in next,wb:GetVars(false)do xb[#xb+1]=yb end return xb end,GetVars=function(zb,Ab)local Bb={}if Ab then for _,Cb in next,zb.Children do for _,Db in next,Cb:GetVars(true)do Bb[#Bb+1]=Db end end else for _,Eb in next,zb.Locals do Bb[#Bb+1]=Eb end for _,Fb in next,zb.Globals do Bb[#Bb+1]=Fb end if zb.Parent then for _,Gb in next,zb.Parent:GetVars(false)do Bb[#Bb+1]=Gb end end end return Bb end,CreateGlobal=function(Hb,Ib)local Jb Jb=Hb:GetGlobal(Ib)if Jb then return Jb end Jb={}Jb.Scope=Hb Jb.Name=Ib Jb.IsGlobal=true Jb.CanRename=true Jb.References=1 Hb:AddGlobal(Jb)return Jb end,GetGlobal=function(Kb,Lb)for _,Mb in next,Kb.Globals do if Mb.Name==Lb then return Mb end end if Kb.Parent then return Kb.Parent:GetGlobal(Lb)end end,GetVariable=function(Nb,Ob)return Nb:GetLocal(Ob)or Nb:GetGlobal(Ob)end,RenameSingles=function(Pb)for _,Qb in next,Pb.Locals do if Qb.References<=1 then Pb:RenameLocal(Qb.Name,"_")end end end,ObfuscateLocals=function(Rb,Sb,Tb)Sb=Sb or 7 local Ub=Tb or"QWERTYUIOPASDFGHJKLZXCVBNMqwertyuioplkjhgfdsazxcvbnm1234567890"for _,Vb in next,Rb.Locals do if Vb.References<=1 then Rb:RenameLocal(Vb.Name,"_")else local Wb=""local Xb=0 repeat local Yb=math.random(#Ub)Wb=Wb..Ub:sub(Yb,Yb)for _ =1,math.random(0,Xb>5 and 30 or Sb)do local Zb=math.random(#Ub)Wb=Wb..Ub:sub(Zb,Zb)end Xb=Xb+1 until not Rb:GetVariable(Wb)Wb=("."):rep(math.random(20,50)):gsub(".",function()return({"l","I"})[math.random(2)]end).."_"..Wb Rb:RenameLocal(Vb.Name,Wb)end end end,MinifyVariables=function(ac)for _,bc in next,ac.Locals do ac:RenameLocal(bc.Name,(bc.References<=1 and"_")or j(ac.Locals))end end}local function l(cc)local dc={}local ec,fc=pcall(function()local jc,kc,lc=1,1,1 local function mc()local rc=cc:sub(jc,jc)if rc=="\n"then lc=1 kc=kc+1 else lc=lc+1 end jc=jc+1 return rc end local function nc(sc)sc=sc or 0 return cc:sub(jc+sc,jc+sc)end local function oc(tc)local uc=nc()for vc=1,#tc do if uc==tc:sub(vc,vc)then return mc()end end end local function pc(wc)return error(">> :"..kc..":"..lc..": "..wc,0)end local function qc()local xc=jc if nc()=="["then local yc=0 local zc=1 while nc(yc+1)=="="do yc=yc+1 end if nc(yc+1)=="["then for _ =0,yc+1 do mc()end local Ac=jc while true do if nc()==""then pc("Expected `]"..("="):rep(yc).."]` near <eof>.",3)end local Dc=true if nc()=="]"then for Ec=1,yc do if nc(Ec)~="="then Dc=false end end if nc(yc+1)~="]"then Dc=false end else if nc()=="["then local Fc=true for Gc=1,yc do if nc(Gc)~="="then Fc=false break end end if nc(yc+1)=="["and Fc then zc=zc+1 for _ =1,yc+2 do mc()end end end Dc=false end if Dc then zc=zc-1 if zc==0 then break else for _ =1,yc+2 do mc()end end else mc()end end local Bc=cc:sub(Ac,jc-1)for _ =0,yc+1 do mc()end local Cc=cc:sub(xc,jc-1)return Bc,Cc else return nil end else return nil end end while true do local Hc={}local Ic=""local Jc=false while true do local Oc=nc()if Oc=="#"and nc(1)=="!"and kc==1 then mc()mc()Ic="#!"while nc()~="\n"and nc()~=""do Ic=Ic..mc()end local Pc={Type="Comment",CommentType="Shebang",Data=Ic,Line=kc,Char=lc}Pc.Print=function()return"<"..(Pc.Type..(" "):rep(7-#Pc.Type)).."  "..(Pc.Data or"").." >"end Ic=""Hc[#Hc+1]=Pc end if Oc==" "or Oc=="\t"then local Qc=mc()Hc[#Hc+1]={Type="Whitespace",Line=kc,Char=lc,Data=Qc}elseif Oc=="\n"or Oc=="\r"then local Rc=mc()if Ic~=""then local Sc={Type="Comment",CommentType=Jc and"LongComment"or"Comment",Data=Ic,Line=kc,Char=lc}Sc.Print=function()return"<"..(Sc.Type..(" "):rep(7-#Sc.Type)).."  "..(Sc.Data or"").." >"end Hc[#Hc+1]=Sc Ic=""end Hc[#Hc+1]={Type="Whitespace",Line=kc,Char=lc,Data=Rc}elseif Oc=="-"and nc(1)=="-"then mc()mc()Ic=Ic.."--"local _,Tc=qc()if Tc then Ic=Ic..Tc Jc=true else while nc()~="\n"and nc()~=""do Ic=Ic..mc()end end else break end end if Ic~=""then local Uc={Type="Comment",CommentType=Jc and"LongComment"or"Comment",Data=Ic,Line=kc,Char=lc}Uc.Print=function()return"<"..(Uc.Type..(" "):rep(7-#Uc.Type)).."  "..(Uc.Data or"").." >"end Hc[#Hc+1]=Uc end local Kc=kc local Lc=lc local Mc=nc()local Nc=nil if Mc==""then Nc={Type="Eof"}elseif c[Mc]or b[Mc]or Mc=="_"then local Vc=jc repeat mc()Mc=nc()until not(c[Mc]or b[Mc]or e[Mc]or Mc=="_")local Wc=cc:sub(Vc,jc-1)if h[Wc]then Nc={Type="Keyword",Data=Wc}else Nc={Type="Ident",Data=Wc}end elseif e[Mc]or nc()=="."and e[nc(1)]then local Xc=jc if Mc=="0"and nc(1):lower()=="x"then mc()mc()while d[nc()]do mc()end if oc"Pp"then oc"+-"while e[nc()]do mc()end end else while e[nc()]do mc()end if oc"."then while e[nc()]do mc()end end if oc"Ee"then oc"+-"while e[nc()]do mc()end end end Nc={Type="Number",Data=cc:sub(Xc,jc-1)}elseif Mc=="'"or Mc=="\""then local Yc=jc local Zc=mc()local ad=jc while true do local Mc=mc()if Mc=="\\"then mc()elseif Mc==Zc then break elseif Mc==""then pc"Unfinished string near <eof>"end end local bd=cc:sub(ad,jc-2)local cd=cc:sub(Yc,jc-1)Nc={Type="string",Data=cd,Constant=bd}elseif Mc=="["then local dd,ed=qc()if ed then Nc={Type="string",Data=ed,Constant=dd}else mc()Nc={Type="Symbol",Data="["}end elseif oc">=<"then if oc"="then Nc={Type="Symbol",Data=Mc.."="}else Nc={Type="Symbol",Data=Mc}end elseif oc"~"then if oc"="then Nc={Type="Symbol",Data="~="}else pc("Unexpected symbol `~` in source.",2)end elseif oc"."then if oc"."then if oc"."then Nc={Type="Symbol",Data="..."}else Nc={Type="Symbol",Data=".."}end else Nc={Type="Symbol",Data="."}end elseif oc":"then if oc":"then Nc={Type="Symbol",Data="::"}else Nc={Type="Symbol",Data=":"}end elseif f[Mc]then mc()Nc={Type="Symbol",Data=Mc}else local fd,gd=qc()if fd then Nc={Type="string",Data=gd,Constant=fd}else pc("Unexpected Symbol `"..Mc.."` in source.",2)end end Nc.LeadingWhite=Hc Nc.Line=Kc Nc.Char=Lc Nc.Print=function()return"<"..(Nc.Type..(" "):rep(7-#Nc.Type)).."  "..(Nc.Data or"").." >"end dc[#dc+1]=Nc if Nc.Type=="Eof"then break end end end)if not ec then return false,fc end local gc={}local hc={}local ic=1 function gc:getp()return ic end function gc:setp(hd)ic=hd end function gc:getTokenList()return dc end function gc:Peek(id)id=id or 0 return dc[math.min(#dc,ic+id)]end function gc:Get(jd)local kd=dc[ic]ic=math.min(ic+1,#dc)if jd then jd[#jd+1]=kd end return kd end function gc:Is(ld)return gc:Peek().Type==ld end function gc:Save()hc[#hc+1]=ic end function gc:Commit()hc[#hc]=nil end function gc:Restore()ic=hc[#hc]hc[#hc]=nil end function gc:ConsumeSymbol(md,nd)local od=self:Peek()if od.Type=="Symbol"then if md then if od.Data==md then self:Get(nd)return true else return nil end else self:Get(nd)return od end else return nil end end function gc:ConsumeKeyword(pd,qd)local rd=self:Peek()if rd.Type=="Keyword"and rd.Data==pd then self:Get(qd)return true else return nil end end function gc:IsKeyword(sd)local td=gc:Peek()return td.Type=="Keyword"and td.Data==sd end function gc:IsSymbol(ud)local vd=gc:Peek()return vd.Type=="Symbol"and vd.Data==ud end function gc:IsEof()return gc:Peek().Type=="Eof"end return true,gc end local function m(wd)local xd,yd if type(wd)~="table"then xd,yd=l(wd)else xd,yd=true,wd end if not xd then return false,yd end local function zd(Pd)local Qd=">> :"..yd:Peek().Line..":"..yd:Peek().Char..": "..Pd.."\n"local Rd=0 if type(wd)=="string"then for Sd in wd:gmatch"[^\n]*\n?"do if Sd:sub(-1,-1)=="\n"then Sd=Sd:sub(1,-2)end Rd=Rd+1 if Rd==yd:Peek().Line then Qd=Qd..">> `"..Sd:gsub("\t","\t").."`\n"for Td=1,yd:Peek().Char do local Ud=Sd:sub(Td,Td)if Ud=="\t"then Qd=Qd.."\t"else Qd=Qd.." "end end Qd=Qd.."   ^^^^"break end end end return Qd end local function Ad(Vd)local Wd=k:new(Vd)Wd.RenameVars=Wd.ObfuscateLocals Wd.ObfuscateVariables=Wd.ObfuscateLocals Wd.MinifyVars=Wd.MinifyVariables Wd.Print=function()return"<Scope>"end return Wd end local Bd local Cd local Dd,Ed,Fd,Gd local function Hd(Xd,Yd)local Zd=Ad(Xd)if not yd:ConsumeSymbol("(",Yd)then return false,zd"`(` expected."end local ae={}local be=false while not yd:ConsumeSymbol(")",Yd)do if yd:Is"Ident"then local ee=Zd:CreateLocal(yd:Get(Yd).Data)ae[#ae+1]=ee if not yd:ConsumeSymbol(",",Yd)then if yd:ConsumeSymbol(")",Yd)then break else return false,zd"`)` expected."end end elseif yd:ConsumeSymbol("...",Yd)then be=true if not yd:ConsumeSymbol(")",Yd)then return false,zd"`...` must be the last argument of a function."end break else return false,zd"Argument name or `...` expected"end end local xd,ce=Cd(Zd)if not xd then return false,ce end if not yd:ConsumeKeyword("end",Yd)then return false,zd"`end` expected after function body"end local de={}de.AstType="Function"de.Scope=Zd de.Arguments=ae de.Body=ce de.VarArg=be de.Tokens=Yd return true,de end function Fd(fe)local ge={}if yd:ConsumeSymbol("(",ge)then local xd,he=Bd(fe)if not xd then return false,he end if not yd:ConsumeSymbol(")",ge)then return false,zd"`)` Expected."end if false then he.ParenCount=(he.ParenCount or 0)+1 return true,he else local ie={}ie.AstType="Parentheses"ie.Inner=he ie.Tokens=ge return true,ie end elseif yd:Is"Ident"then local je=yd:Get(ge)local ke=fe:GetLocal(je.Data)if not ke then ke=fe:GetGlobal(je.Data)if not ke then ke=fe:CreateGlobal(je.Data)else ke.References=ke.References+1 end else ke.References=ke.References+1 end local le={}le.AstType="VarExpr"le.Name=je.Data le.Variable=ke le.Tokens=ge return true,le else return false,zd"primary expression expected"end end function Gd(me,ne)local xd,oe=Fd(me)if not xd then return false,oe end while true do local pe={}if yd:IsSymbol"."or yd:IsSymbol":"then local qe=yd:Get(pe).Data if not yd:Is"Ident"then return false,zd"<Ident> expected."end local re=yd:Get(pe)local se={}se.AstType="MemberExpr"se.Base=oe se.Indexer=qe se.Ident=re se.Tokens=pe oe=se elseif not ne and yd:ConsumeSymbol("[",pe)then local xd,te=Bd(me)if not xd then return false,te end if not yd:ConsumeSymbol("]",pe)then return false,zd"`]` expected."end local ue={}ue.AstType="IndexExpr"ue.Base=oe ue.Index=te ue.Tokens=pe oe=ue elseif not ne and yd:ConsumeSymbol("(",pe)then local ve={}while not yd:ConsumeSymbol(")",pe)do local xd,xe=Bd(me)if not xd then return false,xe end ve[#ve+1]=xe if not yd:ConsumeSymbol(",",pe)then if yd:ConsumeSymbol(")",pe)then break else return false,zd"`)` Expected."end end end local we={}we.AstType="CallExpr"we.Base=oe we.Arguments=ve we.Tokens=pe oe=we elseif not ne and yd:Is"string"then local ye={}ye.AstType="StringCallExpr"ye.Base=oe ye.Arguments={yd:Get(pe)}ye.Tokens=pe oe=ye elseif not ne and yd:IsSymbol"{"then local xd,ze=Dd(me)if not xd then return false,ze end local Ae={}Ae.AstType="TableCallExpr"Ae.Base=oe Ae.Arguments={ze}Ae.Tokens=pe oe=Ae else break end end return true,oe end function Dd(Be)local Ce={}if yd:Is"Number"then local De={}De.AstType="NumberExpr"De.Value=yd:Get(Ce)De.Tokens=Ce return true,De elseif yd:Is"string"then local Ee={}Ee.AstType="StringExpr"Ee.Value=yd:Get(Ce)Ee.Tokens=Ce return true,Ee elseif yd:ConsumeKeyword("nil",Ce)then local Fe={}Fe.AstType="NilExpr"Fe.Tokens=Ce return true,Fe elseif yd:IsKeyword"false"or yd:IsKeyword"true"then local Ge={}Ge.AstType="BooleanExpr"Ge.Value=yd:Get(Ce).Data=="true"Ge.Tokens=Ce return true,Ge elseif yd:ConsumeSymbol("...",Ce)then local He={}He.AstType="DotsExpr"He.Tokens=Ce return true,He elseif yd:ConsumeSymbol("{",Ce)then local Ie={}Ie.AstType="ConstructorExpr"Ie.EntryList={}while true do if yd:IsSymbol("[",Ce)then yd:Get(Ce)local xd,Je=Bd(Be)if not xd then return false,zd"Key Expression Expected"end if not yd:ConsumeSymbol("]",Ce)then return false,zd"`]` Expected"end if not yd:ConsumeSymbol("=",Ce)then return false,zd"`=` Expected"end local xd,Ke=Bd(Be)if not xd then return false,zd"Value Expression Expected"end Ie.EntryList[#Ie.EntryList+1]={Type="Key",Key=Je,Value=Ke}elseif yd:Is"Ident"then local Le=yd:Peek(1)if Le.Type=="Symbol"and Le.Data=="="then local Me=yd:Get(Ce)if not yd:ConsumeSymbol("=",Ce)then return false,zd"`=` Expected"end local xd,Ne=Bd(Be)if not xd then return false,zd"Value Expression Expected"end Ie.EntryList[#Ie.EntryList+1]={Type="KeyString",Key=Me.Data,Value=Ne}else local xd,Oe=Bd(Be)if not xd then return false,zd"Value Exected"end Ie.EntryList[#Ie.EntryList+1]={Type="Value",Value=Oe}end elseif yd:ConsumeSymbol("}",Ce)then break else local xd,Pe=Bd(Be)Ie.EntryList[#Ie.EntryList+1]={Type="Value",Value=Pe}if not xd then return false,zd"Value Expected"end end if yd:ConsumeSymbol(";",Ce)or yd:ConsumeSymbol(",",Ce)then elseif yd:ConsumeSymbol("}",Ce)then break else return false,zd"`}` or table entry Expected"end end Ie.Tokens=Ce return true,Ie elseif yd:ConsumeKeyword("function",Ce)then local xd,Qe=Hd(Be,Ce)if not xd then return false,Qe end Qe.IsLocal=true return true,Qe else return Gd(Be)end end local Id={["-"]=true,["not"]=true,["#"]=true}local Jd=8 local Kd={["+"]={6,6},["-"]={6,6},["%"]={7,7},["/"]={7,7},["*"]={7,7},["^"]={10,9},[".."]={5,4},["=="]={3,3},["<"]={3,3},["<="]={3,3},["~="]={3,3},[">"]={3,3},[">="]={3,3},["and"]={2,2},["or"]={1,1}}function Ed(Re,Se)local xd,Te if Id[yd:Peek().Data]then local Ue={}local Ve=yd:Get(Ue).Data xd,Te=Ed(Re,Jd)if not xd then return false,Te end local We={}We.AstType="UnopExpr"We.Rhs=Te We.Op=Ve We.OperatorPrecedence=Jd We.Tokens=Ue Te=We else xd,Te=Dd(Re)if not xd then return false,Te end end while true do local Xe=Kd[yd:Peek().Data]if Xe and Xe[1]>Se then local Ye={}local Ze=yd:Get(Ye).Data local xd,af=Ed(Re,Xe[2])if not xd then return false,af end local bf={}bf.AstType="BinopExpr"bf.Lhs=Te bf.Op=Ze bf.OperatorPrecedence=Xe[1]bf.Rhs=af bf.Tokens=Ye Te=bf else break end end return true,Te end Bd=function(cf)return Ed(cf,0)end local function Ld(df)local ef=nil local ff={}if yd:ConsumeKeyword("if",ff)then local gf={}gf.AstType="IfStatement"gf.Clauses={}repeat local xd,hf=Bd(df)if not xd then return false,hf end if not yd:ConsumeKeyword("then",ff)then return false,zd"`then` expected."end local xd,jf=Cd(df)if not xd then return false,jf end gf.Clauses[#gf.Clauses+1]={Condition=hf,Body=jf}until not yd:ConsumeKeyword("elseif",ff)if yd:ConsumeKeyword("else",ff)then local xd,kf=Cd(df)if not xd then return false,kf end gf.Clauses[#gf.Clauses+1]={Body=kf}end if not yd:ConsumeKeyword("end",ff)then return false,zd"`end` expected."end gf.Tokens=ff ef=gf elseif yd:ConsumeKeyword("while",ff)then local lf={}lf.AstType="WhileStatement"local xd,mf=Bd(df)if not xd then return false,mf end if not yd:ConsumeKeyword("do",ff)then return false,zd"`do` expected."end local xd,nf=Cd(df)if not xd then return false,nf end if not yd:ConsumeKeyword("end",ff)then return false,zd"`end` expected."end lf.Condition=mf lf.Body=nf lf.Tokens=ff ef=lf elseif yd:ConsumeKeyword("do",ff)then local xd,of=Cd(df)if not xd then return false,of end if not yd:ConsumeKeyword("end",ff)then return false,zd"`end` expected."end local pf={}pf.AstType="DoStatement"pf.Body=of pf.Tokens=ff ef=pf elseif yd:ConsumeKeyword("for",ff)then if not yd:Is"Ident"then return false,zd"<ident> expected."end local qf=yd:Get(ff)if yd:ConsumeSymbol("=",ff)then local rf=Ad(df)local sf=rf:CreateLocal(qf.Data)local xd,tf=Bd(df)if not xd then return false,tf end if not yd:ConsumeSymbol(",",ff)then return false,zd"`,` Expected"end local xd,uf=Bd(df)if not xd then return false,uf end local xd,vf if yd:ConsumeSymbol(",",ff)then xd,vf=Bd(df)if not xd then return false,vf end end if not yd:ConsumeKeyword("do",ff)then return false,zd"`do` expected"end local xd,wf=Cd(rf)if not xd then return false,wf end if not yd:ConsumeKeyword("end",ff)then return false,zd"`end` expected"end local xf={}xf.AstType="NumericForStatement"xf.Scope=rf xf.Variable=sf xf.Start=tf xf.End=uf xf.Step=vf xf.Body=wf xf.Tokens=ff ef=xf else local yf=Ad(df)local zf={yf:CreateLocal(qf.Data)}while yd:ConsumeSymbol(",",ff)do if not yd:Is"Ident"then return false,zd"for variable expected."end zf[#zf+1]=yf:CreateLocal(yd:Get(ff).Data)end if not yd:ConsumeKeyword("in",ff)then return false,zd"`in` expected."end local Af={}local xd,Bf=Bd(df)if not xd then return false,Bf end Af[#Af+1]=Bf while yd:ConsumeSymbol(",",ff)do local xd,Ef=Bd(df)if not xd then return false,Ef end Af[#Af+1]=Ef end if not yd:ConsumeKeyword("do",ff)then return false,zd"`do` expected."end local xd,Cf=Cd(yf)if not xd then return false,Cf end if not yd:ConsumeKeyword("end",ff)then return false,zd"`end` expected."end local Df={}Df.AstType="GenericForStatement"Df.Scope=yf Df.VariableList=zf Df.Generators=Af Df.Body=Cf Df.Tokens=ff ef=Df end elseif yd:ConsumeKeyword("repeat",ff)then local xd,Ff=Cd(df)if not xd then return false,Ff end if not yd:ConsumeKeyword("until",ff)then return false,zd"`until` expected."end local xd,Gf=Bd(Ff.Scope)if not xd then return false,Gf end local Hf={}Hf.AstType="RepeatStatement"Hf.Condition=Gf Hf.Body=Ff Hf.Tokens=ff ef=Hf elseif yd:ConsumeKeyword("function",ff)then if not yd:Is"Ident"then return false,zd"Function name expected"end local xd,If=Gd(df,true)if not xd then return false,If end local xd,Jf=Hd(df,ff)if not xd then return false,Jf end Jf.IsLocal=false Jf.Name=If ef=Jf elseif yd:ConsumeKeyword("local",ff)then if yd:Is"Ident"then local Kf={yd:Get(ff).Data}while yd:ConsumeSymbol(",",ff)do if not yd:Is"Ident"then return false,zd"local var name expected"end Kf[#Kf+1]=yd:Get(ff).Data end local Lf={}if yd:ConsumeSymbol("=",ff)then repeat local xd,Nf=Bd(df)if not xd then return false,Nf end Lf[#Lf+1]=Nf until not yd:ConsumeSymbol(",",ff)end for Of,Pf in next,Kf do Kf[Of]=df:CreateLocal(Pf)end local Mf={}Mf.AstType="LocalStatement"Mf.LocalList=Kf Mf.InitList=Lf Mf.Tokens=ff ef=Mf elseif yd:ConsumeKeyword("function",ff)then if not yd:Is"Ident"then return false,zd"Function name expected"end local Qf=yd:Get(ff).Data local Rf=df:CreateLocal(Qf)local xd,Sf=Hd(df,ff)if not xd then return false,Sf end Sf.Name=Rf Sf.IsLocal=true ef=Sf else return false,zd"local var or function def expected"end elseif yd:ConsumeSymbol("::",ff)then if not yd:Is"Ident"then return false,zd"Label name expected"end local Tf=yd:Get(ff).Data if not yd:ConsumeSymbol("::",ff)then return false,zd"`::` expected"end local Uf={}Uf.AstType="LabelStatement"Uf.Label=Tf Uf.Tokens=ff ef=Uf elseif yd:ConsumeKeyword("return",ff)then local Vf={}if not yd:IsKeyword"end"then local xd,Xf=Bd(df)if xd then Vf[1]=Xf while yd:ConsumeSymbol(",",ff)do local xd,Yf=Bd(df)if not xd then return false,Yf end Vf[#Vf+1]=Yf end end end local Wf={}Wf.AstType="ReturnStatement"Wf.Arguments=Vf Wf.Tokens=ff ef=Wf elseif yd:ConsumeKeyword("break",ff)then local Zf={}Zf.AstType="BreakStatement"Zf.Tokens=ff ef=Zf elseif yd:ConsumeKeyword("continue",ff)then local ag={}ag.AstType="ContinueStatement"ag.Tokens=ff ef=ag else local xd,bg=Gd(df)if not xd then return false,bg end if yd:IsSymbol","or yd:IsSymbol"="or(yd:Peek(1).Data=="="and g[yd:Peek().Data])then if(bg.ParenCount or 0)>0 then return false,zd"can not assign to parenthesized expression, is not an lvalue"end local cg={bg}while yd:ConsumeSymbol(",",ff)do local xd,gg=Gd(df)if not xd then return false,gg end cg[#cg+1]=gg end local dg={}if g[yd:Peek().Data]then for hg in next,g do if yd:IsSymbol(hg)then yd:ConsumeSymbol(hg,ff)dg.LuaUAssign=" "..hg.."= "break end end end if not yd:ConsumeSymbol("=",ff)then return false,zd"`=` Expected."end local eg={}local xd,fg=Bd(df)if not xd then return false,fg end eg[1]=fg while yd:ConsumeSymbol(",",ff)do local xd,ig=Bd(df)if not xd then return false,ig end eg[#eg+1]=ig end dg.AstType="AssignmentStatement"dg.Lhs=cg dg.Rhs=eg dg.Tokens=ff ef=dg elseif bg.AstType=="CallExpr"or bg.AstType=="TableCallExpr"or bg.AstType=="StringCallExpr"then local jg={}jg.AstType="CallStatement"jg.Expression=bg jg.Tokens=ff ef=jg else return false,zd"Assignment Statement Expected"end end if yd:IsSymbol";"then ef.Semicolon=yd:Get(ef.Tokens)end return true,ef end local Md={["end"]=true,["else"]=true,["elseif"]=true,["until"]=true}Cd=function(kg)local lg={}lg.Scope=Ad(kg)lg.AstType="Statlist"lg.Body={}lg.Tokens={}while not Md[yd:Peek().Data]and not yd:IsEof()do local xd,mg=Ld(lg.Scope)if not xd then return false,mg end lg.Body[#lg.Body+1]=mg end if yd:IsEof()then local ng={}ng.AstType="Eof"ng.Tokens={yd:Get()}lg.Body[#lg.Body+1]=ng end return true,lg end local function Nd()local og=Ad()return Cd(og)end local xd,Od=Nd()return xd,Od end local function n(pg)return"\""..(loadstring("return "..pg)():gsub(".",function(qg)return"\\"..qg:byte()end)).."\""end local o=false local function p(rg,sg,tg,ug,vg,wg)o=false i=0 local xg,yg,zg,Ag=0,"\n",false,false local function Bg(Fg,Gg,Hg)Hg=Hg or""local Ig,Jg=Fg:sub(-1,-1),Gg:sub(1,1)if c[Ig]or b[Ig]or Ig=="_"then if not(c[Jg]or b[Jg]or Jg=="_"or e[Jg])then return Fg..Gg elseif Jg=="("then return Fg..Hg..Gg else return Fg..Hg..Gg end elseif e[Ig]then if Jg=="("then return Fg..Gg else return Fg..Hg..Gg end elseif Ig==""then return Fg..Gg else if Jg=="("then return Fg..Hg..Gg else return Fg..Gg end end end local Cg={}local function Dg(Kg,vg)if sg and not Cg[Kg]then Cg[Kg]=true if vg then Kg.Scope:ObfuscateVariables()else Kg.Scope:MinifyVars()end end end local function Eg(Lg)o=Lg.AstType~="Parentheses"and Lg.AstType or o if Lg.Scope then if sg then Dg(Lg,vg)else Lg.Scope:RenameSingles()end end local Mg=("("):rep(Lg.ParenCount or 0)if Lg.AstType=="VarExpr"then if Lg.Variable then Mg=Mg..Lg.Variable.Name else Mg=Mg..Lg.Name end elseif Lg.AstType=="NumberExpr"then local Ng=tostring(tonumber(Lg.Value.Data))local yg=Ng:sub(1,1)=="-"if yg then Ng=Ng:sub(2)end if Ng=="inf"then Ng="math.huge"elseif Ng=="nan"then Ng="0 / 0"elseif Ng:sub(1,2)=="0."then Ng=Ng:sub(2)elseif Ng:match"%d+"==Ng then local Dg=ug and tonumber(Ng)if Dg and Dg>=1 then Ng=("0x%x"):format(Dg)else local tg=Ng:match"000+$"Ng=tg and(Ng:sub(1,#Ng-#tg).."e"..#tg)or Ng end end Mg=Mg..(yg and("-"..Ng)or Ng)elseif Lg.AstType=="StringExpr"then Mg=Mg..n(Lg.Value.Data)elseif Lg.AstType=="BooleanExpr"then Mg=Mg..tostring(Lg.Value)elseif Lg.AstType=="NilExpr"then Mg=Bg(Mg,"nil")elseif Lg.AstType=="BinopExpr"then Mg=Bg(Mg,Eg(Lg.Lhs)).." "if wg=="js"then Mg=Bg(Mg,({["and"]="&&",["or"]="||",["=="]="===",["~="]="!==",[".."]="+"})[Lg.Op]or Lg.Op).." "else Mg=Bg(Mg,Lg.Op).." "end Mg=Bg(Mg,Eg(Lg.Rhs))elseif Lg.AstType=="UnopExpr"then if wg=="js"then if Lg.Op~="#"and Lg.Op=="not"then Mg=Bg(Mg,"!")end Mg=Bg(Mg,Eg(Lg.Rhs))..(Lg.Op=="#"and".length"or"")else Mg=Bg(Mg,Lg.Op)..(#Lg.Op~=1 and" "or"")Mg=Bg(Mg,Eg(Lg.Rhs))end elseif Lg.AstType=="DotsExpr"then Mg=Mg.."..."elseif Lg.AstType=="CallExpr"then Mg=Mg..Eg(Lg.Base)if tg and wg~="js"and#Lg.Arguments==1 and(Lg.Arguments[1].AstType=="StringExpr"or Lg.Arguments[1].AstType=="ConstructorExpr")then Mg=Mg..Eg(Lg.Arguments[1])else Mg=Mg.."("for Og,Pg in next,Lg.Arguments do Mg=Mg..Eg(Pg)if Og~=#Lg.Arguments then Mg=Mg..", "end end Mg=Mg..")"end elseif Lg.AstType=="TableCallExpr"then if tg and wg~="js"then Mg=Mg..Eg(Lg.Base)..Eg(Lg.Arguments[1])else Mg=Mg..Eg(Lg.Base).."("Mg=Mg..Eg(Lg.Arguments[1])..")"end elseif Lg.AstType=="StringCallExpr"then if tg and wg~="js"then Mg=Mg..Eg(Lg.Base)..n(Lg.Arguments[1].Data)else Mg=Mg..Eg(Lg.Base).."("Mg=Mg..n(Lg.Arguments[1].Data)..")"end elseif Lg.AstType=="IndexExpr"then Mg=Mg..Eg(Lg.Base).."["..Eg(Lg.Index).."]"elseif Lg.AstType=="MemberExpr"then Mg=Mg..Eg(Lg.Base)..Lg.Indexer..Lg.Ident.Data elseif Lg.AstType=="Function"then Mg=Mg.."function("if#Lg.Arguments>0 then for Ag=#Lg.Arguments,1,-1 do if Lg.Arguments[Ag].References<=1 then table.remove(Lg.Arguments,Ag)else break end end for Qg,Rg in next,Lg.Arguments do Mg=Mg..Rg.Name if Qg~=#Lg.Arguments then Mg=Mg..", "elseif Lg.VarArg then if wg=="js"then Mg=Mg..", ...VARARGS"else Mg=Mg..", ..."end end end elseif Lg.VarArg then if wg=="js"then Mg=Mg..", ...VARARGS"else Mg=Mg.."..."end end if wg=="js"then Mg=Mg..") {"..yg else Mg=Mg..")"..yg end xg=xg+1 Mg=Bg(Mg,zg(Lg.Body))xg=xg-1 Mg=Bg(Mg,("\t"):rep(xg)..(wg=="js"and"}"or"end"))elseif Lg.AstType=="ConstructorExpr"then local Sg=(function()for _,Wg in next,Lg.EntryList do if Wg.Type=="Key"or Wg.Type=="KeyString"then return false end end return true end)()if wg=="js"and Sg then Mg=Mg.."["else Mg=Mg.."{"end local Tg,Ug,Vg=false,false,false for Xg,Yg in next,Lg.EntryList do Tg,Ug=Yg.Type=="Key"or Yg.Type=="KeyString",Tg xg=xg+(Sg and 0 or 1)if Tg or Vg then Vg=Tg if not Ug then Mg=Mg.."\n"end Mg=Mg..("\t"):rep(xg)end if Yg.Type=="Key"then Mg=Mg.."["..Eg(Yg.Key).."]"..(wg=="js"and": "or" = ")..Eg(Yg.Value)elseif Yg.Type=="Value"then Mg=Mg..Eg(Yg.Value)elseif Yg.Type=="KeyString"then Mg=Mg..Yg.Key..(wg=="js"and": "or" = ")..Eg(Yg.Value)end if Xg~=#Lg.EntryList then Mg=Mg..","if not Tg then Mg=Mg.." "end end if Tg then Mg=Mg.."\n"end xg=xg-(Sg and 0 or 1)end if#Lg.EntryList>0 and Tg then Mg=Mg..("\t"):rep(xg)end if wg=="js"and Sg then Mg=Mg.."]"else Mg=Mg.."}"end elseif Lg.AstType=="Parentheses"then local Zg=0 local Bg=false repeat Bg=(Bg or Lg).Inner Zg=Zg+1 until Bg.AstType~="Parentheses"if({["VarExpr"]=true,["MemberExpr"]=true,["IndexExpr"]=true})[Bg.AstType]or(({["NumberExpr"]=true,["BooleanExpr"]=true,["NilExpr"]=true,["DotsExpr"]=true,["StringExpr"]=true,["UnopExpr"]=true,["ConstructorExpr"]=true,["Function"]=true})[Bg.AstType]and not({["MemberExpr"]=true,["IndexExpr"]=true,["CallStatement"]=true,["CallExpr"]=true,["TableCallExpr"]=true,["StringCallExpr"]=true})[o])then Mg=Mg..Eg(Bg)else Mg=Mg.."("..Eg(Bg)..")"end end Mg=Mg..(")"):rep(Lg.ParenCount or 0)return Mg end function Ag(ah)o=ah.AstType~="Parentheses"and ah.AstType or o if ah.Scope then if sg then Dg(ah,vg)else ah.Scope:RenameSingles()end end local bh=""if ah.AstType=="AssignmentStatement"then bh=("\t"):rep(xg)for ch,dh in next,ah.Lhs do bh=bh..Eg(dh)if ch~=#ah.Lhs then bh=bh..", "end end if#ah.Rhs>0 then if ah.LuaUAssign then bh=bh..ah.LuaUAssign else bh=bh.." = "end for eh,fh in next,ah.Rhs do bh=bh..Eg(fh)if eh~=#ah.Rhs then bh=bh..", "end end end elseif ah.AstType=="CallStatement"then bh=("\t"):rep(xg)..Eg(ah.Expression)elseif ah.AstType=="LocalStatement"then bh=("\t"):rep(xg)..bh..(wg=="js"and"let "or"local ")for gh,hh in next,ah.LocalList do bh=bh..hh.Name if gh~=#ah.LocalList then bh=bh..", "end end if#ah.InitList>0 then bh=bh.." = "for ih,jh in next,ah.InitList do bh=bh..Eg(jh)if ih~=#ah.InitList then bh=bh..", "end end end elseif ah.AstType=="IfStatement"then bh=("\t"):rep(xg)..Bg("if "..(wg=="js"and"("or""),Eg(ah.Clauses[1].Condition))bh=Bg(bh,(wg=="js"and") {"or" then"))..yg xg=xg+1 bh=Bg(bh,zg(ah.Clauses[1].Body))xg=xg-1 for kh,lh in next,ah.Clauses do if kh>=2 then if lh.Condition then bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"} else if ("or"elseif "))bh=Bg(bh,Eg(lh.Condition))bh=Bg(bh,(wg=="js"and") {"or" then"))..yg else bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"} else {"or"else"))..yg end xg=xg+1 bh=Bg(bh,zg(lh.Body))xg=xg-1 end end bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"}"or"end"))elseif ah.AstType=="WhileStatement"then bh=("\t"):rep(xg)..Bg("while "..(wg=="js"and"("or""),Eg(ah.Condition))bh=Bg(bh,(wg=="js"and") {"or" do"))..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"}"or"end"))elseif ah.AstType=="DoStatement"then bh=("\t"):rep(xg)..Bg(bh,(wg=="js"and"if (true) {"or"do"))..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"}"or"end"))elseif ah.AstType=="ReturnStatement"then bh=("\t"):rep(xg).."return"if#ah.Arguments>0 then bh=bh.." "end for mh,nh in next,ah.Arguments do bh=Bg(bh,Eg(nh))if mh~=#ah.Arguments then bh=bh..", "end end elseif ah.AstType=="BreakStatement"then bh=("\t"):rep(xg).."break"elseif ah.AstType=="ContinueStatement"then bh=("\t"):rep(xg).."continue"elseif ah.AstType=="RepeatStatement"then bh=("\t"):rep(xg)..(wg=="js"and"do {"or"repeat")..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"} while (!("or"until "))bh=Bg(bh,Eg(ah.Condition)..(wg=="js"and"))"or""))elseif ah.AstType=="Function"then if ah.IsLocal and wg=="js"then bh=wg=="js"and"let "..ah.Name.Name.." = function"elseif ah.IsLocal then bh="local "bh=Bg(bh,"function ")end if not ah.IsLocal then bh=Bg(bh,"function ")end bh=("\t"):rep(xg)..bh if ah.IsLocal and wg~="js"then bh=bh..ah.Name.Name else bh=bh..Eg(ah.Name)end bh=bh.."("if#ah.Arguments>0 then for oh,ph in next,ah.Arguments do bh=bh..ph.Name if oh~=#ah.Arguments then bh=bh..", "elseif ah.VarArg then if wg=="js"then bh=bh..", ...VARARGS"else bh=bh..", ..."end end end elseif ah.VarArg then if wg=="js"then bh=bh..", ...VARARGS"else bh=bh.."..."end end bh=bh..")"..(wg=="js"and" {"or"")..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"}"or"end"))elseif ah.AstType=="GenericForStatement"then if wg=="js"then bh=("\t"):rep(xg).."for (let "for Ag,qh in next,ah.VariableList do ah.VariableList[Ag]=qh.Name end bh=bh..table.concat(ah.VariableList,", ").." of "for rh,sh in next,ah.Generators do bh=Bg(bh,Eg(sh))if rh~=#ah.Generators then bh=Bg(bh,", ")end end bh=Bg(bh,") {")..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg).."}")else bh=("\t"):rep(xg).."for "if#ah.VariableList>1 then for Ag=#ah.VariableList,2,-1 do if ah.VariableList[Ag].References<=1 then table.remove(ah.VariableList,Ag)else break end end end for th,uh in next,ah.VariableList do bh=bh..uh.Name if th~=#ah.VariableList then bh=bh..", "end end bh=bh.." in "for vh,wh in next,ah.Generators do bh=Bg(bh,Eg(wh))if vh~=#ah.Generators then bh=Bg(bh,", ")end end bh=Bg(bh," do")..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"}"or"end"))end elseif ah.AstType=="NumericForStatement"then if wg=="js"then bh=("\t"):rep(xg).."for ("local xh=ah.Variable.Name local yh,zh=Eg(ah.Start),Eg(ah.End)local Ah,Bh=tonumber(yh),tonumber(zh)local Ch=Ah and Bh and Ah>=Bh bh=bh.."let "..xh.." = "..yh.."; "..xh.." "..(Ch and">="or"<=").." "..zh if ah.Step then bh=bh.."; "..xh.." = "..xh.." + "..Eg(ah.Step)else bh=bh.."; "..xh.."++"end bh=Bg(bh,") {")..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg).."}")else bh=("\t"):rep(xg).."for "bh=bh..ah.Variable.Name.." = "bh=bh..Eg(ah.Start)..", "..Eg(ah.End)if ah.Step then bh=bh..", "..Eg(ah.Step)end bh=Bg(bh," do")..yg xg=xg+1 bh=Bg(bh,zg(ah.Body))xg=xg-1 bh=Bg(bh,("\t"):rep(xg)..(wg=="js"and"}"or"end"))end elseif ah.AstType=="LabelStatement"then bh=("\t"):rep(xg).."::"..ah.Label.."::"..yg elseif ah.AstType=="GotoStatement"then bh=("\t"):rep(xg).."goto "..ah.Label..yg elseif ah.AstType=="Comment"then print(ah.CommentType,ah.Data)if ah.CommentType=="Shebang"then bh=("\t"):rep(xg)..ah.Data elseif ah.CommentType=="Comment"then bh=("\t"):rep(xg)..ah.Data elseif ah.CommentType=="LongComment"then bh=("\t"):rep(xg)..ah.Data end elseif ah.AstType~="Eof"then print("Unknown AST Type: ",ah.AstType)end return bh end function zg(Dh)local Eh=""Dg(Dh,vg)for _,Fh in next,Dh.Body do Eh=Bg(Eh,Ag(Fh)..yg)end return Eh end return zg(rg):gsub(",%.%.%.",", ..."):gsub(", \n",",\n"):gsub("for _[, _]+ in","for _ in"):gsub("function%(%)[\n\t]+end","function() end"):match"^%s*(.-)%s*$"end local function q(Gh)return(Gh:gsub("\"[\\%d]+\"",function(Hh)return"\""..loadstring("return "..Hh)():gsub("[%z%c\"\\]",function(Ih)return({["\\"]="\\\\",["\a"]="\\a",["\b"]="\\b",["\f"]="\\f",["\n"]="\\n",["\r"]="\\r",["\t"]="\\t",["\v"]="\\v",["\""]="\\\""})[Ih]or("\\"..Ih:byte())end).."\""end))end local function r(Jh,Kh,Lh)local Mh,Nh=m(Jh)if not Mh then return Jh,Nh end local Oh=p(Nh,Lh,false,Kh,false)if not Kh then Oh=q(Oh)end return Oh end local function s(Ph,Qh)local Rh,Sh=m(Ph)if not Rh then return Ph,Sh end local Th=p(Sh,true,true,Qh,false)for _ =1,2 do Th=Th:gsub("%s+"," "):gsub("([%w_]) (%p)",function(Uh,Vh)if Vh~="_"then return Uh..Vh end end):gsub("(%p) (%p)",function(Wh,Xh)if Wh~="_"and Xh~="_"then return Wh..Xh end end):gsub("(%p) ([%w_])",function(Yh,Zh)if Yh~="_"then return Yh..Zh end end)end if not Qh then Th=q(Th)end return Th end local function t(ai,bi,ci)local di,ei=m(ai)if not di then return ai,ei end local fi=p(ei,true,true,bi,true)if not ci then for _ =1,2 do fi=fi:gsub("%s+"," "):gsub("([%w_]) (%p)",function(gi,hi)if hi~="_"then return gi..hi end end):gsub("(%p) (%p)",function(ii,ji)if ii~="_"and ji~="_"then return ii..ji end end):gsub("(%p) ([%w_])",function(ki,li)if ki~="_"then return ki..li end end)end end if not bi then fi=q(fi)end return fi end local function u(mi,ni)local oi,pi=m(mi)if not oi then return mi,pi end local qi=p(pi,false,false,ni,false,"js")if ni then qi=qi:gsub("\"[\\%d]+\"",function(ri)return"\""..loadstring("return "..ri)():gsub(".",function(si)return("\\x%02x"):format(si:byte())end).."\""end)else qi=q(qi)end return qi end return{beautify=r,minify=s,uglify=t,tojavascript=u}
