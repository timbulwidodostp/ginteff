*! version 1 17Nov2022

program ginteff, eclass
	version 14

	syntax [if] [in] [fw aw pw iw] [, at(str) atdxs(str) obseff(str) dydxs(str) fd(str) firstdiff(str) INTEQuation(str) ///
									Level(cilevel) many NOLegend NOWEIGHTs nunit(str) post PRedict(str) vce(str) __s_ginteffplot ]
	
	cap noi {
		qui {
			if ("`__s_ginteffplot'"!="") {
				if ("`obseff'"!="") mata __s_gplot = st_local("obseff")
				else mata __s_gplot = J(1,1,"")
				exit
			}
			
			local cmdl `"ginteff `*'"'

			cap mata: st_local("sname",strofreal(max(substr(direxternal("*"),1,4):=="__s_"))) 
			if (!_rc & "`sname'"!="0") {
				di as err "There are Mata matrixes that start with '__s_' which you must either drop or rename before proceeding;" ///
				_n " see {manhelpi mata_drop M-3} or {manhelpi mata_rename M-3}" ///
				_n "(To drop all, type {cmd:mata: mata drop __s_*} in Stata's command window.)"
				exit(198)
			}

			if ("`e(marginsnotok)'"=="_ALL") {
				di as err "{bf:ginteff} not appropriate after {bf:`e(cmd)'}"
				exit(198)
			}
			else if ("`e(cmd)'"=="ginteff") {
				di as err "{bf:ginteff} cannot work with its own posted results"
				exit(198)
			}
			else if ("`e(est_cmd)'"!="" & "`e(cmd)'"!="`e(est_cmd)'") {
				di as err "{bf:ginteff} cannot work with posted results of non estimation commands"
				exit(198)
			}
				
			local N = _N
			cap estimates esample
			if ("`r(who)'"=="zero'd") {
				di as err "{bf:e(sample)} does not identify the estimation sample"
				exit(198)
			}
			cap confirm matrix e(b)
			if (_rc) {
				di as err "last estimates not found"			
				exit(198)
			}
	
			tempvar obsno
			gen long `obsno' = _n
			marksample touse
			replace `touse' = 0 if !e(sample)
			cap sum `obsno' if `touse', meanonly
			if ("`r(min)'"=="") {
				di as err "no observations"
				exit(198)
			}
			else {
				local fsamp `r(min)'
				mata: __s_rN = st_numscalar("r(N)")
			}

			if (e(k_eq)!=. & e(k_eq)!=1) {
				if ("`intequation'"!="") {
					local inteq "`intequation'"			
					cap margins in `fsamp', predict(eq(`inteq')) nose
					if (_rc==303) {
						di as err "invalid option {bf:intequation()};" _n " equation `inteq' not found"
						exit(198)
					}
					else if (_rc) {
						di as err "option {bf:intequation()} incorrectly specified"
						exit(198)
					}
					local inteq = subinstr("`inteq'",","," ",1)
					local inteq = strtrim(subinstr("`inteq'","#"," ",1))
					if (strpos("`inteq'"," ")) {
						di as err "invalid option {bf:intequation()};" _n " too many equations specified"
						exit(198)
					}
									
					cap confirm number `inteq'
					if (!_rc) local numeq 1
				}
				else {
					local inteq 1
					local numeq 1
					if (e(k_dv)!=1) local intequation "#1"
				}
			}
			else if ("`intequation'"!="") {
				cap margins in `fsamp', predict(eq(`intequation')) nose
				if (_rc==303) {
					di as err "invalid option {bf:intequation()};" _n " equation `intequation' not found"
					exit(198)
				}
				else if (_rc) {
					di as err "option {bf:intequation()} incorrectly specified"
					exit(198)
				}
				else {
					local inteq "`intequation'"
					local inteq = subinstr("`inteq'",","," ",1)
					local inteq = strtrim(subinstr("`inteq'","#"," ",1))
					if (strpos("`inteq'"," ")) {
						di as err "invalid option {bf:intequation()};" _n " too many equations specified"
						exit(198)
					}
									
					cap confirm number `inteq'
					if (!_rc) local numeq 1
				}
			}

			margins in `fsamp', predict(`predict') at((base) _factor) noatlegend nose
			local keq = colsof(r(b))
			mata: __s_keq = strtoreal(st_local("keq"))
			if ("`r(atstats1)'"!="") mata: __s_fvar = colnonmissing(st_matrix("r(at)"))
			else mata: __s_fvar = .
			
			if ("`obseff'"!="") {
				if (`: word count `obseff''>1) {
					di as err "invalid option {bf:obseff()};" _n " too many names specified"
					exit(198)
				}
				else local prnm `obseff'
				if (strlen("`prnm'")>16) {
					di as err "'`prnm'' invalid name"
					exit(198)
				}

				tempname genpr 
			}
				
			local wght "[`weight' `exp']"

			mata __s_vlab = J(1,1,"")
			mata __s_nlev = 1
			local inteff 0
			local ndyoz 0
			if ("`dydxs'"!="") {
				if (strpos("`dydxs'","(")) {
					if (strlen(subinstr("`dydxs'","(","",.))!=strlen(subinstr("`dydxs'",")","",.))) {
						di as err "invalid option {bf:dydxs()};" _n " parentheses unbalanced"
						exit(198)
					}
					else {
						local dydxs = subinstr("`dydxs'","("," ",.)
						local dydxs = subinstr("`dydxs'",")"," ",.)
					}
				}
			
				local wcdyx = wordcount("`dydxs'")
				if (`wcdyx'>3) {
					di as err "maximum 3 arguments allowed with {bf:dydxs()}"
					exit(198)
				}
				else if (`wcdyx') {
					mata: __s_vint = .
					mata: inteff(__s_nlev,__s_vint,__s_vlab)

					if ((!`inteff' & "`fvdx'"=="") | inlist(`inteff',3,7)) local stvi 1
					else if (inlist(`inteff',2,6)) local stvi 2
					else local stvi 3

					if (inlist(`inteff',3,6,7)) {
						if (substr("`predict'",1,2)=="xb") {
							if (`inteff'==6) {
								local ndyoz 1
								mata: st_local("prxb",invtokens(__s_vint[2,3]))
							}
							else {
								local ndyoz 0
								mata: st_local("prxb",invtokens(__s_vint))
							}
							local inteff 0
						}
						else {
							if (!inlist("`numeq'","","1")) {
								di as err "second- and third-order cross partial derivatives are allowed" ///
										_n " only with 'logistic', 'logit', and 'probit' models"
								exit(198)
							}
							else local inteq

							margins in `fsamp', dydx(`dydxs') predict(`predict') nose
							if ("`r(predict_label)'"!="") local r_prc = r(predict_label) + ", " + r(expression)
							else if ("`r(predict1_label)'"!="") local r_prc = r(predict1_label) + ", " + r(expression)
							else local r_prc = r(expression)
						}
					}

					margins in `fsamp', at(`atdyx') noatlegend nose
					mata: __s_dyoz = colnonmissing(st_matrix("r(at)"))
				}
			}
			else mata: __s_vint = J(1,0,"")

			if ("`fd'"!="" & "`firstdiff'"!="") {
				di as err "only one {bf:firstdiff()} option allowed"
				exit(198)
			}
			else if ("`fd'"!="" | "`firstdiff'"!="") {
				local fdxs `fd' `firstdiff'

				if (strpos("`fdxs'","(asobserved") | strpos("`fdxs'","( asobserved")) {
					local fdxs = subinstr("`fdxs'","( asobserved","(asobserved",.)
					cap margins in `fsamp', at(`=subinstr("`fdxs'","(asobserved","(mean",.)') noatlegend nose 
				}
				else cap margins in `fsamp', at(`fdxs') noatlegend nose

				if (inlist(_rc,111,198,322)) {
					di as err "invalid option {bf:firstdiff()};"
					cap noi margins in `fsamp', at(`=subinstr("`fdxs'","(asobserved","(mean",.)') noatlegend nose
					exit(198)					
				}
				else if (_rc) {
					di as err "invalid option {bf:firstdiff()}"
					exit(198)					
				}
				else if ("`r(atstats1)'"!="") {
					if (rowsof(r(at))>1) {
						di as err "invalid option {bf:firstdiff()};" _n " {it:numlist} not allowed"
						exit(198)
					}

					mata: __s_fdnm = st_matrixcolstripe("r(at)")[,2]'							
					mata: __s_fdoz = (st_matrix("r(at)"):!=.)
					mata: if (__s_fvar[1]!=.) st_local("max",strofreal(max(__s_fvar:+__s_fdoz))) ;;
					if ("`max'"!="" & "`max'"!="1") {
						di as err "factor variables not allowed with {bf:firstdiff()}"
						exit(198)								
					}
					mata: __s_svfd = selectindex(__s_fdoz)
				}
				else local fdxs
			}
	
			if ("`atdxs'"!="") {
				if ("`dydxs'"=="") {
					di as err "option {bf:atdxs()} requires {bf:dydxs()}"
					exit(198)
				}
					
				cap margins in `fsamp', at(`atdxs') noatlegend nose
				if (inlist(_rc,111,198,322)) {
					di as err "invalid option {bf:atdxs()};"
					margins in `fsamp', at(`atdxs') noatlegend nose
				}
				else if (_rc) {
					di as err "invalid option {bf:atdxs()}"
					exit(198)
				}
				else if ("`r(atstats1)'"=="") local atdxs
				else {
					if (rowsof(r(at))>1) {
						di as err "invalid option {bf:atdxs()};" _n " {it:numlist} not allowed"
						exit(198)
					}
				
					if (strpos("`atdxs'","(")) {
						local atdxs = subinstr("`atdxs'","("," ( ",.)
						local atdxs = subinstr("`atdxs'",")"," ) ",.)
					}
					if (strpos(" `atdxs'"," _f") & "`fvdx'"!="") {
						di as err "factor variables cannot be set via {bf:atdxs()}"
						exit(198)
					}
							
					if (strpos(" `atdxs' "," _all ")) local atdxs = subinstr(" `atdxs' "," _all "," _continuous ",.)					
					if (strpos(" `atdxs'"," _c")) {
						if (`inteff'==0 & "`fvdx'"=="") mata: st_local("cx",invtokens(__s_vint))
						if (`inteff'==2) mata: st_local("cx",__s_vint[2])
						if (`inteff'==5) mata: st_local("cx",invtokens(__s_vint[3]))
						if (`inteff'==6) mata: st_local("cx",invtokens(__s_vint[2,3]))
						if (inlist(`inteff',3,7)) mata: st_local("cx",invtokens(__s_vint))

						local cont "_continuous _continuou _continuo _continu _contin _conti _cont _con _co _c" 
						foreach c of local cont {
							if (strpos(" `atdxs' "," `c' ")) local atdxs = subinstr(" `atdxs' "," `c' "," `cx' ",.)
						}
					}
					
					mata: atdx(__s_vint)
										
					if (strpos("`atdxs'","( asobserved ")) margins in `fsamp', at(`=subinstr("`atdxs'","( asobserved ","( mean ",.)') noatlegend nose 
					else if ("`cx'"!="") margins in `fsamp', at(`atdxs') noatlegend nose
					mata: __s_xsoz = (tokens(st_global("r(atstats1)")):!="asobserved")
					mata: st_local("mx",strofreal(max(__s_xsoz:-__s_dyoz)))
					if (`mx'==1) {
						di as err "variables in {bf:dydxs()} and {bf:atdxs()} do not match"
						exit(198)
					}
					mata: __s_xsoz = (tokens(st_global("r(atstats1)")):!="asobserved")						
					mata: if (__s_fvar[1]!=.) st_local("fvxs",strofreal(max(__s_xsoz:* __s_dyoz:* __s_fvar))) ;;
					if ("`fvxs'"=="1") {
						di as err "factor variables cannot be set via {bf:atdxs()}"
						exit(198)
					}
				}
			}

			if ("`atdxs'"=="" & "`stvi'"!="") mata: for (i=strtoreal(st_local("stvi")); i<=cols(__s_vint); i++) st_local("wrt"+strofreal(i), st_local("wrt"+strofreal(i)) + " (asobserved)")
					
			if ("`prxb'"!="") {
				if ("`fdxs'"!="") {			
					mata: st_local("max",strofreal(max(__s_dyoz:+__s_fdoz)))
					if (`max'>1) {
						di as err "variables in {bf:dydxs()} and {bf:firstdiff()} must be distinct"
						exit(198)
					}
					cap mata st_local("fdnmxb",__s_fdnm[__s_svfd])
					if (_rc) {
						di as err "too many interacting variables specified in {bf:dydxs()} and {bf:firstdiff()}"
						exit(198)
					}

					if ("`atdxs'"!="") local fdxs "(asobserved) `prxb' `atdxs' `fdxs'"
					else local fdxs `prxb'
					if ("`nunit'"!="") local nunit "(1) `prxb' `nunit'"

					margins in `fsamp', at(`prxb') noatlegend nose
					mata: __s_fdoz = __s_fdoz:+ (st_matrix("r(at)"):!=.)
					mata: __s_svfd = selectindex(__s_fdoz)
				}
				else {
					if ("`atdxs'"!="") local fdxs "(asobserved) `prxb' `atdxs'"
					else local fdxs `prxb'

					margins in `fsamp', at(`prxb') noatlegend nose
					mata: __s_fdnm = st_matrixcolstripe("r(at)")[,2]'							
					mata: __s_fdoz = (st_matrix("r(at)"):!=.)
					mata: __s_svfd = selectindex(__s_fdoz)
				}
				
				if ("`fvdx'"=="") mata __s_vint = J(1,0,"")
				else mata __s_vint = __s_vint[1]

				local dydxs
			}

			if (!`ndyoz' & "`fdxs'"=="") {
				di as err "one of {bf:dydxs()} or {bf:firstdiff()} is required"
				exit(198)
			}
			
			if ("`at'"!="") {				
				if (strpos("`at'","(")) {
					local at = subinstr("`at'","("," ( ",.)
					local at = subinstr("`at'",")"," ) ",.)
				}
				if (strpos(" `at' "," _all ")) local intasob 1	
				else {
					local cont "_continuous _continuou _continuo _continu _contin _conti _cont _con _co _c" 
					foreach c of local cont {
						if (strpos(" `at' "," `c' ")) {
							local intasob 1
							continue, break
						}
					}
				}
				if ("`intasob'"=="") {
					local fact "_factor _facto _fact _fac _fa _f" 
					foreach f of local fact {
						if (strpos(" `at' "," `f' ")) {
							local intasob 1
							continue, break
						}
					}
				}
				if ("`intasob'"!="") {
					if ("`fdxs'"!="") mata: st_local("intasob", " (asobserved) " + invtokens((__s_vint, __s_fdnm[__s_svfd])) + " ")				
					else mata: st_local("intasob", " (asobserved) " + invtokens(__s_vint) + " ")
					local at "`at' `intasob'"
				}

				cap margins in `fsamp', at(`at') noatlegend nose `noweights'
				if (_rc) {
					di as err "invalid option {bf:at()};"
					margins in `fsamp', at(`at') noatlegend nose `noweights'
				}
				else if ("`r(atstats1)'"!="") {
					local rats1 1
					local nat = rowsof(r(at))
					mata: __s_slb = tokens(st_global("r(atstats1)"))
					mata: __s_atoz = (__s_slb:!="asobserved")
				
					if (`ndyoz' | "`prxb'"!="") {
						mata: st_local("max",strofreal(max(__s_dyoz:+__s_atoz)))
						if (`max'>1) {
							di as err "variables in {bf:dydxs()} and {bf:at()} must be distinct"
							exit(198)
						}
					}
					if ("`fdxs'"!="") {
						mata: st_local("max",strofreal(max(__s_atoz:+__s_fdoz)))
						if (`max'>1) {
							di as err "variables in {bf:firstdiff()} and {bf:at()} must be distinct"
							exit(198)
						}
					}

					mata: st_local("any",strofreal(anyof(substr(__s_slb[selectindex(__s_atoz)],1,1),"m") + anyof(substr(__s_slb[selectindex(__s_atoz)],1,1),"p")))				
					if (`any') margins if `touse', at(`at') noatlegend nose `noweights'
					if ("`nolegend'"=="") {
						mata: __s_atlv=__s_atname=__s_ats= .	
						mata: atinteff(__s_atlv,__s_atname,__s_ats,__s_atoz,__s_fvar,__s_slb)
					}
					tempname rat
					mat rename r(at) `rat'
					local ratst `r(atstats1)'
				}
				else local nat 1			
			}
			else local nat 1
			mata: __s_atr = strtoreal(st_local("nat"))
			if ("`atdxs'"!="") local at "`at' (mean) `atdxs'"
		
			if ("`fdxs'"=="") local options at(`at') level(`level') noatlegend `noweights' predict(`predict') vce(`vce')
			else local options level(`level') noatlegend `noweights' predict(`predict') vce(`vce')
			
			if ("`fdxs'"!="") {	
				mata: __s_vint = __s_vint, __s_fdnm[__s_svfd]
				mata: st_local("nint",strofreal(cols(__s_svfd)))			
				if (`nint'>3) {
					di as err "invalid option {bf:firstdiff()};" _n " too many interacting variables specified"
					exit(198)
				}
				else if (`nint'==3) mata: __s_g = -1,1,1,-1,1,-1,-1,1
				else if (`nint'==2) mata: __s_g = 1,-1,-1,1				
				else mata: __s_g = -1,1

				mata __s_L=__s_Lat = .
				mata L(__s_atr,__s_g,__s_keq,__s_L,__s_Lat,__s_nlev)

				margins in `fsamp', at((asobserved) `fdxs') noatlegend nose `noweights'
				if ("`r(atstats1)'"!="") {
					mata: __s_fdgn = tokens(st_global("r(atstats1)"))
					mata: __s_fdst = __s_fdgn[__s_svfd]
					mata: __s_rfdst = invtokens(__s_fdst)					
					mata: st_local("any",strofreal(anyof(substr(__s_fdst,1,1),"m") + anyof(substr(__s_fdst,1,1),"p")))
					if (`any') margins if `touse', at((asobserved) `fdxs') noatlegend nose `noweights'

					mata: __s_fdvl = st_matrix("r(at)")
					mata: __s_rfd = __s_fdvl[__s_svfd]
					mata: _editmissing(__s_fdvl,0)
					mata: __s_fdiv = __s_fdvl[__s_svfd]				

					mata: __s_fdgn[selectindex(colnonmissing(st_matrix("r(at)")))] = J(1,nonmissing(st_matrix("r(at)")),"")
					mata: __s_pos = (__s_fdgn:=="asobserved")
					mata: __s_pos = (__s_pos:==1)
					mata: __s_fdgn[selectindex(__s_pos)] = __s_fdnm[selectindex(__s_pos)]:+"+"
					mata: __s_fdgn[selectindex(strpos(__s_fdgn,"("))] = __s_fdgn[selectindex(strpos(__s_fdgn,"("))]:+"+"

					mata: __s_pos = selectindex(st_matrix("r(at)")[__s_svfd]:<.)
					mata: __s_fdst[__s_pos] = " at (":+__s_fdst[__s_pos]:+")"
					mata: __s_pos = selectindex(st_matrix("r(at)")[__s_svfd]:==.)
					mata: __s_fdst[__s_pos] = " (":+__s_fdst[__s_pos]:+")"
					mata: __s_pos = selectindex(st_matrix("r(at)"):>.)
					mata: __s_fdst[__s_pos] = " = gen":+__s_fdst[__s_pos]
					mata: __s_fdst[selectindex(__s_fdst:==" at (value)")] = " = ":+ strofreal(st_matrix("r(at)")[__s_svfd[selectindex(__s_fdst:==" at (value)")]])
					mata: __s_fdst = __s_fdnm[__s_svfd]:+ __s_fdst	
				}
				else {
					mata: __s_fdvl = J(1,cols(__s_fdoz),0)
					mata: __s_fdiv = J(1,cols(__s_svfd),0)
					mata: __s_fdgn = __s_fdnm:+"+"
					mata: __s_rfdst = invtokens(J(1,cols(__s_svfd),"asobserved"))					
					mata: __s_rfd = J(1,cols(__s_svfd),.)
					mata: __s_fdst = __s_fdnm[__s_svfd]:+ J(1,cols(__s_svfd)," (asobserved)")				
				}

				if (!`ndyoz') {
					mata: __s_fdwrt = strofreal(1..cols(__s_svfd))
					if (`nint'==2) mata: __s_vlab = "∆(x1)#∆(x2)    "
					else mata: __s_vlab = "∆(x1)#∆(x2)#∆(x3)      "
				}
				else {
					mata: __s_fdwrt = strofreal(strtoreal(st_local("ndyoz")):+(1..cols(__s_svfd))) 
					if (`nint'==1) mata: __s_vlab = __s_vlab:+ "#∆(x2)    "
					else mata: __s_vlab = __s_vlab:+ "#∆(x2)#∆(x3)      "
				}
				mata: for (i=1; i<=cols(__s_fdwrt); i++) st_local("d"+__s_fdwrt[i],"∆(x"+__s_fdwrt[i]+")")				

				if (`keq'!=1 | `nat'!=1) {
					if ("`fvdx'"=="") {
						if (`keq'==1) mata: __s_vlab = J(rows(__s_vlab),1,strofreal(1::__s_atr):+"._at#") :+ J(__s_atr,1,__s_vlab)
						else if (`nat'==1) mata: __s_vlab = J(rows(__s_vlab),1,strofreal(1::__s_keq):+"._pr#") :+ J(__s_keq,1,__s_vlab)
						else {
							mata: __s_vlab = J(rows(__s_vlab),1,strofreal(1::__s_atr):+"._at#") :+ J(__s_atr,1,__s_vlab)
							mata: __s_vlab = J(rows(__s_vlab),1,strofreal(1::__s_keq):+"._pr#") :+ J(__s_keq,1,__s_vlab)
						}
					}
					else {
						if (`keq'==1) mata: __s_vlab = vec(J(rows(__s_vlab),1,strofreal(1..__s_atr):+"._at#")) :+ J(__s_atr,1,__s_vlab)
						else if (`nat'==1) mata: __s_vlab = vec(J(rows(__s_vlab),1,strofreal(1..__s_keq):+"._pr#")) :+ J(__s_keq,1,__s_vlab)
						else {
							mata: __s_vlab = vec(J(rows(__s_vlab),1,strofreal(1..__s_atr):+"._at#")) :+ J(__s_atr,1,__s_vlab)
							mata: __s_vlab = vec(J(rows(__s_vlab),1,strofreal(1..__s_keq):+"._pr#")) :+ J(__s_keq,1,__s_vlab)
						}
					}
				}
				
				if (`ndyoz') local nint = `nint'+`ndyoz'
				if (`nint'<2) {
					di as err "not enough interacting variables specified"
					exit(198)
				}
				else if (`nint'>3) {
					di as err "too many interacting variables specified in {bf:dydxs()} and {bf:firstdiff()}"
					exit(198)
				}

				if ("`nunit'"!="") {
					if (strpos("`nunit'","((") | strpos("`nunit'","( (")) {
						di as err "invalid option {bf:nunit()};" _n " (() invalid number"
						exit(198)
					}

					local npar = strlen("`nunit'")-strlen(subinstr("`nunit'","(","",.))
					if (!`npar') {
						di as err "invalid option {bf:nunit()};" _n " at least one '(#)' required"
						exit(198)
					}
					else {
						mata: st_local("nvar",strofreal(cols(__s_svfd)))
						if (`npar'>`nvar') {
							di as err "invalid option {bf:nunit()};" _n " too many (#)"
							exit(198)
						}
					}

					local nunit = subinstr("`nunit'","( ","(",.) 
					local nunit = subinstr("`nunit'"," )",")",.) 
					local nunit = subinstr("`nunit'","()","( )",.)

					if (`npar'==1 & regexm("`nunit'", "\(([^)]+)")) {
						cap confirm number `=regexs(1)'
						if (_rc) {
							di as err "invalid {bf:nunit()} option;"
							confirm number `=regexs(1)'
						}
						local nu1 `=regexs(1)'
							
						local newnu = subinstr("`nunit'","(`nu1')","(zero)",1)
					}
					else if (`npar'==2 & regexm("`nunit'", "\(([^)]+)")) {
						cap confirm number `=regexs(1)'
						if (_rc) {
							di as err "invalid {bf:nunit()} option;"
							confirm number `=regexs(1)'
						}
						local nu1 `=regexs(1)'

						if (regexm("`=substr("`nunit'",strrpos("`nunit'","("),.)'", "\(([^)]+)")) {
							cap confirm number `=regexs(1)'
							if (_rc) {
								di as err "invalid {bf:nunit()} option;"
								confirm number `=regexs(1)'
							}
							local nu2 `=regexs(1)'
						}

						local newnu = subinstr("`nunit'","(`nu1')","(zero)",1)
						local newnu = subinstr("`newnu'","(`nu2')","(mean)",1)
					}
					else if (`npar'==3 & regexm("`nunit'", "\(([^)]+)")) {
						cap confirm number `=regexs(1)'
						if (_rc) {
							di as err "invalid {bf:nunit()} option;"
							confirm number `=regexs(1)'
						}
						local nu1 `=regexs(1)'

						if (regexm("`=substr("`nunit'",strpos("`nunit'",")")+1,strrpos("`nunit'","(")-1)'", "\(([^)]+)")) {
							cap confirm number `=regexs(1)'
							if (_rc) {
								di as err "invalid {bf:nunit()} option;"
								confirm number `=regexs(1)'
							}
							local nu2 `=regexs(1)'
						}
						if (regexm("`=substr("`nunit'",strrpos("`nunit'","("),.)'", "\(([^)]+)")) {
							cap confirm number `=regexs(1)'
							if (_rc) {
								di as err "invalid {bf:nunit()} option;"
								confirm number `=regexs(1)'
							}
							local nu3 `=regexs(1)'
						}
							
						local newnu = subinstr("`nunit'","(`nu1')","(zero)",1)
						local newnu = subinstr("`newnu'","(`nu2')","(mean)",1)
						local newnu = subinstr("`newnu'","(`nu3')","(median)",1)
					}

					cap margins in `fsamp', at(`=ustrregexra("`nunit'","\(.+?\)","")') noatlegend nose
					if (inlist(_rc,111,198,322)) {
						di as err "invalid option {bf:nunit()};"
						margins in `fsamp', at(`=ustrregexra("`nunit'","\(.+?\)","")') noatlegend nose
					}
					else if (_rc) {
						di as err "invalid option {bf:nunit()}"
						exit(198)					
					}
					else if ("`r(atstats1)'"!="") {
						mata: st_local("mimx", invtokens(strofreal(minmax(__s_fdoz:- colnonmissing(st_matrix("r(at)"))))))
						if (`:word 1 of `mimx''==-1) {
							di as err "variables in {bf:firstdiff()} and {bf:nunit()} do not match"
							exit(198)
						}
						else if (`:word 2 of `mimx''==1) {
							di as err "all {it:firstdiff} variables must be specified in {bf:nunit()}"
							exit(198)
						}
					}
					else {
						di as err "all {it:firstdiff} variables must be specified in {bf:nunit()}"
						exit(198)
					}
					
					if (inlist("0","`nu1'","`nu2'","`nu3'")) {
						noi di "one {it:n}-unit is '(0)';"
						noi di " the interaction effect is always 0 when one of the interacted variables does not change"
						cap if (`N'>_N) drop in `=`N'+1'/`=_N'
						cap mata: mata drop __s_*
						exit
					}
							
					margins in `fsamp', at(`newnu') noatlegend nose
					mata: __s_fdvl2 = tokens(st_global("r(atstats1)"))
					mata: __s_fdvl2 = subinstr(__s_fdvl2,"zero",st_local("nu1"),1)
					mata: __s_fdvl2 = subinstr(__s_fdvl2,"mean",st_local("nu2"),1)
					mata: __s_fdvl2 = subinstr(__s_fdvl2,"median",st_local("nu3"),1)						
					mata: __s_fdvl[__s_svfd] = __s_fdvl[__s_svfd]:+strtoreal(__s_fdvl2[__s_svfd])					
					
					if ("`prxb'"=="") mata: for (i=1; i<=cols(__s_fdwrt); i++) st_local("wrt"+__s_fdwrt[i],"(y|x"+__s_fdwrt[i]+"+n"+__s_fdwrt[i]+")-(y|x"+__s_fdwrt[i]+"); x"+__s_fdwrt[i]+" : "+__s_fdst[i]+", n"+__s_fdwrt[i]+" = "+__s_fdvl2[__s_svfd[i]])					
					else if ("`fdnmxb'"!="") {
						mata: st_local("fdnms", invtokens(__s_fdnm[__s_svfd]))
						local i 1
						foreach l of local fdnms {
							if ("`l'"=="`fdnmxb'") {
								if (`i'==1) {
									local wrt3 `wrt2'
									local wrt2 `wrt1'
									mata: st_local("wrt1", "(y|x1+n1)-(y|x1); x1 : "+__s_fdst[1]+", n1 = 1")
								}
								else if (`i'==2) {
									local wrt3 `wrt2'
									mata: st_local("wrt2", "(y|x2+n2)-(y|x2); x2 : "+__s_fdst[2]+", n2 = 1")
								}
								else mata: st_local("wrt3", "(y|x3+n3)-(y|x3); x3 : "+__s_fdst[3]+", n3 = 1")
								continue, break
							}
							else local i = `i'+1
						}
						local i
					}
				}
				else {
					mata: __s_fdvl[__s_svfd] = __s_fdvl[__s_svfd]:+1					
					if ("`prxb'"=="") mata: for (i=1; i<=cols(__s_fdwrt); i++) st_local("wrt"+__s_fdwrt[i],"(y|x"+__s_fdwrt[i]+"+n"+__s_fdwrt[i]+")-(y|x"+__s_fdwrt[i]+"); x"+__s_fdwrt[i]+" : "+__s_fdst[i]+", n"+__s_fdwrt[i]+" = 1")	
					else if ("`fdnmxb'"!="") {
						mata: st_local("fdnms", invtokens(__s_fdnm[__s_svfd]))
						local i 1
						foreach l of local fdnms {
							if ("`l'"=="`fdnmxb'") {
								if (`i'==1) {
									local wrt3 `wrt2'
									local wrt2 `wrt1'
									mata: st_local("wrt1", "(y|x1+n1)-(y|x1); x1 : "+__s_fdst[1]+", n1 = 1")
								}
								else if (`i'==2) {
									local wrt3 `wrt2'
									mata: st_local("wrt2", "(y|x2+n2)-(y|x2); x2 : "+__s_fdst[2]+", n2 = 1")
								}
								else mata: st_local("wrt3", "(y|x3+n3)-(y|x3); x3 : "+__s_fdst[3]+", n3 = 1")
								continue, break
							}
							else local i = `i'+1
						}
						local i
					}
				}
		
				tempname sc1 sc2 sc3 sc4 sc5 sc6
				mata: fdnuat(__s_fdgn,__s_fdiv,__s_fdnm,__s_fdvl,__s_svfd,"`sc1'","`sc2'","`sc3'","`sc4'","`sc5'","`sc6'")
			}
			else if (`ndyoz'<2) {
				di as err "not enough interacting variables specified"
				exit(198)
			}
			else if ("`nunit'"!="") {
				di as err "option {bf:nunit()} requires {bf:firstdiff()}"
				exit(198)
			}

			mata: st_local("nres",strofreal(__s_atr*__s_keq*__s_nlev))
			if ("`nres'"=="") local nres = colsof(r(b))*`keq'*`nat'
			if (`nres'>1000) {
				di as err "too many individual results requested; the limit is 1,000."
				exit(198)
			}
			else if (`nres'>100 & "`many'"=="") {
				di as err "more than 100 individual results requested; option {bf:many} required."
				exit(198)
			}

			tempname mexp
			scalar `mexp' = ""
			mata: chk_intpol(__s_vint,"`mexp'")

			// Results
			mata: __s_prord=__s_ord=__s_vl = .
			if (`inteff'==0) {
				mata: __s_mtpr=__s_qoi=__s_seq = J(0,1,.)
				if ("`fvdx'"=="") {
					if ("`prnm'"!="") {
						margins if `touse' `wght', generate(`genpr') dydx(`dydxs') `options' `fdat' `nuat'						
						scalar `genpr' = "`genpr'"
						mataput, tname(`genpr')
					}
					else margins if `touse' `wght', dydx(`dydxs') `options' `fdat' `nuat'
				}
				else {
					if ("`prnm'"!="") {		
						margins `dnm1' if `touse' `wght', generate(`genpr') `options' `fdat' `nuat'
						scalar `genpr' = "`genpr'"
						mataput, tname(`genpr')
					}
					margins r.`dnm1' if `touse' `wght', contrast(nowald) `options' `fdat' `nuat'
					if ("`prnm'"!="") mata: __s_mtpr = __s_mtpr*st_matrix("r(L)")'
				}
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
			}
			else if (`inteff'==1) {
				if ("`prnm'"!="") {
					margins `dnm1'#`dnm2' if `touse' `wght', generate(`genpr') `options' `fdat' `nuat'
					scalar `genpr' = "`genpr'"
					mataput, tname(`genpr')
				}
				margins r.`dnm1'#r.`dnm2' if `touse' `wght', contrast(nowald) `options' `fdat' `nuat'
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
				mata: vslabel(__s_prord,__s_ord,__s_vl)
				if ("`fdxs'"=="") mata: __s_vlab = __s_vl[,1]:+__s_vl[,2]:+".x2)    "
				else mata: __s_vlab = __s_vl[,1]:+__s_vl[,2]:+ ".x2)#∆(x3)      "
			}
			else if (`inteff'==2) {
				if ("`prnm'"!="") {
					margins `dnm1' if `touse' `wght', generate(`genpr') dydx(`dnm2') `options' `fdat' `nuat'
					scalar `genpr' = "`genpr'"
					mataput, tname(`genpr')
				}
				margins r.`dnm1' if `touse' `wght', dydx(`dnm2') contrast(nowald) `options' `fdat' `nuat'
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
				mata: vslabel(__s_prord,__s_ord,__s_vl)			
				if ("`fdxs'"=="") mata: __s_vlab = __s_vl[,1]:+"x2)    "
				else mata: __s_vlab = __s_vl[,1]:+ "x2)#∆(x3)      "
			}
			else if (`inteff'==3) {
				if ("`fdxs'"=="") {
					margins if `touse' `wght', generate(`prnm') expression(`=`mexp'') dydx(`dnm1') `options'			
					if (`nat'==1) mata: __s_vlab = "∆(x1)#∆(x2)    "
					else mata: __s_vlab = subinstr(st_matrixcolstripe("r(b)")[,2],"bn.",".",1) :+"#∆(x1)#∆(x2)    "
				}
				else {
					margins if `touse' `wght', generate(`genpr') expression(`=`mexp'') dydx(`dnm1') `options' `fdat' `nuat'
					if ("`prnm'"!="") {
						scalar `genpr' = "`genpr'"
						mataput, tname(`genpr')
					}
					if (`nat'==1) mata: __s_vlab = "∆(x1)#∆(x2)#∆(x3)      "
					else mata: __s_vlab = subinstr(st_matrixcolstripe("r(b)")[1::(cols(st_matrix("r(b)"))/2),2],"bn.",".",1) :+"#∆(x1)#∆(x2)#∆(x3)      "
				}
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
			}
			else if (`inteff'==4) {
				if ("`prnm'"!="") {
					margins `dnm1'#`dnm2'#`dnm3' if `touse' `wght', generate(`genpr') `options'
					scalar `genpr' = "`genpr'"
					mataput, tname(`genpr')
				}
				margins r.`dnm1'#r.`dnm2'#r.`dnm3' if `touse' `wght', contrast(nowald) `options'		
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
				mata: vslabel(__s_prord,__s_ord,__s_vl)
				mata: __s_vlab = __s_vl[,1]:+__s_vl[,2]:+".x2)#∆(":+__s_vl[,3]:+".x3)      "
			}
			else if (`inteff'==5) {
				if ("`prnm'"!="") {
					margins `dnm1'#`dnm2' if `touse' `wght', generate(`genpr') dydx(`dnm3') `options'
					scalar `genpr' = "`genpr'"
					mataput, tname(`genpr')
				}
				margins r.`dnm1'#r.`dnm2' if `touse' `wght', dydx(`dnm3') contrast(nowald) `options'
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
				mata: vslabel(__s_prord,__s_ord,__s_vl)
				mata: __s_vlab = __s_vl[,1]:+__s_vl[,2]:+".x2)#∆(":+"x3)      "
			}
			else if (`inteff'==6) {
				if ("`prnm'"!="") {
					margins `dnm1' if `touse' `wght', expression(`=`mexp'') dydx(`dnm2') generate(`genpr') `options'
					scalar `genpr' = "`genpr'"
					mataput, tname(`genpr')
				}
				margins r.`dnm1' if `touse' `wght', expression(`=`mexp'') dydx(`dnm2') contrast(nowald) `options'
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
				mata: vslabel(__s_prord,__s_ord,__s_vl)
				mata: __s_vlab = __s_vl[,1]:+"x2)#∆(x3)      "				
			}
			else {
				margins if `touse' `wght', generate(`prnm') expression(`=`mexp'') dydx(`dnm1') `options'
				mata: if (max(st_matrix("r(V)"))==.) st_local("vmat","Variance matrix is nonsymmetric or highly singular.") ;;
				if (`nat'==1) mata: __s_vlab = "∆(x1)#∆(x2)#∆(x3)      "
				else mata: __s_vlab = subinstr(st_matrixcolstripe("r(b)")[,2],"bn.",".",1) :+"#∆(x1)#∆(x2)#∆(x3)      "
			}

			mata: __s_rb = st_matrix("r(b)")
			mata: __s_rV = st_matrix("r(V)")
			mata: __s_colbV = J(cols(__s_rb),1,"") , (J(cols(__s_rb),1,"_inteff"):+ strofreal(1::cols(__s_rb)))
		
			if ("`fdxs'"!="") {
				mata: __s_qoi = (st_matrix("r(b)")*__s_Lat)'
				mata: __s_seq = diagonal(sqrt(__s_Lat'*st_matrix("r(V)")*__s_Lat))
				if ("`ord'"=="1") {
					mata: _collate(__s_qoi,__s_ord)
					mata: _collate(__s_seq,__s_ord)
				}
			}
			
			if ("`prnm'"!="" & "`fvdx'"=="1") {
				if (colsof(r(b))<rowsof(r(L))) { 
					mata: __s_L = selectindex(colmax(st_matrix("r(L)"):!=0))
					mata: __s_L = (st_matrix("r(L)")[,__s_L])'
				}
				else mata __s_L = st_matrix("r(L)")'

				if (`keq'>1) {
					if ("`fdxs'"=="") mata __s_g=__s_Lat = .
					mata L(__s_atr,__s_g,__s_keq,__s_L,__s_Lat,__s_nlev)
				}
			}

			mata: __s_svy = J(1,3,.)
			if ("`r(df_r)'"!="") mata: __s_svy[1] = st_numscalar("r(df_r)")
			if ("`r(N_psu)'"!="") mata: __s_svy[2] = st_numscalar("r(N_psu)")
			if ("`r(N_strata)'"!="") mata: __s_svy[3] = st_numscalar("r(N_strata)")

			if ("`nolegend'"=="") {
				mata: st_local("rno",strofreal(__s_rN))
				local len = strlen("`rno'")
				if (`len'>3) local rno = string(`rno',"%-"+"`=`len'+int(`len'/3)+2'"+".0gc")

				tempvar pra prb prc
				gen `pra' = ""
				gen `prb' = ""
				gen `prc' = ""
				if (_N<`keq') set obs `keq' 
				if ("`r(predict2_label)'"=="") {
					replace `pra' = "Expression" in 1
					replace `prb' = ":" in 1				
					if (!inlist(`inteff',3,6,7)) {
						if ("`r(predict_label)'"!="") replace `prc' = r(predict_label) + ", " + r(expression) in 1
						else if ("`r(predict1_label)'"!="") replace `prc' = r(predict1_label) + ", " + r(expression) in 1
						else replace `prc' = r(expression) in 1
					}
					else replace `prc' = "`r_prc'" in 1
				}
				else {
					local rexp = substr(r(expression),1,strlen(r(expression))-1) 
					replace `pra' = string(_n) + "._pr" in 1/`keq'
					replace `prb' = ":" in 1/`keq'
					forval i = 1/`keq' {
						replace `prc' = r(predict`i'_label) + ", `rexp'" + r(predict`i'_opts) + ")" in `i'
					}
				}
				format `pra' %-14s
				format `prb' %-1s
				format `prc' %-`=strlen(`prc'[`keq'])'s
			}

			tempvar	atlv atname ats ceq hi_sdi lo_sdi quant ra rb rc statis std_err teq cln
			tempname lbl
			mata: __s_rlab = 1::rows(__s_vlab)
			mata: st_vlmodify("`lbl'", __s_rlab, __s_vlab)
			mata: st_local("nlab",strofreal(rows(__s_rlab)))
	
			local df = el(r(table),7,1)
			if (`df'==.) local df 0
			mata: __s_rwst = st_matrixrowstripe("r(table)")
			if ("`fdxs'"=="") {	
				mata: __s_qoi = st_matrix("r(table)")
				mata: __s_gm = __s_qoi[(1,2,5,6),]'
				if ("`ord'"=="1") {
					mata: __s_qoi = __s_qoi[,__s_ord]
					mata: _collate(__s_gm,__s_ord)
				}
				getmata `quant'=__s_rlab (`statis' `std_err' `lo_sdi' `hi_sdi')=__s_gm, force replace
			}
			else {		
				if (!`df') mata: __s_crt = invnormal(1-.5*(1-(strtoreal(st_local("level"))/100)))
				else mata: __s_crt = invttail(st_matrix("r(table)")[7,1],(.5*(1-(strtoreal(st_local("level"))/100))))			
				mata: __s_gm = __s_qoi , __s_seq , (__s_qoi:-__s_seq*__s_crt) , (__s_qoi:+__s_seq*__s_crt)
				getmata `quant'=__s_rlab (`statis' `std_err' `lo_sdi' `hi_sdi')=__s_gm, force replace
				mata: __s_qoi = J(8,rows(__s_gm),.)
				mata: __s_qoi[(1,2,5,6),] = __s_gm'
				mata: __s_qoi[3,] = __s_qoi[1,]:/__s_qoi[2,]
				if (!`df') mata: __s_qoi[4,] = 2*(normal(-abs(__s_qoi[3,])))
				else {
					mata: __s_qoi[7,] = J(1,cols(__s_qoi),strtoreal(st_local("df")))
					mata: __s_qoi[4,] = 2*ttail(__s_qoi[7,],abs(__s_qoi[3,]))
				}
				mata: __s_qoi[8,] = J(1,cols(__s_qoi),__s_crt)
			}
			mata: __s_clst = J(cols(__s_qoi),1,""),(strofreal(1::cols(__s_qoi)):+"._no")
			
			if ("`prnm'"!="" & "`fvdx'"=="1") {
				if ("`fdxs'"=="") mata: __s_newpr = __s_mtpr* __s_L
				else mata: __s_newpr = (__s_mtpr* __s_L)*__s_Lat
				if ("`ord'"=="1") mata: __s_newpr=__s_newpr[,__s_ord]
				cap getmata (`prnm'*)=__s_newpr, double
				if (_rc==110) {
					di as err "a variable named {it:`prnm'#} already exists;" _n " specify a different stub name in {bf:obseff()}"
					exit(198)
				}
				else if (_rc) {
					di as err "invalid option {bf:obseff()}"
					exit(198)
				}
			}

			noi di as text _newline "{bf:Interaction Effects}"

			if ("`nolegend'"=="") {
					
				if ("`rats1'"!="") {
					getmata `ats'=__s_ats `atname'=__s_atname `atlv'=__s_atlv, force
					gen `cln' = ":" in 1/`ato'
					gen `teq' = "=" in 1/`ato'
					replace `teq' = ":" if substr(`atlv',1,1)=="("
					
					format `ats' %-14s
					local len = strlen(`atname'[1])
					if (`len'>15) local len=15
					format `atname' %-`len's
					local fmt: format `atlv'
					local fmt: subinstr local fmt "%" "%-"
					format `atlv' `fmt'
					format `cln' %-1s
					format `teq' %-1s
				}
				else mata: st_rclear()				
					
				gen `ra' = ""
				gen `rb' = ""
				gen `rc' = ""
				format `ra' %-15s
				format `rb' %-27s
				format `rc' %-1s
					
				if ("`wrt3'"=="") {
					if (_N<5) set obs 5			
					replace `ra' = "`d1'" in 3
					replace `rb' = "`wrt1'" in 3
					replace `ra' = "`d2'" in 4
					replace `rb' = "`wrt2'" in 4
					replace `ra' = "Number of obs" in 5
					replace `rb' = "`rno'" in 5	
					replace `rc' = ":" in 1/4
					replace `rc' = "=" in 5
					local nra 5
				}
				else {
					if (_N<6) set obs 6			
					replace `ra' = "`d1'" in 3
					replace `rb' = "`wrt1'" in 3
					replace `ra' = "`d2'" in 4
					replace `rb' = "`wrt2'" in 4
					replace `ra' = "`d3'" in 5
					replace `rb' = "`wrt3'" in 5
					replace `ra' = "Number of obs" in 6
					replace `rb' = "`rno'" in 6
					replace `rc' = ":" in 1/5
					replace `rc' = "=" in 6
					local nra 6
				}
				replace `ra' = "Statistic" in 1
				replace `rb' = "Average interaction effect" in 1
				replace `ra' = "Standard error" in 2
				if ("`vce'"=="unconditional") replace `rb' = "Linearized" in 2
				else replace `rb' = "Delta-method" in 2

				noi flist `ra' `rc' `rb' in 1/`nra', noobs noh clean

				noi di as text ""
				noi flist `pra' `prb' `prc' in 1/`keq', noobs noh clean

				if ("`rats1'"!="") {
					noi di as text ""
					noi flist `ats' `cln' `atname' `teq' `atlv' in 1/`ato', noobs noh clean
				}
			}
			
				
			label var `quant' "`=char(13)'"
			label values `quant' `lbl'
			label var `statis' "Statistic"
			label var `std_err' "Std. Err."
			label var `lo_sdi' "[`level'% Conf."
			label var `hi_sdi' "Interval]"

			noi tabdis `quant' in 1/`nlab', c(`statis' `std_err' `lo_sdi' `hi_sdi' `levtype') left cellwidth(13)
			if ("`fvdx'"!="") {
				noi di as text "Note: dy/dx for factor levels is the discrete change from the base level."
				if ("`vmat'"!="") noi di as text "      `vmat'"
			}
			else if ("`vmat'"!="") noi di as text "Note: `vmat'"

			mata: st_rclear()			
			mata: st_global("r(cmd)","ginteff")
			mata: st_global("r(cmdline)",st_local("cmdl"))
			mata: st_global("r(est_cmd)",st_global("e(cmd)"))
			mata: st_global("r(est_cmdline)",st_global("e(cmdline)"))
			mata: st_global("r(model_vce)",st_global("e(vce)"))
			if ("`prnm'"!="") mata: st_global("r(obseff)", invtokens(st_local("prnm"):+ strofreal(J(1,1,1..rows(__s_clst)))))
			if ("`vce'"!="") mata: st_global("r(vce)",st_local("vce"))
			else mata: st_global("r(vce)","delta")
			mata: st_numscalar("r(level)",strtoreal(st_local("level")))
			mata: st_numscalar("r(N)",__s_rN)
			mata: if (__s_svy[1]!=.) st_numscalar("r(df_r)",__s_svy[1]) ;;
			mata: if (__s_svy[2]!=.) st_numscalar("r(N_psu)",__s_svy[2]) ;;
			mata: if (__s_svy[3]!=.) st_numscalar("r(N_strata)",__s_svy[3]) ;;
			mata: st_matrix("r(b)",__s_rb)
			mata: st_matrixrowstripe("r(b)",("","inteff"))
			mata: st_matrixcolstripe("r(b)",__s_colbV)
			mata: st_matrix("r(V)",__s_rV)
			mata: st_matrixrowstripe("r(V)",__s_colbV)
			mata: st_matrixcolstripe("r(V)",__s_colbV)
			if (`df') {
				mata: st_matrix("r(ginteff)",__s_qoi[(1..8),]')
				mata: st_matrixrowstripe("r(ginteff)",__s_clst)
				mata: st_matrixcolstripe("r(ginteff)",__s_rwst[(1..8),])
			}
			else {
				mata: st_matrix("r(ginteff)",__s_qoi[(1..6,8),]')
				mata: st_matrixrowstripe("r(ginteff)",__s_clst)
				mata: st_matrixcolstripe("r(ginteff)",__s_rwst[(1..6,8),])
			}

			if ("`rats1'"!="") {
				mata: st_matrix("r(at)",st_matrix("`rat'"))
				mata: st_matrixrowstripe("r(at)",st_matrixrowstripe("`rat'"))
				mata: st_matrixcolstripe("r(at)",st_matrixcolstripe("`rat'"))
				mata: st_global("r(atstat)",st_local("ratst"))
			}
			if ("`fdxs'"!="") {
				mata: st_global("r(fdstats)",__s_rfdst)
					
				mata: st_matrix("r(fd)",__s_rfd)
				mata: __s_clst = J(cols(__s_rfd),1,""),__s_fdnm[__s_svfd]'
				mata: st_matrixcolstripe("r(fd)",__s_clst)
				mata: st_matrixrowstripe("r(fd)",("","1._fd"))
					
				if ("`nunit'"=="") mata: st_matrix("r(nunit)",J(1,cols(__s_fdwrt),1))
				else mata: st_matrix("r(nunit)",strtoreal(__s_fdvl2[__s_svfd]))
				mata: st_matrixcolstripe("r(nunit)",__s_clst)
				mata: st_matrixrowstripe("r(nunit)",("","1._nunit"))
			}
			if ("`post'"!="") {				
				tempname b V df_r N_psu N_strata

				mat `b' = r(b)
				mat `V' = r(V)
				eret post `b' `V', esample(`touse') obs(`r(N)') properties("b V")
									
				eret local cmd "ginteff"
				mata: if (__s_svy[1]!=.) st_numscalar("`df_r'",__s_svy[1]) ;;
				mata: if (__s_svy[2]!=.) st_numscalar("`N_psu'",__s_svy[2]) ;;
				mata: if (__s_svy[3]!=.) st_numscalar("`N_strata'",__s_svy[3]) ;;
				cap eret scalar df_r = scalar(`df_r')
				cap eret scalar N_psu = scalar(`N_psu')
				cap eret scalar N_strata = scalar(`N_strata')
			}
		}
	}

	if (_rc) {
		local rc = _rc
		cap if (`N'>_N) drop in `=`N'+1'/`=_N'
		cap mata: mata drop __s_*
		mata: st_rclear()
		exit(`rc')
	}
	else {
		tempname myr 
		_return hold `myr'
		cap if (`N'>_N) drop in `=`N'+1'/`=_N'
		cap mata: mata drop __s_*
		_return restore `myr'
	}

end


program mataput, rclass
	version 14

	syntax, tname(name)
    return add
	putmata __s_mtpr=(`tname'*), replace
end


version 14

mata

	void atdx(__s_vint) {
		cl = st_matrixcolstripe("r(at)")[,2]
		tkn = tokens(st_global("r(atstats1)"))'
		tv = substr(tkn,1,1):=="v"
		if (any(tv)) tkn[selectindex(tv)] = " = ":+st_matrix("r(at)")[selectindex(tv)] ;;
		ts = !tv :* (tkn:!="asobserved")
		if (any(ts)) tkn[selectindex(ts)] = " at (":+tkn[selectindex(ts)]:+")" ;;
		if (any(strpos(cl,"."))) cl = (substr(cl,strpos(cl,"."):+1,.) , tkn)[selectindex(tkn:!="asobserved"),]
		else cl = (cl,tkn)[selectindex(tkn:!="asobserved"),]
			
		info = panelsetup(cl,1)
		cl = cl[info[,1],]
			
		for (i=strtoreal(st_local("stvi")); i<=cols(__s_vint); i++) {
			if (anyof(cl[,1],__s_vint[i])) {
				st_local("wrt"+strofreal(i), st_local("wrt"+strofreal(i)) + cl[selectindex(cl[,1]:==__s_vint[i]),2])
			}
			else st_local("wrt"+strofreal(i), st_local("wrt"+strofreal(i)) + " (asobserved)")
		}
	}

	void atinteff(__s_atlv,__s_atname,__s_ats,__s_atoz,__s_fvar,__s_slb) {
		__s_atlv = st_matrix("r(at)")
		atr = rows(__s_atlv)
		cna = st_matrixcolstripe("r(at)")[,2]
		cna = subinstr(cna,"b.",".",.)
	
		if (__s_fvar[1]!=.) {
			fvls = ((__s_slb:=="base") :+ (__s_slb:=="value") :+ (__s_slb:=="values")):*(__s_fvar:*__s_atoz)
			baln = (__s_slb:=="asbalanced"):*(__s_fvar:*__s_atoz)

			if (any(fvls)) {
				ind = strtoreal(substr(cna,1,strpos(cna,"."):-1))
				id = substr(cna,strpos(cna,"."):+1,.)
				info = panelsetup(id,1,2)
				info = info[selectindex(fvls[info[,1]]),]
				info = info,(info[,2]:-1),(info[,2]-info[,1])
				ri = rows(info)
				
				for (i=1; i<=ri; i++) {
					__s_atlv[|1,info[i,1]\atr,info[i,2]|] = __s_atlv[|1,info[i,1]\atr,info[i,2]|]:*ind[|info[i,1]\info[i,2]|]'
					__s_atlv[,info[i,2]] = rowsum(__s_atlv[|1,info[i,1]\atr,info[i,2]|])
					cna[info[i,2]] = substr(cna[info[i,2]],strpos(cna[info[i,2]],"."):+1,.)
					__s_atoz[|info[i,1]\info[i,3]|] = J(1,info[i,4],0)
				}
			}
			else id = ""
			
			if (any(baln)) {
				if (id[1]=="") id = substr(cna,strpos(cna,"."):+1,.) ;;
				info = panelsetup(id,1,2)
				info = info[selectindex(baln[info[,1]]),]
				info = info,(info[,2]:-1),(info[,2]-info[,1])
				ri = rows(info)
				
				for (i=1; i<=ri; i++) {
					cna[info[i,2]] = substr(cna[info[i,2]],strpos(cna[info[i,2]],"."):+1,.)
					__s_atoz[|info[i,1]\info[i,3]|] = J(1,info[i,4],0)
				}
				baln = baln:*__s_atoz
			};
		};

		vals = ((__s_slb:=="value"):+(__s_slb:=="values")):*__s_atoz
		gnlb = (substr(__s_slb,1,1):=="("):*__s_atoz
		if (any(gnlb)) __s_slb[selectindex(gnlb)] = substr(__s_slb[selectindex(gnlb)],2,strlen(__s_slb[selectindex(gnlb)]):-2) ;;
		if (__s_fvar[1]!=.) {
			__s_slb[selectindex(baln)] = "(":+__s_slb[selectindex(baln)]:+")"
			gnlb = gnlb:+baln
		};		
		stlb = __s_atoz:-(vals:+gnlb)
		__s_slb[selectindex(vals)] = J(1,sum(vals),"")
		__s_slb[selectindex(stlb)] = " (":+__s_slb[selectindex(stlb)]:+")"

		selc = selectindex(__s_atoz)
		if (any(gnlb)) {
			__s_atlv = strofreal(__s_atlv)
			__s_atlv[,selectindex(gnlb)] = J(rows(__s_atlv),cols(selectindex(gnlb)),"")
			__s_atlv = colshape(__s_atlv[,selc],1)+J(atr,1,__s_slb[selc]')
		}
		else __s_atlv = colshape(strofreal(__s_atlv[,selc]),1)+J(atr,1,__s_slb[selc]')
		st_local("ato",strofreal(rows(__s_atlv)))
		__s_atname = J(atr,1,cna[selc])

		atc = cols(selc)
		if (atc==1 & atr==1) __s_ats = "at"
		else if (atc>1 & atr==1) __s_ats = "at"\J(atc-1,1,"")
		else if (atc==1 & atr>1) __s_ats = strofreal(1::atr):+"._at"
		else {
			__s_ats = strofreal(1::atr):+"._at",J(atr,atc-1,"")
			__s_ats = colshape(__s_ats,1)
		}
	}

	void chk_intpol(__s_vint,string scalar me) {
		if (st_local("inteq")!="") {
			seleq = st_matrixcolstripe("e(b)")[,1]
			if (st_local("numeq")=="") seleq = selectindex(seleq:==st_local("inteq"))
			else {
				info = panelsetup(seleq,1)						
				seleq = selectindex(seleq:==seleq[info[strtoreal(st_local("inteq")),1]])
			}

			s_cl = st_matrixcolstripe("e(b)")[seleq,2]			
		}
		else {
			s_cl = selectindex(st_matrix("e(b)"))
			s_cl = st_matrixcolstripe("e(b)")[s_cl,2]
		}

		s_hash = strlen(s_cl):- strlen(subinstr(s_cl,"#","",.))
		rcl = rows(s_cl)
		s_sel = J(rcl,max(s_hash)+1,"")
		for (i=1; i<=rcl; i++) s_sel[|i,1\i,(s_hash[i]+1)|] = tokens(s_cl[i,],"#")[range(1,s_hash[i]*2+1,2)']
		s_sel = substr(s_sel,editvalue(strpos(s_sel,"c."),0,-1):+2,.)
		s_sel = subinstr(s_sel,"b.",".",.)

		ncv = cols(__s_vint)
		cl = J(rcl,0,.)
		if (max(strpos(s_sel,"."))) for (i=1; i<=ncv; i++) cl = cl,rowsum(__s_vint[i]:==substr(s_sel,strpos(s_sel,"."):+1,.))
		else for (i=1; i<=ncv; i++) cl = cl,rowsum(__s_vint[i]:==s_sel)
		cmx = colmax(cl)
		if (ncv==2) mult = cl[,1]:* cl[,2]
		else mult = cl[,1]:* cl[,2]:* cl[,3]
		mult = selectindex(mult)
		nrm = rows(mult)
		if (nrm>=1) {
			dup  = s_sel[mult,]'
			dup2 = J(1,nrm,.)
			for (i=1; i<=nrm; i++) dup2[i] = rows(uniqrows(select(dup[,i], strlen(dup[,i]):>0)))
			if (max(dup2)>ncv) {
				if (ncv==2) {
					if (st_local("intequation")!="") errprintf("in equation(" +st_local("intequation")+ ")\n ") ;;
					errprintf("the interaction between '"+__s_vint[1]+"' and '"+__s_vint[2]+"' is not a two-way interaction\n")
					exit(198)
				}
				else {
					if (st_local("intequation")!="") errprintf("in equation(" +st_local("intequation")+ ")\n ") ;;
					errprintf("the interaction between '"+__s_vint[1]+"', '"+__s_vint[2]+"', and '"+__s_vint[3]+"' is not a three-way interaction\n")
					exit(198)
				}
			};
		}
		else if (!nrm | !cols(mult)) {
			if (ncv==2) {
				if (st_local("intequation")!="") errprintf("in equation(" +st_local("intequation")+ ")\n ") ;;
				errprintf("variables '"+__s_vint[1]+"' and '"+__s_vint[2]+"' are not interacted\n")
				exit(198)
			}
			else {
				if (st_local("intequation")!="") errprintf("in equation(" +st_local("intequation")+ ")\n ") ;;
				errprintf("variables '"+__s_vint[1]+"', "+__s_vint[2]+"', and '"+__s_vint[3]+"' are not interacted\n")
				exit(198)
			}
		};

		inteff = strtoreal(st_local("inteff"))
		if (anyof((3,6,7),inteff)) {
			if (max(cmx)==1) {
				if (inteff!=7) {
					msel = selectindex(cl[,2])
					mexp = s_sel[msel,]
					mexp = subinstr(mexp,__s_vint[2],"",.)
					for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
					mexp = mexp[,1]
					sexp = selectindex(mexp:!="")
					mexp[sexp] = mexp[sexp]:+ "*"
					mexp2 = mexp:+ "_b[":+s_cl[msel]:+"]"
				}
				else {
					msel = selectindex(cl[,2])
					mexp = s_sel[msel,]
					mexp = subinstr(mexp,__s_vint[2],"",.)
					for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
					mexp = mexp[,1]
					sexp = selectindex(mexp:!="")
					mexp[sexp] = mexp[sexp]:+ "*"
					mexp2 = mexp:+ "_b[":+s_cl[msel]:+"]"
					
					msel = selectindex(cl[,3])
					mexp = s_sel[msel,]
					mexp = subinstr(mexp,__s_vint[3],"",.)
					for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
					mexp = mexp[,1]
					sexp = selectindex(mexp:!="")
					mexp[sexp] = mexp[sexp]:+ "*"
					mexp3 = mexp:+ "_b[":+s_cl[msel]:+"]"
					
					bsel = selectindex(cl[,2]:* cl[,3])
					bint = "(_b["+s_cl[bsel[selectindex(bsel:!=mult)]]+"]+" +__s_vint[1]+ "*_b["+s_cl[mult]+"])"
				}
			}
			else {
				if (cols(selectindex(cmx:>1))==1) {
					if (inteff!=7) {
						if (cmx[1]!=1) sidx = 2
						else {
							sidx = 1
							if (inteff==3) st_local("dnm1",__s_vint[2])
							else st_local("dnm2",__s_vint[2])
						}

						msel = selectindex(cl[,sidx])
						mexp = s_sel[msel,]
						mexp = subinstr(mexp,__s_vint[sidx],"",.)
						for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
						mexp = mexp[,1]
						sexp = selectindex(mexp:!="")
						mexp[sexp] = mexp[sexp]:+ "*"
						mexp2 = mexp:+ "_b[":+s_cl[msel]:+"]"
					}
					else {
						if (cmx[1]!=1) sidx = 2,3
						else {
							sidx = selectindex(cmx:==1)
							st_local("dnm1",__s_vint[selectindex(cmx:>1)])
						}
						
						msel = selectindex(cl[,sidx[1]])
						mexp = s_sel[msel,]
						mexp = subinstr(mexp,__s_vint[sidx[1]],"",.)
						for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
						mexp = mexp[,1]
						sexp = selectindex(mexp:!="")
						mexp[sexp] = mexp[sexp]:+ "*"
						mexp2 = mexp:+ "_b[":+s_cl[msel]:+"]"
						
						msel = selectindex(cl[,sidx[2]])
						mexp = s_sel[msel,]
						mexp = subinstr(mexp,__s_vint[sidx[2]],"",.)
						for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
						mexp = mexp[,1]
						sexp = selectindex(mexp:!="")
						mexp[sexp] = mexp[sexp]:+ "*"
						mexp3 = mexp:+ "_b[":+s_cl[msel]:+"]"
					
						bsel = selectindex(cl[,sidx[1]]:* cl[,sidx[2]])
						if (nrm==1) {
							pwr = strofreal((strlen(s_cl[mult])-strlen(subinstr(s_cl[mult],__s_vint[selectindex(cmx:>1)],"",.)))/strlen(__s_vint[selectindex(cmx:>1)]))
							bint = "(_b["+s_cl[bsel[selectindex(bsel:!=mult)]]+"]+" +__s_vint[selectindex(cmx:>1)]+"^"+ pwr + "*_b["+s_cl[mult]+"])"
						}
						else {
							bint = s_sel[bsel,]
							bint = subinstr(bint,__s_vint[sidx[1]],"",.)
							bint = subinstr(bint,__s_vint[sidx[2]],"",.)
							for (i=1; i<=rows(bsel); i++) bint[i,1] = invtokens(select(bint[i,], strlen(bint[i,]):>0), "*")
							bint = bint[,1]
							sbin = selectindex(bint:!="")
							bint[sbin] = bint[sbin]:+ "*"
							bint = bint:+ "_b[":+s_cl[bsel]:+"]"
							bint = "("+ invtokens(bint',"+") +")"
						}
					}
				}
				else if (cols(selectindex(cmx:>1))==2) {
					if (inteff!=7) sidx = 2
					else {
						if (cmx[1]!=1) {
							sidx = selectindex(cmx:!=1)[2]
							sidx1 = selectindex(cmx:==1)
						}
						else {
							sidx = 3
							sidx1 = 1
							st_local("dnm1",__s_vint[2])
						}

						msel = selectindex(cl[,sidx1])
						mexp = s_sel[msel,]
						mexp = subinstr(mexp,__s_vint[sidx1],"",.)
						for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
						mexp = mexp[,1]
						sexp = selectindex(mexp:!="")
						mexp[sexp] = mexp[sexp]:+ "*"
						mexp3 = mexp:+ "_b[":+s_cl[msel]:+"]"
					}
						
					msel = selectindex(cl[,sidx]:==1)
					if (rows(msel)) {
						mexp = s_sel[msel,]
						mexp = subinstr(mexp,__s_vint[sidx],"",.)
						
						if (inteff==7){
							if (rows(selectindex((cl[,sidx]:==1):* cl[,sidx1]))) {
								bsel = J(rows(msel),1,.)
								bsel1 = selectindex((cl[,sidx]:==1):* cl[,sidx1])
								for (i=1; i<=rows(bsel); i++) bsel[i] = anyof(bsel1,msel[i])
								bsel = selectindex(bsel)
								bint1 = mexp[bsel,]
								bint1 = subinstr(bint1,__s_vint[sidx1],"",.)		
								for (i=1; i<=rows(bsel); i++) bint1[i,1] = invtokens(select(bint1[i,], strlen(bint1[i,]):>0), "*")
								bint1 = bint1[,1]
								sbin = selectindex(bint1:!="")
								bint1[sbin] = bint1[sbin]:+ "*"
								bint1 = bint1:+ "_b[":+s_cl[msel[bsel]]:+"]"
							};
						};

						for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
						mexp = mexp[,1]
						sexp = selectindex(mexp:!="")
						mexp[sexp] = mexp[sexp]:+ "*"
						mexp2 = mexp:+ "_b[":+s_cl[msel]:+"]"
					}
					else mexp2=bint1 = J(0,1,"")
						
					msel = selectindex(cl[,sidx]:>1)
					mexp = s_sel[msel,]'
					nc = rows(msel)
					for (i=1; i<=nc; i++) {
						_sort(mexp,i)	
						info = panelsetup(mexp[,i],1)
						decl = selectindex(mexp[info[,1],i]:==__s_vint[sidx])
						ndcl = info[decl,2]-info[decl,1]
						if (ndcl==1) mexp[info[decl,1],i] = "2*"+__s_vint[sidx]
						else mexp[info[decl,1],i] = strofreal(ndcl+1)+"*"+__s_vint[sidx]+"^"+strofreal(ndcl)
						mexp[|(info[decl,1]+1),i\info[decl,2],i|] = J(ndcl,1,"")
					}
					mexp = mexp'

					if (inteff==7) {
						if (rows(selectindex((cl[,sidx]:>1):* cl[,sidx1]))) {
							bsel = J(rows(msel),1,.)
							bsel1 = selectindex((cl[,sidx]:>1):* cl[,sidx1])
							for (i=1; i<=rows(bsel); i++) bsel[i] = anyof(bsel1,msel[i])
							bsel = selectindex(bsel)
							bint = mexp[bsel,]
							bint = subinstr(bint,__s_vint[sidx1],"",.)		
							for (i=1; i<=rows(bsel); i++) bint[i,1] = invtokens(select(bint[i,], strlen(bint[i,]):>0), "*")
							bint = bint[,1]
							sbin = selectindex(bint:!="")
							bint[sbin] = bint[sbin]:+ "*"
							bint = bint1 \ (bint:+ "_b[":+s_cl[msel[bsel]]:+"]")
							bint = "("+ invtokens(bint',"+") +")"
						}
						else bint = "("+ invtokens(bint1',"+") +")"
					};			
						
					for (i=1; i<=rows(mexp); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
					mexp = mexp[,1]
					sexp = selectindex(mexp:!="")
					mexp[sexp] = mexp[sexp]:+ "*"
					mexp2 = mexp2 \ (mexp:+ "_b[":+s_cl[msel]:+"]")
				}
				else {
					for (j=2; j<=3; j++) {
						msel = selectindex(cl[,j]:==1)
						if (rows(msel)) {
							mexp = s_sel[msel,]
							mexp = subinstr(mexp,__s_vint[j],"",.)
							
							if (j==3) {
								if (rows(selectindex((cl[,2]:==1):* (cl[,3]:==1)))) {
									bsel = J(rows(msel),1,.)
									bsel1 = selectindex((cl[,2]:==1):* (cl[,3]:==1))
									for (i=1; i<=rows(bsel); i++) bsel[i] = anyof(bsel1,msel[i])
									bsel = selectindex(bsel)
									bint1 = mexp[bsel,]
									bint1 = subinstr(bint1,__s_vint[2],"",.)		
									for (i=1; i<=rows(bsel); i++) bint1[i,1] = invtokens(select(bint1[i,], strlen(bint1[i,]):>0), "*")
									bint1 = bint1[,1]
									sbin = selectindex(bint1:!="")
									bint1[sbin] = bint1[sbin]:+ "*"
									bint1 = bint1:+ "_b[":+s_cl[msel[bsel]]:+"]"
								}
								else bint1 = J(0,1,"")

								if (rows(selectindex((cl[,2]:>1):* (cl[,3]:==1)))) {
									bsel = J(rows(msel),1,.)
									bsel0 = selectindex((cl[,2]:>1):* (cl[,3]:==1))
									for (i=1; i<=rows(bsel); i++) bsel[i] = anyof(bsel0,msel[i])
									bsel = selectindex(bsel)
									bsel0 = msel[bsel]						
									bint0 = mexp[bsel,]'
									_sort(bint0,1..rows(bsel0))
								}
								else {
									bsel0 = J(0,1,.)
									bint0 = J(1,0,"")
								}
							};
							
							for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
							mexp = mexp[,1]
							sexp = selectindex(mexp:!="")
							mexp[sexp] = mexp[sexp]:+ "*"
							if (j==2) mexp2 = mexp:+ "_b[":+s_cl[msel]:+"]"
							else mexp3 = mexp:+ "_b[":+s_cl[msel]:+"]"
						}
						else if (j==2) mexp2 = J(0,1,"")
						else {
							mexp3=bint1 = J(0,1,"")
							bsel0 = J(0,1,.)
							bint0 = J(1,0,"")
						}

						msel = selectindex(cl[,j]:>1)
						mexp = s_sel[msel,]'
						nc = rows(msel)
						for (i=1; i<=nc; i++) {
							_sort(mexp,i)
							info = panelsetup(mexp[,i],1)
							decl = selectindex(mexp[info[,1],i]:==__s_vint[j])
							ndcl = info[decl,2]-info[decl,1]
							if (ndcl==1) mexp[info[decl,1],i] = "2*"+__s_vint[j]
							else mexp[info[decl,1],i] = strofreal(ndcl+1)+"*"+__s_vint[j]+"^"+strofreal(ndcl)
							mexp[|(info[decl,1]+1),i\info[decl,2],i|] = J(ndcl,1,"")
						}

						if (j==3) {
							if (rows(selectindex((cl[,2]:==1):* (cl[,3]:>1)))) {
								bsel = J(rows(msel),1,.)
								bsel1 = selectindex((cl[,2]:==1):* (cl[,3]:>1))
								for (i=1; i<=rows(bsel); i++) bsel[i] = anyof(bsel1,msel[i])
								bsel = selectindex(bsel)
								bint2 = mexp[,bsel]'
								bint2 = subinstr(bint2,__s_vint[2],"",.)		
								for (i=1; i<=rows(bsel); i++) bint2[i,1] = invtokens(select(bint2[i,], strlen(bint2[i,]):>0), "*")
								bint2 = bint2[,1]
								sbin = selectindex(bint2:!="")
								bint2[sbin] = bint2[sbin]:+ "*"
								bint1 = bint1 \ bint2:+ "_b[":+s_cl[msel[bsel]]:+"]"
							};
							if (rows(selectindex((cl[,2]:>1):* (cl[,3]:>1)))) {
								bsel = J(rows(msel),1,.)
								bsel1 = selectindex((cl[,2]:>1):* (cl[,3]:>1))
								for (i=1; i<=rows(bsel); i++) bsel[i] = anyof(bsel1,msel[i])
								bsel = selectindex(bsel)
								bsel0 = bsel0 \ msel[bsel]
								if (cols(bint0)) bint0 = bint0 , mexp[,bsel]
								else bint0 = mexp[,bsel]
							};

							nc = cols(bint0)
							if (nc) {
								for (i=1; i<=nc; i++) {
									_sort(bint0,i)
									info = panelsetup(bint0[,i],1)
									decl = selectindex(bint0[info[,1],i]:==__s_vint[2])
									ndcl = info[decl,2]-info[decl,1]
									if (ndcl==1) bint0[info[decl,1],i] = "2*"+__s_vint[2]
									else bint0[info[decl,1],i] = strofreal(ndcl+1)+"*"+__s_vint[2]+"^"+strofreal(ndcl)
									bint0[|(info[decl,1]+1),i\info[decl,2],i|] = J(ndcl,1,"")
								}
								bint0 = bint0'
								for (i=1; i<=rows(bsel0); i++) bint0[i,1] = invtokens(select(bint0[i,], strlen(bint0[i,]):>0), "*")
								bint0 = bint0[,1]
								sbin = selectindex(bint0:!="")
								bint0[sbin] = bint0[sbin]:+ "*"
								bint = bint0:+ "_b[":+s_cl[bsel0]:+"]"
							}
							else bint = J(0,1,"")
							
							bint = bint1 \ bint
							bint = "("+ invtokens(bint',"+") +")"
						};

						mexp = mexp'			
						for (i=1; i<=rows(msel); i++) mexp[i,1] = invtokens(select(mexp[i,], strlen(mexp[i,]):>0), "*")
						mexp = mexp[,1]
						sexp = selectindex(mexp:!="")
						mexp[sexp] = mexp[sexp]:+ "*"
						if (j==2) mexp2 = mexp2 \ (mexp:+ "_b[":+s_cl[msel]:+"]")
						else mexp3 = mexp3 \ (mexp:+ "_b[":+s_cl[msel]:+"]")
					}
				}
			}
		
			ecmd = st_global("e(cmd)")
			if (ecmd=="probit") ecmd = "normal"
			else if (ecmd=="logit") ecmd = "logistic"
			else if (ecmd!="logistic") {
				errprintf("second- and third-order cross partial derivatives are allowed\n")
				errprintf(" only with 'logistic', 'logit', and 'probit' models\n")
				exit(198)
			};

			if (inteff!=7) {
				mexp = "("+invtokens(mexp2',"+")+")"
				st_strscalar(me,ecmd+"den(xb())*"+mexp)
			}
			else {
				mexp = "("+invtokens(mexp2',"+")+") * ("+invtokens(mexp3',"+")+")"
				if (ecmd=="logistic") st_strscalar(me,"predict()*(1-predict())*(1-2*predict())*"+mexp+" + predict()*(1-predict())*"+bint)
				else st_strscalar(me,"normalden(xb())*(-xb())*"+mexp+" + normalden(xb())*"+bint)
			}
		};
	}
	
	void inteff(__s_nlev,__s_vint,__s_vlab) {
		dyt = tokens(st_local("dydxs"))'		
		if (max(strpos(dyt,"b."))) {
			errprintf("invalid option {bf:dydxs()};\n base level incorrectly specified\n")
			exit(198)
		}
		else dnm1 = substr(dyt,strrpos(dyt,"."):+1,.)				
		
		rnm = 1::rows(dnm1)
		ord = order((dnm1,strofreal(rnm)),(1,2))
		if (!all(ord:==rnm)) {
			_collate(dyt,ord)
			_collate(dnm1,ord)			
		};
	
		info = panelsetup(dnm1,1)
		ri = rows(info)
		if (ri<strtoreal(st_local("wcdyx"))) {
				errprintf("invalid option {bf:dydxs()};\n only one argument per interacting variable allowed\n")
				exit(198)
		}
		else st_local("atdyx", invtokens(dnm1[info[,1]]'))

		if (any(strpos(dyt,".") :* (substr(dyt,1,1):=="b"))) {
			base1 = substr(dyt,1,strpos(dyt,"."):-1)
			bsel1 = (substr(base1,1,1):=="b")	
			base1[selectindex(bsel1)] = substr(base1[selectindex(bsel1)],2,.)
			
			flev = substr(dyt,1,strrpos(dyt,"."):-1)
			if (max(strpos(flev,".b"))) {
				errprintf("invalid option {bf:dydxs()};\n base level incorrectly specified \n")
				exit(198)
			}
			else if (max(strlen(flev) :- strlen(subinstr(flev,"b","",.)))>1) {
				errprintf("multiple base levels for the same factor variable specified\n")
				exit(198)
			};	
			lwrt = strpos(flev,".") :* bsel1			
			if (any(lwrt)) {
				flev = substr(flev,lwrt,.)
				nc = strlen(flev):- strlen(subinstr(flev,".","",.))
				twrt = J(ri,max(nc),"")
				for (i=1; i<=ri; i++) {
					if (lwrt[i]) {
						tkns = tokens(substr(flev[i], 2, .) , ".") 
						twrt[i,1..nc[i]] = tkns[range(1,2*nc[i]-1,2)'] 
					};
				}
			};
		}
		else if (max(strpos(substr(dyt,1,strrpos(dyt,"."):-1),".b"))) {
			errprintf("invalid option {bf:dydxs()};\n base level incorrectly specified\n")
			exit(198)
		}
		else {
			base1 = J(ri,1,"")
			lwrt = J(ri,1,0)
		}

		_stata("cap margins in `" +"fsamp"+ "', dydx(`" +"dydxs"+ "') nose")
		_stata("local rc = _rc")
		rc = strtoreal(st_local("rc"))
		if (rc) {
			errprintf("invalid option {bf:dydxs()};\n")
			if (anyof((111,322),rc)) {
				_stata("cap noi margins in `" +"fsamp"+ "', at(`" +"dydxs"+ "') nose")
			}
			exit(198)
		};
	
		dyt = tokens(st_global("r(xvars)"))'
		dnm = substr(dyt,strpos(dyt,"."):+1,.)		
		rnm = 1::rows(dnm)
		eqrnm = strofreal(rnm)
		mimx = minmax(strlen(eqrnm))
		if (mimx[1]==mimx[2]) ord = order((dnm,eqrnm), (1,2))
		else {
			eqrnm = strofreal(rnm, "%-0"+strofreal(mimx[2])+".0f")
			ord = order((dnm,eqrnm), (1,2))
		}
		if (!all(ord:==rnm)) {
			_collate(dyt,ord)
			_collate(dnm,ord)			
		};
		info = panelsetup(dnm,1)
		ri = rows(info)
		if (ri>3) {
				errprintf("invalid option {bf:dydxs()};\n too many interacting variables specified \n")
				exit(198)
		}
		else st_local("ndyoz", strofreal(ri))
			
		noleg = (st_local("nolegend")=="")
	
		bdot = strpos(dyt,".")
		if (any(bdot)) {
			st_local("fvdx","1")
			__s_nlev = 0

			bsel = selectindex(strpos(dyt,"b.") :+ (!strpos(dyt,".")))
			dyt = subinstr(dyt,"b.",".",.)
			ind = substr(dyt,1,strpos(dyt,"."):-1)				
			
			base = ind[bsel]			
			if (!allof(base1,"")) {
				sl1 = (base1:=="") :!= (base:=="")					
				if (max(sl1)) base1[selectindex(sl1)] = base[selectindex(sl1)] ;;
				diff = (base:!=base1)

				if (any(diff)) {
					diff = selectindex(diff)					
					if (any(lwrt[diff])) {
						for (i=1; i<=rows(diff); i++) {
							if (lwrt[diff[i]]) {
								if (!anyof(twrt[diff[i],], base[diff[i]])) ind[selectindex(ind[info[diff[i],1]::info[diff[i],2]]:==base[diff[i]])+info[diff[i],1]-1] = ""
								else lwrt[diff[i]] = 0
							};
						}
					};

					base[diff] = base1[diff]										
				};
			};

			if (ri==1) {
				__s_vint = dnm[1]
			
				if (info[1,2]==1) {
					if (ind[1]==base) {
						errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint+ "' has only one level\n")
						exit(198)
					};
				}
				else if (lwrt & (info[1,2]==2)) {
					if (sort(ind[1::2],-1)[1]==base) {
						errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint+ "' has only one level\n")
						exit(198)
					};
				};

				ind = ind[selectindex(ind:!=base)]
				if (any(lwrt)) ind = select(ind, strlen(ind):>0) ;;
									
				__s_nlev = rows(ind)
				if (base=="") __s_vlab = "∆(x1)"
				else {
					__s_vlab = "∆(":+ind:+".x1)"
					st_local("dnm1", "b"+base+"." +invtokens(ind',".")+"."+ __s_vint)
				}
				if (noleg) {
					st_local("wrt1","dy/dx w.r.t. x1; x1 : b" +base+ ".i(" +invtokens(ind')+ ")." +__s_vint)
					st_local("d1","∆(i.x1)")
				};
			}
			else {
				fv = (bdot[info[,1]]:!=0)
				if (!(all(fv) | (allof(fv,0)))) {
					fvor = order((fv, (1::ri)),(-1,2))
					if (!all(fvor:==(1::ri))) {
						_collate(base,fvor)
						_collate(base1,fvor)
						_collate(fv,fvor)
						_collate(info,fvor)
						_collate(lwrt,fvor) ;;
					};
				};
				__s_vint = dnm[info[,1]]'

				if (noleg) inds = J(0,1,"") ;;
				if (ri==2) {
					if (fv[2]) {
						st_local("inteff","1")

						for (i=1; i<=2; i++) {
							if (info[i,2]-info[i,1]==0) {
								if (ind[info[i,1]]==base[i]) {
									errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[i]+ "' has only one level\n")
									exit(198)
								};
							}
							else if (lwrt[i] & (info[i,2]-info[i,1]==1)) {
								if (sort(ind[info[i,1]::info[i,2]],-1)[1]==base[i]) {
									errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[i]+ "' has only one level\n")
									exit(198)
								};
							};
					    
							isel = selectindex(ind[info[i,1]::info[i,2]]:!=base[i])
							indi = (ind[info[i,1]::info[i,2]])[isel]
							if (any(lwrt[i])) indi = select(indi, strlen(indi):>0) ;;

							if (rows(indi)>1) __s_nlev = __s_nlev+rows(indi) ;;
							
							if (noleg) {
								inds = inds \ invtokens(indi',".")
								st_local("dnm"+strofreal(i), "b"+base[i]+"." +inds[i]+ "."+__s_vint[i])
							}
							else st_local("dnm"+strofreal(i), "b"+base[i]+"." +invtokens(indi',".")+ "."+ __s_vint[i])
						}
					}
					else {
						st_local("inteff","2")
						st_local("dnm2", __s_vint[2])

						if (info[1,2]-info[1,1]==0) {
							if (ind[info[1,1]]==base[1]) {
								errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[1]+ "' has only one level\n")
								exit(198)
							};
						}
						else if (lwrt[1] & (info[1,2]-info[1,1]==1)) {
							if (sort(ind[info[1,1]::info[1,2]],-1)[1]==base[1]) {
								errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[1]+ "' has only one level\n")
								exit(198)
							};
						};
						
						isel = selectindex(ind[info[1,1]::info[1,2]]:!=base[1])
						indi = (ind[info[1,1]::info[1,2]])[isel]
						if (any(lwrt[1])) indi = select(indi, strlen(indi):>0) ;;
						
						__s_nlev = rows(indi)
						if (noleg) {
							inds = invtokens(indi',".")
							st_local("dnm1", "b"+base[1]+"." +inds+ "."+__s_vint[1])						
						}
						else st_local("dnm1", "b"+base[1]+"." +invtokens(indi',".")+"."+ __s_vint[1])						
					}
				}
				else {
					if (fv[3]) {
						st_local("inteff","4")
						
						for (i=1; i<=3; i++) {
							if (info[i,2]-info[i,1]==0) {
								if (ind[info[i,1]]==base[i]) {
									errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[i]+ "' has only one level\n")
									exit(198)
								};
							}
							else if (lwrt[i] & (info[i,2]-info[i,1]==1)) {
								if (sort(ind[info[i,1]::info[i,2]],-1)[1]==base[i]) {
									errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[i]+ "' has only one level\n")
									exit(198)
								};
							};
							
							isel = selectindex(ind[info[i,1]::info[i,2]]:!=base[i])
							indi = (ind[info[i,1]::info[i,2]])[isel]
							if (any(lwrt[i])) indi = select(indi, strlen(indi):>0) ;;
							
							if (rows(indi)>1) __s_nlev = __s_nlev+rows(indi) ;;
							
							if (noleg) {
								inds = inds \ invtokens(indi',".")
								st_local("dnm"+strofreal(i), "b"+base[i]+"." +inds[i]+ "."+__s_vint[i])
							}
							else st_local("dnm"+strofreal(i), "b"+base[i]+"." +invtokens(indi',".")+ "."+ __s_vint[i])
						}
					}
					else if (fv[2]) {
						st_local("inteff","5")
						st_local("dnm3", __s_vint[3])
						
						for (i=1; i<=2; i++) {
							if (info[i,2]-info[i,1]==0) {
								if (ind[info[i,1]]==base[i]) {
									errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[i]+ "' has only one level\n")
									exit(198)
								};
							}
							else if (lwrt[i] & (info[i,2]-info[i,1]==1)) {
								if (sort(ind[info[i,1]::info[i,2]],-1)[1]==base[i]) {
									errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[i]+ "' has only one level\n")
									exit(198)
								};
							};

							isel = selectindex(ind[info[i,1]::info[i,2]]:!=base[i])
							indi = (ind[info[i,1]::info[i,2]])[isel]
							if (any(lwrt[i])) indi = select(indi, strlen(indi):>0) ;;
							
							if (rows(indi)>1) __s_nlev = __s_nlev+rows(indi) ;;
							
							if (noleg) {
								inds = inds \ invtokens(indi',".")
								st_local("dnm"+strofreal(i), "b"+base[i]+"." +inds[i]+ "."+__s_vint[i])
							}
							else st_local("dnm"+strofreal(i), "b"+base[i]+"." +invtokens(indi',".")+ "."+ __s_vint[i])
						}
					}
					else {
						st_local("inteff","6")
						st_local("dnm3", __s_vint[3])
						st_local("dnm2", __s_vint[2])

						if (info[1,2]-info[1,1]==0) {
							if (ind[info[1,1]]==base[1]) {
								errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[1]+ "' has only one level\n")
								exit(198)
							};
						}
						else if (lwrt[1] & (info[1,2]-info[1,1]==1)) {
							if (sort(ind[info[1,1]::info[1,2]],-1)[1]==base[1]) {
								errprintf("invalid option {bf:dydxs()};\n factor '" +__s_vint[1]+ "' has only one level\n")
								exit(198)
							};
						};

						isel = selectindex(ind[info[1,1]::info[1,2]]:!=base[1])
						indi = (ind[info[1,1]::info[1,2]])[isel]
						if (any(lwrt[1])) indi = select(indi, strlen(indi):>0) ;;
						
						__s_nlev = rows(indi)
						if (noleg) {
							inds = invtokens(indi',".")
							st_local("dnm1", "b"+base[1]+"." +inds+ "."+__s_vint[1])						
						}
						else st_local("dnm1", "b"+base[1]+"." +invtokens(indi',".")+"."+ __s_vint[1])						
					}
				}

				if (noleg) {
					base = "b":+ base :+"."
					st_local("wrt1","dy/dx w.r.t. x1; x1 : " +base[1]+ "i(" +inds[1]+ ")." +__s_vint[1])
					st_local("d1","∆(i.x1)")

					for (i=2; i<=ri; i++) {
						if (fv[i]) {
							st_local("wrt"+strofreal(i),"dy/dx w.r.t. x" +strofreal(i)+ "; x" +strofreal(i)+ " : " +base[i]+ "i(" +inds[i]+ ")." +__s_vint[i])
							st_local("d"+strofreal(i),"∆(i.x" +strofreal(i)+ ")")
						}
						else {
							st_local("wrt"+strofreal(i),"dy/dx w.r.t. x" +strofreal(i)+ "; x" +strofreal(i)+ " : " +__s_vint[i])
							st_local("d"+strofreal(i),"∆(x" +strofreal(i)+ ")")
						}
					}
				};
			
				if (!__s_nlev) __s_nlev = 1 ;;
			}		
		}
		else {
			__s_vint = dnm'
			for (i=1; i<=ri; i++) st_local("dnm"+strofreal(i), __s_vint[i])

			if (ri==1) {
				__s_vlab = "∆(x1)"
				if (noleg) {
					st_local("wrt1","dy/dx w.r.t. x1; x1 : " + __s_vint)
					st_local("d1","∆(x1)")
				};
			}
			else {
				if (ri==2) st_local("inteff","3")
				else st_local("inteff","7")

				if (noleg) {
					for (i=1; i<=ri; i++) {
						st_local("wrt"+strofreal(i),"dy/dx w.r.t. x" +strofreal(i)+ "; x" +strofreal(i)+ " : " + __s_vint[i])
						st_local("d"+strofreal(i),"∆(x" +strofreal(i)+ ")")
					}
				};
			}
		}
	}

	void L(__s_atr,__s_g,__s_keq,__s_L,__s_Lat,__s_nlev) {
		cL = cols(st_matrix("r(L)"))
		if (!cL) {
			g = __s_g'
			rg = rows(g)
			ak = __s_atr*__s_keq
			kl = __s_keq*__s_nlev
			akl = __s_atr*kl

			__s_Lat = J(akl*rg,akl,0)
			if (__s_atr==1) {
				ord = 1::rg
				for (i=1; i<=kl; i++) __s_Lat[ord:+ rg*(i-1), i] = g 
			}
			else {
				ord = range(1,__s_atr*rg-(__s_atr-1),__s_atr)

				if (kl==1) for (i=1; i<=__s_atr; i++) __s_Lat[ord:+(i-1),i] = g
				else {
					at1 = 1
					at2 = __s_atr
					for (j=1; j<=kl; j++) {
						for (i=at1; i<=at2; i++) __s_Lat[ord:+(i-at1),i] = g
						at1 = at2+1
						at2 = at1+__s_atr-1
						ord = ord:+ rg*__s_atr
					}
				}
			}

			if (st_local("prnm")!="") {
				if (st_local("fvdx")=="" & __s_keq>1) {
					__s_L = J(akl*rg,akl,0)

					ordk = range(1,ak*rg-(__s_keq-1),__s_keq)
					if (__s_atr>1) {
						ord = range(1,__s_atr*rg-(__s_atr-1),__s_atr)
						_collate(ordk,ord)
					};

					if (__s_atr==1) for (i=1; i<=__s_keq; i++) __s_L[ordk:+ (i-1), i] = g 
					else {
						at1 = 1
						at2 = __s_atr
						for (j=1; j<=__s_keq; j++) {
							for (i=at1; i<=at2; i++) __s_L[ordk:+ __s_keq*(i-at1), i] = g
							at1 = at2+1
							at2 = at1+__s_atr-1
							ordk = ordk:+ 1
						}
					}
				}
				else __s_L = __s_Lat
			};
		}
		else {
			olv = range(1, cL-(__s_keq-1), __s_keq)
			okl = olv \ ( J(__s_keq-1,1,olv):+ vec(J(rows(olv), 1, 1..(__s_keq-1))) )		
			_collate(__s_L,order(okl,1))
		}
	}

	void vslabel(__s_prord,__s_ord,__s_vl) {
		cl = st_matrixcolstripe("r(b)")[,2]

		if (strpos(cl[1],"@") & st_local("fdxs")!="") {
			if (st_local("prnm")!="" & (strrpos(cl[1],"#")>strpos(cl[1],"@"))) {
				cl = subinstr(cl,"bn.",".",.)
				atov = substr(cl,strpos(cl,"@"):+1,.)
				ord = strtoreal((substr(atov, 1, strpos(atov,"."):-1), substr(atov, strpos(atov,"#"):+1, strrpos(atov,"."):-(strpos(atov,"#"):+1)))), (1::rows(atov))
				__s_prord = order(ord,(1..cols(ord)))
				st_local("prord", strofreal(!(all(__s_prord:==(1::rows(atov))))))
			};
			
			if (st_local("nat")!="1") {
				nat = strtoreal(st_local("nat"))
				r10 = J(rows(cl)/(nat*2), 1, vec(J(nat,1,(1,0))))
				cl = cl[selectindex(r10)]		
			}
			else {
				r10 = J(rows(cl)/2,1,(1\0))
				cl = cl[selectindex(r10)]
				if (strrpos(cl[1],"#")>strpos(cl[1],"@")) cl = substr(cl, 1, strrpos(cl,"#"):-1)
				else cl = substr(cl, 1, strpos(cl,"@"):-1)
			}
		};
		
		if (strpos(cl[1],"@")) {	
			sp = strpos(cl,"@")
			cl = subinstr(cl,"bn.",".",.)
			cl = subinstr(cl,"._predict","._pr",.)
			
			atov = substr(cl,sp:+1,.)
			if (strpos(atov[1],"#")) ord = strtoreal((substr(atov, 1, strpos(atov,"."):-1), substr(atov, strpos(atov,"#"):+1, strrpos(atov,"."):-(strpos(atov,"#"):+1)))), (1::rows(atov))
			else ord = strtoreal(substr(atov, 1, strpos(atov,"."):-1)), (1::rows(atov))
			__s_ord = order(ord,(1..cols(ord)))
				
			cl = substr(cl,1,sp:-1)
			dots = strlen(cl[1])-strlen(subinstr(cl[1],".","",.))
			if (dots==1) cl = substr(cl,1,strpos(cl,"."):-1)
			else if (dots==2) cl = substr(cl,1,strpos(cl,"."):-1) , substr(cl,strpos(cl,"#"):+1,strrpos(cl,"."):-strpos(cl,"#"):-1)
			else {
				pd2 = strpos(substr(cl,strpos(cl,"#"):+1,.),"."):+strpos(cl,"#")
				cl = substr(cl,1,strpos(cl,"."):-1) , substr(cl,strpos(cl,"#"):+1,pd2:-1) , substr(cl,strrpos(cl,"#"):+1,strrpos(cl,"."):-strrpos(cl,"#"):-1)
			}
			__s_vl = substr(cl,2,strpos(cl,"vs"):-2) , atov
			__s_vl[,1] = __s_vl[,cols(__s_vl)]:+"#∆(":+__s_vl[,1]:+".x1)#∆("

			if (all(__s_ord:==(1::rows(atov)))) __s_ord = .
			else _collate(__s_vl,__s_ord)
		}
		else {
			dots = strlen(cl[1])-strlen(subinstr(cl[1],".","",.))
			if (dots==1) cl = substr(cl,1,strpos(cl,"."):-1)
			else if (dots==2) cl = substr(cl,1,strpos(cl,"."):-1) , substr(cl,strpos(cl,"#"):+1,strrpos(cl,"."):-strpos(cl,"#"):-1)
			else {
				pd2 = strpos(substr(cl,strpos(cl,"#"):+1,.),"."):+strpos(cl,"#")
				cl = substr(cl,1,strpos(cl,"."):-1) , substr(cl,strpos(cl,"#"):+1,pd2:-1) , substr(cl,strrpos(cl,"#"):+1,strrpos(cl,"."):-strrpos(cl,"#"):-1)
			}
			__s_vl = substr(cl,2,strpos(cl,"vs"):-2)
			__s_vl[,1] = "∆(":+__s_vl[,1]:+".x1)#∆("
		}
		
		st_local("ord", strofreal(__s_ord[1]!=.))
	}
	
	void fdnuat(__s_fdgn,__s_fdiv,__s_fdnm,__s_fdvl,__s_svfd,string scalar s_1,string scalar s_2,string scalar s_3,string scalar s_4,string scalar s_5,string scalar s_6) {
		fdsc1 = s_1,s_2,s_3
		fdsc2 = s_4,s_5,s_6
		fdvl = __s_fdvl[__s_svfd]
		fdnm = __s_fdnm[__s_svfd]
		fdgn = __s_fdgn[__s_svfd]
		cl = cols(__s_svfd)
		
		for (i=1; i<=cl; i++) {
			st_local("nuat",st_local("nuat")+fdnm[i]+"=gen("+fdgn[i]+"`"+"="+fdsc2[i]+"') ")
			st_numscalar(fdsc2[i],fdvl[i])
			st_numscalar(fdsc1[i],__s_fdiv[i])
		}
		st_local("nuat"," at( "+st_local("at")+" "+st_local("nuat")+") ")
		
		st_local("fdat"," at( "+st_local("at")+" (asobserved) "+st_local("fdxs")+") ")
		if (cl==1) exit(0)
		else if (cl==2) {
			for (i=1; i<=2; i++) {
				if (i==1) {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc1[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc2[2]+"')) ")							
				}
				else {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc2[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc1[2]+"')) ")							
				}
			}
		}
		else {
			for (i=1; i<=6; i++) {
				if (i==1) {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc1[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc1[2]+"') "+fdnm[3]+"=gen("+fdgn[3]+"`"+"="+fdsc2[3]+"')) ")							
				}
				else if (i==2) {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc1[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc2[2]+"') "+fdnm[3]+"=gen("+fdgn[3]+"`"+"="+fdsc1[3]+"')) ")							
				}
				else if (i==3) {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc1[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc2[2]+"') "+fdnm[3]+"=gen("+fdgn[3]+"`"+"="+fdsc2[3]+"')) ")							
				}
				else if (i==4) {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc2[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc1[2]+"') "+fdnm[3]+"=gen("+fdgn[3]+"`"+"="+fdsc1[3]+"')) ")							
				}
				else if (i==5) {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc2[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc1[2]+"') "+fdnm[3]+"=gen("+fdgn[3]+"`"+"="+fdsc2[3]+"')) ")							
				}
				else {
					st_local("fdat",st_local("fdat")+" at( "+st_local("at")+" "+fdnm[1]+"=gen("+fdgn[1]+"`"+"="+fdsc2[1]+"') " ///
					+fdnm[2]+"=gen("+fdgn[2]+"`"+"="+fdsc2[2]+"') "+fdnm[3]+"=gen("+fdgn[3]+"`"+"="+fdsc1[3]+"')) ")							
				}
			}
		}
	}
	
end
