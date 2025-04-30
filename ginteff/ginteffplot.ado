*! version 1 17Nov2022

program ginteffplot
	*version 14 // not all 'save(export_options)' are available in version 14

	syntax [, AIEPoint(str asis) AIERange(str) ASPECTratio(str) GRAPHRegion(str) LEGend(str asis) note(str asis) ///
			OBSEFFs OBSEFFs_s(str asis) output(integer 1) PLOTRegion(str) save(str) scheme(str) SUBtitle(str asis) TItle(str asis) ///
			XCOMmon(str) XTitle(str asis) XLABel(str) XSIZe(str) XSCAle(str) YTitle(str asis) YLABel(str) YSIZe(str) YSCale(str) zeroline ZEROLINE_s(str) ]

	cap noi {
		qui {
			tempname myr
			_return hold `myr'
			_return restore `myr', hold
			if ("`r(cmd)'"!="ginteff") {
				di as err "previous command was not {cmd:ginteff}"
				exit(198)
			}
			`r(cmdline)' __s_ginteffplot
			mata st_local("prnm",__s_gplot)
			
			local rofg = rowsof(r(ginteff))
			if ((`rofg'<`output') | (`output'<1)) {
				di as err "invalid option {bf:output()};" _n " output `output' not found"
				exit(198)
			}

			if ("`prnm'"!="") {
				local prnmout "`prnm'`output'"
				
				if (`"`obseffs_s'"'!="") {
					cap obseffs, `obseffs_s'					
					if (_rc) {
						di as err "invalid option {bf:obseffs()};"
						obseffs, `obseffs_s'
					}
					mata st_local("pct",__s_pct)
					if ("`pct'"!="") {
						sum `prnmout', meanonly
						if (`r(N)'>99) {
							local mi `r(min)'
							local mx `r(max)'
							if (_N<101) {
								local N = _N
								set obs 101
							}					
							tempvar orgprnm myobs // original 'prnm' variable
							pctile `myobs' = `prnmout', nquantiles(100) `pct'
							local r1 = `myobs'[50]
							replace `myobs' = `mi' in 100
							replace `myobs' = `mx' in 101
							rename (`prnmout' `myobs') (`orgprnm' `prnmout')
						}
						else local pct
					}
					
					_return restore `myr', hold

					mata st_local("marker",__s_oplot[1])
					mata st_local("nomed",__s_oplot[2])
					mata st_local("median",__s_oplot[3])
					if (`"`median'"'=="") local median `"(6) "(median)", msymbol(Oh) mcolor(black) mlabcolor(black) mlabsize(medsmall) mlabgap(*5)"'
					else {
						if strpos(`"`median'"',",") {
							local ac = substr(`"`median'"',strpos(`"`median'"',",")+1,.) // after comma
							local median = substr(`"`median'"',1,strpos(`"`median'"',",")-1)
						}				
						if (`"`median'"'=="") local median `"(6) "(median)", msymbol(Oh) mcolor(black) mlabcolor(black) mlabsize(medsmall) mlabgap(*5) `ac'"'
						else {
							local median = strltrim(`"`median'"')
							local txt `""(median)""'
							local pos 6
							local medword = subinstr(`"`median'"'," ","",.)
							local medword = subinstr(`"`medword'"',")",") ",.)

							if (`: word count `medword''>2) {
								di as err "invalid option {bf:median()};" _n " only one location and one marker label allowed"
								exit(198)
							}
							else if (`: word count `medword''==1) {
								if (substr(`"`median'"',1,1)=="(") local pos = substr(`"`median'"',2, strlen(`"`median'"')-2) 
								else if (substr(`"`median'"',1,1)==`"""') local txt = substr(`"`median'"',1, strrpos(`"`median'"',`"""'))
								else {
									di as err "option {bf:median()} incorrectly specified"
									exit(198)
								}
							}
							else {
								if (substr(`"`median'"',1,1)=="(") {
									local pos = substr(`"`median'"',2, strrpos(`"`median'"',")")-2) 
									local txt = substr(`"`median'"', strpos(`"`median'"',`"""'), strrpos(`"`median'"',`"""')-strpos(`"`median'"',`"""')+1)
								}	
								else if (substr(`"`median'"',1,1)==`"""') {
									local txt = substr(`"`median'"',1, strrpos(`"`median'"',`"""'))
									local pos = substr(`"`median'"', strpos(`"`median'"',"(")+1, strrpos(`"`median'"',")")-strpos(`"`median'"',"(")-1) 
								}
								else {
									di as err "option {bf:median()} incorrectly specified"
									exit(198)
								}
							}

							local median `"(`pos') `txt', msymbol(S) mcolor(black) mlabcolor(black) mlabsize(medsmall) mlabgap(*5) `ac'"'
						}
					}
				}
				else if ("`obseffs'"!="") local nomed 1
				else local prnm
			}

			local dv `e(depvar)'
			if ("`dv'"!="") {
				if ("`: var label `dv''"!="") local dv: var label `dv'
			}
			tempvar b ll ul yaxis1 yaxis2
			gen `b'		= el(r(ginteff),`output',1) in 1
			gen `ll'	= el(r(ginteff),`output',5) in 1
			gen `ul'	= el(r(ginteff),`output',6) in 1
				
			if ("`aierange'"=="") local aierange "lcolor(black) lwidth(medthick)"
			else {
				foreach a of local aierange {	
					if (!inlist(substr("`a'",1,2),"lp","lw","lc","la") & !inlist(substr("`a'",1,4),"msiz","lsty","psty")) {
						di as err "invalid option {bf:aierange()};" _n " suboption {bf:`a'} not allowed"
						exit(198)
					}
				}
				local aierange "lcolor(black) `aierange'"
			}
			if (`"`aiepoint'"'=="") local aiepoint `"(12) "AIE", msymbol(S) mcolor(black) mlabcolor(black) mlabsize(medsmall) mlabgap(*5)"'
			else {
				if strpos(`"`aiepoint'"',",") {
					local ac = substr(`"`aiepoint'"',strpos(`"`aiepoint'"',",")+1,.) // after comma
					local aiepoint = substr(`"`aiepoint'"',1,strpos(`"`aiepoint'"',",")-1)
				}				
				
				if (`"`aiepoint'"'=="") local aiepoint `"(12) "AIE", msymbol(S) mcolor(black) mlabcolor(black) mlabsize(medsmall) mlabgap(*5) `ac'"'
				else {
					local aiepoint = strltrim(`"`aiepoint'"')
					local txt `""AIE""'
					local pos 12
					local aieword = subinstr(`"`aiepoint'"'," ","",.)
					local aieword = subinstr(`"`aieword'"',")",") ",.)

					if (`: word count `aieword''>2) {
						di as err "invalid option {bf:aiepoint()};" _n " only one location and one marker label allowed"
						exit(198)
					}
					else if (`: word count `aieword''==1) {
						if (substr(`"`aiepoint'"',1,1)=="(") local pos = substr(`"`aiepoint'"',2, strlen(`"`aiepoint'"')-2) 
						else if (substr(`"`aiepoint'"',1,1)==`"""') local txt = substr(`"`aiepoint'"',1, strrpos(`"`aiepoint'"',`"""'))
						else {
							di as err "option {bf:aiepoint()} incorrectly specified"
							exit(198)
						}
					}
					else {
						if (substr(`"`aiepoint'"',1,1)=="(") {
							local pos = substr(`"`aiepoint'"',2, strrpos(`"`aiepoint'"',")")-2) 
							local txt = substr(`"`aiepoint'"', strpos(`"`aiepoint'"',`"""'), strrpos(`"`aiepoint'"',`"""')-strpos(`"`aiepoint'"',`"""')+1)
						}	
						else if (substr(`"`aiepoint'"',1,1)==`"""') {
							local txt = substr(`"`aiepoint'"',1, strrpos(`"`aiepoint'"',`"""'))
							local pos = substr(`"`aiepoint'"', strpos(`"`aiepoint'"',"(")+1, strrpos(`"`aiepoint'"',")")-strpos(`"`aiepoint'"',"(")-1) 
						}
						else {
							di as err "option {bf:aiepoint()} incorrectly specified"
							exit(198)
						}
					}
					
					local aiepoint `"(`pos') `txt', msymbol(S) mcolor(black) mlabcolor(black) mlabsize(medsmall) mlabgap(*5) `ac'"'
				}
			}
	
			if ("`marker'"=="") local marker "msymbol(O) mcolor(black) msize(vtiny)"
			if ("`plotregion'"=="") local plotregion "margin(sides)"
			if ("`graphregion'"=="") local graphregion "fcolor(white)"
			if (`"`legend'"'=="") local legend "off"
			if (`"`note'"'=="") local note `" "" "'
			if ("`scheme'"=="") local scheme "s2mono"
			if (`"`subtitle'"'=="") local subtitle `" "" "'
			if (`"`title'"'=="") local title `" "" "'
			if ("`ylabel'"=="") local ylabel "none"
			if ("`yscale'"=="") local yscale "titlegap(0) range(0 2)"
			if (`"`ytitle'"'=="") local ytitle `" "" "'
			if ("`xlabel'"=="") {
				if ("`xcommon'"!="") {
					if ("`xcommon'"=="*") local xcommon "1/`rofg'"
					else local xcommon "`xcommon' `output'"
					cap numlist "`xcommon'", range(>=1 <=`rofg')
					if (_rc) {
						di as err "invalid option {bf:xcommon()};"						
						numlist "`xcommon'", range(>=1 <=`rofg')
					}
					local xcommon "`r(numlist)'"
					_return restore `myr', hold
					mata __s_mimx = strtoreal(tokens(st_local("xcommon")))
					mata __s_mimx = uniqrows(__s_mimx)
					mata __s_mimx = strofreal(minmax(st_matrix("r(ginteff)")[__s_mimx,(5,6)]))
					mata st_local("xlo",__s_mimx[1])
					mata st_local("xhi",__s_mimx[2])
					if ("`prnm'"!="") {
						tabstat `prnmout'-`prnm'`rofg', stats(min max) save
						mata __s_mimx = strofreal(minmax(st_matrix("r(StatTotal)")))
						mata st_local("prmi",__s_mimx[1])
						mata st_local("prmx",__s_mimx[2])
					}					
					local xlo = min(`xlo',`prmi')
					local xhi = max(`xhi',`prmx')
				}
				else if ("`prnm'"!="") {
					sum `prnmout', meanonly
					local xlo = min(`ll'[1],`r(min)')
					local xhi = max(`ul'[1],`r(max)')
				}
				else {
					local xlo = `ll'[1]
					local xhi = `ul'[1]
				}
				
				local oxlo `xlo' // original value
				local oxhi `xhi'
				
				if (int(`=`xhi'-`xlo'') & (int(`xhi') | int(`xlo'))) {
					if (!int(`xlo')) {
						local ldec = string(`=`xlo'-int(`xlo')', "%7.6f")
						local ldec = substr("`ldec'",(strpos("`ldec'",".")+1),.)
						forval i = 1/6 {
							if (substr("`ldec'",`i',1)!="0") {
								local xlo = string(`xlo', "%`=`i'+1'.`i'f")
								continue, break
							}
						}
					}
					else local xlo = round(`xlo')

					if (!int(`xhi')) {
						local hdec = string(`=`xhi'-int(`xhi')', "%7.6f")
						local hdec = substr("`hdec'",(strpos("`hdec'",".")+1),.)
						forval i = 1/6 {
							if (substr("`hdec'",`i',1)!="0") {
								local xhi = string(`xhi', "%`=`i'+1'.`i'f")
								continue, break
							}
						}
					}
					else local xhi = round(`xhi')
				}
				else {
					local ldec = string(`=`xlo'-int(`xlo')', "%7.6f")
					local ldec = substr("`ldec'",(strpos("`ldec'",".")+1),.)
					local prec 0
					forval i = 1/6 {
						if (substr("`ldec'",`i',1)!="0") {
							local prec `i'
							continue, break
						}
					}

					local hdec = string(`=`xhi'-int(`xhi')', "%7.6f")
					local hdec = substr("`hdec'",(strpos("`hdec'",".")+1),.)
					forval i = 1/6 {
						if (substr("`hdec'",`i',1)!="0") {
							if (`prec'<`i') local prec `i'
							continue, break
						}
					}

					if (`prec') {
						local xlo = string(`xlo', "%`=strlen("int(`xlo')")+`prec'+1'.`prec'f")
						local xhi = string(`xhi', "%`=strlen("int(`xhi')")+`prec'+1'.`prec'f")
					} 
				}
	
				if ("`zeroline'"!="" | "`zeroline_s'"!="") {
					if (`xlo'>0) {
						local xn = `xhi'/5
						local xn2 = `xn'/2
						if ((`oxhi'>(`xhi'+`xn2')) | (`oxhi'<(`xhi'-`xn2'))) {
							if (`oxhi'>(`xhi'+`xn2')) local xhi = `xhi'+`xn'
							else local xhi = `xhi'-`xn'
							
							local xn = `xhi'/5
						}
						numlist "0(`xn')`xhi' `xhi'"
					}
					else if (`xhi'<0) {
						local xn = abs(`xlo')/5
						local xn2 = `xn'/2
						if ((`oxlo'>(`xlo'+`xn2')) | (`oxlo'<(`xlo'-`xn2'))) {
							if (`oxlo'>(`xlo'+`xn2')) local xlo = `xlo'+`xn'
							else local xlo = `xlo'-`xn'
							
							local xn = abs(`xlo')/5
						}
						numlist "`xlo'(`xn')0 0"
					}
				}
				if ("`xn'"=="") {
					local xd = `xhi'-`xlo'
					local xn = `xd'/5
					local xn2 = `xn'/2
					if ((`oxlo'>(`xlo'+`xn2')) | (`oxlo'<(`xlo'-`xn2')) | (`oxhi'>(`xhi'+`xn2')) | (`oxhi'<(`xhi'-`xn2'))) {
						if (`oxlo'>(`xlo'+`xn2')) local xlo = `xlo'+`xn'
						else if (`oxlo'<(`xlo'-`xn2')) local xlo = `xlo'-`xn'
						else if (`oxhi'>(`xhi'+`xn2')) local xhi = `xhi'+`xn'
						else local xhi = `xhi'-`xn'

						local xn = (`xhi'-`xlo')/5
					}
					numlist "`xlo'(`xn')`xhi' `xhi'"
				}

				local xlabel `r(numlist)'
				local xlabel : list uniq xlabel
				if (!strpos(" `xlabel' "," 0 ") & ("`zeroline'"!="" | "`zeroline_s'"!="")) local xlabs "xlabel(`xlabel') xlabel(`oxlo' " " `oxhi' " ", tlwidth(none) custom add) xlabel(0, tlength(*3.5) custom add)"
				else local xlabs "xlabel(`xlabel') xlabel(`oxlo' " " `oxhi' " ", tlength(*3.5) tlwidth(none) custom add)" 
			}
			else local xlabs "xlabel(`xlabel')"
			if ("`xscale'"=="") local xscale "titlegap(4)"
			if (`"`xtitle'"'=="") local xtitle `""Change in {it:`dv'}", size(4)"'
			if ("`zeroline_s'"!="") local zeroline "0, `zeroline_s'"
			else if ("`zeroline'"!="") local zeroline "0, lpattern(shortdash) lwidth(medthin) lcolor(red)"

			if ("`prnm'"!="") {
				gen `yaxis1' = 1.1 in 1
				gen `yaxis2' = 0.9 if `prnmout'!=.

				if (!`nomed' & "`r1'"=="") {
					_pctile `prnmout', p(50)
					local r1 `r(r1)'
				}
				else if (`nomed') {
					local r1 = `b'[1]
					local median `"(6) "", msymbol(none)"'
				}

				#delimit ;
				graph tw
					|| rcap `ll' `ul' `yaxis1' in 1, horizontal `aierange'
					|| scatteri 1.1 `=`b'[1]' `aiepoint'
					|| scatter `yaxis2' `prnmout', `marker'
					|| scatteri 0.9 `r1' `median'
					||,
						legend(`legend') note(`note')
						title(`title') subtitle(`subtitle')
						`xlabs' xline(`zeroline') xscale(`xscale') xsize(`xsize') xtitle(`xtitle')
						ylabel(`ylabel') yscale(`yscale') ysize(`ysize') ytitle(`ytitle')
						aspectratio(`aspectratio') plotregion(`plotregion') graphregion(`graphregion') scheme(`scheme') ;
				#delimit cr
				if ("`save'"!="") graph export `save'

				if ("`pct'"!="") {
					drop `prnmout'
					rename `orgprnm' `prnmout'
				}				
			}
			else {
				gen `yaxis1' = 1 in 1
				#delimit ;
				graph tw
					|| rcap `ll' `ul' `yaxis1' in 1, horizontal `aierange'
					|| scatteri 1 `=`b'[1]' `aiepoint'
					||,
						legend(`legend') note(`note')
						title(`title') subtitle(`subtitle')
						`xlabs' xline(`zeroline') xscale(`xscale') xsize(`xsize') xtitle(`xtitle')
						ylabel(`ylabel') yscale(`yscale') ysize(`ysize') ytitle(`ytitle')
						aspectratio(`aspectratio') plotregion(`plotregion') graphregion(`graphregion') scheme(`scheme') ;
				#delimit cr
				if ("`save'"!="") graph export `save'
			}
		}
	}	

	
	
	if (_rc) {
		local rc = _rc
		cap if (`N'>_N) drop in `=`N'+1'/`=_N'
		cap mata: mata drop __s_*
		_return restore `myr'
		exit(`rc')
	}
	else {
		cap if (`N'>_N) drop in `=`N'+1'/`=_N'	
		cap mata: mata drop __s_*
		_return restore `myr'
	}

end

				
program obseffs
	version 14
	
	syntax [, marker(str) median MEDIAN_s(str asis) pctile PCTILE_s(str)]
	if ("`pctile_s'"!="") {
		if (!inlist("`pctile_s'","alt","altd","altde","altdef")) {
			di as err " suboption {bf:pctile(`pctile_s')} not allowed"
			exit(198)
		}
		mata __s_pct = "alt"
	}
	else if ("`pctile'"!="") mata __s_pct = " "
	else mata __s_pct = ""
	
	mata __s_oplot = J(1,3,"")
	if ("`marker'"!="") mata __s_oplot[1] = st_local("marker")
	if ("`median'"!="" | "`median_s'"!="") mata __s_oplot[2] = "0"
	else  mata __s_oplot[2] = "1"
	if ("`median_s'"!="") mata __s_oplot[3] = st_local("median_s")

end
