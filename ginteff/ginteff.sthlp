{smcl}
{* *! version 1 22Nov2022}{...}
{viewerjumpto "Title" "ginteff##title"}{...}
{viewerjumpto "Syntax" "ginteff##syntax"}{...}
{viewerjumpto "Description" "ginteff##description"}{...}
{viewerjumpto "Options" "ginteff##options"}{...}
{viewerjumpto "Syntax of at()" "ginteff##atspec"}{...}
{viewerjumpto "Syntax of atdxs()" "ginteff##atdxspec"}{...}
{viewerjumpto "Syntax of dxspec()" "ginteff##dxspec"}{...}
{viewerjumpto "Syntax of firstdiff()" "ginteff##fdspec"}{...}
{viewerjumpto "Syntax of nunit()" "ginteff##nunit"}{...}
{viewerjumpto "Examples" "ginteff##examples"}{...}
{viewerjumpto "Stored results" "ginteff##results"}{...}
{viewerjumpto "The User's Manual" "ginteff##manual"}{...}
{viewerjumpto "Author" "ginteff##author"}{...}
{viewerjumpto "Also see" "ginteff##alsosee"}{...}
{cmd:help ginteff}{right: ({browse "https://doi.org/10.1177/1536867X231175253":SJ23-2: st0711})}
{hline}

{marker title}{...}
{title:Title}

{p2colset 5 16 18 2}{...}
{p2col :{cmd:ginteff} {hline 2}}Compute two- and three-way interaction
effects{p_end}


{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:ginteff}
{ifin}
{weight}{cmd:,}
{it:effect_computation} [{it:options}]

{synoptset 26 tabbed}{...}
{synopthdr:effect_computation}
{synoptline}
{p2coldent:* {cmd:dydxs(}{it:{help ginteff##dxspec:dxspec}}{cmd:)}}specify the interacted variables for which to compute the effect via the partial derivative{p_end}
{synopt:{cmd:fd(}{it:{help ginteff##fdspec:fdspec}}{cmd:)}}shorthand for {cmd:firstdiff()}{p_end}
{p2coldent:* {cmd:firstdiff(}{it:{help ginteff##fdspec:fdspec}}{cmd:)}}specify the interacted variables for which to compute the effect via first difference{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
* One of {cmd:dydxs()} or {cmd:firstdiff()} is required.  A minimum of two and
a maximum of three variables must be specified in {cmd:dydxs()} or
{cmd:firstdiff()}.

{synoptset 26 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt:{cmd:atdxs(}{it:{help ginteff##atdxspec:atdxspec}}{cmd:)}}fix the interacted variables in {cmd:dydxs()} to specified values{p_end}
{synopt:{opt nunit((#) varlist)}}specify the unit increase for each variable in {cmd:firstdiff()}{p_end}
{synopt:{opt obseff(stub)}}create new variables with the interaction effect for each observation{p_end}
{syntab:Auxiliary}
{synopt:{cmd:at(}{it:{help ginteff##atspec:atspec}}{cmd:)}}compute the interaction effect at specified values of covariates{p_end}
{synopt:{opt inteq:uation(eqno)}}identify the interaction equation; default is {cmd:intequation(#1)}{p_end}
{synopt:{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt:{opt many}}report more than 100 results; maximum is 1,000{p_end}
{synopt:{opt nol:egend}}suppress output legend{p_end}
{synopt:{opt noweight:s}}ignore weights specified in estimation{p_end}
{synopt:{opt post}}post interaction effects and their variance–covariance
estimate (VCE) as estimation results{p_end}
{synopt:{opt pr:edict(pred_opt)}}compute the interaction effect for
{cmd:predict,} {it:pred_opt}{p_end}
{synopt:{opt vce(vcetype)}}specify how the VCE and standard errors are calculated; the default is {cmd:vce(delta)}{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{cmd:fweight}s, {cmd:aweight}s, {cmd:iweight}s, and {cmd:pweight}s are
allowed; see {help weight}.


{marker description}{...}
{title:Description}

{pstd}
{cmd:ginteff} computes the average and individual-level interaction effects
for two- and three-way interactions.  The effect of the interacted variables
can be computed via either the partial derivative or the first difference.	


{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{cmd:dydxs(}{it:{help ginteff##dxspec:dxspec}}{cmd:)} specifies the interacted
variables for which the effect is to be computed via the partial derivative.
For factor variables, {cmd:dydxs()} calculates the discrete change from the
base level.  See the {it:Syntax of dydxs()} section for more information.  One
of {cmd:dydxs()} or {cmd:firstdiff()} is required.

{phang}
{cmd:firstdiff(}{it:{help ginteff##fdspec:fdspec}}{cmd:)} specifies the
interacted variables for which the effect is to be computed via the first
difference and also sets their starting values.  Variables in
{cmd:firstdiff()} must be continuous.  See the {it:Syntax of firstdiff()}
section for more information.  One of {cmd:dydxs()} or {cmd:firstdiff()} is
required.

{phang}
{cmd:atdxs(}{it:{help ginteff##atdxspec:atdxspec}}{cmd:)} fix the continuous
variables in {cmd:dydxs()} at specific values.  See the {it:Syntax of atdxs()}
section for more information.

{phang}
{opt nunit((#) varlist)} indicates the unit increase for each variable in
{cmd:firstdiff()}.  See the {it:Syntax of nunit()} section for more
information.

{phang}
{opt obseff(stub)} creates a new variable containing the interaction effect
for each observation in the sample data.  The (possibly many) variables are
named consecutively, starting with {it:stub}{cmd:1}.  {it:stub} may not exceed
16 characters in length.

{dlgtab:Auxiliary}

{phang}
{cmd:at(}{it:{help ginteff##atspec:atspec}}{cmd:)} specifies values for
covariates to be treated as fixed.  See the {it:Syntax of at()} section for
more information.  Only one {cmd:at()} option can be specified.

{phang}
{opt intequation(eqno)} is relevant only when you have previously fit a
multiequation model and identifies the interaction equation.  For instance,
{cmd:intequation(#1)} would mean the interacted variables are in the first
equation, {cmd:intequation(#2)} would mean the second, and so on.  You could
also refer to the equations by their names.  {cmd:intequation(health)} would
refer to the equation named {cmd:health}, and {cmd:intequation(diabetes)} to
the equation named {cmd:diabetes}.  If you do not specify {cmd:intequation()},
results are the same as if you specified {cmd:intequation(#1)}.

{phang}
{opt level(#)} specifies the confidence level, as a percentage, for confidence
intervals.  The default is {cmd:level(95)} or as set by {cmd:set level}.

{phang}
{opt many} raises the maximum number of output estimates from 100 to 1,000.

{phang}
{opt nolegend} specifies that the legend detailing the {cmd:ginteff} output
and that showing the fixed values of covariates be suppressed.

{phang}
{opt noweights} specifies that any weights specified on the previous
estimation command be ignored by {cmd:ginteff}.  By default, {cmd:ginteff}
uses the weights specified on the estimator.  If weights are specified on the
{cmd:ginteff} command, they override previously specified weights, making it
unnecessary to specify {cmd:noweights}.

{phang}
{opt post} causes {cmd:ginteff} to behave like a Stata estimation (e-class)
command.  {cmd:ginteff} posts the vector of interaction effects along with the
estimated variance-covariance matrix to {cmd:e()}, so you can treat these
estimates just as you would results from any other estimation command.  For
example, you could use {cmd:nlcom} to test whether two interaction effects are
statistically different.

{phang}
{opt predict(pred_opt)} specifies the options to be used with the
{cmd:predict} command to produce the variable that will be used as the
response.  After estimation by {cmd:logistic}, one could specify
{cmd:predict(xb)} to obtain linear predictions rather than the {cmd:predict}
command's default, the probabilities.  Only one {cmd:predict()} option can be
specified.

{phang}
{cmd:vce(delta)} and {cmd:vce(unconditional)} specify how the VCE and standard
errors are calculated.

{phang2}
{cmd:vce(delta)} is the default.  The delta method is applied to the formula
for the response and the VCE of the estimation command.  This method assumes
that values of the covariates used to calculate the response are given or, if
all covariates are not fixed using {cmd:at()}, that the data are given.

{phang2}
{cmd:vce(unconditional)} specifies that the covariates that are not fixed be
treated in a way that accounts for their having been sampled.  The VCE is
estimated using the linearization method.  This method allows for
heteroskedasticity or other violations of distributional assumptions and
allows for correlation among the observations in the same manner as
{cmd:vce(robust)} and {cmd:vce(cluster} ...{cmd:)}, which may have been
specified with the estimation command.  This method also accounts for complex
survey designs if the data are {cmd:svyset}.


{marker atspec}{...}
{title:Syntax of at()}

{pstd}
In {cmd:at(}{it:atspec}{cmd:)}, {it:atspec} may contain one or more of the
following specifications,

{p 12 12 2}
{it:varlist}

{p 12 12 2}
{cmd:(}{it:stat}{cmd:)} {it:varlist} 

{p 12 12 2}
{it:varname} {cmd:=} {it:#}

{p 12 12 2}
{it:varname} {cmd:= (}{it:{help numlist}}{cmd:)} 

{p 12 12 2}
{it:varname} {cmd:=} {opth gen:erate(exp)}

{pstd}
where

{p 12 15 2}
1. Variable names (whether in {it:varname} or {it:varlist}) may be continuous
variables, factor variables, or specific level variables, such as {cmd:age},
{cmd:group}, or {cmd:3.group}.

{p 12 15 2}
2. {it:varlist} may also be one of three standard lists:{p_end}
{p 19 22 2}
a. {opt _all} (all covariates),{p_end}
{p 19 22 2}
b. {opt _f:actor} (all factor-variable covariates), or{p_end}
{p 19 22 2}
c. {opt _c:ontinuous} (all continuous covariates).{p_end}

{p 12 15 2}
3. {it:stat} can be any of the following:

{p2colset 5 22 24 2}{...}
{p2line}
{p2col :}         				{space 44}Variables{p_end}
{p2col :{it:stat}} Description			{space 32}allowed{p_end}
{p2line}
{p2col :{opt asobs:erved}} at observed values in the sample (default)
			{space 1}all{p_end}
{p2col :{opt mean}}   means (default for {it:varlist})  {space 16}all{p_end}
{p2col :{opt median}} medians			{space 36}continuous{p_end}
{p2col :{opt p1}}     1st percentile		{space 29}continuous{p_end}
{p2col :{opt p2}}     2nd percentile		{space 29}continuous{p_end}
{p2col :...}     3rd-49th percentiles {space 23}continuous{p_end}
{p2col :{opt p50}}    50th percentile (same as {cmd:median}) 
						{space 11}continuous{p_end}
{p2col :...}     51st-97th percentiles {space 22}continuous{p_end}
{p2col :{opt p98}}    98th percentile		{space 28}continuous{p_end}
{p2col :{opt p99}}    99th percentile		{space 28}continuous{p_end}
{p2col :{opt min}}    minimums			{space 35}continuous{p_end}
{p2col :{opt max}}    maximums			{space 35}continuous{p_end}
{p2col :{opt zero}}   fixed at zero		{space 30}continuous{p_end}
{p2col :{opt base}}   base level 		{space 33}factors{p_end}
{p2col :{opt asbal:anced}} all levels equally probable and sum to 1
						{space 3}factors{p_end}
{p2line}
{p2colreset}{...}

{pstd}
When no {it:stat} is specified, {cmd:mean} is assumed.  If
{cmd:(}{it:stat}{cmd:)} is not followed by a {it:varlist},
{cmd:(}{it:stat}{cmd:)} is ignored.  The various {it:stat}s are computed using
the estimation sample.

{pstd}
{cmd:at()} cannot be used to set the interacted variables listed in
{cmd:dydxs()} or {cmd:firstdiff()}.  If the interacted variables are listed in
{cmd:at()}, the program will stop and issue an error.  The standard variable
lists (that is, {cmd:_all}, {cmd:_factor}, and {cmd:_continuous}) can still be
used with {cmd:at()}, but they will affect the variables in the respective
categories except the interacted variables.


{marker atdxspec}{...}
{title:Syntax of atdxs()}

{pstd}
{cmd:atdxs(}{it:atdxspec}{cmd:)} can be used only in combination with
{cmd:dydxs()}, and the variables listed in the two options must match.  In
other words, only the {cmd:dydxs()} variables can be set via {cmd:atdxs()}.
Save the exceptions below, the specifications of {it:atdxspec} are identical
to those of {it:{help ginteff##atspec:atspec}} (see the {it:Syntax of at()}
section).

{pstd}
The exceptions of {it:atdxspec}:

{p 12 15 2}
1. Factor variables cannot be set via {cmd:atdxs()}, because the derivative is
the discrete change from the base level.  As a result, factor variables cannot
be fixed at specific values or levels.  The {cmd:_factor} variable list is not
allowed, but one may still employ the remaining standard lists (that is,
{cmd:_all} and {cmd:_continuous}).  Because factor variables cannot be set via
{cmd:atdxs()}, specifying either {cmd:_all} or {cmd:_continuous} produces the
same result.

{p 12 15 2}
2. If specifying {it:varname} = {it:#}, {it:#} must be a single value (that
is, numeric lists are not allowed).

 
{marker dxspec}{...}
{title:Syntax of dydxs()}

{pstd}
{opt dydxs(dxspec)} specifies the covariates for which the effect is to be
computed by partial derivative.  Up to three variables can be specified (xs ∈
{c -(}x1, x2, x3{c )-}) to respectively indicate a partial, a second- or a
third-order cross-partial derivative, that is, ∂y/∂x1, ∂^2y/(∂x1 ∂x2), or
∂^3y/(∂x1 ∂x2 ∂x3).  For factor variables, {cmd:dydxs()} calculates the
discrete change from the base level.

{pstd}
In {opt dydxs(dxspec)}, {it:dxspec} may contain one or more of the following
specifications:

{p 12 12 2}
{it:varlist}

{p 12 12 2}
{it:j}{cmd:.}{it:factorvar}

{p 12 12 2}
{cmd:b}{it:k}{cmd:.}{it:factorvar}

{p 12 12 2}
{cmd:b}{it:k}{cmd:.}{it:j}{cmd:.}{it:factorvar}

{pstd}
where

{p 12 15 2}
1. variable names (whether in {it:varname} or {it:varlist}) may be continuous
or factor variables that are interacted in the model,

{p 12 15 2}
2. only the syntax for factor variables is used,

{p 19 22 2}
a. {it:j} and {it:k} are actual factor-level values, and

{p 19 22 2}
b. {cmd:b} stands for base level.

{pstd}
As the base level, {it:k} must be a single value.  {it:j} indicates the
specific factor levels for which the discrete change is to be calculated and
can be either one value or a list of factor levels separated by a dot (for
example, {cmd:1.2.3.}{it:factorvar}).  To illustrate the use of specific
factor levels, let us say we have a three-level factor variable, {c -(}1, 2,
3{c )-}.  Assuming 1 is the base level, {cmd:dydxs(}{it:factorvar}{cmd:)}
calculates two discrete changes, (2 versus 1) and (3 versus 1).  Typing
{cmd:dydxs(3.}{it:factorvar}{cmd:)} calculates a single discrete change, (3
versus 1), because only one level is specified.  This is particularly useful
when a factor variable has many levels but the researcher is interested in one
particular contrast.

{pstd}
Typing {cmd:dydxs(b2.}{it:factorvar}{cmd:)} changes the base level from 1 to 2
on the fly without having to refit the model.  The two discrete changes are
now (1 versus 2) and (3 versus 2).  Because {cmd:ginteff} automatically
calculates the discrete change for all factor levels, typing
{cmd:dydxs(b2.}{it:factorvar}{cmd:)} produces the same result as
{cmd:dydxs(b2.1.3.}{it:factorvar}{cmd:)}.  In contrast,
{cmd:dydxs(b2.3.}{it:factorvar}{cmd:)} calculates a single discrete change, (3
versus 2).  When you reset the base level, that value must be specified first.
Thus, {cmd:dydxs(3.b2.}{it:factorvar}{cmd:)} is not a valid specification.

{pstd}
Only one argument per interacted variable is allowed.  Thus, to examine a
subset of factor contrasts, you must list the respective levels together, for
example, {cmd:dydxs(2.3.}{it:factorvar}{cmd:)} and not
{cmd:dydxs(2.}{it:factorvar} {cmd:3.}{it:factorvar}{cmd:)}.


{marker fdspec}{...}
{title:Syntax of firstdiff()}

{pstd}
In option {cmd:firstdiff(}{it:fdspec}{cmd:)}, {it:fdspec} may contain one or
more of the following specifications,

{p 12 12 2}
{it:varlist}

{p 12 12 2}
{cmd:(}{it:fdstat}{cmd:)} varname

{p 12 12 2}
{it:varname} {cmd:=} {it:#}

{p 12 12 2}
{it:varname} {cmd:=} {opth gen:erate(exp)}

{pstd}
where

{p 12 15 2}
1. variable names (whether in {it:varname} or {it:varlist}) must be continuous
variables that are interacted in the model,

{p 12 15 2}
2. {it:#} must be a single value (that is, numeric lists are not allowed), and

{p 12 15 2}
3. {it:fdstat} can be any of the following:

{p2colset 5 22 24 2}{...}
{p2line}
{p2col :{it:fdstat}} Description{p_end}
{p2line}
{p2col :{opt asobs:erved}} at observed values in the sample (default){p_end}
{p2col :{opt mean}}   means{p_end}
{p2col :{opt median}} medians{p_end}
{p2col :{opt p1}}     1st percentile{p_end}
{p2col :{opt p2}}     2nd percentile{p_end}
{p2col :{it:...}}     3rd-49th percentiles{p_end}
{p2col :{opt p50}}    50th percentile (same as {cmd:median}){p_end}
{p2col :{it:...}}     51st-97th percentiles{p_end}
{p2col :{opt p98}}    98th percentile{p_end}
{p2col :{opt p99}}    99th percentile{p_end}
{p2col :{opt min}}    minimums{p_end}
{p2col :{opt max}}    maximums{p_end}
{p2col :{opt zero}}   fixed at zero{p_end}
{p2line}
{p2colreset}{...}

{pstd}
When no {it:fdstat} is specified, {cmd:(asobserved)} is assumed.  If
{cmd:(}{it:fdstat}{cmd:)} is not followed by a {it:varlist},
{cmd:(}{it:fdstat}{cmd:)} is ignored.  The various {it:fdstat}s are computed
using the estimation sample.


{marker nunit}{...}
{title:Syntax of nunit()}

{pstd}
{cmd:nunit()} can be used only in combination with {cmd:firstdiff()}, and the
variables listed in the two options must match.  {cmd:nunit()} takes just one
specification,

{p 12 12 2}
{cmd:(}{it:#}{cmd:)} {it:varlist}

{pstd}
where

{p 12 15 2}
1. {cmd:(}{it:#}{cmd:)} indicates the unit increase for the respective
variable.

{p 12 15 2}
2. {cmd:(}{it:#}{cmd:)} must be a single value (that is, numeric lists are not
allowed).

{p 12 15 2}
3. All or a subset of the interacted variables can be listed after the same
{cmd:(}{it:#}{cmd:)}.  Alternatively, separate values for the unit increase
can be specified for each individual variable.  For example, both
{cmd:nunit((5) x1 x2 x3)} and {cmd:nunit((3) x1 (10) x2 (2) x3)} are valid
arguments.  The former specification evaluates a 5-unit increase in {cmd:x1},
{cmd:x2}, and {cmd:x3}, whereas the latter a 3-unit increase in {cmd:x1}, a
10-unit increase in {cmd:x2}, and a 2-unit increase in {cmd:x3}.

{p 12 15 2}
4. Only one {cmd:(}{it:#}{cmd:)} can be applied to a given covariate.  If more
than one is specified, the rightmost specification is respected.  For example,
{cmd:nunit((1) x1 x2 (2) x1 x3)} evaluates a 1-unit increase in {cmd:x2}, and
a 2-unit increase in {cmd:x1} and {cmd:x3}.

{pstd}
If {cmd:nunit()} is missing, {cmd:ginteff} automatically computes the effect
of a one-unit increase for all variables in {cmd:firstdiff()}.  Thus,
{cmd:ginteff, fd(x1 x2) nunit((1) x1 x2)} produces the same result as
{cmd:ginteff, fd(x1 x2)}.


{marker examples}{...}
{title:Examples}

{pstd}
These examples are intended for quick reference.  For a more detailed overview
of {cmd:ginteff} and examples with discussion, see 
{browse "ginteff_manual.pdf":{it:The ginteff User's Manual}}.

{pstd}
Setup

{phang2}{cmd:. webuse nhanes2f}{p_end}
{phang2}{cmd:. keep health diabetes race female age height weight}{p_end}
{phang2}{cmd:. clonevar health_2l = health}{p_end}
{phang2}{cmd:. recode health_2l (1/3=0) (4/5=1)}{p_end}

{pstd}
Example 1: Compute the average and observation-level interaction effects for
factor variables

{phang2}{cmd:. logit health_2l b0.female##b1.race age height weight, nolog}{p_end}
{phang2}{cmd:. ginteff, dydxs(female race) obseff(obs_fr) level(90)}{p_end}

{pstd}
Example 2: Compute the average interaction effect for continuous variables via
the partial derivative

{phang2}{cmd:. logit health_2l c.age##c.height##c.weight i.female i.race, nolog}{p_end}
{phang2}{cmd:. ginteff, dydxs(age height weight)}{p_end}

{pstd}
Example 3: Compute the average interaction effect for continuous variables via
the first difference

{phang2}{cmd:. ginteff, firstdiff(age height weight) nunit((1) age height weight)}{p_end}

{pstd}
Example 4: Compute the average interaction effect for multiequation models

{phang2}{cmd:. biprobit (health_2l = i.female##i.race age) (diabetes= c.age##i.female), nolog}{p_end}
{phang2}{cmd:. ginteff, dydxs(female race) predict(p11) inteq(#1)}{p_end}

{pstd}
Example 5: Compute the average interaction effect for models with a multilevel
dependent variable

{phang2}{cmd:. ologit health c.age##i.female, nolog}{p_end}
{phang2}{cmd:. ginteff, dydxs(age female) atdxs((mean) age) predict(outcome(#2))}{p_end}

	   
{marker results}{...}
{title:Stored results}

{pstd}
{cmd:ginteff} stores the following in {cmd:r()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:r(N)}}number of observations{p_end}
{synopt:{cmd:r(N_psu)}}number of sampled primary sampling units, survey data only{p_end}
{synopt:{cmd:r(N_strata)}}number of strata, survey data only{p_end}
{synopt:{cmd:r(df_r)}}variance degrees of freedom, survey data only{p_end}
{synopt:{cmd:r(level)}}confidence level of confidence intervals{p_end}

{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:r(cmd)}}{cmd:ginteff}{p_end}
{synopt:{cmd:r(cmdline)}}command as typed{p_end}
{synopt:{cmd:r(est_cmd)}}{cmd:e(cmd)} from original estimation results{p_end}
{synopt:{cmd:r(est_cmdline)}}{cmd:e(cmdline)} from original estimation results{p_end}
{synopt:{cmd:r(fdstat)}}the {cmd:firstdiff()} specification{p_end}
{synopt:{cmd:r(model_vce)}}{it:vcetype} from estimation command{p_end}
{synopt:{cmd:r(obseff)}}the list of new variables created because of the {cmd:obseff()} option{p_end}
{synopt:{cmd:r(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:r(atstat)}}the {cmd:at()} specification{p_end}

{p2col 5 20 24 2:Matrices}{p_end}
{synopt:{cmd:r(at)}}matrix of values from the {cmd:at()} option{p_end}
{synopt:{cmd:r(b)}}the interaction effect estimates{p_end}
{synopt:{cmd:r(fd)}}matrix of values from the {cmd:firstdiff()} option{p_end}
{synopt:{cmd:r(ginteff)}}matrix containing the average interaction effects with their standard errors, test statistics, {it:p}-values, upper and lower confidence limits, and critical values{p_end}
{synopt:{cmd:r(nunit)}}matrix of values from the {cmd:nunit()} option{p_end}
{synopt:{cmd:r(V)}}variance-covariance matrix of the interaction effect estimates{p_end}
{p2colreset}{...}

{pstd}
{cmd:ginteff} with the {cmd:post} option also stores the following in
{cmd:e()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(N_psu)}}number of sampled primary sampling units, survey data only{p_end}
{synopt:{cmd:e(N_strata)}}number of strata, survey data only{p_end}
{synopt:{cmd:e(df_r)}}variance degrees of freedom, survey data only{p_end}

{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:ginteff}{p_end}
{synopt:{cmd:e(properties)}}{cmd:b V}{p_end}

{p2col 5 20 24 2:Matrices}{p_end}
{synopt:{cmd:e(b)}}estimates{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of the estimates{p_end}

{p2col 5 20 24 2:Functions}{p_end}
{synopt:{cmd:e(sample)}}marks estimation sample{p_end}
{p2colreset}{...}


{marker manual}{...}
{title:The User's Manual}

{pstd}
{browse "ginteff_manual.pdf":{it:The ginteff User's Manual}}{p_end}


{marker author}{...}
{title:Author}

{pstd}
Marius Radean{break}
Department of Government{break}
University of Essex{break}
Colchester, U.K.{break}
mradean@essex.ac.uk


{marker alsosee}{...}
{title:Also see}

{p 4 14 2}
Article:  {it:Stata Journal}, volume 23, number 2: {browse "https://doi.org/10.1177/1536867X231175253":st0711}{p_end}
