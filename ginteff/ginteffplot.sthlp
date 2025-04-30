{smcl}
{* *! version 1 21Nov2022}{...}
{viewerjumpto "Title" "ginteffplot##title"}{...}
{viewerjumpto "Syntax" "ginteffplot##syntax"}{...}
{viewerjumpto "Description" "ginteffplot##description"}{...}
{viewerjumpto "Options" "ginteffplot##options"}{...}
{viewerjumpto "Syntax of obseff[()]" "ginteffplot##obseff"}{...}
{viewerjumpto "Syntax of save()" "ginteffplot##save"}{...}
{viewerjumpto "Examples" "ginteffplot##examples"}{...}
{viewerjumpto "The User's Manual" "ginteffplot##manual"}{...}
{viewerjumpto "Author" "ginteffplot##author"}{...}
{viewerjumpto "Also see" "ginteffplot##alsosee"}{...}
{cmd:help ginteffplot}{right: ({browse "https://doi.org/10.1177/1536867X231175253":SJ23-2: st0711})}
{hline}

{marker title}{...}
{title:Title}

{p2colset 5 20 22 2}{...}
{p2col :{cmd:ginteffplot} {hline 2}}Graph results from ginteff{p_end}


{marker syntax}{...}
{title:Syntax}

{p 8 19 2}
{cmd:ginteffplot}[{cmd:,} {it:options}]

{synoptset 30 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt:{cmd:{ul:aiep}oint(}{it:aiepoint_options}{cmd:)}}customize marker for the point estimate of the average interaction effect{p_end}
{synopt:{cmd:{ul:aier}ange(}{it:aierange_options}{cmd:)}}customize the range plot of the average interaction effect using capped spikes{p_end}
{synopt:{cmd:obseff}[{cmd:(}{it:obseff_options}{cmd:)}]}plot observation-level interaction effects{p_end}
{synopt:{opt output(#)}}identify the {cmd:ginteff} output to be graphed; default is {cmd:output(1)}{p_end}
{synopt:{opt save(save_options)}}export current graph{p_end}
{synopt:{cmd:{ul:xcom}mon(}[{it:numlist}] [{cmd:*}]{cmd:)}}give {it:x} axes common scale{p_end}
{synopt:{cmd:zeroline}[{cmd:(}{it:line_options}{cmd:)}]}add a vertical line at the 0 value{p_end}

{syntab:{it:x} and {it:y} axes}
{synopt:{opt xt:itle(axis_title)}}customize {it:x} axis title{p_end}
{synopt:{opt yt:itle(axis_title)}}specify {it:y} axis title{p_end}
{synopt:{opt xlab:el(rule_or_values)}}customize ticks and labels for {it:x} axis{p_end}
{synopt:{opt ylab:el(rule_or_values)}}specify ticks and labels for {it:y} axis{p_end}
{synopt:{opt xsc:ale(axis_suboptions)}}customize how {it:x} axis looks{p_end}
{synopt:{opt ysc:ale(axis_suboptions)}}specify how {it:y} axis looks{p_end}

{syntab:Plot and graph areas}
{synopt:{cmdab:aspect:ratio(}{it:#} [{cmd:,} {it:pos_option}]{cmd:)}}plot region aspect ratio{p_end}
{synopt:{opt graphr:egion(suboptions)}}customize attributes of graph region{p_end}
{synopt:{opt plotr:egion(suboptions)}}customize attributes of plot region{p_end}
{synopt:{opt scheme(schemename)}}customize the graphics scheme{p_end}
{synopt:{opt xsiz:e(#)}}change width of graph{p_end}
{synopt:{opt ysiz:e(#)}}change height of graph{p_end}

{syntab:Titles, legend, and notes}
{synopt:{cmd:{ul:leg}end(}[{it:contents}] [{it:location}]{cmd:)}}standard
legend with contents and location{p_end}
{synopt:{opt note(tinfo)}}note about graph{p_end}
{synopt:{opt ti:tle(tinfo)}}overall title{p_end}
{synopt:{opt sub:title(tinfo)}}subtitle of the graph{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}


{marker description}{...}
{title:Description}

{pstd}
{cmd:ginteffplot} graphs the results of the immediately preceding {cmd:ginteff} command.		


{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{cmd:aiepoint(}[{it:#_clockposstyle}] [{cmd:"}{it:text for label}{cmd:"}]
[{cmd:,} {it:marker_label_options marker_options}]{cmd:)} customizes the
marker for the point estimate of the average interaction effect.  The default
is {cmd:aiepoint((12) "AIE", msymbol(S) mcolor(black) mlabsize(medsmall)}
{cmd:mlabcolor(black) mlabgap(*5))}.  {manhelpi clockposstyle G-4} customizes
the location of the label relative to the point.  
{manhelpi marker_label_options G-3} customize the overall look and color of
the marker, the size and color of the label text, and the space between the
marker and the label.  {manhelpi marker_options G-3} customizes the shape,
color, and size of the marker.

{phang}
{cmd:aierange(}[{it:line_options}] [{opt msize(markersizestyle)}]{cmd:)}
customizes the range plot of the average interaction effect, using capped
spikes (I-beams) to connect the upper and lower confidence limits.  The
significance level (typically 95%) is set by {cmd:ginteff}'s {cmd:level()}
option.  The default is {cmd:aierange(lpattern(solid) lcolor(black)}
{cmd:lwidth(medthick) msize(medium))}.  {manhelpi line_options G-3} customize
the look of the line used to draw the spikes and caps, including pattern,
width, and color.  {manhelpi markersizestyle G-4} changes the width of the
cap.

{phang}
{cmd:obseff}[{cmd:(}{it:obseff_options}{cmd:)}] plots the observation-level
interaction effects for each case in the sample data.  {cmd:obseff} is ignored
if the preceding {cmd:ginteff} command was specified without the
{cmd:obseff()} option.  {cmd:ginteffplot} graphs only estimated parameters; if
the individual interaction effects were not computed via {cmd:ginteff}, there
is nothing to plot.  For more information, see the section
{bf:Syntax of obseff[()]} below.

{phang}
{opt output(#)} identifies the {cmd:ginteff} output to be graphed and is
relevant only when there is more than one set of results.  Multiple results
occur when you previously fit a multiequation model or specified more than one
{cmd:at()} scenario.  For instance, {cmd:output(1)} would plot the results
displayed in the first row of the {cmd:ginteff} output, {cmd:output(2)} would
mean the second row, and so on.  The default is {cmd:output(1)}.

{phang}
{cmd:save(}{it:newfile}{cmd:.}{it:suffix}[{cmd:,} {it:export_options}]{cmd:)}
exports the graph displayed in a Graph window to a file.  For more
information, see the section {bf:Syntax of save()} below.

{phang}
{cmd:xcommon(}[{it:numlist}] [{cmd:*}]{cmd:)} specifies that the graph be put
on a common {it:x}-axis scale with the graphs corresponding to the
{cmd:ginteff} outputs listed in the suboption of {cmd:xcommon()}.  You can
specify one other output, {it:#}, a subset of outputs, {it:numlist}, or all
outputs, {cmd:*}.

{phang}
{cmd:zeroline}[{cmd:(}{it:line_options}{cmd:)}] adds a vertical line at the 0
value.  This is typically used when the confidence interval of the interaction
effect contains zero to graphically indicate that the effect is statistically
insignificant at the specified significance level.  The default is
{cmd:zeroline(lpattern(shortdash) lwidth(medthin) lcolor(red))}.  These
settings are used if {cmd:zeroline} is used without suboptions.  If zero falls
within the equally spaced values on the {it:x} axis (see option
{cmd:xlabel()}), its label will be displayed under the major {it:x}-axis
values using a tick 3.5 times as long as the default, {cmd:tlength(*3.5)}.
{manhelpi line_options G-3} customize the look of the line, including pattern,
width, and color.

{dlgtab:x and y axes}

{phang}
{opt xtitle(axis_title)} and {opt ytitle(axis_title)} specify or customize the
title to appear on the {it:x} and {it:y} axes.  For the {it:x} axis, the
default is {cmd:xtitle("Change in }{it:depvar}{cmd:", size(4))}, where
{it:depvar} is the dependent variable's label or, if it does not have a label,
its name.  {cmd:xtitle()} customizes the title text and font size.  The {it:y}
axis is not titled, and specifying {cmd:ytitle()} adds a title.  For more
information, see {manhelpi axis_title_options G-3}.

{phang}
{opt xlabel(rule_or_values)} and {opt ylabel(rule_or_values)} specify or
customize the major values to be labeled and ticked along the {it:x} and
{it:y} axes.  The default is {cmd:ylabel(none)} and
{cmd:xlabel(xmin(`=(xmax-xmin)/5')xmax)}.  The
{cmd:xmin(`=(xmax-xmin)/5')xmax} rule specifies that the minimum and maximum
values, along with four equally spaced intermediate values, are to be labeled
and ticked along the {it:x} axis.  The xmin and xmax values are retrieved
automatically from the {cmd:ginteff} output.  For more information, see
{manhelpi axis_label_options G-3}.

{phang}
{opt xscale(axis_suboptions)} and {opt yscale(axis_suboptions)} customize the
look of the {it:x} and {it:y} axes.  The default is
{cmd:xscale("titlegap(4)")} and {cmd:yscale(titlegap(0) range(0 2))}.  For
more information, see {manhelpi axis_scale_options G-3}.

{dlgtab:Plot and graph areas}

{phang}
{cmd:aspectratio(}{it:#} [{cmd:,} {it:pos_option}]{cmd:)} specifies the aspect
ratio and, optionally, the placement of the plot region.  For example, when
{cmd:aspectratio(1)}, the height and width will be equal (their ratio is 1),
and the plot region will be square; when {cmd:aspectratio(2)}, the plot region
is twice as tall as it is wide; and, when {cmd:aspectratio(0.5)}, the plot
region is twice as wide as it is tall.  For more information, see
{manhelpi aspect_option G-3}.

{phang}
{opt graphregion(suboptions)} customizes attributes for the graph region.
The default is {cmd:graphregion(fcolor(white))}.  For more information, see
{manhelpi region_options G-3}.

{phang}
{opt plotregion(suboptions)} customizes attributes for the plot region.
The default is {cmd:plotregion(margin(sides))}.  For more information, see
{manhelpi region_options G-3}.

{phang}
{opt scheme(schemename)} customizes the graphics scheme to be used.  The
default is {cmd:scheme(s2mono)}.  For more information, see
{manhelpi scheme_option G-3}.

{phang}
{opt xsize(#)} changes the width of the graph.

{phang}
{opt ysize(#)} changes the height of the graph.

{dlgtab:Titles, legend, and notes}

{phang}
{opt legend([contents] [location])} defines the contents of the standard
legend, along with how it is to look and whether and where it is to be
displayed.  The default is {cmd:legend(off)}.  For more information, see
{manhelpi legend_options G-3}.

{phang}
{opt note(tinfo)} specifies notes to be displayed with the graph.  The default
is {cmd:note("")}, which means no notes.  For more information, see 
{manhelpi title_options G-3}.

{phang}
{opt title(tinfo)} and {opt subtitle(tinfo)} specify the overall title and
subtitle of the graph.  The default is {cmd:title("")} and {cmd:subtitle("")},
which means no title or subtitle.  For more information, see 
{manhelpi title_options G-3}.


{marker obseff}{...}
{title:Syntax of option obseff[()]}

{p 8 17 2}
{cmdab:obseff}[{cmd:(}{it:obseff_options}{cmd:)}]

{synoptset 30}{...}
{synopthdr:obseff_options}
{synoptline}
{synopt:{opt marker(marker_options)}}customize markers{p_end}
{synopt:{opt median(median_options)}}add marker label for the median{p_end}
{synopt:{cmd:pctile}[{cmd:(}{cmdab:alt:def}{cmd:)}]}plot only 101 representative values{p_end}
{synoptline}
{p2colreset}{...}


{title:Suboptions of obseff()}

{phang}
{opt marker(marker_options)} customizes the shape, color, and size of markers
indicating the observation-level interaction effects.  The default is
{cmd:marker(msymbol(O) mcolor(black) msize(vtiny))}.  For more information,
see {manhelpi marker_options G-3}.

{phang}
{cmd:median}[{cmd:(}[{it:#_clockposstyle}] [{cmd:"}{it:text for label}{cmd:"}]
[{cmd:,} {it:marker_label_options marker_options}]{cmd:)}] adds and customizes
the marker for the median value of the variable containing the
observation-level interaction effects.  The default is {cmd:median((6)}
{cmd:"(median)", msymbol(Oh) mcolor(black) mlabsize(medsmall) mlabcolor(black)}
{cmd:mlabgap(*5))}.  These settings are used if {cmd:median} is used without
suboptions.  {manhelpi clockposstyle G-4} customizes the location of the label
relative to the point.  {manhelpi marker_label_options G-3} customize the
overall look and color of the marker, the size and color of the label text,
and the space between the marker and the label.  {manhelpi marker_options G-3} customize the shape, color, and size of the marker.

{phang}
{cmd:pctile}[{cmd:(}{cmd:altdef}{cmd:)}] plots only the 1st, 2nd, ..., 99th
percentiles, as well as the minimum and maximum values of the variable
containing the observation-level interaction effects (101 values in total).
The specific variable is created using {cmd:ginteff}'s {cmd:obseff()} option.
For large datasets, with thousands of observations (or more), plotting the
effect for each observation can overload the graph and significantly increase
the file-size of the figure.  If there are fewer than 99 observations, option
{cmd:pctile} is ignored.  The default method for calculating percentiles is to
invert the empirical distribution function by using averages, {x_i +
x_(i+1)}/2, where the function is flat.  When the suboption {cmd:altdef} is
specified, an alternative formula that uses an interpolation method is used.
For more information on the formulas used to compute percentiles (with or
without the {cmd:altdef} suboption), see {manhelpi pctile D}.


{marker save}{...}
{title:Syntax of save()}

{p 8 16 2}
{cmd:save(}{it:newfile}{cmd:.}{it:suffix}
[{cmd:,}
{it:export_options}]{cmd:)}

    {it:export_options}{col 35}Description
    {hline 69}
    {cmd:name(}{it:windowname}{cmd:)}{...}
{col 35}name of graph window to export 
    {cmd:as(}{it:fileformat}{cmd:)}{...}
{col 35}desired format of output
    {cmd:replace}{...}
{col 35}{it:newfile} may already exist
    {it:override_options}{...}
{col 35}override defaults in conversion
    {hline 69}

{pstd}
If {cmd:as()} is not specified, the output format is determined by the suffix
of {it:newfile}{cmd:.}{it:suffix}:

    {col 20}Implied
    {it:suffix}{col 20}option{col 35}Output format
    {hline 69}
    {cmd:.ps}{col 20}{cmd:as(ps)}{col 35}PostScript
    {cmd:.eps}{col 20}{cmd:as(eps)}{col 35}EPS (Encapsulated PostScript)
    {cmd:.svg}{col 20}{cmd:as(svg)}{col 35}SVG (Scalable Vector Graphics)
    {cmd:.emf}{col 20}{cmd:as(emf)}{col 35}EMF (Enhanced Metafile)
    {cmd:.pdf}{col 20}{cmd:as(pdf)}{col 35}PDF (Portable Document Format)
    {cmd:.jpg}{col 20}{cmd:as(jpg)}{col 35}JPEG (Joint Photographic Experts Group)
    {cmd:.png}{col 20}{cmd:as(png)}{col 35}PNG (Portable Network Graphics)
    {cmd:.tif}{col 20}{cmd:as(tif)}{col 35}TIFF (Tagged Image File Format)
    {cmd:.gif}{col 20}{cmd:as(gif)}{col 35}GIF (Graphics Interchange Format)
    {it: other}{col 35}must specify {cmd:as()}
    {hline 69}
{phang}
{cmd:tif} is not available for Stata(console); {cmd:emf} is available only for
Stata for Windows; and {cmd:gif} is available only for Stata for Mac.

    {it:override_options}{col 35}Description
    {hline 69}
    {it:{help ps_options}}{...}
{col 30}when exporting to {cmd:.ps}
    {it:{help eps_options}}{...}
{col 30}when exporting to {cmd:.eps}
    {it:{help svg_options}}{...}
{col 30}when exporting to {cmd:.svg}
    {it:{help tif_options}}{...}
{col 30}when exporting to {cmd:.tif}
    {it:{help png_options}}{...}
{col 30}when exporting to {cmd:.png}
    {it:{help gif_options}}{...}
{col 30}when exporting to {cmd:.gif}
    {it:{help jpg_options}}{...}
{col 30}when exporting to {cmd:.jpg}
    {hline 69}

{phang}
There are no {it:override_options} for the PDF format.

{title:Suboptions of save()}

{phang}
{cmd:name(}{it:windowname}{cmd:)} specifies which window to export from when
exporting a graph.  The name for a window is displayed inside parentheses in
the window title.  For example, if the title for a Graph window is {cmd:Graph}
{cmd:(MyGraph)}, the name for the window is {cmd:MyGraph}.  If a graph is an
{cmd:asis} or {cmd:graph7} graph where there is no name in the window title,
specify {cmd:""} for {it:windowname}.

{phang}
{cmd:as(}{it:fileformat}{cmd:)} specifies the file format to which the graph
is to be exported.  The default is the format from the suffix of the file
being created.

{phang}
{cmd:replace} specifies that it is okay to replace
{it:filename}{cmd:.}{it:suffix} if it already exists.

{phang}
{it:override_options}
    modify how the graph is converted.  See 
    {manhelpi ps_options G-3},
    {manhelpi svg_options G-3},
    {manhelpi eps_options G-3},
    {manhelpi gif_options G-3},
    {manhelpi jpg_options G-3},
    {manhelpi tif_options G-3}, and
    {manhelpi png_options G-3}.

	
{marker examples}{...}
{title:Examples}

{pstd}
These examples are intended for quick reference.  For a more detailed overview
of {cmd:ginteffplot} and examples with discussion, see
{browse "ginteffplot_manual.pdf":{it:The ginteffplot User's Manual}}.

{pstd}
Setup

{phang2}{cmd:. webuse nhanes2f}{p_end}
{phang2}{cmd:. keep health race female age}{p_end}
{phang2}{cmd:. clonevar health_3l = health}{p_end}
{phang2}{cmd:. recode health_3l (2=1) (3=2) (4/5=3)}{p_end}
{phang2}{cmd:. ologit health_3l i.race##i.female age, nolog level(90)}{p_end}
{phang2}{cmd:. ginteff, dydxs(female race) obseff(obs_ol2w) level(90)}{p_end}

{pstd}
Example 1: Plot the average interaction effect from the third row of the
{cmd:ginteff} output

{phang2}{cmd:. ginteffplot, output(3)}{p_end}

{pstd}
Example 2: Plot the average and individual interaction effects

{phang2}{cmd:. ginteffplot, output(3) obseff}{p_end}

{pstd}
Example 3: Plot the average and individual interaction effects, and more

{phang2}{cmd:. ginteffplot, output(3) obseff(median) zeroline}{p_end}


{marker manual}{...}
{title:The User's Manual}

{pstd}
{browse "ginteffplot_manual.pdf":{it:The ginteffplot User's Manual}}{p_end}


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
