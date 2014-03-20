# datamining-functions.r
# Created: 		  03-Jul-2013
# Modified: 	  25-Jul-2013
# Author: 		  Daniel Mastropietro
# Description: 	Set of Data Mining functions
# Dependencies: startup-functions.r, from which the following functions are used:
#               - checkVariables()
# R version:    R-2.8.0 (used in SPSS 18.0.0 @NAT starting 2013)
#

# INDEX:
# Profiles, DistibutionsByGroup (these are the same function with 2 different names, the first is more easily remembered in data mining)
# InformationValue
# ScoreDistribution

################################ Profiles, DistributionsByGroup ###############################
# 2013/07/03
# Description: Generates the distribution of continuous variables by a set of grouping variables
# Application: Client profiles by predicted group of a scoring model (e.g. the VERY BEST vs. the VERY BAD)
#
# HISTORY: (2013/07/12)
#          - Added parameter 'method' in order to estimate the histogram using a varying bin size which is determined
#					 by equal-size groups of the density value (i.e. equal-size groups on the vertical axis!)
#
# TODO:
# -[DONE-2013/07/12] 2013/07/12: Show a histogram with varying breaks, defined by the slope of the estimated density.
# This is easily done by choosing the breaks based on the density values.
# Here is an example:
# x = xexp(100)
# dens = density(x)
# xsupp = which(min(x) <= dens$x & dens$x <= max(x))
# # Cut the density values in 10 groups of equal width (equal in the vertical axis!)
# D = as.data.frame(cbind(x=dens$x[xsupp],y=dens$y[xsupp],g=cut(dens$y[xsupp],breaks=10)))
# D = cbind(D, b=c(NA,diff(D$g)))
# View(D)
# # Define the breaks given by the x values where assigned group g changes
# breaks = D$x[which(abs(D$b)==1)]
# 
# h = hist(x, plot=FALSE)
# h2 = hist(x, breaks=c(min(x), breaks, max(x)), plot=FALSE)
# View(cbind(breaks=c(min(x), breaks), mid=h2$mids, dens=h2$density, freq=h2$counts))
# 
# par(mar=c(5,5,4,5))
# ylim = range(c(h$density, h2$density))*1.1
# plot(h, freq=FALSE, col=rgb(1,0,0,0.2), ylim=ylim)
# lines(D, col="red") 
# plot(h2, add=TRUE, freq=FALSE, labels=as.character(h2$counts), cex=0.5, col=rgb(0,1,0,0.1))
#
# - 2013/08/02: Give the user the possibility of plotting the NUMBER OF CASES on the vertical axis instead of the percent frequency.
#               Ref: Roman
# - 2013/08/02: In the case of plotting the percent frequency of both the histogram and the density, try to figure out a way to make the scale of the densities
#               so that they look like they sum up to 1 when computing the area underneath. Recall the observation by Antoine that in some cases it is clear that
#               the areas are not comparable since one density curve (Good accounts) is contained completely inside the other one! (Bad accounts)
#               What I need to change is the adjustment of the scale dens$y, which I have defined via trial and error in order to have its values
#								be about the height of the histogram when using percentages as the vertical scale (this is done twice in the code).
#               I think the solution would be to use the same process I do in ScoreDistribution where I plot the histogram in frequencies and show the percents on the
#               vertical axis, by simply putting the correct labels. Similary in this case, I would leave the scale of the histogram bars and the density unchanged
#               and just show the percentages on the vertical axis adding the appropriate labels (but I am not sure how this would behave when overlaying histograms of two groups)
#               Ref: Antoine
# - 2013/12/13: Change the definition of the default colors to use for groups by using as many colors as the number of groups to plot
#								equally spaced on a color palette that goes e.g. between blue and red or between magenta and red as the rainbow.
#								For this the function colorRampPalette() may come handy, which defines a FUNCTION that takes the number of colors
#								as an argument.
#								Ex:
#								colorPaletteFun = colorRampPalette(c("blue","red"))
#								colors = colorPaletteFun(ngroups)	# to actually generate the colors (in hexadecimal format)
# - 2014/02/10: Add rug plots below the density plot when no histogram is plotted. This plot consists of a set of tick marks
#								showing where the data is concentrated, thus giving support to the density estimation. Rug plots can be added to
#								a density plot using the rug() function which is part of the graphics package.
#								Ref: http://www.analyticbridge.com/profiles/blogs/exploratory-data-analysis-kernel-density-estimation-and-rug-plots
Profiles <- DistributionsByGroup <- function(
  data,                             # Name of the data matrix or data frame with the variables to analyze
  vars,                             # Blank-separated string with the names of the continuous variables to analyze
  groupvars=NULL,                   # Blank-separated string with the names of the group variables defining the populations for which distributions are created.
  transforms=NULL,                  # Blank-separated string of function names to use to transform the variables in vars before computing their distribution (accepted values are: "log", "safeLog", "." (for no transformation))
  valueLabels=NULL,                 # Blank-separated string of characters "." or "T" stating whether the original or transformed variable values should be used as labels for the horizontal axis (defaults to all ".")
  labelOrientation=3,               # Integer number (1, 2, 3, 4) defining the orientation of the text in the way that parameter 'las' in mtext() does.
  stats="q5 median q95",            # Blank-separate string indicating the statistics to show on the graph (e.g. mean, median, q25, etc.)
  method="FD",                      # Method to use to determine the histogram bins. Possible values are: "density" and any other values accepted by the breaks parameter of hist().
  alltogetherGroups=TRUE,           # Whether to overlay the distributions of all groups on the same graph
  histFlag=FALSE,                   # Whether to show the histogram as well (besides the density)
  cdfFlag=FALSE,                    # Whether to show the CDF instead of the PDF as density
  xadjustFlag=FALSE,                # Whether to adjust the ANALYSIS range on which histograms and densities are based to the values given in parameter xlim
  xlabSimpleFlag=TRUE,              # Whether to show simple labels on the horizontal axis, meaning only min(x) and max(x) and any requested statistics in 'stats'
  xlim=NULL,                        # Range to be used for the histogram and density of each analyzed variable (see also: xadjustFlag)
  colors="_AUTO_",                  # String with the color names to use for each population defined by the variables listed in groupvars
  save=FALSE,                       # Whether the output plot should be saved to a file
  saveDir=NULL,                     # Directory where the output plot is saved when save=TRUE
  jpegfile="r_graph_dbg_plot.jpg")  # Name of the JPEG file that stores the output plot
# EXAMPLE:
# tofit = read.csv("E:\\Daniel\\Documents\\Dropbox\\NAT\\tofit_v3_M01.csv")
# salario_log = tofit$ZL_V_EQX_SALARIO
# salario = sign(salario_log)*(10^salario_log - 1)
# tofit$salario = salario
#---- Client profiles ----
# score.dist = summary(tofit$score)
# Q1 = score.dist[["1st Qu."]]
# Q3 = score.dist[["3rd Qu."]]
# toplot = tofit[which(tofit$score<Q1 | tofit$score>Q3),]
# # Good and Bad group
# toplot$goodbad = ifelse(toplot$score<Q1, "0-Good", "1-Bad")
# DistributionsByGroup(
#  toplot,
#  "N_CLI_NUMBERPH_QNT N_EQX_EDAD P_CLI_MONTO_RATEPLAN_SALARIO salario",
#  groupvars="goodbad",
#  transforms="safelog . safelog safelog",
#  valueLabels=". . . T",
#  stats="mean q10 q90",
#  method="density",
#  altogetherGroups=TRUE,
#  histFlag=TRUE,
#  cdfFlag=TRUE,
#  xadjustFlag=FALSE,
#  xlim=NULL,
#  colors=c("green","red"),
#  save=FALSE)
#
{

  
#----- DEFINE AUXILIARY FUNCTIONS -----#
# Computation of histogram
histogram = function(x, density, method="density") {
	# Compute histogram (the definition of the bins or breaks is defined by 'method')
	if (tolower(method) == "density") {
    # Support of x for selection of histogram breaks
    xsupp = which(min(x) <= density$x & density$x <= max(x))
    densx = density$x[xsupp]
    densy = density$y[xsupp]
    # Cut the density values into 10 groups of equal width (equal in the vertical axis! (the density))
    # This method gives smaller bins in the vertical density axis when the density value varies less...
    # When going back from this bins to the horizontal data axis (x), the result is that there will be
    # more bins in regions of x where the density function varies more (i.e. larger first derivative).
    densityGroups = cut(densy, breaks=10)
    densityGroupChanges = c(NA, diff(densityGroups))    # Add a NA at the first element of the vector since diff() returns one less element than in the original vector being differenced
    # Define the breaks given by the x values where the assigned group g changes
    breaks = densx[which(abs(densityGroupChanges)==1)]
    if (length(breaks) >= 5) {
      # Compute histogram using these breaks
      hist = hist(x, breaks=c(min(x), breaks, max(x)), plot=FALSE)
    } else {
      # Compute histogram using Sturges breaks when there the number of bins is too small
      hist = hist(x, breaks="Sturges", plot=FALSE)
    }
	} else {
    ### Use the method given by the user, but if this method is FD, the number of brekas may be only 1 or 2 when the variable takes very few distinct values...
    ### i.e. when it could be regarded as categorical. For such cases, use Sturges method instead.
    # Try making the histogram with the method given by the user and check if the number of breaks, hist$breaks, is large enough.
	  hist = hist(x, breaks=method, plot=FALSE)
	  if (tolower(method) != "sturges" & length(hist$breaks) < 5)
      hist = hist(x, breaks="Sturges", plot=FALSE)
	}
	
	return(hist)
}

# Function that transforms x based on the name passed in parameter 'fn' which should be a name listed in constant VALIDTRANSFORMS
transform = function(x, fn) {
  switch(fn,
         LOG=log10(x),
         SAFELOG=sign(x)*log10(1+abs(x)),
         x)
}

# Function that inverse transforms y based on the name passed in parameter 'fn' which should be a name listed in constant VALIDTRANSFORMS
itransform = function(y, fn) {
  switch(fn,
         LOG=10^y,
         SAFELOG=sign(y)*(10^abs(y)-1),
         y)
}

# Function that creates the label to show on the X axis to indicate that the variable has been transformed
transformLabel = function(fn) {
  switch(fn,
         LOG="(log scale)",
         SAFELOG="(log scale)",
         "")
}

# Plot initialization
initializePlot = function(objplot) {
  ## objplot:     Plot object with the characteistics of the plot to initialize
  plot(objplot$x, objplot$y, xlim=objplot$xlim, ylim=objplot$ylim, type="n", main="", xlab=objplot$xlab, ylab="", xaxt=objplot$xaxt)
  if (objplot$xaxt == "n") {
    # Add the horizontal tick marks and labels: the tickmark positions are defined by the transformed x variable and the labels are defined by the original  x variable.
    axis(1, at=objplot$xaxpos, labels=FALSE);
    mtext(objplot$xaxlab, side=1, at=objplot$xaxpos, line=1, cex=cex, las=objplot$xaxlas);
    # When xlabSimpleFlag is TRUE and statistics have been requested, add a text indicating the names of the requested statistics next to the axis
    if (xlabSimpleFlag & nstats > 0) {
      mtext(paste("(", stats, ")", sep=""), side=1, at=at.stats, line=-0.1, cex=0.8*cex, las=objplot$xaxlas, adj=0)    # adj=0 means left-aligned
    }
  }
  # Only show the vertical axis label for plots shown on the first column of the output tiling
  # Note that the value of mfg is only updated AFTER the plot has been made!! In fact the first value of mfg is NOT "1 1 m n", but "m n m n"!
  mfg = par("mfg")
  if (mfg[2] == 1)
    mtext(objplot$ylab, side=2, line=3, cex=cex)

  return(mfg)
}
#----- DEFINE AUXILIARY FUNCTIONS -----#

  
#----- DEFINE CONSTANTS ------#
# Valid transformations to apply to the analyzed variables
VALIDTRANSFORMS = c(".", "LOG", "SAFELOG")
  ## The "." transformation (i.e. no transformation) has to be the FIRST element because of how VALIDTRANSFORMS is treated in the MAIN section of this program.
  ## SAFELOG is the symmetric log (taken from the name used in SPSS)
VALIDVALUELABELS = c(".", "T")
#----- DEFINE CONSTANTS ------#


#------------------------------ Parse Input parameters ------------------------------#
### Convert strings passed in parameters to arrays so that the user can pass these arguments as a string
# VARS
vars = unlist(strsplit(vars,"[ \n]"))   # [ \n] is a regular expression that indicates to use either the space or the new line character (\n) as a splitting character.
nvars = length(vars)
# GROUPVARS
if (!is.null(groupvars)) {
  groupvars = unlist(strsplit(groupvars, "[ \n]"))
} else {
  # No grouping variable => Plot only one distribution for a fictitious group which is the same for all observations
  groupvars = "SINGLEGROUP_"
  # Add a variable containing the fictitious group variable (all observations equal to 0)
  data$SINGLEGROUP_ = 0
}
ngroupvars = length(groupvars)
# TRANSFORMS
if (!is.null(transforms)) {
  # Convert 'transforms' to a vector and capitalize all letters
  transforms = unlist(strsplit(toupper(transforms),"[ \n]"))
  ntransforms = length(transforms)
  if (ntransforms != nvars) {
    # Note that the stop() function automatically displays an error message before the message I write here.
    stop("The number of transform functions given in parameter 'transforms' (", ntransforms, ") is different from the number of variables passed in 'vars' (", nvars, ")")
  }
  # Check validity of function names passed (the function match() below returns NA for a value in the first argument not found in the second argument)
  transformMatch = match(transforms, VALIDTRANSFORMS)
  if (any(is.na(transformMatch))) {
    cat("WARNING: The function name specified in one or more transformations given in parameter 'transforms' is not valid.\n")
    cat("         The corresponding variablewill NOT be transformed.\n")
    cat("         Valid function names are: ", VALIDTRANSFORMS, "\n")
  }
} else {
  # Set the transforms vector to all "." which means "no trasformation" to the corresponding variable passed in 'vars'
  transforms = rep(".", nvars)
}
# VALUELABELS
if (!is.null(valueLabels)) {
  # Convert 'valueLabels' to a vector and capitalize all letters
  valueLabels = unlist(strsplit(toupper(valueLabels),"[ \n]"))
  nvalueLabels = length(valueLabels)
  if (nvalueLabels != nvars) {
    stop("ERROR: the number of label values given in parameter 'valueLabels' (", valueLabels, ") is different from the number of variables passed in 'vars' (", nvars, ")")
  }
  # Check validity of function names passed (the function match() below returns NA for a value in the first argument not found in the second argument)
  valueLabelsMatch = match(valueLabels, VALIDVALUELABELS)
  if (any(is.na(valueLabelsMatch))) {
    cat("WARNING: One or more label values specified in parameter 'valueLabels' is not valid.\n")
    cat("         The original labels will be used for the horizontal axis of the corresponding variable.\n")
    cat("         Valid labels values are: ", VALIDVALUELABELS, "\n")
  }
} else {
  # Set the valueLabels vector to all "." which means "no trasformation" to the corresponding variable passed in 'vars'
  valueLabels = rep(".", nvars)
}
# STATS
nstats = 0
if (!is.null(stats)) {
  # Set the default value when an empty string is passed (instead of NULL)
  if (stats == "") { stats = c("q5 median q95") }
  stats = unlist(strsplit(stats, "[ \n]"))
  nstats = length(stats)
  statFuns = stats
  statValues = rep(0,nstats) # Set a non-NA value to statValues[s] as default value. 
  # This value is updated when the statistic is a quantile and it does NOT affect the result of normal statistics (e.g. mean, median, etc.).
  # Identify the statistic function (when quantiles are passed on the form "q<N>")
  for (s in 1:nstats)
    if (tolower(substring(statFuns[s],1,1)) == "q") {
      statValues[s] = as.numeric(substring(statFuns[s],2))/100
      statFuns[s] = "quantile"
    }
  # Define the location values to show on the overall distribution
  if ("median" %in% tolower(stats)) {
    locFun = "median"
  } else {
    locFun = "mean"
  }
}

# Reduce the data to analyze to the variables passed as parameters
data = data[, c(vars,groupvars)]
#print(str(data))
nobs = nrow(data)

# Define whether the data should be analyzed only on a specified range of values
if (is.null(xlim)) {
	xlimFlag = FALSE
	xlim = NA
} else {
	xlimFlag = TRUE
	# Convert the string to a vector of numbers
#	xlim = as.double(unlist(strsplit(xlim, " ")))
}
#------------------------------ Parse Input parameters ------------------------------#



#----------------------------------- OUTPUT FILE ------------------------------------#
if (save) {
  if (is.null(saveDir)) { saveDir = getwd() }
  # Open a file to store the plots (the file size is an A4 page)
  jpgfilename = file.path(saveDir, jpegfile)
  jpeg(file=jpgfilename, width=950, height=950)
  	## units: 	defines the dimension of the plot. Note that it is set to cm, although based on A4 being 210x297 mm(*) it should be mm... (???) ... it seems to be a bug in R?
  	## res:		the resolution parameter needs to be set every time the units are NOT px (pixels, the default)
  	##			it was set by TRIAL and ERROR as I do not understand how it works!!
  cat("Graph stored in JPG file: ", jpgfilename, "\n")
}
#----------------------------------- OUTPUT FILE ------------------------------------#


#--------------------------------- GROUP VARIABLE -----------------------------------#
### ONLY ONE GROUP VARIABLE IS ALLOWED!!!
# Get the values taken by the group variables
tab = table(data[,groupvars])
groupvalues = dimnames(tab)[[1]]
ngroupvalues = length(groupvalues)
groupvalueslen = sapply(groupvalues,nchar)	# Count the number of characters in each group value (used to determine the space to leave for the legend)
maxchar = 20; groupvaluesLegend = sapply(groupvalues,substring,1,maxchar)	# Cut the value to show for each group value in the legend to maxchar

# Store the GROUP variable name indicated in the 'groupvars' array and assign it to a scalar for easier handling in the loop below.
gvar = groupvars[1]
#--------------------------------- GROUP VARIABLE -----------------------------------#


#-------------------------------- GRAPHICAL SETTINGS --------------------------------#
# Store current settings
opar = par(no.readonly=TRUE)
	## Note the use of no.readonly=TRUE, so that only the parameters that can be changed are returned
	## thus avoiding an error when resetting the graphical parameters on exit below.
# Restore the graphical settings when this function exits (either normally or by an error)
#on.exit(par(mfrow=opar$mfrow, mar=opar$mar, oma=opar$oma, usr=opar$usr, cex=opar$cex, ps=opar$ps))
## The above was left as reference until I verify that using par(opar) when opar has been created using no.readonly=TRUE works fine.
on.exit(par(opar))

# Create the tiling of the graph
if (nvars == 1 & !alltogetherGroups) {
  nrows = ngroupvalues
} else {
  nrows = nvars
  alltogetherGroups = TRUE
}
ncols = 1
if (nvars > 3)
{
	# Define the tiling structure when the number of variables is larger than 3, so that it is as square as possible,
	# but give priority to displaying the variables into different rows and then into different columns.
	ncols = floor(sqrt(nvars))
	nrows = ncols
	if (sqrt(nvars) > ncols)		# Ex: nvars = 5, create a 3 x 2 tiling, but if nvars = 4 (a perfect square), create a 2 x 2 tiling
		nrows = nrows + 1
	if (nvars / ncols > nrows)	# Ex: nvars = 7, create a 3 x 3 tiling (instead of 3 x 2 because not all the 7 graphs fit in a 3 x 2 tiling)
		ncols = ncols + 1
}

# Use this when the legend is placed at the right of the graph (see end of code for the legend placement)
#spaceLegend = max(nchar(gvar), max(sapply(groupvaluesLegend,nchar)) + 4 + (floor(log10(nobs))+1) + 4)
	## First '+4' is used to leave space for the lines shown in the legend representing each group
	## floor(log10(...)) is used to leave space for the group sizes (nobs is the number of observations in the dataset to plot)
	## Second '+4' is used to leave space for the string added to the group sizes (like "(n=....)") 
#par(mfrow=c(nrows,ncols), mar=c(6,5,0.5,1), oma=c(0,0,4,spaceLegend), ps=18)
# Use this when the legend is placed at one of the corners of the graph (see end of code for the legend placement)
#spaceLegend = 0
#par(mfrow=c(nrows,ncols), mar=c(6,5,0.5,1), oma=c(0,0,4,0), ps=18)
# Use this when the legend is placed at the bottom of the graph (see end of code for the legend placement)
spaceLegend = ngroupvalues + 2
## The space left for the legend when its location is at the bottom is based on the following facts:
## - The values of the mar and oma parameters are "lines of text" (as specified in the par() documentation)
## - When there are n entries in the legend and we use the default values of font size (cex=1) and line separation (y.intersep=1)
## each legend entry occupies exactly one line of text.
## - I leave some more lines (+2) for the title of the legend
par(mfrow=c(nrows,ncols), mar=c(6,5,0.5,1), oma=c(spaceLegend,0,4,0), ps=18)
	## mar setting: leave no margin at the top nor at the right of the graphs
	## oma setting: leave space for the overall title and the legend for the whole graph (note that this is independent of the tiling and does not affect the space in each plot)
	## ps setting:	font size (in points, e.g. 12 = 12pt)
# Read the cex value for the above plot configuration (cex gets smaller as the number of plots in the same graph increases)
# and reduce its value a little bit
cex = 0.8*par("cex")
par(cex=cex)
if (histFlag & cdfFlag) {
	# Prepare for the secondary plot (leave space for the secondary axis)
	# Note that I change "mar" NOT "oma". The value of mar was previously set to [5 5 0.5 1] so now I increase the right margin by 2 to get enough space
	# to add a vertical label on the right (using line=2)
	mar = par("mar")
	par(mar=mar+c(0,0,0,2))
}

# Colors
colornames = c("blue", "red", "green", "yellow", "orange", "cyan", "violet")
colorpalette = c("#255B89", "#9D0E2D", "#6AA94E", "#FFC726", "#ED8000", "#6CCBED", "#92499E") # This is CS palette...
if (is.null(colors) || max(toupper(colors) == "_AUTO_")) {      # Use the max() function because 'colors' may be a vector (this avoids a Warning message of comparing a vector with a string)
	# CS palette (taken from "Corporate-Identity-Richtlinien B.11, 19-Jan-2007")
	# Only apply these colors when the number of groups to plot is less than or equal to the number of colors in the palette.
	# If not, use rainbow colors based on the number of groups
	if (ngroupvalues <= length(colorpalette)) {
		colors = colorpalette
	} else {
		colors = rainbow(ngroupvalues)
	}
} else {
	if (max(toupper(colors) == "_RAINBOW_")) {      # Use the max() function because 'colors' may be a vector (this avoids a Warning message of comparing a vector with a string)
		colors = rainbow(ngroupvalues)
	} else {
		# Convert the string given by the user into a vector of color names
		colors = unlist(strsplit(tolower(colors), "[ \n]"))
		indmatch = match(colors, colornames)                # The order of the 2 vectors being compared is important, as I want the indices of vector colornames where the values given in vector colors are found.
		indmatch = indmatch[!is.na(indmatch)]
		# Only set the colors to the color palette if ALL colors given by the user are found in the list
		if (length(indmatch) == length(colors))
			colors = colorpalette[indmatch]
		# Complete the colors with rainbow colors when less colors are specified than number of groups in the data
		if (length(colors) < ngroupvalues)
		    colors = c(colors, rainbow(ngroupvalues-length(colors)))     # Note that this does NOT take into account the problem of having the same color for 2 groups when the user specifies a color given by rainbow()) 
	}
}
# Create RGB colors from color names to be used when generating overlapping histograms with added transparency
# Note: the RGB components for each color are stored on different columns
rgbcolors = col2rgb(colors)/255
print(rgbcolors)

### Define some general plot parameters
ylab = "Density"
if (cdfFlag)
    ylab = "Probability (%)"
if (histFlag)
    ylab = paste(ylab, "/ % Frequency")
#-------------------------------- GRAPHICAL SETTINGS --------------------------------#


#-------------------------------------- MAIN ----------------------------------------#
### Iterate on the analyzed variables
for (i in 1:nvars)
{
	cat("\nAnalyzing variable", i, "of", nvars, ":", vars[i], "...\n")

	#--------------------------------------- RESET ------------------------------------------------
	### Reset the Y range and the list of histograms and density estimations for ALL values of x and of x by each group
	ylim = NULL
	zlim = c(0,100)		# Range for the CDF (recall that the CDF is expressed in percentages)
	histList = list()
	densList = list()
	#--------------------------------------- RESET ------------------------------------------------


	#----------------------------- COMPUTE HISTOGRAMS/DENSITIES -----------------------------------
	### Compute the histograms and/or densities for all groups in order to define the Y axis limits
	### Note that it is NOT possible to adjust the Y axis on the fly while doing the histograms/densities
	# Get the overall range
	x = data[!is.na(data[,i]),i]
	if (xlimFlag & xadjustFlag)
		# Reduce the values to analyze to the specified limits
		x = x[xlim[1] <= x & x <= xlim[2]]
	# Transform the variable according to the function indicated in parameter 'transforms' and keep a copy of the original variable in 'xorig' (used to show the labels of the horizontal axis)
	xorig = x
	x = transform(x, transforms[i])
	# Compute the density on x
	dens = density(x)
	# Compute CDF (from 0 to 100%)
	dens$z = cumsum(dens$y)
	dens$z = dens$z / max(dens$z) * 100
	# Store the original x values in 'dens' to be used as labels on the x tick marks
	dens$xorig = xorig
  hist = histogram(x, dens, method=method)
	if (!xlimFlag)
		# Adjust the X limits only when no user-specified limits were passed
#		xlim = range(hist$breaks, dens$x[which(dens$x>=min(x) & dens$x<=max(x))])   # Limit the range of the density to the range of x (as the density estimation will usually go beyond these limits)
		xlim = range(x) # Limit the range of X as when plotting the density, its values usually go beyond the range of X.
	print(xlim)
#	if (histFlag & hist$equidist)
	if (histFlag) {
		# Convert densitites to percentages when plotting both the histogram and the density so that the vertical axis is more understandable to the user
		# Note that hist$density = (hist$counts / sum(hist$counts)) / diff(hist$breaks), that is the shorter the bin size the larger sum(hist$density) is
		# (because diff(hist$breaks) is the bin size (for equal bins as is the case when method != "density") and it appears dividing in the expression of hist$density)
		hist$density = hist$density / sum(hist$density) * 100
		# Note that for the density estimation, we cannot simply adjust by dens$y*100 or dens$y/max(dens$y)*100 or dens$y/sum(dens$y)*100.
		# They do not work to make the density estimation be at the same height as the histogram percentages...
		# I found that the following works very well. It is not exactly the same as the matching between the original hist$density (relative value)
		# and dens$y but it is really pretty close (at least with the examples I tried: income and debtinc variables from the bankloan.sav dataset.
		dens$y = dens$y / max(dens$y) * max(hist$density)
		# 2013/08/05 (after observation by Antoine): Trying to scale differently so that the densities are in comparable areas but still does not work...
#		 dens$y = dens$y / sum(hist$density) * 100
	}
print("BREAKS")
print(hist$breaks)
	# Define the range of the Y axis (based on overall histogram/density)
	ylim = c(0, max(hist$density, dens$y))
	# Store the current histogram/density
	histList[[1]] = hist
	densList[[1]] = dens

	# Distribution for each group
 	# Matrix used to store the vertical limits on each plot (used only when alltogetherGroups = FALSE, that is when distributions are plotted on separate graphs for each group)
  vlims = matrix(NA, nrow=ngroupvalues, ncol=2)
  for (g in 1:ngroupvalues)
	{
		# Value of the GROUP variable currently analyzed
		gval = groupvalues[g]
		ind = which(as.character(data[,gvar])==as.character(gval) & !is.na(data[,i]))
		if (length(ind) > 0)
		{
			x = data[ind,i]
			if (xlimFlag & xadjustFlag)
				# Reduce the values to analyze to the specified limits
				x = x[xlim[1] <= x & x <= xlim[2]]
			# Transform the variable according to the function indicated in parameter 'transforms' and store a copy of the original variable
            xorig = x
			x = transform(x, transforms[i])
			# Compute the density
			dens = density(x)
			# Compute CDF (from 0 to 100%)
			dens$z = cumsum(dens$y)
			dens$z = dens$z / max(dens$z) * 100
      # Store any statistics requested in parameter 'stats'
      if (nstats > 0) {
        for (s in 1:nstats) {
          dens$stats[s] = eval(call(statFuns[s], xorig, statValues[s])) # This is equivalent to using do.call(stats[s], list(xorig)) (note the use of a list as second argument)
        }
      }
	    # Compute the histogram
	    hist = histogram(x, dens, method=method)
#			if (histFlag & hist$equidist) {
			if (histFlag) {
		    # Only adjust the density to represent the percentages when the bins are of equal size (this usually happens when method is NOT "density" unless the "density" method did not work out)
		    # Only in the case of equal bin size does the density of the histogram represent percentages.
		    # (the percentages in the case when the bin sizes are different is given by hist$counts / sum(hist$counts) * 100 but plotting this is not what we want to show because
		    # it would be a distorted visual representation of a histogram, precisely because of the varying bin size)
				hist$density = hist$density / sum(hist$density) * 100
				dens$y = dens$y / max(dens$y) * max(hist$density)
		    # 2013/08/05 (after observation by Antoine): Trying to scale differently so that the densities are in comparable areas but still does not work...
#        dens$y = dens$y / sum(hist$density) * 100
			}
  		# Store the vertical limits for the current group and update the range of the Y axis
      vlims[g,] = range(hist$density, dens$y)
  		ylim = c(0, max(c(ylim, vlims[g,])))
  		# Store the current histogram/density
  		histList[[g+1]] = hist
  		densList[[g+1]] = dens
		}
	}
	cat("Limits of Y axis:\n")
	print(ylim)
	#----------------------------- COMPUTE HISTOGRAMS/DENSITIES -----------------------------------


	#----------------------------- HISTOGRAM / DENSITY PLOT ---------------------------------------
	### 1.- Create the plot
  xorig = densList[[1]]$xorig
	x = densList[[1]]$x
	y = densList[[1]]$y
	if (histFlag) {
		# Allow space for the labels on top of each histogram bar
		ylim = 1.1*ylim
		vlim = ylim
	} else {
		if (cdfFlag) {
			# Prepare plot for the CDF only
			y = densList[[1]]$z
			vlim = zlim
		} else {
			# Prepare the scale for the PDF only
			vlim = ylim
		}
	}
	cat("Updated limits of Y axis:\n")
	print(vlim)
    # Variable name label
	xlab = paste(vars[i], transformLabel(transforms[i]))  # The call to transformLabel adds text stating the transformation used, if any
	
    #-- Define the tick marks
    # There are 2 methods to define the tick marks ("at" values) so that the values shown are "pretty":
    # (M1) Define first the LABELS (based on the original scale => labels=pretty(range(xorig))) and then the POSITIONS ("at", based on those labels => at=transform(labels, transforms[i])
	# (M2) Define first the POSITIONS ("at", based on the transformed scale => at=pretty(range(x))) and then the LABELS (based on those positions => labels=itransform(at, transforms[i]))
	if (xlabSimpleFlag) {
	  # Only show the min, and max values and any statistics requested in 'stats'
    at.stats = array(dim=nstats)
	  if (nstats > 0)
	    for (s in 1:nstats) {
	      at.stats[s] = transform(do.call(statFuns[s], list(xorig, statValues[s])), transforms[i])
	    }
	  at = c(min(x), at.stats, max(x))
	  if (!is.null(labelOrientation)) {
      las = labelOrientation
    } else {
      las = 1  # las = 1: no rotation of text (used in mtext() function below)
    }
	} else {
    at = pretty(range(x))
	  if (!is.null(labelOrientation)) {
      las = labelOrientation
    } else {
      las = 3  # las = 3: rotate text 180 degrees (used in mtext() function below)
    }
	}

  #-- Define the labels to show on the horizontal axis, either the original or transformed x values
  # The labels to show depend on the values of parameters 'transforms', 'valueLabels' for current variable
  # and overall parameter 'xlabSimpleFlag', which indicates whether simple labels should be used.
  # For 'transforms' and 'valueLabels', the "." value means ORIGINAL scale of variable, any other value means transformed scale.
  xaxt = "n"
	if (transforms[i] != "." & valueLabels[i] == ".") {
        # Labels should show original scale on a transformed axis
        labels = as.character(formatC(itransform(at, transforms[i]), format="g", digits=2)) # when format="g", digits is the number of SIGNIFICANT digits and E+ format is used if needed!
	} else {
    # Labels should show axis values (which are either original or transformed)
    if (xlabSimpleFlag) {
        labels = as.character(formatC(at, format="g", digits=2)) # when format="g", digits is the number of SIGNIFICANT digits and E+ format is used if needed!
    } else {
        xaxt = "s"  # This is the default value of xaxt which means "square" (I think)
    }
	}
	
  #-- Initialize the plots (one for each group when alltogetherGroups=False)
  objplot = new("list")
  objplot$x = x
  objplot$y = y
  objplot$xlim = xlim
  objplot$ylim = vlim
  objplot$xlab = xlab
  objplot$ylab = ylab
  objplot$xaxt = xaxt
  objplot$xaxpos = at
  objplot$xaxlab = labels
  objplot$xaxlas = las
  if (!alltogetherGroups) {
      # Create one plot for each group
      for (g in 1:ngroupvalues) {
          # Define vertical range based on the values to be plotted in each graph and do this independently of what is plotted on the other graphs.
          # The vertical range is defined in matrix vlims.
      	if (histFlag) {
      		# Allow space for the labels on top of each histogram bar
      		vlims[g,] = 1.1*vlims[g,]
      	} else {
      		if (cdfFlag) {
      			# Prepare plot for the CDF only
      			objplot$y = densList[[g+1]]$z
      			vlims[g,] = zlim     # zlim is the range [0, 100]
      		}
      	}
          objplot$ylim = vlims[g,]
          # Only show the horizontal axis label for the plot shown at the bottom when groups are shown on separate plots.
          objplot$xlab = ""
          mfg = initializePlot(objplot)
          if (mfg[1] == ngroupvalues) {
              mtext(xlab, side=1, line=3, cex=cex)
          }
      }
  } else {
      initializePlot(objplot)
  }
	
	### 2.- Iterate on the different values taken by the GROUP variable and for each of them add a histogram/density on the current plot
	size = numeric(0)
	legend = character(0)
	for (g in 1:ngroupvalues)
	{
    if (!alltogetherGroups) {
        # When not all groups should be plotted together on the same graph, select the tile where the current group should be plotted
        par(mfg=c(g,1,nrows,1)) # Recall that nrows = ngroupvalues in this case (as set at the GRAPHICAL SETTINGS section)
    }
		size[g] = 0
		if (length(histList[[g+1]]$x) > 0) {
			size[g] = sum(histList[[g+1]]$count)
			if (histFlag) {
				# Plot the histogram (the labels add the percentage of occurrence of each bin)
				#labels = as.character(round(histList[[g+1]]$counts/sum(histList[[g+1]]$counts)*100))	# Labels are the % cases
				labels = as.character(histList[[g+1]]$counts)											# Labels on each bar are the # cases
				plot(histList[[g+1]], add=TRUE, freq=FALSE, labels=labels, xlab="", ylab="", cex=cex, col=rgb(rgbcolors[1,g],rgbcolors[2,g],rgbcolors[3,g],1/10), border=colors[g], xaxt="n")
				if (cdfFlag) {
					par(new=TRUE)	# This setting only stays for the next plot() call
					plot(densList[[g+1]]$x, densList[[g+1]]$z, xlim=xlim, ylim=c(0,100), xaxt="n", yaxt="n", xlab="", ylab="", type="l", lwd=2, col=colors[g])
					if (g == 1)
						# Add the tickmarks only after the plot for the first group.
						# NOTE that since for every plot we use the same ylim values ([0 1]) it is no problem about adding the axis tickmarks only at the first plot,
						# that is there will be no shift problem!
#						axis(4, at=pretty(densList[[g+1]]$z))	# pretty() creates aesthetically 'nice' tickmarks.
						axis(4, at=seq(0,100,20))   # Recall that the CDF scale is in percentages (i.e. from 0 to 100%)
					mfg = par("mfg")
					if (mfg[2] == mfg[4])
						# Indicate that the second axis shows the CDF (only shown for the rightmost plots in the output tiling)
						mtext("CDF", side=4, line=2, cex=cex)
					# Reset the scale of the vertical axis scale (so that the histogram for the next group can be added)
					par(new=TRUE)
					plot(x, y, xlim=xlim, ylim=vlims[g,], xlab="", ylab="", type="n", xaxt="n")
				} else {
					# Add estimated non parametric density
					lines(densList[[g+1]]$x, densList[[g+1]]$y, lty=1, lwd=2, col=colors[g])
				}
			} else {
				if (cdfFlag) {
					# Plot CDF
					lines(densList[[g+1]]$x, densList[[g+1]]$z, lty=1, lwd=2, col=colors[g])
				} else {
					# Plot PDF
					lines(densList[[g+1]]$x, densList[[g+1]]$y, lty=1, lwd=2, col=colors[g])
				}
			}
      # Add the requested statistics for each group (e.g. mean)
      if (nstats > 0) {
        # Read the coordinates of the plot (which are used below in a couple of places)
        usr = par("usr")	    # usr contains the following coordinates (xlower,xupper,ylower,yupper)
        lines = c(-0.1, -0.1) # Alternating positions of the statistics values w.r.t. the axis (0: outside the axis; 1: inside the axis)
                              # When different values are assigned, the goal is to avoid collision between texts.
        # Position of the vertical lines showing the statistics
        at = transform(densList[[g+1]]$stats, transforms[i])
        for (s in 1:nstats) {
          abline(v=at[s], col=colors[g], lty=2, lwd=2)
          if (valueLabels[i] == "T") {
            # Show the values of the statistics in the transformed scale (note that the transformation can also be "no transformation")
            labels = paste(stats[s],"=", formatC(transform(densList[[g+1]]$stats[s], transforms[i]), format="g", digits=2), sep="")
            ## Note the "g" format in the formatC() function so that 'digits' corresponds to the minimum number of SIGNIFICANT digits, and the E+ format is used if the number becomes too long, NICE!
          } else {
            # Show the values of the statistics in the original scale
            labels = paste(stats[s],"=", formatC(densList[[g+1]]$stats[s], format="g", digits=2), sep="")
          }            
          if (!alltogetherGroups) {
            if (!is.null(labelOrientation)) {
              las = labelOrientation
            } else {
              las = 1
            }
          } else {
            if (!is.null(labelOrientation)) {
              las = labelOrientation
            } else {
              las = 3
            }
          }
          if (las == 3) {
            # Shift a little bit the text shown to avoid overlap with the statistic line
            shift = -0.01*(usr[2]-usr[1])
          } else {
            shift = 0
          }
          mtext(labels, side=3, line=lines[s%%2+1], at=at[s]+shift, col=colors[g], cex=0.8*cex, las=las, adj=1)  # adj=1 means right-aligned
        } # for (s in 1:nstats)
        # When no histogram was requested, shade the area between the extreme values of the statistics requested (for better visibility)
        if (!histFlag) {
          polygon(c(min(at), min(at), max(at), max(at)), c(usr[3], usr[4], usr[4], usr[3]), density=NA, col=rgb(rgbcolors[1,g],rgbcolors[2,g],rgbcolors[3,g],1/10))
        }
      }
	  }
    # Add the legend
    legend[g] = paste(groupvaluesLegend[g], " (n=", size[g], ")", sep="")
	}

	### 3.- Add a legend
	# Get the graph coordinates so that I can place the legend at the top-left of the plot
	#usr = par("usr")	# usr contains the following coordinates (xlower,xupper,ylower,yupper)
	#legend(usr[1], 0.98*usr[4], legend=legend, title=groupvars, cex=0.8, lty=1, lwd=2, col=colors[1:ngroupvalues])
	#----------------------------- HISTOGRAM / DENSITY PLOT ---------------------------------------
}

# Add an overall title and a legend indicating the values of the GROUP variable being plotted
par(mfrow=c(1,1))
#-- Title
mtext(paste("GROUPING VARIABLE:", groupvars), line = 2, outer = TRUE, cex=0.8)
#-- Legend
# Reset the margins so that I can place the legend. Note that it is IMPORTANT to reset the oma parameter to all zeros
# so that the "right" position of the legend corresponds to the right position of the WHOLE FIGURE!
# It is also important to set the "usr" coordinates to (0,1,0,1) because the position of the legend is defined in terms of the [0,1] interval in the figure!!
par(mfrow=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0), usr=c(0,1,0,1))
legend("bottom", legend=legend , title=groupvars, cex=0.7, y.intersp=1, lty=1, lwd=2, col=colors[1:ngroupvalues], bg=rgb(0.7,0.7,0.7,1/10), horiz=FALSE)
  ## - y.intersp defines the separation between legend entries (1 is normal separation, 0.8 would be a smaller separation)
  ## - bg value means "grey fill with transparency of 1/10"
#usr = par("usr")	# usr contains the following coordinates (xlower,xupper,ylower,yupper)
#legend((1-spaceLegend)*usr[2], usr[4], legend=groupvalues , title=groupvars, cex=1, lty=1, lwd=2, col=colors[1:ngroupvalues], horiz=TRUE)

#----- OLD ------
# Reset the margins and usr coordinates so that I can place the legend at the top right corner of the figure
#par(mar=c(0,0,0,0), oma=c(0,0,0,0), usr=c(0,1,0,1))
#usr = par("usr")	# usr contains the following coordinates (xlower,xupper,ylower,yupper)
#legend("topright", legend=groupvalues , lty=1, lwd=2, col=colors[1:ngroupvalues], horiz=TRUE, xpd=TRUE)
	## xpd=TRUE:	All additions to the plot are done to the figure region (as opposed to the plot region)
#legend((1-0.01*ngroupvalues)*usr[2], usr[4], legend=groupvalues , title=groupvars, cex=0.8, lty=1, lwd=2, col=colors[1:length(groupvalues)], horiz=TRUE)
	## 1-0.10*space:	Leave 10% of the space to be used by each group value or by the group variable name, whatever is longer
	## horiz=TRUE:	Place the different lines represeting the groups horizontally, instead of vertically.
#----- OLD ------

if (save) {
  # Close the file with the plot
  dev.off()
}

}
################################ Profiles, DistributionsByGroup ###############################



######################################## InformationValue #####################################
# 2013/07/25
# Description: Information Value for categorical and continuous variables on a binary variable
# Application: Variable screening for a predictive model
#
# HISTORY: (2013/08/08)
#          - Took care of missing values (NA and NaN) both in the target and in the input variables. The treatment of missing values is as follows:
#             - Target: they are removed from the analysis
#             - Input: they are kept in the analysis as a valid level (BOTH for categorical and continuous variables!). Missing values appear as
#               the last category of the variable and they are labeled "NaN" (as NaN values are considered valid by the aggregate() function used
#               to compute the information value and all NA values are replaced by NaN prior to computing the information value).
#               (Note that I tried placing the NaN values as the first category but I didn't succeed because the behaviour of my method in the
#               case of categorical variables was not as expected and I didn't understand what R did in that case (there was a problem when converting
#               the analyzed variable to a factor and specifying the levels of the variable (where NaN were placed as the first level), but then
#               NaN and Inf may not appear as valid levels in the computation of the information value...))
#           - Added new columns and new information to the WOE table, namely:
#             - the number of cases in each group of the input variable
#             - the percentage of each level of the target (FIRST and LAST)
#             - the total WOE of the variable as the sum of the WOE on the different groups of the input variable (this could be used as an indicator
#             of what direction is the variable providing more information on, whether it is on the Bads or on the Goods; for example if the event of
#             interest for the WOE computation is chosen to be the LAST level of a target variable taking the values 0 & 1, then a positive TOTAL WOE
#             would indicate that the information on the target provided by the input variable is coming mostly from categories where the odss 1 over 0
#             is much larger than the global odds 1 over 0)
#						(2014/01/06)
#						- Fixed a not-so-wished grouping of values of continuous variables when the number of different cases taken by the variable
#						is smaller than the number of groups requested making some of the groups too large
#					  (this happens often with variables that take a few different integer values).
#						This was a quite elaborate fix since a check of the frequency of occurrence of the different cut values in the data
# 					is needed and based on that the actual cutvalues are defined (either including or not including -Inf as the first cut value)
#						- Fixed an error when the breaks to use for continuous variables are given by the user in parameter 'breaks'.
#						The fix is indicated with a comment holding the current date (2014/01/06) and is given by adding just one line:
#							groups = factor(groups)
#						which removes non-occurring groups from factor variable 'groups', as is done by the iv() function computing the
#						Information Value.
#
# TODO:
# -[DONE-2013/08/04] 2013/07/25: Need to fix a problem in the iv() auxiliary function that happens when one of the values taken by the target
# does NOT show up in some of the values of the categorized x variable. The error happens after transposing the data frame y.count
# because instead of having 2 columns containing the counts of each target value, there is only 1!
# Ex:
#   Group.1  Group.2    x
# 1     0-OK (-Inf,4]    1
# 2     0-OK    (4,5]    6
# 3 1-NOPAGO    (4,5]    8
# 4     0-OK    (5,6] 2088
# 5 1-NOPAGO    (5,6] 1400
# 6     0-OK    (6,7]  688
# 7 1-NOPAGO    (6,7]  292
# 8 1-NOPAGO    (7,8]    2
# Group.1          x
# 1 (-Inf,4]          1
# 2    (4,5]       6, 8
# 3    (5,6] 2088, 1400
# 4    (6,7]   688, 292
# 5    (7,8]          2
# Error in y.count[, 2][, class1] : incorrect number of dimensions
# instead of having two columns and 0's in the second data frame... Should be:
# Group.1     x.1   x.2
# 1 (-Inf,4]    0     1
# 2    (4,5]    6     8
# 3    (5,6] 2088  1400
# 4    (6,7]  688   292
# 5    (7,8]    0     2
# I THINK THIS IS EASILY SOLVED BY CHECKING THE NUMBER OF COLUMNS IN THE TRANSPOSED DATA FRAME AND THEN USING THE strsplit() function to convert the e.g. "6, 8" into "6", "8"
# and the "1" into "1", "0" on the first row of the above example; and the 
# - 2014/01/03: Add a weight parameter to compute a weighted Information Value. This need came about during the fit of the NBC model
# 							for 2014 where we used weights to take into account the rejected applications and the different number of applications
#								received over the months.
InformationValue = function(
  data,             # Dataset containing the variables to analyze listed in 'vars', 'varclass' and/or 'varnum
  target,           # Either an unquoted variable name or a string indicating the name of the binary target variable in 'data' on which the Information Value is computed
  vars=NULL,        # Blank-separated string with the names of the variables to analyze (they are assumed to be continuous)
  varclass=NULL,    # Blank-separated string with the names of the CATEGORICAL variables to analyze
  varnum=NULL,      # Blank-separated string with the names of the CONTINUOUS  variables to analyze (these variables together with those specified in VARS determine the continuous variables to analyze)
  groups=20,        # Number of groups into which the continuous variables are binned (equal-size bins)
  breaks=NULL,      # Vector defining the upper boundaries of the bins into which ALL CONTINUOUS variables are binned
  stat="mean",      # Statistic to compute on each bin of the analyzed continuous variables
  event="LAST")     # Event of interest ("FIRST" or "LAST") which is used as numerator in the WOE formula
  # Created: 2013/07/25
  # Modified: 2013/07/25
  # Descrtiption: Information Value for continuous and categorical variables on a binary target
  # Example:
  # iv = InformationValue(
  #   tofit,
  #   "B_TARGET",
  #   varclass="I_CLI_CANAL I_CLI_TYPE".,
  #   varnum="N_EQX_EDAD V_EQX_SALARIO")
{
  
#----- DEFINE AUXILIARY FUNCTIONS -----#
iv = function(y, group, x=NULL, event="LAST", stat="mean") {
  # Parameters:
  # y:      target variable on which the Information Value of x is computed
  # group:  group variable defining the different categories of x used to compute the Information Value
  # x:      continuous variable for which the Information Value is computed (set it to NULL when x is categorical)
  # event:  defines the event of interest for the WOE computation.
  #         Either the "FIRST" or the "LAST" ordered value out of the two values taken by variable 'y'
  #         is used at the numerator of the WOE formula: WOE = log(#Class1/#Class2)
  #         When event = "LAST", Class1 is the LARGEST of the values
  #         Otherwise, Class1 is the SMALLEST of the values
  # Output: A data frame containing the following columns:
  #         - group:  distinct categories of x used to compute the Information Value on y
  #         - stat:   statistic value for each category when x is continuous or NA when x is NULL (meaning that the variable on which the Information Value is computed is categorical)
  #         - woe:    Weight of Evidence for each category of x on y
  #         - iv:     contribution to the total Information Value by the corresponding category of x.
  #
  
  #---------------------- Parse input parameters -------------------------
  ### TARGET
  # Compute the levels of the target variable
  y.levels = data.frame(order=1:nlevels(as.factor(y)), row.names=levels(as.factor(y)))

  ### GROUP
  # Just in case, eliminate not-observed levels of the factor variable GROUP (this is done by applying the factor() function to the variable)
  group = factor(group)

  ### EVENT
  if (toupper(event) == "LAST") {
    # The following variables indicate the column numbers of the #groups X 2 matrix stored in
    # y.count.t[,2] below containing the counts of the level of y that should go in the numerator (class1)
    # and in the denominator (class2) of the WOE formula given above.
    class1 = 2
    class2 = 1
  } else {
    class1 = 1
    class2 = 2
  }
  #---------------------- Parse input parameters -------------------------
  
  # Count the occurrence of each class of Y by the values of group
  # (Note that NA and NaN values in 'y' are removed with function is.na())
  indok = !is.na(y)
  y.count = aggregate(y[indok], by=list(y[indok],group[indok]), FUN=length)
  y.count$newcol = y.levels[as.character(y.count$Group.1),] 
    ## It is important the user of as.character() to correctly index the rows of the data frame y.levels!

  # Transpose by group value: I create a new data frame where the row names contain the values taken by the group value
  # NOTE: I could have used the reshape() function to do this, which ALSO takes care of unexisting levels of the 
  # target variable for some group values (unlike the t() transpose operator, whose result is explained below)
  y.count.t = data.frame(row.names=levels(group), x.1=rep(0,nlevels(group)), x.2=rep(0,nlevels(group)))
  # Transpose y.count by Group.2 to get y.count.t
  for (i in 1:nrow(y.count)) {
    y.count.t[as.character(y.count$Group.2[i]), y.count$newcol[i]] = y.count$x[i]
      ## The as.character() function is again important in order to remove the 'factor' aspect of Group.2, o.w. there is an assignment error!
  }
  # NOTE: A possible other way to do the same is by using the transpose operator 't' applied on each value of Group.2 using the apply() function.
  # HOWEVER, this does not work for 2 reasons:
  # - When one of the levels of the 'y' variable is NOT present for one value of Group.2, the output has only 1 column instead of 2!
  # (and this is not what we want!)
  # - The transpose operator is NOT valid in R versions prior to R-2.11 as FUN can only return a scalar!!
  # (see help(aggregate)) => this process does not work in R for SPSS 18.0.0 which requires R-2.8.0 to function.    
  #y.count.t = aggregate(y.count$x, by=list(y.count$Group.2), FUN="t")
  ## NOTE that the result stored in y.count.t using the aggregate() function is a #groups x 2 data frame
  ## where the first column (named Group.1 --despite aggregating by Group.2 above)
  ## contains the distinct group values and the second column is a (#groups x 2) matrix!
  ## The matrix contains what we need to compute the WOE, i.e. the counts of each value of y by group.
  ## The first column of such matrix contains the count of the LOWEST value of y.
  ## The second column of the matrix contains the count of the LARGEST value of y.
  
  # Statistic value given in 'stat' for x on each group value
  if (!is.null(x)) {
    x.stat = aggregate(x, by=list(group), FUN=stat)
    x.stat = x.stat$x
  } else {
    x.stat = NA
  }
  # WOE and IV contribution for each group value
  count.classFIRST = y.count.t[,1]  # Count of the FIRST level of y (this is used to compute pctClassFIRST and pctClassLAST in the data frame returned by the function)
  count.classLAST = y.count.t[,2]   # Count of the LAST level of y (this is used to compute pctClassFIRST and pctClassLAST in the data frame returned by the function)
  count.class1 = y.count.t[,class1]
  count.class2 = y.count.t[,class2]
  count.total.class1 = sum(count.class1)
  count.total.class2 = sum(count.class2)
  # Note that I add 0.01 to the counts in class1 and class2 to avoid log(0) or divide by 0 when any of the counts is 0.
  # Note however that, before applying this formula to the WOE, I check whether the total cases on each group value
  # is different from 0 before computing the WOE because if there are no cases in a particular group value, the formula
  # for the WOE would give a misleading value (= log(0.01/0.01) - log(count.total.class1/count.total.class2)), and
  # instead the WOE --and also the IV for that group value-- should be undefined in that case (since there are no cases to evaluate the WOE!)
  woe = ifelse(count.class1+count.class2==0, NA, log( (count.class1+0.01) / (count.class2+0.01) ) - log( count.total.class1 / count.total.class2 ))
  iv = ifelse(count.class1+count.class2==0, 0, (count.class1/count.total.class1 - count.class2/count.total.class2) * woe)
  
  output = data.frame(group=rownames(y.count.t), stat=signif(x.stat, digits=4), nobs=count.class1+count.class2, pctClassFIRST=signif(count.classFIRST/(count.classFIRST+count.classLAST)*100, digits=3), pctClassLAST=signif(count.classLAST/(count.classFIRST+count.classLAST)*100, digits=3), woe=signif(woe, digits=4), iv=signif(iv, digits=4))
  return(output)
}
#----- DEFINE AUXILIARY FUNCTIONS -----#  
  
  #------------------------------- Parse input parameters -------------------------------------
  ### TARGET
  # Make a local copy of the target variable and compute its distinct values (there should be EXACTLY 2 values)
  # (recall that 'target' specifies the target variable WITHOUT quotes --as opposed to 'vars', 'varclass' and 'varnum')
  # Note: if 'target' is actually a STRING indicating the name of the variable, use:
  # table(eval(as.name(target)))
  # or
  # table(get(target))
  # Check whether parameter 'target' contains a string with a variable name or an actual variable
  if (is.character(target) & length(target)==1) {
    # A string was passed containing the name of the target variable in 'data'
    targetname = target
    if (target %in% colnames(data)) {
      target = data[,target]
    } else {
      stop("The target variable '", targetname, "' does not exist in dataset '", deparse(substitute(data)), "'")
    }
  } else {
    # A variable name was passed (note that in this case R automatically checks if the variable exists; also note that I don't know how to check whether the variable e.g. tofit$B_TARGET exists)
    targetname = deparse(substitute(target))
    target = target  # A local copy of the variable passed in target is stored because the variable passed could be e.g. data$y and we want to get rid of the data frame in future references to 'target'.
  }
  tab = as.data.frame(table(target))  ## By default NA and NaN values in 'target' are EXCLUDED from the frequency table.
  values = tab[,1]          # The first column of 'tab' contains the values taken by the target variable
  values = factor(values)   # Just in case, remove any non-occurring value of target
  if (nlevels(values) != 2) {
    # Note: deparse(substitute(target)) returns the variable name (instead of its value) passed in 'target'!
    stop("The target variable '", targetname, "' is not binary. It takes the following values:\n", toString(values))
  }

  ### VARS, VARCLASS, VARNUM
  if (!is.null(vars)) { vars = unlist(strsplit(vars,"[ \n]")) }
  if (!is.null(varclass)) { varclass = unlist(strsplit(varclass,"[ \n]")) }
  if (!is.null(varnum)) { varnum = unlist(strsplit(varnum,"[ \n]")) }
  # Construct all the continuous variables to analyze
  varnum = unique(c(vars, varnum))
  # Check the existence of the variables in the dataset 'data'
  varsNotFound = checkVariables(data=data, vars=c(varclass, varnum))
  if (!is.null(varsNotFound)) {
    stop("Execution of InformationValue() stops.\n")
  }

  ### BREAKS
  # Add the -Inf and +Inf values to the given breaks
  # (note the use of the unique() function to avoid repeated break values which are not accepted by the cut() function)
  if (!is.null(breaks)) {
    breaks = unique(c(-Inf, breaks, +Inf))
    cutvalues = breaks
  }
  ### DATA
  # Convert 'data' into a data frame for easier reference to the variables
  data = as.data.frame(data[,c(varclass, varnum)])
  # Assign the column names for data frame 'data'
  # (this is necessary when there is only ONE variable to analyze, in which case the column name
  # assigned to the single column in 'data' is, for instance, "data[,c(varclass)])")
  colnames(data) = c(varclass, varnum)  # Note that this is fine because in the above assignment, the columns of 'data' are read in the order given!
  #------------------------------- Parse input parameters -------------------------------------
  
  # Create the data frames to store the WOE and IV results for each variable
  WOE = as.data.frame(matrix(nrow=0, ncol=9))
  colnames(WOE) = c("var", "type", "group", "nobs", "pctClassFIRST", "pctClassLAST", stat, "woe", "iv")
  ## NOTE: The columns of WOE are of the following type:
  ## char, char, char, num, num, num, num, num, num => the values of 'group' are converted to character even though they may be originally numeric (in categorical variables)
  IV = as.data.frame(matrix(nrow=ncol(data), ncol=6))
  colnames(IV) = c("var", "type", "nobs", "nlevels", "TotalWOE", "IV")

  # Bin continuous variables in equal-size groups as many as given by parameter 'groups'
  # unless 'breaks' is NOT NULL in which case the values given in breaks are used for binning
  i = 0
  lastrow = 0
  cat("Information Value calculation on target variable", targetname, "\n")
  for (v in varclass) {
    i = i + 1
    cat("\t", i, ": Computing Information Value for categorical variable", v, "\n")
    
    # Number of levels in the categorical variable
    # First replace NA values with NaN so that missing values are considered as a possible value in the computation of the Information Value
    # Note that NaN values are considered as valid values of the BY variables in the aggregate() function used in the iv() function above,
    # but NA values are NOT (see help(aggregate))
    data[is.na(data[,v]), v] = NaN
    ## IMPORTANT: DO NOT SET THE ANALYZED VARIABLE 'v' AS A factor BECAUSE THIS MAKES THE RESULT OF THE table() FUNCTION NOT HANDLE Inf and NaN values correctly (they may not be counted for instance! --don't know WHY)
    tab = as.data.frame(table(data[,v], useNA="always"))
    values = factor(tab[,1])  #  Note: in principle the output of tab[,1] is already a factor but just in case I still use the factor() function here
    nlevels = nlevels(values)

    # Compute the Information Value
    InfValue = iv(target, data[,v], event=event)

    # Store the results of the current variable into the WOE and IV data frames
    rows = (lastrow+1):(lastrow+nlevels)
    WOE[rows,] = cbind(v, "categorical", as.character(InfValue$group), InfValue$nobs, InfValue$pctClassFIRST, InfValue$pctClassLAST, InfValue$stat, InfValue$woe, InfValue$iv)
      ## The use of as.character(InfValue$group) is important because o.w. its values are shown as numbers, because of the fact that group is a factor variable!
    totalnobs = sum(InfValue$nobs)
    totalpctClassFIRST = sum(InfValue$nobs*InfValue$pctClassFIRST) / totalnobs
    totalpctClassLAST = sum(InfValue$nobs*InfValue$pctClassLAST) / totalnobs
    totalwoe = sum(InfValue$woe)
    totaliv = sum(InfValue$iv)
    WOE[rows[nlevels]+1,] = c("--TOTAL--", "-----------", NA, totalnobs, signif(totalpctClassFIRST, digits=3), signif(totalpctClassLAST, digits=3), NA, signif(totalwoe, digits=4), signif(totaliv, digits=4))
    IV[i,] = c(v, "categorical", totalnobs, nlevels, signif(totalwoe, digits=4), signif(totaliv, digits=4))
    lastrow = lastrow + nlevels + 1
  }

  for (v in varnum) {
    i = i + 1
    cat("\t", i, ": Computing Information Value for continuous variable", v, "\n")
    
    # Compute the number of levels in the categorized continuous variable
    checkGroups = FALSE
    	## This variable specifies whether the frequency of the groups obtained using cut() below should be checked for any of the groups being too small.
    	## Its value may be updated if breaks=NULL when the number of unique cut values is 2, because this may imply that both cut
    	## values are sufficiently populated and should be considered as separate groups.
    if (is.null(breaks)) {
      # CAUTION! Do NOT update variable 'breaks' because its value is used in the next loop to check if the user passed parameter 'breaks'!!
      # NOTES:
      # - I use na.rm=TRUE because otherwise, if any NaN or NA are present in the data, an error is raised.
      # - type=3 is the SAS definition (the default in R is type=7)
      cutvalues = quantile(data[,v], probs=seq(0,1,1/groups), type=3, na.rm=TRUE)
      # Remove possible duplicate values on cutvalues (which would generate an error on the call to cut() below)
      # This happens when a cut value repeats itself many times in the analyzed variable and therefore appears more than once
      # in the output generated by quantile().
      # Note that the -Inf value is either added or not added to the cut values. This depends on whether the
      # frequency of occurrence of the lowest value (i.e. the 0% quantile) is larger than the expected group size
      # so that it deserves to have its own separate group.
      # Checking the size of occurrence of the lowest value is easy because we just check whether the 0% quantile
      # is equal to the following quantile (cutvalues[2]).
      # Note also that in the particular case when there are only 2 cut values, I set checkGroups = TRUE so that the
      # groups generated by the chosen cutvalues are check for their size below: if one of them is too small, the
      # groups are recomputed by putting them into the same group (this is simply done by eliminating the -Inf value from
      # the cutvalues array).
      if (cutvalues["0%"] == cutvalues[2]) {		# No need to check for the length of cutvalues since its lengths is at least 2: the 0% quantile and the 100% quantile
				if (length(unique(cutvalues)) == 2) { checkGroups = TRUE }
				cutvalues = unique(c(-Inf, cutvalues))
      } else {
      	# This case should be the most common case...
				cutvalues = unique(cutvalues)
			}
    }

    # Create the categorical grouping variable that is passed to the iv() function below
    # Note the use of include.lowest=TRUE so that the smallest value is NOT assigned to group NA
    group = cut(data[,v], breaks=cutvalues, include.lowest=TRUE)
    if (checkGroups) {
    	# checkGroups is set to FALSE just before the if (is.null(breaks)) block above and is set to TRUE inside the block
    	# if the following conditions are both satisfied:
    	# - the number of unique cut values (as created by the cut() function above) is exactly 2
    	# - the number of cases in the 0% quantile is the same as the number of cases in the next quantile
    	#   (which means that the lowest value of the variable is very frequent)
    	groupFreq = table(group)
    	if (min(groupFreq) / sum(groupFreq) < 0.1 * 1/groups) {
    		# When the occurrence of one of the two groups in the variable is much smaller than the expected group size (given by 1/groups)
    		# update the groups by removing the -Inf from the lower end of the cut values, so that only one group is created for the whole
    		# variable. This happens when the variable takes just two values and one of the values has a very low frequency.
		    group = cut(data[,v], breaks=cutvalues[-1], include.lowest=TRUE)
		  }
    }

    # Compute the Information Value
    # First replace NA values by NaN so that missing values in 'v' are included in the analysis as another group value!
    # (Note that both NA and NaN values in the analyzed variable 'v' are mapped to group=NA by the cut() function above
    # so any NaN values present in the original 'v' variable are finally assigned to group=NaN after doing the NA replacement here)
    # (Note also that in order to replace NA with NaN, we first need to "unfactor" the group variable (using as.character()),
    # otherwise the NA values will still be there!
    group = factor(group)					# 2014/01/06: Remove non-occurring factors (o.w. there is an error when assigning the output from the iv() function to the WOE matrix because the iv() function returns a matrix whose rows are given by the populated groups!
    group.levels = levels(group)
    group = as.character(group)
    indNA = is.na(group)
    foundNA = FALSE
    if (sum(indNA) > 0) {
      foundNA = TRUE
      group[indNA] = NaN
    }
    # Reconstruct 'group' as a factor but KEEPING THE ORIGINAL ORDER OF THE LEVELS (so that e.g. [1,3] < (3,Inf], which in terms of 'as.character(group)' is NOT the case)
    # The ordered=TRUE option is not really necessary but I keep it here because the group levels are really ordered this way
    # Note that I place the NaN value as the LAST level because this is how NaN is placed for CATEGORICAL variables (i.e. those passed in parameter 'varclass')
    group = factor(group, levels=c(group.levels, rep(NaN, foundNA)), ordered=TRUE)
    nlevels = nlevels(group)
    InfValue = iv(target, group, x=data[,v], stat=stat, event=event)

    # Store the results of the current variable into the WOE and IV data frame
    rows = (lastrow+1):(lastrow+nlevels)
    WOE[rows,] = cbind(v, "continuous", as.character(InfValue$group), InfValue$nobs, InfValue$pctClassFIRST, InfValue$pctClassLAST, InfValue$stat, InfValue$woe, InfValue$iv)
      ## The use of as.character(InfValue$group) is important because o.w. its values are shown as numbers, because of the fact that group is a factor variable!
    totalnobs = sum(InfValue$nobs)
    totalpctClassFIRST = sum(InfValue$nobs*InfValue$pctClassFIRST) / totalnobs
    totalpctClassLAST = sum(InfValue$nobs*InfValue$pctClassLAST) / totalnobs
    totalwoe = sum(InfValue$woe)
    totaliv = sum(InfValue$iv)
    WOE[rows[nlevels]+1,] = c("--TOTAL--", "-----------", NA, totalnobs, signif(totalpctClassFIRST, digits=3), signif(totalpctClassLAST, digits=3), NA, signif(totalwoe, digits=4), signif(totaliv, digits=4))
    IV[i,] = c(v, "continuous", totalnobs, nlevels, signif(totalwoe, digits=4), signif(totaliv, digits=4))
    # Update lastrow for the next variable
    lastrow = lastrow + nlevels + 1
  }

  # Convert the numeric columns in WOE and IV to 'numeric' because by default all numbers are set to characters in a data frame!
  WOE[,stat] = as.numeric(WOE[,stat])
  WOE$woe = as.numeric(WOE$woe)
  WOE$iv = as.numeric(WOE$iv)
  IV$IV = as.numeric(IV$IV)
  # Sort variables in IV by decreasing Information Value
  # Note that a new column called "row.names" is added as first column giving the original order of the rows! (nice!)
  IV = IV[order(IV$IV, decreasing=TRUE),]
  
  return(list(WOE=WOE, IV=IV))
}
######################################## InformationValue #####################################



####################################### ScoreDistribution #####################################
# 2013/07/26
# Show disribution of score and observed and predicted probabilities.
#
# HISTORY:  (2013/09/06)
#           - Changed the default values of parameters 'target' and 'score' to NULL as now I extended the functionality of this function to
#           just plot the target variable or just plot the score variable.
#           (2013/09/19)
#           - Fixed a problem of incompatible lengths of the x and y vectors when ploting the observed and predicted values (blue and red lines)
#           when some of the bins of the histogram have counts 0. (To fix this I just subset the x values to the bins where h$counts > 0)
#           (2013/10/02)
#           - Made the function robust to missing values in the score and/or target variable by using the na.omit() function on the input dataset.
#           - Added a check that the target variable takes just the values 0 and 1 then binaryFlag=TRUE.
#           - Added two parameters:
#             - colorhist: that indicates whether the histogram bar should be colored and how (e.g. from greenish to reddish or from reddish to greenish)
#             - plotFit:   that indicates whether to plot the target vs. score fit
#
# TODO:
# - 2013/07/31: Improve the case when h$equidist = FALSE (i.e. when unequal bin sizes are used for the histogram) so that the calculation
#               of the percentages to show on the vertical axis does NOT require to have a few consecutive bins with equal size but rather 
#               compute those percentages using as the most frequen bin sizes as data for the fit that estimates them.
# - 2014/01/03: Add a 'cutoff' parameter that specifies the line where to define the equal mixture of the color of the bars,
#								so that we can represent green to red bars that change at the probability given by the event rate.
ScoreDistribution = function(
    data,                                                         # Dataset containing the target and score variables to analyze
    target=NULL,                                                  # Target variable name as a string (must be the name of a variable in dataset 'data')
    score=NULL,                                                   # Score variable name as a string (must be the name of a variable in dataset 'data')
    breaks="Sturges",                                             # Breaks method or break values to use for the histogram computation
    binaryFlag=TRUE,                                              # Whether the target variable is binary (assumed --and checked-- to take the values 0 and 1 in such case)
    plotFit=TRUE,                                                 # Whether to plot the target vs. score fit.
    barsFlag=TRUE,                                                # Whether to show error bars for the observed values
    barsMultiplier=1,                                             # Multiplier to use for the error bars (i.e. how many Standard Errors to show)
    xlim=NULL,                                                    # Limits for the horizontal axis
    ylim=NULL,                                                    # Limits for the vertical axis (defaults to the [0,1] interval for a binary target variable)
    colorhist="GreenRed",                                         # How to color the histogram bars, whether from green to red ("GreenRed") or from red to green ("RedGreen") or gray (any other value).
    colors=c("blue", "red"),                                      # Colors to use for the observed and predicted average values of the target
    title="Distribution of the Score",                            # Graph title
    xlab="Score",                                                 # Label for the horizontal axis
    ylab="% Frequency",                                           # Label for the primary vertical axis (showing the frequency distribution of cases)
    ylab2="Average Probability",                                  # Label for the secondary vertical axis (showing the observed and predicted averages of the target)
    legend=c("Observed Probability", "Predicted Probability"),    # Legend text
    cex=1)                                                        # Adjustment factor for the legend font size
{
  #------------------- Parse input parameters --------------------
  ### YLIM
  # Store the value of ylim as ylim2 because it refers to the secondary axis
  ylim2 = ylim
  
  ### COLORS
  # Repeat the color if only one was passed by the user (this is to make the whole function work when only one thing is plotted, either the observed values or the predicted (score) values)
  if (length(colors) == 1) colors = c(colors, colors)

  ### TARGET
  # Set TARGET to SCORE if no variable was passed
  if (is.null(target)) {
    targetNullFlag = TRUE
    scoreNullFlag = FALSE
    binaryFlag = FALSE    # Since there is no target variable, the binaryFlag is set to FALSE
    target = score
    colind = c(2,2)   # Only use the second color in the plot (since we are plotting the score only)
    pch=c("x", "")    # Symbols to use for the legend
    if (sum(is.na(match(legend,c("Observed Probability", "Predicted Probability")))) == 0) { legend = "Predicted Probability" }
  } else if (is.null(score)) {
    targetNullFlag = FALSE
    scoreNullFlag = TRUE
    score = target
    colind = c(1,1)   # Only use the first color in the plot (since we are plotting the target only)
    pch=c("o", "")    # Symbols to use for the legend
    if (sum(is.na(match(legend,c("Observed Probability", "Predicted Probability")))) == 0) { legend = "Observed Probability" }
  } else {
    targetNullFlag = FALSE
    scoreNullFlag = FALSE
    colind = c(1,2)       # Normal plotting process
    pch=c("o","x", "")    # Symbols to use for the legend
  }

  ### Read the data and keep the variables of interest
  cols = unique(c(target, score))     # The unique() function takes into account the possibility that target was not passed and thus was set equal to score
  # Check the existence of the variables in the dataset 'data'  
  varsNotFound = checkVariables(data=data, vars=cols)
  if (!is.null(varsNotFound)) {
    stop("Execution of ScoreDistribution() stops.\n")
  }
  data = na.omit(as.data.frame(data[,cols]))
  names(data) = cols                  # Need to re-name the columns of 'data' because not doing that generates an error when the name in 'target' is the same as the name in 'score'
  target = data[,target]
  score = data[,score]
  
  # Check whether the binaryFlag value is correct, that is if the target variable really takes 2 values and 
  if (binaryFlag) {
    targetvalues = unique(target)
    if (length(targetvalues) > 2 | min(targetvalues) != 0 | max(targetvalues) != 1) {
      cat("WARNING: The target variable does NOT take just the values 0 and 1. It will be considered to be a non-binary target variable.\n")
    }
  }

  ### Compute the histogram of the score
  h = hist(score, plot=FALSE, breaks=breaks)
  print(str(h))

  ### Predicted and observed values
  score_cat = cut(score, breaks=h$breaks, include.lowest=TRUE)
  toplot_mean = aggregate(cbind(target, score), by=list(score_cat), FUN=mean)
  toplot_sd = aggregate(cbind(target, score), by=list(score_cat), FUN=sd)
  toplot_n = aggregate(cbind(target, score), by=list(score_cat), FUN=length)
  cat("toplot_mean\t toplot_n\t toplot_sd\n")
  print(cbind(toplot_mean, toplot_n, toplot_sd))
  
  # x values defining the position of the predicted and observed values
  # Note that I need to subset the x values to the bins where the histogram counts are > 0
  # because these bins are NOT included in the categorization of the target and score variables
  # done above by the cut() function!
  x = h$mids[h$counts>0]
  
  ### Plot
  mar = par("mar"); on.exit(par(mar=mar))
  par(mar=mar+c(0,0,0,1))
  if (is.null(xlim)) {
    xlim = range(h$breaks)
  }
  # (1) Histogram (note the coloring of the histogram from greener to redder or from redder to greener depending on the specification by the user)
  # Here I show percentages on the vertical axis and counts on top of histogram bars
  # (All this lengthy process is done in order to have pretty percentage numbers on the vertical axis...!)
  # A much simpler approach is used (and shown) below (in the ELSE block) when the breaks are not of equal size,
  # which plots the histogram as relative frequencies and then adds the counts on top of the bars using the text() function.
  # However, I decided to leave the more complicated approach here also in order to document the manipulation of axes!
  # Define the colors of the bins, based on the score value.
  colorHigh = (h$mids - min(h$mids)) / (max(h$mids) - min(h$mids))
  colorLow  = (max(h$mids) - h$mids) / (max(h$mids) - min(h$mids))
  # Bound the values of r between 0 and 1 using the pmin and pmax functions (which stand for parallel min and parallel max from the base package)
  # This is much easier/clearer/shorter than using the apply(..., FUN=max) approach!!
  if (tolower(colorhist) == "greenred") {
    r = pmax(0, pmin(colorHigh,1))
    g = pmax(0, pmin(colorLow,1))
    b = 0
  } else if (tolower(colorhist) == "redgreen") {
    r = pmax(0, pmin(colorLow,1))
    g = pmax(0, pmin(colorHigh,1))
    b = 0
  } else {
    r = 0.5
    g = 0.5
    b = 0.5
  }
	# Before plotting, change the clipping of the objects to plot to the FIGURE so that when adding count labels to an existing plot
	# they are fully visible even if part of them fall outside the plot region.
	opar = par(xpd=TRUE)
	on.exit(par(xpd=opar$xpd), add=TRUE)
  if (h$equidist) {
    ylim = range(h$counts + 0.01*max(h$counts)) # +0.01*max() is to leave space for the counts on top of each histogram bar
    plot(h, freq=FALSE, lty=0, xaxt="n", yaxt="n", main="", xlab="", ylab="")   # Generates first plot in the percent scale (but does NOT show the histogram)
    yaxp = par("yaxp"); usr = par("usr");                                       # Reads current vertical axis properties used below
    par(new=TRUE);                                                              # Plots the ACTUAL HISTOGRAM showing the counts as labels on top of the bars
    plot(h, labels=TRUE, freq=TRUE, xlim=xlim, ylim=ylim, col=rgb(r,g,b,0.50), main=title, xlab=xlab, ylab=ylab, yaxt="n")
    yticks = seq(yaxp[1], yaxp[2], length.out=yaxp[3]+1)                        # Construct the tick positions from the yaxp property
    binsize = diff(h$breaks)[1];                                                # Bin size (used to compute the actual percent values to show as labels)
    labels = binsize*yticks;                                                    # Computes the labels to show (which depends on the bin size in order to show actual percentages!!)
    par(usr=usr);                                                               # Goes back to the original percent scale!!
    axis(2, at=yticks, labels=paste(round(labels*100, digits=0), "%", sep=""))  # Add the axis labels
  } else {
    ylim = range(h$density + 0.01*max(h$density)) # +0.01*max() is to leave space for the counts on top of each histogram bar
    plot(h, freq=FALSE, xlim=xlim, ylim=ylim, col=rgb(r,g,b,0.50), main=title, xlab=xlab, ylab=ylab, yaxt="n")
    yaxp = par("yaxp")                                      # Property yaxp is a vector of length 3 containing (first tick value, last tick value, number of intervals)
    usr = par("usr")                                        # Property usr used to shift a little bit the counts on the top of the histogram bars
    shift = 0.03*(usr[4] - usr[3])                          # Shift that places the count labels a little bit above the histogram bars
    text(h$mids, h$density+shift, labels=h$counts)
    # Add the percentage values on the primary vertical axis as the counts are shown on top of each histogram bar
    yticks = seq(yaxp[1], yaxp[2], length.out=yaxp[3]+1)    # This constructs the tick values from the yaxp property
    # Since the bin size is not uniform in this case, it is NOT so easy to automatically compute the percentages associated to the density value
    # (which is the value shown on the vertical axis)
    # The problem is solved assuming that some consecutive bins have equal size and a linear regression is fitted on those bins
    # relating frequency with density. If this does not work, the original labels defined by the plot() function are used (which are those stored in vector x.yticks)
    # In the following I use the "x." prefix to designate any variable used as x variable in the regression and the "y." prefix to designate variables designating variables to be predicted.
    x.yticks = yticks
    x.dens = h$density
    y.freq = h$density*diff(h$breaks)   # % Frequency values based on the density and the bin size!
    delta = diff(h$breaks)              # Find out consecutive bins having equal size (=> need to differentiate the bin sizes computed as diff(h$breaks)
    delta2 = diff(delta)
    x.dens2fit = h$density[delta2==0]
    y.freq2fit = y.freq[delta2==0]
    if (length(x.dens2fit)>1) {
      model = lm(y.freq2fit ~ x.dens2fit - 1)
      y.freq.pred = model$coefficients["x.dens2fit"]*x.yticks
      labels = paste(round(y.freq.pred*100, digits=0), "%", sep="")
    } else {
      labels = yticks
    }
    axis(2, at=yticks, labels=labels)   # Show the percentages as labels
  }
 
  if (plotFit) {
    # (2) Error bars (when requested) --this computation is here in order to adjust the vertical axis to make the error bars fit in the plot
    # Use the parameter passed by the user as the secondary vertical axis limits
    # (This information is stored in ylim2 where I copied it from parameter 'ylim' at the beginning in order to avoid confusion with the 'ylim' values used above for the primary vertical axis)
    ylim = ylim2
    if (barsFlag) {
      # Observed values and group sizes
      y = toplot_mean$target
      n = toplot_n$target
      sigma = toplot_sd$target
      # Replace NAs with 0 in the standard deviation sd (which occur when only 1 value exists in the corresponding group)
      sigma[is.na(sigma)] = 0
      # Standard error of the observed values
      if (binaryFlag) {
        se = sqrt(y*(1-y)/n)
      } else {
        se = sigma/sqrt(n)
      }
      dy = barsMultiplier*se
      # Set the vertical axis limits
      if (is.null(ylim2))
        if (binaryFlag) {
          # Limit the lower and upper limits to the interval [0,1]
          ylim = range(c(toplot_mean$target, toplot_mean$score, apply(cbind(0,y-dy), 1, max), apply(cbind(y+dy,1), 1, min)))
        } else {
          ylim = range(c(toplot_mean$target, toplot_mean$score, y-dy, y+dy))
        }
    } else {
      # Secondary vertical axis limits when no bars are shown
      if (is.null(ylim2))
        ylim = range(c(toplot_mean$target, toplot_mean$score))
    }
  
    # (3) Average observed values
    par(new=TRUE)
    ylim = ylim     # The value of ylim was set up above
    if (!targetNullFlag) {
      plot(x, toplot_mean$target, type="p", pch=21, col=colors[colind[1]], bg=colors[colind[1]], xlim=xlim, ylim=ylim, yaxt="n", xlab="", ylab="")
    }
  
    # (4) Average predicted values
    if (!scoreNullFlag) {
      par(new=TRUE)
      plot(x, toplot_mean$score, type="b", pch="x", col=colors[colind[2]], xlim=xlim, ylim=ylim, yaxt="n", xlab="", ylab="")
    }
  
    # (5) Error bars: PLOT THEM
    if (barsFlag) {
      for (i in 1:length(x)) {
        if (binaryFlag) {
          # Limit the upper and lower bounds of the error bars to the interval [0,1]
          lines(c(x[i],x[i]), c(max(0,y[i]-dy[i]),min(y[i]+dy[i],1)), col=colors[colind[1]])
        } else {
          lines(c(x[i],x[i]), c(y[i]-dy[i],y[i]+dy[i]), col=colors[colind[1]])
        }
      }
      # Add a floor and a roof to the error bars
      points(x, y+dy, pch="_", cex=2, col=colors[colind[1]])	# Note that cex defines the point size!
      points(x, y-dy, pch="_", cex=2, col=colors[colind[1]])	# Note that cex defines the point size!
    }
    
    # (6) Tick values for the secondary axis and legend
    yticks = pretty(ylim)
    axis(4, at=yticks)
    mtext(yticks, side=4, at=yticks, outer=TRUE)
    mtext(ylab2, side=4, line=2, las=3)
    # Add legend
    legend("top", legend=c(legend, paste("Total number of cases:", sum(toplot_n$target))), cex=cex, y.intersp=1, pch=pch, pt.bg=colors[colind], col=colors[colind], bg=rgb(0.7,0.7,0.7,1/10), horiz=FALSE)
  } else {
    # Add legend
    legend("top", legend=paste("Total de casos:", sum(toplot_n$target)), cex=cex, bg=rgb(0.7,0.7,0.7,1/10), horiz=FALSE)
  }
}
####################################### ScoreDistribution #####################################
