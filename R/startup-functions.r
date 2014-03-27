# startup-functions.r
# Created:      July 2008
# Modified:     15-Sep-2013
# Author:       Daniel Mastropietro
# Description: 	Startup settings to be invoked when R starts
# R version:    R-2.8.0 (used in SPSS 18.0.0 @NAT starting 2013)
#


# INDEX
# - GENERAL functions
# - DATA ANALYSIS functions
# - GRAPHICAL functions

# HISTORY:
# - 2013/09/03: Took care of missing values in functions:
#               plot.binned(), pairs.custom()
# - 2013/09/19: Created function safeLog(x) to compute sign(x)*log10(constant + abs(x))
# - 2013/12/23: Created function getAxisLimits() to retrieve the correct X and Y limits of the active plot, since
# 							the values returned by par("usr") are NOT correct for the plots done by default, which use xaxs="r"
#								and yaxs="r" (regular axis) which imply that the actual "usr" limits are extended from the xlim and
#								ylim extremes by 4% of the limits range!! (Ref: documentation of par() and the 'xaxs' and 'yaxs'
#								options therein).
# - 2014/03/12: Created functions:
#								- safeLogInv(): computes the inverse of the safeLog() function
#								- logitInv(): computes the inverse of the logit() function available in the car package
#								- plot.cdf(): plots the CDF of a variable
# - 2014/03/26: Created function:
#								- plot.bar(), plot.bars(): makes a bar plot of a target variable in terms of categorical variable
#									where the bar widths are proportional to the number of cases in each category.
#

# TODO:
# - [DONE-2013/08/05: Function checkVariables()] 2013/08/02: Write a function to check the existence of variables in a dataset so that the user knows which variables are NOT found when passed as parameters
#

# Necessary Libraries
require(gdata)		# For rename.vars() etc. (used at least in plot.binned) (note: gdata also has a trim() function to remove leading and trailing blanks in strings)
require(graphics)	# For symbols(), filled.contour(), etc. (used at least in plot.binned, biplot.custom)
require(grDevices)# For colorRampPalette() (used in biplot.custom)
require(MASS)     # For kde2d() etc. (used at least in pairs.custom)


######################################### GENERAL functions ###################################
# INDEX (sorted alphabetically)
# getAxisLimits
# checkVariables
# who
# whos

# Get the axis limits of the current plot
getAxisLimits = function(ext=0.04)
# Created: 			2013/12/23
# Modified: 		2013/12/23
# Author: 			Daniel Mastropietro
# Description: 	Returns the axis limits of the active plot by taking care of the fact that by default the user coordinates
#							 	do NOT represent the xlim and ylim values of a plot!!! Since by default the value of graphical parameters
#							 	xaxs and yaxs is "r" (regular) which means that the user coordinates are extended below and above the xlim
#							 	and ylim values by 4% of the range.
#						   	For example, if xlim=c(1,10) then the user coordinates for the X axis is set to:
#							 	[1 - 4%*(10-1), 1 + 4%*(10-1)] = [0.64, 1.36]
#							 	This does not happen when xaxs and yaxs are equal to "i" (internal) the other possible value for these parameters.
# Parameters:		ext: proportion of the axis limits range by which the user coordinates are extended.
#								This is currently (version 3.0.1) equal to 4% as indicated by the 'xaxs' and 'yaxs' options of the par() documentation.
# Examples:		 	axisLimits = getAxisLimits()
{
	# Read useful graphical parameters of the active plot
	usr = par("usr")
	xaxs = par("xaxs")
	yaxs = par("yaxs")

	xlim = usr[1:2]
	ylim = usr[3:4]
	if (xaxs == "r") {
		# Range in the extended coordinates
		xdelta = diff(xlim)
		# Correct xlim values
		# (the lower end gets increased and the upper end gets decreased by a factor of ext/(1+2*ext) --e.g. 0.04/1.08 when ext = 0.04)
		xlim = xlim + c(+1,-1) * xdelta * ext/(1 + 2*ext)
	}
	if (yaxs == "r") {
		# Range in the extended coordinates
		ydelta = diff(ylim)
		# Correct ylim values
		# (the lower end gets increased and the upper end gets decreased by a factor of ext/(1+2*ext) --e.g. 0.04/1.08 when ext = 0.04)
		ylim = ylim + c(+1,-1) * ydelta * ext/(1 + 2*ext)
	}

	return(c(xlim, ylim))
}

CheckVariables <- checkVariables <- function(data, vars, print=FALSE)
# Created: 			2013/08/05
# Modified: 		2013/08/05
# Author: 			Daniel Mastropietro
# Description: 	Checks whether a set of variables exist in a data frame
# Parameters:
#             	data: data frame where the existence of the variables is checked
#             	vars: vector of variable names given as strings
# Output: 			A vector with the names of the variables not found in the dataset is returned.
# Examples:			varsNotFound = checkVariables(df, c("x1","z","tt"), print=TRUE)
{
  if (!is.data.frame(data)) {
    stop("CHECKVARIABLES - ERROR: The dataset '", deparse(substitute(data)), "' is not a data frame")
  }
  
  # Start
  varsNotFound = NULL
  varsInData = colnames(data)
  for (v in vars) {
    if (!v %in% varsInData) {
      varsNotFound = c(varsNotFound, v)
    }
  }
  
  if (!is.null(varsNotFound)) {
    cat("CHECKVARIABLES: The following variables do not exist in data frame '", deparse(substitute(data)), "':\n", sep="")
    for (v in varsNotFound)
      cat(v, "\n")
  }

  if (!is.null(varsNotFound)) {
    return(varsNotFound)
  } else {
  	# Show a message either when print=TRUE or when this function was called from the parent most environment (i.e. from the GlobalEnv environment)
#    if (print | grepl("GlobalEnv", environmentName(sys.frame(sys.parent())))) # Note the use of grepl() instead of grep() so that a boolean value is returned; grep() instead returns numeric(0) when the string is not found.
    if (print | length(grep("GlobalEnv", environmentName(sys.frame(sys.parent())))) > 0) # HOWEVER, grepl() does NOT exist in R-2.8.0!! (used in SPSS 18.0.1) So in SPSS I use length(grep()) > 0 as an equivalent condition to grepl() == TRUE.
      cat("All variables found in dataset '", deparse(substitute(data)), "'\n")
  }
}

who = function(envir=.GlobalEnv)
# Created: 			2008
# Descriptoin: 	Function that implements the Matlab-like function 'who' listing the objects defined in memory
{
	print(as.matrix(ls(envir=envir)))
}

whos = function(envir=.GlobalEnv, sortby=c("name","size"), decreasing=FALSE)
# Created: 			2008
# Modified: 		09-Sep-2008
# Author: 			Daniel Mastropietro
# Description: 	Function that implements the Matlab-like function 'whos' showing the objects defined in memory
#								along with their sizes, optionally sorting by size in ascending or descending order.
{
  if (length(sortby) == 2) { sortby = "name" }
	
	size = sapply(ls(envir=envir) , function(x) object.size(get(x)))
	if (tolower(sortby) == "size") 
	{
		size = sort(size, decreasing=decreasing)
	}
	else if (decreasing)
	{
		size = size[order(names(size), decreasing=TRUE)]
	}
	
	print(as.data.frame(size))
}
######################################### GENERAL functions ###################################



###################################### DATA ANALYSIS functions ################################
# INDEX (sorted alphabetically)
# quantile.weight
# logitInv
# safeLog
# safeLogInv
#

# 2013/08/18
# Group of numeric variable using equal-size groups similar to the quantile() function but using weights for each observation.
# Motivation: create quantile groups based on the estimated density of a numeric variable so that the histogram bins are smaller where the density is larger
# For an example see: dev\Density&Histogram.r
quantile.weight = function(x, weight, probs=seq(0,1,0.25), type=4, precision=4)
  # Parameters:
  # x:          numeric vector for which weighted quantiles are computed
  # weight:     weights to use for each value in x (any non-negative real number is allowed)
  # probs:      vector of probabilities defining the quantiles (same meaning as 'probs' in quantile() function in R)
  # precision:  number of significant digits to keep in the quantile values
  #
  # Output:     A vector of quantiles computed using the weights given in parameter weight.
  #             The method used for the quantile computation is the method implemented as type=4 in the quantile() function in R,
  #             i.e. a linear interpolation is performed on the empirical weighted cdf.
  # Data example to try this out:
# x = c(0.532911970399581 , 0.343041113563211 , 0.386032839804403 , 0.700412550080721 , 0.532742005037833 , 0.58089531748196 , -0.217689730141691 , 0.22090301819094 , 0.623072695280153 , 0.566896956080503 , 0.750391260907804 , 0.516034289364722 , 0.302135940489829 , 0.489357487843298 , 0.778118464095309 , 0.28906342442205 , 0.0515489742182842 , 0.603788130986552 , 0.403093379536764 , 0.650808860183577)
# weight = c(1 , 0 , 1 , 0 , 1 , 1 , 1 , 1 , 1 , 1 , 2 , 3 , 1 , 4 , 3 , 5 , 1 , 8 , 3 , 2)
{
  if (length(weight) != length(x))
    stop("The weight variable ", deparse(substitute(weight))," and the analysis variable ", deparse(substitute(x))," have different lengths:", length(weight), "and", length(x), "respectively.")
  if (type != 4)
    stop("The type parameter value ", type, " is not valid. The only accepted value is 4, which corresponds to the type=4 method described in R function quantile().")
  
  # Note that order() is different from rank()!! order() gives the order of the indices of x so that x is in sorted.
  # rank() gives the rank of each x in the order that x is arranged!
  # This implies the x[order(x)] returns x in ascending order, but x[rank(x)] is something completely different!
  xorder = order(x)
  xsort = x[xorder]
  wsort = weight[xorder]
  wtotal = sum(weight)
  
  # Compute the CDF of x weighted by variable weight (i.e. each x is represented as many times as the corresponding value given in weight)
  X = as.data.frame(cbind(x=xsort, w=wsort, p=cumsum(wsort/wtotal)))
  # p is cdf with weights, q is cdf w.o. weights!
  X$q = as.numeric(rownames(X))/nrow(X)
  # Look for the cut points of the weight-based quantile groups
  xsubset = X$x
  psubset = X$p
  indX = 0
  X$group = NA
  xquantilePrev = xsubset[1]
  xquantiles = xquantilePrev
  for (p in probs[probs>0]) {
    ind = (psubset<=p)
    last = sum(ind)
    xgroup = xsubset[ind]
    xmin = xgroup[1]
    xmax = xgroup[last]
    if (psubset[last] < p & p < 1) {
      # Interpolate to get the quantile
      xquantile = xsubset[last] + (p - psubset[last]) / (psubset[last+1] - psubset[last]) * (xsubset[last+1] - xsubset[last])
    } else {
      # Assign the quantile to be equal to xmax when the value of X$p coincides exactly with the current probability being processed in probs
      xquantile = xmax
    }
    X$group[indX+1:last] = rep(paste("(", signif(xquantilePrev, digits=precision), ",", signif(xquantile, digits=precision), "]", sep=""), last)
    # Update X index, subset vectors and quantiles
    indX = indX + last
    xquantilePrev = xquantile
    xquantiles = c(xquantiles, xquantile)
    xsubset = xsubset[-(1:last)]
    psubset = psubset[-(1:last)]
  }
  
  output = xquantiles
  names(output) = paste(probs*100,"%", sep="")
  
  return(output)
}

# Compute the safe-log in base 10, which accepts ALL real values as input
safeLog = function(x, constant=1)
{
  return(sign(x)*log10(constant + abs(x)))
}

# Compute the inverse of the safe-log function in base 10 implemented in function safeLog()
safeLogInv = function(x, constant=1)
{
  return(sign(x)*(10^abs(x) - constant))
}

# Compute the inverse of the logit() function defined in the car package
logitInv = function(x, adjust=0)
# The logit() function in the car package allows an adjustment of the 0 and 1 probabilities to
# avoid -Inf and +Inf, by using an adjustment factor 'adj' and computes the logit as:
# log(padj/(1-padj))
# where padj = p*(1-2*adj) + adj, linear mapping from [0,1] to [adj,1-adj]
# Note: This was verified by intuition and comparing the output from logit() with my own
# computation as described above.
{
	y = 1/(1+exp(-x))
	yadj = (y - adjust)/(1-2*adjust)
	return(yadj)
}
###################################### DATA ANALYSIS functions ################################



######################################### GRAPHICAL functions #################################
# INDEX (sorted alphabetically)
# hist.log
# biplot.custom
# panel.cor
# panel.dist
# panel.hist
# pairs.custom
# pairsXAxisPosition
# pairsYAxisPosition
# plot.axis
# plot.bar		(NEW Mar-2014)
# plot.binned
# plot.cdf		(NEW Mar-2014)
# plot.dist (in construction, which calls panel.dist --when finalized should I remove the 'add' parameter form panel.dist() --because this is a panel function--, or should I leave it anyway (with default value always add=TRUE)? perhaps leave it...)
# plot.hist
# plot.image
# plot.log
# 

# 2013/09/21
# NEW FUNCTION TO CREATE THAT SHOWS A SCATTER PLOT AND HISTOGRAMS, BOXPLOTS AND DENSITY ON EACH SIDE!!
# This function calls panel.dist()!! (also defined in this file)
# NOT TESTED YET!!
plot.dist = function(dat, plot.fun=plot, respect=FALSE, histogram=FALSE, xlim=NULL, ylim=NULL, ...)
  ## respect is the aspect ratio respecting of the layout() function
{
  ### Parse the function call
  call = match.call()
  # Put all calling parameters into a parameter list
  paramsList = as.list(call)
  # Remove the function name from the parameter list
  paramsList[[1]] = NULL
  # Remove also the data frame because this name is not recognized by plot() or similar functions, whose first argument is named 'x'
  paramsList$dat = NULL
    
  ### Define the graphical layout, as follows:
  # Plot #1: will be done on the bottom-left tile (scatter plot)
  # Plot #2: will be done on the top-left tile (distribution of variable 1)
  # Plot #3: will be done on the bottom-right tile (distribution of variable 2)
  # No plot on the upper-right tile
  # This position is defined by the numbers 0, 1, 2, 3, where 0 means NO plot.
  # The widths and heights of each subplot are different, accordingly to what is plotted
  mar = par(mar=c(3,3,0,0))
  layout = layout(matrix(c(2,0,1,3), ncol=2, byrow=TRUE), widths=c(4/5,1/5), heights=c(1/5,4/5), respect=respect)
  x1 = dat[,1]
  x2 = dat[,2]
  # Add this variables to the parameter list to pass to the plot.fun function
  paramsList$x = x1
  paramsList$y = x2
  print(paramsList)
  ### Main plot
  # Still cannot make the do.call() work. The problem I have is that instead of points, the plot is filled with the data values!! (strange)
#  do.call(deparse(substitute(plot.fun)), paramsList)
  plot(x1, x2, xlim=xlim, ylim=ylim, ...)
  
  ### Distribution of x1
  par(mar=c(0,3,0,0), pty="m")
    ## Make the horizontal axis of the distribution stick to the top axis of the scatter plot
    ## Also make the plot region maximal so that the whole subplot is occupied by the distribution
  panel.dist(x1, histogram=histogram, horizontal=TRUE, add=FALSE, main="", axes=FALSE, xlim=xlim, ylim=ylim)

  ### Distribution of x2
  par(mar=c(3,0,0,0), pty="m") 
    ## Make the horizontal axis of the distribution stick to the right axis of the scatter plot
    ## Also make the plot region maximal so that the whole subplot is occupied by the distribution
  panel.dist(x2, histogram=histogram, horizontal=FALSE, add=FALSE, main="", axes=FALSE, xlim=xlim, ylim=ylim)

  ### Restore mar option
  par(mar=mar)
}

# Define the position of the x-axis in a pairs plot
pairsXAxisPosition = function(xaxt="s")
# Created: 2013/09/16
# Modified: 2013/09/16
# Description: Defines the position in a pairs panel to place the x-axis ticks using the same logic as pairs()
{
  mfg = par("mfg")  # 4-element array with the following info: current panel position, panel structure (e.g. (2,3,4,4))

  # X-axis
  if (xaxt == "n") {
    # Place the axis ticks next to the axis when no axis ticks have been required at the pairs() call
    outer = FALSE
    line = 0
    side = 1
  } else {
    # Place the axis ticks on the outer panel side o.w.
    # The side is chosen based on the mfg option value, in order to mimic the same logic as the pairs() function
    outer = TRUE
    line = 0  # This number should be -gap, where gap is the space between the subplots in margin lines. However, I don't know yet how to retrieve the value of gap in the current pairs() call!
    if (mfg[2] %% 2 == 1) {
      side = 1
    } else {
      side = 3
    }
  }

  return(list(side=side, outer=outer, line=line))
}  
  
# Define the position of the y-axis in a pairs plot
pairsYAxisPosition = function(yaxt="s")
# Created: 2013/09/16
# Modified: 2013/09/16
# Description: Defines the position in a pairs panel to place the y-axis ticks using the same logic as pairs()
{
  mfg = par("mfg")  # 4-element array with the following info: current panel position, panel structure (e.g. (2,3,4,4))
  
  # Y-axis
  if (yaxt == "n") {
    # Place the axis ticks next to the axis when no axis ticks have been required at the pairs() call
    outer = FALSE
    line = 0
    side = 2
  } else {
    # Place the axis ticks on the outer panel side o.w.
    # The side is chosen based on the mfg option value, in order to mimic the same logic as the pairs() function
    outer = TRUE
    line = 0  # This number should be -gap, where gap is the space between the subplots in margin lines. However, I don't know yet how to retrieve the value of gap in the current pairs() call!
    if (mfg[1] %% 2 == 1) {
      side = 4
    } else {
      side = 2
    }
  }
  
  return(list(side=side, outer=outer, line=line))
}

# Correlations as an off-diagonal panel function in pairs(). The font size is proportional to the correlation.
# TODO:
# - 2013/12/13: Make the background color of the whole panel vary according to the correlation level, e.g. blue for negative
# correlation, white for no correlation, red for positive correlation.
panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", cex.cor, ...)
# Created: 2008/06/20
# Modified: 2013/06/25
# Function to use as a panel function in pairs() that writes the correlations whose font size is ~ correlations.
# Typically used as the lower.panel function in pairs().
# (taken from help(pairs))
# Note the inclusion of the '...' parameter in order to avoid any errors when additional parameters
# are passed to other functions used in the pairs() call to be used for the different panels
# (e.g. plot.binned() below which accepts many parameters)
#
# HISTORY: (2013/06/25) added parameter 'use' passed in the cor() function call to define how missing values should be treated for the computation of the correlation.
#
{
  opar <- par(usr=c(0, 1, 0, 1)); on.exit(par(opar))
  r <- abs(cor(x, y, use=use))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 1/strwidth(txt)
  text(0.5, 0.5, txt, cex=cex*r)
}

# 1D density estimation and boxplot as a diagonal panel of a pairs() plot.
# (Ref: Uwe Ligges, https://stat.ethz.ch/pipermail/r-help/2011-July/282611.html)
# TODO:
# - 2013/09/21: Replace the use of the barplot() function with the use of the rect() function to draw the histogram bars
# horizontally (when horizontal=FALSE --because 'horizontal' refers to the boxplot horizontality) and
# thus avoid the problem with the barplot() function which does NOT use the x scale but a simple 1 ... #bins scale, and
# this makes the overlaying of the histogram and the density not always coincide!
# (see ane example of the rect() function in panel.hist())
panel.dist = function(x, add=TRUE, histogram=FALSE, horizontal=TRUE, xlim=NULL, ylim=NULL, ...)
# Created: 2013/07/08
# Modified: 2013/09/21
# Description: Shows a density estimation and a boxplot overlayed
# Parameters: x: numeric variable of interest
#
# HISTORY:  (2013/09/21) Added parameters 'add', 'hist', 'horizontal' to control whether to:
#                        - create a new plot (add=FALSE)
#                        - plot also a histogram (histogram=TRUE)
#                        - show the density and boxplot in vertical position (horizontal=FALSE)
{ 
  # Density estimation
  dens = density(x, na.rm=TRUE)
  
  # Histogram
  if (histogram) {
    hist = hist(x, plot=FALSE)
  }
  
  # Create a new plot if requested
  if (!add) {
    if (horizontal) { # boxplot is horizontal => density is horizontal
      if (missing(xlim)) xlim = range(dens$x)
      ylim = range(dens$y)
      if (histogram) ylim = range(c(ylim), hist$density*1.5)
    } else {  # boxplot is vertical => density is rotated 180 degrees (to show the density on a vertical axis on the side of a scatter plot)
      xlim = range(dens$y)
      if (missing(ylim)) ylim = range(dens$x)
      if (histogram) xlim = range(c(xlim), hist$density*1.5)
    }
    plot(dens, type="n", xlab="", ylab="", xlim=xlim, ylim=ylim, ...)
  }
  
  # Start plotting
  usr <- par("usr")
  if (histogram) {
    ### Plot the histogram using barplot()
    ### Note that I need to use barplot() and NOT plot(hist, ...) because I want to plot the histogram
    ### horizontally (horizontal=FALSE, because this parameter refers to the boxplot horizontality!) and
    ### plot.hist() function does NOT have an option to show the plot rotated from the normal display)
    ### However, I could consider using plot(hist$density, hist$mid, type="s") to show the histogram rotated but this
    ### still needs some development because the way it is shown is not in the normal histogram way... 
    ### A possible evolution of this is using the rect() function to create bars, as is done by panel.hist() in this code! (by Uwe Ligges)
    # IMPORTANT: Define the scale for the barplot (so that the x values coincide with the x values of the plot!)
    # This is necessary because the barplot positions the bars ALWAYS on the values 0.5, 1.5, ..., length(hist$mid)-0.5,
    # regardless of the values of x!!!
    # Note also that the computation of the scale is different depending on whether the histogrma is shown horizontally or vertically
    # (This was done by trial and error and it is not assured that it works in all situations!!)
    scale = hist$mid[length(hist$mid)] / (length(hist$mid) + (as.numeric(horizontal)-0.5)*2)
    if (horizontal) {
      par(usr=c(usr[1:2]/scale, usr[3:4]))
    } else {
      par(usr=c(usr[1:2], usr[3:4]/scale))
    }
    barplot(hist$density, axes=FALSE, space=0, horiz=!horizontal, add=TRUE)
    # Restore the user coordinates
    par(usr=usr)
  }
  if (horizontal) {
    par("usr"=c(usr[1:2],0,max(dens$y)*1.5))
    lines(dens)
    par("usr"=c(usr[1:2],0,1))
    boxplot(x, at=0.5, boxwex=0.3, horizontal=TRUE, add=TRUE, ...)
  } else {
    par("usr"=c(0,max(dens$y)*1.5,usr[3:4]))
    lines(dens$y, dens$x)
    par("usr"=c(0,1,usr[3:4]))
    boxplot(x, at=0.5, boxwex=0.3, horizontal=FALSE, add=TRUE, ...)
  }
}

# Histogram as a diagonal panel function in pairs()
panel.hist <- function(x, ...)
  # Created: 2008/06/20
  # Modified: 2008/06/20
  # Function to use as a panel function in pairs() that plots the histogram of the variables.
  # Typically used as the diag.panel function in pairs().
  # (taken from help(pairs))
  # Note the inclusion of the '...' parameter in order to avoid any errors when additional parameters
  # are passed to other functions used in the pairs() call to be used for the different panels
  # (e.g. plot.binned() below which accepts many parameters) 
{
  usr <- par("usr"); on.exit(par(usr=usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="light cyan")
}

# Smoohting spline fit by a grouping variable
panel.smooth <- function(x, y, group, ...)
# Created: 2009/02/02
# Modified: 2009/02/02
{


}

# Image showing the 2D density estimation
# (Ref: Uwe Ligges, https://stat.ethz.ch/pipermail/r-help/2011-July/282611.html)
plot.image = function(x, y, col="red", nlevels=12, xaxt="s", yaxt="s", addXAxis=FALSE, addYAxis=FALSE, add=TRUE, ...)
# Created: 2013/07/08
# Modified: 2013/11/14
# Description: Shows an image that represents the levels of the 2D density estimate of a pair of numeric variables using kde2d.
# Parameters: 
#             x, y:     numeric variables whose 2D density we want to plot
#             nlevels:  number of colors used in the image representing the 2D density estimation 
#                       nlevels=12 is the value used by default by the image() function to define the number of colors used.
# Note: the xaxt and yaxt parameters are needed in order to receive the value passed by the user when calling pairs()
# For some reason the xaxt and yaxt options are not passed to this function when not explicitly listed here! (I guess this happens with all other graphical parameters passed in ...???)
#
# HISTORY:  (2013/09/15) Converted the function to a 'plot' function rather than 'panel' function in the sense that
#           it can be called to create a NEW plot. For this I simply added parameter 'add' (which defaults to TRUE anyway, so that  it still works with pairs())
#           and added the '...' parameter list on the call to image(), so that the user can pass customized graphical parameters.
#           (2013/09/23) Added parameter 'col' to define the color of the image intensity.
#						(2013/11/14) Fixed an error happening when the 2D density estimation produced by kde2d yields ALL NA values for xy$z.
#						This may happen when the x and y data have e.g. too many repeated values (it seems this is the reason of all NA values!)
# 					The fix was done by jittering the data a bit (i.e. adding noise with the jitter() function)
{
  # Color definition for the different levels of the image plot
  whitered.palette = colorRampPalette(c("white", col))  # This is a function so it can be used as the 'col' argument of image()

  # Remove any missing values
  indok = !is.na(x) & !is.na(y)
  xy <- kde2d(x[indok],y[indok])
  if (all(is.na(xy$z))) {
  	# If the above density estimation fails (i.e. all z values are NA because of flawed data --e.g. too many repeated values)
  	# create a new density estimation based on jittered x and y values (i.e. with added noise)
  	# Otherwise calling image() on all NA data gives an error. Note that it suffices for ONE value to be non-NA for the image() function to work.
  	xy <- kde2d(jitter(x[indok]),jitter(y[indok]))
  }
  
  # Show the image plus contour lines
  image(xy, add=add, col=whitered.palette(nlevels), ...)
  contour(xy, add=TRUE)
  
  # Add axis ticks if requested
  if (addXAxis) {
    axisProperty = pairsXAxisPosition(xaxt=xaxt)
    axis(at=pretty(x[indok]), side=axisProperty$side, outer=axisProperty$outer, line=axisProperty$line)
  }
  if (addYAxis) {
    axisProperty = pairsYAxisPosition(yaxt=yaxt)
    axis(at=pretty(y[indok]), side=axisProperty$side, outer=axisProperty$outer, line=axisProperty$line)
  }
}


# Binned plot
# TODO:
# - 2008/09/09: Add a parameter that allows to show the labels of the original scale when the plotted variable
# is log-transformed.
# - 2013/09/17: Add parameter 'lmbands' which can take values NULL, "confidence", "prediction" which are the possible
# values of parameter 'interval' of the predict.lm() function in order to add the confidence or prediction bands to the lm fit.
# The code to compute those bands would be the following:
# lmfit.pred = predict(lmfit, interval=lmbands)
# lines(toplot$x_center, lmfit.pred[,"upr"], col="blue", lty=0.8)
# lines(toplot$x_center, lmfit.pred[,"lwr"], col="blue", lty=0.8)
# - 2013/09/21: Solve the minor issue of correctly placing the secondary axis for 'target' when plot.binned() is called from
# pairs(): We should set parameter 'line' in the axis() function --used to indicate the position of the secondary axis-- equal to -gap,
# where 'gap' is the space (in margin lines) between the pairs subplot. However, I don't know how to read the value of such parameter.
# One option would be to use the sys.calls() function (already used at the beginning of plot.binned()), identify the pairs() call and
# check whether the 'gap' parameter is listed in that call. If not, assume that the default 'gap' value is in use (gap=1).
# - 2013/11/14: If requested, have the function call InformationValue() to compute the information value of the analyzed variable
# on the target and in that case display it as the title of the plot and also show a table of the WOE by bin on the right-hand side
# of the plot (but only when add=FALSE, which is the case for instance when the plot is NOT part of a pairs plot)
plot.binned = function(
	x, y, pred=NULL, target=NULL, grouped=FALSE,
	center=mean, scale=sd, groups=20, breaks=NULL, rank=TRUE,
  lm=TRUE, loess=TRUE,
  circles=NULL, thermometers=NULL,  # 'circles' and 'thermometers' must be EXPRESSIONS that parameterize the corresponding symbol based on the computed grouped values (e.g. circles=expression(x_n) or thermometers=expression(cbind(x_n,y_n,target_center)), since these values would not exist during the function call)
  col="blue", col.pred="black", col.target="red", col.lm="blue", col.loess="green",
	pointlabels=TRUE, bands=FALSE, width=2, stdCenter=TRUE, # stdCenter: whether to show the bands using the standard deviation of the *summarized* y value (calculated as scale/sqrt(n), where n is the number of cases in the x category)
  boxplots=FALSE, add=FALSE, offset=1, inches=0.5, size.min=0.05, cex.label=1,
	xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, ylim2=NULL,  # ylim for the secondary axis used to plot the target values
	xlimProperty="orig",  # xlimProperty can be either "new" or "orig"; in the latter  case the range of the ORIGINAL x values are used as limits for the x-axis => within a pairs() call the same x-axis scale is expected to be shown for ALL pairs plot (unless there are different missing values depending on the analyzed variables)
	ylimProperty="orig",  # ylimProperty can be either "new" or "orig"; in the latter case the range of the ORIGINAL y values are used as limits for the y-axis => within a pairs() call the same y-axis scale is expected to be shown for ALL pairs plot (unless there are different missing values depending on the analyzed variables)
  ylim2Property="orig", # ylim2Property can be either "new" or "orig"; in the latter case the range of the ORIGINAL target values are used as limits for the secondary y-axis => within a pairs() call the same secondary y-axis scale is expected to be used for ALL pairs plot (unless there are different missing values depending on the analyzed variables)
  type2="b", pch2=21,	  # type and pch par() options defining the form and symbol of the target variable
  xaxt="s", yaxt="s",
	addXAxis=(xlimProperty!="orig"), addYAxis=(ylimProperty!="orig"), # addXAxis, addYAxis: Whether to manually add the x/y-axis ticks next to EACH axis, because the axis on the different pair combinations may be potentially different (this was invented because the value of par("xaxt") is NULL when checking its value from within a function called as a pairs() panel function; this means that I cannot decide whether to manually add the axis checking whether the axis ticks have already been placed by the pairs() function)
  addY2Axis=(ylim2Property!="orig"), # addY2Axis: whether to manually add the secondary y-axis ticks next to EACH axis, because the axis on the different pair combinations may be potentially different (this is set to TRUE when the secondary axis limits are NOT requested to be in the original coordinates --in which case the secondary y-axis have the same range for all pairs plots)
	title=NULL,
	clip="figure",				# clipping area, either "figure" or "region". This parameter affects the xpd option of par(). Note that "region" is the default in par() but here I set "figure" to the default so that big bubbles are shown full --instead of being partially hidden by the axes.
	plot=TRUE, print=TRUE, ...)
# Created: 			09-Sep-2008
# Modified: 		11-Mar-2014
# Description:	Plots on binned variables, optionally with a third variable that for example represents a fit from a model.
# HISTORY:  (2013/06/25) Added parameter cex to change minimum label size for the points (added using text()) and to define overall label size (such as for labels added by mtext())
#           (2013/07/08) Added parameter pairsFlag so that this function can be used for the plots in the panels of a pairs() call (which does NOT allow the generation of a new plot in the panels)
#           (2013/08/14) Added parameter groups which plays the role of the previously names 'breaks', and I use now 'breaks' in case the user wants to define their own break points (instead of specifying 'groups')
#           (2013/09/03) Took care of missing values in the data (i.e. removed cases that have missing values in ANY of the analyzed variables x, y and pred.
#           (2013/09/15) Added parameter 'target' to define a target variable to plot on secondary axis
#                        Renamed parameter 'new' to 'add' to avoid confusion of the use of the 'new' parameter of par(), since until now I was setting par(new=!new), which is rather confusing...
#                        Renamed parameter 'cex' to 'cex.label' to reflect that this cex affects only labels and NOT point sizes.
#                        Renamed parameter 'value' with 'center' to be in accordance with the R convention given by function scale()
#                        Added parameter 'scale' that defines the function to use for the scale statistic. This can be any scale statistic accepted by an apply() function. E.g. 'sd', 'mad'.
#                        Added parameters 'thermometers' and 'circles' to define what is plotted by the symbols() function and parameterize them.
#                        Added parameters 'lm' and 'loess' to define whether to show the linear fit and the loess fit.
#                        Added parameter 'ylim2' for the limits of the secondary axis where the target variable is plotted.
#                        Added parameters 'xlimProperty', 'ylimProperty', 'ylim2Property' which can be either "new" or "orig" to indicate whether to use the new (after x categorization) range of values or the original range of values
#                        respectively for the x-axis scale, the y-axis scale and the secondary y-axis scale used for the target variable.
#                        Added parameters 'addXAxis' and 'addYAxis' to solve the problem of manually adding the axis ticks on the pairs() plots. The idea is that the axis ticks are manually added when pairs() is run with xaxt="n" and/or yaxt="n" and the user explicitly requests to add the axis ticks.
#                        Added parameter 'addY2Axis' to make sure that the secondary y-axis is shown on every panel subplot when the secondary y-axis scale is NOT the same (which happens --unless different missing patterns occur in the data-- when ylim2Property="orig") 
#                        Added parameter 'plot' that defines wheter the plot is produced or not. This is useful when we just want to have the binned data generated by this function.
#                        Eliminated parameter 'pairsFlag'. Now its value is computed automatically by the function by checking whether this function was called from a pairs() function, using sys.calls().
#                        Eliminated parameter 'return'. Now I ALWAYS return the binned data used for plotting.
#                        Redefined the way xlim and ylim are computed:
#                        - in both cases the newly added parameters xlimProperty and ylimProperty are used to define whether the scale used is the scale of the new categorized values or the scale of the original x and/or y values.
#                        - for xlim in addition I had to make sure that its value is used in the symbols() and the plot() functions in order to have
#                        the plot of the target vs. x_cat values and the plot of the y vs. x_cat values aligned. At first this seems not necessary,
#                        but it is required because the way that the symbols() and the plot() functions compute the user coordinates based on range of values of x
#                        are different! (generating therefore a desalignment unless the xlim option is explicitly defined with the same value in both function)
#           (2013/09/21) Added parameter 'pointlabels' to define whether labels showing the group size are shown.
#                        Added parameters 'col', 'col.lm', 'col.loess' to define the colors of the points and the line fits to draw.
#                        Changed the behaviour of xlim, ylim, ylim2 with xlimProperty, ylimProperty and ylim2Property, respectively,
#                        so that now any value given in xlim, ylim or ylim2 has precedence over the value given in xlimProperty, ylimProperty and ylim2Property.
#						(2013/11/15) Added parameter 'clip' which indicates the area to which plotting should be clipped. This affects the xpd
#												 option of par() and is set to "figure" by default in order to avoid having large bubbles near the axis being
#												 partially hidden by the axis itself.
#						(2013/12/31) Added the min and max values of x and y for each x category as part of the data returned by the function as output$data.
#												 NOTE: THIS STILL NEEDS FIXING BECAUSE IT DOES NOT WORK WHEN 'pred' OR 'target' ARE PASSED. For now it is commented out.
#						(2014/01/04) Added parameters col.pred and col.target to define the colors of the 'pred' and 'target' lines in the plot.
#						(2014/03/04) Changed the color parameter for the pointlabels and error bars from "blue" to the one specified in 'col'.
#						(2014/03/11) Added parameters 'type2' and 'pch2' defining the form and symbols for the plot of the target variable.
{
  #----------------------------------- Parse input parameters ---------------------------------#
  ### Check whether this function is called by pairs, in order to set parameter add=TRUE, so a new plot is NOT created (which gives an error in pairs())
  # Get the list of all calling functions (since the very beginning)
  callingFun = sys.calls()
  # Extract just the function name from each call (which includes also parameter names)
  # Note the use of the [[]] operator instead of the [] operator because [1] returns the function name followed by parenthesis '()' and [[1]] returns just the function name.
  callingFun = lapply(callingFun, "[[", 1)
  # Compute the flag variable that indicates whether this function was called by pairs()
  pairsFlag = length(unlist(sapply(callingFun, grep, "$pairs"))) > 0

  ### Parse parameters regarding pairs() function call, add option, etc.
  if (print)	{	cat("Function call...\n"); print(match.call()) }
  if (pairsFlag) {
    # Set add=TRUE to avoid this function from creating a new plot since the pairs() function does NOT allow
    # the creation of a new plot. Plots can only be ADDED to each panel.
    # Note that add=TRUE used in plotting functions is *logically* equivalent to new=TRUE in the par() function.
    # In fact, add=TRUE means "add a plot to the current graph *respecting the current user coordinates*"
    # while par(new=TRUE) means "create a new plot on the current graph but update the user coordinates".
    add = TRUE
  }
  if (add) {
    # Do not show the axis ticks when the plot is added to an existing graph
    xaxt = "n"
    yaxt = "n"
  } else {
    if (!exists("xaxt", envir=sys.nframe())) { xaxt = par("xaxt") }
    if (!exists("yaxt", envir=sys.nframe())) { yaxt = par("yaxt") }
    # Axis labels
    if (missing(xlab)) { xlab = deparse(substitute(x)) }
    if (missing(ylab)) { ylab = deparse(substitute(y)) }
  }

  ### Parse graphical options
  # Read the current mfg property (used when this function is called by pairs() to define where to place the secondary axis for the target values (when passed)
  mfg = par("mfg")  # The structure of mfg is: (current panel (row, col), whole panel size (rows, cols))
  # Set the plot clipping to the area indicated by the 'clip' parameter, which can be either "figure" or "region".
  # By default the clipping area is set to "figure" so that when bubbles fall outside of the axis ranges (e.g. when they are too big),
  # the whole bubble is seen in its entire (instead of being hidden by the axis)
  xpd = par(xpd=(tolower(clip)=="figure"))
  on.exit(par(xpd))
  #----------------------------------- Parse input parameters ---------------------------------#

  
  #------------------------------------ Data preparation --------------------------------------#
  ### NOTE: (2013/09/16) This section of reading the data could be radically improved by:
  ### - allowing missing values for the y, target and pred variables
  ### - creating first the data frame (as I do below) and then removing missing values using the na.omit() function for example or other useful function to deal with missing values in matrices.

  ### Collapse x, y, pred and target into numeric matrices.
  # The numeric conversion is motivated because we need to compute averages on them and only numeric values are accepted for that
  # (note that factor variables do not accept averages to be computed on them and as.numeric() removes the factor property)
  # (note also the need to use as.character() inside the as.numeric() function and this is to preserve the original values of a
  # potential factor variable because for example as.numeric(factor(c(0,1))) returns '1 2' and NOT '0 1'!!! Note that it seems that no precision is lost on numeric variables when their values are converted to character first with as.character())
  # The matrix conversion is to avoid having data frames to deal with because the handling done below may lead to errors on data frames.
  # Store the original names of x and y for later use when returning the object with the plotted data.
  x = as.matrix(as.numeric(as.character(x))); xname = colnames(x);
  y = as.matrix(as.numeric(as.character(y))); yname = colnames(y);
  if (!is.null(pred))	  { pred = as.matrix(as.numeric(as.character(pred))) }
  if (!is.null(target))	{ target = as.matrix(as.numeric(as.character(target))) }

  # Consider only valid observations, i.e. the observations not having missing values in any of x, y and pred
	if (!is.null(pred)) {
    indok = !is.na(x) & !is.na(y) & !is.na(pred)
  } else {
    indok = !is.na(x) & !is.na(y)
  }
  x = x[indok]
  y = y[indok]
	if (!is.null(pred)) pred = pred[indok]

	# Sort x for plotting reasons
	xorder = order(x)
	x = x[xorder]
	y = y[xorder]
	if (!is.null(pred))   { pred = pred[xorder] }
	if (!is.null(target)) { target = target[xorder] }
  
	if (!grouped)
	{
	  if (is.null(breaks)) {
	    breaks = groups
	  } else  {
	    # Add -Inf and Inf to the breaks vector so that all values in the input variable x are mapped to a category
	    breaks = unique(c(-Inf, breaks, Inf))
	  }
		# Group the values of x when it is not already grouped.
		if (rank)
		{	
		  # Group the values of x based on its rank (this is equivalent to using the quantile() function to define equally-sized groups!)
		  # Note that here the breaks apply to the rank values of x, NOT to the original x values! (the rank values look like 1, 2, ..., n)
		  x_cat = as.numeric(cut(rank(x), breaks=breaks, right=FALSE))
		} else {
		  # Group the values of x in equally size distances (but not in equal size groups!)
		  x_cat = as.numeric(cut(x, breaks=breaks, right=FALSE))
		}
	}
	else
	{	x_cat = x	}
	
  dat = data.frame(x=x, y=y)
	if (!is.null(pred))   { dat$pred = pred }
  if (!is.null(target)) { dat$target = target }

  # Aggregated values using 'center' as the function to compute such value (e.g. center=mean)
  # Min and Max values are also computed for x and y, as well as the scale, using 'scale' as the function to compute the scale (e.g. scale=sd)
	dat_center = aggregate(dat, by=list(x_cat), FUN=center, na.rm=TRUE)
	dat_scale = aggregate(dat, by=list(x_cat), FUN=scale, na.rm=TRUE)
	dat_n = aggregate(dat, by=list(x_cat), length)
	toplot = merge(dat_center, dat_scale, by="Group.1", suffixes=c("_center","_scale"))
	toplot = merge(toplot, dat_n, by="Group.1", suffixes=c("","_n"))
	toplot = rename.vars(toplot, from=c("Group.1", "x", "y"), to=c("x_cat", "x_n", "y_n"), info=FALSE)
	if (!is.null(pred)) { toplot = rename.vars(toplot, from=c("pred"), to=c("pred_n"), info=FALSE) }
	if (!is.null(target)) { toplot = rename.vars(toplot, from=c("target"), to=c("target_n"), info=FALSE) }
	# 2013/12/31: New way but this does NOT work when pred and target are NOT empty because pred_center and target_center are not found.
	# The aggregate with FUN=center should be the first to be computed!
#	dat_min = aggregate(dat, by=list(x_cat), FUN=min, na.rm=TRUE); dat_min = rename.vars(dat_min, from=c("x", "y"), to=c("x_min", "y_min"), info=FALSE);
#	dat_max = aggregate(dat, by=list(x_cat), FUN=max, na.rm=TRUE); dat_max = rename.vars(dat_max, from=c("x", "y"), to=c("x_max", "y_max"), info=FALSE);
#	dat_center = aggregate(dat, by=list(x_cat), FUN=center, na.rm=TRUE); dat_center = rename.vars(dat_center, from=c("x", "y"), to=c("x_center", "y_center"), info=FALSE);
#	dat_scale = aggregate(dat, by=list(x_cat), FUN=scale, na.rm=TRUE); dat_scale = rename.vars(dat_scale, from=c("x", "y"), to=c("x_scale", "y_scale"), info=FALSE);
#	dat_n = aggregate(dat, by=list(x_cat), length); dat_n = rename.vars(dat_n, from=c("x", "y"), to=c("x_n", "y_n"), info=FALSE);
#	toplot = merge(dat_min, dat_max, by="Group.1")
#	toplot = merge(toplot, dat_center, by="Group.1")
#	toplot = merge(toplot, dat_scale, by="Group.1")
#	toplot = merge(toplot, dat_n, by="Group.1")
#	toplot = rename.vars(toplot, from="Group.1", to="x_cat", info=FALSE)
#	toplot = toplot[,c("x_cat","x_min","x_max","x_center","y_center","y_min","y_max","x_scale","y_scale","x_n","y_n")]
  #------------------------------------ Data preparation --------------------------------------#
  

  #------------------------------------ Prepare to plot ---------------------------------------#
	attach(toplot)
	on.exit(detach(toplot), add=TRUE)		# The add=TRUE option adds the current command to the list of commands to execute "on exit" that has been started already.
	# Linear fit (weighted by the number of cases in each group)
  if (lm)     { lmfit = lm(y_center ~ x_center, weights=x_n) }
	# Loess fit (local regression, weighted by the number of cases in each group)
	if (loess)  { loessfit = loess(y_center ~ x_center, weights=x_n) }

  # X-axis limits (use the original range?)
  if (missing(xlim) & tolower(xlimProperty)=="orig") {
    # Use the range of the ORIGINAL x values (before computing the summary statistic value for each x_cat) when xlimProperty="orig"
    xlim = range(x)
  } else if (missing(xlim)) {
    # Initialize xlim either when it was not specified or when xlimProperty is not equal to "orig"
    xlim = range(x_center)
  }

  # Y-axis limits (use the original range?)
  if (missing(ylim) & tolower(ylimProperty)=="orig") {
    # Use the range of the ORIGINAL y values (before computing the summary statistic value for each x_cat) when ylimProperty="orig"
    ylim = range(y)
  } else if (missing(ylim)) {
    # Initialize ylim either when it was not specified or when ylimProperty is not equal to "orig"
    ylim = range(y_center)
  }
  
  # Secondary Y-axis limits (use the original range of target?)
  if (!missing(target)) {
    if (missing(ylim2) & tolower(ylim2Property)=="orig") {
      # Use the range of the ORIGINAL target values (before computing the summary statistic value for each x_cat) when ylim2Property="orig"
      ylim2 = range(target)
    } else if (missing(ylim2)) {
      # Initialize ylim either when it was not specified or when ylimProperty is not equal to "orig"
      ylim2 = range(target_center)
    }
  }

  # Update ylim if bands for the y values are requested for each x_cat
	if (bands)
	{
	  # If requested, compute bands based on +- stdev or +- stdev/sqrt(n) around the summarized y values. Use either one depending on the value of stdCenter
	  if (stdCenter) {
	    toplot$y_upper = y_center + width * y_scale / sqrt(y_n)
	    toplot$y_lower = y_center - width * y_scale / sqrt(y_n)
	  } else {
	    toplot$y_upper = y_center + width * y_scale
      toplot$y_lower = y_center - width * y_scale
	  }

    # Update ylim so that the upper and lower bands fit in the graph.
		ylim = range(c(ylim, y_center, toplot$y_lower, toplot$y_upper), na.rm=TRUE)
		# Extend the y axis range so that the labels fit in the plot (specially this extension is necessary for the
		# lower limit if pos=1 is used in the text() function below. If pos=3 is used, the upper limit should be extended)
		# Note that in package grDevices there is also the function extendrange() but I am not using it here because
		# I don't know if grDevices is included with the base installation of R.
		ylim[1] = ylim[1] - 0.1*(ylim[2] - ylim[1])
		ylim[2] = ylim[2] + 0.1*(ylim[2] - ylim[1])
	}

  # Update ylim if boxplots of the y values are requested for each x_cat
	if (boxplots)
	{
		box = boxplot(y ~ x_cat, plot=FALSE)
		# Update ylim so that the boxplots fit in the graph.
		ylim = range(c(ylim, box$stats, box$out))
	}

	# Compute the parameter inches so that the symbol's minimum size is the one given in size.min
	nmin = min(x_n)
	nmax = max(x_n)
	inches = min(size.min * nmax / nmin, inches)
  #----------------------------------- Prepare to plot ---------------------------------------#
  

  #----------------------------------------- Plots -------------------------------------------#
  if (plot) {
    # Plot symbols
    par(new=add)
    if (missing(xlim)) {
      # Explicitly set the xlim values because the symbols() and the plot() function behave differently
      # when setting the user coordinates (par$usr) for the plot.
      # Since I use the plot() function to plot the potentially existing target values on top of the
      # y vs. x values, I need to explicitly set the xlim option both in symbols() and in plot()
      xlim = range(x_center)
    }
    if (is.null(circles) & is.null(thermometers)) {
      # Plot circles by default whose size is defined by the number of cases in each categorized x point
      symbols(x_center, y_center, circles=x_n, inches=inches, col=col, bg=col, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, xaxt=xaxt, yaxt=yaxt, ...)
    } else {
        if (!is.null(circles)) {
          # Plot circles whose size is defined by the variable passed by the user
          symbols(x_center, y_center, circles=eval(circles), inches=inches, col=col, bg=col, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, xaxt=xaxt, yaxt=yaxt, ...)
        }
        if (!is.null(thermometers)) {
          # Plot thermometers
          # If a target variable was passed, the thermometers color is defined by the target value
          if (!is.null(target)) {
            # Coefficient of variation of the target values (weighted by the bin the x_cat category sizes!)
            # This is done to scale the colors so that when the variation is not so much (<< 100%) the colors are not so much spread out about the average yellow color
            target_mean =  weighted.mean(target_center, x_n)
            cv = sqrt(cov.wt(as.matrix(target_center), wt=x_n)$cov) / target_mean
            r = 0.5 + (target_center - target_mean) / diff(range(target_center)) * cv * 2
            g = 0.5 + (target_mean - target_center) / diff(range(target_center)) * cv * 2
            # Bound the values of r between 0 and 1
            ifelse(r<0,0,ifelse(r>1,1,r))
            ifelse(g<0,0,ifelse(g>1,1,g))
            fg = rgb(r,g,0,1)
          } else {
            fg = col
          }
          symbols(x_center, y_center, thermometers=eval(thermometers), inches=inches, col="black", fg=fg, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, xaxt=xaxt, yaxt=yaxt, ...)
          # Add a text showing the target values
          xpd = par(xpd=TRUE)
          text(x_center, y_center, pos=4, offset=offset, label=as.character(signif(target_center,2)), cex=cex.label*log10(1 + x_n/10), col=rgb(r,g,0,1))
            ## use pos=4 because in the other text() function below I use pos=1 to add the values of the category sizes
          par(xpd=xpd)
          # Add information at the top regarding the overall average of target
          mtext(paste("Target average:", signif(target_mean,2)), side=3, cex=0.8*cex.label)
        }
    }
    # Decide whether to add axis tick marks when this function is called from pairs()
    # Note that when this is not the case and the user passed parameter add=TRUE,
    # no axis ticks are added here because it is the user's responsibility to add them wherever they want.
    # (in fact since we don't know where the primary axes are located we cannot decide where to add the axes for the plot currently being added)
    if (pairsFlag) {
      # If requested, manually place the x-axis ticks
      if (addXAxis) {
        axisProperty = pairsXAxisPosition(xaxt=xaxt)
        axis(at=pretty(c(xlim,x_center)), side=axisProperty$side, outer=axisProperty$outer, line=axisProperty$line)
      }
      # If requested, manually place the y-axis ticks
      if (addYAxis) {
        axisProperty = pairsYAxisPosition(yaxt=yaxt)
        axis(at=pretty(c(ylim,y_center)), side=axisProperty$side, outer=axisProperty$outer, line=axisProperty$line)
      }
    }
    if (lm) { lines(x_center, lmfit$fitted, col=col.lm,  lwd=2, lty=2) }
    if (loess) { lines(x_center, loessfit$fitted, col=col.loess, lwd=2) }
    
    # If non-null, plot the fitted values (given in pred).
    if (!is.null(pred))
    {
    	lines(x_center, pred_center, col=col.pred, lwd=2)
    }
    # If requested plot boxplots of y for each x_cat
    # (note that boxplots should come BEFORE any 'bands' because boxplots hide any previously plotted bands (because they normally are larger than the bands!)
    if (boxplots)
    {
      boxplot(y ~ x_cat, add=TRUE, at=x_center, boxwex=0.01, xaxt="n", top=TRUE)
      ## xaxt="n" to avoid showing the x values that may cause confusion in the plot.
    }
    # If requested, plot the +- stdev or +- stdev/sqrt(n) bands around the summarized y values.
    if (bands)
    {
    	points(x_center, toplot$y_upper, col=col, pch="_", lwd=4, lty=2)
    	points(x_center, toplot$y_lower, col=col, pch="_", lwd=4, lty=2)
    	segments(x_center, toplot$y_lower, x_center, toplot$y_upper, col=col, lwd=2)
    }
    
    # Reference line and labels
    abline(h=0)
    if (is.null(thermometers) & pointlabels) {
      # Only add labels when thermometers are NOT used as symbols (because they already show the target value as labels)
      text(x_center, y_center, x_n, pos=1, offset=offset, cex=cex.label*log10(1 + x_n/10), col="black")
    }
    title(title)
    
    # If a target variable was passed and the requested symbols plot is NOT thermometers,
    # plot the summarized target values by x_cat on a secondary axis.
    # If thermometers symbols are used above, the target variable has already been used to define the fill level of the thermometers
    # This plot must come AFTER plotting everything else related to x and y because a new plot is made --over the existing graph-- with potentially *new* user coordinates (i.e. new y axis limits)
    if (!is.null(target) & is.null(thermometers))
    {
      # Keep the same user coordinates as the current plot on the x axis so that the x positions of the target points coincide with the existing points
      # (this may not be the case even if the x variable is the same as the previous plot!!)
      # Note that setting the same user coordinates does NOT have the same effect as setting the same xlim range!!
      par(new=TRUE)
      plot(x_center, target_center, type=type2, pch=pch2, col=col.target, bg=col.target, lwd=2, xlab="", ylab="", xlim=xlim, ylim=ylim2, xaxt="n", yaxt="n")
      abline(h=weighted.mean(target_center), col=col.target, lty=2)
      # Add the secondary axis ticks only when the panel is at one of the edge columns
      # This is useful when this function is called by pairs() as one of the pair functions (e.g. through upper.panel option) and it mimics what pairs() does.
      # Note that this condition does NOT affect the case when the plot is done on one single panel (i.e. when it is not called by pairs() as one of the pair functions)
      if (mfg[2] == 1 | mfg[2] == mfg[4] | addY2Axis) {
        if (pairsFlag & !addY2Axis) {
          # When this function is called by pairs() and the user did NOT request to explicitly add the secondary y-axis explicitly on every panel subplot,
          # decide where to place the secondary axis ticks so that they do not overlap with the axis used by pairs().
          # pairs() places the vertical axis on the right when the panel row is odd and on the left when the panel row is even.
          outer = TRUE
          line = 0  # This number should be -gap, where gap is the space between the subplots in margin lines. However, I don't know yet how to retrieve the value of gap in the current pairs() call!
          if (mfg[1] %% 2 == 1) {
            side = 2
          } else {
            side = 4
          }
        } else {
          outer = FALSE
          line = 0
          side = 4
        }
        axis(side=side, at=pretty(c(ylim2,target_center)), col=col.target, col.ticks=col.target, outer=outer, line=line)
        ## Note the combined use of outer=TRUE and line=-1 which makes the secondary axis tickmarks to be shown on the sides of the big pairs() plot, as opposed to on the sides of the current panel
        ## It seems it is not possible to also color the tick labels because there is no option col.lab in axis()...
      }
    }
  }
  #----------------------------------------- Plots -------------------------------------------#
#	detach(toplot)
#	# Restore plotting parameters changed above
#	par(xpd=xpd)
  
	if (print) { cat("Data plotted...\n"); print(toplot) }
	
  output = list(data=toplot)
  if (lm)    { output$lm.fit = lmfit }
  if (loess) { output$loess.fit = loessfit }
  return(output)
}


# Pairs plot of a set of variables where customized information is shown on the lower, diagonal and upper panels
# CHECK ALSO the car package (companion to applied regression) for interesting functions to enhance plots commonly used for regression)
pairs.custom = function(x, lower.panel=plot.image, diag.panel=panel.dist, upper.panel=points, addXAxis=FALSE, addYAxis=FALSE, max.panels=6, pause=TRUE, ...)
# Created:      2013/07/08
# Modified:     2013/07/08
# Description:  A manual pairs plot is produced. The plot is manual in that no call to the pairs() function is done but rather
#               the tiling of plots is generated using the par(mfrow) option.
#               (In fact the pairs() function does NOT allow a plotting function as panel function to make a NEW plot.
#               This implies that the axis limits cannot be redefined for e.g. a binned plot where the minimum and maximum values
#               of the plotted variables change after binning! => I cannot call plot.binned() as a panel function of pairs() and redefine the axis limits at the same time)
# Details:      The customized pairs plot show:
#               - Diagonal: density estimation with overimposed boxplot
#               - Upper diagonal: binned scatter plots produced by plot.binned() function defined above.
#               - Lower diagonal: correlation values produced by panel.cor() function defined above or the 2D kernel density estimation shown as image intensities
# Parameters:  
#               x:        	Matrix or data frame whose columns are used to produce the pairs plot
#               addXAxis:	  Whether to manually add the x-axis ticks (this was invented because the value of par("xaxt") is NULL when checking its value from within a function called as a pairs() panel function; this means that I cannot decide whether to manually add the axis checking whether the axis ticks have already been placed by the pairs() function)
#               addYAxis: 	Whether to manually add the y-axis ticks (this was invented because the value of par("yaxt") is NULL when checking its value from within a function called as a pairs() panel function; this means that I cannot decide whether to manually add the axis checking whether the axis ticks have already been placed by the pairs() function)
#								max.panels: Max number of panels to show per pairs plot. Several pairs plot are constructed when the number of variables in x is larger than max.panels.
#
# HISTORY:  (2013/09/03) Took care of missing values in the data (i.e. removed cases that have missing values in ANY of the analyzed variables x, y and pred.
#           (2013/09/15) Placed the two functions used by default for the lower and diagonal panels outside of this function so that they can be called by any pairs() call.
#						(2014/03/11) Added two new parameters: 'max.panels' and 'pause'.
#												 max.panels specifies the maximum number of panels to show per pairs plot.
#												 pause indicates whether to pause between plots and wait for a key pressed to continue.
#												 NOTE: If there is a target variable, it should be placed FIRST in the x matrix, so that it is plotted in ALL pairs plot at the first row.
#												 Also, for some reason the target variable CANNOT be passed as e.g. dat[,"target"] but it should be passed as dat$target.
#												 In the former case, I got the error "only 0's can be mixed with negative subscripts"... Not yet clear why I get this error.
#
{
  # Store the list of explicitly defined parameters in this function
  # (note that this information is not used but was left here as a reference of how to read the parameters explicitly passed by the user during the function call)
  funParams = ls(pos=sys.frame(sys.nframe())) # The list of function parameters is returned in alphabetical order
  
  # Read the function call
  call = match.call()
#  params = names(call)  # The parameters are listed as follows: first the explictily defined parameter, then the parameters passed in '...'
#  # Remove the function name and the explicitly defined parameters from the parameter list
#  paramsList[[1]] = NULL
#  paramsListToRemove = c(1, which(params %in% funParams))
#   ## Index 1 is the function name (i.e. pairs.custom), the following indices are consecutive indices
#   ## since 'params' contains the parameter list in the order explained above (first explicit parameters, then parameters passed in '...')
#  # Remove the function name and the other parameters explicitly listed in this function from the parameter list
#  for (i in 1:length(paramsListToRemove)) {
#    p = paramsListToRemove[i]
#    paramsList[[p]] = NULL
#    # Next parameter to remove is located at paramsListToRemove[p+1]-1 since after removing element 'p' there is one less element in the list!!
#    paramsListToRemove = paramsListToRemove - 1
#  }
  
  # Put all calling parameters into a parameter list for the pairs() function call with do.call()
  paramsList = as.list(call)
  # Remove the function name from the parameter list
  paramsList[[1]] = NULL

  # Set parameters not passed by the user to the custom value I want to use here.
  # Note that I need to check if they were not passed by the user before assigning the custom value
  # because I don't want to override the user's specification!
  # Note also that NOT all parameters defined by the function are included in the parameter list
  # returned by match.call() because match.call() only includes parameters explicitly passed by the user!
  if (is.null(paramsList$lower.panel)) paramsList$lower.panel = plot.image
  if (is.null(paramsList$diag.panel))  paramsList$diag.panel  = panel.dist
  ## Note that the third parameter upper.panel is NOT defined because there is no custom value assigned to it.
  if (is.null(paramsList$gap)) paramsList$gap = 0       # pairs() parameter specifying the distance between panel plots in margin lines
  if (is.null(paramsList$new)) paramsList$new = FALSE
  if (is.null(paramsList$oma)) paramsList$oma = c(2,3,2,3)

	# Limit the number of panels on each plot to the value specified in 'max.panels'
  imax = ncol(x)
  nplots = ceiling(imax/max.panels)
	if (nplots > 1) {
		# Define the first column with input variables to plot in the pairs plot (i.e without considering any target variable passed by the user)
		if (!is.null(paramsList$target)) {
			# It is assumed that the target variable is at the first column of x
			target = x[,1]
			# Reduce the value of max.panels because there is one more row for the target variable already in every pairs plot
			max.panels = max.panels - 1
			# First independent variable to plot
			i = 2
		} else {
			target = NULL
			# First independent variable to plot
			i = 1
		}
		if (!is.null(target)) {
			# The number of panels per pairs plot is max.panels + 1
			cat("For better view, the pairs plot is divided into", nplots, "plots of", max.panels+1, "panels each.\n")
		} else {
			# The number of panels per pairs plot is max.panels
			cat("For better view, the pairs plot is divided into", nplots, "plots of", max.panels, "panels each.\n")
		}
		plotnum = 0
		for (i in seq(i,imax,max.panels)) {
			plotnum = plotnum + 1
			cat("Plot", plotnum, "of", nplots, "...\n")
			if (!is.null(paramsList$target)) {
				paramsList$x = x[,c(1,i:min(i+max.panels-1, imax))]
			} else {
				paramsList$x = x[,i:min(i+max.panels-1, imax)]
			}			
		  # Call the pairs() function
		  do.call("pairs", paramsList)
		  i = i + max.panels
		  if (pause & i <= imax) {
		  	cat("Press ENTER for next plot:")
		  	readline()
		  }
		}
	} else {
		# Call the pairs() function
		do.call("pairs", paramsList)	
	}

}

# Histogram in log scale
hist.log = function(x, constant=1, format="E", cex=1, las=1, ...)
# Created: 		    2008/09/19
# Modified: 	    2008/09/19
# Description:	  Histogram of a variable using log scale, with the original scale used as labels.
# Parameters:	
#				x			    Vector of values to use in the histogram.
# 			constant	Constant to shift the log transformation in sign(x)*log10(constant + x).
# 			format		Format to use for the numbers in the axis labels.
# 			cex		  	Character EXpantion option for the labels.
# 			las			  las option for the label definining the text orientation (1 = horizontal, 2 = vertical)
#				...		  	Additional parameters to pass to the hist() function.
{
	xlog = safeLog(x, constant)

	xmin = floor(min(x))
	xmax = ceiling(max(x))
	
	hist(x, xaxt="n", ...)
	
	axis(1, at=xmin:xmax, labels=FALSE);
	labels = as.character(formatC(10^(xmin:xmax), format=format, digits=0))
	mtext(labels, side=1, at=xmin:xmax, line=1, cex=cex, las=las);
}

# Plot in log scale (the log transformation is safeLog(x) = sign(x)*log10(constant + x))
# NOTE (2008/10/03): Could use function get() to decide what kind of plot to do depending on parameter (e.g. plot or hist)
#fname <- "pchisq";
#fname <- get(fname , mode="function");
#fname(x,3);				# This is exactly the same as doing pchisq(x,3);
plot.log = function(x, y, log="x", constant=c(1,1), format=c("E","E"), cex=c(1,1), las=c(1,1), ...)
# Created: 		  2008/09/19
# Modified:   	2008/09/19
# Description: 	Plot with log scale in one or the two axis, with the original scale used as labels.
# Parameters:	
#				        x			    Vector of values to plot in the horizontal axis.
#				        y			    Vector of values to plot in the vertical axis.
# 				      log		    1-D or 2-D vector listing the axes to be log-transformed
#							            Ex: c("x","y")
# 				      constant	1-D or 2-D vector specifying the constant to use in shifting the log transformation
#							            in safeLog(x) = sign(x)*log10(constant + x).
# 				      format		1-D or 2-D vector specifying the format to use for the numbers in the axis tick labels.
# 				      cex 			1-D or 2-D vector specifying the Character EXpantion option for the axis tick labels.
# 				      las	  		1-D or 2-D vector specifying the orientation of the axis tick labels
#							            (1 = horizontal, 2 = vertical)
#				        ...			Additional parameters to pass to the plot() function.
{
	# Get the range of values to be plotted for each axis
	xmin = floor(min(x))
	xmax = ceiling(max(x))
	ymin = floor(min(y))
	ymax = ceiling(max(y))

	#------------------------------- Parse input parameters ----------------------------------#
	x = x; logx = FALSE; xaxt = par("xaxt")
	y = y; logy = FALSE; yaxt = par("yaxt") 
	if ("x" %in% tolower(log))
	{ 
		logx = TRUE
		xlog = safeLog(x, constant[1])
		x = xlog
		xaxt = "n"
	}
	
	if ("y" %in% tolower(log))
	{ 
		logy = TRUE
		ylog = safeLog(y, constant[2])
		y = ylog
		yaxt = "n"
	}
	
	xcex = ycex = 1
	if (!is.null(cex))
	{
		xcex = cex[1]
		if (length(cex) > 1)	{ ycex = cex[2] }
	}
	
	xlas = ylas = 1
	if (!is.null(las))
	{
		xlas = las[1]
		if (length(las) > 1)	{ ylas = las[2] }
	}
	#------------------------------- Parse input parameters ----------------------------------#


	#--------------------------------------- Plot --------------------------------------------#
	plot(x, y, xaxt=xaxt, yaxt=yaxt, ...)
	
	if (logx)
	{
		axis(1, at=xmin:xmax, labels=FALSE);
		labels = as.character(formatC(10^(xmin:xmax), format=format[2], digits=0))
		mtext(labels, side=1, at=xmin:xmax, line=1, cex=xcex, las=xlas);
	}
	
	if (logx)
	{
		axis(2, at=ymin:ymax, labels=FALSE);
		labels = as.character(formatC(10^(ymin:ymax), format=yformat, digits=0))
		mtext(labels, side=2, at=ymin:ymax, line=1, cex=ycex, las=ylas);
	}
	#--------------------------------------- Plot --------------------------------------------#
}

# Plot using specified x tick marks
plot.axis = function(x, y, xticks=NULL, xlabels=NULL, las=1, grid=TRUE, lty.grid=2, lwd.grid=1, ...)
# Created: 		    2008/09/25
# Modified:   	  2008/09/25
# Description: 	  Plot using specified x tick marks. Optionally a grid is shown.
# Parameters:	
#				x			    Vector of values to plot in the horizontal axis.
#				y			    Vector of values to plot in the vertical axis.
#				xticks	  Ticks to use in the x axis
#				xlabels	  Labels to use in the x axis
#				las			  Orientation for the labels (1 = horizontal, 2 = vertical)
#				grid		  Whether to show a vertical grid
# 			lty.grid	Line type for the grid lines
#				lwd.grid	Line width for the grid lines
#				...			  Additional parameters to pass to the plot() function.
{
	xaxt = par("xaxt")
	if (!is.null(xticks))	{ xaxt = "n" }

	plot(x, y, xaxt=xaxt, ...)

	if (xaxt == "n")
	{
		axis(1, at=xticks, labels=xlabels, las=las)
	}
	
	if (grid)
	{
		abline(v=xticks, lty=lty.grid, lwd=lwd.grid)
	}
}

# Plot a histogram (i.e. plot boxes like the ones generated by hist())
plot.hist = function(x, y, ...)
# Created: 		  2008/10/03
# Modified: 	  2008/10/03
# Description: 	Plot a histogram.
# Parameters:	
#				x			  Breaks of histogram
#				y			  Counts or density of histogram
#				...		  Additional parameters to pass to the plot() function.
{
	toplot = cbind(x[-1], y)
	tp = matrix(nrow=3*nrow(toplot)+1, ncol=2)
	tp[1,] = c(x[1],0)
	xprev = tp[1,1]
	for (i in 1:nrow(toplot))
	{
		tp[i*3-1,] = c(xprev, toplot[i,2])
		tp[i*3,] = toplot[i,]
		tp[i*3+1,] = c(toplot[i,1], 0)
		xprev = tp[i*3,1]
	}
	# Plot the histogram
	polygon(tp, ...)
}

# TODO:
# - 2013/11/15: Extend this function so that biplots of a selected set of principal components (more than 2!) are produced in a pairs
# plot fashion. Currently this is not possible because I am using function filled.contour() which always generates a new plot
# (thus making it unsuitable for a pairs plot...). However, we could try using the contour() function which can be used to ADD a
# graph to an existing plot (using add=TRUE). The contour() function does not produce colors to indicate the height of the contour,
# however I think the colors can be produced by calling the image() function first, or perhaps better my own plot.image() function.
# For examples of using contour() in this way, see the examples of the hist2d() function in the gplots package.
biplot.custom = function(x, pc=1:2, arrowsFlag=TRUE, pointsFlag=FALSE, outliersFlag=TRUE, pointlabels=NULL, thr=4, outliersMaxPct=0.5, ...)
# Created:      2013/08/28
# Modified:     2013/08/28
# Description:  Custom biplot suitable for many points
# Details:      The customized biplot shows:
#               - 2D estimated desnsity instead of actual points
#               - Axis limits are computed properly
#               - Optionally, a panel plot of the first k principal components in pairs (e.g. top PCs explaining 80% of the variation)
# Parameters:  
#               x:  An object of class princomp
# Output:       When outliersFlag=TRUE, a data frame containing the points identified as outliers on the analyzed principal components
#								is returned.
# Assumptions:  There are no missing values (NA) in the data!
{
  
  #----- DEFINE AUXILIARY FUNCTIONS -----#
  plot.ArrowsAndPoints = function(x, arrowsFlag=TRUE, pointsFlag=FALSE, outliersFlag=TRUE, pointlabels=NULL, thr=4, outliersMaxPct=0.5) {
    # Function to plot the arrows representing the projection of the variable axes into the PCs and optionally the observations as points
    # x:              An object of class princomp
    # pointlabels:    Vector whose values define the symbols to show instead of points
    # thr:            Multiplier threshold to use to define an outlier based on the matrix diagonals, h, as follows:
    #                 flag a case as outlier if h > thr*p/n, where p is the number of analyzed PCs and n is the number of cases
    # outliersMaxPct: Maximum percentage of cases to flag as outliers
    #

    # Read the scores for the PCs to analyze
    # (which represent the points to be plotted or the data whose mean vector define the centroid of the points --always shown)
    X = x$scores[,pc]

    # Restore axis ticks and labels for the density contour plot (which are removed by using this plot.arrows() function as plot.axes option of filled.contour() below)
    xaxp = par("xaxp")
    xticks = seq(xaxp[1], xaxp[2], length.out=xaxp[3]+1)
    yaxp = par("yaxp")
    yticks = seq(yaxp[1], yaxp[2], length.out=yaxp[3]+1)
    axis(1, at=xticks, labels=xticks, col="blue", col.ticks="blue")
    axis(2, at=yticks, labels=yticks, col="blue", col.ticks="blue")
    mtext(paste("Comp.", pc[1], sep=""), side=1, line=3, las=1)   # xlabel
    mtext(paste("Comp.", pc[2], sep=""), side=2, line=3, las=0)   # ylabel
    
    ### Variables on which the PCA was applied (they are given by the row names of the loadings matrix provided by the princomp object x)
    vars = rownames(x$loadings)

    ### Points
    if (pointsFlag) {
      axis(1); axis(2);
      if (is.null(pointlabels)) {
        points(X, pch=21, col=rgb(0,0,0,0.2), bg=rgb(0,0,0,0.2), cex=0.3)
      } else {
        text(X, label=pointlabels, adj=0, offset=0, col=rgb(0,0,0,0.8), bg=rgb(0,0,0,0.8), cex=0.5)
      }
    }

    ### Label the outliers based on the hat matrix diagonal elements
    # Create the indOutliers variable in the environment of biplot.custom(), which is TWO (2) environment positions before the current function, since this function is called by filled.contour())
    assign("indOutliers", pos=sys.nframe()-2, NULL)
    if (outliersFlag) {
      # Identify the outliers on the analyzed PCs based on the hat matrix (i.e. distance from the centroid relative to disperson of the data)
      H = X %*% solve(t(X) %*% X) %*% t(X)
      hat = diag(H)
      p = length(pc)
      n = nrow(X)
      if (!is.null(outliersMaxPct)) {
        hat.quant = quantile(hat, probs=1-outliersMaxPct/100)
      } else {
        hat.quant = 0
      }
      assign("indOutliers", pos=sys.nframe()-2, which((hat>thr*p/n) & (hat> hat.quant)))  # NOTE: It seems it's important to use parenthesis to enclose each > condition!
      if (is.null(pointlabels)) {
        label = 1:length(indOutliers)
      } else {
        label = pointlabels[indOutliers,]
      }
      # Clip the points and its labels to the figure region
      op = par()
      par(xpd=TRUE)
      points(X[indOutliers,], pch=21, col="black", bg="black", cex=0.5)
      text(X[indOutliers,], label=label, pos=4, cex=0.5)
      mtext(paste(signif(length(indOutliers)/n*100,1), "% of the observations are identified as outliers"), side=3, cex=0.8)
      par(xpd=op$xpd)
    }

    ### Position of centroid of analyzed PCs
    # Add a star representing the average value of the analyzed PCs
    # (note that I compute the average of the PCs coordinates, not the projection onto the PCs of the data average.
    # This is so because the 2D density estimation is based on the PC coordinates and it is NOT the project of the
    # n-dimensional density estimation on the n-dimensional data!)
    meanPC = apply(X, 2, FUN=mean)
    points(meanPC[1], meanPC[2], pch="x", col="blue", cex=1.5)

    ### Arrows representing the original axes on the analyzed PCs
    if (arrowsFlag) {
      # Define the arrow start and end positions
      npcs = ncol(x$loadings)
      arrows.start = matrix(data=0, nrow=npcs, ncol=2) # column 1 is x coordinate of the start, column 2 is y coordinate
      arrows.end = x$loadings[,pc]
      # Compute contributed percentage of each variable on the analyzed PCs w.r.t. all PCs
      contrPct = x$loadings / apply(abs(x$loadings), 1, FUN=sum) * 100
      contrPct.pc = apply(abs(contrPct[,pc]), 1, FUN=sum)
      
      # Jitter arrow ends for the position of variable labels to increase readability
      set.seed(1717)
      pos.labels = jitter(arrows.end, factor=10)
  
      # Define symmetric axis so that the (0,0) of the arrows coincide with the (0,0) of the density contours already plotted!
      xlim = range(c(arrows.start[,1], arrows.end[,1], pos.labels[,1])); xlim = c(-max(abs(xlim)), max(abs(xlim)))
      ylim = range(c(arrows.start[,2], arrows.end[,2], pos.labels[,2])); ylim = c(-max(abs(ylim)), max(abs(ylim)))
  
      # Create a new plot on the existing density contour
      par(new=TRUE)
      plot(arrows.end, type="n", xaxt="n", yaxt="n", xlim=xlim, ylim=ylim, xlab="", ylab="")
      arrows(arrows.start[,1], arrows.start[,2], arrows.end[,1], arrows.end[,2], length=0.15, col="blue")
  
      # Add the labels indicating the variable names corresponding to each arrow
      usr = par("usr")
      xticks = axTicks(1, usr=usr[1:2], log=FALSE)
      yticks = axTicks(2, usr=usr[3:4], log=FALSE)
      axis(3, at=xticks, labels=xticks)
      axis(4, at=yticks, labels=yticks)
      par(xpd=TRUE) # Clip plotting to the figure region so that if labels fall outside the plot they are still shown!
      text(pos.labels, labels=paste(vars, " (", signif(contrPct.pc, digits=2), "%)", sep=""), offset=0, cex=0.7)
    }
  }

  #--------------------------- Parse input parameters ---------------------------
  # Color definition for the different levels of the filled contour plot
  whitered.palette = colorRampPalette(c("white", "red"))
    ## White color is used as starting color so that it shades out with the default background color of plots

  # Compute the 2D estimated density of the first PCs
  x.kde = kde2d(x$scores[,pc[1]], x$scores[,pc[2]], n=25)  # n=25 is the default number of grid points to generate for the density plotting
  
  # Set symmetric plot limits so that the (0,0) for the density contour (of observations) and variables match on the same position!
  xlim = range(x.kde$x); xlim = c(-max(abs(xlim)), max(abs(xlim)))
  ylim = range(x.kde$y); ylim = c(-max(abs(ylim)), max(abs(ylim)))
  # filled.contour: Note the use of the plot.axes option to add the points, so that they are correctly added
  # on the same graphical region as the contours --o.w. they fall outside the plotting region because of the color scale shown on the right, see help() for more info)
  filled.contour(x.kde$x, x.kde$y, x.kde$z, xlim=xlim, ylim=ylim,
                              color.palette=my.colors<-function(nlevels) { whitered.palette(nlevels) },  # nlevels is a parameter defined by default in filled.contour()
                              plot.axes=plot.ArrowsAndPoints(x, arrowsFlag=arrowsFlag, pointsFlag=pointsFlag, outliersFlag=outliersFlag, pointlabels=pointlabels, thr=thr, outliersMaxPct=outliersMaxPct), ...)
  
  if (outliersFlag) {
    # indOutliers is created in function plot.ArrowsAndPoints()
    outliers= data.frame(V1=x$scores[indOutliers,pc[1]], V2=x$scores[indOutliers,pc[2]])
    names(outliers) = c(paste("Comp.", pc[1], sep=""), paste("Comp.", pc[1], sep=""))
    if (!is.null(pointlabels)) {
      rownames = pointlabels[indOutliers,]
    } else {
      rownames = indOutliers
    }
    rownames(outliers) = rownames
    return(outliers)
  }
}

# Plot CDF of a numeric variable
plot.cdf = function(x, probs=seq(0,1,0.01), add=FALSE, ...)
# Created:      12-Mar-2014
# Modified:     12-Mar-2014
# Description:  Plot CDF of a numeric variable in the quantiles specified in parameter 'probs'
# Details:			The parameter probs corresponds to the parameter of the same name in function quantile()
# Parameters:   x: A numeric array
# Output:       A matrix containing the CDF of x where the row names are the quantile values defined in 'probs'.
# Assumptions:  There are no missing values (NA) in the data!
{
	# Stop if X is not numeric (e.g. stop if it is character or if it is a matrix of more than one column)
	stopifnot(is.numeric(x))

	# Check if NAs are present in the data and issue a warning
	na.check = sum(is.na(x))
	if (na.check > 0) {
		na.pct = formatC(na.check/length(x)*100, format="f", digits=1)
		cat("PLOT.CDF: WARNING -", na.check, "NA(s) (", na.pct, "% ) found in array", deparse(substitute(x)), ".\n")
		cat("PLOT.CDF: Analysis based on", length(x)-na.check, "valid values out of", length(x), ".\n")
	}

	# Compute CDF
	x.cdf = quantile(x, probs=probs, na.rm=TRUE)
	
	# Plot
	# Define default values for graphical parameters (by checking whether the user passed any of those defined)
	if (!exists("ylab", envir=sys.nframe())) { type = "l" } 
	if (!exists("xaxt", envir=sys.nframe())) { xaxt = par("xaxt") }
	if (!exists("yaxt", envir=sys.nframe())) { yaxt = par("yaxt") }
	if (!exists("xlab", envir=sys.nframe())) { xlab = deparse(substitute(x)) }
	if (!exists("ylab", envir=sys.nframe())) { ylab = "cdf" } 
	if (!exists("main", envir=sys.nframe())) { main = paste("Total cases:", length(x)-na.check) }
  if (add) {
    # Do not show the axis ticks nor the x label when the plot is added to an existing graph
    xaxt = "n"
    yaxt = "n"
    xlab = ""
	}

	# Construct the CDF values to plot on the vertical axis from the names of the x.dist array
	cdf.values = as.numeric(gsub("%","",names(x.cdf)))
	par(new=add)
	plot(x.cdf, cdf.values, type=type, xlab=xlab, ylab=ylab, xaxt=xaxt, yaxt=yaxt, main=main, ...)
	
	return(as.matrix(x.cdf))
}

plot.bar <- plot.bars <- function(x, y, event="1", FUN="table", decreasing=TRUE, horiz=FALSE, bars=TRUE, width=1, cex=0.8, las=1, col.bars="black", plot=TRUE, ...)
# Created:      26-Mar-2014
# Modified:   	26-Mar-2014
# Description: 	Make a Bar Plot of a categorical variable x showing the average of a target variable y,
#								where the width of the bars are proportional to the size of each x category.
# Parameters:   - x: Either a character or numeric array corresponding to a categorical variable, or a matrix containing  the
#									data (already aggregated) to plot. In the latter case, the columns of x should contain columns named as:
#									- the value of parameter 'event'
#									- "n" (with the number of cases per category)
# 								- "sd" (with the standard deviation of the target variable to plot per category --note that this value
#										should NOT be already divided by sqrt(n))
#								- y: A character or numeric variable that represents some target variable of interest.
#								- event: Value taken by the y variable representing an event of interest (e.g. "1" for a binary 0/1 variable)
#								- FUN: Function to use to apply to the (x, y) pair. It can be either "table" or any other function that is
#									accepted by the aggregate() function that applies to numeric variables. In the latter case, the y variable
#									must be numeric.
#									When FUN=="table", table(x,y) is computed and the proportion of occurrence of 'event' in the y variable is plotted as a bar plot.
#									In all other cases, aggregate(y ~ x, FUN=FUN, na.rm=TRUE) is computed and the resulting value of the y variable is plotted as a bar plot.
#								- decreasing: how the x categories should be sorted in the bar plot. Either TRUE, FALSE or NA (alphabetical order).
#								- horiz: Whether the bar plot should show horizontal or vertical bars (same parameter used by barplot())
#								- bars: Whether to show error bars proportional to the SE of y on each bar.
#								- width: Width of the error bars as multiplier of the SE of y.
#								- cex: Character expansion value to use in the bar plot (parameter cex.names of barplot())
#								- las: Label orientation for the x category axis (parameter las of barplot())
#								- col.bars: Color to use for the error bars
#								- plot: Whether to generate the plot or just return the table with the data to be plotted
#								- ...: Additional parameters to be passed to graphical functions called (bars(), segments(), points())
# Output:       A bar plot is generated where the x categories are sorted by decreasing number of cases.
#								An matrix containing the following columns:
#								- The x categories
#								- The number of non-missing cases in each category
#								- The average value of y and its standard error in each category
# Assumptions:  None
{
	tab = NULL

	# Check if parameter x is already a table containing the raw numbers of what should be plotted
	if (is.matrix(x)) {
		# The input matrix is considered to be already a computed table (i.e. the output of 
		tab = x
		if (!("n" %in% colnames(tab)) | (bars & !("sd" %in% colnames(tab)))) {
			stop("Input parameter '", deparse(substitute(x)), "' does NOT contain the necessary column names ('n' and/or 'sd').\nColumn names are: ", colnames(x))
		}
		if (FUN == "table") {
			xnames = row.names(tab)
			ycol = event
			if (!(event %in% colnames(tab))) {
				stop("Input parameter '", deparse(substitute(x)), "' does NOT contain a column named '", event, "'\n")
			}
		} else {
			xnames = tab[,1]
			ycol = 2
		}
	}
	
	# Start computation in the case x is NOT a matrix
	if (FUN == "table" & is.null(tab)) {
		tab.counts = table(x, y)
		tab = cbind(prop.table(tab.counts, margin=1), n=apply(tab.counts, 1, sum))
		# Standard deviation of y using the probability of occurrence of 'event'
		# (note that I don't yet divide by n this happens at the moment of plotting below)
		tab = cbind(tab, sd=sqrt( tab[,2] * (1 - tab[,2]) ))
		# Names to be used as x categories in the bar plot
		xnames = row.names(tab)
		# Column of 'tab' containing the values of y to plot
		ycol = event
	} else if (is.null(tab)) {
		# The y variable is numeric and FUN is applied to its values to get a representative value for each x category
		if (!is.numeric(y)) {
			stop("Variable '", deparse(substitute(y)), "' is NOT numeric.\n")
		} else {
			tab = aggregate(y ~ x, FUN=FUN, na.rm=TRUE)
			# Compute the number of non-missing cases in each x category.
			# Note that the variable y can have missing values but these are not excluded from the count of non-missing values,
			# only the non-missing values in x are of interest.
			tab = cbind(tab, n=as.numeric(table(x)))			# Note that NAs are NOT part of the table() output
			tab = cbind(tab, sd=aggregate(y ~ x, FUN=sd, na.rm=TRUE)[,2])
			# Names to be used as x categories in the bar plot
			xnames = tab[,1]
			# Column of 'tab' containing the values of y to plot
			ycol = 2
			colnames(tab)[1:2] = c(deparse(substitute(x)), deparse(substitute(y)))
		}
	}

	if (plot) {
		### Bar plot
		xlim = NULL
		ylim = NULL
		# Sort x categories as requested
		if (!is.na(decreasing)) {
			ord = order(tab[,ycol], decreasing=decreasing)
		} else {
			ord = 1:nrow(tab)
		}
		tab = tab[ord,]
		xnames = xnames[ord]
		# Set appropriate limits before calling barplot() when error bars are requested
		if (bars) {
			if (horiz) {
				xlim = range(c(0, tab[,ycol], tab[,ycol] + width*tab[,"sd"]/sqrt(tab[,"n"])))
			} else {
				ylim = range(c(0, tab[,ycol], tab[,ycol] + width*tab[,"sd"]/sqrt(tab[,"n"])))
			}
		}
		# Generate the bar plot and store the position of the center of each bar in 'barpos' so that we can add the error bars if requested
		barpos = barplot(tab[,ycol], names.arg=xnames, horiz=horiz, cex.names=cex, width=tab[,"n"], xlim=xlim, ylim=ylim, las=las, ...)
		if (bars) {
			if (horiz) {
				# Horizontal bars
				x.upper = tab[,ycol] + width*tab[,"sd"]/sqrt(tab[,"n"])
				x.lower = pmax(0, tab[,ycol] - width*tab[,"sd"]/sqrt(tab[,"n"]))
				segments(x.lower, barpos, x.upper, barpos, lwd=2, col=col.bars, ...)
				points(x.upper, barpos, pch="|", lwd=2, col=col.bars, ...)
				points(x.lower, barpos, pch="|", lwd=2, col=col.bars, ...)
			} else {
				# Vertical bars
				y.upper = tab[,ycol] + width*tab[,"sd"]/sqrt(tab[,"n"])
				y.lower = pmax(0, tab[,ycol] - width*tab[,"sd"]/sqrt(tab[,"n"]))
				segments(barpos, y.lower, barpos, y.upper, lwd=2, col=col.bars, ...)
				points(barpos, y.upper, pch="_", lwd=2, col=col.bars, ...)
				points(barpos, y.lower, pch="_", lwd=2, col=col.bars, ...)
			}
		}
	}

	# When x is not a matrix and FUN == "table" add the counts of each y value to the output table
	if (!is.matrix(x) & FUN == "table") {
		tab = cbind(tab.counts[ord,], tab)
	}

	return(tab)
}
######################################### GRAPHICAL functions #################################


########################################## EXTERNAL functions #################################
# 2013/09/19
# Copied from Antoine Thibaud and changed a little bit, as follows:
# - renamed 'cant.bines' to 'groups'
# - added parameter 'lwd' for the line width of the ROC line
roc.1 <- function(formula, data, pos = 1, groups = 20, quantile.type = 1, round.AUX = 2, lwd=1){
  # Genera una curva roc a partir de un df con una columna de clase binaria y otra de probabilidades
  # Hace un rank de las probabilidades y construye una roc en base al mismo
  
  # Mediante el parametro pos, se pueden ir superponiendo hasta 4 curvas roc
  # Ejemplo de uso:
  #   roc.1(clase ~ prob, mod1)
  #   roc.1(clase ~ prob, mod2, pos = 2)
  #   roc.1(clase ~ prob, mod2, pos = 3)
  # donde mod1, mod2 y mod3 son dfs con clase y probabilidad predicha...
  
  df <- model.frame(formula, data)
  colnames(df) <- c('clase', 'pm')
  df$clase <- factor(df$clase)
  
  descrip.clase <- unique(df$clase)
  # Rank de la prob. de 'malo'
  #   df$grupo <- findInterval(df$pm, 
  #                            quantile(df$pm, seq(0, 1, len = groups - 1), type = quantile.type),
  #                            all.inside = TRUE, rightmost.closed = TRUE)
  ranking <- rank(df$pm,  ties.method = "average")
  df$grupo <- findInterval(ranking, 
    quantile(ranking, seq(0, 1, len = groups), type = quantile.type),
    all.inside = TRUE, rightmost.closed = TRUE)
  
  tbl <- as.matrix(table(df$grupo, df$clase), ncol=3)
  
  tbl.df <- as.data.frame(cbind(tbl[, 1], tbl[, 2]))
  colnames(tbl.df) <- dimnames(tbl)[[2]]
  
  min.prob <- tapply(df$pm, df$grupo, min, simplify = TRUE)
  tbl.df <- cbind(tbl.df, min.prob[match(dimnames(min.prob)[[1]], rownames(tbl.df))])
  colnames(tbl.df)[length(colnames(tbl.df))] <- 'min.prob'
  
  max.prob <- tapply(df$pm, df$grupo, max, simplify = TRUE)
  MAX.max.prob <- max(max.prob)
  tbl.df <- cbind(tbl.df, max.prob[match(dimnames(max.prob)[[1]], rownames(tbl.df))])
  colnames(tbl.df)[length(colnames(tbl.df))] <- 'max.prob'
  
  tbl.df[order(-as.numeric(rownames(tbl.df))), 'N.acum'] <-
    cumsum(tbl.df[order(-as.numeric(rownames(tbl.df))), 1])
  tbl.df[order(-as.numeric(rownames(tbl.df))), 'S.acum'] <-
    cumsum(tbl.df[order(-as.numeric(rownames(tbl.df))), 2])
  
  tbl.df$N.acum2 <- tbl.df$N.acum / sum(tbl.df[[1]])
  tbl.df$S.acum2 <- tbl.df$S.acum / sum(tbl.df[[2]])
  
  # Agrego una ?ltima fila para generar el punto (0, 0)
  ## Lo hago en dos pasos porque me falla el rbind si los nombres de columnas no son iguales...
  ult.fila <- data.frame(0, 0, 
    min.prob = MAX.max.prob, max.prob = 1,
    N.acum = 0, S.acum = 0, N.acum2 = 0, S.acum2 = 0,
    row.names = as.vector(as.character(groups)))
  colnames(ult.fila) <- colnames(tbl.df)
  tbl.df <- rbind(tbl.df, ult.fila)
  
  AUC = -sum((((2 * tbl.df$S.acum2[-1]) - diff(tbl.df$S.acum2)) /2) * diff(tbl.df$N.acum2))
  color = switch(pos, 'blue', 'green', 'red', 'black', 'purple', 'yellow')
  
  if (pos == 1) {
    plot(tbl.df$N.acum2, tbl.df$S.acum2, type = 'b', pch=21, col=color, bg=color,
      xlim = c(0, 1), ylim = c(0, 1),
      xlab = '% buenos eliminados', ylab = '% malos eliminados',
      main = 'Curva ROC', lwd=lwd)
    lines(c(0, 1), c(0, 1), col = 'red', lty = 3)
  } else{
    par(new=TRUE)
    plot(tbl.df$N.acum2, tbl.df$S.acum2, type = 'b', pch=21, col=color, bg=color, 
      xlim = c(0, 1), ylim = c(0, 1), lwd=lwd,
      ann = FALSE, axes = FALSE)
  }
  text(0.6, pos / 10, paste(deparse(substitute(data)), ': AUC = ', round(AUC, round.AUX)), cex = 0.8, col = color)
  
  tbl.df
  
  return(AUC)
}
########################################## EXTERNAL functions #################################
