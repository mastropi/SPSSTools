# datamining-functions.r
# Created: 		  03-Jul-2013
# Modified: 	  31-Jul-2014
# Author: 		  Daniel Mastropietro
# Description: 	Set of Data Mining functions
# Dependencies: startup-functions.r, from which the following functions are used:
#               - checkVariables()
# R version:    R-2.8.0 (used in SPSS 18.0.0 @NAT starting 2013)
#

# INDEX:
# GroupCategories
# AssignCategories
# Profiles, DistributionsByGroup (these are the same function with 2 different names, the first is more easily remembered in data mining)
# InformationValue
# ScoreDistribution
# PrecisionRecall, F1
# ModelFit, model.fit
# roc

# HISTORY:
# - 2014/03/26: Created functions:
#								- F1(): computes the F1 value as taught at the Machine Learning course for binary target models.
#								- GroupCategories(): automatically groups contiguous categories of a categorical variable based on the value of a target variable.
#								- AssignCategories(): assign categories of a categorical variable to new grouped categories into a new variable based on the output
#									of GroupCategories().
# - 2014/03/30: Created function:
#								- ModelFit(), model.fit(): evaluates the model fit of a binary or continuous target by each input variable in a set of variables.
# - 2014/04/15: Updated function GroupCategories():
#								- Fixed the behaviour of the VERY PARTICULAR case when the LEFT category/group is too small and at the same time the RIGHT category/group
#								is in the exclusions list. See section "NOT MERGE: FINALIZE NEW GROUP" for how this was done. Note that the fix works both when
#								othergroup=TRUE and othergroup=FALSE.
# - 2014/05/05: Updated function PrecisionRecall(), F1():
#								- Extended the computation of precision and recall to a vector of cutoff values.
#								- Added a plot= option to show a plot of Recall vs. Precision.
# - 2014/05/19: Added function:
#								- roc(): generates ROC curve and computes AUC and AR/Gini.
#									The function was originally copied from Antoine Thibaud's roc.1(), to which I added some additional functionality.
#

# TODO:
# - 2015/01/16: GroupCategories() and AssignCategories():
#								- Make them work on a data frame and a set of analysis variables passed as strings.
#								- Remove trailing blanks from variable values when performing the analysis. I have seen variables having trailing blanks
#								when running these functions from R in SPSS (Ref: 03-Prepare.sps in Nextel project, folder 02-Otorgamiento)
#								- Add the option in GroupCategories() of generating an SPSS syntax for the RECODE so that when GroupCategories()
#								is run form R in SPSS, the user just needs to copy the syntax to run the grouping process, and optionally change the
#								assigned categories if they wish.
#
# - 2015/01/29: ModelFit():
#								- Re-write the signature so that it takes a MODEL as argument. That is, from the model information, the function should infer
#								the target variable, the input variables, which ones are continuous variables (i.e. the non-factor ones so that a model fit
#								plot makes sense) --although 0/1 binary variables might still be considered as continuous in case they were not specified
#								as factor variables by the user!
#


######################################## GroupCategories ######################################
# 2014/03/26
# [DONE-2014/03/28] UNDER DEVELOPMENT: The code below works but needs to be setup as a function. It was tested in the Moni project.
# The goal of this function is to automatically group contiguous categories of categorical variables when they are sorted by 
# the value of a target variable based on Chi-Square or t tests of 2x2 contingency tables formed by x
# (the categorical variable under analysis) and y (the target variable of interest, usually binary variable)
# The user should have the option of specifying particular categories that they want to leave alone (i.e. do not group with others)
# There is also a threshold of the number of cases (and percent of cases) that a category should have NOT to be grouped.
# Too small of categories should be placed together with the MEAN group (i.e. with the group having average target value) or 
# with a separate group called e.g. "Z-REST".
# Ref: Moni project -> 2 Modeling.r
#
# TODO:
# -[DONE-2014/04/02] 2014/03/26: Allow an option to assign categories with too few cases to the same "other" group.
# - 2014/03/26: Allow passing several input variables to analyze. In order to link this function with the AssignCategories() function,
# the output of such call should be a LIST (e.g. called groupedCat). See the use of groupedCat in AssignCategories() below.
# - 2014/04/02: Allow passing the value of an existing x category (e.g. "ZZZ-OTHER") where all categories with small size should be absorbed.
# -[DONE-2014/07/31] 2014/07/31: Extend the application of this function to categorical x values that have MISSING values (NA), so that missing values are treated
# as a different valid category. Currently, when running the function on an x with missing values, these values are removed, but I don't know
# where in the code!!
GroupCategories = function(
		x,									# Input categorical variable to analyze
		y,									# Target variable,
		decreasing=TRUE,		# How the x categories should be sorted for the analysis based on the y values. Either TRUE (by decreasing y), FALSE (by increasing y) or NA (alphabetical order)
		type="cat",					# Type of target variable y: "cat" (for categorical) or "cont" (for continuous). This affects the test that is used for significant differences among contiguous x categories.
		event="1",					# Event of interest of categorical variable y when type="cat"
		stat="mean",				# Statistics to use for the computation of the representative value of y for each category of x when type != "cat"
		na.rm=FALSE,				# Whether NAs in the x variable should be excluded from the analysis. Note that NAs in the y variable are still acceptable but are always ignored. (For more information of this treatment, see help(table))
		varname=NULL,				# Name of the analyzed variable (useful when calling this function from within a FOR loop where x is passed as tofit[,v], where e.g. v = "x1"
		# Settings for merging consecutive categories
		pthr=c(0.50,0.10),	# Threshold for the p-value of the Chi-square test or t test that is used to decide whether contiguous categories are merged.
												# Defaults to 0.5 for a categorical target and to 0.1 for continuous target.
		propthr=0.01,				# Minimum proportion of cases (w.r.t. to total number of cases in dataset) to be observed in a category so that it can be let alone
		nthr=20,						# Minimum number of cases in a category so that the category can be let alone
		othergroup=TRUE,		# Whether categories with too few cases (n < nthr) should be sent to the "other" group or instead joined to the category of the LEFT.
		exclusions=NULL,		# Vector of categories to be excluded from the merge (they should be left alone)
												# *** NOTE: exclusions COULD ALSO BE WISHED TO BE ASSIGNED TO A SINGLE GROUP CALLED "other" (note the small caps becase capital letters come before non-capital letters in the ASCII coding!) ***
		# Output settings
		print=TRUE,					# Whether to print messages about the merging of the categories and its properties
		plot=TRUE,					# Whether to plot the evolution of the merging
		cex=0.8,						# Character expansion factor for annotations in the plots except for the cex.names option of barplot()
		cex.names=0.6				# Character expansion factor for the names (values of categorical variable) of the barplot
)
# Created:			27-Mar-2014
# Modified:			09-Feb-2015
# Author:				Daniel Mastropietro
# Description:	Automatically group contiguous categories of a categorical variable based on the value of a target variable
#								and a chi-square statistic.
# Parameters:		(See above)
# Output:				A list with the following 2 elements:
#								- $bars: the data of the original barplot of y vs. x
#								- $outbars: the data of the new barplot of y vs. (x after grouping by the algorithm).
# Examples:			GroupCategories(x, y, pthr=0.5, exclusions=c("ZZ-OTHER", "ZZZ-UNKNOWN"))
{
	#--------------------------------- Auxiliary functions --------------------------------------
	# Test the difference between contiguous groups in terms of target variable
	# - for categorical target: chi-square test
	# - for continuous target: t test
	fxTestDiff = function(x, type)
	# Input: x: a matrix with 2 rows and the columns needed for testing.
	#						In the categorical case these columns are the counts of non-event and event of interest
	#						In the continuous case these columns are the mean of the target variable, the counts n and the standard error .
	# 			 type: the type of the target variable defining the test to perform:
	#						when "cat": chi-square test (categorical target)
	#						o.w.: one-sided t test (contintuous target)
	# Output: a vector containing the updated columns of the input x
	{
		if (type == "cat") {
			# Do a chi-square test of independence between the two rows of x and the value of the target variable
			chisqt = chisq.test(x, correct=TRUE)		# correct=TRUE implies to do a correction for continuity
			pvalue = chisqt$p.value
		} else {
			# Compute a one-sided p-value for the t test to compare the mean values of the two rows in x
			# The t test is one-sided because the categories are assumed sorted from larger value of the target variable to smaller value.
			# Note that the t test is performed assuming that the statistic summarizing the target variable in each category is the mean
			mu1 = x[1,colmean]; std1 = x[1,colstd]; n1 = x[1,coln]
			mu2 = x[2,colmean]; std2 = x[2,colstd]; n2 = x[2,coln]
			# Standard Deviation of the two samples (pooled variance) and standard error of the difference of means
			std = sqrt( ((n1-1)*std1^2 + (n2-1)*std2^2) / (n1 + n2 - 2) )
			se = std * sqrt(1/n1 + 1/n2)
			df = n1 + n2 - 2
			pvalue = 1 - pt((mu1 - mu2)/se, df)
		} 
		
		return(pvalue)
	}
	
	# Collapse contiguous groups
	fxCollapseGroups = function(x, type)
	# Input: x: a matrix with 2 rows and the columns to update after collapsing
	# 			 type: the type of collapsing to do: "cat" for categorical target variable; any other value for continuous target variable
	# Output: a vector containing the updated columns of the input x
	{
		# Initialize the output of the function
		xout = array(NA, dim=ncol(x)); dimnames(xout) = list(colnames(x))		# dimnames of an array should be defined as a a list...(!)
		
		if (type == "cat") {
			# Sum vertically
			xout = apply(x, 2, FUN=sum)
		} else {
			# Update mean of y by doing a weighted sum (by the number of cases in each group being merged)
			# (note that we are summing vectors and we don't need apply())
			xout[colmean] = sum(x[,colmean] * x[,coln]) / sum(x[,coln])
			# Update the standard error of mean(y) (assuming pooled variance, as: se = sqrt( (std1^2*n1 + std2^2*n2 ) / (n1 + n2) )
			# (note that we are summing vectors and we don't need apply())
			xout[colstd] = sqrt( sum(x[,colstd]^2 * (x[,coln]-1)) / (sum(x[,coln]) - 2) )
			# Compute the counts in the new group
			# (note that we are summing vectors and we don't need apply())
			xout[coln] = sum(x[,coln])
		}

		return(xout)
	}

	# Plot the updated bar plot. I define it as a function because it is called twice from the main function
	fxPlot = function(x, type)
	# Input: x: a matrix where the row names are the different x categories to plot and 
	# 			 type: the type of the target variable defining how to compute the height of the bars to plot:
	#						when "cat": height = proportion of events (categorical target)
	#						o.w.: representative value of the target in each category (e.g. mean) (contintuous target)
	# 			 type: the type of collapsing to do: "cat" for categorical target variable; any other value for continuous target variable
	# Output: none
	{
		if (type == "cat") {
			height = prop.table(x[,col4ncases,drop=FALSE], margin=1)[,2]
			width = apply(x[,col4ncases,drop=FALSE], 1, sum)
		} else {
			height = x[,colmean]
			width = x[,coln]
		}
		barpos = barplot(height, width=width,
										 horiz=FALSE, las=3,
										 main=paste("Step", i, "; p-value", asterisk.pvalue, "=", formatC(pvalue, digits=3), "; n", asterisk.ncases, "= (", ncases.left, ",", ncases.right, ")"),
										 cex.names=cex.names,
										 cex.main=cex,
										 cex=cex)
		text(barpos, height, labels=width, offset=0.5, pos=1, cex=cex)
	}
	#--------------------------------- Auxiliary functions --------------------------------------


	#---------------------------------- Graphical settings --------------------------------------
	opar = par(no.readonly=TRUE)
	on.exit(par(opar))
	par(mar=c(5.5,2,2,1), xpd=TRUE)		# xpd=TRUE => clip all the plotting to the figure region (as opposed to the plot region) so that labels showing the number of cases in each bar are always seen)
	#---------------------------------- Graphical settings --------------------------------------


	#---------------------- Initial bar plot and threshold definition ---------------------------
	# Compute and plot (if requested) the barplot that represents the average target variable vs. the categorical variable 
	if (type == "cat") {
		FUN = "table"
		if (missing(pthr)) pthr = 0.50
	} else {
		FUN = stat
		if (missing(pthr)) pthr = 0.10
	}
	bars = plot.bar(as.vector(x), as.vector(y),
									event=event, na.rm=na.rm, FUN=FUN, las=3,
									decreasing=decreasing,
									main=paste("Initial categories of variable", ifelse(!is.null(varname), varname, deparse(substitute(x)))),
									cex.names=cex.names,
									cex.main=cex,
									cex.axis=cex,
									plot=plot)

	# Compute the final minimum number of cases to let a category on its own
	cat("Threshold for the minimum number of cases per category (based on parameters nthr ( =", nthr)
	nthr = ceiling(max(nthr, propthr*sum(bars[,"n"])))
  cat(" ) and propthr ( =", propthr, ")):", nthr, "\n")
	#---------------------- Initial bar plot and threshold definition ---------------------------


	#--------------------------------- Merging process ------------------------------------------
	### Initializations
	# Initialize the output matrix xout
	if (type == "cat") {
		# Keep the necessary columns of 'bars' for the processing
		xout = bars[,1:2]
			## 'bars', the output of plot.bar() above, is assumed to have the following columns:
			## 1: count of non-event cases (of y)
			## 2: count of event cases (of y)
			## 3: proportion of non-event cases (of y)
			## 4: proportion of event cases (of y)
			## 5: count of non-missing cases in each category
			## 6: standard error of the proportion of both non-event and event cases (of y)
		col4ncases = 1:2		# Columns in xout containing the values to sum to get the total number of cases per xout category/group
												# These contain the counts of the two categories of the target variable resulting from the plot.bar() function:
												# non-'event' and 'event'. Ex: typically 0 and 1
												# Note that the target variable may have more than 2 categories but it is collapsed to 2 categories of interest
												# by the plot.bar() function above.
		if (print) {
			cat("The target variable is considered categorical.\n")
			cat("A chi-square test is used to compute the p-value for whether differences exist between contiguous categories of the input variable in terms of the target variable.\n")
		}
	} else {
		# Set the row names of bars to be the x categories
		# (this is important for the final list of the groups obtained which uses rownames(bars))
		rownames(bars) = bars[,1]

		# Keep the necessary columns of 'bars' for the processing
		xout = bars[,2:ncol(bars)]
			## 'bars', the output of plot.bar() above, is assumed to have the following columns:
			## 1: x categories values
			## 2: "mean" of y
			## 3: count of non-missing cases in each category
			## 4: standard deviation of y (i.e. NOT the standard error of the mean but the standard deviation of the values!)
		colnames(xout)[1] = "mean"
		colmean = "mean"		# Name of the column in xout containing the (assumed) mean value of y for the t test
												# It should be the mean of y for each x category, although any statistic is accepted and a warning is issued if it is not the mean.
		coln = "n"					# Name of the column in xout containing the counts in each x category
		colstd = "sd"				# Name of the column in xout containing the standard deviation of y for each x category
		col4ncases = coln		# Columns of xout containing the values to sum to get the total number of cases per xout category/group
												# In the continuous target case, this is just one column containing the category/group counts and stored in xout[,coln]
		if (FUN != "mean") {
			# Issue a warning when the statistic passed in 'stat' is NOT "mean", since still the one-sided t-test p-value is computed
			# and used to decide whether contiguous categories should be merged.
			cat("GROUPCATEGORIES: WARNING - The statistic computed of the target variable for each category of the indendent variable is NOT the mean\n")
			cat("The p-value for the one-sided t-test is still computed to analyze if differences exist.\n")
			cat("This test is supposed to be valid only when the statistic is the mean.\n")
		} else if (print) {
			cat("The target variable is considered continuous.\n")
			cat("A one-sided t test is used to compute the p-value for whether differences exist between contiguous categories of the input variable in terms of the target variable.\n")
			cat("A pooled variance approach is used for the variance of the difference and to determine the degrees of freedom of the test.\n")
		}
	}
	cat("\nTwo contiguous groups are merged when the p-value of test is >=", pthr, "\n")
	if (othergroup) {
		cat("Categories with too few cases (n <", nthr, ") are put together into a separate 'other' group.\n")
	} else {
		cat("Categories with too few cases (n <", nthr, ") are merged to the category to the LEFT.\n")
	}
	
	# Initialize arrays needed for the process
	categories = rownames(xout)							# Store the original x categories
	groups = array(NA, dim=nrow(bars))			# There will be at most as many merged groups as the nro. of categories in the original x variable.
	pvalues = array(NA, dim=nrow(bars)-1)		# There will be at most (no. categories - 1) p-values for the tests of difference between contiguous groups
	i = 1																		# i stores the index corresponding to the last x category present in the LEFT category/group being analyzed for potential merging
	i4test = 1															# i4test contains the index for the pvalues array.
																					# Its value is always equal to i but I define it as a separate variable to increase code clarity.
	j = 1																		# j stores the index of xout corresponding to the LEFT category/group being analyzed for potential merging
	groups[1] = 1														# Value initially assigned to the first output group

	# Initialize the "other" group (only used when othergroup = TRUE)
	# Create 'xother', a vector of length equal to the number of columns of xout to store the characteristics of the "other" group
	# where all categories with too small number of cases are grouped (when othergroup = TRUE)
	# The rownames of 'xother' will store the list of the original x categories that go to the new "other" category,
	# which during the process is temporarily store in variable 'ogroups'
	xother = array(0, dim=ncol(xout)); dimnames(xother) = list(colnames(xout))		# dimnames of an array are defined as a list...(!)
	ogroups = NULL		# Variable to temporarily store the list of categories that fall into the new group "other"
	
	### Start looping on the rows of the matrix of new groups 'xout'
	while (j < nrow(xout)) {
		# Left and right x categories being compared
		# In general, the left category may be the last x category existing in the GROUP to the left (when merges have already occurred).
		# The right category is defined as the x category that coincides with the next new group value (stored in rownames(xout)[j+1]
		ileft = i
		iright = which(categories==rownames(xout)[j+1])

		# Assign a value to RIGHT category/group of the xout output matrix (groups[j+1])
		# This category/group could potentially be merged with the LEFT category/group (groups[j]) or o.w. sent to the "other" group.
		# Currently (2014/04/02) the next output category/group is just an x category (as observed by looking at the value assigned
		# to groups[j+1], which is iright, computed above), but I store this value in groups[j+1] forseeing a future development
		# where one could merge starting from both ends and stop at the mid-position categories
		# (this might improve the identification of different groups...?)
		groups[j+1] = iright

		# Test the difference between contiguous xout categories/groups in terms of the target variable
		pvalues[i4test] = fxTestDiff(xout[j:(j+1),], type)
		# Store the current p-value to be shown later at the plot title
		pvalue = pvalues[i4test]

    # Number of cases in currently analyzed category (or grouped categories)
    ncases.left  = sum(xout[j,  col4ncases])		# Number of cases in LEFT group
    ncases.right = sum(xout[j+1,col4ncases])		# Number of cases in RIGHT group

    # Evaluate what conditions are satisfied for grouping
    # (either p-value is too large or ncases.left or ncases.right is too small when parameter othergroup = TRUE)
    cond.pvalue = as.logical(min(pvalues[i] >= pthr, TRUE, na.rm=TRUE)); asterisk.pvalue = ifelse(cond.pvalue, "*", "")
    	## The min() function to compute cond.pvalue is used to avoid problems when cond.pvalue is NA (which happens rarely when the chisq.test() above fails) 
    cond.ncases = ncases.left < nthr | ncases.right < nthr; asterisk.ncases = ifelse(cond.ncases, "*", "")
    cond.exclusion.left  = categories[ileft]  %in% exclusions 
    cond.exclusion.right = categories[iright] %in% exclusions 

		if (print) {
			catstr = paste("Comparing x categories", ileft, "and", iright, ":")
			tabstr = paste(rep(" ", nchar(catstr)), collapse="")
			cat(catstr, " pvalue =",  formatC(pvalue, digits=3), "; n =", ncases.left, ",", ncases.right)
		}

		# MERGE OR NOT MERGE?
		if ((cond.pvalue | cond.ncases) & !(cond.exclusion.left | cond.exclusion.right)) {
			# Entering here means that the RIGHT category of x (iright) should be merged into a new group,
			# either to the "other" group or to the LEFT category of x being analyzed (ileft). 
			if (othergroup & cond.ncases) {
				#----------------------- SEND CATEGORIES TO THE "OTHER" GROUP -------------------------
				# Add the category/ies containing a small number of cases to xother and update the string 'ogroups'
				# containing the indices of the x categories that are sent to the "other" group
				ind2remove = NULL
				if (ncases.left < nthr) {
					if (print) cat("\n", tabstr, "--> LEFT category sent to OTHER group because size <", nthr, ":", rownames(xout)[j])
					# Collapse the LEFT category/group to the "other" group 
					xother = fxCollapseGroups(rbind(xother, xout[j,]), type)
					ogroups = paste(ogroups, groups[j], sep=", ")
					# Add row j to the vector of row indices to be removed from xout
					ind2remove = j
					# Go to the next x category since the current i category was sent to the "other" group
					i = iright
					# In this case the index for pvalues should be i (OK)
					i4test = i
					# Update the groups array to reflect that the group to process in the next loop, groups[j], is the next x category
					# (which is now 'i', NOT 'i+1' since i has just been updated)
					groups[j] = i
				}
				if (ncases.right < nthr) {
					if (print) cat("\n", tabstr, "--> RIGHT category sent to OTHER group because size <", nthr, ":", rownames(xout)[j+1])
					# Collapse the RIGHT category/group to the "other" group 
					xother = fxCollapseGroups(rbind(xother, xout[j+1,]), type)
					ogroups = paste(ogroups, groups[j+1], sep=", ")
					# Add row j+1 to the vector of row indices to be removed from xout
					ind2remove = c(ind2remove, j+1)
					# Check if the previous group was also sent to the "other" group
					# (this should only happen for the first (leftmost) group, since the LEFT group is either large enough
					# or has been obtained by merging, operation that makes it even larger)
					if (ncases.left < nthr) {
						# If the previous group was also sent to the "other" group, it means that there is no longer a "previous" group
						# to process in the next loop. In this case we should:
						# - Indicate that the x category to process in the next loop is the x category that is to the right of the category
						#   just sent to the "other" group => i = i + 1 (sum +1 and not +2 because i has been updated in the previous IF clause)
						# - Update the value of the group representing the left category to be analyzed in the next loop
						# 	to such x category => groups[j] = i (NOT i+1 because i has just been updated to i+1)
						i = i + 1				# NOTE that in this case we should do i = i + 1, as opposed to i = iright (as in all other cases)
														# because iright contains the value of i before updating since this i is already the RIGHT category
														# being analyzed.
						groups[j] = i
					}
					# Store the p-value for the test between the x category analyzed in this loop and the category just sent to the "other" group
					# so that it is stored in the output table xout to return.
					pvalues[i4test+1] = pvalue
					# In this case the index for pvalues should be set to i-1 (because the next test will be between the i-th x category PRIOR to the update of i and the next x category)
					i4test = i
				}
				# Remove the rows that were sent to the "other" group from xout since they should no longer be processed
				# Note that j should NOT be updated after removing these rows...!
				xout = xout[-ind2remove,,drop=FALSE]
				if (print) cat("\n")
			} else {
				#------------------------------ MERGE CONTIGUOUS GROUPS -------------------------------
				if (print) {
					if (cond.ncases) {
						cat("\n", tabstr, "--> Categories MERGED based on small group size(s) <", nthr, ":", rownames(xout)[j], ";", rownames(xout)[j+1], "\n")
					} else if (cond.pvalue) {
						cat("\n", tabstr, "--> Categories MERGED based on p-value >=", pthr, ":", rownames(xout)[j], ";", rownames(xout)[j+1], "\n")
					}
				}
				# Name the new group that results from merging the 2 contiguous categories/groups
				# (these groups are groups[j] which, if not resulting from an earlier merge, should be the i-th category of x
				# and groups[j+1], which normaly is just the iright-th category of x)
				groups[j] = paste(groups[j], groups[j+1], sep=", ")
				# Collapse rows j and j+1
				xout[j,] = fxCollapseGroups(xout[j:(j+1),], type)
				rownames(xout)[j] = groups[j]
				# Shift all remaining rows one row up
				if (j+2 <= nrow(xout)) {
					xout = xout[c(1:j, (j+2):nrow(xout)),,drop=FALSE]
				} else {
					xout = xout[1:j,,drop=FALSE]
					## Note the use of drop=FALSE so that xout is still a matrix even when this subsetting from 1:j makes it have only one row (in case j = 1 and nrow(xout) = 2 prior to merging the categories)
				}

				# Set the next LEFT category to be the current RIGHT category of x
				# (note that the RIGHT category may NOT be obtained by doing (LEFT category index)+1... as there could have been categories in between that were sent to the "other" group)
				i = iright
				i4test = i
			}
		} else {	# MERGE OR NOT MERGE?
			#--------------------------- NOT MERGE: FINALIZE NEW GROUP (unless the left group is too small) ------------------------------
			finalize = TRUE
			# Before deciding to directly finalize the new group, check whether we ended up here because of EXCLUSIONS
			# and in addition the LEFT category/group being analyzed is too small and should be sent to the "other" group or
			# (when othergroup=FALSE) merged to the category/group coming to the right of the current RIGHT category/group.
			# (if the latter is the case, the current LEFT category/group is actually switched with the RIGHT category/group so
			# that it is analyzed on the next iteration)
			if ((cond.exclusion.right & !cond.exclusion.left) & cond.ncases & ncases.left < nthr) {
				if (othergroup) {
					# Send the left category to the "other" group
					# (this section is a copy of the first 3 statements of the section above where the left category is sent to the "other" group)
					# (the last 4 statements are not needed here)
					if (print) cat("\n", tabstr, "--> LEFT category sent to OTHER group because size <", nthr, ":", rownames(xout)[j], "\n")
					# Collapse the LEFT category/group to the "other" group 
					xother = fxCollapseGroups(rbind(xother, xout[j,]), type)
					ogroups = paste(ogroups, groups[j], sep=", ")
					# Remove the j row of xout since it was just sent to the "other" group and update groups[j] with groups[j+1]
					xout = xout[-j,,drop=FALSE]
					groups[j] = groups[j+1]
					## *** Note that j should NOT be updated after removing the j-th row! 	 ***
					## *** (because the j-th row of xout was just sent to the "other" group) ***
					# Set finalize to FALSE because the current j group should NOT be finalized (since it was just sent to the "other" group)
					finalize = FALSE
				} else {
					# Switch the j row of xout with the j+1 row because the j+1 row is under the exclusion list and the j row should be merged with the
					# next category/group that comes to the right of the current RIGHT category/group.
					if (print) cat("\n", tabstr, "--> LEFT and RIGHT categories SWITCHED because of small left group size <", nthr, "and right group exclusion:", rownames(xout)[j], ";", rownames(xout)[j+1], "\n")
					aux = xout[j,]
					xout[j,] = xout[j+1,]
					xout[j+1,] = aux
					# Do the same switching with the groups[j] and groups[j+1] values
					aux = groups[j]
					groups[j] = groups[j+1]
					groups[j+1] = aux
					# Update iright so that it is correctly assigned to the i value to be used for the next loop below:
					# iright should take the value of the last x category present in the NOW (j+1)-th group, groups[j+1] (which contains the former j-th group)
					pos = max(unlist(gregexpr(", ", groups[j+1])))
						## This is the last position of occurrence of a comma. If no comma is found, then -1 is returned,
						## which is quite handy because I retrieve the number starting at position pos+2 (great!)
						## Note that for this to work in the special case when no comma is found, the separator should EXACTLY be of length 2
						## (so that -1 + 2 = 1, the first character of groups[j+1] when the group is not the result of merging x categories
						## --i.e when no commas are found using gregexpr() above)
					iright = as.numeric( substring(groups[j+1], pos+2) )
				}
			} else {
				# No merging should be carried out nor any category should be sent to the "other" group nor switched with the current RIGHT category/group.
				# This ELSE statement only contains strings to be printed out (when requested by parameter 'print')
				if (print) {
					if (cond.exclusion.left) {
						cat("\n", tabstr, "--> Categories NOT merged:", rownames(xout)[j], "(exclusion) ;", rownames(xout)[j+1], "\n")
					} else if (cond.exclusion.right) {
						cat("\n", tabstr, "--> Categories NOT merged:", rownames(xout)[j], ";", rownames(xout)[j+1], "(exclusion)\n")
					} else {
						cat("\n", tabstr, "--> Categories NOT merged:", rownames(xout)[j], ";", rownames(xout)[j+1], "\n")
					}
				}
			}

			if (finalize) {
				# If finalize=TRUE, a new group has been defined and I can finalize it in xout with its full name
				# (specifying which original x categories are part of it)
				rownames(xout)[j] = groups[j]
				# The current new group is finalized and we can now go to the next group => increment j by 1
				# Note that after updating j, groups[j] will have the value of groups[j+1] prior to updating j, which already had a value.
				# Therefore groups[j] is NEVER missing.
				j = j + 1
			}

			# Set the LEFT category for the next loop to be the current RIGHT category of x
			# (note that the RIGHT category may NOT be obtained by doing (LEFT category index)+1... as there could have been categories in between that were sent to the "other" group)
			i = iright
			i4test = i
		}

		# Check if the current loop is the last loop of the process
		# Note that this may happen because:
		# (a) j has been just updated and now points to the last row of xout (from "NOT MERGE: FINALIZE NEW GROUP" section above)
		# (b) all remaining rows of xout have been either eliminated or collapsed (from the other possible section above)
		# In that case, do the following depending on whether the situation is (a) or (b)
		# (a) check if the size of the last category/group in xout is large enough (compared to nthr) or should be merged with the previous group
		# (b) assign the correct value for the last category/group in xout (showing which indices of the original x categories include)
		# 		(note that this "correct value" is not assigned when the last group left in xout has NOT been merged and the last
		#			 x category was sent to the "other" group because it is too small)
		if (j == nrow(xout)) {
			jlast = j
			ncases = sum(xout[jlast,col4ncases])		# Sum horizontally
			# Only cond.ncases (on the last category) is computed here because cond.pvalue cannot be TRUE at this last category
			cond.ncases = ncases < nthr; asterisk.ncases = ifelse(cond.ncases, "*", "")
			# Check if any of the two last groups are in the exclusions list
	    cond.exclusion.left  = categories[ileft]  %in% exclusions 
	    cond.exclusion.right = categories[iright] %in% exclusions 
			if (cond.ncases & !(cond.exclusion.left | cond.exclusion.right)) {
				groups[jlast-1] = paste(groups[jlast-1], groups[jlast], sep=", ")
				# Collapse rows jlast-1 and jlast
				xout[jlast-1,] = fxCollapseGroups(xout[(jlast-1):jlast,], type)
				rownames(xout)[jlast-1] = groups[jlast-1]
				# Eliminate the last row of xout because it has just been merged
				xout = xout[1:(nrow(xout)-1),,drop=FALSE]
			} else {
				# When the last category is large enough leave it on its own
				rownames(xout)[jlast] = groups[jlast]
			}
		}
		if (plot) {
			if (any(xother>0)) {
				fxPlot(rbind(xout, xother), type)
			} else {
				fxPlot(xout, type)
			}
		}
	}
	# To finalize, place the "other" group as the last row of xout, if it has absorbed any of the original x categories
	if (any(xother>0)) {
		# Convert xother from array to matrix in order to assign row names
		xother = t(as.matrix(xother))
		# Store the content of 'ogroups' as rownames of 'xother'
		# (note that I start considering the character at position 3 of 'ogroups' because the first two characters are ', '
		# because of the algorithm used above when constructing its value --so that I avoid checking whether it is the first category
		# being added to the ogroups string)
		rownames(xother) = substring(ogroups,3)
		# Add the xother group to xout
		xout = rbind(xout, xother)
	}

	### Add the p-value information to the output dataset and display how the new groups are generated
	# Extend the columns of xout
	ncols = ncol(xout)
	xout = cbind(xout, matrix(nrow=nrow(xout), ncol=length(pvalues)))
	colnames(xout)[(ncols+1):ncol(xout)] = paste("p", 1:(ncol(xout)-ncols), sep="")
	indmax = 0
	subscripts = rownames(xout)
  cat("\nGrouping of variable",  ifelse(!is.null(varname), varname, deparse(substitute(x))), ":\n")
	for (j in 1:nrow(xout)) {
		ind = eval(parse(text=paste("c(", subscripts[j], ")", sep="")))
		# Categories in the current group
		xvalues = paste("'", paste(rownames(bars)[ind], collapse="', '"), "'", sep="")
			## Note that the first and last double quotes are NOT part of xvalues, they are shown because 'xvalues' is of type 'character'.
		# Number of cases and representative value of the y variable for each group (to print out below)
		if (type == "cat") {
			nvalue = sum(xout[j,col4ncases])
			yvalue = prop.table(xout[j,col4ncases])[2]
		} else {
			nvalue = xout[j,coln]
			yvalue = xout[j,colmean]
		}
		if (any(xother>0) & j == nrow(xout)) {
			cat("Group (OTHER) ( ncat =", length(ind), ", n =", nvalue, ", mean =", formatC(yvalue, format="g", digits=3), "):", xvalues, "\n")
		} else {
			cat("Group", j, "( ncat =", length(ind), ", n =", nvalue, ", mean =", formatC(yvalue, format="g", digits=3), "):", xvalues, "\n")
		}
		# Define the indices for retrieving the p-values of the merges or non-merges
		if (j == nrow(xout) & !any(xother>0)) {
			# When there is no "other" group, there is no p-value for the last x category, therefore eliminate it from 'ind'
			# the array of indices on which to subset the array pvaalues below.
			ind = ind[1:(length(ind)-1)]
		}
		indmax = max(indmax, length(ind))
		# Update xout
		xout[j,ncols+1:length(ind)] = pvalues[ind]
	}
	# Keep only the columns with non-missing pvalues 
	xout = xout[,1:(ncols+indmax),drop=FALSE]
  	## Note the use of drop=FALSE so that xout is still a matrix even when it has only one row.

	# Final grouping
	if (plot) {
		# When the "other" group exists (at the last row of xout) change temporarily its name for plotting to reflect that is the "other" group
		if (any(xother>0)) {
			rownames(xout)[nrow(xout)] = paste(rownames(xout)[nrow(xout)], "(OTHER)")
		}
		if (type == "cat") {
			toplot = cbind(prop.table(xout[,1:2,drop=FALSE], margin=1), n=apply(xout[,1:2,drop=FALSE], 1, sum))
			toplot = cbind(toplot, sd=sqrt(toplot[,"1"]*(1 - toplot[,"1"])))
		} else {
			toplot = cbind(rownames(xout), xout[,c(colmean,coln,colstd)])
		}
		plot.bar(toplot, FUN=FUN, main=paste("Final grouping (", ifelse(!is.null(varname), varname, deparse(substitute(x))), ")", sep=""), cex.names=cex.names, cex.main=cex, cex.axis=cex, las=3)

		# Remove the "(OTHER)" keyword from the rownames of xout because this should contain just numbers referring to the indices
		# of the original x categories! (otherwise for example running AssignCategories() on this output will fail)
		if (any(xother>0)) {
			rownames(xout)[nrow(xout)] = gsub("\\(OTHER\\)", "", rownames(xout)[nrow(xout)])
		}
	}

	return(list(bars=bars, outbars=xout))
}
######################################## GroupCategories ######################################



####################################### AssignCategories ######################################
AssignCategories = function(dat, vars, newvars, newvalues, groupedCat)
# Created:			27-Mar-2014
# Author:				Daniel Mastropietro
# Description:	Create new categorical variables in a dataset which come from grouping of categories of old variables
# Parameters:		- dat: dataset to modify
#								- vars: string or array with the ORIGINAL variable names
#								- newvars: string or array with the NEW variable names
#								- newvalues: List of vectors containing new values to be assigned. The structure of this list is as follows:
#														 - each list element contains one vector and indexes a different variable listed in 'vars'
#														 - the vector in each list element has the following properties:
#																- its length is equal to the number of new categories to create in the corresponding variable listed in 'newvars'
#																- it contains the values to use to represent the new categories to be stored in the corresponding variable listed in 'newvars'.
#								- groupedCat: List (that MUST be indexed by integer numbers 1, 2, ...) each element of which contains the result of
#									applying the GroupCategories() function to each variable in vars.
# Output:				The dataset 'dat' is returned containing the newly created variables specified in 'newvars'.
# Examples:			Example 1:
#									vars.gc = list()
#									vars.gc[[1]] = GroupCategories(tofit$x, tofit$y)										# Categorize variable x in terms of target y
#									vars.gc[[2]] = GroupCategories(tofit$z, tofit$y)										# Categorize varibale z in terms of target y
#									newvalues[[1]] = c(10, 20, 999)																			# Categories of the new categorized variable x_cat
#									newvalues[[2]] = c("1-GRP3", "2-GRP5", "ZZ-OTHER", "ZZZ-UNKNOWN")		# Categories of the new categorized variable z_cat
#									tofit = AssignCategories(tofit, "x z", "x_cat z_cat", newvalues, vars.gc)
{
	# Check if the variables passed were passed as arrays (length>1) or strings (length=1)
	if (length(vars) == 1) { vars = unlist(strsplit(vars,"[ \n]")) }
	if (length(newvars) == 1) { newvars = unlist(strsplit(newvars,"[ \n]")) }
	
	i = 0
	for (v in vars) {
		i = i + 1
		newv = newvars[i]
		cat("*** Variable", i, "of", length(vars), ":", v, "--->", newv, "***\n")
		subscripts = rownames(groupedCat[[i]]$outbars)
		names = rownames(groupedCat[[i]]$bars)
		dat[,newv] = as.character(dat[,v])  # remove the factor attribute from v (o.w. the original levels are kept in newv)
		for (j in 1:nrow(groupedCat[[i]]$outbars)) {
			ind = eval(parse(text=paste("c(", subscripts[j], ")", sep="")))
			# Categories in the current group
			names4group = names[ind]
			#  if (newvalues[j] == "Z-REST") { newvalues[j] = paste(newvalues[j], length(ind), sep="") }
			dat[dat[,v] %in% names4group, newv] = newvalues[[i]][j]
			# Show the grouping being carried out.
			# (note: for correct alignment of the original group values, it is important to:
			# 	- add a \t before the paste(names4group...)
			#		- add a space as final character in the collapse= option
			#		- add a space between \t and ---> in the final cat() function
			cat("Group", j, "(", length(ind), "):\n\t", paste(names4group, collapse="\n\t "), "\n")
			cat("\t ---> assigned to:", newvalues[[i]][j], "\n\n")
		}
	}
	
	return(dat)
}
####################################### AssignCategories ######################################



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
#								Possible functions and packages to use for this:
#								- ddply() in plyr package: note the functions summarize and mutate available to be called as aggregation functions.
#								Note that this function allows for aggregating several variables on multiple statistics on the same go. However,
#								the problem is that there seems to be no automatic naming for the aggregated variables (like in aggregate)
#								(ex from the documentation: ddply(dfx, .(group, sex), summarize, mean = mean(age), sd = sd(age)))
#								Ref paper: http://www.jstatsoft.org/v40/i01/
#								- data frame management with data.table package
#								For examples see this post by Carlos Gil Bellosta:
#								http://www.datanalytics.com/2014/03/25/totales-agregados-por-bloques-en-tablas/
InformationValue = function(
  data,             # Dataset containing the variables to analyze listed in 'vars', 'varclass' and/or 'varnum
  target,           # Either an unquoted variable name or a string indicating the name of the binary target variable in 'data' on which the Information Value is computed
  vars=NULL,        # Blank- or line-separated string with the names of the variables to analyze (they are assumed to be continuous)
  varclass=NULL,    # Blank- or line-separated string with the names of the CATEGORICAL variables to analyze
  varnum=NULL,      # Blank- or line-separated string with the names of the CONTINUOUS  variables to analyze (these variables together with those specified in VARS determine the continuous variables to analyze)
  groups=20,        # Number of groups into which the continuous variables are binned (equal-size bins)
  breaks=NULL,      # Vector defining the upper boundaries of the bins into which ALL CONTINUOUS variables are binned
  stat="mean",      # Statistic to compute on each bin of the analyzed continuous variables
  event="LAST",     # Event of interest ("FIRST" or "LAST") which is used as numerator in the WOE formula
  spsscode=FALSE)		# Whether to include the SPSS code to create WOE variables as part of the output
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
	# Return the type of a variable in a data frame (character or numeric)
	getVarType = function(x)
	{
#		# OLD way of retrieving the variable type
#    if ( is.na( max( as.numeric(as.character(x[!is.na(x)])) ) ) ) { vartype = "character" } else { vartype = "numeric" }

		# Check if the variable is factor otherwise, typeof() returns numeric!! (since factor levels are converted or stored as numbers)
 		if (is.factor(x)) {
 			vartype = typeof(levels(x))
 		} else {
 			vartype = typeof(x)
 		}
 		
 		return(vartype)
	}
	
	# Return the lower and upper bound of intervals of the type (x1,x2] or [x1,x2), etc. and also the brackets
	# (Inspired by the midpoints() function published on Matt's Stats around May-2014)
	getBounds <- function(x)
	{
		x = as.character(x)
		left <- substr(x,1,1); 								opleft = ""; 	if (left == "(") { opleft = "<" } else if (left == "[") { opleft = "<=" }
		right <- substr(x,nchar(x),nchar(x)); opright = ""; if (right == ")") { opright = "<" } else if (right == "]") { opright = "<=" }
		lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
		upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
		return(list(lower=lower, upper=upper, left=left, right=right, opleft=opleft, opright=opright))
	}
	
	iv = function(y, group, x=NULL, event="LAST", stat="mean")
	{
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
  # NOTE that unlist(strstplit()) also works when the argument is already a vector!!! GREAT!!
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
  # Store the var types (character or numeric) in a list (used at least for the SPSS code generation)
  # Note that this information needs to be stored in a list (and not an array) because it's need to be indexed by the variable names!
  # (since the order of processing of the variables may be different from the one defined by the array c(varclass, varnum)!
  vartypes = list()
  cat("Information Value calculation on target variable", targetname, "\n")
  for (v in varclass) {
    i = i + 1
    cat("\t", i, ": Computing Information Value for categorical variable", v, "\n")
    
    # Store the variable type (character or numeric)
    # THIS IS VERY COMPLICATED TO DO SO I STORED THE PROCESS IN A FUNCTION!!
		vartypes[[v]] = getVarType(data[,v])

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

    # Store the variable type (character or numeric)
    # THIS IS VERY COMPLICATED TO DO SO I STORED THE PROCESS IN A FUNCTION!!
		vartypes[[v]] = getVarType(data[,v])

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
  
  # Create SPSS code for creating WOE variables
  if (spsscode) {
  	SPSS = matrix(nrow=nrow(WOE)-sum(WOE$var=="--TOTAL--"), ncol=1, data="")
  	bin = 1			# Counter of the bins or groups within each variable
  	ii = 1			# output index (used for matrix SPSS)
  	imax = nrow(WOE)
  	for (i in 1:imax) {
  		vname = WOE$var[i]												# Variable name
  		newvname = paste("woe_", vname, sep="")		# Variable name for the WOE variable
  		vtype = vartypes[[vname]]									# Variable type: character or numeric
  		type = WOE$type[i]												# Type of variable: categorical or continuous
  		group = WOE$group[i]											# Group or bin that is currently analyzed
  		nextgroup = ""; if (i < imax) { nextgroup = WOE$group[i+1] }
  		# 1.- Check if this is an actual group
  		if (is.na(group)) {	# NA indicates that a new variable starts (since it is used as separator among variables in the WOE table)
  			bin = 1
  		} else {
  			# 2.- Check if this group is a Missing value in the original variable
  			if (group == "NaN") {
		  		expr = paste("if ( missing(", vname, ") )", newvname, "=", WOE$woe[i], ".")
  			} else {
  				# 3.- Check if the current variable is categorical
  				if (type == "categorical") {
  					# Simple equal comparison
  					# 4.- Check if the current variable is character
  					if (vtype == "character") {
	  					expr = paste("if (", vname, "=", paste("'",trim(group),"'",sep=""), ")", newvname, "=", WOE$woe[i], ".")
	  				} else {
	  					expr = paste("if (", vname, "=", group, ")", newvname, "=", WOE$woe[i], ".")
	  				} # (4)
  				} else {
						# Get the numeric bounds of the current group
						bounds = getBounds(group)
						# 5.- Check if:
						#			- the last group of the variable (nextgroup==NA) OR
						#			- this is the very last group (nextgroup=="") OR
						#			- the next group is Missing (nextgroup=="NaN")
						if (is.na(nextgroup) || nextgroup %in% c("", "NaN")) {
							# No upper bound on the condition
							expr = paste("if (", bounds$lower, bounds$opleft, vname, ")", newvname, "=", WOE$woe[i], ".")
						} else {
							# 6.- Check if this is the first group of the variable
							if (bin == 1) {
								# No lower bound on the condition
								expr = paste("if (", vname, bounds$opright, bounds$upper, ")", newvname, "=", WOE$woe[i], ".")
							} else {
								# Both lower and upper bound on the condition
								expr = paste("if (", bounds$lower, bounds$opleft, vname, " and ", vname, bounds$opright, bounds$upper, ")", newvname, "=", WOE$woe[i], ".")
							} # (6)
						} # (5)
					} # (3)
				} # (2)
				# Store the SPSS expression
				SPSS[ii] = expr
				# Go to the next group or bin
		  	bin = bin + 1
		  	# Increase the output index (used for matrix SPSS)
		  	ii = ii + 1
  		} # (1)
  	}
  }

	if (spsscode) {
	  return(list(WOE=WOE, IV=IV, SPSS=SPSS))
	} else {
	  return(list(WOE=WOE, IV=IV))
	}
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
    cex=1,																												# Adjustment factor for the legend font size
    cex.main=1)                                                   # Adjustment factor for the title of the plot
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
	opar = par(xpd=FALSE)
	on.exit(par(xpd=opar$xpd), add=TRUE)
  if (h$equidist) {
    ylim = range(h$counts + 0.01*max(h$counts)) # +0.01*max() is to leave space for the counts on top of each histogram bar
    plot(h, freq=FALSE, lty=0, xaxt="n", yaxt="n", main="", xlab="", ylab="")   # Generates first plot in the percent scale (but does NOT show the histogram)
    yaxp = par("yaxp"); usr = par("usr");                                       # Reads current vertical axis properties used below
    par(new=TRUE);                                                              # Plots the ACTUAL HISTOGRAM showing the counts as labels on top of the bars
    plot(h, labels=TRUE, freq=TRUE, xlim=xlim, ylim=ylim, col=rgb(r,g,b,0.50), main=title, xlab=xlab, ylab=ylab, yaxt="n", cex.main=cex.main)
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



########################################## ModelFit ###########################################
ModelFit <- model.fit <- function(dat, target="y", score="p", vars="p", groups=20, legend=TRUE, ...)
# Created: 			30-Mar-2014
# Author:				Daniel Mastropietro
# Description:	Evaluate using plot.binned() the model fit by selected input variables.
#								The target variable can be binary or continuous.
# Details:			A two-panel plot is created for each analyzed variable using plot.binned().
#								The first panel contains both the input and target variables in their original scale (before binning)
#								The second panel contains the input and target variables restricted to the new scale (after binning)
#	Parameters:		- dat: matrix or data frame with the modeling data
#								- target: name of variable in dat or vector with model target variable
#              	- score: name of variable in dat or vector with model score
#              	- vars: string or vector of variable names to be analyzed
#								- groups: number of groups to use for the binned plots
#								- ...: additional parameters to pass to plot.binned()
# Output: 			None
#
{
	op = par(mfrow=c(1,2), mar=c(2,2,2,0), no.readonly=TRUE); on.exit(par(op))

	#---------------------------- Parse input parameters -------------------------------
	# This parsing allows that these variables are specified as either names or actual numeric arrays
	target = parseVariable(dat, target)
	score = parseVariable(dat, score)

	# Create the function call in order to avoid conflicts between the parameters passed by the user in ... and those
	# explicitly listed in the function calls to plot.binned() below.
	# NOTE that this could be solved by simply adding every single parameter passed at the call to plot.binned() below
	# to the signature of the ModelFit() function above, but I want to keep the function signature as simple as possible.
	# (1) Read the parameters explicitly defined in the signature of the function
	funParams = formals(sys.function(sys.parent()))

	# (2) Read the parameters explicitly passed by the user
  usrParams = as.list(match.call())
  
	# Create the final parameter list by sweeping over the parameters of the function signature and checking which of them
	# were passed by the user.
	# Create the initial list of parameters by removing the function name from usrParams
	paramsList = usrParams[-1]
	for (parm in names(funParams)) {
		if (parm != "..." & !(parm %in% names(usrParams))) {
			# Add the default value of parm defined by the function signature
			paramsList[parm] = funParams[parm]
		}
	}

	# (3) Set the values of the final parameters to be passed to plot.binned()
	# Assign y and pred
	# NOTE the use of quote() --instead of using nothing or using substitute()-- because 'target' and 'score' are LOCAL variables
	# (as opposed to variables passed as parameters to the function)
	paramsList$y = quote(target)
	paramsList$pred = quote(score)
	# Remove parameters that are not used in the function call to plot.binned()
	for (parm in c("dat", "legend", "score", "target", "vars")) paramsList[parm] = NULL
	
	# Set default values when not specified by the user
	if (is.null(paramsList$ylab)) 			paramsList$ylab = "";
	if (is.null(paramsList$col)) 				paramsList$col = "light blue";
	if (is.null(paramsList$col.pred)) 	paramsList$col.pred = "red";
	if (is.null(paramsList$col.lm)) 		paramsList$col.lm = "blue";
	if (is.null(paramsList$col.loess)) 	paramsList$col.loess = "green";
	if (is.null(paramsList$print)) 			paramsList$print = FALSE
	
	# Store the value of paramsList$print on a local variable 'print' because I need it below
	print = paramsList$print
	#---------------------------- Parse input parameters -------------------------------

	# If vars is passed as a string convert it to an array of variable names
	if (is.character(vars) & length(vars) == 1) {
		vars = unlist(strsplit(vars, "[ \n]"))
	}
	i = 0
	for (v in vars) {
		i = i + 1
		cat("Analyzing variable", i, "of", length(vars), ":", v, "...\n")

		# LEFT PLOT (y has the ORIGINAL scale of the y variable)
		# This is what we plot below to the do.call() call to plot.binned():
		# plot.binned(dat[,v], target, pred=score, groups=groups, title=v, ylab="", col="light blue", col.pred="red", col.lm="blue", col.loess="green", print=print, ...)
		# Set the parameter list to be used for the function call to plot.binned()
		paramsList$x = substitute(dat[,v])	# NOTE the use of substitute() in order to pass the variable NAME and NOT its value (this is important to avoid problems when using deparse(substitute(x)) inside the function being called (which assumes that x contains a variable NAME and not its values!!)
		paramsList$title = v
		paramsList$xlim = NULL
		paramsList$ylim = NULL
		paramsList$print = print
		do.call("plot.binned", paramsList)
		if (legend) legend("top", legend=c("observed mean", "predicted mean", "lm fit", "loess fit"), pch=c(21,NA,NA,NA), lty=c(NA,1,2,1), lwd=c(NA,2,2,2), col=c("black", "red", "blue", "green"), pt.bg=c("light blue",NA,NA,NA), pt.cex=c(1.2,NA,NA,NA), cex=0.6)

		# RIGHT PLOT (y has the NEW scale of the y variable --after binning)
		# This is what we plot below to the do.call() call to plot.binned():
		# plot.binned(dat[,v], target, pred=score, groups=groups, title=v, xlim="new", ylim="new", ylab="", col="light blue", col.pred="red", col.lm="blue", col.loess="green", print=print, ...)
		# Update the parameter list to be used for the function call to plot.binned()
		paramsList$xlim = "new"
		paramsList$ylim = "new"
		paramsList$print = FALSE	# The data were already printed at the previous call to plot.binned() when requested by the user... so do NOT print them again!
		do.call("plot.binned", paramsList)	
	}
	
	# Restore graphical settings
	par(op)
}
########################################## ModelFit ###########################################



############################################## F1 #############################################
PrecisionRecall <- F1 <- function(
		target,
		score,
		cuts=seq(0,1,0.1),
		event=1,
		plot=TRUE,
		print=FALSE,
		xlim=c(0,1),
		ylim=c(0,1),
		xlab="precision (True Events / # Predicted Events)",
		ylab="recall (True Events / # Observed Events)",
		add=FALSE,
		pch=21,
		col="black",
		bg="black",
		pos=3,
		cex=0.7)
# Created: 			16-Mar-2014
# Modified:			05-May-2014
# Author:				Daniel Mastropietro
# Description:	Computes the F1 value for binary models for a given cutoff as taught at the Machine Learning course:
#								F1 = 2*P*R/(P+R)
#             	where P = Precision and R = Recall, i.e.
#              	P = True Positives / Predicted Positives
#             	R = True Positives / Observed Positives      (a.k.a Sensitivity)
#	Parameters:		- target: vector of observed binary target
#              	- score: vector of predicted probabilities
#              	- cuts: number representing a single cutoff or vector of cutoff values above which the event of interest is predicted (i.e. when pred >= cut => prediction = event).
#												When more than one cutoff values is given, the precision and recall for each of those cutoff values is returned as vectors.
#								- event: event of interest for the model prediction
#								- plot: whether to show a plot of Recall (Sensitivity) vs. Precision
#								- print: whether to show the 2x2 table of Observed vs. Predicted events for the chosen cutoff value.
#								- graphical parameters used when plot=TRUE follow.
# Output: 			A list containing the cut values and the Precision, Recall and F1 values for each cutoff.
# Examples:			pr = PrecisionRecall(tofit$y, tofit$p, plot=FALSE)
#								plot(pr$precision, pr$recall, type="b", pch=21, bg="black")
#								text(pr$precision, pr$recall, pr$cuts, offset=0.5, pos=1, cex=0.7)
{
  ### Observed values
  OBS = as.numeric(target==event)
  
  ### Predicted values
  c = 0
  ncuts = length(cuts)
  precision = vector(ncuts, mode="numeric")
  recall = vector(ncuts, mode="numeric")
  F1 = vector(ncuts, mode="numeric")
  for (cut in as.vector(cuts)) {
  	c = c + 1
		# Predicted Values
		PRED = as.numeric(score >= cut)

		### Classification table
		if (print) {
			cat("\nCutoff value", c, "of", ncuts,":", cut, "\n");
			print(table(OBS, PRED))
		}

		### Precision, Recall and F1 (they are all based on the True Positives --i.e. NOT on the True Negatives)
		# True Positives
		TP = sum(PRED==1 & PRED==OBS)
		# Precision
		if (sum(PRED) == 0) {
			precision[c] = NA
		} else {
			precision[c] = TP / sum(PRED)
				## Note: for cut = 0, precision = event rate! (because all predicted values are classified as "Event",
				## therefore the True Positives gives the number of events => precision = TP/#cases = event rate).
		}
		# Recall (a.k.a Sensitivity)
		recall[c] = TP / sum(OBS)
		# F1 value = 2*P*R/(P+R)
		if (is.na(precision[c]) | (precision[c] + recall[c]) == 0) {
			F1[c] = NA
		} else {
			F1[c] = 2*precision[c]*recall[c] / (precision[c] + recall[c])
		}
	}

	if (plot & ncuts > 1) {
		if (add) par(new=TRUE)
		# Recall is shown on the vertical axis because Recall = Sensitivity and Sensitivity usually goes on the vertical axis
		plot(precision, recall, type="b", pch=pch, col=col, bg=bg, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
		opar = par(xpd=TRUE); on.exit(par(xpd=opar$xpd))
		text(precision, recall, cuts, pos=pos, cex=cex)
		title(sub="Label is cut-off value", cex.sub=0.8)
	}

	return(list(cuts=cuts, precision=precision, recall=recall, F1=F1))
}
############################################## F1 #############################################



############################################## roc ############################################
# 2013/09/19
# Copied from Antoine Thibaud and changed a little bit, as follows:
# - renamed 'cant.bines' to 'groups'
# - added parameter 'lwd' for the line width of the ROC line
# - added report of AR (Accuracy Ratio) or Gini Index along with the AUC.
roc <- function(formula, data, pos = 1, groups = 20, print=FALSE, quantile.type = 1, round.AUC = 2, lwd=1, col=NULL,
		 												   label=NULL, xlab="Proporcion de buenos identificados", ylab="Proporcion de malos identificados", title="Curva ROC", cex=0.8, cex.main=1)
  # Genera una curva roc a partir de un df con una columna de clase binaria y otra de probabilidades
  # Hace un rank de las probabilidades y construye una roc en base al mismo
  
  # Mediante el parametro pos, se pueden ir superponiendo hasta 4 curvas roc
  # Ejemplo de uso:
  #   roc(clase ~ prob, mod1)
  #   roc(clase ~ prob, mod2, pos = 2)
  #   roc(clase ~ prob, mod2, pos = 3)
  # donde mod1, mod2 y mod3 son dfs con las variables 'clase' (valor observado del target) y 'prob' (probabilidad predicha de clase 1)
{
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
  
  # Area Under the Curve
  AUC = -sum((((2 * tbl.df$S.acum2[-1]) - diff(tbl.df$S.acum2)) /2) * diff(tbl.df$N.acum2))
  # Accuracy Ratio or Gini Index = (2*AUC - 1).
  # Note that the AUC is already independent of event rate and that is why the event rate is not needed to compute the AR or Gini index.
  # The Gini index is the ratio of the ABSS and 0.5, where:
  # ABSS = Area Between the...
  # 	- Sensitivity curve = "True Positive Rate as a function of decreasing score" = "%Hits of Events as a function of decreasing score"
  # 	- (1-Specificity) curve = "False positive Rate as a function of decreasing score" = "%Hits of Non-Events as a function of decreasing score"
  # and 0.5 = the maximum possible ABSS area (i.e. the area between the OPTIMUM Sensitivity and (1-Specificity) curves).
  # For more info, see my notes on the Nemo notebook started at Meridian in Mexico.
  AR = 2*AUC - 1
  if (is.null(col)) {
  	color = switch(pos, 'blue', 'green', 'red', 'black', 'purple', 'yellow')
  } else {
    color = col
  }
  
  if (pos == 1) {
    plot(tbl.df$N.acum2, tbl.df$S.acum2, type = 'b', pch=21, col=color, bg=color,
      xlim = c(0, 1), ylim = c(0, 1),
      xlab = xlab, ylab = ylab,
      main = title, cex.main=cex.main, lwd=lwd)
    lines(c(0, 1), c(0, 1), col = 'red', lty = 3)
  } else{
    par(new=TRUE)
    plot(tbl.df$N.acum2, tbl.df$S.acum2, type = 'b', pch=21, col=color, bg=color, 
      xlim = c(0, 1), ylim = c(0, 1), lwd=lwd,
      ann = FALSE, axes = FALSE)
  }
  if (is.null(label)) label = deparse(substitute(data))

  text(0.6, cex * pos / 7, paste(label, ':\nAUC=', formatC(AUC, format='g', round.AUC), ', AR/Gini=', formatC(AR, format='g', round.AUC), sep=""), cex = cex, col = color)

	if (print) print(tbl.df)

  return(invisible(list(data=tbl.df, AUC=AUC, AR=AR)))
}
############################################## roc ############################################
