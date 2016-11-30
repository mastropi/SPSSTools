# Created:      17-Jan-2016
# Author:       Daniel Mastropietro
# Description: 	New functions that I develop which need to be cleaned up before adding it to my tools bundle.
#


# INDEX
# Biplot
# Learning Curves
#


# --------------------------------------- Biplots ------------------------------------------
# 2016/01/16: Multivariate comparison of two distributions via biplots.
# Ref:
# Projects/R/code/Heatmap and Biplot Example to Compare Samples.r
# Moni project 2015
compareDistributions = function(dat,
                                vars,
                                varsample="sample",
                                samplevalues=c("1-TRAIN", "2-VALID", "3-TEST"),
                                method=c("pca", "heatmap")) {

  # Parse input parameters
  vars = unlist(strsplit(vars, "[ \n]"))
  varsNotFound = checkVariables(dat, c(vars, sample))
  if (!is.null(varsNotFound))
    stop("Execution of compareDistributions() stops.\n")

  # Remove missing values in dat
  dat = na.omit(dat[, c(vars, varsample)])
  
  method = match.arg(method)

  # Samples  
  nsamples = length(samplevalues)                                   # Number of samples to analyze
  allsamplenames = c("train", "valid", "test")                      # All possible sample names
  samplenames = allsamplenames[1:min(nsamples, length(allsamplenames))] # Sample names to use in this case
  idx = vector("list", nsamples)
  names(idx) = samplenames
  for (i in 1:nsamples) {
    idx[[samplenames[i]]] = which(dat[, varsample] == samplevalues[i])
  }
  print(str(idx))

  if (tolower(method) == "pca") {
    ### Biplot by sample
    # PCA for TRAIN, mean and sd
    idxtrain = idx[["train"]]
    print(head(idxtrain))
    train.pca = princomp(dat[idxtrain, vars], cor=TRUE)
    train.mean = apply(dat[idxtrain, vars], 2, mean)
    train.sd = apply(dat[idxtrain, vars], 2, sd)
    train.n = apply(dat[idxtrain, vars], 2, length)	# Note the use of apply() to compute the nobs (instead of nrow()) in case there are missing values in some variables

    # Initialize the axis limits which will be updated by the FOR loop below
    xlim = range(train.pca$scores[,"Comp.1"])
    ylim = range(train.pca$scores[,"Comp.2"])
    if (nsamples > 1) {
      for (s in samplenames[2:nsamples]) {
        eval( parse( text=paste("idx", s, " = idx[['", s, "']]", sep="") ) )
        # PCA for the non-TRAIN samples (JUST to compute XLIM and YLIM! --see comments below)
        eval( parse( text=paste(s, ".data = dat[idx", s, ", vars]", sep="") ) )
        eval( parse( text=paste(s, ".pca = princomp(", s, ".data, cor=TRUE)", sep="") ) )
        
        # Compute the scores of VALID and TEST on the TRAIN principal components
        # so that we can compare the multidimensional position of the points in VALID and TEST
        # w.r.t. the points in TRAIN
        # Note the use of the center and scale of the TRAIN data and the use of the factor adjustement sqrt((n-1)/n))
        # for the scale as princomp() uses N as divisor for the standard deviation, not (N-1)!
        eval( parse( text=paste(s, ".pca.1 = train.pca", sep="") ) )
        eval( parse( text=paste(s, ".pca.1$data = scale(", s, ".data, center=train.mean, scale=train.sd*sqrt((train.n-1)/train.n))", sep="") ) )
        eval( parse( text=paste(s, ".pca.1$scores = ", s, ".pca.1$data %*% train.pca$loadings", sep="") ) )
        
        # Update the limits for the biplots based on ALL samples (to ease visual comparison)
        # (this should be the limits of the principal components (i.e. of the observations,
        # which are NOT the values returned by par("usr") after making the plot because this
        # refers to the axes used for the variables (arrows), which are the ones plotted last)
        xlim = range(xlim, eval( parse( text=paste(s, ".pca$scores[,'Comp.1']", sep="") ) ))
        ylim = range(ylim, eval( parse( text=paste(s, ".pca$scores[,'Comp.2']", sep="") ) ))
        #range(pretty(range(c(train.pca$scores[,"Comp.2"], valid.pca$scores[,"Comp.2"], test.pca$scores[,"Comp.2"]))))
        xlim = range(pretty(xlim))
        ylim = range(pretty(ylim))
        
        #****** This should be tuned on a case by case basis ********
        # Adjustment of axes based on results:
        #     xlim = xlim / 3
        #     ylim = ylim / 3
        #****** This should be tuned on a case by case basis ********
        
        #     summary(train.pca)
        #     loadings(train.pca)
        #     plot(train.pca)
      }
    }
    
    # Means of VALID and TEST data re-scaled using TRAIN data
#     print("Centered population means (i.e. biplot scale)")
#     print(cbind(train=train.mean, valid=apply(valid.pca.1$data, 2, mean), test=apply(test.pca.1$data, 2, mean)))
    
    # Generate the biplots
    # NOTE: It is important to use pc.biplot=TRUE o.w. the xlim and ylim values would be wrong...
    cex = c(0.1,0.8)
#    biplot(train.pca, main="TRAIN", pc.biplot=TRUE, cex=cex, xlim=xlim, ylim=ylim)
    op = par(mfrow=c(2,2), no.readonly=TRUE); on.exit(par(op))
    biplot.custom(train.pca, pc=c(1,2), main=samplevalues[1], pointsFlag=FALSE, outliersFlag=FALSE)
#    biplot.custom(train.pca, pc=c(1,3), main=samplevalues[1], pointsFlag=FALSE, outliersFlag=FALSE)
#    biplot.custom(train.pca, pc=c(2,3), main=samplevalues[1], pointsFlag=FALSE, outliersFlag=FALSE)
    if (nsamples > 1) {
      for (i in 2:nsamples) {
        s = samplenames[i]
        eval( parse( text=paste("biplot.custom(", s, ".pca.1, pc=c(1,2), main='", samplevalues[i], "', pointsFlag=FALSE, outliersFlag=FALSE)", sep="") ) )
#        eval( parse( text=paste("biplot.custom(", s, ".pca.1, pc=c(1,3), main='", samplevalues[i], "', pointsFlag=FALSE, outliersFlag=FALSE)", sep="") ) )
#        eval( parse( text=paste("biplot.custom(", s, ".pca.1, pc=c(2,3), main='", samplevalues[i], "', pointsFlag=FALSE, outliersFlag=FALSE)", sep="") ) )
      }
    }
  }
}
# --------------------------------------- Biplots ------------------------------------------



# --------------------------------------- Learning Curves ------------------------------------------
# See:
# Projects/R/code/LearningCurve.r
# Projects/P08-JaviMoya/CristalRProject/functions.r --> LearningCurves()
#
# --------------------------------------- Learning Curves ------------------------------------------


# ------------------------------------- Qualify Variables ------------------------------------------
# Qualifies variables in:
# - categorical (character or few categories)
# - continuous (numeric with many distinct values)
# - rejected (0 or 1 distinct value or too many missing)
# Needs package sqldf
library(sqldf)
QualifyVars = function(dat, vars=NULL, maxncat=25, maxpropcat=0.95, maxpropmiss=0.50, log=TRUE) {
# Parameters
# dat								 # Data frame containing the data for analysis
# maxncat = 25       # Maximum number of categories to consider a variable categorical
# maxpropcat = 0.95  # Maximum proportion in category to consider a variable as useful
# maxpropmiss = 0.50 # Maximum proportion of missing values to consider a variable as useful
# log = TRUE				 # Whether to show messages

	# Output variables
	tab.freq = data.frame(Var=character(0), Index=character(0), Var1=character(0), Freq=numeric(0), Prop=numeric(0), Target=numeric(0), Target2=numeric(0), Rejected=numeric(0))
	df.varcont = data.frame(var=character(0), type=character(0), nvalues=numeric(0), pvalues=numeric(0))
	df.varcat = data.frame(var=character(0), type=character(0), nvalues=numeric(0), rejected=numeric(0))
	df.varrej = data.frame(var=character(0), type=character(0), nvalues=numeric(0), pmiss=numeric(0), pmaxcat=numeric(0))

	if (is.null(vars)) {
		vars = colnames(dat)
	} else {
		vars = parseVariables(vars)
	}

	# Start process
	i = 0
	nvarcont = 0
	nvarcat = 0
	nvarrej = 0
	ntotal = nrow(dat)
	for (v in vars) {
		i = i + 1
		if (log) cat("Analyzing variable", i, ":")

		# Variable type and number of distinct values
		type = class(dat[,v])
		result = sqldf(paste("select count(distinct(", v, ")) as nvalues, sum(case when", v, "is null then 1 else 0 end) as nmiss from dat"))

		if (result$nvalues <= 1 || result$nmiss/ntotal > maxpropmiss) {
			nvarrej = nvarrej + 1
			df.varrej = rbind(df.varrej, data.frame(var=v, type=type, nvalues=result$nvalues, pmiss=result$nmiss/ntotal, pmaxcat=NA))
			if (log) cat(" REJECTED ", nvarrej, " (", v, ")\n", sep="")
		} else if (type %in% c("character", "factor") || result$nvalues <= maxncat) {
			nvarcat = nvarcat + 1
			if (log) cat(" CATEGORICAL ", nvarcat, " (", v, ")", sep="")

			# Frequency distribution when there are not so many categories
			if (result$nvalues <= maxncat) {
				tab = as.data.frame( table(dat[,v]) )
				tab$Prop = tab$Freq / sum(tab$Freq)

				# Targets penetration
				xtab = table(dat[,v], dat[,target])
				xtab2 = table(dat[,v], dat[,target2])
				tab$Target = xtab[tab$Var1, event] / tab$Freq
				tab$Target2 = xtab2[tab$Var1, event] / tab$Freq

				# Update variable values so that they are numbered and easily filtered in Excel (with ':')
				tab$Index = rownames(tab)
				tab$Var = v

				# Check max proportion in categorical value
				if (max(tab$Prop) > maxpropcat) {
					# Reject the variable
					rejected = 1
					nvarrej = nvarrej + 1
					df.varrej = rbind(df.varrej, data.frame(var=v, type=type, nvalues=result$nvalues, pmiss=result$nmiss/ntotal, pmaxcat=max(tab$Prop)))
					if (log) cat(" --> REJECTED ", nvarrej, " (max prop. category = ", formatC(max(tab$Prop), digits=2), " > ", maxpropcat, ")\n", sep="")
				} else {
					rejected = 0
					if (log) cat("\n")
				}

				# Update the output tables
				df.varcat = rbind(df.varcat, data.frame(var=v, type=type, nvalues=result$nvalues, rejected=rejected))
				tab$Rejected = rejected
				tab.freq = rbind(tab.freq[,c("Var", "Index", "Var1", "Freq", "Prop", "Target2", "Target", "Rejected")],
												 tab,
												 data.frame(Var=v,
																		Index="--TOTAL--",
																		Var1="",
																		Freq=sum(tab$Freq),
																		Prop=NA,
																		Target2=weighted.mean(tab$Target2, tab$Freq),
																		Target=weighted.mean(tab$Target, tab$Freq),
																		Rejected=rejected)
												)
			}
		} else {
			nvarcont = nvarcont + 1
			df.varcont = rbind(df.varcont, data.frame(var=v, type=type, nvalues=result$nvalues, pvalues=result$nvalues/ntotal))
			if (log) cat("\tCONTINUOUS ", nvarcont, " (", v, ")\n", sep="")
		}
	}

	return(list(tab.freq=tab.freq, df.varcat=df.varcat, df.varcont=df.varcont, df.varrej=df.varrej))
}