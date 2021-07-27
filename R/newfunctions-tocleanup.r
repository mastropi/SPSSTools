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
  varsNotFound = checkVariables(dat, c(vars, varsample))
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
    idx[[samplenames[i]]] = which(substr(dat[, varsample], 1, nchar(samplevalues[i])) == samplevalues[i])
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




# --------------------------------- Boxplots (with n) and Barplots ---------------------------------
# (2017/01/11)
# - Boxplots showing sample size
# - Barplots to show variable distribution
# - Barplots to show relationship between a binary target and a categorical variable
# Ref: NBC -> ModeloPymes2016 -> D-Modelo -> D5 -> aux12-AnalisisVarios.sps

# Boxplot showing sample size
ylim = c(-1,1)
cex = 0.8
xticks = unique(toplot$TW)
bxp = boxplot(woe_CapDevDeuda ~ TW, data=toplot, main="woe_CapDevDeuda")
mtext("TW", side=1, line=3)
mtext(paste("(", bxp$n, ")", sep=""), side=1, line=2, at=xticks, las=2, cex=cex)

# Barplots showing relative distribution of a variable (v) for each value of another variable (TW)
groups = sort(unique(toplot$TW))
vars = "GR_Categoria
GR_SectorEconomico"
ylim = c(0,1)

for (v in parseVariables(vars)) {
	# Calculo de los valores a graficar
	tab = prop.table( table(toplot[,v], toplot$TW), 2)
	print(tab)
	barplot(tab, main=v, ylim=ylim, beside=TRUE)
}

# Binary target vs. categorical variable
tab = prop.table( table(toplot$Malo3Mas, toplot$TW), 2)
print(tab)
plot(colnames(tab), tab["1",])
# --------------------------------- Boxplot with sample size ---------------------------------------
