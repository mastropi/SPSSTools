"""This module contains Python functions for data mining to be used in SPSS >18.0.0
Author: Daniel Mastropietro
IMPORTANT: It is assumed that the output language and the user interface language of SPSS are set to English."""

# HISTORY:
# 2013/06/14: Replaced all TABs with spaces, o.w. compilation in SPSS 18.0.0 with Python 2.6 at NAT would not work.
# 2013/06/17: In Summary() function:
#             added the PAIRWISE keyword in the /MISSING option of the EXAMINE VARIABLES command so missing cases are eliminated for EACH analized variable.
#             In TransformPercent(), TransformLog(), TransformIndicator() functions:
#             added a return value which contains the names of the created variables, so that they can be stored in a Python variable (in addition to an SPSS macro variable).
# 2013/06/18: In PartialPlot() function,
#             fixed an error that interrupted the process when the 'score' variable has a label (more info under function's HISTORY)
# 2013/06/19: In MissingValues(), Summary() functions,
#             fixed an error in the execution of the 'host command' to delete temporary SAV files when the directory or file name contained blank spaces.
#             In TransformPercent function,
#             added a new parameter 'denConditionMiss' where the user can specify an additional condition to be satisfied by the denominator variable (besides being <> 0)
#             in order to actually make the computation of the output percent variable.
# 2013/07/10: In RemoveNamesFromList() function,
#             changed the default behaviour to a non-case sensitive search and added new parameter 'casesensitive'. 
#             New function: TrimVariables()
# 2013/07/26: In PartialPlot() function,
#             implemented the possibility of having the analyzed variable name equal to the score variable name (used to show model fit in terms of predicted values).
# 2013/08/02: In all functions,
#             changed the default value of the 'save' parameter to False (it has been previously set to True in all functions except PartialPlots())
#             In PlotTargetVSCat() function,
#             added the capability of saving the data used to generate the graph.
# 2013/08/14: New function BuildVarListAndString().
#             This function was created to fix the problem of getting an empty variable when there is a blank or an empty line in the list of variables passed by the user.
#             The function is based on the spssaux._buildvarlist() function whose output is then converted to string (using the " ".join() command) to which the strip() method is applied
#             in order to eliminate trailing blanks from the string returned originally by the join() method. The original list is also updated in order to remove the empty variables.
#             The function returns two arguments: a list and a string containing the variable names.
#             The values of the list are converted to integer if their values are all integers or to float if any of the value is float or left as string if any value is a string.
#             All calls to spssaux._buildvarlist() have been replaced with a call to BuildVarListAndString().
# 2013/08/14: In TrimVariables() function,
#             added new parameters: newvar, suffix, macrovar in order to create new variables to store the trimmed output and macro variables to store the list of trimmed variables.
# 2013/08/30: In all relevant functions,
#             added a spss.Submit() at the end of the function that activates the input dataset so that the user can continue working on it without having to activate it.
# 2013/08/30: In TransformIndicator() function,
#             added the possibility of creating indicator of missing values (finally!)
# 2013/09/02: In PartialPlot(), PartialPlots() functions,
#             added a new parameter 'ylim' which can be used to specify fix vertical axis limits for the Impact Plots for ALL analyzed variables.
#             renamed parameter 'ntiles' to 'groups' in order to be more consistent with the way this type of information is named in other functions.
# 2013/09/05: New function Score().
#             This function can be used to score new data with a model built in SPSS using the REGRESSION command or the LOGISTIC REGRESSION command with a binary target.
# 2013/10/11: In Score() function,
#             the SPSS syntax that allows the calculation of the score is now generated and shown in the SPSS output window.
#             added a new parameter 'test' which allows the user to choose between computing the score (test=False) or just creating the SPSS syntax to compute the score in the future.
# 2013/12/08: In several functions and in function BuildVarListAndString()
#             replaced the separator used between variable names in variable lists stored as strings from " " to "\n" (new line).
#             Ex: before I used " ".join(varlist) and now I use "\n".join(varlist)
#             The goal is to show the list of variables in the output in a more readable way (one variable per line).
#             Note that the separator was kept as " " when creating the list of statistics requested in Summary() and other similar cases. 
# 2013/12/11: In ModelCollin() function,
#             fixed the problem of the process failing when a variable to be eliminated because of high VIF
#             has a variable label! In order to do that I eliminate all variable labels from the analyzed variables.
# 2013/12/17: In PartialPlot(), PartialPlots() function,
#             added new boolean parameter 'logit' that specifies whether the Impact Plot should plot the original probabilities or their logit.
# 2013/12/19: In BuildVarListAndString() function,
#             variable names passed to the function are deduped (keeping their original order).
#             added parameter 'sort' that specifies whether the output variable list and variable names should be sorted.
# 2013/12/20: New function CheckVariables().
#             This function checks the existence of variables in a dataset and returns the list of variables that are NOT found.
#             The search can be either case sensitive or not case sensitive (because some functions such as MissingValues() require the case ot match).
# 2013/12/20: In MissingValues() function,
#             added the check whether the variables passed by the user exist in the analyzed dataset (case sensitive).
# 2014/01/04: In PartialPlot(), PartialPlots() functions,
#             added parameter 'weight' to implement weighted partial plots. However, this does NOT work properly yet as the
#             model fit in the Impact Plot for the case when the analyzed variable IS in the model does NOT coincide with the
#             actual model fit (checked by passing the score variable in parameter 'score'), neither for the linear nor the
#             logistic model cases.
# 2014/01/07: In Score() function,
#             added parameters 'debug' and 'debugmaxcases' in order to debug the generation of the score.
#             Ref: following a suggestion by Antoine
# 2014/02/09: In BuildVarListAndString() function,
#             fixed error happening when the parameter passed is a VariableDict object (which is one of the possible outputs
#             of the spssaux._buildvarlist() function.
# 2014/02/14: Moved auxiliary functions to dmspssaux.py (in order to prepare for git control versioning)
#

# TODO:
# [DONE-2013/12/20: new function CheckVariables()] 2013/08/01: Check variable existence (Ref: DM)
#             Use the spss.Dataset class and the spss.VarList class to check whether the variables passed by the user to any function exist in the dataset.
#             Ex: (from PDF documentation on Python programmability, VarList class section, page 76)
#               datasetObj = spss.Dataset('data1')
#               varListObj = datasetObj.varlist
#               if v not in varListObj:
#                 error = True
#             Note that when using a CURSOR to access a dataset, the names of the variables are CASE SENSITIVE!!!
#             For example, the MissingValues() function below uses a cursor but the Summary() does not, so the Summary() function
#             may work and the MissingValues() will NOT work if the variable names are specified with the wrong case!!!!
# [DONE-2013/12/19: I simply added the set() function in the BuildVarListAndString() function,
#       although more elaborate process needs to be used in order to keep the original order of the variable] 2013/12/08: Dedup variable lists when passed to e.g. MissingValues(), Summary(), otherwise if there are repeated variable names,
#             the following error is shown during execution: "TypeError: duplicate attribute name".
# [DONE-2013/08/14: I created the function BuildVarListAndString that solves the problem which is based on the use of the strip() method for strings] 2013/08/01: Correctly parse list of variables with empty lines (Ref: DM)
#             Use the RemoveNamesFromList() function to remove any empty lines passed at first value or last value of a list of variables to analyze
#             (e.g. when using the r""" way of listing variables and leaving the first line empty and the last line empty)
#             Use the following command after the call to vars = spssaux._buildvarlist(vars)
#               vars = RemoveNamesFromList(vars, names="", outformat="list", log=False)
#             Note that this is already done in PartialPlots().
# 2013/08/02: Output to the viewer (Ref: Paula Busto (NAT))
#             Show the content of output datasets in the Viewer (e.g. output of MissingValues and Summary)
# 2013/08/08: Iterative Regression with detection of high collinear variables and influential observations (Ref: DM)
#             For collinearity detection procede as follows:
#             (1) Do it on numeric variables (and perhaps include categorical ordinal variables by converting their values to integers...) using the REGRESSION command.
#             (2) Start iterative procedure that:
#                 (a) Removes high leverage observations (through leverage h value) (h > 2*p/N or perhaps h that is too far away from the rest...(?))
#                     (NOTE: (2013/12/21) Today I discovered a method to automate the threshold for the leverage! See '2 Modeling.sps' in the NBC\ModeloPymes2014
#                     where I set the threshold for log10(h) to be 'median + 3*IQR' (statistics computed on log10(h) obviously!)
#                 (b) Removes THE SINGLE variable having the largest VIF
#                 (c) Go to (a) until all VIF values are < 10 (or another given threshold)
#             For influential observations (check the LogisticRegression.sas program):
#             (1) Do it for BACKWARD or ENTER regressions
#             (2) Start iterative procedure of influential observations elimination:
#                 (a) Performs the regression with ALL the variables in the model at this point.
#                 (b) Removes ALL observations with large Cook distance (cook > 1 or via univariate outlier of e.g. value > 2*@99, where @99 is the 99% quantile)
#                     (NOTE: (2013/12/21) Today I discovered a method to automate the threshold for Cook! See '2 Modeling.sps' in the NBC\ModeloPymes2014
#                     where I use a Box-Cox transformation of the cook distance (removing cases with cook < 0.01 to remove lower end outliers) and then
#                     compute the threshold on Cook by transforming back the boxcox-transformed variable! => I got nice results doing this!!)
#                 (c) Go to (a)
#             (3) Remove ALL influential observations having DFBETA > 2 for the Beta having the largest p-value (which would be eliminated next) --> goal: make sure the estimate of the beta to be eliminated is NOT biased.
#             (4) Remove THE SINGLE variable with p-value > 0.2 (or another threshold)
#             (5) Go to (2) until all variables in the model are significant (p-value < 0.05 or another threshold)
# [DONE-2013/09/05: new Python function Score()] 2013/08/14: Python function to score new data, based on a regression model and needed transformation of variables (e.g. piecewise variables). (Ref: Paula Busto)
# [NOTURGENT-2013/12/19: SPSSINC CREATE DUMMY VARIABLES can be used for this, although the macro variable names giving the list of created dummy variables is not so convenient] 2013/08/29: Python function to create dummy variables from categorical variables so that they can be used in a REGRESSION command. Use the VECTOR command to map an array to the dummy variables.
#             See example of creating dummy variables in SPSS Help.txt. (Ref: Daniel Mastropietro)
# 2013/12/19: Try to implement a forward or backward regression model where we force the inclusion of x and I<n>_x where I<n>_x are
#             indicator functions that x takes the value <n> or missing. Perhaps this must be done in R..., using the add1() and drop1()
#             functions in STAT or similar functions?
#             The motivation behind this is that x may not enter alone but would perhaps enter if we also include its indicator variables!
# 2013/12/20: Put the input parameters parsing into one single function or as subfunctions of a single function called ParseInputParameters.
#             The subfunctions would depend on the parameter being parsed which would be passed as parameter to the function (e.g. DATA, VARS, etc.)
#             This will help homogenize the parsing process and avoid having to repeat the same piece of code in every function!
#

# Some writing standards:
# 1.- Name all temporary datasets with prefix @ (e.g. @data for the temporary dataset containing the input data).
# 2.- Parse input parameters and, if there is an error, stop execution using 'if error: return'
# 

import atexit,os,locale,sys
import spss.PyInvokeSpss        
from spss.errMsg import *
#import cursors         # 2010/10/02: Removed for SPSS 18, because it gives an error that cannot import 'error' from spss in 'from spss import error'


import spss
import spssaux
import operator
import spssdata 
from numpy import *     # I use this import formula here because there are many functions that are used from numpy and
                        # I don't want to refer all the time to the numpy package (e.g. function zeros that creates 
                        # a matrix with zeros)
                        
### My imports
# Global variables used at NAT
from natglobals import tempdir_
# Import auxiliay functions
from dmspssaux import CheckVariables, BuildVarListAndString, Save

__all__ = [ "RemoveNamesFromList",
            "MissingValues",
            "Summary",
            "PartialPlot",
            "PartialPlots",
            "TransformPercent",
            "TransformLog",
            "TransformIndicator",
            "TrimVariables",
            "PlotTargetVSCat",
            "ModelCollin",
            "Score"]

# Define global object error (defined as global in errMsg)
#error = errCode()


############################### String manipulation functions #################################
# 2009/01/28
# HISTORY:  (2011/05/06)
#           - Renamed the function from RemoveNameFromList to RemoveNamesFromList.
#           - Added parameters:
#               - sep:          indicates the separator to use in the output list in case outformat="string"
#               - outformat:    indicates whether the output should be given as a string or as a list
#           - Extended the functionality so that:
#               - both case sensitive and non-case sensitive searches can be carried out
#               - the output list of names returned by the function can either be a string or a list
#               (originally only a string was returned)
#               - ALL occurrences of a given name are removed
#               - Blank names can also be removed by passing the empty string or simply the blank space
#               in parameter 'names' (more explanation below where the parameters are described)
#
#           (2013/07/10)
#           - Added parameter 'casesensitive' to determine whether the search is case sensitive or not.
#           - Renamed parameter 'case' to 'outcase' for increased clarity.
#           - Renamed parameter 'sep' to 'outsep' for increased clarity.
#           - Changed the default search to a non-case sensitive search.
#           - Changed the default output case from "lower" to "upper" when the search is not case-sensitive
#           and the output case is unspecified (i.e. when case=None). If the search is case-sensitive, the
#           case of the names in the output list are left as in the original list when case=None.
#           NOTE that this somewhat complicated behaviour is due to the way letter cases are treated in Python,
#           and the way I remove the names in this function, namely: I use the 'remove' method of a list
#           to remove a name when it is found in the original list. The 'remove' method is case sensitive and
#           this is why I need to first convert all names in 'names' and in 'list' to UPPER case before searching
#           the names in the list when the search is case-sensitive.
def RemoveNamesFromList(list=(), names=(), casesensitive=True, outcase=None, outsep="\n", outformat="string", log=True):
    """Removes a set of names from a list and returns a string of the remaining names separated by 'outsep'.
    The function returns a string containing the names in 'list' that are NOT in 'names'.
    The search can either be case sensitive or not case sensitive depending on parameter 'casesensitive'.
    Parameter 'outcase' specifies the output case of the names in the output list, either
    ALL lower case, ALL upper case, or as they are given in the original list (only applies to a case-sensitive search)

    list:               String, list or tuple containing the names in the list.
                        Default: ()
    names:              String, list or tuple containing the names to remove from 'list'.
                        If an empty string, tuple or list is passed, then all empty
                        strings in list are removed.
                        This is useful to remove empty entries in a list generated by
                        the use of spssaux._buildvarlist() function when the parameter
                        to _buildvarlist is a string defined in multiple lines and some of
                        the lines are empty.
                        Default: ()
    casesensitive:      Boolean indicating whether the search of the names in list is case sensitive.
                        Default: True
    outcase:            Case to use for the names in the output list.
                        Possible values: "lower", "upper" which indicate whether the output
                        list should contain all lower cases or all upper cases.
                        When equal to None, the output case depends on the value of parameter
                        'casesensitive':
                        - when casesensitive=True, the names in the output list respect the case
                        in the original list.
                        - when casesensitive=False, the names in the output list are returned in UPPER CASE.
                        Default: None
    outsep:             Character to use as separator of names in the output string, when the output format is STRING.
                        Default: "\n"
    outformat:          Specifies whether the output list should be a string or a list
                        Default: "string"
    log:                Logical: whether to show messages in the log.
                        Default: True
    """

    #----------------------------------- Parse input parameters -------------------------------
    #-- LIST
    list0 = spssaux._buildvarlist(list)

    #-- NAMES
    names = spssaux._buildvarlist(names)
    if len(names) == 0:
        names = ['']

    #-- CASESENSITIVE
    if not(casesensitive):
        # Convert all names in 'list' and in 'names' to upper case to avoid case sensitive search
        list0 = [s.upper() for s in list0]
        names = [s.upper() for s in names]

    #-- OUTCASE
    if not(outcase):
        # Set OUTCASE to the STRING "None" so that functions lower() and upper() can be applied
        # (otherwise, an error is given that an object of NoneType does not have method lower / upper!)
        outcase = "None"
    #----------------------------------- Parse input parameters -------------------------------

    # Number of names in the list
    total0 = len(list0)
    if log:
        print "\nInput list:"
        for v in list0:
            if outcase.lower() == "upper":
                print v.upper()
            elif outcase.lower() == "lower":
                print v.lower()
            else:
                print v
        print "Total: " + str(total0)
        print "\nNames to remove:"
        for v in names:
            if outcase.lower() == "upper":
                print v.upper()
            elif outcase.lower() == "lower":
                print v.lower()
            else:
                print v
        print "Total: " + str(len(names))

    #-- Remove the names given in NAMES from list0
    for i in range(len(names)):
        found = True
        while found:
            try:
                posName = list0.index(names[i])
                list0.remove(names[i])
            except:
                found = False
    # Number of names in the output list
    total = len(list0)

    if log:
        print "\nOutput list:"
        for v in list0:
            if outcase.lower() == "upper":
                print v.upper()
            elif outcase.lower() == "lower":
                print v.lower()
            else:
                print v
        print "Names removed: " + str(total0 - total)
        print "\n"

    if outcase.lower() == "upper":
        list0 = outsep.join(list0).upper()
    elif outcase.lower() == "lower":       
        list0 = outsep.join(list0).lower()
    else:
        list0 = outsep.join(list0)

    if outformat.lower() == "list":
        list0 = spssaux._buildvarlist(list)

    return list0
############################### String manipulation functions #################################



##################################### MissingValues ###########################################
# 2008/01/28
# HISTORY:  (2009/01/28)
#           - Added savedir and saveformat
#
#           (2013/12/19)
#           - Fixed truncation of long variable names in the output dataset by changing the type of variable 'var' in the GET DATA
#           command from A32 to A64, since the maximum allowed number of characters in SPSS for a variable name is 64.
#
#           (2013/12/20)
#           - Added the check (case sensitive) whether the variables passed in 'vars' exist in dataset 'data'. I use the new
#           CheckVariables() function for this.
#           - Added the usual check of parameter types.
def MissingValues(
    data=None,
    vars=(),
    value=0,
    out=None,
    save=False,
    saveformat="sav",
    savedir=None,
    tempdir=None,
    log=True):
    """Computes number and percentage of missing values and of another selected value (default 0)

    data:               Name of the analysis dataset. If empty the active dataset is used.
                        Default: None
    vars:               String, list or tuple containing the variable names to analyze.
                        Default: ()
    value:              Value for which the occurrences are counted.
                        Default: 0
    out:                Output dataset containing the missing values analysis.
                        Default: None
    save:               Logical: whether the output table should be saved to a file.
                        Default: False
    saveformat:         Format for the output file.
                        Possible values: SAV, XLS (not case sensitive)
                        Default: "sav"
    savedir:            Directory where the output dataset is saved
                        Default: global variable tempdir_ defined at the beginnig of this module
    tempdir:            Temporary directory
                        Default: global variable tempdir_ defined at the beginnig of this module
    log:                Logical: whether to show messages in the log.
                        Default: True
    """

#   error.Reset()

    #-------------------------------- Parse input parameters ----------------------------------
    #-- Check the type of the parameters
#   if not isinstance(data,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(vars,(str,list,tuple)):
#       error.SetErrorCode(1004)
#       raise SpssError,error
#   if not isinstance(value,(int,float)):
#       error.SetErrorCode(1003)
#       raise SpssError,error
#   if not isinstance(out,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(save,bool):
#       error.SetErrorCode(1008)
#       raise SpssError,error
#   if not isinstance(tempdir,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(log,bool):
#       error.SetErrorCode(1008)
#       raise SpssError,error   

    #-- Check type of input parameters
    error = False
    errmsg = ""     # This variable collects all the generated error messages to be shown before exiting (return)

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if vars and not isinstance(vars,(str,list,tuple)):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter VARS must be of type string, list or tuple (value given: " + str(vars) + ")"
        error = True
    if value and not isinstance(value,(str,int,float)):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter VALUE must be of type string, int or float (value given: " + str(value) + ")"
        error = True
    if out and not isinstance(out,str):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter OUT must be of type string (value given: " + str(out) + ")"
        error = True
    if save and not isinstance(save,bool):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter SAVE must be of type boolean (value given: " + str(save) + ")"
        error = True
    if saveformat and not isinstance(saveformat,str):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter saveformat must be of type string (value given: " + str(saveformat) + ")"
        error = True
    if savedir and not isinstance(savedir,str):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter SAVEDIR must be of type string (value given: " + str(savedir) + ")"
        error = True
    if tempdir and not isinstance(tempdir,str):
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - Parameter TEMPDIR must be of type string (value given: " + str(tempdir) + ")"
        error = True
    if log and not isinstance(log,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter LOG must be of type boolean (value given: " + str(log) + ")"
        error = True
    
    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- VARS
    if vars:
        varlist, vars = BuildVarListAndString(vars)
    else:
        # Analyze ALL variables in the dataset
        varlist = spssaux.getVariableNamesList(range(spss.GetVariableCount()))
        vars = "\n".join(varlist)
    # Check if the variables exist in the dataset
    found, varsNotFoundList = CheckVariables(varlist, casesensitive=True, log=False)
    if not found:
        errmsg = errmsg + "\nMISSINGVALUES: ERROR - The following " + str(len(varsNotFoundList)) + " variables were not found in dataset " + data
        for v in varsNotFoundList:
            errmsg = errmsg + "\n\t" + v
        errmsg = errmsg + "\nMISSINGVALUES: (note that the case of the variable should be preserved for this function to work!)"
        error = True

    #-- OUT
    if out:
        outdata = out
    else:
        outdata = data + "_Missing"

    #-- SAVEFORMAT, SAVEDIR, TEMPDIR
    if not saveformat:
        saveformat = "sav"
    if not savedir:
        savedir = tempdir_
    if not tempdir:
        tempdir = tempdir_        

    #-- Show the parameters of execution of the macro
    if log:
        print "\nFunction call resolves to:"
        print "MissingValues("
        print "\tdata =        ", data
        print "\tvars =        ", "\n\t\t\t".join(varlist)
        print "\tvalue =       ", value
        print "\tout =         ", outdata
        print "\tsave =        ", save
        print "\tsaveformat =  ", saveformat
        print "\tsavedir =     ", savedir
        print "\ttempdir =     ", tempdir
        print "\tlog =         ", log
        print ")\n"
        if save:
            print "The following data files will be created in " + savedir + ":"
            print "\t" + outdata + "." + saveformat.lower() + "\t (contains the requested missing value summary)"
            print "\n"

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return
    #-------------------------------- Parse input parameters ----------------------------------

    # Temporary CSV file where the results of the missing value analysis are stored and then read into the output dataset
    tempfile = "/".join([tempdir, "_missing.csv"])

    # Delete the variable DATAPY used to store the SPSS dataset to be processed
    # (the PY refers to the fact that its values are generated using the module numpy)
    try:
        del datapy
    except:
        pass        # Do nothing

    # Read the SPSS data into a matrix (only read variables listed in parameter VARS)
    datapy = spssdata.Spssdata(indexes=varlist)
    ## NEED TO FIND OUT IF IT IS POSSIBLE TO VERIFY THE EXISTENCE OF VARIABLE VARS BEFORE READING IT, BECAUSE
    ## IF THEY DON'T EXIST THEN THERE IS AN ERROR. I checked in packages spss and spssdata but I didn't find anything yet.
    ## (2013/08/01) YES, there is a way: use the spss.Dataset and spss.VarList classes (see TODO section at the top of this file)

    # Compute the number of zeros and missing values in each variable and store it in a matrix (FREQ)
    n = spss.GetCaseCount()         # Number of cases in the active dataset
    nro_vars = len(varlist)         # Number of variables for analysis
    # Create a matrix of size (#vars) X 2 of zeros to store the frequencies of zeros and missing values in each variable
    # so that we get something like
    #       #zeros  #miss
    # var1  8       20
    # var2  7       33
    # var3  0       13
    freq = zeros((nro_vars,2), int32)

    # Go over each record in datapy
    for item in datapy:
        for i in range(nro_vars):
            # Check for 'value' in the current record and current variable
            if item[i] == value:
                freq[i,0] = freq[i,0] + 1
            # Check for a missing value in the current record and current variable
            elif item[i] == None:
                freq[i,1] = freq[i,1] + 1
    del datapy

    #print vars
    #print nro_vars
    #print freq

    # Generate the data with the information to be imported to an SPSS table.
    # Note that in principle I could read the data using the BEGIN DATA-END DATA block
    # but this block DOES NOT accept nothing other than data itself. Therefore, no Python code
    # can be included within this block. This is why I need to create a csv file with the data and
    # then import the file with the DATA LIST statement, outside the PROGRAM block.
    fid = file(tempfile, "w")
    # Write the column headers into the file
    fid.write("var,n,nvalue,pvalue,nmiss,pmiss\n")
    for i in range(nro_vars):
        # Define the dictionary of Python variables used in the fid.write function below.
        dictio = dict(
            var=varlist[i],
            n=n,
            nvalue=freq[i,0],
            pvalue=float(freq[i,0])/float(n),
            nmiss=freq[i,1],
            pmiss=float(freq[i,1])/float(n))
        # Write the values of var, n, nvalue, etc. by reading their values from the %dictio dictionary! (much like we read the local variables via %locals()!!)
        fid.write("%(var)s, %(n)s, %(nvalue)s, %(pvalue)s, %(nmiss)s, %(pmiss)s\n" %dictio)
    fid.close()

    # Note that I use GET DATA instead of DATA LIST for two reasons:
    # - GET DATA requires less amount of memory (as explained in the SPSS help)
    # - DATA LIST generates an error with "maximum number of loops as given by MXLOOPS reached", even though I don't know why.
    spss.Submit(r"""
new file.
get data 
/type=txt
/file='%(tempfile)s'
/arrangement=delimited
/delimiters = ','
/firstcase=2
/variables=
var     A64
n       F10.0
nvalue  F10.0
pvalue  F8.2
nmiss   F10.0
pmiss   F8.2. 
cache.
execute.
dataset name %(outdata)s.
""" %locals())

    # Save output dataset in the temporary directory, if requested
    if save:
        Save(data=outdata, format=saveformat, dir=savedir)
#       spss.Submit("save outfile = '" + savedir + outdata + ".sav'")

    # Close temporary datasets and delete temporary files
    spss.Submit("host command = 'del \"" + tempfile.replace("/","\\") + "\"'")

    # Activate the input dataset so that the user can continue doing processes on it without having to activate it again
    spss.Submit("dataset activate " + data)
##################################### MissingValues ###########################################


######################################### Summary #############################################
# 2008/02/15
# HISTORY:  (2013/08/13)
#           - Changed the order of the output summary table when BY variables are passed: the output is now sorted first
#           by variable name and then by BY variable (so that it is easier to compare the distribution of each variable amongh
#           the different groups)
def Summary(
    data=None,
    where=None,
    by=(),
    vars=(),
    stats=("mean", "std", "min", "max"),
    percentiles=(1, 5, 10, 25, 50, 75, 90, 95, 99),
    format="COMMA15.2",
    out=None,
    save=False,
    saveformat="sav",
    savedir=None,
    tempdir=None,
    log=True):
    """Computes statistics on a set of variables

    data:               Name of the analysis dataset. If empty the active dataset is used.
                        Default: None
    where:              Condition to apply to the analyzed data.
                        Default: None
    by:                 String, list or tuple defining the BY variables by which the analysis is done.
                        Default: ()
    vars:               String, list or tuple containing the variable names to analyze.
                        If the value is the empty string, the empty list or the empty tuple, ALL the variables
                        in the dataset are analyzed.
                        Ex:
                        'xx yy zz'
                        'xx, yy, zz'
                        ('xx', 'yy', 'zz')
                        ['xx', 'yy', 'zz']
                        Default: ()
    stats:              Accepted values are:
                        mean
                        mean_ci95low
                        mean_ci95upp
                        std
                        iqr
                        min
                        median
                        max
                        kurtosis
                        skewness
                        Default: ("mean", "std", "min", "max")
    percentiles:        String, list or tuple containing the percentiles to compute. Non-integer percentiles
                        can be specified.
                        Ex:
                        (2.5, 50, 75)
                        [2.5, 50, 75]
                        '2.5 50 75'
                        '2.5, 50, 75'
                        Deafult: (1, 5, 10, 25, 50, 75, 90, 95, 99)
    out:                Output dataset containing the summary information.
                        Default: <data>_summary
    save:               Logical: whether the output table should be saved to a file.
                        Default: True
    saveformat:         Format for the output file.
                        Possible values: SAV, XLS (not case sensitive)
                        Default: "sav"
    savedir:            Directory where the output dataset is saved
                        Default: global variable tempdir_ defined at the beginnig of this module
    tempdir:            Temporary directory
                        Default: global variable tempdir_ defined at the beginnig of this module
    log:                Logical: whether to show messages in the log.
                        Default: True
    """

    #-------------------------------- Parse input parameters ----------------------------------
    #-- Check type of input parameters
#   if not isinstance(data,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(vars,(str,list,tuple)):
#       error.SetErrorCode(1004)
#       raise SpssError,error
#   if not isinstance(stats,(str,list,tuple)):
#       error.SetErrorCode(1004)
#       raise SpssError,error
#   if not isinstance(percentiles,(str,list,tuple)):
#       error.SetErrorCode(1004)
#       raise SpssError,error
#   if not isinstance(format,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(out,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(save,bool):
#       error.SetErrorCode(1008)
#       raise SpssError,error
#   if not isinstance(tempdir,str):
#       error.SetErrorCode(1001)
#       raise SpssError,error
#   if not isinstance(log,bool):
#       error.SetErrorCode(1008)
#       raise SpssError,error   

#    if not isinstance(data,str):
#        raise TypeError("Parameter DATA must be string.\nValue passed:", str(data))
    if not isinstance(by,(str,list,tuple)):
        raise TypeError("Parameter BY must be string, list or tuple.\nValue passed:", str(by))
    if not isinstance(vars,(str,list,tuple)):
        raise TypeError("Parameter VARS must be string, list or tuple.\nValue passed:", str(vars))
    if not isinstance(stats,(str,list,tuple)):
        raise TypeError("Parameter STATS must be string, list or tuple.\nValue passed:", str(stats))
    if not isinstance(percentiles,(str,list,tuple)):
        raise TypeError("Parameter PERCENTILES must be string, list or tuple.\nValue passed:", str(percentiles))
    if not isinstance(format,str):
        raise TypeError("Parameter FORMAT must be string.\nValue passed:", str(format))
#    if not isinstance(out,str):
#        raise TypeError("Parameter OUT must be string.\nValue passed:", str(out))
    if not isinstance(save,bool):
        raise TypeError("Parameter SAVE must be boolean.\nValue passed:", str(save))
    if tempdir and not isinstance(tempdir,str):
        raise TypeError("Parameter TEMPDIR must be string.\nValue passed:", str(tempdir))
    if not isinstance(log,bool):
        raise TypeError("Parameter LOG must be boolean.\nValue passed:", str(log))
    if not savedir:
        savedir = tempdir_
    if not tempdir:
        tempdir = tempdir_

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data + ".")
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- WHERE
    if where:
        # Look for keyword $CASENUM, in which case the variable CASE is created in order to avoid the problems of for example
        # selecting records that satisfy $CASENUM > 100 (which will select 0 records, because of the logic behind SPSS,
        # which defines the value of $CASENUM as the case number in the OUTPUT dataset!!!)
        try:
            casenum = True
            where.lower().index("$casenum")
            where.lower().replace("$casenum", "case")
        except:
            casenum = False

    #-- BY
    bylist = []
    byvars = ""
    byvarsauto = ""     # Nnames originally used by SPSS to identify the BY variables in the output datasets generated by EXAMINE, etc. These names are: var1, var2, ..., var<n> where n is the number of BY variables.
    sortst = ""
    byst = ""
    byoffst = ""
    nro_byvars = 0
    if by:
        bylist, byvars = BuildVarListAndString(by)
        nro_byvars = len(bylist)
        byvarsauto = "var1 to var" + str(nro_byvars)
        sortst = "sort cases by " + byvars + "."
        byst = "split file by " + byvars + "."
        byoffst = "split file off."
    # Define the names used for the variables that store the different information in the different output datasets generated.
    # These names change according to the number of BY variables. First all the BY variables are named in variables whose
    # name are var1 ... var<n> where n is the number of BY variables. Then the information regarding the analysis variables
    # or the requested statistics follow, and they are stored in variables named var<n+1>...var<n+p> where p is the number
    # of information generated by the procedure regarding the analysis variables.
    n_namevar = "var" + str(nro_byvars + 1)
    summary_namevar = "var" + str(nro_byvars + 1) 
    summary_statvar = "var" + str(nro_byvars + 2)
    summary_infovar = "var" + str(nro_byvars + 3) 
    pctl_descvar = "var" + str(nro_byvars + 1)
    pctl_namevar = "var" + str(nro_byvars + 2)

#    print "byvarsauto:", byvarsauto
#    print "byvars:    ", byvars
#    print "n namevar:", n_namevar
#    print "  namevar:", summary_namevar
#    print "  statvar:", summary_statvar
#    print "  infovar:", summary_infovar
#    print "  descvar:", pctl_descvar
#    print "  namevar:", pctl_namevar

    #-- VARS
    if vars:
        varlist, vars = BuildVarListAndString(vars)
    else:
        # Analyze ALL variables in the dataset
        varlist = spssaux.getVariableNamesList(range(spss.GetVariableCount()))
        vars = "\n".join(varlist)

    #-- STATS
    # Default order of the statistics in the output dataset
    OrderStatList = spssaux._buildvarlist("mean mean_ci95low mean_ci95upp std iqr kurtosis skewness min median max")
    # Final order of the statistics in the output dataset, once the requested statistic are matched with the above list
    OrderStatFinal = ""
    statlist = []
    if stats:
        # Store the requested statistics as a list
        statlist, stats = BuildVarListAndString(stats)
        # Remove duplicates
        statlist = [v for v in set(statlist)]
        # Convert to lower case all names in statlist
        statlist = [s.lower() for s in statlist]
        # Store the list of statistics as a string to be used in the SPSS ANY() function below
        statnames = "'" + "','".join(statlist) + "'"

    #-- PERCENTILES
    pctlst = ""
    pctlnames = ""
    if percentiles:
        percentiles, dummy = BuildVarListAndString(percentiles)
        # Remove duplicates
        percentiles = [v for v in set(percentiles)]
        # Sort the percentiles
        percentiles.sort()
        # Generate the names of the variables containing the percentiles in the order corresponding to the percentiles
        # (e.g. @1 @5 @75, as opposed to e.g. @5 @1 @75)
        # In addition, if the median is requested as part of the statistics listed in stats and p50 is NOT requested
        # the median is placed among the different percentiles
        try:
            statnames.index("median")
            median = True
        except:
            median = False

        pctlnames = "@" + " @".join(map(str,percentiles))
        if median:
            # Divide the percentiles list into percentiles lower than p50 and percentiles larger than p50,
            # so that the median can be inserted in the list and take the place of p50.
            try:
                percentiles.index(50)
                p50 = True
            except:
                percentiles.append(50)
                percentiles.sort()
                p50 = False
        
            if p50:
                # Remove the median from the list of statistics in statlist
                statlist.remove("median")
            else:
                ind_p50 = percentiles.index(50)
                pctl1 = percentiles[:ind_p50]
                pctl2 = percentiles[ind_p50+1:]
                pctlnames = "@" + " @".join(map(str,pctl1)) + " median " + "@" + " @".join(map(str,pctl2))

        pctlst = "/percentiles (" + ",".join(map(str,percentiles)) + ")"

        # Add the percentiles in the list of statistics requested, so that they are also present in the variable
        # OrderStatFinal which contains the order in which we want the statistics to be stored in the dataset.
        for p in spssaux._buildvarlist(pctlnames):
            statlist.append(p)
        # Add the percentile names in thet list of ordered statistics stored in OrderStatList
        OrderStatList = spssaux._buildvarlist("mean mean_ci95low mean_ci95upp std iqr kurtosis skewness min " + pctlnames + " max")

    # Final order of statistics in the output dataset
    # (this is obtained by keeping the statistics in OrderStatList that are also in statlist)
    indices = map(OrderStatList.index, statlist)
    indices.sort()  # This orders the requested statistics in the order they appear in OrderStatList
    OrderStatFinal = " ".join([OrderStatList[v] for v in indices])

    #-- FORMAT
    formatst = ""
    if format and OrderStatFinal:
        formatst = "formats " + OrderStatFinal + " (" + format + ")."

    #-- OUT
    if out:
        outdata = out
    else:
        outdata = data + "_Summary"

    #-- Show the parameters of execution of the macro
    if log:
        print "\nFunction call resolves to:"
        print "Summary("
        print "\tdata =        ", data
        print "\twhere =       ", where
        print "\tby =          ", byvars
        print "\tvars =        ", "\n\t\t\t".join(varlist)
        print "\tstats =       ", tuple(statlist)
        print "\tpercentiles = ", tuple(percentiles)
        print "\tformat =      ", format
        print "\tout =         ", outdata
        print "\tsave =        ", save
        print "\tsaveformat =  ", saveformat
        print "\tsavedir =     ", savedir
        print "\ttempdir =     ", tempdir
        print "\tlog =         ", log
        print ")\n"
        if save:
            print "The following data files will be created in " + savedir + ":"
            print "\t" + outdata + "." + saveformat.lower() + "\t (contains the requested summary information)"
            print "\n"
    #-------------------------------- Parse input parameters ----------------------------------


    #----------------------------- Initial processing of dataset ------------------------------
    if where or by:
        spss.Submit(r"""
dataset copy @data.
dataset activate @data.
""")
        ## NOTE that I don't update the value of Python variable DATA, because this is not used anymore to define the
        ## active dataset.

        if where:
            if casenum:
                spss.Submit("compute case = $CASENUM")
            spss.Submit(["select if (" + where + ").", "execute"])
        if by:
            spss.Submit(sortst)
    #----------------------------- Initial processing of dataset ------------------------------

    ndata = "/".join([tempdir, "_n.sav"])
    summarydata = "/".join([tempdir, "_summary.sav"])
    pctldata = "/".join([tempdir, "_pctl.sav"])

    # OMS table for nro of cases, nro missing values
    spss.Submit(r"""
OMS /select tables
/if subtypes = ['Case Processing Summary']
/destination
format = sav
outfile = '%(ndata)s'
viewer = NO.
""" %locals())
    # OMS table for summary statistics
    if stats:
        spss.Submit(r"""
OMS /select tables
/if subtypes = ['Descriptives']
/destination
format = sav
outfile = '%(summarydata)s'
viewer = NO.
""" %locals())
    # OMS table for percentiles
    if percentiles:
        spss.Submit(r"""
OMS /select tables
/if subtypes = ['Percentiles']
/destination
format = sav
outfile = '%(pctldata)s'
viewer = NO.
""" %locals())
    spss.Submit(r"""
%(byst)s
examine variables = %(vars)s %(pctlst)s
/missing = report pairwise
/plot = none.
OMSEND tag = ALL.
%(byoffst)s
""" %locals())

    # Next:
    # - Change identifiers for some statistics to make their names shorter (e.g. 'Std. Deviation' to 'Std')
    # - Select statistics of interest (listed in parameter 'stats')
    # - Transpose summary information
    # NOTE that I use SPLIT FILE for BY processing and do NOT include the BY variables as an ID variable because this may
    # give the error (only when the BY variables have missing values but I do not understand why) that an INDEX value is repeated 
    # (e.g. Mean, which is the value of a statistic of interest that will later become a column in the transposed data)
    # This error is raised because, according to SPSS --which for me it is not true--, the same name appears more than once,
    # i.e. in other occurrences of the ID variables.
    # Note also that MISSING values of the ID variables are NOT included in the output dataset. This justifies as well
    # the use of the BY variables in the SPLIT statement instead of in the ID option, since this problem does not occur.
    if stats:
        spss.Submit(r"""
new file.
get file = '%(summarydata)s' /rename=(%(byvarsauto)s %(summary_namevar)s = %(byvars)s var).
dataset name @summary.
dataset copy @summary_t.
dataset activate @summary_t.
do if lower(%(summary_statvar)s) = 'minimum'.
+ compute %(summary_statvar)s = 'Min'.
else if lower(%(summary_statvar)s) = 'maximum'.
+ compute %(summary_statvar)s = 'Max'.
else if lower(%(summary_statvar)s) = 'std. deviation'.
+ compute %(summary_statvar)s = 'Std'.
else if lower(%(summary_statvar)s) = '95%% confidence interval for mean' and lower(%(summary_infovar)s) = 'lower bound'.
+ compute %(summary_statvar)s = 'Mean_ci95low'.
else if lower(%(summary_statvar)s) = '95%% confidence interval for mean' and lower(%(summary_infovar)s) = 'upper bound'.
+ compute %(summary_statvar)s = 'Mean_ci95upp'.
else if lower(%(summary_statvar)s) = '5%% trimmed mean'.
+ compute %(summary_statvar)s = 'Mean_trim05'.
else if lower(%(summary_statvar)s) = 'interquartile range'.
+ compute %(summary_statvar)s = 'IQR'.
end if.
execute.
select if any(lower(%(summary_statvar)s),%(statnames)s).
execute.

sort cases by var %(byvars)s %(summary_statvar)s.
casestovars
/id=var %(byvars)s
/index=%(summary_statvar)s
/groupby=variable
/separator=""
/drop=%(summary_infovar)s Command_ Subtype_ Label_ Std.Error.
dataset close @summary.
dataset activate @summary_t.
dataset name @summary.
""" %locals())

    # Info on nro of cases and nro of missing values
    spss.Submit(r"""
new file.
get file='%(ndata)s'
/rename=(%(byvarsauto)s %(n_namevar)s total_N Missing_N Missing_Percent = %(byvars)s var n nmiss pmiss)
/keep=%(byvars)s var n nmiss pmiss.
dataset name @n.
sort cases by var %(byvars)s.
""" %locals())

    if percentiles:
        spss.Submit(r"""
new file.
get file='%(pctldata)s' 
/rename=(%(byvarsauto)s %(pctl_namevar)s = %(byvars)s var)
/drop=Command_ Subtype_ Label_.
dataset name @pctl.
select if %(pctl_descvar)s = 'Weighted Average(Definition 1)'.
execute.
delete variables %(pctl_descvar)s.
sort cases by var %(byvars)s.
""" %locals())

    # Note that the second element in the list is the result of a combination of 3 strings using the + operator,
    # instead of putting each string as a separate element in the list. This is done like this because there will be an error 
    # if any of such strings resolves to the empty string, since SPSS places a '.' --which signifies the end of the MATCH FILES
    # command-- just because one line is empty (!!), which implies that all the other commands that follow (BY, KEEP, etc.)
    # make no more part of the MATCH FILES command, and an error is raised.
    spss.Submit(["new file.",
        "match files file=@n" +
        str(stats and       "/file=@summary"    or "") +
        str(percentiles and "/file=@pctl"       or ""),
        "/by var " + byvars,
        "/keep=" + byvars + " var n nmiss pmiss " + OrderStatFinal + ".",
        "execute.",
        "dataset name " + outdata + ".", formatst])

    # Close temporary datasets and delete temporary files
    spss.Submit("dataset close @n")
    spss.Submit("host command = 'del \"" + ndata.replace("/","\\") + "\"'")
    if where or by:
        spss.Submit("dataset close @data")
    if stats:
        spss.Submit("dataset close @summary")
        spss.Submit("host command = 'del \"" + summarydata.replace("/","\\") + "\"'")
    if percentiles:
        spss.Submit("dataset close @pctl")
        spss.Submit("host command = 'del \"" + pctldata.replace("/","\\") + "\"'")

    # Save output dataset if requested
    if save:
        Save(data=outdata, format=saveformat, dir=savedir)
#       spss.Submit("save outfile = '" + tempdir + outdata + ".sav'")

    # Activate the input dataset so that the user can continue doing processes on it without having to activate it again
    spss.Submit("dataset activate " + data)
######################################### Summary #############################################



######################################## PartialPlot ##########################################
# HISTORY:  (2011/05/06)
#           - Added the following parameters:
#               - score: the name of a variable containing the predicted value from a model to plot
#               - showFit: logical whether the fit from the model fitted here should be shown or not.
#               - output: logical whether to show or not output other than the graphs (like e.g. regression output)
#           - I enhanced the GRAPHS as follows:
#               - Added the possibility of showing the fit given by an externally fitted model,
#               which the user can specify via the parameter 'score'.
#               - Added a new line in the Impact plot, showing what would be the fit by removing
#               the analyzed variable from the model or by adding it to the model, depending on whether
#               the variable is already in the model or not.
#               The display of this line is done as follows:
#                       (a) For variables that ARE in the model a solid red line represents the model fit
#                       and a dashed red line represents the fit w.o. the variable in the model
#                       (b) For variables that are NOT in the model, a solid red line represents again the
#                       model fit (i.e. w.o. the variable) and a dashed green line represents the fit
#                       WITH the variable in the model.
#               In addition a footnote in the graph indicates whether the variable is or is not in the model.
#               - Added a LEGEND. This was a PAIN IN THE NECK!!! I cannot believe how difficult this GPL
#               language is set up for showing legends!!! Also the documentation is not complete or well
#               done in this respect. Essentially the solution was as follows:
#                       (a) Use the TRANS statement to define new categorical variables in the dataset
#                       processed in GPL with the description for each set of points or line to be shown
#                       in the legend (e.g. "Model Fit")
#                       (b) Use the SCALE: cat(aesthetic(...), map(...)) function to MAP colors or shapes
#                       to each element plotted in the graph (e.g. map the blue color to the observed values)
#                       (c) Use each of the variables defined in (a) as a variable defining one of the
#                       plotting parameters, such as color, hue, symbol shape or line shape. HERE IS WHERE
#                       THE TRICK COMES AND WHERE ONES NEED TO BE VERY CAREFUL IN ORDER TO OBTAIN THE DESIRED
#                       RESULT => Each categorical variable defining the plotting format should be used
#                       ONLY ONCE in the ELEMENT statement generating the points or line. If not, several
#                       legends are shown showing incorrect values and with a big mess on the legend part.    
#
#           (2013/06/18)
#           - Fixed an error occuring when the 'score' variable has a label. This was done by removing its label
#           under section "Bin variables"
#
#           (2013/07/26)
#           - Implemented the possibility of having the analyzed variable name equal to the score variable name,
#           which is important when we want to analyze the model fit in terms of the predicted values.
#           - Tried fixing the calculation of the total pred_ probability (which is the combination of predicted value
#           from the first regression (including all variables expect the analyzed one) and the the predicted value
#           from the partial regression (linear regression of the analyzed variable in terms of all other variables in the model))
#           based on the note written below under the PENDING section with date 2011/05/06... but it didn't work as expected!
#           It seems that it is correct to sum the predicted values of each regression...? This is correct in the linear
#           regression case, but what happens in the logistic case where I could be summing a value to the py_ probability
#           which takes the resulting probability out of the (0,1) range??
#           (Note that I left the computation of pr_ I tried commented out inside the spss.Submit() function call that computes pred_.)
#           WELL, I have also seen that the calculation is NOT exactly correct in the LINEAR REGRESSION case!!
#           (although the differences are much smaller)
#           TEST CODE USED (using NBC data, Datos.sav):
#           With LINEAR Regression
#             USE ALL.
#             GLM Malo12  WITH x_pmScComercio AutonomiaFinanciera
#             /method=sstype(3)
#             /save= pred(predL__).
#
#           With LOGISTIC Regression
#             LOGISTIC REGRESSION variables = Malo12 
#             WITH x_pmScComercio AutonomiaFinanciera
#             /save= pred(pred__).
#
#           Call to PartialPlots():
#           BEGIN PROGRAM.
#           score = "predL__"     # Linear Regression case
#           score = "pred__"      # Logistic Regression case
#           PartialPlots(
#             data=None,
#             where=None,
#             target="Malo12",
#             score=score,
#             vars="x_pmScComercio AutonomiaFinanciera CapDevDeuda",
#             varclass=(),
#             varnum="x_pmScComercio AutonomiaFinanciera",
#             model="linear",
#             showfit=True,
#             groups=20,
#             pointlabels=True,
#             both=True,
#             out=None,
#             save=False,
#             saveformat="sav",
#             savedir=savedir,
#             tempdir=tempdir,
#             log=True,
#             output=False)
#           END PROGRAM.
#
#           (2013/09/02)
#           - Added a new parameter 'ylim' which can be used to specify the vertical axis limits for the Impact Plot.
#           - Renamed parameter 'ntiles' to 'groups' in order to be more consistent with the way this type of information is named in other functions.
#
#           (2013/12/17)
#           - Added a new boolean parameter 'logit' that specifies whether the Impact Plot should plot the original probabilities (logit=False)
#           or their logit (logit=True). By default I set logit=False because users usually want to see probabilities rather than logits
#           although if we want to analyze the linearity of the target w.r.t. the variables we should use the logit scale (specially when
#           probabilities are too close to 0 or to 1.
#
# TODO: (2008/09/11)
# -[DONE-2008/10/21] 2008/09/11: Use the local variable 'error' (defined when parsing the input parameters) to stop execution
# when an error occurs. I want to use it in the following way: if error: stop
# -[DONE-2011/05/06: The new parameter taking the name of the variable containing the predicted value is 'score']
# Add the option of doing an Impact Plot with the predicted value already passed to the function. Maybe I should
# rename the function to ImpactPlot().
# -[DONE-2013/12/15: This is implemented through new logical parameter 'logit'] Add the option of plotting the LOGIT(p/(1-p)) (for linearity checking purposes).
# NOTE that this can easily be done by using the GPL statement "SCALE: logit(dim(2))" to define a logit vertical scale.
# -[DONE-2009/02/13: Both the R-Square for the partial plot and the R-Square for the Impact plot are computed
# and added to the output table. The Impact Plot R-Square for the linear case was not tested yet.]
# Add the R-Square information for each Partial Plot so that we can measure the variable importance from it.
# - Add parameter 'plot' to decide whether I want to show the plots or just create the dataset with data for plotting.
# - Add sign in the output table regarding R-Square information for Partial Plot that indicates whether the slope for
# the corresponding variable is negative or positive.
# Well, later I found that the R-Square is simply the squared of the regression coefficient, since the partial
# regression is a simple regression with no intercept.
# - 2011/05/06: Try to add the R-Square value in the title of the Partial Plot and Model Fit plot. However I don't
# know yet how to store the value of the R-Square into a Python variable so that its value can be displayed
# in the title of the plot. --> Well it could be read into a Python variable using the Data Step object and then use
# this Python variable when defining the title of the plot.
# In this title, include:
# - the percentage represented by the partial R-Square w.r.t. to the R-Square without the variable in the model (this is
# the R-Square value already stored in the output dataset rsquare, so it should be easy)
# - the p-value of the estimated regression coefficient of the partial plot so that we can quickly
# assess whether the variable is worth adding to the model or not.
# - 2011/05/06: Compute also the FULL regression R-Square so that we can store it in the output dataset
# (currently the R-Square of the regression WITHOUT the analyzed variable is stored but this is not really informative)
# - 2011/05/06: Verify and finalize the line showing the fit that would be obtained in the Impact Plot when the
# analyzed variable is NOT in the model, so that we can see what is the contribution of adding that variable
# to the model fit. So far there seems to be an error in the implementation because for the case of analysis
# variables that are NOT in the model, the 2 fitting lines (with the variable and without) should be the same
# but they are not. At this point, I am not sure if the predicted value of the full model is correctly computed
# as pred_ = py_ + pr_... Good way to test this is fitting a logistic / linear model in Clementine and using
# the predicted value from that model as score parameter here and see if the red line coincides with the black line!
# I found the following:
#       - The prediction done for the LOGISTIC case is wrong. Reason: the prediction w.o. the analyzed variable
#       coincides with the predicted value given by Clementine when the analyzed variable is NOT in the model (e.g. EMPLOY)
#       (2013/07/02: I think the way to go in the logistic case is computing logit(pred_) = logit(py_) + pr_ and then
#       doing the inverse of the logit() function to get pred_, i.e. pred_ = 1/(1 + exp(-logit(pred_))).)
#       (2013/07/26: I tried fixing this but did NOT succeed. See the HISTORY section above under (2013/07/26) for further details)
#       - The prediction is CORRECT for the LINEAR case.
# To solve the problem with THE LOGISTIC case, we should simply fit the model with ALL the variables in the model to get the predicted line.
# ADDITIONAL NOTE: (2013/11/18) I think this problem happens always in the logistic case, not only when the analyzed variables is NOT
# in the model but also when the variable is IN the model. In fact, the methodology to compute the full model estimate is the same
# in either case and for some reason it is flawed for the logistic model case.
# - 2013/06/18: Solve the error showing up in the LOGISTIC case in SPSS 18.0.0 when some of the categorical variables in the model
# (specified through the CATEGORICAL subcommand) have values that are longer than 8 characters long. This error interrupts
# the whole process. See how the problem can be solved by searching for "IMPORTANT NOTE (2013/06/18)".
# - 2013/08/14: Add a flag parameter that allows the user to JUST plot the fit, NOT the observed values. (Ref: Paula Busto)
# - 2013/09/02: Instead of Partial Plot generate an Added Variable Plot which is simply the Partial Plot scaled to the mean
# of the analyzed variable (on the horizontal axis) and of the target variable (on the vertical axis).
# - 2013/09/03: Implement the choice to plot 2 impact plots, one for the TRAIN data and one for the VALID data in order to understand whether
# there is a "significant" difference between the TRAIN and VALID samples. This could be implemented by adding a parameter called BY to
# indicate the variable defining the samples to plot (they should be at most 2 samples) because there is place only for 2 graphs...
# (and to make my life easier!)
# -[DONEPARTIALLY-2014/01/04: I say it was partially done because
# the fit for the Impact Plot when the variable IS in the model is not correct (since it does not coincide with the actual model fit,
# checked by passing parameter 'score' to the function),
# neither for the LINEAR case nor for the LOGISTIC case... I don't understand why
# at least it doesn't work for the linear case...] 2013/12/26: Add the possibility of doing a weighted regression (this came up when I had to do a weighted regression for NBC).
# Note: weighting is implemented with the WEIGHT BY command before the regression command.
# The main problem here is: how should PLOTTING be carried out? I.e. how do we represent the weights when plotting???
# I guess the best way would be to compute the average of the target variable as a weighted average...
# OK: This would be naturally done when using the command 'WEIGHT BY weight' at the beginning of the code since the average
# computed for each bin would be taken using the weights.
def PartialPlot(
    data=None,
    where=None,
    target="y",
    score=(),
    var="x",
    varclass=(),
    varnum=(),
    weight=None,
    groups=20,
    model="linear",
    showfit=True,
    pointlabels=True,
    both=None,
    logit=None,
    ylim=None,
    out=None,
    save=False,
    saveformat="sav",
    savedir=None,
    tempdir=None,
    printsyntax=False,
    log=True,
    output=False):
    """Generates Partial Plot for one variable in a GLM regression.

    data:               Name of the analysis dataset. If empty the active dataset is used.
                        Default: None
    where:              Condition to use to select a subset of the data.
                        Default: None
    target:             String indicating the target variable.
                        Default: "y"
    score:              String indicating the score variable to use in the plots as predicted values.
                        Default: ()
    var:                String indicating the analysis variable (an independent NUMERIC variable in the full model).
                        Note that CATEGORICAL variables are NOT allowed.
                        Default: "x"
    varclass:           String, list or tuple containing the categorical variables in the model.
                        Ex:
                        'xx yy zz'
                        'xx, yy, zz'
                        ('xx', 'yy', 'zz')
                        ['xx', 'yy', 'zz']
                        Default: ()
    varnum:             String, list or tuple containing the continuous variables in the model.
                        Default: ()
    weight:             String indicating an optional weight variable to use throught the whole process
                        (i.e. in regressions and average calculations)
                        Default: None
    groups:             Number of groups to use for binning the analyzed variable for plotting purposes.
                        Default: 20
    model:              Type of model to fit to the target variable.
                        Possible values are: "linear", "logit", "logistic" (not case sensitive)
                        Default: "linear"
    showfit:            Logical: whether to show the fit of the given model or just the score
                        Default: True
    pointlabels:        Logical: whether point labels showing the number of cases in each point
                        should be plotted or not.
                        Default: True
    both:               Logical: whether to plot both Partial and Impact plots / Model Fit plots or only Impact plots.
                        Default: depends on showfit. When showfit = True then both = True, o.w. both = False
    logit:              Logical: whether to plot the logit(p) in the Impact Plot of logistic models.
                        Applies only when model="logit" or "logistic"
                        Default: False
    ylim:               List or tuple of two values stating the minimum and maximum ylim values to use for ALL Impact Plots
                        Ex: [0,1] (for a binary target)
                        Default: None (meaning that each plot uses its own min and max values)
    out:                Output dataset containing the data used to produce the partial plot.
                        Default: None
    save:               Logical: whether the output table should be saved to a file.
                        Default: False
    saveformat:         Format for the output file.
                        Possible values: SAV, XLS (not case sensitive)
                        Default: "sav"
    savedir:            Directory where the output dataset is saved
                        Default: global variable tempdir_ defined at the beginnig of this module
    tempdir:            Temporary directory
                        Default: global variable tempdir_ defined at the beginnig of this module
    printsyntax:        Logical: whether to print selected SPSS syntax run during the process
                        Default: False
    log:                Logical: whether to show messages in the log.
                        Default: True
    output:             Logical: whether to show output other than graphs (such as regression output).
                        Default: False
    """

    #-------------------------------- Parse input parameters ----------------------------------
    #-- Check type of input parameters
    error = False
    errmsg = ""     # This variable collects all the generated error messages to be shown before exiting (return)

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if where and not isinstance(where,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter WHERE must be of type string (value given: " + str(where) + ")"
        error = True
    if target and not isinstance(target,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter TARGET must be of type string (value given: " + str(target) + ")"
        error = True
    if score and not isinstance(score,(str,list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter SCORE must be of type string, list or tuple (value given: " + str(score) + ")"
        error = True
    if var and not isinstance(var,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter VAR must be of type string (value given: " + str(var) + ")"
        error = True
    if varclass and not isinstance(varclass,(str,list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter VARCLASS must be of type string, list or tuple (value given: " + str(varclass) + ")"
        error = True
    if varnum and not isinstance(varnum,(str,list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter VARNUM must be of type string, list or tuple (value given: " + str(varnum) + ")"
        error = True
    if weight and not isinstance(weight,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter WEIGHT must be of type string (value given: " + str(weight) + ")"
        error = True
    if model and not isinstance(model,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter MODEL must be of type string (value given: " + str(model) + ")"
        error = True
    if showfit and not isinstance(showfit,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter SHOWFIT must be of type boolean (value given: " + str(showfit) + ")"
        error = True
    if groups and not isinstance(groups,int):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter GROUPS must be of type integer (value given: " + str(groups) + ")"
        error = True
    if pointlabels and not isinstance(pointlabels,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter POINTLABELS must be of type boolean (value given: " + str(pointlabels) + ")"
        error = True
    if both and not isinstance(both,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter BOTH must be of type boolean (value given: " + str(both) + ")"
        error = True
    if logit and not isinstance(logit,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter LOGIT must be of type boolean (value given: " + str(logit) + ")"
        error = True
    if ylim and not isinstance(ylim,(list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter YLIM must be of type list or tuple (value given: " + str(ylim) + ")"
        error = True
    if out and not isinstance(out,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter OUT must be of type string (value given: " + str(out) + ")"
        error = True
    if save and not isinstance(save,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter SAVE must be of type boolean (value given: " + str(save) + ")"
        error = True
    if saveformat and not isinstance(saveformat,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter saveformat must be of type string (value given: " + str(saveformat) + ")"
        error = True
    if savedir and not isinstance(savedir,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter SAVEDIR must be of type string (value given: " + str(savedir) + ")"
        error = True
    if tempdir and not isinstance(tempdir,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter TEMPDIR must be of type string (value given: " + str(tempdir) + ")"
        error = True
    if printsyntax and not isinstance(log,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter PRINTSYNTAX must be of type boolean (value given: " + str(printsyntax) + ")"
        error = True
    if log and not isinstance(log,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter LOG must be of type boolean (value given: " + str(log) + ")"
        error = True
    if output and not isinstance(output,bool):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter OUTPUT must be of type boolean (value given: " + str(output) + ")"
        error = True
    if not savedir:
        savedir = tempdir_
    if not tempdir:
        tempdir = tempdir_
    
    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    # Copy the input dataset to a temporary dataset called @data, which should NOT be the name of the analysis dataset!!
    # Note that even when PartialPlot() is called by function PartialPlots(), copying the analysis data to a temporary
    # data IS NECESSARY because in this function, after computing the binned variables (RANK command) I keep ONLY
    # the binned variables.
    spss.Submit(r"""
dataset copy @data.
dataset activate @data.
""")

    #-- WHERE
    if where:
        spss.Submit(r"""
select if (%(where)s).
execute.""" %locals())

    #-- TARGET
    targetlist, target = BuildVarListAndString(target)
    if len(targetlist) != 1:
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Exactly one variable must be passed in parameter 'target'."
        error = True

    #-- SCORE
    # Note that the original name of the score variable is stored in variable 'scoreOrig'
    # This is done like this because the variable 'score' is left to store the string "score"
    # which allows us to avoid using IF statements during the data preparation for process
    # to check whether a score variable was passed by the user or not.
    # Also 2 other variables are created to avoid the use of IF statements:
    # - score_
    # - score__Mean
    # which are used in the data preparation process.
    scorelist, scoreOrig = BuildVarListAndString(score)
    score = ""
    score_ = ""
    score__Mean = ""
    if len(scorelist) > 1:
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - At most one variable can be passed in parameter 'score'."
        error = True
    elif scoreOrig:
        score = "score"
        score_ = "score_"
        score__Mean = "score__Mean"

    #-- VAR
    varlist, var = BuildVarListAndString(var)
    if len(varlist) != 1:
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Exactly one variable must be passed in parameter 'var'."
        error = True

    #-- MODEL and LOGIT (this needs to come first than the parsing of VARCLASS and VARNUM because these can be
    #-- empty in the case of linear regression, but not so in the logistic regression case)
    if model.lower() == "logit" or model.lower() == "logistic":
        model = "logit"
        if logit == None:
            # By default use probability scale (logit=False) for the vertical axis of the Impact Plot
            # (because the user prefers to see probabilities rather than logits)
            # HOWEVER, in order to correctly validate a linear relationship between the variable and the target,
            # the user should set logit=True
            logit = False
    else:
        model = "linear"
        logit = False       # This is used so that the conditions on 'logit' used below when generating the Impact Plot still works for the linear case.

    #-- SHOWFIT and BOTH
    # Define whether to fit the model or not based on the values of 'showfit' and 'both'. The idea is that
    # the model is fitted either when the user requests the fit to be shown or when they request that both
    # Partial Plot and Impact Plot be shown.
    if showfit or both:
        fitModel = True
        if both == None:
            # Set BOTH to True by default when showfit = True
            both = True
    else:
        fitModel = False
        if scoreOrig:
            # In case showfit = False and there is a score variable to plot,
            # then set BOTH to False as there is no sense in showing the partial plot
            # when no model fit is shown
            both = False

    #-- VARCLASS, VARNUM
    ### Convert the parameters passed in varclass and varnum to a blank-separated list of variables
    # varclass
    varclassAll, dummy = BuildVarListAndString(varclass)
    nc = len(varclassAll)             # number of variables passed in varclass
    # varnum
    varnumAll, dummy = BuildVarListAndString(varnum)
    nn = len(varnumAll)               # number of variables passed in varnum

    #-- Check the existence of variables in the analysis dataset
    found, varsNotFoundList = CheckVariables(targetlist + scorelist + varlist + varclassAll + varnumAll + [weight], casesensitive=False, log=False)
    if not found:
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - The following " + str(len(varsNotFoundList)) + " variables were not found in dataset " + data
        for v in varsNotFoundList:
            errmsg = errmsg + "\n\t" + v
        error = True

    # Remove the analyzed variable 'var' from the list varclass and/or varnum when it exists.
    # NOTES:
    # - The search done by the index() function is case sensitive! (SPSS is not but I leave it like this)
    # - The use of 'try' because the index function returns error when the variable is not found.
    # - I could use the function RemoveNamesFromList defined above but I leave it like this at this point
    varInModel = False              # Used to show a message in the graph and to decide the color of the lines
    try:
        j = varclassAll.index(var)
        varclass = varclassAll[0:j] + varclassAll[(j+1):nc]
        varInModel = True
    except:
        varclass = varclassAll
    try:
        k = varnumAll.index(var)
        varnum = varnumAll[0:k] + varnumAll[(k+1):nn]
        varInModel = True
    except:
        varnum = varnumAll

    # Convert varclass and varnum to newline-separated lists of variables
    varclass = "\n".join(varclass)
    varnum = "\n".join(varnum)

    # Used in the GLM regression (for model = "linear")
    byvarclassstr = ""
    if len(varclass) > 0:
        byvarclassstr = "by " + varclass

    withvarnumstr = ""
    if len(varnum) > 0:
        withvarnumstr = "with " + varnum

    # Used in the LOGISTIC regression (for model = "logit")
    withvarstr = ""
    if len(varclass + varnum) > 0:
        withvarstr = "with " + varclass + " " + varnum
    elif fitModel and model == "logit":          # The LOGISTIC regression does NOT accept NO independent variables!!
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - At least one variable must be in the regression without including the analyzed variable.\n"
        error = True
    categoricalstr = ""
    if len(varclass) > 0:
        categoricalstr = "/CATEGORICAL = " + varclass

    #-- OUT
    if out:
        outdata = out
    else:
        outdata = "@pplot"

    #-- Show the parameters of execution of the macro
    if log:
        print "\nFunction call resolves to:"
        print "PartialPlot("
        print "\tdata =         ", data
        print "\twhere =        ", where
        print "\ttarget =       ", target
        print "\tscore =        ", scoreOrig
        print "\tvar =          ", var
        print "\tvarclass =     ", "\n\t\t\t".join(varclassAll)
        print "\tvarnum =       ", "\n\t\t\t".join(varnumAll)
        print "\tweight =       ", weight
        print "\tgroups =       ", groups
        print "\tmodel =        ", model
        print "\tshowfit =      ", showfit
        print "\tpointlabels =  ", pointlabels
        print "\tboth =         ", both
        print "\tlogit =        ", logit
        print "\tylim =         ", ylim
        print "\tout =          ", outdata
        print "\tsave =         ", save
        print "\tsaveformat =   ", saveformat
        print "\tsavedir =      ", savedir
        print "\ttempdir =      ", tempdir
        print "\tprintsyntax =  ", printsyntax
        print "\tlog =          ", log
        print "\toutput =       ", output
        print ")\n"
        if save:
            print "The following data files will be created in " + savedir + ":"
            print "\t" + outdata + "." + saveformat.lower() + "\t (contains the data for the Partial Plot)"
            print "\n"

    # After showing the function call, check whether the score variable has the same name as the analyzed variable 'var'
    # (This happens when we want to evaluate the model fit in terms of the predicted probability, in which case the 'var' variable must be equal to the 'score' variable.
    if scoreOrig == var:
        # Make a copy of the score variable into a variable whose name is stored in variable 'score_' which is the name used to rename the variable stored in 'scoreOrig' below, for processing.
        spss.Submit(r"""
dataset activate @data.
compute %(score_)s = %(scoreOrig)s.
execute.
""" %locals())
        # Change the value of scoreOrig to the same value stored in variable 'score_' which is the variable name to which scoreOrig is RENAMEd below during processing.
        # (note that there is no problem in renaming to the same name)
        scoreOrig = score_

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        print "Closing temporary datasets..."
        spss.Submit("dataset close @data")
        print "done."
        return
    #-------------------------------- Parse input parameters ----------------------------------


    #----------------------------------- Partial Regressions ----------------------------------
    if not output:
        spss.Submit("OMS /select ALL /destination viewer = NO.")
    if fitModel:
        if model == "logit":
            # Fit the regression excluding variable 'var'
            # IMPORTANT NOTE (2013/06/18): When categorical variables are specified through the CATEGORICAL subcommand of LOGISTIC REGRESSION,
            # their VALUES (caution: NOT the variable names) should be at most 8 characters long. If not, the OMS output table @rsquare0 is NOT created
            # because the following error message is produced:
            #   "stub sizes of all the tables must matchup".
            # This error message in principle means that the OMS output table was not fully specified (via e.g. the SUBTYPES subcommand of the
            # OMS command --Ref: http://www-01.ibm.com/support/docview.wss?uid=swg21481182), which is NOT the case here since I am fully specifying
            # the OMS output table using the SUBTYPES subcommand.
            # It seems that this is a bug because when the CATEGORICAL subcommand of LOGISTIC REGRESSION is NOT used, the error message does NOT show up.
            # The problem could be fixed by removing the CHARACTER variables from the CATEGORICAL subcommand, since the problem only happens
            # with CHARACTER variables, but this would imply a little more complex coding.
            # SPSS Version: 18.0.0 in a Windows 8, 64-bit machine (@NAT Consultores)
            if weight:
                spss.Submit("weight by " + weight)
            spss.Submit(r"""
dataset declare @rsquare0.
OMS /tag = 'rsquare'
/select ALL /if subtypes = ['Model Summary'] /destination format=sav outfile=@rsquare0.
LOGISTIC REGRESSION VARIABLES = %(target)s
%(withvarstr)s
%(categoricalstr)s
/method=ENTER
/save=pred(py_) resid(ry_) zresid(zry_).
OMSEND tag = ['rsquare'].""" %locals())
            if weight:
                spss.Submit("weight off")

            # Compute the R-Square of the regression (excluding variable 'var').
            spss.Submit(r"""
dataset activate @rsquare0.
ADD FILES file=* /rename=(CoxSnellRSquare=RSquare) /keep=RSquare.
string type(A32).
compute type = 'Impact Plot'.
execute.""")

        else:
            # Fit the regression excluding variable 'var'
            if weight:
                spss.Submit("weight by " + weight)
            spss.Submit(r"""
dataset declare @rsquare0.
OMS /tag = 'rsquare'
/select ALL /if subtypes = ['Test of Between Subjects Fixed Effects'] /destination format=sav outfile=@rsquare0.
GLM %(target)s %(byvarclassstr)s %(withvarnumstr)s
/method=sstype(3)
/design=%(varclass)s %(varnum)s
/save=pred(py_) resid(ry_) zresid(zry_).
OMSEND tag = ['rsquare'].""" %locals())
            if weight:
                spss.Submit("weight off")

            # Compute the R-Square of the regression (excluding variable 'var').
            # Note that I could have computed the R-Square as Beta^2 of the partial regression parameter, where Beta is the STANDARDIZED regression coefficient, but I realized this later.
            spss.Submit(r"""
dataset activate @rsquare0.
SELECT IF any(Var1,'Error','Corrected Total').
ADD FILES file=* /keep=Var1 TypeIIISumofSquares.
execute.
FLIP VARIABLES=TypeIIISumofSquares /NEWNAME=Var1.
dataset close @rsquare0.
dataset name @rsquare0.
string type(A32).
compute type = 'Impact Plot'.
compute RSquare = 1 - Error / Corrected_Total.
add files file=* /keep=type RSquare.
execute.""")
            
        # Compute the residuals of the analysis variable against the other variables
        spss.Submit("dataset activate @data")
        if weight:
            spss.Submit("weight by " + weight)
        spss.Submit(r"""
GLM %(var)s %(byvarclassstr)s %(withvarnumstr)s
/method=sstype(3)
/design=%(varclass)s %(varnum)s
/save=resid(rx_).""" %locals())

        # Regress the two residuals and save the R-Square of this regression as a measure of variable importance.
        spss.Submit(r"""
dataset declare @rsquare.
OMS /tag = 'rsquare'
/select ALL /if subtypes=['Model Summary'] /destination format=sav outfile=@rsquare viewer=NO.
REGRESSION variables = ry_ rx_
/dependent = ry_
/method = ENTER rx_
/save pred(pr_) resid(rr_) zpred(zpr_).
OMSEND tag = ['rsquare'].""")
        if weight:
            spss.Submit("weight off")

        # Compute the R-Square of the partial regression.
        spss.Submit(r"""
dataset activate @rsquare.
add files file=* /keep=RSquare.
string type(A32).
compute type = 'Partial Plot'.
execute.""")

        # Close all open OMS
        spss.Submit("OMSEND")


        # Put all the R-Square information together (the final dataset @rsquares contains 2 variables: type and RSquare)
        spss.Submit(r"""
add files file=@rsquare0 /file=@rsquare.
execute.
dataset name @rsquares.
dataset close @rsquare0.
dataset close @rsquare.""")
    #----------------------------------- Partial Regressions ----------------------------------

    else:   # if FitModel (=> No model fit is needed)
        # Create dummy variables in the input dataset @data with residuals and predicted variables
        # from the "non-fitted" model so that the code below is easier to implement
        spss.Submit(r"""
compute rx_ = 0.
compute py_ = 0.
compute ry_ = 0.
compute zry_ = 0.
compute zpr_ = 0.
compute pr_ = 0.
compute rr_ = 0.
execute.""")
        # Create a dummy @rsquares dataset with missing values for RSquare
        spss.Submit(r"""
DATA LIST FIXED /type 1-32 (A) RSquare 34.
BEGIN DATA.
Impact Plot                     .
Partial Plot                    .
END DATA.
dataset name @rsquares.""")

    #-------------------------------------- Bin Variables -------------------------------------
    # Bin the analysis variable and the residuals obtained from the partial regression in order to make binned graphs.
    spss.Submit("dataset activate @data")
    spss.Submit(r"""
OMS /select ALL /destination viewer = NO.
* Remove labels just in case the original variables have labels.
* Otherwise the new variables are created using the labels as names.
variable labels rx_ "" py_ "" ry_ "" zry_ "" zpr_ "" pr_ "" rr_ "".
rank variables=%(var)s(A) rx_(A)
/ntiles(%(groups)s) into x_cat_ rx_cat_ 
/ties=mean.
OMSEND.""" %locals())

    # Keep variables to use for the Partial Plot and theImpact Plot or Model Fit w.r.t. the analyzed variable
    spss.Submit(r"""
add files file=*
/rename=(%(var)s %(target)s %(scoreOrig)s = x_ y_ %(score_)s)
/keep=x_cat_ rx_cat_ rx_ py_ ry_ zry_ pr_ zpr_ rr_ x_ y_ %(score_)s %(weight)s.
* Remove labels just in case the original variables have labels.
* Otherwise the new variable for x and y in 'outdata' is created using the labels as names.
variable labels x_ "" y_ "" x_cat_ "" rx_cat_ "" """ %locals() + (scoreOrig and r"""%(score_)s "" """ %locals() or "."))

    # Compute the predicted value of the target variable using the predicted value from the first regression plus the predicted
    # value from the partial regression.
    # This is used in the Impact plot of the analyzed variable.
    if model == "logit":
        # The predicted values from first and partial regression are added using the logit scale for the first regression (which is actually a probability)
        # and the original scale from the partial regression (which is a linear regression).
        spss.Submit(r"""
*compute pred_ = 1 / (1 + exp( -( ln((py_+0.0001)/(1-py_+0.0001)) + pr_ ) )).
compute pred_ = py_ + pr_.
execute.""")
    else:
        spss.Submit(r"""
compute pred_ = py_ + pr_.
execute.""")

    # Compute the average values of the variables to plot in the vertical axes of the Impact Plot
    # and the Partial Plot.
    if weight:
        spss.Submit("weight by " + weight)
    spss.Submit(r"""
OMS /select ALL /destination viewer = NO.
dataset declare %(outdata)s.
OMS /select ALL
/if subtypes=['Report']
/destination format=sav outfile=%(outdata)s viewer=NO
/columns dimnames = ['Variables' 'Statistics'].
means tables=x_ y_ pred_ py_ %(score_)s by x_cat_
/rx_ ry_ zry_ pr_ zpr_ by rx_cat_
/cells = mean count.
OMSEND.""" %locals())
    if weight:
        spss.Submit("weight off")

    # Add a variable identifying the analysis variable,
    # rename the variables in the dataset to plot,
    # change the label identifying each type of plot (Model Fit or Partial Plot)
    # add the R-Square from the partial regression
    spss.Submit("dataset activate " + outdata)

    spss.Submit(r"""
string var (A64).
compute var = '%(var)s'.
if index(Label_,'* x_cat_')  > 0 Label_ = 'Impact Plot'.
if index(Label_,'* rx_cat_') > 0 Label_ = 'Partial Plot'.
execute.
match files file=*
/rename=(
Label_
Var1
rx__Mean
ry__Mean
zry__Mean
pr__Mean
zpr__Mean
x__Mean
y__Mean
pred__Mean
py__Mean
%(score__Mean)s
rx__N
x__N
= 
type
tile
rx
ry
zry
pr
zpr
x
y
pred
predWO
%(score)s
nr
nx)
/table=@rsquares
/by=type
/keep=
var
type
tile
rsquare
rx
ry
zry
pr
zpr
x
y
pred
predWO
%(score)s
nr
nx
.
""" %locals())

    # Remove the Total information added at the end with value tile = "Total"
    spss.Submit(r"""
select if lower(tile) <> 'total'.
execute.
dataset close @rsquares.""")

    # Save dataset used for the plot if requested.
    if save:
        Save(data=outdata, format=saveformat, dir=savedir)
#       spss.Submit("save /outfile = " + outdata + ".sav")

    # Close temporary dataset
    spss.Submit("dataset close @data")
    #-------------------------------------- Bin Variables -------------------------------------


    #-------------------------------------- Partial Plot --------------------------------------
    spss.Submit("dataset activate " + outdata)

    # Set the label to show in the Viewer so that the graph can be identified
    label = var.upper() + " Partial Plot"
    # Title of the plots
    if weight:
        titlePartial = "Weighted Partial Plot for " + var
        titleImpact  = "Weighted Impact Plot for " + var
    else:
        titlePartial = "Partial Plot for " + var
        titleImpact  = "Impact Plot for " + var

    # Define plotting parameters such as color and line format
    # The hue and brightness properties are used for the additional line showing the "would be" fit,
    # that is how the fit would be with or without the analyzed variable in the model.
    # This is needed to MAKE THE LEGEND work, as each plotting element in the graph needs to
    # use a different plotting property (e.g. color OR hue OR brightness OR shape, etc.)
    # Note that when using the hue as a specification of color, the color property has no effect
    # but instead we need to specify the saturation and the brightness. The hue values are as follows:
    # - red: 0 or 1
    # - green: 0.333
    # - blue: 0.666
    # coming from the rgb2hsv function in R.
    if varInModel:
        colorPred = "red"
        huePred = "0"
        brightPred = "1"
        linePred = "solid"
        colorPredWO = "red"
        huePredWO = "0"
        brightPredWO = "1"
        linePredWO = "dash"
    else:
        colorPred = "green"
        huePred = "0.333"       # Hue value for green (obtained with the rgb2hsv function in R)
        brightPred = "0.50"
        linePred = "dash"
        colorPredWO = "red"
        huePredWO = "0"
        brightPredWO = "1"
        linePredWO = "solid"

    # Note that GPL does NOT accept the variables to be specified with SINGLE QUOTES. That is why I am using double quotes to name them.
    # Note also that it is NOT possible to specify the variables to plot in the DATA sections via a Python variable (like %(x)s)
    submitstr = r"""
GGRAPH
/graphdataset name="impactplot" variables=rx ry pr x y pred predWO """ + (scoreOrig and "%(score)s" %locals() or "") + r""" nr nx missing=listwise reportmissing=yes
/graphspec source=inline label="%(label)s".
BEGIN GPL
SOURCE: model = userSource(id("impactplot")) 

DATA: rx = col(source(model), name("rx"))
DATA: ry = col(source(model), name("ry")) 
DATA: pr = col(source(model), name("pr"))
DATA: x = col(source(model), name("x"))
DATA: y = col(source(model), name("y"))
DATA: pred = col(source(model), name("pred"))
DATA: predWO = col(source(model), name("predWO"))""" %locals() + \
(scoreOrig and """\nDATA: score = col(source(model), name("%(score)s"))""" %locals() or "") + r"""
DATA: nr = col(source(model), name("nr"))
DATA: nx = col(source(model), name("nx"))""" %locals() + r"""

""" + \
(both and r"""
GRAPH: begin(origin(15%%,20%%), scale(37.5%%,60%%))
SCALE: linear(dim(1))
SCALE: linear(dim(2))
GUIDE: axis(dim(1), label("Residual(%(var)s)"))
GUIDE: axis(dim(2), label("Residual(%(target)s w.o. %(var)s)"))
GUIDE: text.title(label("%(titlePartial)s"))""" %locals() or "") + \
(both and       (varInModel and r"""
GUIDE: text.subsubtitle(label("(based on Model Fit)"))""" %locals() or \
                not varInModel and r"""
GUIDE: text.subsubtitle(label("(based on Model Fit WITH variable)"))""" %locals()) or "") + \
(both and varInModel and r"""
GUIDE: text.footnote(label("(variable in the model)"))""" or "") + \
(both and not varInModel and r"""
GUIDE: text.footnote(label("(variable NOT in the model)"))""" or "")+ \
(both and r"""
ELEMENT: point(position(rx*ry), color(color.blue), shape(shape.circle)""" %locals() or "") + (both and pointlabels and ", label(nr)" or "") + (both and ")" or "") + \
(both and showfit and varInModel and r"""
ELEMENT: point(position(rx*pr), color.interior(color.%(colorPred)s), color.exterior(color.%(colorPred)s), shape(shape.star))""" %locals() or "") + \
(both and showfit and r"""
ELEMENT: line(position(rx*pr), color(color.%(colorPred)s), shape(shape.%(linePred)s))""" %locals() or "") + \
(both and r"""
GRAPH: end()

GRAPH: begin(origin(62.5%%,20%%), scale(37.5%%,60%%))""" %locals() or r"""
GRAPH: begin()""") + r"""
SCALE: linear(dim(1))""" + \
(logit and r"""
SCALE: logit(dim(2))""" or \
not logit and r"""
SCALE: linear(dim(2)""" + (ylim and ", min(" + str(ylim[0]) + "), max(" + str(ylim[1]) + "))" or ")")) + r"""
GUIDE: axis(dim(1), label("%(var)s"))
GUIDE: axis(dim(2), label("%(target)s"))
GUIDE: text.title(label("%(titleImpact)s"))
GUIDE: text.footnote(label("(variable""" %locals() + (not varInModel and " NOT " or " ") + r"""in the model)"))
TRANS: obsLegend = eval("Observed Mean")
TRANS: scoreLegend = eval("Score")""" + \
(varInModel and r"""
TRANS: predLegend = eval("Model Fit")
TRANS: predWOLegend = eval("Model Fit w.o. variable")""" or \
not varInModel and r"""
TRANS: predLegend = eval("Model Fit WITH variable")
TRANS: predWOLegend = eval("Model Fit")""") + r"""
SCALE: cat(aesthetic(aesthetic.color.interior), map(("Observed Mean", color.blue)))""" + \
(varInModel and r"""
SCALE: cat(aesthetic(aesthetic.color.exterior), map(("Model Fit", color.%(colorPred)s)))
SCALE: cat(aesthetic(aesthetic.color.hue),      map(("Model Fit w.o. variable", color.hue."%(huePredWO)s")))""" %locals() or \
 not varInModel and r"""
SCALE: cat(aesthetic(aesthetic.color.exterior), map(("Model Fit", color.%(colorPredWO)s)))
SCALE: cat(aesthetic(aesthetic.color.hue),      map(("Model Fit WITH variable", color.hue."%(huePred)s")))""" %locals()) + r"""
SCALE: cat(aesthetic(aesthetic.shape.interior), map(("Score", shape.square)))
ELEMENT: point(position(x*y), color(obsLegend), shape(shape.circle)""" %locals() + (pointlabels and ", label(nx)" or "") + ")" + \
(scoreOrig and r"""
ELEMENT: point(position(x*score), color.interior(color.black), color.exterior(color.black), shape(scoreLegend))
ELEMENT: line(position(x*score), color(color.black), shape(shape.solid))""" %locals() or "") + \
(showfit and varInModel and r"""
ELEMENT: point(position(x*pred), color.interior(color.%(colorPred)s), color.exterior(predLegend), shape(shape.star))
ELEMENT: line(position(x*pred), color(color.%(colorPred)s), shape(shape.%(linePred)s))
ELEMENT: line(position(x*predWO), color.hue(predWOLegend), color.saturation(color.saturation."1"), color.brightness(color.brightness."%(brightPredWO)s"), shape(shape.%(linePredWO)s))""" %locals() or "") + \
(showfit and not varInModel and r"""
ELEMENT: point(position(x*predWO), color.interior(color.%(colorPredWO)s), color.exterior(predWOLegend), shape(shape.star))
ELEMENT: line(position(x*pred), color.hue(predLegend), color.saturation(color.saturation."1"), color.brightness(color.brightness."%(brightPred)s"), shape(shape.%(linePred)s))
ELEMENT: line(position(x*predWO), color(color.%(colorPredWO)s), shape(shape.%(linePredWO)s))""" %locals() or "") + r"""
GRAPH: end()

END GPL.""" %locals()

    if printsyntax:
        print(submitstr)
    spss.Submit(submitstr)
    #-------------------------------------- Partial Plot --------------------------------------
    
    # Activate the input dataset so that the user can continue doing processes on it without having to activate it again
    spss.Submit("dataset activate " + data)
######################################## PartialPlot ##########################################



######################################## PartialPlots #########################################
# 2008/10/21
# HISTORY: (2011/05/06)
#           - Moved the treatment of the varclass and varnum variables (concerning the removal of
#           the currently analyzed variable var[i]) to the PartialPlot() function so that the latter
#           also works when calling it directly.
#           - Fixed the problem generated by passing the variables to analyze in 'vars' using the
#           r""" format and leaving the first or last line empty. This was solved by calling
#           the function RemoveNamesFromList() --defined above-- with parameter names="", after
#           adapting the aforementioned function so that it treats this as meaning "remove all empty
#           names in the input list".
#
#           (2013/09/02)
#           - Added a new parameter 'ylim' which can be used to specify fix vertical axis limits for the Impact Plots for ALL analyzed variables.
#           Note that in order to set a more useful parameter where the user can specify to use the same vertical axis for all graphs WITHOUT
#           explicitly indicating the axis limits I would need to change the logic of generating the Partial Plots because in the sense that I
#           will first need to compute ALL the partial plots, determine the min and max of the target variable on each Impact Plot and ONLY THEN
#           generate the plots using the same vertical axis for all.
#           To this end, I could implement the pending task in PartialPlot() of adding a new parameter where the user specifies to generate
#           just the @pplot output dataset with the data needed for the plot, WITHOUT actually making the plot. At the same time, separate the
#           step in PartialPlot() creates the plot into a separate function that would be called after computing the limits for the axis limits
#           to use for ALL variables.
#           - Renamed parameter 'ntiles' to 'groups' in order to be more consistent with the way this type of information is named in other functions.
#
#           (2013/12/17)
#           - Added a new boolean parameter 'logit' that specifies whether the Impact Plot should plot the original probabilities (logit=False)
#           or their logit (logit=True). By default I set logit=False because users usually want to see probabilities rather than logits
#           although if we want to analyze the linearity of the target w.r.t. the variables we should use the logit scale (specially when
#           probabilities are too close to 0 or to 1.
#
# TODO:
# - 2009/09/22: Add the possibility of plotting all the partial plots using the same vertical scale
# (so that the significance of the slopes can be easier judged graphically)
def PartialPlots(
    data=None,
    where=None,
    target="y",
    score=(),
    vars=(),
    varclass=(),
    varnum=(),
    weight=None,
    groups=20,
    model="linear",
    showfit=True,
    pointlabels=True,
    both=None,
    logit=None,
    ylim=None,
    out=None,
    save=False,
    saveformat="sav",
    savedir=None,
    tempdir=None,
    printsyntax=False,
    log=True,
    output=False):
    """Generates a Partial Plot for each of the variables passed in vars.

    data:               Name of the analysis dataset. If empty the active dataset is used.
                        Default: None
    where:              Condition to use to perform the analysis on a subset of the data.
                        Ex: "A_SAMPLE = '1-TRAIN'"
                        Default: None
    target:             String indicating the target variable.
                        Default: "y"
    score:              Score variable to use in the plots as predicted value.
                        Default: ()
    vars:               String, list or tuple containing the list of analysis variables.
                        This generally includes the NUMERIC independent variables in the model. However it may
                        include less variables than all the variables in the model, or include other
                        variables of interest that are not in the model.
                        Note that CATEGORICAL variables are NOT allowed here.
                        Default: ()
    varclass:           String, list or tuple containing the categorical variables in the model.
                        Ex:
                        'xx yy zz'
                        'xx, yy, zz'
                        ('xx', 'yy', 'zz')
                        ['xx', 'yy', 'zz']
                        Default: ()
    varnum:             String, list or tuple containing the continuous variables in the model.
                        Default: ()
    weight:             String indicating an optional weight variable to use throught the whole process
                        (i.e. in regressions and average calculations)
                        Default: None
    groups:             Number of groups to use for binning the analyzed variables for plotting purposes.
                        Default: 20
    model:              Type of model to fit to the target variable.
                        Possible values are: "linear", "logit", "logistic" (not case sensitive)
                        Default: "linear"
    showfit:            Logical: whether to show the fit of the given model or just the score
                        Default: True
    pointlabels:        Logical: whether point labels showing the number of cases in each point
                        should be plotted or not.
                        Default: True
    both:               Logical: whether to plot both Partial and Impact / Model Fit plots or only Impact plots.
                        Default: depends on showfit. When showfit = True then both = True, o.w. both = False
    logit:              Logical: whether to plot the logit(p) in the Impact Plot of logistic models.
                        Applies only when model="logit" or "logistic"
                        Default: False
    ylim:               List or tuple of two values stating the minimum and maximum ylim values to use for ALL Impact Plots
                        Ex: [0,1] (for a binary target)
                        Default: None (meaning that each plot uses its own min and max values)
    out:                Output dataset containing the data used to produce the partial plots.
                        Default: None
    save:               Logical: whether the output table should be saved to a file.
                        Default: False
    saveformat:         Format for the output file.
                        Possible values: "sav", "xls" (not case sensitive)
                        Default: "sav"
    savedir:            Directory where the output dataset is saved
                        Default: global variable tempdir_ defined at the beginnig of this module
    tempdir:            Temporary directory
                        Default: global variable tempdir_ defined at the beginnig of this module
    printsyntax:        Logical: whether to print selected SPSS syntax run during the process
                        Default: False
    log:                Logical: whether to show messages in the log.
                        Default: True
    output:             Logical: whether to show output other than graphs (such as regression output).
                        Default: False
    """

    #-------------------------------- Parse input parameters ----------------------------------
    error = False
    errmsg = ""

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if where and not isinstance(where,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter WHERE must be of type string (value given: " + str(where) + ")"
        error = True
    if target and not isinstance(target,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter TARGET must be of type string (value given: " + str(target) + ")"
        error = True
    if score and not isinstance(score,(str,list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter SCORE must be of type string, list or tuple (value given: " + str(score) + ")"
        error = True
    if not(vars) or (vars and not isinstance(vars,(str,list,tuple))):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter VARS must be non-empty and of type string, list or tuple (value given: " + str(vars) + ")"
        error = True
    if varclass and not isinstance(varclass,(str,list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter VARCLASS must be of type string, list or tuple (value given: " + str(varclass) + ")"
        error = True
    if varnum and not isinstance(varnum,(str,list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter VARNUM must be of type string, list or tuple (value given: " + str(varnum) + ")"
        error = True
    if weight and not isinstance(weight,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter WEIGHT must be of type string (value given: " + str(weight) + ")"
        error = True
    if groups and not isinstance(groups,int):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter GROUPS must be of type integer (value given: " + str(groups) + ")"
        error = True
    if model and not isinstance(model,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter MODEL must be of type string (value given: " + str(model) + ")"
        error = True
    if showfit and not isinstance(showfit,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter SHOWFIT must be of type boolean (value given: " + str(showfit) + ")"
        error = True
    if pointlabels and not isinstance(pointlabels,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter POINTLABELS must be of type boolean (value given: " + str(pointlabels) + ")"
        error = True
    if both and not isinstance(both,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter BOTH must be of type boolean (value given: " + str(both) + ")"
        error = True
    if logit and not isinstance(logit,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter LOGIT must be of type boolean (value given: " + str(logit) + ")"
        error = True
    if ylim and not isinstance(ylim,(list,tuple)):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter YLIM must be of type list or tuple (value given: " + str(ylim) + ")"
        error = True
    if out and not isinstance(out,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter OUT must be of type string (value given: " + str(out) + ")"
        error = True
    if save and not isinstance(save,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter SAVE must be of type boolean (value given: " + str(save) + ")"
        error = True
    if saveformat and not isinstance(saveformat,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter SAVEFORMAT must be of type string (value given: " + str(saveformat) + ")"
        error = True
    if savedir and not isinstance(savedir,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter SAVEDIR must be of type string (value given: " + str(savedir) + ")"
        error = True
    if tempdir and not isinstance(tempdir,str):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter TEMPDIR must be of type string (value given: " + str(tempdir) + ")"
        error = True
    if printsyntax and not isinstance(log,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter PRINTSYNTAX must be of type boolean (value given: " + str(printsyntax) + ")"
        error = True
    if log and not isinstance(log,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter LOG must be of type boolean (value given: " + str(log) + ")"
        error = True
    if output and not isinstance(output,bool):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - Parameter OUTPUT must be of type boolean (value given: " + str(output) + ")"
        error = True
    if not savedir:
        savedir = tempdir_
    if not tempdir:
        tempdir = tempdir_

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- WHERE (PENDING)
    # If it makes sense and it is easy to implement, it may be appropriate to do the following here (before calling
    # function PartialPlot()):
    # - copy the analysis dataset to a temporary dataset (should NOT be called @data though because this name is used
    # inside PartialPlot()!)
    # - apply any filtering defined by the WHERE option to such temporary dataset (so that it is not applied every time
    # from within PartialPlot(), which may imply a considerable waste of time)

    #-- TARGET
    targetlist, target = BuildVarListAndString(target)
    if len(targetlist) != 1:
        print "PARTIALPLOTS: ERROR - Exactly one variable must be passed in parameter 'target'."
        error = True

    #-- SCORE
    scorelist, score = BuildVarListAndString(score)

    #-- VARS
    # Convert the parameter to a list, so that the extraction of its elements is easier
    varlist, dummy = BuildVarListAndString(vars)

    #-- VARCLASS, VARNUM
    # Convert the parameters to lists, so that the extraction of its elements is easier
    varclass, dummy = BuildVarListAndString(varclass)
    varnum, dummy = BuildVarListAndString(varnum)

    #-- Check the existence of variables in the analysis dataset
    found, varsNotFoundList = CheckVariables(targetlist + scorelist + varlist + varclass + varnum + [weight], casesensitive=False, log=False)
    if not found:
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - The following " + str(len(varsNotFoundList)) + " variables were not found in dataset " + data
        for v in varsNotFoundList:
            errmsg = errmsg + "\n\t" + v
        error = True

    #-- MODEL
    model = model.lower()
    if model not in ("logit","logistic","linear"):
        errmsg = errmsg + "\nPARTIALPLOTS: ERROR - The possible values of parameter 'model' are 'linear', 'logit' or 'logistic'. Value passed: " + str(model) + "\n"
        error = True
#   if model.lower() == "logit":
#       model = "logit"
#   else:
#       model = "linear"

    #-- OUT
    if out:
        outdata = out
    else:
        outdata = "@pplots"

    #-- Show the parameters of execution of the macro
    if log:
        print "\nFunction call resolves to:"
        print "PartialPlots("
        print "\tdata =         ",  data
        print "\twhere =        ",  where
        print "\ttarget =       ",  target
        print "\tscore =        ",  score
        print "\tvars =         ",  "\n\t\t\t".join(varlist)
        print "\tvarclass =     ",  "\n\t\t\t".join(varclass)
        print "\tvarnum =       ",  "\n\t\t\t".join(varnum)
        print "\tweight =       ",  weight
        print "\tgroups =       ",  groups
        print "\tmodel =        ",  model
        print "\tshowfit =      ",  showfit
        print "\tpointlabels =  ",  pointlabels
        print "\tboth =         ",  both
        print "\tlogit =        ",  logit
        print "\tylim =         ",  ylim
        print "\tout =          ",  out
        print "\tsave =         ",  save
        print "\tsaveformat =   ",  saveformat
        print "\tsavedir =      ",  savedir
        print "\ttempdir =      ",  tempdir
        print "\tprintsyntax =  ",  printsyntax
        print "\tlog =          ",  log
        print "\toutput =       ",  output
        print ")\n"
        if save:
            print "The following data files will be created in " + savedir + ":"
            print "\t" + outdata + "." + saveformat.lower() + "\t (contains the data for the Partial Plots)"
            print "\n"

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return
    #-------------------------------- Parse input parameters ----------------------------------


    #------------------------ Partial Plot for each analysis variable -------------------------
    n = len(varlist)
    print(varlist)
    for i in range(n):
        print "\nAnalyzing variable", varlist[i], "(" + str(i+1), "out of", str(n) + ") ..."
        PartialPlot(
                data=data,
                where=where,
                target=target,
                score=score,
                var=varlist[i],
                varclass=varclass,
                varnum=varnum,
                weight=weight,
                groups=groups,
                model=model,
                showfit=showfit,
                pointlabels=pointlabels,
                both=both,
                logit=logit,
                ylim=ylim,
                out="@pplot",
                save=False,
                tempdir=tempdir,
                printsyntax=printsyntax,
                log=False,
                output=output)
        
        # Append the data used to generate the above plots to the output table
        if i == 0:
            if log:
                print "Adding data for plotting to the output dataset\n"
            spss.Submit(r"""
add files file=@pplot.
execute.
dataset name %(outdata)s.""" %locals())
        else:
            if log:
                print "Adding data for plotting to the output dataset\n"
            spss.Submit(r"""
dataset activate %(outdata)s.
add files file=* /file=@pplot.
execute.""" %locals())
    #------------------------ Partial Plot for each analysis variable -------------------------

    # Save dataset used for all the plots if requested.
    if save:
        Save(data=outdata, format=saveformat, dir=savedir)

    # Activate the input dataset so that the user can continue doing processes on it without having to activate it again
    spss.Submit("dataset activate " + data)
######################################## PartialPlots #########################################


###################################### TransformPercent #######################################
# 2009/01/19
# HISTORY:  (2013/08/01)
#           - Renamed parameter 'condition' to 'numConditionMiss' to better understand the meaning
#           of this parameter.
#           - Changed the impact of 'denConditionMiss' in the result of the percent operation for better
#           clarity, since the name of the parameter seems to indicate: "the (additional) condition to be
#           satisfied by the denominator variable in order to set the result of the percent operation to
#           the value given in parameter 'miss' --instead of $SYSMIS". The new impact of this parameter is
#           the following:
#           "compute the ratio num/den*100 only when den <> 0 and denConditionMiss is NOT satisfied."
#           The new part is the "NOT", in other words, when the condition 'denConditionMiss' is satisfied
#           by the denominator variable (and numConditionMiss is satisfied as well) set the result of the
#           percent operation to the value given in parameter 'miss' --as opposed to setting it to $SYSMIS.
def TransformPercent(
    data=None,
    vars=(),
    den=(),
    min=None,
    max=None,
    miss=0,
    numConditionMiss=None,
    denConditionMiss=None,
    prefix="P_",
    suffix=None,
    preplace=True,
    sreplace=False,
    macrovar="pctvars",
    test=False,
    log=True):
    """Computes Percent transformation of a set of variables and optionally limits the percentage values
    between a minimum and a maximum.
    The percentage is computed as the division num / den * 100, whenever den is not 0.
    Otherwise, if den = 0, two different values may be returned, depending on the result of applying 'numConditionMiss'
    on the numerator variable, namely:
    - if 'numConditionMiss' is satisfied => the result is the value given in 'miss'.
    - if 'numConditionMiss' is NOT satisfied => the result is $SYSMIS.
    
    New variables with the prefix given in 'prefix' and suffix given in 'suffix' are created, as many as
    variables listed in 'vars'.
    By default the percent variables are called P_<var>.
    The list of created variable names is optionally returned by the function (when parameter macrovar is not empty).

    data:                   Name of the analysis dataset. If empty the active dataset is used.
                            Default: None
    vars:                   String, list or tuple containing the list of variables for which the percentage is computed.
                            If empty, all variables in the dataset are used (which are assumed all numeric!)
                            Default: ()
    den:                    Denominator variable used to compute the percentage
                            Default: ()
    min:                    Minimum value used as lower bound for the percentage result.
                            Default: None
    max:                    Maximum value used as upper bound for the percentage result.
                            Default: None
    miss:                   Numeric value or string containing an expression whose result is used as 'missing' when the
                            denominator is 0 or satisfies any other condition as specified in parameter 'denConditionMiss'.
                            If this is an expression, whenever the keyword 'var' appears in it, its value is replaced
                            by the actual variable that is being processed at the moment.
                            Ex: miss = "((var>0)-(var<0))*100"
                            defines the 'missing' value as +100 or -100 depending on the sign of the variable
                            being processed)
                            Default: 0
    numConditionMiss:       Condition to be satisfied by the numerator variable in order to set the result of the
                            percent operation to the value given in parameter 'miss' when the denominator variable
                            'den' is 0 (or satisfies the condition given in 'denConditionMiss').
                            If the condition is not satisfied, the value used for the result of the percent
                            operation is $SYSMIS.
                            Ex: "var = 0" (only when the numerator variable is 0 is the resulting
                            percent variable set to the value or expression given in 'miss' --whenever the denominator
                            variable is also 0 or satisfies the condition given in 'denConditionMiss')
                            Default: None
    denConditionMiss:       Additional condition to be satisfied by the denominator variable specified in 'den'
                            (besides the condition 'den = 0') in order to set the result of the percent operation
                            to the value given in 'miss' (as long as the condition specified in parameter 'numConditionMiss'
                            is also satisfied).
                            Ex: "den = -0.01" (whenever the condition given by the expression 'den = 0 or den = -0.01'
                            is satisfied set the result of the percent operation to the value given in 'miss';
                            in other words, only compute the value of the percent variable as the ratio between
                            the numerator variable and the denominator variable when the denominator variable is not 0
                            and not equal to -0.01. The typical application of this is when the denominator variable,
                            previous to this call to TransformPercent, had its missing values replaced by -0.01
                            (instead of e.g. 0 for distinction) in which case we do NOT want the percent operation
                            to be applied as the meaning of the result will be meaningless)
                            Default: None
    prefix:                 Prefix to use for the new variables containing the percentage.
                            Default: "P_"
    suffix:                 Suffix to use for the new variables containing the percentage.
                            Default: None
    preplace:               Logical: whether the prefix should replace the initial part of the variable name
                            that is as long as the prefix.
                            Default: True
                            Ex: (when True) The percentage on V_VOLUME when prefix = 'P_' is named P_VOLUME.
    sreplace:               Logical: whether the suffix should replace the end part of the variable name
                            that is as long as the suffix.
                            Default: False
                            Ex: (when False) The percentage on V_VOLUME when suffix = '_PCT' is named V_VOLUME_PCT.
    macrovar:               Name of the Python variable and the SPSS macro variable to store the list of variable names
                            created by the process, separated by new line characters.
                            For the SPSS macro variable name, the nme given is uppercased and a '!' is added at the beginning.
                            Default: "pctvars" => SPSS macro variable name: '!PCTVARS'
    test:                   Logical: whether to run the function in test mode. That is shows the generated lines
                            but does not execute them.
                            A True value can also be used to create the SPSS macro variable with the list of
                            transformed variable names.
                            Default: False
    """

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- VARS
    if vars:
        varlist, vars = BuildVarListAndString(vars)
    else:
        # Analyze ALL variables in the dataset
        varlist = spssaux.getVariableNamesList(range(spss.GetVariableCount()))
        vars = "\n".join(varlist)

    #-- NUMCONDITIONMISS
    if not isinstance(numConditionMiss,str):
        numConditionMiss = False

    #-- DENCONDITIONMISS
    if not isinstance(denConditionMiss,str):
        denConditionMiss = False
    else:
        # Replace any occurrence of 'den' with the variable name passed in parameter 'den'.
        denConditionMiss = denConditionMiss.lower().replace("den","%(den)s") %locals()

    #-- PREFIX and SUFFIX
    # The following is to avoid an error when preplace = True and prefix = None for example.
    if prefix == None:
        prefix = ""
    if suffix == None:
        suffix = ""
        
    #-- TEST
    if test:
        print "\n--- Test mode execution ---"
    #-------------------------------- Parse input parameters ----------------------------------

    #-- Create the list of percent variables
    pvarlist = []   # Prefix 'p' stands for PERCENT
    for var in varlist:
        print "\nProcessing variable " + var + "..."
        if preplace:
            if sreplace:
                pvar = prefix + var[len(prefix):len(var)-len(suffix)] + suffix
            else:
                pvar = prefix + var[len(prefix):]
        else:
            if sreplace:
                pvar = prefix + var[:len(var)-len(suffix)] + suffix
            else:
                pvar = prefix + var + suffix
        pvarlist.append(pvar)

        # Value to use when the denominator is 0
        if isinstance(miss,(int,float)):
            missvalue = miss
        elif isinstance(miss,str):
            #-- Replace 'var' with %(var)s so that the missing expression is evaluated on the actual variable being processed.
            missvalue = miss.lower().replace("var","%(var)s") %locals()
        else:
            missvalue = 0

        submitstr = r"""
do if not(%(den)s = 0) and not(%(denConditionMiss)s).
+ compute %(pvar)s = %(var)s / %(den)s * 100.
"""
        if numConditionMiss:
            #-- Replace 'var' with %(var)s and 'den' with %(den)s so that the condition is applied on the actual variables being processed (indicated in parameters 'var' and 'den')
            numConditionMiss = numConditionMiss.lower().replace("var","%(var)s") %locals()
            numConditionMiss = numConditionMiss.lower().replace("den","%(den)s") %locals()
            submitstr = submitstr + r"""else if %(numConditionMiss)s.
+ compute %(pvar)s = %(missvalue)s.
else.
+ compute %(pvar)s = $SYSMIS.
end if.
"""
        else:
            submitstr = submitstr + r"""else.
+ compute %(pvar)s = %(missvalue)s.
end if.
"""
        if min <> None:
            submitstr = submitstr + r"""
if %(pvar)s < %(min)s %(pvar)s = %(min)s.
"""
        if max:
            submitstr = submitstr + r"""
if %(pvar)s > %(max)s %(pvar)s = %(max)s.
"""
        submitstr = submitstr + "execute.\n"

        if log or test:
            print(submitstr %locals())
        if not test:
            spss.Submit(submitstr %locals())
    
    if test:
        print "--- Test mode execution ---"
        
    #-- Show the list of percent variables.
    if log:
        if test:
            print "The following percent variables would be created in dataset " + data + ":"
        else:
            print "The following percent variables were created in dataset " + data + ":"
        for pvar in pvarlist:
            print pvar
        print "Total: " + str(len(pvarlist))

    #-- Create the SPSS macro variable if required
    if macrovar:
        print "\nMacro variable " + "!" + macrovar.upper() + " created containing the list of percent variables."
        spss.SetMacroValue("!" + macrovar,"\n".join(pvarlist))
      
        # Return the list of variable names as a Python variable
        print "This list is also RETURNED (needs to be assigned to an external variable by the user) as a Python variable"
        pvarnames = "\n".join(pvarlist)
        return pvarnames
###################################### TransformPercent #######################################


######################################## TransformLog #########################################
# 2009/01/19
# HISTORY:  (2013/08/01)
#           - Changed parameter 'symmetric' to 'safe' to refer to the safe log transformation sign(x)*log10(1 + abs(x))
#           This was done after discovering that SPSS has this transformation built in in the Chart Builder dialog box
#           and calls it "safe log-transformation".
def TransformLog(
    data=None,
    vars=(),
    safe=True,
    prefix="ZL_",
    suffix=None,
    preplace=False,
    sreplace=False,
    pposition=1,
    sposition=1,
    macrovar="logvars",
    test=False,
    log=True):
    """Computes Safe or regular Log transformation of a set of numerical variables.
    The safe log transformation (default) is:
        sign(x)*Log(1 + abs(x))
    The regular log transformation is:
        Log(x) if x > 0, else $SYSMIS
    New variables with the prefix given in 'prefix' and suffix given in 'suffix' are created, as many as
    variables listed in 'vars'.
    By default the log-transformed variables are called ZL_<var>.
    The list of created variable names is optionally returned by the function (when parameter macrovar is not empty).

    data:                   Name of the analysis dataset. If empty the active dataset is used.
                            Default: None
    vars:                   String, list or tuple containing the list of variables for which the log is computed.
                            If empty, all variables in the dataset are used (which are assumed all numeric!)
                            Default: ()
    safe:                   Logical: whether to use the safe Log transformation.
                            When True, the transformation applied is:
                                sign(x)*Log(1 + abs(x))
                            When False, the transformation applied is:
                                Log(x) if x > 0, else $SYSMIS
                            Default: True
    prefix:                 Prefix to use for the new variables containing the percentage.
                            Default: "ZL_"
    suffix:                 Suffix to use for the new variables containing the percentage.
                            Default: None
    preplace:               Logical: whether the prefix should replace the initial part of the variable name
                            that is as long as the prefix.
                            Ex: (when True) The percentage on V_VOLUME when prefix = 'P_' is named P_VOLUME.
                            Default: False
    sreplace:               Logical: whether the suffix should replace the end part of the variable name
                            that is as long as the suffix.
                            Ex: (when False) The percentage on V_VOLUME when suffix = '_PCT' is named V_VOLUME_PCT.
                            Default: False
    pposition:              Position from the START of the original variable name at which the prefix STARTS being
                            inserted.
                            Ex: When prefix = 'ZL_', preplace = False, pposition = 3
                            The variable containing the Log transformation of variable V_VOLUME is named V_ZL_VOLUME.
                            Default: 1
    sposition:              Position from the END of the original variable name at which the suffix ENDS being
                            inserted.
                            Ex: When suffix = '_L', preplace = False, pposition = 1
                            The variable containing the Log transformation of variable V_VOLUME is named V_VOLUME_L
                            Ex: When suffix = '_L', preplace = True, pposition = 1
                            The variable containing the Log transformation of variable V_VOLUME_PC is named V_VOLUME_L
                            Ex: When suffix = '_L', preplace = False, pposition = 5
                            The variable containing the Log transformation of variable V_VOLUME_PCT is named V_VOLUME_L_PCT
                            Default: 1
    macrovar:               Name of the Python variable and the SPSS macro variable to store the list of variable names
                            created by the process, separated by new line characters.
                            For the SPSS macro variable name, the nme given is uppercased and a '!' is added at the beginning.
                            Default: "logvars" => SPSS macro variable name: '!LOGVARS'
    test:                   Logical: whether to run the function in test mode. That is shows the generated lines
                            but does not execute them.
                            A True value can also be used to create the SPSS macro variable with the list of
                            transformed variable names.
                            Default: False
    """

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- VARS
    if vars:
        varlist, vars = BuildVarListAndString(vars)
    else:
        # Analyze ALL variables in the dataset
        varlist = spssaux.getVariableNamesList(range(spss.GetVariableCount()))
        vars = "\n".join(varlist)

    #-- PREFIX and SUFFIX
    # The following is to avoid an error when preplace = True and prefix = None for example.
    if prefix == None:
        prefix = ""
    if suffix == None:
        suffix = ""

    #-- PPOSITION and SPOSITION
    # Note that 'None' satisfies the condition '<= 0'
    if pposition <= 0:
        pposition = 1
    if sposition <= 0:
        sposition = 1

    #-- TEST
    if test:
        print "\n--- Test mode execution ---"
    #-------------------------------- Parse input parameters ----------------------------------

    #-- Create the list of Log-transformed variables
    lvarlist = []       # Prefix 'l' stands for LOG
    for var in varlist:
        print "\nProcessing variable " + var + "..."
        if preplace:
            if sreplace:
                lvar = var[:pposition-1] + prefix + var[len(prefix)+pposition-1:len(var)-len(suffix)-sposition+1]   + suffix + var[len(var)-sposition+1:]
            else:
                lvar = var[:pposition-1] + prefix + var[len(prefix)+pposition-1:len(var)-sposition+1]               + suffix + var[len(var)-sposition+1:]
        else:
            if sreplace:
                lvar = var[:pposition-1] + prefix + var[pposition-1:len(var)-len(suffix)-sposition+1]               + suffix + var[len(var)-sposition+1:]
            else:
                lvar = var[:pposition-1] + prefix + var[pposition-1:len(var)-sposition+1]                           + suffix + var[len(var)-sposition+1:]
        lvarlist.append(lvar)

        if safe:
            submitstr = r"""
compute %(lvar)s = ((%(var)s>0)-(%(var)s<0)) * LG10(1 + abs(%(var)s)).
"""
        else:
            submitstr = r"""
if %(var)s > 0.
+ compute %(lvar)s = LG10(%(var)s).
else.
+ compute %(lvar)s = $SYSMIS.
end if.
""" %locals()

        if log or test:
            print(submitstr %locals())
        if not test:
            spss.Submit(submitstr %locals())

    if not test:
        if log:
            print("execute.")
        spss.Submit("execute")
    else:
        print("execute.")
        print "--- Test mode execution ---"

    #-- Show the list of indicator variables.
    if log:
        if test:
            print "The following variables containing the log-transformation would be created in dataset " + data + ":"
        else:
            print "The following variables containing the log-transformation were created in dataset " + data + ":"
        for lvar in lvarlist:
            print lvar
        print "Total: " + str(len(lvarlist))

    #-- Create the SPSS macro variable if required
    if macrovar:
        print "\nMacro variable " + "!" + macrovar.upper() + " created containing the list of log-transformed variables."
        spss.SetMacroValue("!" + macrovar,"\n".join(lvarlist))

        # Return the list of variable names as a Python variable
        print "This list is also RETURNED (needs to be assigned to an external variable by the user) as a Python variable"
        lvarnames = "\n".join(lvarlist)
        return lvarnames
######################################## TransformLog #########################################



##################################### TransformIndicator ######################################
# 2009/01/19
# HISTORY:  (2013/12/09) 
#           - Changed the default value of parameter 'reverse' from True to False, because we normally do NOT
#           remember the existence of this parameter and it is confusing when we use the function in the simplest way.
#           - Fixed the error that the Python variable 'condition' was not found when the number of values passed in 'values'
#           is a tuple or list and does not include "$SYSMIS".
#
# TODO:
# [DONE-2013/08/30] 2013/08/01: Include the possibility of computing indicators of missing values!
#               For this we need to parse parameter 'value' so that when its value is '$SYSMIS'
#               replace the condition expression from "%(var)s = %(values)s" to "missing(%(var)s)"
def TransformIndicator(
    data=None,
    vars=(),
    create=True,
    values=0,
    reverse=False,
    prefix="IN_",
    suffix=None,
    preplace=False,
    sreplace=False,
    pposition=1,
    sposition=1,
    macrovar="indicators",
    test=False,
    log=True):
    """Computes Indicator variables (Flags 0/1) of a list of variables indicating the occurrence or absence
    of a set of values (typically 0, Missing, ...).
    New variables with the prefix given in 'prefix' and suffix given in 'suffix' are created, as many as
    variables listed in 'vars'.
    By default the indicator variables are called IN_<var> and indicate the NON occurrence of the 0 value.
    The list of created variable names is optionally returned by the function (when parameter macrovar is not empty).

    data:                   Name of the analysis dataset. If empty the active dataset is used.
                            Default: None
    vars:                   String, list or tuple containing the list of variables for which the indicator variable is computed.
                            If empty, all variables in the dataset are used (which are assumed all of the same type!)
                            Default: ()
    create:                 Logical: whether each indicator variable needs to be created or already exists in the dataset.
                            Default: True
    values:                 A single value, list or tuple of values of the same type (except when one value is the missing value)
                            to be indicated by the indicator variables.
                            Use "$SYSMIS" (case sensitive! e.g. "$sysmis" is not allowed) to create indicator variables of
                            missing values.
                            Ex: (0, 1, "$SYSMIS")
                            Default: 0
    reverse:                Logical: whether the indicator variable should be reversed, i.e. indicate the
                            NON occurrence of 'value'.
                            Default: True
    prefix:                 Prefix to use for the new variables containing the percentage.
                            Default: "IN_"
    suffix:                 Suffix to use for the new variables containing the percentage.
                            Default: None
    preplace:               Logical: whether the prefix should replace the initial part of the variable name
                            that is as long as the prefix.
                            Default: False
                            Ex: (when True) The percentage on V_VOLUME when prefix = 'P_' is named P_VOLUME.
    sreplace:               Logical: whether the suffix should replace the end part of the variable name
                            that is as long as the suffix.
                            Default: False
                            Ex: (when False) The percentage on V_VOLUME when suffix = '_PCT' is named V_VOLUME_PCT.
    pposition:              Position from the START of the original variable name at which the prefix STARTS being
                            inserted.
                            Ex: When prefix = 'ZL_', preplace = False, pposition = 3
                            The variable containing the Log transformation of variable V_VOLUME is named V_ZL_VOLUME.
                            Default: 1
    sposition:              Position from the END of the original variable name at which the suffix ENDS being
                            inserted.
                            Ex: When suffix = '_L', preplace = False, pposition = 1
                            The variable containing the Log transformation of variable V_VOLUME is named V_VOLUME_L
                            Ex: When suffix = '_L', preplace = True, pposition = 1
                            The variable containing the Log transformation of variable V_VOLUME_PC is named V_VOLUME_L
                            Ex: When suffix = '_L', preplace = False, pposition = 5
                            The variable containing the Log transformation of variable V_VOLUME_PCT is named V_VOLUME_L_PCT
                            Default: 1
    macrovar:               Name of the Python variable and the SPSS macro variable to store the list of variable names
                            created by the process, separated by new line characters.
                            For the SPSS macro variable name, the nme given is uppercased and a '!' is added at the beginning.
                            Default: "indicators" => SPSS macro variable name: '!INDICATORS'
    test:                   Logical: whether to run the function in test mode. That is shows the generated lines
                            but does not execute them.
                            A True value can also be used to create the SPSS macro variable with the list of
                            transformed variable names.
                            Default: False
    """

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- VARS
    if vars:
        varlist, vars = BuildVarListAndString(vars)
    else:
        # Analyze ALL variables in the dataset
        varlist = spssaux.getVariableNamesList(range(spss.GetVariableCount()))
        vars = "\n".join(varlist)

    #-- VALUES and REVERSE
    # Construct the condition that defines the 1s of the Indicator variables considering the possibility of $SYSMIS among the values.
    condition = ""
    if isinstance(values,(list,tuple)):
        # Convert 'values' to LIST so that operations below can be applied (e.g. del to delete an element from a list)
        values = list(values)
        # Check whether $SYSMIS was given by the user as a value to match (note that only "$SYSMIS" is caught by not $sysmis or $Sysmis, etc.)
        # (this is done like this because below I delete the element $SYSMIS from the list by simply using 'del' of the values.index("$SYSMIS")
        # which returns an error when the value in parenthesis is not found in the list)
        # Note that this will match values of the variables with ANY missing value, either system missing value or user missing value.
        # Note also that I don't expect the user to pass the string "MISSING" to match missing values (which would suggest that also user missing values are matched)
        # because a variable may have the value "MISSING" assigned...(!)
        if values.count("$SYSMIS") > 0:
            condition = "missing(%(var)s)"
            # Remove the value $SYSMIS from the list of values for matching
            del values[values.index("$SYSMIS")]
        # If there are still values to match (e.g. after removing the missing value $SYSMIS), continue building the expression
        if len(values) > 0:
            condition = condition + (len(condition) > 0 and " or" or "") + " ANY(%(var)s,"
            for i in range(len(values)):
                if isinstance(values[i],str):
                    quote = "'"
                else:
                    quote = ""
                if i == 0:
                    condition = condition + "%(quote)s" %locals() + str(values[i]) + "%(quote)s" %locals()
                else:
                    condition = condition + ",%(quote)s" %locals() + str(values[i]) + "%(quote)s" %locals()
            condition = condition + ")"
    else:
        if isinstance(values, str) and values.upper() == "$SYSMIS":
            condition = "missing(%(var)s)"
        else:
            condition = "%(var)s = %(values)s"
    if reverse:
        condition = "not " + "(" + condition + ")"

    #-- PREFIX and SUFFIX
    # The following is to avoid an error when preplace = True and prefix = None for example.
    if prefix == None:
        prefix = ""
    if suffix == None:
        suffix = ""

    #-- PPOSITION and SPOSITION
    # Note that 'None' satisfies the condition '<= 0'
    if pposition <= 0:
        pposition = 1
    if sposition <= 0:
        sposition = 1

    #-- TEST
    if test:
        print "\n--- Test mode execution ---"
    #-------------------------------- Parse input parameters ----------------------------------

    #-- Create the list of indicator variables
    ivarlist = []       # Prefix 'i' stands for INDICATOR
    for var in varlist:
        if preplace:
            if sreplace:
                ivar = var[:pposition-1] + prefix + var[len(prefix)+pposition-1:len(var)-len(suffix)-sposition+1]   + suffix + var[len(var)-sposition+1:]
            else:
                ivar = var[:pposition-1] + prefix + var[len(prefix)+pposition-1:len(var)-sposition+1]               + suffix + var[len(var)-sposition+1:]
        else:
            if sreplace:
                ivar = var[:pposition-1] + prefix + var[pposition-1:len(var)-len(suffix)-sposition+1]               + suffix + var[len(var)-sposition+1:]
            else:
                ivar = var[:pposition-1] + prefix + var[pposition-1:len(var)-sposition+1]                           + suffix + var[len(var)-sposition+1:]
        ivarlist.append(ivar)
    
    #-- If not existent, create the new indicator variables with format F1.0
    ivars = " ".join(ivarlist)
    if create:
        submitstr = "numeric %(ivars)s (F1.0)."
        if not test:
            spss.Submit(submitstr %locals())
        else:
            print(submitstr %locals())

    for i in range(len(varlist)):
        # Note that I need to iterate on the index that references varlist and ivarlist and NOT directly on 
        # ivarlist because %(condition)s needs to evaluate correctly to the corresponding input variable.
        ivar = ivarlist[i]
        var = varlist[i]
        print create and "\nCreating" or not create and "\nUpdating" + " indicator variable of " + str(values) + " for variable " + var + "..."
        submitstr = r"compute %(ivar)s = %(condition)s." %locals()

        if log or test:
            print(submitstr %locals())
        if not test:
            spss.Submit(submitstr %locals())

    if not test:
        if log:
            print("execute.")
        spss.Submit("execute")
    else:
        print("execute.")
        print "--- Test mode execution ---"

    #-- Show the list of indicator variables.
    if log:
        if test:
            print "The following indicator variables would be " + (create and "created" or not create and "updated") + " in dataset " + data + ":"
        else:
            print "The following indicator variables were " + (create and "created" or not create and "updated") + " in dataset " + data + ":"
        for ivar in ivarlist:
            print ivar
        print "Total: " + str(len(ivarlist))

    #-- Create the SPSS macro variable if required
    if macrovar:
        print "\nMacro variable " + "!" + macrovar.upper() + " created containing the list of indicator variables."
        spss.SetMacroValue("!" + macrovar,"\n".join(ivarlist))
        
        # Return the list of variable names as a Python variable
        print "This list is also RETURNED (needs to be assigned to an external variable by the user) as a Python variable"
        ivarnames = "\n".join(ivarlist)
        return ivarnames
##################################### TransformIndicator ######################################



####################################### TrimVariables #########################################
# 2013/07/10
# HISTORY:  (2013/08/14)
#           - Added new parameters 'newvar', 'suffix' and 'macrovar' in order to store the trim variables into new variables, instead of changing the original variables.
#           The 'macrovar' parameter names two SPSS macro variables and two Python variables, containing respectively the list of variables to be trimmed and the list of
#           new variable names containing the trimmed output.
def TrimVariables(
    data,
    dataSummary="summary",
    nameVar="var",
    thrVar="@99",
    extremeVar="Max",
    operator=">",
    threshold=2,
    condition="extremeVar/thrVar > 2",
    newvar=True,                              # Whether to trim the variables into new variables to avoid overriding
    round=False,                              # Whether to round the result after trimming (useful when variables to trim are integer!)
    suffix="Trim",
    macrovar=("trimvars","newtrimvars"),
    test=True,
    log=True):
    """
    Trim variables based on the summary dataset generated using the Summary() function.
    """

    # Check if the variable names specified in nameVar, thrVar and extremeVar exist in the dataSummary dataset
    found, varsNotFoundList = CheckVariables([nameVar, thrVar, extremeVar], data=dataSummary, casesensitive=True, log=False)
    if not found:
        errmsg = errmsg + "\nTRIMVARIABLES: ERROR - The following " + str(len(varsNotFoundList)) + " variables were not found in dataset " + data
        for v in varsNotFoundList:
            errmsg = errmsg + "\n\t" + v
        print "\nTRIMVARIABLES: (note that the case of the variable should be preserved for this function to work!)"
        return

    # Activate summary dataset to read statistics of variables
    spss.Submit("dataset activate " + dataSummary)
    summary = spssdata.Spssdata(indexes=(nameVar, thrVar, extremeVar))    # NOTE: THE VARIABLE NAMES IN THE INPUT DATASET ARE CASE SENSITIVE!

    # Replace any occurrences of the parameter names in 'condition' with their corresponding values
    # CONDITION is not used for now... The idea is that the user can set the condition by which a variable is trimmed! (which now is hardcoded below as ext/thr > threshold)
    # But to do this I need to find out how to evaluate a string expression in Python.
#    condition = condition.lower().replace("thrvar","%(thrVar)s") %locals()
#    condition = condition.lower().replace("trimvar","%(trimVar)s") %locals()

    # Iterate over each record in the dataset (this is the SAS DATA STEP!)
    submitstr = ""
    newline = ""
    tvarlist = []
    newtvarlist = []
    for case in summary:
        var = case[0].strip()
        thr = case[1]
        ext = case[2]
        # Hardcoded condition for trimming --> this should be changed by using the 'condition' parameter passed by the user, but still don't know how to do it...
        # Note that I first check if any of the values ext and thr read from the summary dataset are NOT None (which is the case when their values is missing ('.' in SPSS)
        # If I don't do this check the operation ext/thr raises an error.
        if ext and thr and ext/thr > threshold:
            tvarlist.append(var)
            if newvar:
                newline = "compute " + var + suffix + " = " + var + "."
                var = var + suffix
            newtvarlist.append(var)   # Note that when newvar=False, the list in newtvarlist coincides with tvarlist
            newline = newline + "\n" + "if (" + var + " " + operator + " " + str(threshold) + "*" + str(thr) + ") " + var + " = " + (round and "RND(" or "") + str(threshold) + "*" + str(thr) + (round and ")" or "") + "."
            comment = "* " + extremeVar + " Value = " + str(ext) + "."
            submitstr = submitstr + "\n" + newline + "\n" + comment
    # Delete handle to dataset 'dataSummary'
    del summary

    if len(newline) == 0:
        # No variables to trim
        submitstr = "\nNOTE: There are no variables to trim satisfying the specified condition of " + extremeVar + " > " + str(threshold) + "*" + thrVar + "."
        test = True   # Treat this run as a test so that no code is submitted or run in SPSS.
    else:
        submitstr = "\ndataset activate " + data + "." + submitstr + "\n" + "execute."

    if log or test:
        print(submitstr)
    if not(test):
        spss.Submit(submitstr)

    #-- Create the SPSS macro variable if required
    if macrovar:
        print "\nMacro variable " + "!" + macrovar[0].upper() + " created containing the list of variables that were trimmed."
        spss.SetMacroValue("!" + macrovar[0],"\n".join(tvarlist))
        print "\nMacro variable " + "!" + macrovar[1].upper() + " created containing the list of variables that store the trimmed output."
        spss.SetMacroValue("!" + macrovar[1],"\n".join(newtvarlist))
        
        # Return the list of variable names as a Python variable
        print "These lists are also returned in two Python variables."
        tvarnames = "\n".join(tvarlist)
        newtvarnames = "\n".join(newtvarlist)
        return tvarnames, newtvarnames
####################################### TrimVariables #########################################



##################################### PlotTargetVSCat #########################################
# 2013/07/23
# Taken from pyGrafBivariante by NAT (by Antoine Thibaud)
# HISTORY: (2013/08/02)
#          - Added the capability of saving the data used to generate the graph.
#
# TODO:
# - (2013/07/23) Include ERROR BARS on each measurement of the average target value
# - (2014/01/06) There is a way to sort the variable categories by another variable that can be done directly with GGRAPH and GPL.
#                The goal would be to implement that simplified way in this function.
#                Note however that the advantage of my function is that it creates new variable values (that adds a prefix
#                to the original value specifying the ranking of the variable value in terms of the target value) that aids
#                in writing a RECODE command to group categories with similar target value.
#                Here is the example posted by Jon Peck at the SPSS Users LinkedIn group, on how to do this sorting:
#   << Since the data are being aggregated by GGRAPH before the GPL code sees the data, sorting by count doesn't work.
#   Sort by the Y-axis variable mean. Here is a complete example using the employee data.sav file.
#   It uses the count variable rather than counting within the GPL. 
#
#   GGRAPH 
#   /GRAPHDATASET NAME="graphdataset" VARIABLES=educ COUNT()[name="COUNT"] MISSING=LISTWISE REPORTMISSING=NO
#   /GRAPHSPEC SOURCE=INLINE.
#   BEGIN GPL
#   SOURCE: s=userSource(id("graphdataset"))
#   DATA: educ=col(source(s), name("educ"), unit.category())
#   DATA: COUNT=col(source(s), name("COUNT"))
#   GUIDE: axis(dim(1), label("Educational Level (years)"))
#   GUIDE: axis(dim(2), label("Count"))
#   SCALE: cat(dim(1), sort.statistic(summary.mean(COUNT)))
#   SCALE: linear(dim(2), include(0))
#   ELEMENT: interval(position(educ*COUNT), shape.interior(shape.square))
#   END GPL. 
#   >>
#   Note that sorting by count is done by the SCALE: function "sort.statistic(summary.mean(COUNT))".
#
#   Here is how I made it work for what this function is currently doing:
#   (note that to sort we need to use a summary function (like summary.mean() in this case), which in this case
#   applies to the already summarized value y (wihch is the mean of Malo12), but of course that's not a problem!)
#   (note also the use of the reverse() function with the SCALE: cat() function so that the values are ordered by
#   decreasing values of summary.mean(y))
#     
#     GGRAPH
#     /GRAPHDATASET NAME="graphdataset" VARIABLES=VinculacionUlt12m[LEVEL=NOMINAL] COUNT()[name="N"] MEAN(Malo12)[name="mean_Malo12"] MISSING=LISTWISE REPORTMISSING=NO
#     /GRAPHSPEC SOURCE=INLINE.
#     BEGIN GPL
#     SOURCE: s=userSource(id("graphdataset"))
#     DATA: x=col(source(s), name("VinculacionUlt12m"), unit.category())
#     DATA: n=col(source(s), name("N"))
#     DATA: y=col(source(s), name("mean_Malo12"))
#     GUIDE: axis(dim(1), label("VinculacionUlt12m"))
#     GUIDE: axis(scale(y1), label("Count"), color(color."FC924A"))
#     GUIDE: axis(scale(y2), label("mean of Malo12"), color(color."2EB848"), opposite())
#     SCALE: cat(dim(1), sort.statistic(summary.mean(y)), reverse())
#     SCALE: y1 = linear(dim(2), include(0))
#     SCALE: y2 = linear(dim(2), include(0))
#     ELEMENT: interval(position(x*n), shape.interior(shape.square), color.interior(color."FC924A"), scale(y1))
#     ELEMENT: line(position(x*y), missing.wings(), color.interior(color.green), scale(y2))
#     END GPL.
def PlotTargetVSCat(
    data=None,
    where=(),
    target="y",
    var="x",
    stat="mean",
    n="n",
    out=None,
    save=False,
    saveformat="sav",
    savedir=None,
    tempdir=None,
    log=True):
    """ Plots a Binary or Numeric target variable in terms of the categories of ONE categorical variable.
    """

    #-------------------------------- Parse input parameters ----------------------------------
    #-- Check type of input parameters
    error = False
    errmsg = ""     # This variable collects all the generated error messages to be shown before exiting (return)
    
    if data and not isinstance(data,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if where and not isinstance(where,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter WHERE must be of type string (value given: " + str(where) + ")"
        error = True
    if not isinstance(target,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter TARGET is required and must be of type string (value given: " + str(target) + ")"
        error = True
    if not isinstance(var,str):
        errmsg = errmsg + "\nPARTIALPLOT: ERROR - Parameter VAR is required and must be of type string (value given: " + str(var) + ")"
        error = True
    if not savedir:
        savedir = tempdir_

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- OUT
    if out:
        outdata = out
    else:
        outdata = data + "_" + target + "_VS_" + var

    #---------------------------------------- Summary statistics ------------------------------
    Summary(
        data=data,
        where=where,
        by=var,
        vars=target,
        stats=(stat, "std"),
        percentiles=(),
        format="COMMA7.3",
        out=outdata,
        save=False,
        savedir=None,
        tempdir=None,
        log=True)
    spss.Submit("""
dataset activate %(outdata)s.
sort cases by %(stat)s (D).""" %locals())
    # Add a number in front of the categorical variable value so that this order is kept in the graph and rename 'Mean' with a name which is not a statistic keyword
    spss.Submit("rename variables (%(stat)s=%(stat)s_%(target)s." %locals())
    spss.Submit("string %(var)s_(A255)." %locals())
    spss.Submit("compute %(var)s_ = concat(replace(string($CASENUM,F5.0),' ','0'),'-', rtrim(%(var)s))." %locals())
    spss.Submit("execute.")
    # Update the name of the analyzed variable with the name containing the underscore at the end
    # (which has just been created having a number concatenated with the actual value of the analyzed variable)
    var = var + "_"
    
    # Save the data if requested
    if save:
        print "The data used to make the graph is saved in " + savedir + " as:"
        print "\t" + outdata + "." + saveformat.lower()
        print "\n"
        Save(data=outdata, format=saveformat, dir=savedir)
    #---------------------------------------- Summary statistics ------------------------------


    #---------------------------------------- Make the plot -----------------------------------
    spss.Submit("dataset activate " + outdata)
    submitstr = r"""
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=%(var)s[LEVEL=NOMINAL] %(stat)s_%(target)s %(n)s MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: x=col(source(s), name("%(var)s"), unit.category())
  DATA: n=col(source(s), name("%(n)s"))
  DATA: y=col(source(s), name("%(stat)s_%(target)s"))
  GUIDE: axis(dim(1), label("%(var)s"))
  GUIDE: axis(scale(y1), label("Count"), color(color."FC924A"))
  GUIDE: axis(scale(y2), label("%(stat)s of %(target)s"), color(color."2EB848"), opposite())
  SCALE: y1 = linear(dim(2), include(0))
  SCALE: y2 = linear(dim(2), include(0))
  ELEMENT: interval(position(x*n), shape.interior(shape.square), color.interior(color."FC924A"), scale(y1))
  ELEMENT: line(position(x*y), missing.wings(), color.interior(color.green), scale(y2))
END GPL.
""" %locals()
    print(submitstr)
    spss.Submit(submitstr)
    #---------------------------------------- Make the plot -----------------------------------
    
    # Activate the input dataset so that the user can continue doing processes on it without having to activate it again
    spss.Submit("dataset activate " + data)
##################################### PlotTargetVSCat #########################################



###################################### ModelCollin ############################################
# 2013/11/19
def ModelCollin(
    data=None,
    target="y",
    vars=(),
    method="VIF",
    thr=10,
    out=None,
    save=False,
    saveformat="sav",
    savedir=None,
    log=True,
    output=False):
    """
    Detect highly collinear variables using linear regression and iteratively remove those variables one by one.
    The function returns 2 strings with variable lists, each variable on a different line:
    - the first string contains the remaining non-collinear variables
    - the second string contains the variables that have been detected as highly collinear
    Ex:
    varsOK, varsCollin = ModelCollin(target="dq", vars="x1 x2 x3 x4", method="VIF", thr=10)

    data:               Name of the dataset to analyze. If empty the active dataset is used.
                        Default: None
    target:             Target variable for the linear regression used to perform the collinearity analysis.
                        This variable is irrelevant for the method but it has to be specified and must be numeric.
                        Default: "y"
    vars:               String, list or tuple containing the variable names to analyze for collinearities.
                        They must ALL be numeric variables.
                        Default: ()
    method:             Method for the collinearities analysis.
                        At this moment only one method is allowe: VIF (Variable Inflation Factor) which uses the
                        REGRESSION command in SPSS with the COLLIN and TOL options in the STATISTICS subcommand.
                        Default: "VIF"
    thr:                Threshold used for the collinearity detection method to consider that a variable is collinear
                        with others.
                        Default: 10 (this currently apply to the VIF value)
    out:                Output dataset containing the collinearity analysis.
                        Default: None
    save:               Logical: whether the table containing the collinearity analysis at each step should be saved to a file.
                        Default: False
    saveformat:         Format for the output file.
                        Possible values: SAV, XLS (not case sensitive)
                        Default: "sav"
    savedir:            Directory where the output dataset is saved
                        Default: global variable tempdir_ defined at the beginnig of this module
    log:                Logical: whether to show messages in the log.
                        Default: True
    output:             Logical: whether to show output in the viewer.
                        Default: False
    """
    
    #------------------------------ Parse input parameters -----------------------------------
    error = False
    errmsg = ""     # This variable collects all the generated error messages to be shown before exiting (return)

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nMODELCOLLIN: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if method and not isinstance(method,str):
        errmsg = errmsg + "\nMODELCOLLIN: ERROR - Parameter METHOD must be of type string (value given: " + str(method) + ")"
        error = True

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)
    else:
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- VARS
    if vars:
        varlist, vars = BuildVarListAndString(vars)
    else:
        # Analyze ALL variables in the dataset (except the target variable)
        varlist = spssaux.getVariableNamesList(range(spss.GetVariableCount()))
        varlist.remove(target)
        vars = "\n".join(varlist)

    #-- METHOD
    if method.lower() != "vif":
        print "\nMODELCOLLIN: WARNING - The METHOD parameter has an unsupported value (" + method + ")."
        print "\nMODELCOLLIN: The collinearity detection will be performed using VIF (Variance Inflation Factor)."
        method = "vif"

    #-- OUT
    # Name of dataset to store the iteration history
    if out:
        outdata = out
    else:
        outdata = "@collin"
    # Name of dataset to store the variables removed because of high collinearity
    outdataRemoved = outdata + "Removed"

    #-- SAVEFORMAT, SAVEDIR, TEMPDIR
    if not saveformat:
        saveformat = "sav"
    if not savedir:
        savedir = tempdir_

    #-- Show the parameters of execution of the macro
    if log:
        print "\nFunction call resolves to:"
        print "ModelCollin("
        print "\tdata =         ", data
        print "\ttarget =       ", target
        print "\tvars =         ", "\n\t\t\t".join(varlist)
        print "\tmethod =       ", method
        print "\tthr =          ", thr
        print "\tout =          ", outdata
        print "\tsave =         ", save
        print "\tsaveformat =   ", saveformat
        print "\tsavedir =      ", savedir
        print "\tlog =          ", log
        print "\toutput =       ", output
        print ")\n"
        if save:
            print "The following data files will be created in " + savedir + ":"
            print "\t" + outdata + "." + saveformat.lower() + "\t\t (contains the iteration history with collinearity indicator at each step)"
            print "\t" + outdataRemoved + "." + saveformat.lower() + "\t (contains the variables removed at each step)"
            print "\n"
    #------------------------------ Parse input parameters -----------------------------------

    if not output:
        if log:
            spss.Submit("OMS /select ALL except=LOGS /destination viewer = NO.")
        else:
            spss.Submit("OMS /select ALL /destination viewer = NO.")
    if data:    
        spss.Submit("dataset activate " + data)
    # Make a copy of the analyzed data and keep the relevant variables
    spss.Submit(r"""
dataset copy @data.
dataset activate @data.
add files file=* /keep=%(target)s %(vars)s.
execute.
""" %locals())
    # Remove variable labels because these are used to fill in the Var2 variable of the output generated by the REGRESSION command
    # and it will make the whole process fail.
    syntax = "variable labels "
    for v in varlist:
        syntax = syntax + v + " '' "
    spss.Submit(syntax)

    # Create the dataset to store the list of removed variables because of high collinearities
    # The following command creates an empty dataset with 2 columns: var, VIF.
    # Note that the maximum length for variable names in SPSS is 64 and this is the reason for using 'var (A64)'
    spss.Submit("DATA LIST FREE /iter (F5.0) var (A64) VIF (F10.2)")
    spss.Submit("dataset name " + outdataRemoved)

    # Start iteration for detecting and removing highly collinear variables
    CollinValue = 1.1*thr
    iter = 0
    varCollinList = []
    while CollinValue > thr:
        iter = iter + 1
        spss.Submit(r"""
dataset declare @vif.
OMS /tag='VIF'
/select tables
/if subtypes='Coefficients'
/destination format=sav outfile=@vif viewer=NO.
dataset activate @data.
REGRESSION variables = %(target)s %(vars)s
  /statistics COLLIN TOL
  /dependent=%(target)s
  /method=ENTER %(vars)s.
OMSEND tag='VIF'.""" %locals())
        
        # Sort variables by decreasing VIF
        spss.Submit(r"""
dataset activate @vif.
sort cases by VIF(D).
numeric iter (F5.0).
compute iter = %(iter)s.
execute.""" %locals())

        # Create the dataset object pointing to the VIF dataset created by the regression
        dataObj = spssdata.Spssdata(indexes=["Var2", "VIF"])    # Var2 contains the variable name
        case = dataObj.fetchone()
        dataObj.close()
        var = case[0].strip()
        CollinValue = case[1]
        if CollinValue > thr:
            # Remove the variable with largest VIF and continue iteration by performing a new linear regression step without that variable
            if log:
                print "MODELCOLLIN: Iteration " + str(iter) + " - Variable " + var + " is removed (VIF = " + str(CollinValue) + ")."
            varCollinList.append(var)
            varlist.remove(var)
            vars = "\n".join(varlist)
            # Add the variable removed to the dataset 'outdataRemoved'
            spss.Submit("""
DATA LIST FREE /iter (F5.0) var (A64) VIF (F10.2).
BEGIN DATA.
%(iter)s, %(var)s, %(CollinValue)s 
END DATA.
dataset name @vifRemoved.
dataset activate %(outdataRemoved)s.
add files file=* /file=@vifRemoved.
execute.
dataset close @vifRemoved.
""" %locals())
        else:
            # Stop the iteration
            if log:
                print "\nMODELCOLLIN: The maximum collinearity value (" + method + "=" + str(CollinValue) + ") among the remaining variables"
                print "MODELCOLLIN: is smaller than the specified threshold (thr=" + str(thr) + ")."
                print "MODELCOLLIN: Iteration stops at iteration " + str(iter) + "."
            CollinValue = 0

        # Add the current VIF values to the output dataset 'outdata'
        if iter == 1:
            spss.Submit(r"""
add files file=@vif /rename=(Var2=var) /keep=iter var VIF.
execute.
dataset name %(outdata)s.""" %locals())
        else:
            spss.Submit(r"""
dataset activate %(outdata)s.
add files file=* /file=@vif /rename=(Var2=var) /keep=iter var VIF.
execute.""" %locals())
        spss.Submit("dataset close @vif")

    # Close all open OMS (in order to e.g. restore the output to the viewer)
    spss.Submit("OMSEND")

    #---------------------------------- Finalize process --------------------------------------
    # Save output dataset containing the collinearity analysis, if requested
    if save:
        Save(data=outdata, format=saveformat, dir=savedir)
        Save(data=outdataRemoved, format=saveformat, dir=savedir)

    # Close temporary datasets
    spss.Submit("dataset close @data")

    # Show the list of removed variables sorted alphabetically
    if log:
        spss.Submit("dataset activate " + outdataRemoved)
        print "\nMODELCOLLIN: List of variables removed iteratively because of " + method + " > " + str(thr) + ":"
        spss.Submit("list")
        print "\n"
    
    # Sort removed variables alphabetically
    spss.Submit(r"""
dataset activate %(outdataRemoved)s.
sort cases by var.
execute.""" %locals())

    # Activate the input dataset so that the user can continue doing processes on it without having to activate it again
    spss.Submit("dataset activate " + data)

    # Return the list of remaining (non-collinear) variables
    varlist.sort()
    return "\n".join(varlist), "\n".join(varCollinList)
###################################### ModelCollin ############################################



########################################### Score #############################################
# 2013/09/05
# HISTORY:  (2013/10/11)
#           - Added the generation of the SPSS syntax that can be used to compute the score in the future (without re-calling this function!).
#           - Added parameter 'test' to choose between computing the score or just generating the SPSS syntax in the output.
#           (2014/01/07)
#           - Fixed three errors:
#             - The incorrect generation of the COMPUTE command to compute the score when a variable that comes before "constant"
#               in alphabetical order (this was fixed simply by storing the "constant" or intercept value into a variable called beta0
#               and always initializing beta0 to 0 in case the model has no intercept)
#             - An execution error occurring when test=False, when the model contains categorical variables taking character values
#               (the error was fixed by appropriately referencing the dictionary entry of varTypeDict() to varPrev instead of k[0]
#               in the compound IF that is called inside the block 'if not(test)')
#             - A "variable not found error" in catDict() when the 'codings' dataset was created under an SPSS running under
#             with the following setting: Edit -> Options -> Output Labels -> Pivot Table Labeling set to "Names and Labels"
#             since in that case the variable name in the 'codings' dataset is followed by the variable label!! (after a blank space)
#           - Added parameters 'debug' and 'debugmaxcases' (following a suggestion by Antoine) in order to help the user find
#           an error in the predicted value when values are not as expected.
#
# TODO:
# - 2014/01/13: Show the model parameters in the score equation in the order that is shown in the SPSS output.
#               Note that, although the parameters are read from the 'params' dataset in the same order that they are stored in
#               the SPSS output, it is not so direct to retrieve them in the same order when generating the scoring function
#               because the order in which the keys are retrieved (when doing 'for k in dict.keys()') is not guaranteed.
#               I have solve this uncertainty in the code below by creating a new variable to store the keys, sort this variable
#               in alphabetical order, and finally access the keys through it. Therefore a possible solution to implement the
#               above is to extend the parameter dictionary key to 2-dimension to 3-dimension, where the first dimension is
#               the order in which the parameters are stored, sort the keys alphabetically as usual, and finally access the keys
#               in this order. Note that the parameters for the categorical variables should stay all together in order to check
#               for a wrong category in the data and to check if the currenty category is the reference category. Doing what I
#               just described would still keep the parameters of the categorical variables all together, so no problem.
#               Ref: Antoine Thibaud
def Score(
    data=None,
    dataParams=None,
    dataCodings=None,
    fileParams=None,
    fileCodings=None,
    dir=None,
    score="pred",
    replace=False,
    model="linear",
    step="last",
    test=False,
    debug=False,
    debugmaxcases=10):
    """
    Score a dataset using a linear or binary logistic regression model.
    Output: A string containing the SPSS syntax that can be used to generate the score.
    Assumptions:
    - The model has been adjusted either using the REGRESSION command (for model="linear") or the LOGISTIC REGRESSION command
    on a BINARY target (for model="logit" or "logistic")
    - All variables used in the model already exist in the dataset to score.
    - In case categorical variables exist in the model, they have been coded using the reference level approach (which is the default in SPSS)

    data:               Name of the dataset to score. If empty the active dataset is used.
                        Default: None
    dataParams          Name of the dataset (ALREADY OPEN) containing the estimated parameters of the model.
                        Default: None
    dataCodings         Name of the dataset (ALREADY OPEN) containing the codings of the categorical variables in the model (if any).
                        Assumptions: the reference level coding has been used during model development.
                        Default: None
    fileParams          Name of the FILE containing the estimated parameters of the model.
                        Default: None
    fileCodings         Name of the file containing the codings of the categorical variables in the model (if any)
                        Assumptions: the reference level coding has been used during model development.
                        Default: None
    dir                 Directory where BOTH the file specified in fileParams and the file specified in fileCodings are searched for.
                        Default: None
    score               Name of the variable to be created in the dataset indicated in parameter DATA
                        to hold the predicted value (or score) of the model.
                        Default: "pred"
    replace:            Logical: whether the 'score' variable should be replaced in the dataset to score when it exists.
                        If replace=False and test=False and the 'score' variable already exists in the dataset to score,
                        execution stops and an error is raised.
                        Default: False
    model:              Type of fitted model.
                        Possible values are: "linear", "logit", "logistic" (not case sensitive)
                        Default: "linear"
    step:               String or number identifying the step number of the regression model for which parameters are read.
                        Possible values:
                        - when string: "first" or "last"
                        - when number: non-negative integer (e.g. the number is 1 when the method used during the regression model fit is ENTER)
                        Default: "last"
    test:               Logical: whether to run the function in test mode. That is shows the SPSS syntax needed to generate the score
                        but does not computes the score.
                        Default: False
    debug:              Logical: whether to run the function in debug mode. This means that a small set of first cases of the data
                        to score are shown in the output as each variable is read from the dataset and contributes to the score.
                        One separate line is output for each variable.
                        The number of cases shown is controlled by parameter 'debugmaxcases'.
                        For this parameter to have an effect set test=False
                        Default: False
    debugmaxcases:      Maximum number of cases to show in the output in debug mode.
                        Set it to None or 0 to show all the cases in the dataset.
                        Default: 10
    """

    #------------------------------ Parse input parameters -----------------------------------
    error = False
    errmsg = ""     # This variable collects all the generated error messages to be shown before exiting (return)

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if dataParams is None and fileParams is None:
        errmsg = errmsg + "\nSCORE: ERROR - Either DATAPARAMS or FILEPARAMS needs to be passed indicating the dataset that contains the model parameters"
        error = True
    if dataParams and not isinstance(dataParams,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter DATAPARAMS must be of type string (value given: " + str(dataParams) + ")"
        error = True
    if dataCodings and not isinstance(dataCodings,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter DATACODINGS must be of type string (value given: " + str(dataCodings) + ")"
        error = True
    if fileParams and not isinstance(fileParams,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter FILEPARAMS must be of type string (value given: " + str(fileParams) + ")"
        error = True
    if fileCodings and not isinstance(fileCodings,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter FILECODINGS must be of type string (value given: " + str(fileCodings) + ")"
        error = True
    if dir and not isinstance(dir,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter DIR must be of type string (value given: " + str(dir) + ")"
        error = True
    if not isinstance(replace,bool):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter REPLACE must be a boolean (value given: " + str(replace) + ")"
        error = True
    if model and not isinstance(model,str):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter MODEL must be of type string (value given: " + str(model) + ")"
        error = True
    if step and not isinstance(data,int) and step < 0:
        errmsg = errmsg + "\nSCORE: ERROR - Parameter STEP must be a non-negative integer (value given: " + str(step) + ")"
        error = True
    if not isinstance(test,bool):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter TEST must be a boolean (value given: " + str(test) + ")"
        error = True
    if not isinstance(debug,bool):
        errmsg = errmsg + "\nSCORE: ERROR - Parameter DEBUG must be a boolean (value given: " + str(debug) + ")"
        error = True

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    if not(data):
        data = spssaux.getActiveDatasetName()
    if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
        data = "None"

    #-- DATAPARAMS and FILEPARAMS
    # Give priority to fileParams when both are passed by the user
    if dataParams is not None and fileParams is not None:
        dataParams = None
        print "\tSCORE: WARNING - Both parameters DATAPARAMS and FILEPARAMS were passed. The model parameters will be read from the file specified in FILEPARAMS."
    if fileParams is not None:
        # Read the parameters dataset
        spss.Submit("GET FILE='" + ((dir + "/") or "") + fileParams + "'")
        spss.Submit("dataset name @params_")
        dataParams = "@params_"
        
    #-- DATACODINGS and FILECODINGS
    # Give priority to fileCodings when both are passed by the user
    if dataCodings is not None and fileCodings is not None:
        dataCodings = None
        print "\tSCORE: WARNING - Both parameters DATACODINGS and FILECODINGS were passed. The model parameters will be read from the file specified in FILECODINGS."
    if fileCodings is not None:
        # Read the parameters dataset
        spss.Submit("GET FILE='" + ((dir + "/") or "") + fileCodings + "'")
        spss.Submit("dataset name @codings_")
        dataCodings = "@codings_"

    #-- DIR
    # Add a \\ at the end of the directory passed by the user for proper concatenation with the file names
    if dir:
        dir = dir + "/"

    #-- MODEL (this needs to come first than the parsing of VARCLASS and VARNUM because these can be empty in the case of linear regression, but not so in the logistic regression case)
    if model.lower() == "logit" or model.lower() == "logistic":
        model = "logit"
    else:
        model = "linear"

    #-- STEP
    if isinstance(step,str) and step.lower() != "first" and step.lower() != "last":
        step = "last"
        print "\tSCORE: WARNING - The string value passed in parameter STEP is not valid. The LAST regression step will be used to read the parameters of the model."
        
    #-- TEST
    if test:
        print "\n--- Test mode execution: NO variable will be generated in the input dataset containing the score ---"

    #-- DEBUG
    if debug:
        showAll = False
        # Define the number of spaces to leave maximum when showing the predicted values, after each variable evaluation in debug mode
        # so that the predicted value is aligned.
        tabSpace = 70
        if debugmaxcases is None or debugmaxcases == 0:
            showAll = True
        elif not isinstance(debugmaxcases,int) and debugmaxcases < 0:
            print "\nSCORE: WARNING - Parameter DEBUGMAXCASES is not a positive integer (value given: " + str(debugmaxcases) + ")"
            print "\nSCORE: The default value of 10 cases will be used in debug mode"
    #------------------------------ Parse input parameters -----------------------------------


    ### 0.- Read the variable dictionary of the input dataset
    ### The result of this step is a dictionary called varTypeDict whose key is the variable name and whose value is the variable type
    ### as returned by the variableType method of the VariableDict class (which is a number: 0 for numeric, >0 for string where the number is its length)
    ### This is used when generating the expression giving the score, where we need to check if a categorical variable is string or numeric)
    spss.Submit("dataset activate " + data)
    varsDict = spssaux.VariableDict()
    # Create the variable type dictionary
    varTypeDict = {}
    existsScore = False
    for v in varsDict.range():
        # v contains the variable name
        varTypeDict[v] = varsDict.variableType(v)
        if v.lower() == score.lower().strip():
            existsScore = True

    ### 1.- Process dataCodings dataset, containing the Categorical variable codings
    ### The result of this step is a dictionary called catDict whose key is a 2-uple of the form (<variable name>, <index of categorical variable>) and
    ### whose value is the actual value taken by the cateogorical variable for the corresponding index value (possible index values are 1, 2, 3, ..., L-1,
    ### where L is the number of levels of the categorical variable)
    if dataCodings:
        # Process the data in the Categorical Codings dataset
        spss.Submit("dataset activate " + dataCodings)
        # Get variable names matching the pattern @*, which contain the columns specifying the categorical coding
        # Note that the output of spssaux.VariableDict is NOT really a dictionary because I cannot apply the method keys() to it.
        # It is neither a list because I cannot concatenate it with another list. It is of class VariableDict.
        # This is why I use the range() method of the VariableDict class to create a list from the VariableDict object (which I need for variable manipulation below).
        codingVarsDict = spssaux.VariableDict(dataset=dataCodings, pattern=r"@+")
        codingVars = codingVarsDict.range()
    
    #    varcount = spss.GetVariableCount()
    #    print "varcount: " + str(varcount)
    #    varlist=[]
    #    for i in xrange(varcount):
    #      varlist.append(spss.GetVariableName(i))
    
        varlist = ["Var1", "Var2"] + codingVars # Note the use of the operator + to concatenate lists instead of using the append() method for lists which is used to append only ONE new element!
        dataObj = spssdata.Spssdata(indexes=varlist)
    
        # Iterate on the cases in dataObj
        indexes = []  # Vector of indices 1 thru n representing each variable taken by the categorical variable
        catDict = {}  # Dictionary indexed by the variable name and index value of the categorical variable that should be used to retrieve the variable value corresponding to each parameter estimate of the model.
        for case in dataObj:
            var = case[0]         # Variable name (this should always be of type string)
            ### WARNING: DEPENDING ON THE TVARS AND THE TVALUES SETTINGS IN SPSS THE Score() FUNCTION MAY FAIL  ###
            ### The reason is that when TVars is set to 'Labels' at the time of running the regression that     ###
            ### generates the 'codings' dataset, only the LABEL of the variable will be stored in the latter,   ###
            ### NOT the actual VARIABLE NAME!                                                                   ###
            ### The TVars option can be set using SET TVars=Names.                                              ### 
            # Remove any variable label that may come also as part of the variable name
            # Note that if the variable label exists, it comes after a blank space separating it from the variable name.
            # (this happens when the option in SPSS under Edit -> Options -> Output Labels -> Pivot Table Labeling is set to "Names and Labels" for its second option)
            # Note that there are 3 possibilities for this option: 'Labels', 'Names', 'Names and Labels'.
            # This code works correctly only when the option is set to either 'Names' or 'Names and Labels' at the moment of
            # running the regression when generating the 'codings' dataset.
            # It will FAIL when the option is set to 'Labels' because the 'codings' dataset will only store the value label but NOT the actual value!
            # This option can be set via the command line by using the SET TVars= option. Ex: SET TVars=Names.
            try:
                blankSpace = var.index(" ")
                var = var[0:blankSpace]
            except:
                pass
            ### WARNING: DEPENDING ON THE TVARS AND THE TVALUES SETTINGS IN SPSS THE Score() FUNCTION MAY FAIL  ###
            ### The reason is that when TNumbers is set to 'Labels' or 'Names and Labels' at the time of        ###
            ### the regression that generates the 'codings' dataset, only the value LABEL will be stored in     ###
            ### the latter, NOT the actual VALUE!                                                               ###
            ### The TVars option can be set using SET TVars=Names.                                              ### 
            value = case[1]       # Actual value of the categorical variable corresponding to the index value (this should always be of type string)
            codings = case[2:]    # Tuple of 0's and 1's indicating how the coding of categorical values was done for the model fit.
            try:
                  index = codings.index(1) + 1
            except:
                  index = 0       # Index 0 means that the current value of the categorical variable is the REFERENCE level
            
            # Update the catDict dictionary (note that the key for 'index' is set to be string because the 'index' key stored in the parameters dictionary below (paramsDict) is also string.
            # In fact the key values for both catDict and paramsDict need to match!
            catDict[(var.strip(), str(index))] = value.strip()
    
        dataObj.close()    # Apparently this is equivalent to del dataObj

        # Close dataCodings dataset if it was read from a file
        if dataCodings == "@codings_":
            spss.Submit("dataset close @codings_")

    ### 2.- Process the parameters dataset and create a dictionary containing the estimated parameters
    ### The result of this step is a dictionary called paramsDict whose key is a 2-uple of the form (<variable name>, <index of categorical variable>) and
    ### whose value is the corresponding estimated parameter.
    spss.Submit("dataset activate " + dataParams)
    varlist = ["Var1", "Var2", "B"]
    # When the parameters dataset is NOT read from a file but is already opened by the user, create a copy and sort by regression step (Var2) and variable name (Var1)
    if dataParams != "@params_":    # @params_ is the name I give above to the dataset read from file passed in parameter fileParams
        spss.Submit("dataset copy @params_")
        spss.Submit("dataset activate @params_")

    # Create the dataset object pointing to the parameters dataset
    dataObj = spssdata.Spssdata(indexes=varlist)
    # When step is string (i.e. either equal to "first" or "last") retrieve the first or last step number using either the fetchone() or fetchall() method of the Spssdata class
    if isinstance(step,str):
        if step.lower() == "first":
            caseFirst = dataObj.fetchone()  # Tuple containing the first case in the data object
            step = caseFirst[0]
        else:
            caseAll = dataObj.fetchall()    # Tuple(?) containing ALL cases in the data object
            step = caseAll[len(caseAll)-1][0]
        # Extract just the step NUMBER which comes after the word "Step " and convert 'step' to integer
        try:
            # This is for the parameters table generated by the LOGISTIC REGRESSION command in SPSS
            indStep = step.index("Step")        # This statement could probably be removed because indStep is never used... (but I leave it just in case)
            step = int(step[len("Step ")-1:])
        except:
            # This is for the parameters table generated by the REGRESSION command in SPSS
            step = int(step)
        # Reset the position of the cursor
        dataObj.restart()
    print "\nReading model parameters from regression Step " + str(step)

    # Read the parameter values and store them in a dictionary indexed by the variable name and index value of categorical variables
    paramsDict = {}
    # Initialize the intercept to 0 in case the model does not have an intercept
    beta0 = 0  
    for case in dataObj:
        caseStep = case[0]
        var = case[1]
        beta = case[2]
        # Extract just the step NUMBER which comes after the word "Step " and convert 'step' to integer
        try:
            # LOGISTIC REGRESSION case
            indStep = caseStep.index("Step")            # This statement could probably be removed because indStep is never used... (but I leave it just in case)
            caseStep = int(caseStep[len("Step ")-1:])   # There is a space after the word "Step" and before the step number
        except:
            # LINEAR REGRESSION case
            caseStep = int(caseStep)
        # Retrieve the parameter value only if it corresponds to the correct regression and step and its value is not missing
        if caseStep == step and beta is not None:
            # Check if the current parameter is the intercept and store into a separate variable so that:
            # - I don't need to deal with the intercept when generating the scoring expression below
            # - I can search for categorical variables by simply searching for an open and a close parenthesis
            # without bothering for the REGRESSION case when the intercept is called '(Constant)'
            if var.strip().lower() == '(constant)' or var.strip().lower() == "constant":
                ## The first case is how the intercept is named for the linear REGRESSION output and
                ## the second case is how the intercept is named for the LOGISTIC REGRESSION output.
                beta0 = beta
            else:
                # Check whether the current variable is categorical by searching for parenthesis (e.g. "XCat(2)") at the last part of the variable name
                try:
                    ind1 = var.index("(")
                    ind2 = var.index(")")
                    index = var[ind1+1:ind2]
                    var = var[:ind1]
                except:
                    index = ""
                paramsDict[(var.strip(), index)] = beta
    # Store the keys in the parameters dictionary alphabetically so that categorical variables can correctly be processed below
    paramsKeys = paramsDict.keys()
    paramsKeys.sort()
    dataObj.close()

    # Before proceeding, check if ALL variables read from the parameters dataset exist in the dataset with the data to score
    # Note that I need to do this even before constructing the score expression because the type of the variable in the input dataset
    # is used below when constructing the SPSS expression that computes the score.
    paramVarsList = []
    for k in paramsKeys:
        paramVarsList.append(k[0])
    foundAll, varsNotFoundList = CheckVariables(paramVarsList, data=data, casesensitive=True, log=False)
    if not(foundAll):
        print "\nSCORE: ERROR - The following variables present in the model were not found in the dataset to score:"
        for v in varsNotFoundList:
            print(v)
        print "\nExecution of function Score stops."
        return
  
    ### 3.- Show the dictionary keys and their values and construct the score expression
    print "\nParameters of the model:"
    if len(paramsKeys) == 0:
        print "No parameters found! Is 'step' correctly defined?"
        print "Scoring expression:"
        print pred + " = 0"
    else:
        print "Intercept: " + str(beta0)
        for k in paramsKeys:
            print str(k) + ": " + str(paramsDict[k])
        print "\nScoring expression for SPSS:"
        expression = "numeric " + score + " (F15.8).\n"
        expression = expression + "compute " + score + " = "   # score contains the name of the variable where the score will be stored in the input dataset
        # First place the intercept
        expression = expression + str(beta0)
        for k in paramsKeys:
            expression = expression + " + \n" + str(paramsDict[k]) + " * "
            if k[1] == "":
                # Continuous variable because index = ""
                expression = expression + k[0]
            else:
                # Categorical variable => Get the value taken by the variable from catDict dictionary.
                # Note that the variable is compared against a quoted value when the variable is string (varTypeDict[k[0]] > 0).
                # Otherwise, no quotes are used ot enclose the value against which it is compared.
                expression = expression + "(" + k[0] + "=" + ((varTypeDict[k[0]] and "'") or "") + catDict[k] + ((varTypeDict[k[0]] and "'") or "") + ")"
        # End the SPSS compute statement.
        expression = expression + "\n."
        # If the model is logistic regression, compute the estimated probability as the inverse of the logit function.
        if model == "logit":
            expression = expression + "\ncompute " + score + " = 1 / (1 + exp(-" + score + "))."
        expression = expression + "\nexecute."
        print expression

    # Close temporary parameters dataset
    # Note that this dataset ALWAYS exist, since either the parameters dataset was read from a file and named "@params_"
    # or the parameters dataset was read from an open dataset and a copy was made of it and named "@params_".
    spss.Submit("dataset close @params_")

    if not(test):
        if not(replace) and existsScore:
            print "\nSCORE: ERROR - The variable where the score will be saved (" + score + ") already exists in dataset " + data + "."
            print "SCORE: Use replace=True if you wish to replace the variable in the dataset."
            print "\nExecution of function Score stops."
            return

        ### 4.- Read the data to score and compute the score
        ### The result of this step is the creation of a new variable in the input dataset 'data' called 'pred' containing the predicted value for each observation.
        ### This predicted value is the probability of the target variable taken the event value when the model corresponds to a logistic regression.
        ### IMPORTANT: Note that I could use the expression already computed above (under variable 'expression') to compute the predicted value for each observation.
        ### However, here I use a different technique (which may take longer!) for 2 reasons:
        ### (a) Have an example of elaborated data processing carried out here using cursors
        ### (b) Detect and warn the user whether a categorical variable takes a value that is NOT among the possible values taken by the categorical variable during model development.
        spss.Submit("dataset activate " + data)
        if replace and existsScore:
            spss.Submit("delete variables " + score)
        # Get the list of variables in the dataset to score
        vars = spssaux.getVariableNamesList()
        # Convert variable names to upper case so that the match between variable names in the model and variable names in the dataset is NOT case sensitive
        vars = [v.upper() for v in vars]

        # Open the data to score in write mode so that a new variable containing the predicted value (score) can be created 
        dataObj = spssdata.Spssdata(accessType="w")
        # Create a new variable to store the predicted value (score) generated by the model
        dataObj.append(spssdata.vdef(score, vfmt=["F",15,8]))  # the default type of a new variable is numeric which corresponds to parameter vtype=0
        dataObj.commitdict()  # Commit the variable dictionary just defined
        if debug:
            print "\t>Term" + operator.repeat(" ", tabSpace-len("Term")) + "Contribution to z value"
        # Iterate over the cases in the dataset to score
        for i, case in enumerate(dataObj):
            # Initialize the prediction to be equal to the intercept
            pred = beta0
            categorical = False
            found = False
            varPrev = ""
            if debug and (i < debugmaxcases or showAll):
                print "Case: " + str(i+1)
                print "\t>Intercept" + operator.repeat(" ", tabSpace-len("Intercept")) + str(beta0)
            # Iterate over the parameters of the model
            # IMPORTANT: The keys in paramsKeys should be sorted by var and index as was done above!
            for k in paramsKeys:
                beta = paramsDict[k]
                if k[0] == varPrev and found:
                    continue
                if k[0] != varPrev:
                    if categorical and not(found):
                        # Check if the value (in the dataset to score) taken by the categorical variable analyzed at the previous step
                        # is among the possible values of that variable when the model was built. At this point, all possible values of
                        # the categorical variable appearing in the parameters dataset have been considered and the only value
                        # still left is the reference value which is stored in catDict() with index=0.
                        # In the IF below I check whether the previous variable is string (varTypeDict[varPrev] > 0) or numeric (varTypeDict[varPrev] = 0)
                        # so that I write the appropriate comparison between the categorical value corresponding to the current beta with the variable value.
                        # Note: indVar is computed at the 'try' block just below... (and this is ok because the current block is executed only after the execution of that 'try' block)
                        if varTypeDict[varPrev] and catDict[(varPrev, '0')] != str(case[indVar]).strip() or \
                            not(varTypeDict[varPrev]) and float(catDict[(varPrev, '0')]) != case[indVar]:
                            print "\tSCORE: WARNING - Variable " + varPrev + " takes a value that is NOT a possible value for the variable in the model. The value is: " + str(case[indVar]).strip()
                        elif debug and (i < debugmaxcases or showAll):
                            termExpression = "0*(" + varPrev + "=" + ((varTypeDict[varPrev] and "'") or "") + str(catDict[(varPrev, '0')]) + ((varTypeDict[varPrev] and "'") or "") + ")"
                            print "\t>" + termExpression + operator.repeat(" ", tabSpace-len(termExpression)) + "0.00000000"
                    # For the currently analyzed parameter, set found to False (meaning that the value of the variable corresponding to the currently analyzed parameter has not yet been found at the corresponding variable in the dataset to score)
                    found = False
                    # Now look for the variable name associated to the currently analyzed parameter (stored in k[0]) in the dataset to score.
                    try:
                        indVar = vars.index(k[0].upper())
                    except:
                        print "\tSCORE: WARNING - Variable " + k[0] + " was not found in the dataset to score!"
                        # The following two conditions are set so that, in case the variable that was not found in the dataset is categorical, this message is only shown once
                        # (since the condition above 'if k[0] == varPrev and found' will be satisfied for the next appearance of the categorical variable in the parameters dataset
                        # and the loop will 'continue' to the next case)
                        found = True
                        varPrev = k[0]
                        continue
                if k[1] == "":    # k[1] contains the index value of categorical variables that indexes the actual value of the categorical variable. If this value is empty, it means that the variable is continuous.
                    # The variable associated to the currently analyzed parameter is continuous
                    categorical = False
                    found = True
                    pred = pred + beta*case[indVar]
                    varPrev = k[0]
                    if debug and (i < debugmaxcases or showAll):
                        termExpression = str(beta) + "*" + k[0]
                        print "\t>" + termExpression + operator.repeat(" ", tabSpace-len(termExpression)) + str(beta*case[indVar])
                else:
                    # The variable associated to the current analyzed parameter is categorical
                    categorical = True
                    # In the IF below I check whether the variable is string (varTypeDict[k[0]] > 0) or numeric (varTypeDict[k[0]] = 0)
                    # so that I write the appropriate comparison between the categorical value corresponding to the current beta with the variable value.
                    if varTypeDict[k[0]] and str(catDict[k]) == str(case[indVar]).strip() or \
                     not(varTypeDict[k[0]]) and float(catDict[k]) == case[indVar]:
                        found = True
                        pred = pred + beta
                        if debug and (i < debugmaxcases or showAll):
                            termExpression = str(beta) + "*(" + k[0] + "=" + ((varTypeDict[k[0]] and "'") or "") + str(catDict[k]) + ((varTypeDict[k[0]] and "'") or "") + ")"
                            print "\t>" + termExpression + operator.repeat(" ", tabSpace-len(termExpression)) + str(beta)
                    varPrev = k[0]
                  
            # Compute the probability from the predicted value stored in pred if the model is a logistic regression model
            if debug and (i < debugmaxcases or showAll):
                print "\t>" + operator.repeat(" ", tabSpace-len("SUM(z): ")) + "SUM(z): " + str(pred)
            if model == "logit":
                pred = 1 / (1 + exp(-pred))
            if debug and (i < debugmaxcases or showAll):
                print "\t>" + operator.repeat(" ", tabSpace-len("SCORE: ")) + "SCORE: " + str(pred)
            # Store the predicted value (score) in the dataset to score
            dataObj.casevalues([pred])
            # An alternative way, using setvalues()
            #dataObj.setvalues('pred', pred)
            #dataObj.CommitCase()   # Note that a CommitCase() is necessary if we use setvalues()!

        dataObj.close()
    
    return expression
########################################### Score #############################################
