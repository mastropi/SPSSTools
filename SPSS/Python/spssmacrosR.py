"""This module contains the Python functions that I use in SPSS to call R programs. It uses the capability that an BEGIN PROGRAM R can be nested within a Python block
Author: Daniel Mastropietro"""

# HISTORY:
# 2013/07/03: Created
# 2013/09/09: In function RDistributionsByGroup(),
#             fixed the problem of displaying an older JPG file when the execution of the code fails.
# 2013/12/01: In function RDistributionsByGroup(),
#             Added parameter 'size' that defines whether the graph sent to the SPSS output window should small or large.
# 2013/12/01: Copied the function BuildVarListAndString from module spssmacros.py in order to correclty parse variable lists passed
#             as strings in multiple lines having the first and last lines empty.
#             Note that I explicitly copy this function to this module instead of simply import it because of the way I distribute
#             these Python modules to "production" in Nat, namely: I copy the module to the production directory Software\Tools\Python
#             but then I *rename* the module name! (therefore the import would fail in production)
# 2013/12/11: Created new function RCopyVariables() to copy Python variables into the R environment.
#             Still I was not able to figure out how to extract the variable name and its value from the parameter passed...
#             (See my comments in the function below).
# 2014/03/08: In BuildVarListAndString() function,
#             Added a new parameter 'dedup' so that the list of names are not dedupped. This is needed for example by
#             the RDistributionsByGroup() function when parsing the value of parameter 'transforms' which may have duplicates
#             (since it does NOT contain a variable list but a set of transformation to apply to the variables, which may be the same
#             for different variables!)
#             In RDistributionsByGroup() function,
#             Fixed the error arising when passing parameter 'transforms' and occuring since the BuildVarListAndString() function was
#             changed to dedup names passed. The error was fixed by calling BuildVarListAndString() with sort=False and dedup=True
#

# TODO:
# [DONE-2013/12/11: RCopyVariables() function]
#           2013/08/05: Create a function that copies Python variables into R. Use the BEGIN PROGRAM R inside the PYTHON program to do it and create a syntax that parses
#             the variables to Python variables to copy.
# 2014/07/28: Crate a function that copies R variables to Python or to SPSS. Use the BEGIN PROGRAM R inside a PYTHON program that generates a temporary SPSS
#             from which Python can read the value using a dataStep(). However I am thinking whether some XML functionality would also do the trick...
#             See the answer from Jon Peck in ~ Jun-2014 at the IBM Developerworks forum.
# 2015/01/16: Create a function that copies an R data frame to SPSS.
#             Code taken from M2014-v2SBR-02ProcesoModelizacion_WOE.sps in the NBC 2014 project:
#               BEGIN PROGRAM R.
#                   # Dictionary de las nuevas variables a crear en el dataset
#                   # Nota: cada nueva variable es una nueva COLUMNA en el dictionary dict; la funcion replicate() pone cada vector indicado en c() en columnas distintas
#                   dict.newvars = replicate( length(newvarclass), c("", "", 8, "A8", "nominal") )          # Note: the colum names of the data frame containing the dictionary are not important
#                   # Asigno el nombre de la variable a cada columna del dictionary
#                   j = 0
#                   for (v in newvarclass) {
#                       j = j + 1
#                       dict.newvars[1, j] = v
#                   }
#                   dict = data.frame(dict,  dict.newvars)
#                   # Set the dictionary
#                   spssdictionary.SetDictionaryToSPSS("tofit.new", dict)
#                   # Set the data
#                   spssdata.SetDataToSPSS("tofit.new", tofit.new)
#                   # Close the dataset
#                   spssdictionary.EndDataStep()
#               END PROGRAM.
#

# Some writing standards:
# 1.- Parse input parameters and, if there is an error, stop execution using 'if error: return'
# 

import atexit,os,locale,sys
import spss.PyInvokeSpss
from spss.errMsg import *

import spss
import spssaux
import operator
import spssdata

### My imports
# Global variables used in this code (e.g. tempdir_, Rdir_)
from natglobals import *
# Import auxiliay functions
from dmspssaux import BuildVarListAndString

# Define the symbols that are exported when running import *
__all__ = [ "RSource",
            "RLibPath",
            "RCopyVariables",
            "RDistributionsByGroup",
            ]


########################## Auxiliary Python functions that execute R code #################################
# 2013/07/24
# Compile R functions called by the Python functions defined in this module
def RSource(dir=Rdir_):
    """Create the syntax that 'source's the R codes in the specified directory"""

    # Note the use of .r$ as pattern so that only files ENDING in .r are returned (additional note: the '*' is NOT allowed in the pattern in version R-2.8.0, although it is allowed in version R-3.0.1)
    syntaxR = r"""
BEGIN PROGRAM R.
files = list.files(path="%(dir)s", pattern=".r$")
for (f in files) { cat("Compiling R code present in %(dir)s...", f, "\n"); source(file.path("%(dir)s",f)) }
END PROGRAM.""" %locals()
    spss.Submit(syntaxR)

# 2013/11/13
# Add one or more libraries to the library search list
def RLibPath(libdir):
    """Create the vector in R with the libraries to add to the library path which is used as argument of .libPaths()"""

    libdirlist = spssaux._buildvarlist(libdir)
    libPathsParam = "c('" + "','".join(libdirlist) + "')"
    # Create the syntax that adds a library path to the library search list using R function .libPaths()
    syntaxR = r"""
BEGIN PROGRAM R.
libPathsParam = %(libPathsParam)s
for (i in 1:length(libPathsParam)) {
    cat("Adding library", libPathsParam[i], "to the library search path...\n")
}
.libPaths(%(libPathsParam)s)
END PROGRAM.""" %locals()
    spss.Submit(syntaxR)

# 2013/12/11
def RCopyVariables(**kwargs):
    """Copy variables from Python to R. The result is a copy as is of the Python variable value to the R environment,
    with the exception that, for string variables, any starting or ending new line character is removed.
    The variables must be passed as keyword arguments where the name of the keyword is the variable name to be created in R
    and its value is the value of the Python variable to copy to it.
    Example: (run within a Python block)
        RCopyVariables(varnum=varnum, varc=varclass)
    This creates 
    """
    
    ### NOTE 1: **kwargs is a dictionary containing name-value pairs stored as key-value. The keys can be referenced by kwargs.keys().
    ### The other way of passing a variable number of parameters is by using *args, which is a LIST containing just the values of
    ### the parameters (without names). The values passed can be retrieved as we retrieve elements in a list (e.g. args[2]).
    ###
    ### NOTE 2: My initial trial for implementing this was to pass the "Python variables to copy" as strings in a list
    ### (e.g. RCopyVariables(["varnum","varclas"])) and then retrieve its value using eval(eval("var"), globals())
    ### assuming that the varnum and varclass Python varibales are in the global namespace (or environment in R language).
    ### However this is NOT the case, as the global namespace (retrieved as a dictionary by running globals()) of the
    ### currently executed function (RCopyVariables() in this case) is DIFFERENT from the global namespace of the calling
    ### Python program!! In fact, the global namespace of a function in a module is made up of the variables and functions
    ### defined in the module!!
    
    ### References where this can be investigated in order to make it work:
    ### - http://www.diveintopython.net/html_processing/locals_and_globals.html
    ### - inspect module and http://stackoverflow.com/questions/218616/getting-method-parameter-names-in-python 
    ### which describes commands such as:
    ###     # if you want arguments names as a list:
    ###     args_name = inspect.getargspec(RCopyVariables)[0]
    ###     print args_name
    ###     # if you want names and values as a dictionary:
    ###     args_dict = dict(itertools.izip(args_name, args))
    ###     print(args_dict)    
    ### or look at http://stackoverflow.com/questions/9837485/access-namespace-of-calling-module
    ### in particular pay attention at the following command at the end of the conversation:
    ###     "inspect.getframeinfo(inspect.currentframe().f_back.f_back)"
    ### Note: I tried running inspect.getframeinfo(inspect.currentframe().f_back) in this function as a test
    ### and the object returned is of class inspect.Traceback. Investigating the methods and attributes of this class
    ### may help further...
    ###
    
    syntaxStart = "BEGIN PROGRAM R."
    syntaxEnd = "\nEND PROGRAM."
    syntaxR = ""
    i = 0
    keys = sorted(kwargs.keys())
    for varR in keys:
        i = i + 1
        varPythonValue = kwargs[varR]
        isString = isinstance(varPythonValue,str)
        if isString:
            # When string, remove any initial or final empty line (otherwise the value is not correctly assigned in R)
            # NOTE: BuildVarListAndString ONLY accepts sequences or STRING variables (no numeric variables) and this is due to the call
            # to spssaux._buildvarlist() function which accepts only the aforementioned types as argument (see documentation).
            varPythonValueList, varPythonValue = BuildVarListAndString(varPythonValue)
        print "Copying Python variable " + str(i) + " to R as '" + varR + "'"
        syntaxR = syntaxR + "\n" + varR + " = " + (isString and "'" or "") + str(varPythonValue) + (isString and "'" or "") + r"""
# For string variables, remove any starting and ending new lines in order to avoid generating empty variable names
# when using strsplit(), a function that is often called within an R program to convert a string of variable names into an array.
if (typeof(%(varR)s) == "character") {
    %(varR)s = sub("^\n", "", %(varR)s)
    %(varR)s = sub("\n$", "", %(varR)s)
}
# NOTE: running this cat() creates the following error message (don't know why!): "In dir.create(Rgraphicpath, showWarnings = TRUE, recursive = FALSE) : 'C' already exists"
cat("\t", typeof(%(varR)s),"variable created in R.\n")
""" %locals()

    spss.Submit(syntaxStart + syntaxR + syntaxEnd)
########################## Auxiliary Python functions that execute R code #################################



############################### Distributions by Group for Client Profile ###################################
# 2013/07/03
#
# HISTORY:  2013/09/09
#           - Fixed the problem of displaying an older JPG file already existing in the tempdir directory when the execution of the function fails.
#           2013/12/01
#           - Added parameter 'size' that can take the values "small" or "large" in order to choose whether the output graph should be
#           small (which means that the graph is sent directly to the SPSS output window) or large (which means that the graph is first
#           saved as a large JPEG file and then sent to the SPSS output window). The reason for adding this parameter is that showing
#           large graphs when plotting only one variable makes its color be a little washed out, difficulting its visibility.
#           2013/03/08
#           - Fixed an error arising when passing parameter 'transforms' and occuring since the BuildVarListAndString() function has been
#           changed to dedup names passed. The error was fixed by adding a new parameter 'dedup' to the function.
#
# TODO:
# 2013/08/29: Add a check whether the variables passed by the user exist in the dataset
# [DONE-2013/09/09: Before creating the new plots I delete the JPG files already existing in tempdir using the shell() function in R to execute host commands] 2013/08/29: Avoid having an .jpg file previously created from showing up in the SPSS Viewer when an error occurs with the instance being currently executed by the user.
#             If this happens, a wrong graph may show up and the user may think that it is the graph of what they have just requested!!
def RDistributionsByGroup(
    data=None,
    where=(),                       # Where condition to apply to the data (in SPSS language)
    vars=(),
    groupvars=(),
    transforms=(),                  # Blank-separated string of function names to use to transform the variables in vars before computing their distribution (accepted values are: "log", "safeLog", "." (for no transformation))
    valueLabels=(),                 # Blank-separated string of characters "." or "T" stating whether the original or transformed variable values should be used as labels for the horizontal axis (defaults to all ".")
    labelOrientation=None,          # Integer number (1, 2, 3, 4) defining the orientation of the text in the way that parameter 'las' in mtext() does.
    stats="q5 median q95",          # Blank-separate string indicating the statistics to show on the graph (e.g. mean, median, q25, etc.)
    method="FD",                    # Method to use to determine the histogram bins. Possible values are: "density" and any other values accepted by the breaks parameter of hist().
    alltogetherVars=True,           # Whether to show the distributions of all variables on a tiled plot
    alltogetherGroups=True,         # Whether to overlay the distributions of all groups on the same graph
    histFlag=False,                 # Whether to show the histogram as well (besides the density)
    cdfFlag=False,                  # Whether to show the CDF instead of the PDF as density
    xadjustFlag=False,              # Whether to adjust the ANALYSIS range on which histograms and densities are based to the values given in parameter xlim
    xlabSimpleFlag=True,            # Whether to show simple labels on the horizontal axis, meaning only min(x) and max(x) and any requested statistics in 'stats'
    xlim=(),
    colors=(),
    size="small",                   # Size of the graph: either "small" or "large" (when "small" the graph is sent directly to the output window; when "large" the graph is first saved on a large size JPEG file and then sent to the output window)
    tempdir=tempdir_,
    log=True):
    """Calling Example:
    BEGIN PROGRAM.
    from spssmacrosR import *
    RDistributionsByGroup(
        data="tofit",
        vars="score x1 x2",
        groupvars="target",
        histFlag=False,
        cdfFlag=True,
        xadjustFlag=False,
        xlim=(),
        colors="red green",
        tempdir=tempdir,
        log=True)
    END PROGRAM.
    """
    
    #-------------------------------- Parse input parameters ----------------------------------
    error = False
    errmsg = ""

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nRDISTRIBUTIONBYGROUP: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if where and not isinstance(where,str):
        errmsg = errmsg + "\nRDISTRIBUTIONBYGROUP: ERROR - Parameter WHERE must be of type string (value given: " + str(where) + ")"
        error = True
    if not(vars) or (vars and not isinstance(vars,(str,list,tuple))):
        errmsg = errmsg + "\nRDISTRIBUTIONBYGROUP: ERROR - Parameter VARS must be non-empty and of type string, list or tuple (value given: " + str(vars) + ")"
        error = True
    if transforms and not isinstance(transforms,(str,list,tuple)):
        errmsg = errmsg + "\nRDISTRIBUTIONBYGROUP: ERROR - Parameter TRANSFORMS must be of type string (value given: " + str(transforms) + ")"
        error = True
    if valueLabels and not isinstance(valueLabels,(str,list,tuple)):
        errmsg = errmsg + "\nRDISTRIBUTIONBYGROUP: ERROR - Parameter VALUELABELS must be of type string (value given: " + str(valueLabels) + ")"
        error = True
    if stats and not isinstance(stats,str):
        errmsg = errmsg + "\nRDISTRIBUTIONBYGROUP: ERROR - Parameter STATS must be of type string (value given: " + str(stats) + ")"
        error = True

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    if data:
        syntaxStart = "dataset activate " + data + "."
    else:
        syntaxStart = ""
        data = spssaux.getActiveDatasetName()
        if data is None:            # This value is returned by getActiveDatasetName() when the active dataset does not have a name
            data = "None"

    #-- WHERE
    if where:
        # Copy the input dataset to a temporary dataset called @data
        syntaxStart = syntaxStart + r"""
dataset copy @data.
dataset activate @data.
select if (%(where)s).
execute.""" %locals()

    #-- VARS
    varlist, vars = BuildVarListAndString(vars)

    #-- GROUPVARS
    if groupvars:
        groupvarlist, groupvars = BuildVarListAndString(groupvars)
    else:
        groupvars = ""
        
    #-- TRANSFORMS and VALUELABELS
    transList, transforms = BuildVarListAndString(transforms, sort=False, dedup=False)
    valueLabelList, valueLabels = BuildVarListAndString(valueLabels, sort=False, dedup=False)    

    #-- ALLTOGETHER Flags
    if alltogetherVars and len(varlist) > 1:
        # Cannot show distribution of groups separate when there is more than one variable that are plotted on a single tiled plot
        alltogetherGroups = True

    #-- STATS:
    if stats == None:
        stats = "NULL"

    #-- SIZE:
    if size.upper() == "LARGE":
        RGraphicsOutput = "OFF"
        save = "TRUE"
    else:
        RGraphicsOutput = "ON"
        save = "FALSE"

    #-- Show the parameters of execution of the macro
    if log:
        print "\nFunction call resolves to:"
        print "RDistributionsByGroup("
        print "\tdata =                 ", data
        print "\twhere =                ", where
        print "\tvars =                 ", vars
        print "\tgroupvars =            ", (groupvars or "()")
        print "\ttransforms =           ", transforms
        print "\tvalueLabels =          ", valueLabels
        print "\tlabelOrientation =     ", labelOrientation
        print "\tstats =                ", stats
        print "\tmethod =               ", method
        print "\talltogetherVars =      ", alltogetherVars
        print "\talltogetherGroups =    ", alltogetherGroups
        print "\thistFlag =             ", histFlag
        print "\tcdfFlag =              ", cdfFlag
        print "\txadjustFlag =          ", xadjustFlag
        print "\txlabSimpleFlag =       ", xlabSimpleFlag
        print "\txlim =                 ", xlim
        print "\tcolors =               ", colors
        print "\tsize =                 ", size
        print "\ttempdir =              ", tempdir
        print "\tlog =                  ", log
        print "\n"
    #-------------------------------- Parse input parameters ----------------------------------


    #--------------------------- Parameters for R function call -------------------------------
    #-- Flags need to go to uppercase for R function call
    alltogetherGroups = str(alltogetherGroups).upper()
    histFlag = str(histFlag).upper()
    cdfFlag = str(cdfFlag).upper()
    xadjustFlag = str(xadjustFlag).upper()
    xlabSimpleFlag = str(xlabSimpleFlag).upper()

    #-- Generate parameters that are passed as c(...) to R
    #-- XLIM
    if xlim:
        xlimlist = spssaux._buildvarlist(xlim)
        # Convert the elements of xlimlist from numeric to string, otherwise the join() function below does not work.
        xlimlist = [str(s) for s in xlimlist]
        xlim = "c(" + ",".join(xlimlist) + ")"
    else:  
        xlim = "NULL"
    #-- COLORS
    if colors:
        colorslist = spssaux._buildvarlist(colors)
        colors = "c('" + "','".join(colorslist) + "')"
    else:
        colors = "NULL"
    #--------------------------- Parameters for R function call -------------------------------

    #------------------------------ Build syntax to execute -----------------------------------
    syntaxR = r"""
BEGIN PROGRAM R.
spssRGraphics.SetOutput("%(RGraphicsOutput)s")      # This defines whether the output generated by R is sent directly to the viewer (ON) or not (OFF)
tofit_ = spssdata.GetDataFromSPSS(variables=c('%(vars)s' """ %locals() + (groupvars and ", '%(groupvars)s'" %locals()) + "))"
    # Initialize the syntax to be built now
    syntaxEnd = ""
    if alltogetherVars:
        jpegfile = "r_graph_dbg_plot.jpg"
        if RGraphicsOutput == "OFF":
            # Delete JPG file before creating the new one, in case there is any problem with the execution of this code (in which case an previously-created JPG file will show up as output!!)
            syntaxR = syntaxR + r"""
shell(paste("del", normalizePath("%(tempdir)s/%(jpegfile)s")))""" %locals()
        syntaxR = syntaxR + r"""
DistributionsByGroup(
    data=tofit_,
    vars='%(vars)s',
    groupvars=""" %locals() + (groupvars and "'%(groupvars)s'" %locals() or "NULL") + r""",
    transforms=""" %locals() + (transforms and "'%(transforms)s'" %locals() or "NULL") + r""",
    valueLabels=""" %locals() + (valueLabels and "'%(valueLabels)s'" %locals() or "NULL") + r""",
    labelOrientation=""" %locals() + (labelOrientation and "%(labelOrientation)s" %locals() or "NULL") + r""",
    stats='%(stats)s',
    method='%(method)s',
    alltogetherGroups=%(alltogetherGroups)s,
    histFlag=%(histFlag)s,
    cdfFlag=%(cdfFlag)s,
    xadjustFlag=%(xadjustFlag)s,
    xlabSimpleFlag=%(xlabSimpleFlag)s,
    xlim=%(xlim)s,
    colors=%(colors)s,
    save=%(save)s,
    saveDir='%(tempdir)s',
    jpegfile='%(jpegfile)s')""" %locals()
        if RGraphicsOutput == "OFF":
            syntaxEnd = r"""spssRGraphics.Submit('%(tempdir)s/%(jpegfile)s')""" %locals()

    else:
        for v in range(len(varlist)):
            jpegfile = "r_graph_dbg_plot_" + varlist[v] + ".jpg"
            if RGraphicsOutput == "OFF":
                # Delete JPG file before creating the new one, in case there is any problem with the execution of this code (in which case an previously-created JPG file will show up as output!!)
                syntaxR = syntaxR + r"""
shell(paste("del", normalizePath("%(tempdir)s/%(jpegfile)s")))""" %locals()
            syntaxR = syntaxR + r"""
DistributionsByGroup(
    data=tofit_,
    vars='""" + varlist[v] + r"""',
    groupvars=""" %locals() + (groupvars and "'%(groupvars)s'" %locals() or "NULL") + r""",
    transforms=""" %locals() + (transforms and ("'" + transList[v] + "'") %locals() or "NULL") + r""",
    valueLabels=""" %locals() + (valueLabels and ("'" + valueLabelList[v] + "'") %locals() or "NULL") + r""",
    labelOrientation=""" %locals() + (labelOrientation and "%(labelOrientation)s" %locals() or "NULL") + r""",
    stats='%(stats)s',
    method='%(method)s',
    alltogetherGroups=%(alltogetherGroups)s,
    histFlag=%(histFlag)s,
    cdfFlag=%(cdfFlag)s,
    xadjustFlag=%(xadjustFlag)s,
    xlabSimpleFlag=%(xlabSimpleFlag)s,
    xlim=%(xlim)s,
    colors=%(colors)s,
    save=%(save)s,
    saveDir='%(tempdir)s',
    jpegfile='%(jpegfile)s')""" %locals()
            if RGraphicsOutput == "OFF":
                syntaxR = syntaxR + r"""
spssRGraphics.Submit('%(tempdir)s/%(jpegfile)s')""" %locals()

    syntax = syntaxStart + "\n" + syntaxR + "\n" + syntaxEnd + "\nEND PROGRAM."
        ## NOTE: The END PROGRAM statement CANNOT be EXPLICITLY listed as part of an spss.Submit string in a single line, because in that case SPSS interprets it as the END PROGRAM
        ## of the Python block from which this function is invoked!!
        ## The workaround I use here is to use the 'END PROGRAM' keyword as part of the value taken by the 'syntax' variable which is then executed by the spss.Submit() function below.
        ## Note in addition that the example showing the method of nesting Python or R programs in other Python programs in the PDF documentation on Python programmability is WRONG!
        ## (as it does not take what I mention here into consideration)
    #------------------------------ Build syntax to execute -----------------------------------

    if log:
        print(syntax)
    spss.Submit(syntax)

    # Close temporary dataset
    if where:
        spss.Submit("dataset close @data")
        spss.Submit("dataset activate " + data)
############################### Distributions by Group for Client Profile ###################################
