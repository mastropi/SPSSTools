"""This module contains AUXILIARY Python functions to be used in SPSS >18.0.0.
Up to 14-Feb-2014 these functions were used by spssmacros.aux and/or DMSpssMining.py.
After this date new functions may have been added that are not called by those modules.
Author: Daniel Mastropietro"""

# HISTORY:
# 2014/02/14: Module created.
# 2014/03/08: In BuildVarListAndString() function,
#             Added a new parameter 'dedup' so that the list of names are not dedupped. This is needed for example by
#             the RDistributionsByGroup() function when parsing the value of parameter 'transforms' which may have duplicates
#             (since it does NOT contain a variable list but a set of transformation to apply to the variables, which may be the same
#             for different variables!)
#

import spss
import spssaux

### My imports
# Global variables used at NAT
from natglobals import tempdir_

__all__ = [ "CheckVariables",
            "BuildVarListAndString",
            "Save"]

################################### BuildVarListAndString #####################################
# 2013/08/14
# Function to parse input parameters that can be strings, lists or tuples, similar to spssaux._buildvarlist but with the
# following added functionalities:
# - it removes blank spaces or empty lines at the beginning or end of parameter 'vars' that would otherwise be considered as
# an empty variable name.
# - if requested, it dedups the list of names passed in 'vars' (by keeping its original order!)
# - it removes blank spaces at the beginning or end of each identified variable name
def BuildVarListAndString(vars, sort=False, dedup=True):
    # Convert the variable names to a list (in case they are not a list already)
    varlist = spssaux._buildvarlist(vars)

    # Check if the output returned by _buildvarlist() is a VariableDict object (which is one of the possible outputs, as stated
    # in the comments of the function in the spssaux module), in which case we first need to retrieve the list of variables
    # from the dictionary.
    # Note: If we want to access the keys in the dictionary we first need to call the attribute 'vdict'. In fact, although
    # the VariableDict class is a dictionary, for some reason it is not a regular Python dictionary. I found this out by looking
    # into the code defining the VariableDict class in the spssaux module, where I saw the call to self.vdict.keys() in method
    # variablesf, and that's how I learn the way they retrieved the keys of the dictionary. Apart from this, I didn't see
    # where vdict is defined (for instance, it is NOT defined in the spssaux module, perhaps it is defined in some other module
    # imported by spssaux...?)
    if isinstance(varlist, spssaux.VariableDict):
        varlist = varlist.variables
        ## Note that this method is incorrectly documented in Raynald's guide for programmability in SPSS(!)
        ## where he says that the way to retrieve the variable list is by using the *method* 'Variables()', but this
        ## method does NOT exist.
        ## (probably his document is valid for an older version of spssaux since its dated 2007...)

    # If no sort and dedup are requested, remove duplicates by keeping the order in which the variables are passed
    if not(sort) and dedup:
        varlistOrig = varlist
        # Remove duplicates
        varlist = [v for v in set(varlist)]
        # Get the order of the variables in the original list
        # (doing this in the way I do it here implies that the position of any duplicated variable in the original list is given by its first occurence)
        indices = map(varlistOrig.index, varlist)
        # Restore the original order of the variables
        indices.sort()
        # Re-create the list with no duplicates in the original order
        varlist = [varlistOrig[i] for i in indices]
    else:
        if dedup:
            # Remove duplicates
            varlist = [v for v in set(varlist)]
        if sort:
            # Sort the list if requested
            varlist.sort()

    # Create variable list as a new-line-separated string and removes any blank spaces before or after its name
    # Note that I explicitly convert each element of 'varlist' to string in case the values in vars are numbers  (e.g. percentiles list as passed to Summary())
    # o.w. I get an error when applying the strip() function.
    vars = "\n".join(map(str,varlist)).strip()
    # Re-create the varlist after stripping each variable name from any blank spaces.
    # This process also creates a list of names where no variable is empty (e.g. coming from an empty line in the original parameter 'vars' when passed as string)
    varlist = vars.split()
    # Try converting the values to numbers (integers first, then float) in case they are originally numbers (e.g. percentiles list passed in Summary()).
    # If the conversion does not succeed, leave the values as strings.
    try:
        varlist = map(int, varlist)     # This fails when the value stored as string is a real number (e.g. float)
    except:
        try:
            varlist = map(float, varlist)
        except:
            pass

    return varlist, vars
################################### BuildVarListAndString #####################################



####################################### CheckVariables ########################################
# 2013/12/20
# This function was created following an example on page 76 of the Python programmability documentation where the VariableList object
# is described.
# Another way is described in Raynald's guide to SPSS programmability on page 259 of the 4th edition (2007) where he uses
# the GetVariableCount() and GetVariableName() functions of the spss module. This has the advantage that there is no need to
# start a data step (as is done in the above method and implemented in this function) using spss.StartDataStep().
# The implementation described by Raynald goes roughly as follows:
#   for i in range(spss.GetVariableCount())
#       var = spss.GetVariableName(i)
#       # Check if the variable searched for matches 'var'.
#       ...
def CheckVariables(vars, data=None, casesensitive=True, log=True):
    """Check the existence of variables in a dataset.
    The function returns 2 values:
    - logical: indicates whether all variables were found in the dataset
    - list: list of variables that were NOT found in the dataset

    vars:               String, list or tuple containing the variable names to search for.
    data:               Name of the dataset where the variables are looked for. If empty the active dataset is used.
                        Default: None
    casesensitive:      Logical: whether the search for the variable names is case sensitive.
                        Default: True
    log:                Logical: whether to show messages in the log.
                        Default: True
    """
    
    #----------------------------------- Parse input parameters -------------------------------
    #-- Check type of input parameters
    error = False
    errmsg = ""     # This variable collects all the generated error messages to be shown before exiting (return)

    if data and not isinstance(data,str):
        errmsg = errmsg + "\nCHECKVARIABLES: ERROR - Parameter DATA must be of type string (value given: " + str(data) + ")"
        error = True
    if not vars:
        errmsg = errmsg + "\nCHECKVARIABLES: ERROR - Parameter VARS must have a value"
        error = True
    elif not isinstance(vars,(str,list,tuple)):
        errmsg = errmsg + "\nCHECKVARIABLES: ERROR - Parameter VARS must be of type string, list or tuple (value given: " + str(vars) + ")"
        error = True

    # Stop execution when there is an error in the input parameters.
    if error:
        print "\nStopping execution because of the following errors:"
        print errmsg
        return

    #-- DATA
    #-- NOTE: This parsing of DATA is not ordinary, because if a dataset name is given, it is NOT activated, so that this check
    #-- variables does NOT de-activate the currently active dataset (i.e. it would be just a check on the variables in "another" dataset).
    if not data:
        data = "*"      # This value is used below in the spss.Dataset() function to create a data object from the active dataset.

    #-- VARS
    # I sort the variables so that the list of variables not found is shown in alphabetical order
    varlist, vars = BuildVarListAndString(vars, sort=True)
    #----------------------------------- Parse input parameters -------------------------------

    ### 1.- Read the variables in the specified dataset
    # Start a data step (this is necessary to create a dataset object from the dataset specified by the user)
    spss.StartDataStep()
    # Create a dataset object from the specified dataset
    # Note: from the spss.Dataset() documentation in the Python programmability documentation, in order to create a data object for
    # an EXISTING dataset, we should pass the name of the existing dataset in parameter 'name' or, if the existing dataset is the
    # active one we should pass name="*" or not pass 'name' at all.
    # If we pass name=None or name="" a new SPSS dataset is created with an automatically generated name, which can be retrieved with
    # the name attribute of the Dataset object. Note that when creating a new dataset, the name of the dataset CANNOT be changed within
    # the already open data step.
    dataObj = spss.Dataset(name=data)
    # Get the actual dataset name (to be shown in the message to the user at the end)
    dataname = dataObj.name
    # Retrieve the list of variables in the existing dataset
    # Note: the varlist method does NOT return a regular list object. It returns an object of the VariableList class in the spss module.
    # In order to access the name of the variable in this VariableList object, use the name attribute when iterating over the elements
    # of the list (e.g. for v in varlistObj: print(v.name))
    varlistObj = dataObj.varlist
    # Create the list with the variable names in the dataset, using the name attribute of each element of the VariableList object.
    # Note the differenet way of retrieving the name depending on whether the search is case sensitive or not.
    if casesensitive:
        varlistInData = [v.name for v in varlistObj]
    else:
        varlistInData = [v.name.upper() for v in varlistObj]
    # Stop the data step (this function should be called StopDataStep() or the start data step called BeginDataStep()!!!!
    spss.EndDataStep()

    ### 2.- Check the existence of variables in 'vars' in the list of variables retrieved above
    notFound = False
    varsNotFoundList = []
    for var in varlist:
        if casesensitive and var not in varlistInData or \
           not(casesensitive) and var.upper() not in varlistInData:
            notFound = True
            varsNotFoundList.append(var)

    if log:
        if notFound:
            if data == "*":
                print "\nThe following " + str(len(varsNotFoundList)) + " variables were NOT found in the active dataset (" + dataname + "):"
            else:
                print "\nThe following " + str(len(varsNotFoundList)) + " variables were NOT found in dataset " + dataname + ":"
            for v in varsNotFoundList:
                print "\t" + v
        else:
            if data == "*":
                print "\nAll variables found in the active dataset (" + dataname + ")"
            else:
                print "\nAll variables found in dataset " + dataname

    # Return True when all variables are found
    return not(notFound), varsNotFoundList
####################################### CheckVariables ########################################



############################################ Save #############################################
# 2009/01/28
def Save(data=None, out=None, format="sav", dir=None):
    """Saves a dataset into a file of the specified format. Supported: SAV, XLS (default: SAV)

    data:               Name of the dataset to be saved. If empty the active dataset is used.
                        Default: None
    out:                Name (WITHOUT EXTENSION) of the output file.
                        If ommitted the name passed in 'data' is used.
                        Default: None
    format:             Format for the output file.
                        Possible values: SAV, XLS (not case sensitive)
                        Default: 'sav'
    dir:                Directory where the dataset is saved. If empty the working directory is used.
                        Default: None
    """

    #-- DATA
    if data:
        spss.Submit("dataset activate " + data)

    #-- FORMAT
    if not format:
        format = "sav"
        
    #-- DIR
    if not dir:
        dir = tempdir_

    #-- OUT
    if not out:
        out = data
    outfile = dir + "/" + out + "." + format

    if format.lower() == "xls":
        spss.Submit(r"""
save translate outfile = '%(outfile)s'
/type=%(format)s
/cells=values
/fieldnames
/missing=recode
/replace.
""" %locals())
    else:
        spss.Submit("save outfile = '%(outfile)s'" %locals())
############################################ Save #############################################
