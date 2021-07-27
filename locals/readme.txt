Content of `locals` directory
-----------------------------
DM-27-Jul-2021

It contains programs that are local to the client or server running the programs.

The main program as of Jul-2021 is a Python module:
	natglobals.py
which defines at least:
- the tempdir_ Python variable defining the directory where SPSS temporary files are stored (e.g. by the Summary() function in DMSpssMining.py)
- the Rdir_ Python variable defining the directory where the R tools for SPSS are located.

The values of these variables should be updated ONLY when the Python & R tools for SPSS are going to be accessed from a local computer.
The variables should be updated to the paths in the local computer leading to the meaning of the variables mentioned above.
