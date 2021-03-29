An Example Documentation Folder
================================

#  Purpose

This is a template for a "Documentation" directory that one might use (copy/paste) into a project
from which to build out `pagedown` report(s) and a `bookdown` website using the child RMD files
included in the Universal_Content repo.

#  Included files

* The `Report_Setup.R` file contains `R` code from which appropriate assets can be copied into the 
report directory.  It also has code to `source(...)` other R scripts that appropriately combine 
report configuration and RMD file information into `list` objects that are subsequently used by 
functions in the `Literasee` package to create a "NCIEA" themed report and website.
* The `Make_Report.R` file contains `R` code to render the website/report using the `bookdown` and 
`pagedown` packages respectively.
