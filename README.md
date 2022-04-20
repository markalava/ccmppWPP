# ccmppWPP

Implementation of the cohort component method of population projection (CCMPP).

# Installation

Below are basic instructions to download and install the package on your system. You may be able to skip some or all of steps 1--5 depending on your current system set-up. 

1. Ensure the version of *R* your have installed is 4.0.2 or higher. See [here](https://www.r-project.org/) for installation instructions.
2. If you are using *R Studio* (optional), ensure it is version 1.1.1073 or higher. See [here](https://rstudio.com/) for installation instructions. 
3. 
    1. [Option 1] Launch *R* (with or without *R Studio*) and issue the following commands:

        ```
        install.packages("remotes", dependencies = TRUE) #only do once per user/workstation
    	
        remotes::install_github(https://github.com/markalava/ccmppWPP, 
    	                        build_vignettes = TRUE, dependencies = TRUE)
        ```
	    
        If this is the first time running `install.packages` for this version of *R* on your system you may be asked to select a *CRAN* mirror. You can choose any mirror; the choice does affect the functioning of the package.
	
    2. [Option 2] Scroll to the top of this page and click "Releases", which appears to the left of the page. Click "assests" in the version you want to install and download either the .tar.gz or .zip file (.zip for *Microsoft Windows* systems only). Take note of where the downloaded file is stored on your system. Launch *R* (with or without *R Studio*) and issue the following command:
	
	```
	install.packages("[path-to-downloaded-file]", repos = NULL)
	```
	
4. Once installation is complete, issue the following command to use package functions:

    ```
	library(ccmppWPP)
	```
	
    Vignettes that explain the package structure and functions can be read by issuing the commands:
	
	```
	vignette("CCMPP Input Data Structures Quick Guide", package = "ccmppWPP")
	
    vignette("S3 Classes for CCMPP Input Data Structures", package = "ccmppWPP")
	```
		

# Troubleshooting

Here is a grab bag of various issues that may come up in no particular order. Please feel free to suggest additions, corrections, clarifications. 

1. **Extra *R* packages seem to be required.** 

  Required packages should be installed automatically during step 4 of the installation instructions. However, you could try
  
    ```
	install.packages(c("testthat", "ggplot2", "knitr", "rmarkdown", 
	                   "Rcpp", "stringr", "DemoTools", "devtools", "remotes", "roxygen2",
					   "DiagrammeR"),
	                 dependencies = TRUE)
    ```
	
	before step 4 to head off most missing package warnings that might occur. 
	
2. **Cannot switch to a different *Git* branch.**

  If you see a message to the effect that "changes to local files would be overwritten", you can either delete the files listed if you don't want them or commit your changes to your local branch, then try again.
  
3. **Error messages like "`'add_column' not found`" or "`'column_to_rownames' not found`" (etc.)**

  Try adding `library(tidyverse)` before using the package functions. This should be fixed in future releases.
  
4. **Error messages like "`'lt_abridged_from_complete'` not found`" (etc.)**

  Try issuing the command `library(DemoTools)` before using the package functions. This should be fixed in future releases.
  
5. **The vignettes do not load.**

  You may see this message if you have used install 'Option 1', or are working with the source code and have loaded it with, for example, `devtools::load_all()` or the equivalent via *R Studio*'s menus. To read the vignettes they need to have been 'built'. You will need to use `R CMD build` from your operating system's terminal / command line interface (e.g., `cmd.exe` in Windows), or use `devtools::install(pkg = ".", build = TRUE, build_vignettes = TRUE)` from within *R*. In both cases you will first need to set the working directory of the terminal or *R* to the package directory (which contains the package's 'DESCRIPTION' file). If you're trying this from within *R* (or *R Studio*) you may need to quit and restart before and/or after trying the install. If you restart after the install, remember to `library(ccmppWPP)` or `devtools::load_all()` again once *R* has restarted. 
  
  To get PDF versions of the help files you will need to download and install a version of the [*LaTeX*](https://www.latex-project.org/) typesetting system. Some options include:

  * [TinyTex](https://yihui.org/tinytex/)
  * [TeXLive](https://www.tug.org/texlive/)
  * [MiKTeX](https://miktex.org/)

Follow the installation instructions for the version you choose. The path to *LaTeX* executables should be added to your system's "PATH" environment variable; this will typically be done by the installers. 
  
  You could try using install 'Option 2', at least to get the vignettes. This option uses pre-built versions of the package that include the vignettes.
