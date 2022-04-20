# ccmppWPP

Implementation of the cohort component method of population projection (CCMPP).

# Installation

Below are basic instructions to download and install the package on your system. You may be able to skip some or all of steps 1--5 depending on your current system set-up. 

1. Ensure the version of *R* your have installed is 4.0.2 or higher. See [here](https://www.r-project.org/) for installation instructions.
2. If you are using *R Studio* (optional), ensure it is version 1.1.1073 or higher. See [here](https://rstudio.com/) for installation instructions. 
3. If you are using Microsoft's *Windows* operating system install *Rtools40* by following the instructions [here](https://cran.r-project.org/bin/windows/Rtools/ "link to install Rtools40").
4. Ensure you have a *TeX* distribution installed; see section "PDF Help Files via LaTeX" below.
5. Unless you have done this previously, generate a *Github* *personal access token* [here](https://github.com/settings/tokens). You can either
    1. Save this to the "GITHUB_PAT" [environment variable](https://en.wikipedia.org/wiki/Environment_variable) on your system (recommended); or
	2. Paste it in to the `install_github` command below.
6. Launch *R* (with or without *R Studio*) and issue the following commands (paste in your personal access token as indicated):

    ```
    install.packages("remotes", dependencies = TRUE) #only do once per user/workstation
	
    remotes::install_github(https://github.com/markalava/ccmppWPP, 
	                        ref = "master", build_vignettes = TRUE, dependencies = TRUE,
	                        auth_token = [your-auth-token-here]) 
                            # If you saved your token to your system's "GITHUB_PAT" environment variable you can omit the last argument.
    ```
	
	You can change `ref = "master"` to `ref = "develop/mark"` or `ref = "develop/sara"` to install from different branches. 
	
	If this is the first time running `install.packages` for this version of *R* on your system you may be asked to select a *CRAN* mirror. You can choose any mirror; the choice does affect the functioning of the package.
	
7. Once installation is complete, issue the following command to use package functions:

    ```
	library(ccmppWPP)
	```
	
    Vignettes that explain the package structure and functions can be read by issuing the commands:
	
	```
	vignette("CCMPP Input Data Structures Quick Guide", package = "ccmppWPP")
	
    vignette("S3 Classes for CCMPP Input Data Structures", package = "ccmppWPP")
	```
		

## PDF Help Files via LaTeX 

To get PDF versions of the help files you will need to download and install a version of the [*LaTeX*](https://www.latex-project.org/) typesetting system. Some options include:

* [TinyTex](https://yihui.org/tinytex/)
* [TeXLive](https://www.tug.org/texlive/)
* [MiKTeX](https://miktex.org/)

Follow the installation instructions for the version you choose. The path to *LaTeX* executables should be added to your system's "PATH" environment variable; this will typically be done by the installers. 


# Sourcing the Package and Contributing 

As well as installing the package you can *source* the functions straight from this repository in a number of ways. If you are not familiar with this process please consider going through Part I  (Chapter 3 at the very least) of [*R Packages*](https://r-pkgs.org/index.html) by Wickham and Bryan to ensure your system is properly set up.


## Using *R Studio*

Using *R Studio* is completely optional but is recommended for first-time users and those not yet familiar with *R*. It has many useful features for managing and working with packages, including those hosted remotely like this one on *GitHub*. If you are working with the source code it is a good idea to set up an *R Studio* "project" via the 'File' menu. *R Studio* can do this from already downloaded source files or by linking directly to the remote *GitHub* repository. See Wickam and Bryan (link above) and/or the resources linked to next for instructions on how to set this up. 
  
 
## Contributing via GitHub

To contribute to the package you will need a (free) account with [*GitHub*](https://docs.github.com/en/github/getting-started-with-github) *and* a [*Git*](https://git-scm.com/) distribution. 

If you are using *R Studio* you can interact with *GitHub* from within the program. Here are some additional learning resources to help you get started:

* [*Version Control with Git and SVN*](https://support.rstudio.com/hc/en-us/articles/200532077?version=1.3.1073&mode=desktop)
* [*Happy Git and GitHub for the useR*](https://happygitwithr.com/)


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

  You may see this message if you are working with the source code and have loaded it with, for example, `devtools::load_all()` or the equivalent via *R Studio*'s menus. To read the vignettes they need to have been 'built'. You will need to use `R CMD build` from your operating system's terminal / command line interface (e.g., `cmd.exe` in Windows), or use `devtools::install(pkg = ".", build = TRUE, build_vignettes = TRUE)` from within *R*. In both cases you will first need to set the working directory of the terminal or *R* to the package directory (which contains the package's 'DESCRIPTION' file). If you're trying this from within *R* (or *R Studio*) you may need to quit and restart before and/or after trying the install. If you restart after the install, remember to `library(ccmppWPP)` or `devtools::load_all()` again once *R* has restarted. 
  
