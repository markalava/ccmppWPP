# ccmppWPP

Development of a CCMPP specification and implementation(s) at UNPD &amp; Co.


# Installation

1. Ensure the version of *R* your have installed is 4.0.2 or higher. See [here](https://www.r-project.org/) for installation instructions.
2. If you are using *R Studio* (optional), ensure it is version 1.1.1073 or higher. See [here](https://rstudio.com/) for installation instructions. 
3. If you are using *Windows* install *Rtools40* by following the instructions [here](https://cran.r-project.org/bin/windows/Rtools/ "link to install Rtools40"). 
4. Launch *R* (with or without *R Studio*) and issue the following commands. 

    ```
    install.packages("remotes", dependencies = TRUE) #only do once per user/workstation
	
    remotes::install_github(https://github.com/markalava/ccmppWPP, ref = "develop/sara")
    ```
	
	You can change `ref = "develop/sara"` to `ref = "develop/mark"` or `ref = "master"` to install from different branches. 
	
	If this is the first time running `install.packages` for this version of *R* on your system you may be asked to select a *CRAN* mirror. You can choose any mirror; the choice does affect the functioning of the package.
	

## PDF Help Files via LaTeX 

To get PDF versions of the help files a version of *LaTeX* is required. Some options include:
* [TinyTex](https://yihui.org/tinytex/)
* [TeXLive](https://www.tug.org/texlive/)
* [MiKTeX](https://miktex.org/)
  
 
## Contributing via GitHub

To contribute to the package you will need a (free) account with [*GitHub*](https://github.com/) *and* a [*Git*](https://git-scm.com/) distribution. If you are using *R Studio* you can interact with *GitHub* from within the program. 
