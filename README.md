# ccmppWPP

Development of a CCMPP specification and implementation(s) at UNPD &amp; Co.


# Installation

1. Ensure the version of *R* your have installed is 4.0.2 or higher.
2. If you are using *R Studio*, ensure it is 1.1.1073 or higher.
3. If you are using *Windows* install *Rtools40* by following the instructions [here](https://cran.r-project.org/bin/windows/Rtools/ "link to install Rtools40"). 
4. Launch *R* (with or without *R Studio*) and issue the following commands^[If this is the first time running `install.packages` for this version of *R* on your system you may be asked to select a *CRAN* mirror. You can choose any mirror; the choice does affect the functioning of the package].

    ```
    install.packages("remotes", dependencies = TRUE) #only do once per user/workstation
	
    remotes::install_github(https://github.com/markalava/ccmppWPP, ref = "develop/sara")
    ```
  
 
