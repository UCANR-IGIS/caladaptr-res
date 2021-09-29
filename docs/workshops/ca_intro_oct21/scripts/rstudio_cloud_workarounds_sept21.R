############################################################################################
## Additional Tips for Installing caladaptr and all the workshop packages on RStudio Cloud
## Sept 2021

## The following workarounds were used to install the packages on RStudio Cloud. 
## If you run this script in RStudio Cloud *before* you run caladaptr_setup.R, it should work.

## s2 (a dependency of sf) was not installing because it was running out of memory during compilation
## I filed an issue on GitHub and was told you can either wait until a binary version comes out
## (takes a few days), or rent an RStudio Machine with more memory.
## Another workaround (used below) is to install the previous version which is available as a binary
if (!require(remotes)) install.packages("remotes")
if (!require(s2)) remotes::install_version("s2", "1.0.6")
if (!require(sf)) install.packages("sf")

## The binary version of tidyr could not be found. When this happens you can install it
## from CRAN 

if (!require(tidyr)) install.packages("tidyr", dependencies=TRUE, repos="https://cloud.r-project.org")
