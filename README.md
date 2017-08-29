# uwinr
-------

An R package to query and summarize camera trap data from the Urban Wildlife Information Network (UWIN) database.

## Setup
--------

This package was written in R version 3.3.3. Please update R to at least this version before trying to install this package. 
The [installr](https://cran.r-project.org/web/packages/installr/index.html) package makes updating R easy.

The Microsoft Access Database Engine must be installed in order to query the UWIN database from R. It can be found [here](https://www.microsoft.com/en-us/download/details.aspx?id=13255) and requires a computer that can run windows.

### Installation

Downloading this package requires the use of `install_github()` function from the *devtools* package.

`install.packages("devtools") # if you do not already have this R package`
`library(devtools) # load the devtools package`
`install_github(mfidino/uwinr) # install uwinr`

