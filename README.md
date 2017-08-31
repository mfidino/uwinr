# uwinr


An R package to query and summarize camera trap data from the Urban Wildlife Information Network (UWIN) database. Furthermore, uwinr provides a number of quality assurance / quality control checks to ensure that data has been entered correctly. A general workflow using uwinr looks like:

1. Pull a copy of the UWIN database into R.
2. Perform checks on the database to ensure that data is correctly entered (If there are errors, view the error report that is generated, address the issues within the database, and return to step 1 to see if you have correctly fixed these errors).
3. Query the seasons of data you are interested in summarizing.
4. Summarize the data the way you would like to (e.g., detection histories for each species).

## Setup

This package was written in R version 3.3.3. Please update R to at least this version before trying to install this package. 
The [installr](https://cran.r-project.org/web/packages/installr/index.html) package makes updating R easy.

The Microsoft Access Database Engine must be installed in order to query the UWIN database from R. It can be found [here](https://www.microsoft.com/en-us/download/details.aspx?id=13255) and requires a computer that can run windows.

### Installation

Downloading this package requires the use of `install_github()` function from the *devtools* package.

``` r
install.packages("devtools") # if you do not already have this R package
library(devtools) # load the devtools package
install_github("mfidino/uwinr") # install uwinr
```

## Usage

``` r
library(uwinr)

# bring in your data, assuming that at copy of your UWIN database is within your 
# current working directory
uwin_db <- collect_tables( "MY_UWIN_DATABASE.accdb" )

# apply quality assurance / quality control
qaqc_uwin <- do_qaqc( uwin_db )

# remove photos that have an incorrect date / time
censored_uwin <- censor_photos(qaqc_uwin)

# get only the seasons we are interested in
uwin_ju17 <- reduce_seasons( censored_uwin, start = "JU17" )

# create an observation matrix (the number of days a camera trap was active each season)
obs_mat <- create_observation_matrix( uwin_ju17 )

# create a detection history
de_mat <- create_detection_matrix( uwin_ju17, obs_mat, binomial_detections = TRUE)
```

For further documentation on all off the functions in uwinr, see the manual located [here](https://github.com/mfidino/uwinr/blob/master/uwinr.pdf).

