

create_surveyID <- function(x = NULL){
  if (!"SurveyID" %in% colnames(x)) {
    #season_switch <- c(2, 3, 4, 1)
    x$SurveyID <- with(x, {
      paste(LocationID, SeasonID,
        substr(data.table::year(VisitDate),3,4), sep = "-")})
  }

  return(x)
}



create_time_check <- function(x = NULL, start = NULL) {
  # used within photos_qaqc

  # if start, then we compare to the camera set (3), else compare t
  # camera pull (1)
  vt <- ifelse(start, 3, 1)

  chck <- x %>% dplyr::filter(VisitTypeID == vt) %>%
    dplyr::select(dplyr::one_of(c("VisitDateTime", "SurveyID"))) %>%
    distinct

  to_join_chck <- x %>% dplyr::select(dplyr::one_of(c("ImageDate",
    "ImageID", "SurveyID")))

  chck <- dplyr::left_join( chck, to_join_chck, by = "SurveyID")
  if (start) {
    issues <- which(as.numeric(difftime(chck$ImageDate,
      chck$VisitDateTime,
      units = "days")) < -7)
  } else {
    issues <- which(as.numeric(difftime(chck$ImageDate,
      chck$VisitDateTime,
      units = "days")) > 7)
  }
  return(chck[issues,])
}


create_photo_time_summary <- function(x = NULL) {
  data.table::setkey(x, SurveyID, ImageID)
  ans <- x %>% dplyr::group_by(SurveyID) %>%
    dplyr::mutate(lags = ImageID - lag(ImageID)) %>%
    dplyr::mutate(lags = ifelse(is.na(lags), 1, lags)) %>%
    dplyr::mutate(lags = lags == 1) %>%
    dplyr::mutate(lags = rep(rle(lags)$lengths, rle(lags)$lengths)) %>%
    dplyr::mutate(lags = ifelse(lags == 1, lead(lags), lags)) %>%
    dplyr::group_by(SurveyID, lags) %>%
    dplyr::summarise(FirstID = min(ImageID),
      LastID = max(ImageID), N_errors = length(ImageID)) %>%
    dplyr::mutate(InSequence = (LastID - (FirstID - 1)) == N_errors) %>%
    dplyr::select(dplyr::one_of(c("SurveyID", "FirstID", "LastID",
      "N_errors")))
  return(ans)
}

# used to create base error file
# empty
create_error_file <- function(file_conn = NULL) {
  if (!"./error_reports" %in% list.dirs()) {
    dir.create("./error_reports")
  }
  close(file(file_conn, open = "wt"))
  to_split <- paste(rep("-", 50), collapse = "")
  uwinr:::fwrt(to_split, file_conn)
  uwinr:::fwrt(paste("Error report:", Sys.Date()), file_conn)
  uwinr:::fwrt(to_split, file_conn)
}

fwrt <- function(x, fp = NULL) {
  cat(x, file = fp, sep = "\n", append = TRUE)
}

create_split <- function(x = NULL, addn = TRUE) {
  if(addn) {
  to_spl <- paste(c("\n",rep(x, 50), "\n"), collapse = "")
  } else {
    to_spl <- paste(rep(x, 50), collapse = "")
  }
  return(to_spl)
}

#' The number of days each camera trap is operable per season
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}
#'   and after it has been through \code{\link{do_qaqc}} .
#'   If the \code{Visits} table is not within this object an error will occur.
#' @param binomial_detections If \code{TRUE}, \code{create_observation_matrix}
#'   will return the total number of days a site was operable. If \code{FALSE},
#'   \code{create_observation_matrix} will return a vector of binary elements that
#'   take the value of \code{1} if it was operable on a given day or \code{0} if
#'   it was not.
#' @param drop_tails This will check if the date range for a site taken from
#'   the camera trap images occurs between when the camera set date and camera
#'   pull date entered into the \code{Visits} table of the UWIN database. If
#'   the date range is > 7 days before the first recorded camera set for a
#'   sampling season or > 7 days after the last recorded camera pull for a
#'   sampling season then those days will be removed from the analysis.
#'
#' @return A list with two elements. The first element, \code{mat}, contains
#'   either a survey ID (i.e., site-season-year abbrevaition) by date matrix
#'   if \code{binomial_detections = FALSE} or a named vector of the number of
#'   days each survey ID was active. The second element, \code{days_active},
#'   is a vector of the days that camera traps were active in a given season.
#'
#' @export
#'
#' @examples
#'
#'  # read in the data
#'  dat <- collect_tables("UWIN_DB_CHIL.accdb")
#'
#'  # apply qaqc
#'  dat <- do_qaqc(dat)
#'
#'  # collect only one season of data
#'  dat <- reduce_seasons(dat, start = "JU17")
#'
#'  # make observation matrix
#'  obser_matrix <- create_observation_matrix(dat)

create_observation_matrix <- function(uwin_data = NULL,
  binomial_detections = FALSE,
  drop_tails = FALSE) {

  if(!"SurveyID" %in% colnames(uwin_data$Visits)) {
    stop("Apply do_qaqc to uwin_data before using create_possible_days.
       See ?do_qaqc")
  }


  # We are going to create the active days based off of the ActiveStart
  # and ActiveEnd columns that are in the visits table. However,
  # we want to make sure the dates are within some margin
  # of the actual visits.

  # connect the perfect SurveyID's in Visits to the photos table.
  # We omit sites that have zero photos and camera sets.
  # Note: We will need camera sets later though, but they are
  #       already in the visits table
  photoID <- x$Visits %>%
    dplyr::select(dplyr::one_of(c("SurveyID", "VisitID", "VisitDateTime",
      "ActiveStart", "ActiveEnd", "VisitTypeID"))) %>%
    na.omit


  # split into checks and pull
  checks <- photoID[VisitTypeID == 2]
  pulls  <- photoID[VisitTypeID == 1]

  # if we have checks make a checks_list
  if (nrow(checks) > 1) {
    days_check <- vector("list", nrow(checks))
    for (i in 1:nrow(checks)){
      # make sequence of days from active start to end
      days_check[[i]] <- seq(checks$ActiveStart[i],
                             checks$ActiveEnd[i], by = "1 day")
    }
    names(days_check) <- checks$SurveyID # sid to each list
    days_check <- sapply(days_check, format, format = "%Y-%m-%d") %>%
      sapply(.,as.POSIXct) # date/time to date
  }

  # make a pulls_list.
  days_pull <- vector("list", nrow(pulls))
  for (i in 1:nrow(pulls)){ # see comments above
    days_pull[[i]] <- seq(pulls$ActiveStart[i],
                          pulls$ActiveEnd[i], by = "1 day")
  } # close for // 1 to # of pull events
  names(days_pull) <- pulls$SurveyID
  days_pull <- sapply(days_pull, format, format = "%Y-%m-%d") %>%
    sapply(.,as.POSIXct)

  # if no checks make pull = check (gets removed later)
  if (nrow(checks) == 0) {
    days_check <- days_pull

  }
  # combine the two lists

    check_key <- names(days_check) # sids on check
    pull_key <- names(days_pull) # sids on pull
    both_key <- pull_key[pull_key %in% check_key] # sids in both
    just_pull <- pull_key[-which(pull_key %in% check_key)] # sids only in pull
    just_check <- check_key[-which(check_key %in% pull_key)] # sids in check
    both_list <- vector("list", # list for all dates
      length = length(both_key) + length(just_pull) + length(just_check))
    names(both_list) <- unique(photoID$SurveyID) # name it based off of photoID

    # fill up both_list when data in check and pull
    for (i in 1:length(both_key)) {
      both_list[[both_key[i]]] <- c(unlist(days_check[both_key[i]][[1]]),
        unlist(days_pull[both_key[i]][[1]]))
    }

    # if there are more pulls then checks, fill those in
    if( length( just_pull ) > 0 ) {
    for ( i in 1:length( just_pull ) ) {
      both_list[[just_pull[i]]] <- days_pull[just_pull[i]][[1]]
    }}
    # if there are any checks w/o pulls, fill those in
    if ( length( just_check ) > 0 ) {
      for ( i in 1:length( just_check ) ) {
        both_list[[just_check[i]]] <- days_check[just_check[i]][[1]]
      }
    }

    # remove duplicate days and sort earliest to latest date
    for( i in 1:length( both_list ) ) {
      if( sum( duplicated( both_list[[i]] ) ) >  0 ) { # if duplicates
      both_list[[i]] <- sort( both_list[[i]][-which(
        duplicated( both_list[[i]] )==TRUE)] )
      } else { # close if & open else// if duplicate dates
        both_list[[i]] <- sort( both_list[[i]] )
      } # close else // if no duplicates
    } # close for // 1: length both_list

  # melt list to dataframe
 days_long <- reshape2::melt(both_list)
  # make it wide
 days_wide <- reshape2::dcast(data = days_long, formula = L1 ~ value,
   fun.aggregate = length)
 # make rownames the surveyID
 row.names(days_wide) <- days_wide$L1
 # remove the L1 columns, which is just the surveyID repeated
 days_wide <- days_wide[-which(colnames(days_wide) == "L1")]
 # this would be a bernoulli matrix for a single season for detections
 obs_mat <- list(mat = days_wide, days_active = colnames(days_wide) )
 colnames(obs_mat$mat) = 1:ncol(obs_mat$mat)


 # Remove data that is a week before camera set or after camera pull.
 if( drop_tails ) {
   min_max <- x$Visits %>%
     dplyr::mutate( sea_yr = substr( SurveyID, 5, 8 ) ) %>%
     dplyr::group_by( sea_yr ) %>%
     dplyr::mutate( min_vis = min( VisitDate ), max_vis = max( VisitDate ) ) %>%
     dplyr::select( dplyr::one_of( c( "sea_yr", "min_vis", "max_vis" ) ) ) %>%
     dplyr::distinct( . ) %>%
     mutate( min_thresh = min_vis - as.difftime( 7, units = "days" ),
             max_thresh = max_vis + as.difftime( 7, units = "days" ) ) %>%
     dplyr::select( dplyr::one_of( c( "sea_yr", "min_thresh", "max_thresh") ) )

   min_max_obs <- data.table( sea_yr =
       unique( substr( row.names( obs_mat$mat ), 5, 8) ),
                             min_obs = min( obs_mat$days_active ),
                             max_obs = max( obs_mat$days_active ) )
   joined_thresh <- dplyr::left_join( min_max, min_max_obs, by = "sea_yr" ) %>%
     mutate( flag_min = min_obs < min_thresh,
             flag_max = max_obs > max_thresh )


   if( sum( joined_thresh$flag_min ) > 0 ) {

     sea_yr_to_change <- joined_thresh$sea_yr[
       which( joined_thresh$flag_min > 0) ]

     for( i in 1: length( sea_yr_to_change ) ) {
       rows_2_change <- grep( sea_yr_to_change[i], row.names( obs_mat$mat ) )

       one_thresh <- joined_thresh[joined_thresh$sea_yr == sea_yr_to_change[i],]
       cols_to_cut <- which( obs_mat$days_active < one_thresh$min_thresh )
       obs_mat$mat <- obs_mat$mat[ ,-cols_to_cut]
       obs_mat$days_active <- obs_mat$days_active[-cols_to_cut]
     } # close for // 1 : length sea_yr_to_change
   } # close if // flag min > 0

   if( sum( joined_thresh$flag_max ) > 0 ) {
     sea_yr_to_change <- joined_thresh$sea_yr[
       which( joined_thresh$flag_max > 0) ]

     for( i in 1: length( sea_yr_to_change ) ) {
       rows_2_change <- grep(sea_yr_to_change[i], row.names(obs_mat$mat))

       one_thresh <- joined_thresh[joined_thresh$sea_yr == sea_yr_to_change[i],]
       cols_to_cut <- which( obs_mat$days_active > one_thresh$max_thresh)
       obs_mat$mat <- obs_mat$mat[,-cols_to_cut]
       obs_mat$days_active <- obs_mat$days_active[-cols_to_cut]
     } # close for // over sea_yr_to_change
   } # close if // flag_max > 0

   if ( with( joined_thresh, sum( flag_min, flag_max) ) > 0 ) {
     warning("There are sampling days that were removed because they
were > 7 days before the first recorded camera set for a sampling season or
> 7 days after the last recorded camera pull.")
   } # close if // flag_min or flag_max  > 0
 } # close if // drop_tails = TRUE

 # Check to see if the number of obs > 45
 if(ncol(obs_mat$mat) > 45) {
   warning("One of your sampling seasons has > 45 days. Check to make sure
that the date/time data on your images is correct.")
 } # close if // ncol obs_mat$mat > 45

   if(binomial_detections == TRUE) {
     obs_mat$mat <- t(t(rowSums(obs_mat$mat)))
   } # close if // binonimal_detections = TRUE

 # make the days active POSIXct
 obs_mat$days_active <- as.POSIXct(obs_mat$days_active)

 # return obs_mat
 return(obs_mat)

} # close function // create_possible_days



#' Create species detection non-detection matrix
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}
#'   and after it has been through \code{\link{do_qaqc}} and reduced to the
#'   seasons of interest via \code{\link{reduce_seasons}}.
#' @param observation_matrix The list object returned by
#'   \code{\link{create_observation_matrix}}.
#' @param binomial_detections If \code{TRUE}, \code{create_detection_matrix}
#'   will return the total number of days a species was observed at a site.
#'   If \code{FALSE}, \code{create_detection_matrix} will return a vector of
#'   binary elements that take the value of \code{1} if a species was observed
#'   on a given day, \code{0} if it was not, or \code{NA} if the camera was
#'   not operable.
#' @param species A vector of the species names from the \code{ShortName}
#'   column of the \code{Species} table within the UWIN database. If left NULL
#'   and \code{select_species = FALSE} then a detection matrix
#'   will be made for each species in the \code{Species} table.
#' @param select_species If TRUE, a pop-up list will open up that you can use
#'   to select the species you would like to make a detection matrix for. You
#'   can hold \code{Ctrl} to select multiple species that are seperated by
#'   other species you do not want to make a detection matrix for. Defaults
#'   to FALSE.
#'
#' @return
#' @export
#'
#' @examples
create_detection_matrix <- function( uwin_data = NULL,
  observation_matrix = NULL, binomial_detections = FALSE,
  select_species = FALSE, species = NULL){

  # simple error checks
  if( !is.logical( select_species ) ) {
    stop( paste0("\nselect_speices must be a logical (T/F) statement"))
  }
  if( !is.logical( binomial_detections ) ) {
    stop( paste0("\nbinomial_detections must be a logical (T/F) statement"))
  }
  if( !is.list( uwin_data ) ){
    stop( paste0("\nuwin_data must be a list object." ) )
  }
  if( !"SurveyID" %in% colnames(uwin_data$Visits ) ){
    stop( paste0("\nuwin_data must be put through do_qaqc first.",
      "\nSee ?do_qaqc") )
  }
  # end simple error checks

  # use select.list
  if( select_species & length( species ) == 0 ) {
    choices <- select.list( as.character( uwin_data$Species$ShortName ),
      multiple = TRUE, graphics = TRUE )
  } # end if // select_species = TRUE and species = NULL

  # use species names provided by species argument
  if( length( species ) > 0 ) {
    # make sure species is a character vector
    if( !is.character( species ) ){
      stop(paste0("\nThe species argument must either be NULL or a\n",
                  "character vector. It is currently neither of these."))
    }
    # check to see if there are species put into argument that are not
    # in the species table
    error_species <- species[ which( !species %in% uwin_data$Species$ShortName)]
    stop(paste0("\nA species provided in the species argument is either\n",
          "misspelledor not in the Species table of the UWIN database.\n",
          "This error was generated from the following entries:\n\n",
      paste(error_species, collapse = "\n") ) )
    # make choices species
    choices <- species
    } # end if // species has a character vector argument

  # get all of the species
  if( select_species == FALSE & length( species ) == 0 ){
     choices <- uwin_data$Species$ShortName
  } # end if // select_species = FALSE and no species provided

  # get only photos of the species you want
  # convert choices to SpeciesID
  choices_num <- uwin_data$Species %>%
    dplyr::filter( ShortName %in% choices ) %>%
    dplyr::select( dplyr::one_of( "SpeciesID", "ShortName" ) )

  # These are the detections we want
  detects <- uwin_data$Detections %>%
    dplyr::filter( SpeciesID %in% choices_num$SpeciesID )

  # only get photos and detections that are within days active
  # observation matrix
  photos <- uwin_data$Photos %>%
    dplyr::filter( ImageID %in% detects$ImageID) %>%
    mutate(Date = format(ImageDate, format = "%Y-%m-%d")) %>%
    dplyr::filter(Date %in% format(observation_matrix$days_active,
      format = "%Y-%m-%d")) %>%
    dplyr::left_join(., detects, by = "ImageID") %>%
    dplyr::left_join(., uwin_data$Visits, by = "VisitID") %>%
    dplyr::select( dplyr::one_of( c( "ImageDate", "Date", "SpeciesID",
      "Individuals", "SurveyID")))

  # make an site x day x species array for a season
  nsite <- uwin_data$Visits$SurveyID %>% unique %>%
    strsplit("-") %>%
    lapply( ., function(x) x[-1] ) %>%
    lapply( ., paste, collapse = "-" ) %>%
    unlist %>%
    table


  nseason <- length( nsite ) # not using yet
  sites_per_survey <- vector("list", length = nseason )


  nday <- length( observation_matrix$days_active )
  nspec <- length( choices )
  ymat <- vector( "list", length = nseason )

  for( i in 1:nseason ) {
    # get surveyID for a given season
    sites_per_survey[[i]] <- uwin_data$Visits %>%
      dplyr::filter(.,grep(paste0(names(nsite)[i],"$"), SurveyID)) %>%
      dplyr::select( dplyr::one_of( "SurveyID" ) ) %>% unique
    # make a blank detection matrix
    ymat[[i]] <- array( NA, dim = c(nsite, nday, nspec ),
      dimnames = list(sites_per_survey[[i]]$SurveyID, NULL,
      choices_num$ShortName))

  }

  # determine which sites these detections occured



}




