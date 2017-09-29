#' @title Quality assurance / quality control for UWIN database
#'
#' @description
#' \code{do_qaqc} is a utility function that calls all other QA/QC functions
#'   available in \code{uwinr} for the tables that are loaded
#'   via \code{\link{collect_tables}}. Currently, the tables where QA/QC
#'   functions exist include: Visits (\code{\link{visits_qaqc}}) and
#'   Photos (\code{\link{photos_qaqc}}).
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#' @param show_error_file If \code{TRUE}, then an error report will be opened
#'   up if there are errors in the UWIN database. This error report will direct
#'   you to a number of csv files within an errors sub-folder within your
#'   working directory.
#'
#' @return Returns the list object from \code{\link{collect_tables}}.
#'   Furthermore, this function will create a sub-folder in your
#'   working directory titled\code{error_reports} if it does not exist and
#'   populate thatsub-folder with an error report titled
#'   \code{error_report_DATE.txt} where
#'   \code{DATE} is the current date called via \code{\link{Sys.Date}}. This
#'   error report will describe potential issues with the data in your
#'   UWIN database and point you out to a number of csv files that further
#'   describe these errors.
#'
#' @author Mason Fidino
#'
#' @examples
#'
#' # do qaqc, assuming you had loaded data with collect_tables
#' uwin_list <- do_qaqc(uwin_test)
#'
#' @export
#' @importFrom utils write.csv
#' @importFrom magrittr "%>%"
#'
#'
do_qaqc <- function(uwin_data = NULL, show_error_file = TRUE) {
  oname <- deparse(substitute(uwin_data))

  # create error name
  fpath<-paste0("./error_reports/error_report_", Sys.Date(),".txt")

  uwinr:::create_error_file(fpath)

  # Do Visits check if it is in uwin_data
  if ("Visits" %in% names(uwin_data)) {
  uwin_data <- visits_qaqc(uwin_data, fpath)
  }

  # Do photos check if it is in uwin_data
  if ("Photos" %in% names(uwin_data)) {
    uwin_data <- photos_qaqc(uwin_data, fpath)
  }

  if(length(readLines(fpath)) == 17) {
    message("\nNo errors found\n")
    return(uwin_data)
  } else {
  message("do_qaqc finished, check error report for problems.\nIf show_error_file = TRUE (the default) than this will open automatically.\nIf not, look in error_reports subfolder within your current working directory.")

  if(show_error_file) {
     # open in Rstudio if that is what you are using
    if ( Sys.getenv("RSTUDIO") == "1" ) {
    get("file.edit")(fpath)
    } else { # else open it in some other text editor
      file.show(fpath)
    }
  }

  return(uwin_data)
  }
}



#' @title Check 'Visits' table for errors
#'
#' @description
#' \code{visits_qaqc} looks for data entry errors in the 'Visits' table
#'   in the UWIN database.
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#'   If the \code{Visits} table is not within this object an error will occur.
#' @param file_conn The file path in which to write errors to supplied
#'   as a character string. This argument is managed automatically if
#'   \code{\link{do_qaqc}} is called instead. If left \code{NULL} then
#'   \code{visits_qaqc} will create a \code{error_reports} sub-folder
#'   in the working directory and populate it with an error report titled
#' \code{error_report_DATE.txt} where
#' \code{DATE} is the current date called via \code{\link{Sys.Date}}
#'
#' @return Returns \code{uwin_data}, will stop if data is not correctly entered
#'   and \code{stop_on_error} is \code{TRUE}.
#'
#'   \code{visits_qaqc} also creates a new column in the visits table titled
#'   \code{SurveyID} which concatenates the \code{LocationID}, \code{SeasonID},
#'   and year from \code{VisitDate}, seperated by a dash (e.g.,
#'   \code{"449-4-17"}).
#'
#' @author Mason Fidino
#'
#' @examples
#'
#' uwin_list <- visits_qaqc(uwin_data = uwin_test)
#'
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom stats na.omit
#' @importFrom dplyr select one_of group_by mutate distinct filter left_join ungroup
#'
visits_qaqc <- function(uwin_data = NULL, file_conn = NULL){
  if (!exists("Visits", uwin_data)){
    stop("The uwin data list does not include the Visits table,
      include 'Visits' in the 'tables' argument of collect_tables ")
  }
  visits_log <- data.table::data.table(uwin_data$Visits)
  visits_log$VisitDate <- as.Date(visits_log$VisitDate, format = "%Y-%m-%d")
  visits_log$Action1ID <- factor(visits_log$Action1ID,
    levels = as.character(0:6))
  visits_log$Action2ID <- factor(visits_log$Action2ID,
    levels = as.character(0:6))
  visits_log$Action3ID <- factor(visits_log$Action3ID,
    levels = as.character(0:6))

  # vector that shows what errors occured

  #-------------
  ### QAQC ERROR: Same actions set twice in a visit
  #-------------
  if (nrow(visits_log[visits_log$Action2ID == "1",]) > 0) {
    visits_log[visits_log$Action2ID == "1",] <- within(
      visits_log[Action2ID == "1",],{
      Action2ID <- Action1ID #Move Action1 number to Action2
      Action1ID <- "1" #Make Action1 a "1"
    })
  }
  # the above bit of script fixes the column if Action1 and Action2 are unique.
  # if both are equal to 1 then we will still have an error.  We will
  # want to print this out.
  # If there is still an error, make a table of it.
  if (nrow(visits_log[visits_log$Action2ID == "1",]) >0){ # error 1
    errors <- 1
  } else {
    errors <- 0
  }

  # do the same thing with Action 3 and Action 1
  if(nrow(visits_log[visits_log$Action3ID == "1",]) > 0){
    to_switch <- which(visits_log$Action3ID == "1")
    visits_log$Action3ID[to_switch] <- visits_log$Action1ID[to_switch]
    visits_log$Action1ID[to_switch] <- "1"

  }

  # the same for Action3 and Action1
  if (nrow(visits_log[visits_log$Action3ID == "1",]) >0){ # error 2
    errors <- c(errors, 1)
  } else {
    errors <- c(errors, 0)
  }

#-------------
### QAQC ERROR: Surveys where camera was set twice
#-------------

# set Action1ID as the key to the data.table
data.table::setkey(visits_log, Action1ID)

# Look for duplicates
visits_log <- uwinr:::create_surveyID(visits_log)

# get only the actions and surveyID, and make it long format
just_actions <- suppressWarnings(data.table::melt(subset(visits_log,
  select = grep("Action|SurveyID",
  names(visits_log))), id.vars = "SurveyID"))

# get table of actions
n_actions <- t(table(just_actions$value,just_actions$SurveyID))

# get only camera set actions
ones <- n_actions[, grep("^1$", colnames(n_actions))]

# get instances where camera set twice
issues <- names(ones)[which(ones>1)]
to_check_extra_ones <- visits_log[which(visits_log$SurveyID %in% issues),]
# say something if write_errors = FALSE
if (nrow(to_check_extra_ones)>0) { # error 3
  errors <- c(errors, 1)
} else {
  errors <- c(errors, 0)
}

#-------------
### QAQC ERROR: Surveys where camera was not set
#-------------
# this uses the ones vector from last QAQC
issues <- names(ones)[which(ones == 0)]
# get rows from visits_log
to_check_no_ones <- visits_log[visits_log$SurveyID %in% issues,]

# messge if write_errors is FALSE
if (nrow(to_check_no_ones)>0) { # error 4
  errors <- c(errors, 1)
} else {
  errors <- c(errors, 0)
}
# write errors if they occur and write_errors = TRUE

#-------------
### QAQC ERROR: Camera removed twice from a site in a season
#-------------
sixes <- n_actions[, grep("^6$", colnames(n_actions))]
issues <- names(sixes)[which(sixes>1)]
to_check_extra_sixes <- visits_log[visits_log$SurveyID %in% issues,]

if (nrow(to_check_extra_sixes)>0) { # error 5
  errors <- c(errors, 1)
} else {
  errors <- c(errors, 0)
}

#-------------
### QAQC ERROR: Camera only has < 2 or > 3 rows per season
#-------------

sids <- table(visits_log$SurveyID)

# less than 2
lt2 <- names(sids)[which(sids < 2)]

# more than 3
mt3 <- names(sids)[which(sids > 3)]

if (length(lt2) > 0 | length(mt3) > 0) { # error 6
  errors <- c(errors, 1)
  to_check_row_error <- c(lt2, mt3)
} else {
  errors <- c(errors, 0)
}
#-------------
### QAQC ERROR: camera set date < camera check date < camera pull date
#-------------

# note, VisitTypeID is 1 = pull, 2, = check, 3 = set. Thus,
# when we order by VisitTypeID everything is backwards and we want
# to make sure that the visit dates are actually decreasing.
data.table::setkey(visits_log, SurveyID, VisitTypeID)

check_decrease <- visits_log %>% dplyr::group_by(SurveyID) %>%
  dplyr::mutate(DatesIncreasing = any(diff(VisitDate) >= 0),
    IDDecreasing = any(diff(VisitTypeID) < 0)) %>%
  dplyr::select(dplyr::one_of(c("SurveyID", "DatesIncreasing",
    "IDDecreasing"))) %>%
  dplyr::distinct()

# If VisitID is not increasing we have some weird type of error.
if (sum(check_decrease$IDDecreasing) > 0) {
  emes <- c(" There is an error with the VisitTypeID column in the",
    "\t'Visits' table. You have possibly set it as a factor. Please reload",
    "\tthe data and attempt qaqc again.")
  stop(paste(emes, collapse = "\n"))
}
if (sum(check_decrease$DatesIncreasing) > 0) { # error 7
  errors <- c(errors, 1)
  vdate_errors <- visits_log[which(visits_log$SurveyID %in%
      check_decrease$SurveyID[check_decrease$DatesIncreasing == TRUE]),]
} else {
  errors <- c(errors, 0)
}

#-------------
### QAQC ERROR: Active dates manually set to an erroneous date
#-------------
data.table::setkey(visits_log, SurveyID, VisitTypeID)

min_max <- visits_log %>%
  dplyr::mutate( SeaYear = substr( SurveyID, 5, 8 ) ) %>%
  dplyr::group_by( SeaYear ) %>%
  dplyr::mutate( MinVis = min( VisitDate ), MaxVis = max( VisitDate ) ) %>%
  dplyr::select( dplyr::one_of( c( "SeaYear", "MinVis", "MaxVis",
    "ActiveStart", "ActiveEnd", "VisitTypeID", "SurveyID", "VisitID") ) ) %>%
  dplyr::filter( VisitTypeID %in% c( 1, 2 ) ) %>%
  dplyr::mutate( MinThresh = MinVis - as.difftime( 7, units = "days" ),
    MaxThresh = MaxVis + as.difftime( 7, units = "days" ) ) %>%
  dplyr::mutate(MinAct = ActiveStart < as.POSIXct( MinThresh ),
                MaxAct = ActiveEnd   > as.POSIXct( MaxThresh)) %>%
  dplyr::ungroup() %>%
  dplyr::select( dplyr::one_of( c( "SurveyID", "MinAct", "MaxAct",
    "ActiveStart", "ActiveEnd", "MinThresh", "MaxThresh", "VisitID") ) )

# we need to remove instances when the activeDate is wrong due to
# cameras being set wrong (this only should locate incorrect manual changes)

# if set before minimum start
if ( sum( min_max$MinAct, na.rm = TRUE ) > 0 ) {
  manual_start_error <- min_max[which( min_max$MinAct == TRUE ),]
  phot_min_max <- uwin_data$Photos %>% dplyr::group_by( VisitID ) %>%
    dplyr::mutate( MinImageDate = min(ImageDate),
                   MaxImageDate = max(ImageDate)) %>%
    dplyr::select( dplyr::one_of( c( "VisitID", "MinImageDate",
      "MaxImageDate"))) %>%
    dplyr::distinct( . )
  manual_start_error <- dplyr::left_join(manual_start_error,
    phot_min_max, by = "VisitID")
  to_go_e8  <- with( manual_start_error, which( ActiveStart == MinImageDate ))
  if( length( to_go_e8 ) > 0 ) {
  manual_start_error <- manual_start_error[-to_go_e8,]
  }

  manual_start_error <- manual_start_error %>%
    dplyr::select( dplyr::one_of( c( "SurveyID",
      "ActiveStart", "MinThresh") ) )
  if( nrow( manual_start_error ) > 0 ){ # error 8
    errors <- c( errors, 1 )
  } else { # if actually no errors
    errors <- c(errors, 0)
  }
} else {
  errors <- c( errors, 0 )
}

# if end after maximum pull
if ( sum( min_max$MaxAct, na.rm = TRUE ) > 0 ) {
  manual_end_error <- min_max[which( min_max$MaxAct == TRUE ), ]
  phot_min_max <- uwin_data$Photos %>% dplyr::group_by( VisitID ) %>%
    dplyr::mutate( MinImageDate = min(ImageDate),
      MaxImageDate = max(ImageDate)) %>%
    dplyr::select( dplyr::one_of( c( "VisitID", "MinImageDate",
      "MaxImageDate"))) %>%
    dplyr::distinct( . )
  manual_end_error <- dplyr::left_join(manual_end_error,
    phot_min_max, by = "VisitID")
  to_go_e9  <- with( manual_end_error, which( ActiveEnd == MaxImageDate ))
  if( length( to_go_e9 ) > 0 ) {
    manual_end_error <- manual_end_error[-to_go_e9,]
  }

  manual_end_error <- manual_end_error %>%
    dplyr::select( dplyr::one_of( c( "SurveyID",
      "ActiveEnd", "MaxThresh") ) )

  if( nrow( manual_end_error ) > 0 ){ # error 9
  errors <- c( errors, 1 )
  } else {
    errors <- c( errors, 0)
  }
} else {
  errors <- c( errors, 0 )
}

# if set after end

flipped_entries <- visits_log %>%
  dplyr::mutate( SeaYear = substr( SurveyID, 5, 8 ) ) %>%
  dplyr::group_by( SeaYear ) %>%
  dplyr::mutate( MinVis = min( VisitDate ), MaxVis = max( VisitDate ) ) %>%
  dplyr::select( dplyr::one_of( c( "SeaYear", "MinVis", "MaxVis",
    "ActiveStart", "ActiveEnd", "VisitTypeID", "SurveyID", "VisitID") ) ) %>%
  dplyr::filter( VisitTypeID %in% c( 1, 2 ) ) %>%
  dplyr::mutate(StartAftEnd = ActiveStart > as.POSIXct( MaxVis ),
    EndB4Start = ActiveEnd   < as.POSIXct( MinVis))

 # if end occurs before start

if ( sum( flipped_entries$EndB4Start, na.rm = TRUE ) > 0 ) {
  end2quick <- flipped_entries[which( flipped_entries$EndB4Start == TRUE ), ]
  phot_min_max <- uwin_data$Photos %>% dplyr::group_by( VisitID ) %>%
    dplyr::mutate( MinImageDate = min(ImageDate),
      MaxImageDate = max(ImageDate)) %>%
    dplyr::select( dplyr::one_of( c( "VisitID", "MinImageDate",
      "MaxImageDate"))) %>%
    dplyr::distinct( . )
  end2quick <- dplyr::left_join(end2quick,
    phot_min_max, by = "VisitID")
  to_go_e10  <- with( end2quick, which( ActiveEnd == MaxImageDate ))
  if( length( to_go_e10 ) > 0 ) {
    end2quick <- end2quick[-to_go_e10,]
  }

  end2quick <- end2quick %>%
    dplyr::select( dplyr::one_of( c( "SurveyID",
      "ActiveEnd", "MinVis") ) )
  if( nrow( end2quick ) > 0 ){ # error 10
    errors <- c( errors, 1 )
  } else {
      errors <- c( errors, 0)
    }
  } else {
  errors <- c( errors, 0 )
}

# if start occurs after end

if ( sum( flipped_entries$StartAftEnd, na.rm = TRUE ) > 0 ) {
  start_aft_end <- flipped_entries[which(flipped_entries$StartAftEnd == TRUE), ]
  phot_min_max <- uwin_data$Photos %>% dplyr::group_by( VisitID ) %>%
    dplyr::mutate( MinImageDate = min(ImageDate),
      MaxImageDate = max(ImageDate)) %>%
    dplyr::select( dplyr::one_of( c( "VisitID", "MinImageDate",
      "MaxImageDate"))) %>%
    dplyr::distinct( . )
  start_aft_end <- dplyr::left_join(start_aft_end,
    phot_min_max, by = "VisitID")
  to_go_e11  <- with( start_aft_end, which( ActiveStart == MinImageDate ) )
  if( length( to_go_e11 ) > 0 ) {
    start_aft_end <- start_aft_end[-to_go_e11,]
  }
  start_aft_end <- start_aft_end %>%
    dplyr::select( dplyr::one_of( c( "SurveyID",
      "ActiveStart", "MaxVis") ) )
  if( nrow( start_aft_end ) > 0 ){ # error 11
    errors <- c( errors, 1 )
  } else {
      errors <- c( errors, 0)
    }
  } else {
  errors <- c( errors, 0 )
  }

# Check to make sure there are ActiveDates for a given survey

n_na <- visits_log %>% dplyr::filter(is.na(ActiveStart) | is.na(ActiveEnd)) %>%
  dplyr::filter(VisitTypeID != 3 ) %>%
  dplyr::select(dplyr::one_of(c("SurveyID", "VisitID", "VisitTypeID")))



if( nrow(n_na) > 0 ) {
  # check to see if we actually have photos for the site
  has_photos <- dplyr::left_join(n_na, uwin_data$Photos, by = "VisitID") %>%
    dplyr::select(dplyr::one_of(c("SurveyID",
      "VisitID", "ImageDate", "VisitTypeID"))) %>%
    dplyr::group_by(VisitID) %>%
    stats::na.omit(.)

   if(nrow(has_photos) > 0) {

     has_photos <- has_photos %>%
       dplyr::mutate(MinDateInPhotos = as.character(min(ImageDate, na.rm = TRUE)),
         MaxDateInPhotos = as.character(max(ImageDate, na.rm = TRUE))) %>%
       dplyr::select(dplyr::one_of(c("VisitTypeID","SurveyID",
         "VisitID", "MinDateInPhotos", "MaxDateInPhotos" ))) %>%
       dplyr::distinct() %>%
       stats::na.omit(.)

     for(i in 1: nrow( has_photos ) ) {
       visits_log$ActiveStart[visits_log$VisitID == has_photos$VisitID[i]] <-
         has_photos$MinDateInPhotos[i]
       visits_log$ActiveEnd[visits_log$VisitID == has_photos$VisitID[i]] <-
         has_photos$MaxDateInPhotos[i]
     }
     visits_log$ActiveStart[visits_log$VisitID %in% has_photos$VisitID]
    no_active <- data.frame(uwinr:::convert_sid(has_photos$SurveyID, uwin_data),
       has_photos)
    no_active$VisitTypeID <- with(uwin_data$lkupVisitTypes, {
      VisitType[no_active$VisitTypeID]
    })
     errors <- c(errors, 1)
   } else {
     errors <- c(errors, 0)
   }
} else {
  errors <- c(errors, 0)
}





# posting errors
if (is.null(file_conn)) {
  dtime <- Sys.Date()
  file_conn<-paste0("./error_reports/error_report_", dtime,".txt")
}
if (!file.exists(file_conn)) {
  uwinr:::create_error_file(file_conn)
}

if (sum(errors) > 0) {
  # write the files where errors occur
  if (errors[1] == 1) {
    v1 <- visits_log[visits_log$Action2ID == "1", ]
    v1 <- cbind( v1, uwinr:::convert_sid( v1$SurveyID, uwin_data ) )
    write.csv( v1,
      "./error_reports/Visits_action12_error.csv", row.names = FALSE)
  }
  if (errors[2] == 1) {
    v2 <- visits_log[visits_log$Action3ID == "1", ]
    v2 <- cbind( v2, uwinr:::convert_sid( v2$SurveyID, uwin_data ) )
    write.csv( v2 ,
      "./error_reports/Visits_action13_error.csv", row.names = FALSE)
  }
  if (errors[3] == 1) {
    to_check_extra_ones <- cbind( to_check_extra_ones,
      uwinr:::convert_sid( to_check_extra_ones$SurveyID, uwin_data ) )
    write.csv(x = to_check_extra_ones,
      file = "./error_reports/multiple_camera_set_actions.csv",
      row.names = FALSE)
  }
  if (errors[4] == 1) {
    to_check_no_ones <- cbind( to_check_no_ones,
      uwinr:::convert_sid( to_check_no_ones$SurveyID, uwin_data ) )
    write.csv(x = to_check_no_ones,
      file = "./error_reports/absent_camera_set_actions.csv",
      row.names = FALSE)
  }
  if (errors[5] == 1) {
    to_check_extra_sixes <- cbind( to_check_extra_sixes,
      uwinr:::convert_sid( to_check_extra_sixes$SurveyID, uwin_data ) )
    write.csv(x = to_check_extra_sixes,
      file = "./error_reports/camera_removed_twice.csv",
      row.names = FALSE)

  }
  if (errors[6] == 1) {
    to_check_row_error <- data.frame( surveyID = to_check_row_error,
      uwinr:::convert_sid( to_check_row_error, uwin_data ) )
    write.csv(x = to_check_row_error,
        file = "./error_reports/site_season_with_1_or_4plus_records.csv",
        row.names = FALSE)
  }
  if (errors[7] == 1) {
    vdate_errors <- cbind( vdate_errors,
      uwinr:::convert_sid( vdate_errors$SurveyID, uwin_data ) )
    write.csv(x = vdate_errors,
      file = "./error_reports/visit_dates_entered_out_of_order.csv",
      row.names = FALSE)
  }

  if (errors[8] == 1) {
    manual_start_error <- cbind( manual_start_error,
      uwinr:::convert_sid( manual_start_error$SurveyID, uwin_data ) )
    write.csv(x = manual_start_error,
      file = "./error_reports/active_start_set_wrong_manually.csv",
      row.names = FALSE)
  }

  if (errors[9] == 1) {
    manual_end_error <- cbind( manual_end_error,
      uwinr:::convert_sid( manual_end_error$SurveyID, uwin_data ) )
    write.csv(x = manual_end_error,
      file = "./error_reports/active_end_set_wrong_manually.csv",
      row.names = FALSE)
  }

  if (errors[10] == 1) {
    end2quick <- cbind( end2quick,
      uwinr:::convert_sid( end2quick$SurveyID, uwin_data = NULL ) )
    write.csv(x = end2quick,
      file = "./error_reports/active_end_before_set.csv",
      row.names = FALSE)
  }

  if (errors[11] == 1) {
    start_aft_end <- cbind( start_aft_end,
      uwinr:::convert_sid( start_aft_end$SurveyID, uwin_data ) )
    write.csv(x = start_aft_end,
      file = "./error_reports/active_start_after_pull.csv",
      row.names = FALSE)
  }

  if (errors[12] == 1){
    write.csv(x = no_active,
      file = "./error_reports/no_active_dates_in_visits_table.csv",
      row.names = FALSE)
  }
# some text for the error messages
  error_frame <-  c(
"\nThere are rows where Action2ID equals Action1ID during a single\nvisit in the 'Visits' table.\n
Check file 'Visits_action12_error.csv' in error_reports.\n",

"\nThere are rows where Action3ID equals Action1ID during a single\nvisit in the 'Visits' table.\n
Check file 'Visits_action13_error.csv' in error_reports.\n",

"\nThe 'Visits' table has a camera 'set' multiple times at a site within\na season.\nCheck file 'multiple_camera_set_actions.csv' in error_reports.\n",

"\nThe 'Visits' table has a site where other actions were taken\nbut the camera was never 'set'.\n
Check file 'absent_camera_set_actions.csv' in error_reports.\n",

"\nThe 'Visits' table has a site where the camera was removed twice\nat a site within the same season.\n
Check file 'camera_removed_twice.csv in error_reports.\n",

"\nThe 'Visits' table has a site where there is < 2 or > 3 records for\na season (should be between 2 and 3).\n
Check file 'site_season_with_1_or_4plus_records.csv' in error_reports.\n",

"\nThe 'Visits' table has instances when the the VisitDate does not\nincrease from camera set to check to pull.\n
Check file 'visit_dates_entered_out_of_order.csv' in error_reports.\n",

"\n The 'Visits' table has instances when the 'ActiveStart' was manually\nset to a date > 7 days before a sampling season started.\n
Check file 'active_start_set_wrong_manually.csv' in error_reports.\n",

"\n The 'Visits' table has instances when the 'ActiveEnd' was manually\nset to a date > 7 days after a sampling season ended.\n
Check file 'active_end_set_wrong_manually.csv' in error_reports.\n",

"\n The 'Visits' table has instances when the 'ActiveEnd' was manually\nset before a sampling season should have started.\n
Check file 'active_end_before_set.csv' in error_reports.\n",

"\n The 'Visits' table has instances when the 'ActiveStart' was manually\nset after a sampling season should have ended.\n
Check file 'active_start_after_pull.csv' in error_reports.\n",

"\n The 'Visits' table has instances where there is no dates in the\n'ActiveStart' or 'ActiveEnd' for a sampling season but the camera trap took photos.\n However, this has been fixed within the Visits table in R\n(but not in the database).\n
Check file 'no_active_dates_in_visits_table.csv' in error_reports.\n")

  to_spl <- uwinr:::create_split("#")
  error_message <- paste("\nThere are/is", sum(errors),
    "kind(s) of errors in the 'Visits' table.\nThese errors include:\n")
  message_to_print <- paste(error_message,
  paste(error_frame[which(errors == 1)],
  collapse = to_spl), collapse = "")
  uwinr:::fwrt("\n---VISITS TABLE---\n", file_conn)
  uwinr:::fwrt("Errors in the Visits table should be addressed", file_conn)
  uwinr:::fwrt("before further summarizing your data.", file_conn)
  uwinr:::fwrt(message_to_print, file_conn)
  uwinr:::fwrt(uwinr:::create_split("-"), file_conn)
} else {
  uwinr:::fwrt("\n---VISITS TABLE---\n", file_conn)
  uwinr:::fwrt("No errors in 'Visits' table", file_conn)
  uwinr:::fwrt(uwinr:::create_split("-"), file_conn)
}

# get object name of what was included into the function
uwin_data$Visits <- visits_log
return(uwin_data)
}


#' @title Check 'Photos' table for errors
#'
#' @description \code{photos_qaqc} looks for errors in the timestamps of
#'   the photos uploaded into the UWIN database. The 'Visits' table must
#'   also be present in the list object supplied by \code{\link{collect_tables}}
#'   as the timestamps are compared to the set and pull records in this table.
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#'   If the \code{Photos} and \code{Visits} table is not within this object
#'   an error will occur.
#' @param file_conn The file path in which to write errors to supplied
#'   as a character string. This argument is managed automatically if
#'   \code{\link{do_qaqc}} is called instead. If left \code{NULL} then
#'   \code{visits_qaqc} will create a \code{error_reports} sub-folder
#'   in the working directory and populate it with an error report titled
#' \code{error_report_DATE.txt} where
#' \code{DATE} is the current date called via \code{\link{Sys.Date}}
#'
#' @author Mason Fidino
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select one_of group_by mutate distinct filter left_join
#' @export
#' @examples
#'
#' uwin_list <- photos_qaqc(uwin_data = uwin_test)

photos_qaqc <- function(uwin_data = NULL, file_conn = NULL){
  # check to make sure both tables are present
  if (!exists("Visits", uwin_data) &
      !exists("Photos", uwin_data)) {
    which_missing <- c(exists("Visits", uwin_data),
                       exists("Photos", uwin_data))
    names(which_missing) <- c("Visits", "Photos")
    if (sum(which_missing) == 2) {
      which_missing <- paste(names(which_missing),
                             collapse = " and ")
    } else {
      which_missing <- names(which_missing[which_missing == TRUE])
    }
    error_report <-
      paste("\nThe uwin_data list does not include the ",
            which_missing, " table(s).\nBoth Visits and Photos",
            " must be included in the 'tables' arument of\ncollect_tables",
            sep = "")
    stop(error_report)
  }
  # format the time column in Visits
  uwin_data$Visits$VisitTime <- format(uwin_data$Visits$VisitTime,
    "%H:%M:%S")

  # make a DateTime column
  uwin_data$Visits$VisitDateTime <- as.POSIXct(paste(as.character(
    uwin_data$Visits$VisitDate),
    uwin_data$Visits$VisitTime))

 # make SurveyID just in case visits_qaqc was not run.
  if (!"SurveyID" %in%  colnames(uwin_data$Visits)) {
    uwin_data$Visits <- uwinr:::create_surveyID(uwin_data$Visits)
  }

  # merge photos and visits
  varbs <- c("VisitTypeID","VisitDateTime", "ActiveStart",
            "ActiveEnd", "ImageDate", "ImageID", "SurveyID" )
  phvi <- dplyr::left_join(uwin_data$Visits, uwin_data$Photos,
                           by = "VisitID") %>%
    dplyr::select(dplyr::one_of(varbs))

  #-------------
  ### QAQC ERROR: Photos occured > 7 days from camera set
  #-------------
  errors <- rep(0, 2)
  # get the start times

  starts_b4_set <- uwinr:::create_time_check(phvi, start = TRUE)
 # create a table if there are issues
  if (nrow(starts_b4_set) > 0) {
    data.table::setkey(starts_b4_set, SurveyID, ImageID)
    start_summary <- uwinr:::create_photo_time_summary(starts_b4_set)
    write.csv(start_summary,
      "./error_reports/imageDate_start_before_camera_set.csv",
      row.names = FALSE)
    errors[1] <- 1
  }
  #-------------
  ### QAQC ERROR: Photos occured > 7 days from camera pull
  #-------------
  ends_aft_pull <- uwinr:::create_time_check(phvi, start = FALSE)

  if (nrow(ends_aft_pull) > 0) {
    data.table::setkey(ends_aft_pull, SurveyID, ImageID)
    end_summary <- uwinr:::create_photo_time_summary(ends_aft_pull)
    write.csv(end_summary, "./error_reports/imageDate_after_camera_pull.csv",
      row.names = FALSE)
    errors[2] <- 1
  }

  # error posting
  if (is.null(file_conn)) {
    file_conn<-paste0("./error_reports/error_report_", Sys.Date(),".txt")
  }
  if (!file.exists(file_conn)) {
    uwinr:::create_error_file(file_conn)
    close(file_conn)
  }

  if(sum(errors)>0) {
    to_spl <- uwinr:::create_split("-", FALSE)

    uwinr:::fwrt("\n --- PHOTOS TABLE ---\n", file_conn)
    to_add <- c("These errors need to be fixed if possible (e.g., date set wrong)",
      "but will be censored when data are further summarized.",
      "However, the 'Active Dates' in the Visits table must be",
      "manually altered for each site that these errors occur.",
      "If you have already edited these active dates then you",
      "can remove these photos with censor_photos.",
      "see ?censor_photos\n")
    uwinr:::fwrt(paste(to_add, collapse = "\n"), file_conn)
    if(errors[1] == 1) {
      ereport <- c("There are images in the 'Photos' table with timestamps that",
         "are 7 days earlier than the date that the camera was 'set' in",
         "the 'Visits' table. Check file 'imageDate_start_before_camera_set.csv'",
         "in the error_reports sub-folder of your working directory to see",
         "the ImageID's causing this error.")
      uwinr:::fwrt(paste(ereport, collapse = "\n"), file_conn)
      if (sum(errors)>1) {
        uwinr:::fwrt(uwinr:::create_split("#", TRUE), file_conn)
      }
    }
    if(errors[2] == 1) {
       ereport <- c("There are images in the 'Photos' table with timestamps that",
         "are 7 days later than the date that the camera was 'pulled' in",
         "the 'Visits' table. Check file 'imageDate_after_camera_pull.csv'",
         "in the error_reports sub-folder of your working directory to see",
         "the ImageID's causing this error.")
    uwinr:::fwrt(paste(ereport, collapse = "\n"), file_conn)
    }
    uwinr:::fwrt(uwinr:::create_split("-"), file_conn)
  } else {
    uwinr:::fwrt("\n---PHOTOS TABLE---\n", file_conn)
    uwinr:::fwrt("No errors in 'Photos' table", file_conn)
    uwinr:::fwrt(uwinr:::create_split("-"), file_conn)
  }
return(uwin_data)
}


