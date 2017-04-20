#' @title Quality assurance / quality control for UWIN database
#'
#' @description
#' \code{do_qaqc} is a utility function that calls all other QA/QC functions
#' that are available in \code{uwinr} for the tables that are loaded
#' via \code{\link{collect_tables}}. Currently, the tables where QA/QC
#' functions exist include: Visits (\code{\link{visits_qaqc}}) and
#' Photos (\code{\link{photos_qaqc}}).
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#'
#' @return Returns the list object from \code{\link{collect_tables}}.
#' Furthermore, this function will create a sub-folder in your
#' working directory titled\code{error_reports} if it does not exist and
#' populate thatsub-folder with an error report titled
#' \code{error_report_DATE.txt} where
#' \code{DATE} is the current date called via \code{\link{Sys.Date}}. This
#' error report will describe potential issues with the data in your
#' UWIN database and point you out to a number of csv files that further
#' describe these errors.
#'
#' @author Mason Fidino
#'
#' @examples
#' uwin_list <- collect_tables("UWIN_DB_CHIL.accdb")
#' uwin_list <- do_qaqc(uwin_list)
#'
#' @export
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
    assign(oname, uwin_data, envir = .GlobalEnv)
  } else {
  message("do_qaqc finished, check error report for problems.\nIf show_error_file = TRUE (the default) than this will open automatically.\nIf not, look in error_reports subfolder")

  if(show_error_file) {
    file.edit(fpath)
  }

  assign(oname, uwin_data, envir = .GlobalEnv)
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
#' # read in the data
#' uwin_list <- collect_tables("UWIN_DB_CHIL.accdb")
#'
#' # apply qaqc to 'Visits' table
#' uwin_list <- visits_qaqc(uwin_data = uwin_list)
#'
#' @export
#'
visits_qaqc <- function(uwin_data = NULL, file_conn = NULL){
  if (!exists("Visits", uwin_data)){
    stop("The uwin data list does not include the Visits table,
      include 'Visits' in the 'tables' argument of collect_tables ")
  }
  visits_log <- data.table(uwin_data$Visits)
  visits_log$VisitDate <- as.Date(visits_log$VisitDate, format = "%Y-%m-%d")
  visits_log$Action1ID <- as.factor(visits_log$Action1ID)
  visits_log$Action2ID <- as.factor(visits_log$Action2ID)
  visits_log$Action3ID <- as.factor(visits_log$Action3ID)

  # vector that shows what errors occured

  #-------------
  ### QAQC ERROR: Same actions set twice in a visit
  #-------------
  if (nrow(visits_log[visits_log$Action2ID == "1"]) > 0) {
    visits_log[visits_log$Action2ID == "1"] <- within(
      visits_log[Action2ID == "1"],{
      Action2ID <- Action1ID #Move Action1 number to Action2
      Action1ID <- "1" #Make Action1 a "1"
    })
  }
  # the above bit of script fixes the column if Action1 and Action2 are unique.
  # if both are equal to 1 then we will still have an error.  We will
  # want to print this out.
  # If there is still an error, make a table of it.
  if (nrow(visits_log[visits_log$Action2ID == "1"]) >0){
    errors <- 1
  } else {
    errors <- 0
  }

  # do the same thing with Action 3 and Action 1
  if (nrow(visits_log[visits_log$Action3ID == "1"]) >0){
    visits_log[visits_log$Action3ID == "1"] <-within(
      visits_log[visits_log$Action3ID == "1"],{
      Action3ID <- Action1ID
      Action1ID <- "1"
    })
  }

  # the same for Action3 and Action1
  if (nrow(visits_log[visits_log$Action3ID == "1"]) >0){
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
if (nrow(to_check_extra_ones)>0) {
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
if (nrow(to_check_no_ones)>0) {
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

if (nrow(to_check_extra_sixes)>0) {
  errors <- c(errors, 1)
} else {
  errors <- c(errors, 0)
}

#-------------
### QAQC ERROR 6: Camera only has < 2 or > 3 rows per season
#-------------

sids <- table(visits_log$SurveyID)

# less than 2
lt2 <- names(sids)[which(sids < 2)]

# more than 3
mt3 <- names(sids)[which(sids > 3)]

if (length(lt2) > 0 | length(mt3) > 0) {
  errors <- c(errors, 1)
  to_check_row_error <- c(lt2, mt3)
} else {
  errors <- c(errors, 0)
}
#-------------
### QAQC ERROR 7: camera set date < camera check date < camera pull date
#-------------

# note, VisitTypeID is 1 = pull, 2, = check, 3 = set. Thus,
# when we order by VisitTypeID everything is backwards and we want
# to make sure that the visit dates are actually decreasing.
data.table::setkey(visits_log, SurveyID, VisitTypeID)

check_decrease <- visits_log %>% dplyr::group_by(SurveyID) %>%
  dplyr::mutate(DatesIncreasing = any(diff(VisitDate) > 0),
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
if (sum(check_decrease$DatesIncreasing) > 0) {
  errors <- c(errors, 1)
  vdate_errors <- visits_log[which(visits_log$SurveyID %in%
      check_decrease$SurveyID[check_decrease$DatesIncreasing == TRUE]),]
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
    write.csv(visits_log[visits_log$Action2ID == "1",],
      "./error_reports/Visits_action12_error.csv", row.names = FALSE)
  }
  if (errors[2] == 1) {
    write.csv(visits_log[visits_log$Action3ID == "1",],
      "./error_reports/Visits_action13_error.csv", row.names = FALSE)
  }
  if (errors[3] == 1) {
    write.csv(x = to_check_extra_ones,
      file = "./error_reports/multiple_camera_set_actions.csv",
      row.names = FALSE)
  }
  if (errors[4] == 1) {
    write.csv(x = to_check_no_ones,
      file = "./error_reports/absent_camera_set_actions.csv",
      row.names = FALSE)
  }
  if (errors[5] == 1) {
    write.csv(x = to_check_extra_sixes,
      file = "./error_reports/camera_removed_twice.csv",
      row.names = FALSE)

  }
  if (errors[6] == 1) {
    write.csv(x = to_check_row_error,
        file = "./error_reports/site_season_with_1_or_4plus_records.csv",
        row.names = FALSE)
  }
  if (errors[7] == 1) {
    write.csv(x = vdate_errors,
      file = "./error_reports/visit_dates_entered_out_of_order.csv",
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
Check file 'visit_dates_entered_out_of_order.csv' in error_reports.\n")

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
#' @examples
#' # read in the data
#' uwin_list <- collect_tables("UWIN_DB_CHIL.accdb")
#'
#' # conduct qaqc on 'Photos' table
#' uwin_list <- photos_qaqc(uwin_list)

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
  ### QAQC ERROR: Photos occured > 1 month from camera set
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
  ### QAQC ERROR: Photos occured > 1 month from camera pull
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
    to_add <- c("These errors should be fixed if possible (e.g., date set wrong)",
      "but will be censored when data are further summarized\n")
    uwinr:::fwrt(paste(to_add, collapse = "\n"), file_conn)
    if(errors[1] == 1) {
      ereport <- c("There are images in the 'Photos' table with timestamps that",
         "are 30 days earlier than the date that the camera was 'set' in",
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
         "are 30 days later than the date that the camera was 'pulled' in",
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


