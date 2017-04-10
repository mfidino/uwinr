#' @title Check visits table for errors
#'
#' @description
#' \code{visits_qaqc} looks for data entry errors in the visits table
#'  in the UWIN database. This currently checks if the same action occurs
#'  twice within a single visit, or if a camera is 'set' multiple
#'  times in a single season.
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#'   If the \code{Visits} table is not within this object an error will occur.
#' @param stop_on_error A logical statement to determine if this function
#'   stops when it finds any errors in the Visits table. Currently defaults to
#'   \code{TRUE}.
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
#' uwin_list <- visits_qaqc(uwin_data = uwin_list)
#'
#' # This function even works without assigning the object
#' visits_qaqc(uwin_data = uwin_list)
#' @export
#' @import data.table

visits_qaqc <- function(uwin_data = NULL, stop_on_error = TRUE){
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
visits_log$SurveyID <- with(visits_log, {
  paste(LocationID, SeasonID,
  substr(lubridate::year(VisitDate),3,4), sep = "-")})

# get only the actions and surveyID, and make it long format
just_actions <- suppressWarnings(reshape2::melt(subset(visits_log,
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


# posting errors

if (sum(errors) > 0) {
  # write the files where errors occur
  if (errors[1] == 1) {
    write.csv(visits_log[visits_log$Action2ID == "1",],
      "Visits_action12_error.csv", row.names = FALSE)
  }
  if (errors[2] == 1) {
    write.csv(visits_log[visits_log$Action3ID == "1",],
      "Visits_action13_error.csv", row.names = FALSE)
  }
  if (errors[3] == 1) {
    write.csv(x = to_check_extra_ones,
      file = "multiple_camera_set_actions.csv",
      row.names = FALSE)
  }
  if (errors[4] == 1) {
    write.csv(x = to_check_no_ones,
      file = "absent_camera_set_actions.csv",
      row.names = FALSE)
  }
  if (errors[5] == 1) {
    write.csv(x = to_check_extra_sixes,
      file = "camera_removed_twice.csv",
      row.names = FALSE)
  }

# some text for the error messages
  error_frame <-  c(
"\nThere are rows where Action2ID equals Action1ID during a single\nvisit in the 'Visits' table.\nCheck file 'Visits_action12_error.csv' in your working\ndirectory.\n",

"\nThere are rows where Action3ID equals Action1ID during a single\nvisit in the 'Visits' table. Check file 'Visits_action13_error.csv' in your working\n directory.\n",

"\nThe 'Visits' table has a camera 'set' multiple times at a site within\na season.\nCheck file 'multiple_camera_set_actions.csv' in your working\ndirectory.\n",

"\nThe 'Visits' table has a site where other actions were taken\nbut the camera was never 'set'.\n
Check file 'absent_camera_set_actions.csv' in your working directory.\n",

"\nThe 'Visits' table has a site where the camera was removed twice\nat a site within the same season.\n
Check file 'camera_removed_twice.csv in your working directory.\n")

  to_split <- paste(c("\n",rep("-", 50), "\n"), collapse = "")
  error_message <- paste("\nThere are", sum(errors),
    "different kinds of errors in the 'Visits' table.\nThese errors include:\n")
  message_to_print <- paste(error_message,
    paste(error_frame[which(errors == 1)],
    collapse = to_split), collapse = "")

  if (stop_on_error) {
    message(message_to_print)
    stop("Errors in Visits table. Scroll up in console to see all reported errors.")
  } else {
    message(message_to_print)
  }
} else {
  message("No errors in 'Visits' table.")
}
#e1

# get object name of what was included into the function
oname <- deparse(substitute(uwin_data))
uwin_data$Visits <- visits_log
assign(oname, uwin_data, envir = .GlobalEnv)

}


