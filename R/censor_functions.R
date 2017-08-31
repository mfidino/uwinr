#'
#' @title Remove UWIN Images With Faulty Timestamps
#'
#' @description \code{censor_photos} removes photos from the \code{'Photos'}
#'   table and associated detections from the \code{'Detections'} table if the
#'   time stamp on the photo is > 7 days before a camera was set at
#'   a site or > 7 days after a camera was pulled at a site.
#'   These latter values are stored in the \code{'Visits'} table.
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#'
#' @return Returns the list object, but with offending photos and detections
#'   removed from their respective tables. This also will update the active
#'   dates within the 'Visits' table
#'
#' @details This function requires the following tables in the list object to
#'   work : \code{'Photos'}, \code{'Detections'}, and \code{'Visits'}.
#'
#'   Furthermore, \code{\link{do_qaqc}} must be applied to the uwin list object
#'   before photos can be summarised.
#'
#'   @importFrom magrittr %>%
#'   @importFrom dplyr left_join select one_of
#'
#' @examples
#'
#'  # check for errors
#'  uwin_list <- do_qaqc(uwin_test)
#'
#'  # censor the photos
#'  uwin_list <- censor_photos(uwin_data = uwin_list)
#'
#' @export

censor_photos <- function(uwin_data = NULL) {
  # check to make sure both tables are present
  if (!exists("Visits", uwin_data) &
      !exists("Photos", uwin_data) &
      !exists("Detections", uwin_data)) {
    which_missing <- c(exists("Visits", uwin_data),
                       exists("Photos", uwin_data),
                       exists("Detections", uwin_data))
    names(which_missing) <- c("Visits", "Photos","Detections")
    if (sum(which_missing) >1) {
      which_missing <- paste(names(which_missing),
                             collapse = ", ")
    } else {
      which_missing <- names(which_missing[which_missing == TRUE])
    }
    error_report <-
      paste("\nThe uwin_data list does not include the follwings table(s): ",
            which_missing, ".\ 'Visits', 'Photos', and 'Detections' ",
            " must be included in the 'tables' argument of\ncollect_tables",
            sep = "")
    stop(error_report)
  }
  if (!"VisitDateTime" %in% colnames(uwin_data$Visits)) {
    emess <- c(" You are attempting to censor data before running do_qaqc.",
               "\tPlease  use do_qaqc before summarizing these data.")
    stop(paste(emess, collapse = "\n"))
  }

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

  # Remove images (and resulting detections)
  # that started 30 days before set
  start_b4 <- uwinr:::create_time_check(phvi, TRUE)
  if (nrow(start_b4) > 0 ) {
    to_go <- which(uwin_data$Photos$ImageID %in% start_b4$ImageID)
      if (length(to_go) > 0) {
        uwin_data$Photos <- uwin_data$Photos[-to_go,]
      }
    to_go <- which(uwin_data$Detections$ImageID %in% start_b4$ImageDate)
      if (length(to_go) > 0) {
        uwin_data$Detections <- uwin_data$Detections[-to_go,]
      }
  }

  # Remove images (and resulting detections)
  # that occured 30 days after pull
  ends_aft <- uwinr:::create_time_check(phvi, FALSE)
  if (nrow(ends_aft) > 0) {
    to_go <- which(uwin_data$Photos$ImageID %in% ends_aft$ImageID)
    if (length(to_go) > 0) {
      uwin_data$Photos <- uwin_data$Photos[-to_go,]
    }
    to_go <- which(uwin_data$Detections$ImageID %in% ends_aft$ImageDate)
    if (length(to_go) > 0) {
      uwin_data$Detections <- uwin_data$Detections[-to_go,]
    }
  }

  cat(paste0(nrow(start_b4), " photos were censored."))
  return(uwin_data)

}


