

create_surveyID <- function(x = NULL){
  if (!"SurveyID" %in% colnames(x)) {
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
      units = "days")) < -30)
  } else {
    issues <- which(as.numeric(difftime(chck$ImageDate,
      chck$VisitDateTime,
      units = "days")) > 30)
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

create_possible_days <- function(x = NULL) {
  visits <- x$Visits

  # camera condition 1 the whole time
  perfects <- visits %>% dplyr::group_by(SurveyID) %>%
    dplyr::summarise(nchck = length(CameraConditionID),
                     nones = sum(CameraConditionID == 1)) %>%
    dplyr::filter(nones == nchck)




  y <- uwin_data$Photos

 test <-  y %>% dplyr::group_by(VisitID) %>%
    dplyr::mutate(Start = min(ImageDate),
                  End = max(ImageDate)) %>%
    dplyr::select(dplyr::one_of(c("Start", "End", "VisitID"))) %>%
    dplyr::distinct() %>%
   dplyr::summarise(Start = as.POSIXct(format(Start, format = "%Y-%m-%d")),
                    End = as.POSIXct(format(End, format = "%Y-%m-%d")),
                    VisitID = VisitID)

 t2 <- apply(data.frame(test), 1, function(x) {seq(x[1], x[2], by = "1 day")})
 vl <- vector("list", nrow(test))
 for(i in 1:nrow(test)){
   vl[[i]] <- seq(test$Start[i], test$End[i], by = "1 day")
 }

}






