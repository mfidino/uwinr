#' @title Collect tables from UWIN database
#'
#' @description
#' \code{collect_tables} uses the \code{\link{RODBC}} package to connect
#'   to the UWIN database and collect the requested tables.
#'
#'   Note: For this function to work the Microsoft Access Database Engine
#'   must be installed. It can be found
#'   \href{https://www.microsoft.com/en-us/download/details.aspx?id=13255}{here}.
#'
#' @param database File name of the UWIN Access database as a character vector.
#'   If \code{database} does contain an absolute path, the file name is relative
#'   to the current working directory.
#' @param tables A character vector of the table names to be pulled from the
#'   UWIN database. If this argument is left blank than \code{tables} is set to
#'   \code{c("CameraLocations", "Detections", "Photos", "Species",
#'   "StudyAreas", "Visits", "lkupAction", "lkupDetectionStatus",
#'   "lkupSeasons")}.
#'
#' @return A named list of tables from the UWIN database. Each table will be
#'   returned as a \code{\link{data.table}} instead of a
#'   \code{\link{data.frame}}, as this considerably speeds up
#'   summarizing these data.
#'
#' @author Mason Fidino
#'
#' @examples
#' dat <- collect_tables("UWIN_DB_CHIL.accdb")
#' dat <- collect_tables("UWIN_DB_CHIL.accdb", tables = c("Photos", "Visits"))
#'
#' @export
collect_tables <- function(database = NULL, tables = NULL) {
# Error handling
if (is.null(database)) {
  stop("Include the file path to UWIN database.")
}
if (length(database) > 1) {
  stop("Multiple paths included in database")
}
if (grep("accdb", database)!= 1) {
  stop("include file type to database (i.e., database = file_path.accdb)")
}
message("Connecting to UWIN database")
uwin <- RODBC::odbcConnectAccess2007(database)
message("Connected")
message("Collecting tables")
if (is.null(tables)){
  tables <- c("CameraLocations", "Detections", "Photos",
    "Species","StudyAreas", "Visits","lkupAction", "lkupSeasons" )
}

uwin_data <- lapply(tables, FUN = function(x) {
  data.table::data.table(RODBC::sqlFetch(uwin, x))
})

names(uwin_data) <- tables
RODBC::odbcClose(uwin)
return(uwin_data)
}

#' @title Format the detection table in UWIN database
#'

format_detections <- function(uwin_data = NULL, only_verified = FALSE) {
  if (!exists("Detections", uwin_data)){
    stop("The uwin data list does not include the 'Detections' table,
      include 'Detections' in the 'tables' argument of collect_tables ")
  }
  detections <- uwin_data$Detections
  # remove all deleted ID's
  togo <- which(detections$StatusID == 3)
  detections <- detections[-togo,]

  # remove Pending ID's
  if (only_verified) {
    togo <- which(detections$StatusID == 1)
    detections <- detections[-togo,]
  }

  oname <- deparse(substitute(uwin_data))
  uwin_data$Detections <- detections
  print(oname)
  assign(oname, uwin_data, envir = .GlobalEnv)
}
