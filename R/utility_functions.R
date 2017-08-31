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
#'   UWIN database. If this argument is left blank then \code{tables} is set to
#'   include CameraLocations, Detections, Photos, Species,
#'   StudyAreas, Visits, lkupAction, lkupDetectionStatus,
#'   lkupSeasons, lkupVisitTypes.
#'
#' @return A named list of tables from the UWIN database. Each table will be
#'   returned as a \code{\link{data.table}} instead of a
#'   \code{\link{data.frame}}, as this considerably speeds up
#'   summarizing these data.
#'
#' @author Mason Fidino
#'
#' @examples
#' # not run: dat <- collect_tables("UWIN_DB_CHIL.accdb")
#' # not run: dat <- collect_tables("UWIN_DB_CHIL.accdb", tables = c("Photos", "Visits"))
#'
#' @export
#' @importFrom RODBC odbcConnectAccess2007 sqlFetch odbcClose
#' @importFrom data.table data.table
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
message("Collecting tables")
if (is.null(tables)){
  tables <- c("CameraLocations", "Detections", "Photos",
    "Species","StudyAreas", "Visits","lkupAction","lkupDetectionStatus",
    "lkupSeasons", "lkupVisitTypes" )
}

uwin_data <- lapply(tables, FUN = function(x) {
  data.table::data.table(RODBC::sqlFetch(uwin, x))
})

names(uwin_data) <- tables
RODBC::odbcClose(uwin)
return(uwin_data)
}

#' @title Query data within two sampling periods
#'
#' @description
#' \code{reduce_seasons} queries data for a single season or between
#' two seasons depending on how arguments are filled.
#'
#' @param uwin_data The list object returned from \code{\link{collect_tables}}.
#'
#' @param start A character vector that contains the first sampling period
#'   and year that you would like to query.
#'   \code{start} must be 4 characters long with the first two characters denoting
#'   the sampling season and the last two characters denoting the year
#'   (e.g., April 2017 would be \code{"AP17"}).
#'
#' @param end A character vector that contains the last sampling period and year
#'   that you would like to query.
#'   \code{start} must be 4 characters long with the first two characters denoting
#'   the sampling season and the last two characters denoting the year
#'   (e.g., April 2017 would be \code{"AP17"}).
#'   If only querying one season of data \code{end} should be left as \code{NULL}.
#'
#' @return Returns the list object from \code{\link{collect_tables}} with data
#'   from either a single season (if \code{end} is left NULL) or with data
#'   that lie between the sampling periods specified in \code{start} and
#'   \code{end}. Note that this only queries data in the Visits, Photos, and
#'   Detections table within the Access database.
#'
#' @author Mason Fidino
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#'
#' dat <- do_qaqc(uwin_test)
#' dat <- reduce_seasons(dat, start = "JA16", end = "JU17")
#'
#' # if only collecting data from one season.
#'
#' dat <- do_qaqc(uwin_test)
#' dat <- reduce_seasons(dat, start = "JA16")
#'
#' @export
reduce_seasons <- function(uwin_data = NULL, start = NULL, end = NULL){

  if (nchar(start) != 4) {
    stop("The start argument needs to be 4 characters (e.g., AP17)")
  }

  if (class(end) != "NULL") {
    if (nchar(end) != 4) {
    stop("The end argument must be NULL or 4 characters (e.g., AP17)")
  }}

  if (length(end) == 0) {
    end <- start
  }

  if ("SurveyID" %in% colnames(uwin_data$Visits) == FALSE) {
    stop("qaqc must be done to data before querying it. see ?do_qaqc")
  }

  # get the first two characters from start and end (the seasons)
  season_codes <- c(substr(start, 1, 2), substr(end, 1, 2))

  # get the ID code in lkupSeasons table
  season_codes <- uwin_data$lkupSeasons$ID[uwin_data$lkupSeasons$Season
    %in% season_codes]


  # get the range of seasons
  seasons_used <- seq(min(season_codes), max(season_codes),1)

  # years used
  years <- c(substr(start, 3, 4), substr(end, 3, 4)) %>%
    unique %>% as.numeric

  # get all combinations
  season_year <- expand.grid(seasons_used, years)

  # make season_year the same format as SurveyID in visits table
  season_year <- apply(season_year, 1, paste, collapse = "-") %>% paste0("-",.)

  # make one big grep
  season_grep <- paste(season_year, collapse = "|")

  # reduce visits table
  uwin_data$Visits <- uwin_data$Visits[grep(season_grep,
    uwin_data$Visits$SurveyID)]

  # remove rows from photos that do not have a VisitID in visit table
  uwin_data$Photos <- uwin_data$Photos[uwin_data$Photos$VisitID %in%
    uwin_data$Visits$VisitID]

  # and now the same thing for ImageID in Detections
  uwin_data$Detections <- uwin_data$Detections[uwin_data$Detections$ImageID %in%
    uwin_data$Photos$ImageID]

  return(uwin_data)
}



format_detections <- function(uwin_data = NULL, only_verified = FALSE) {
  if (!exists("Detections", uwin_data)){
    stop("The uwin data list does not include the 'Detections' table,
      include 'Detections' in the 'tables' argument of collect_tables ")
  }
  detections <- uwin_data$Detections
  # remove all deleted ID's (i.e., StatusID == 3)
  detections <- detections[-which(detections$StatusID == 3),]

  # remove Pending ID's (i.e., StatusID == 1)
  if (only_verified) {
    detections <- detections[-which(detections$StatusID == 1),]
  }

  oname <- deparse(substitute(uwin_data))
  uwin_data$Detections <- detections
  #print(oname)
  assign(oname, uwin_data, envir = .GlobalEnv)
}


datetime <- function(x) {
  timedat <- strftime(x$VisitTime,  format="%H:%M:%S")
  dtime <- as.POSIXct(paste(x$VisitDate, timedat),
                      format="%Y-%m-%d %H:%M:%S")
  return(dtime)
}


#' @importFrom magrittr "%>%"
#' @importFrom dplyr select one_of bind_rows right_join
convert_sid <- function( sid = NULL, uwin_data = NULL ) {

  # converts a surveyID to a vector [LocationName, Season, Year]
  # if given a vector it will return a dataframe.

  split_sid <- unlist(strsplit(sid, "-")) %>% as.numeric %>%
    matrix(., ncol = 3, nrow = length(sid), byrow = TRUE) %>%
    data.table::data.table(.)

  colnames(split_sid) <- c("LocationID", "SeasonID", "Year")

  LocationName <- uwin_data$CameraLocations %>%
    dplyr::select( dplyr::one_of( c( "LocationName", "LocationID" ) ) ) %>%
    right_join( . , split_sid, by = "LocationID" ) %>%
    dplyr::select(dplyr::one_of( c( "LocationName" ) ) )

  Season <- with(uwin_data$lkupSeasons,
    Season[split_sid$SeasonID] ) %>%  as.character

  to_return <- data.frame(LocationName, Season, Year = split_sid$Year)
  return(to_return)

  }
