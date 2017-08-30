#' A sample of the Chicago UWIN database
#'
#' A list object that is a small subset of the Chicago UWIN database. This
#' is used so that the examples in the help files are executable.
#'
#'
#' Detections
#' @format A list object of length 8.
#'  \describe{
#'    \item{Detections}{A data.table with 6698 rows and 6 columns. Contains info
#'      on the species observed within a photo.}
#'    \item{Photos}{A data.table with 26360 rows and 3 columns. Used to link
#'      detections to a particular image, which can then be linked to a camera
#'      deployment.}
#'    \item{Species}{A data.table with 30 rows and 5 columns. Links species
#'      common names to a unique numeric ID used wtihin the database.}
#'    \item{Visits}{A data.table with 139 rows and 13 columns. Holds data
#'      for given camera deployment.}
#'    \item{lkupAction}{A lookup table that links actions taken during a camera
#'      trap deployment to a numeric ID.}
#'    \item{lkupDetecitonStatus}{A lookup that that links what happened to a
#'      camera trap during a deployment to a numeric ID}
#'    \item{lkupSeasons}{A lookup table that links the season a camera was
#'      deployed to a numeric ID.}
#'    \item{lkupVisitTypes}{A lookup table that links the type of deployment
#'      events (i.e., camera set, camera check, and camera pull) to a numeric ID.}
#'    }
#'
#'  @source A whole bunch of camera trapping
"uwin_test"
