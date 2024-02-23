#-------------------------------------------------------------------------------
#' Run all of the calculations to go from database export to calcualtion of final BMDh values
#'
#' `driver` Run all of the calculations to go from database export to calcualtion of final BMDh values
#'
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @param user The username for the MySQL database. The database instance is
#' hard-coded in the function setDBConn().
#' @param password The user's MySQL database password.
#' @export
#-------------------------------------------------------------------------------
driver <- function(toxval.db="res_toxval_v95",sys.date="2024-02-23",user,password) {
  printCurrentFunction()
  export.for.bmdh(toxval.db,user,password)
  filter.for.bmdh(toxval.db,sys.date)
  bmdh.per.study(toxval.db,sys.date)
  bmdh.per.chemical(toxval.db,sys.date)
  bmdh.percentile.plot(T,toxval.db,sys.date,minstudies=3,cutoff.logsd=2)
}
