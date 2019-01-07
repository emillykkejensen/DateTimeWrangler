#' DateTime to columns
#'
#' Extract various info from DateTime such as weekday, time of day, during working hours, etc.
#'
#' @param datetime A datetime object
#'
#' @return A Data Frame including the following columns:
#'
#' \itemize{
#'   \item \strong{datetime:} The inputtet datetime
#' }
#'
#' @import data.table magrittr
#'
#' @export
datetime_to_cols <- function(datetime, timeOfDay = "danish"){

  datetime_objects <- c("POSIXct", "POSIXt")
  if(!(class(datetime)[1] %in% datetime_objects)) stop(paste0("datetime has to be a datetime object - curretly one of: ", paste(datetime_objects, collapse = ", ")))

  datetimeDT <- data.table::data.table(datetime = datetime)
  datetimeDT[, date := data.table::as.IDate(datetime)]
  datetimeDT[, time := data.table::as.ITime(datetime)]
  datetimeDT[, year := data.table::year(datetime)]
  datetimeDT[, quarter := data.table::quarter(datetime)]
  datetimeDT[, month := data.table::month(datetime)]
  datetimeDT[, week := data.table::week(datetime)]
  datetimeDT[, yday := data.table::yday(datetime)]
  datetimeDT[, mday := data.table::mday(datetime)]
  datetimeDT[, wday := (unclass(as.IDate(datetime)) + 4L)%%7L]
  datetimeDT[, hour := data.table::hour(datetime)]
  datetimeDT[, minute := data.table::minute(datetime)]
  datetimeDT[, second := data.table::second(datetime)]

  datetimeDT[, timeOfDay := time_of_day(hour)]

  datetimeDT[, isWeekend := FALSE]
  datetimeDT[wday %in% c(6L, 7L), isWeekend := TRUE]

  datetimeDT[, withinWorkingHours := FALSE]
  datetimeDT[hour >= 8L & hour < 17L & isWeekend == FALSE, withinWorkingHours := TRUE]

  datetimeDT[, isHoliday := is.holiday(date)]


}
