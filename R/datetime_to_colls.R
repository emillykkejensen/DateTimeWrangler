#' DateTime to columns
#'
#' Extract various info from a DateTime value such as day of the week, time of day, day of the year,
#' whether the time and day is during working hours, if it is a holiday or national vacation day, etc.
#'
#' @param datetime A datetime object.
#' @param country What country to get country specific values (holidays, vacations and time of day) from.
#' See details for a list of countries.
#' @param suffixes 	A character specifying the suffixes to be used for all columns.This is
#' particularly handy if you want to keep track of multiple datetime inputs. NULL == no suffixes.
#' @param return character; What class to return the output data in - either 'data.table',
#' 'data.frame' or 'tibble'.
#' @param verbose Show what is going on...
#'
#' @details The following countries are supported:
#'
#' \itemize{
#'   \item denmark
#' }
#'
#' @return A Data Frame including the following columns (class in parenthesis):
#'
#' \itemize{
#'   \item \strong{datetime:} The inputtet datetime parameter (same class as your input)
#'   \item \strong{date:} Date (IDate or Date)
#'   \item \strong{time:} Time (ITime)
#'   \item \strong{year:} Year (integer)
#'   \item \strong{quarter:} Quarter of the year (integer)
#'   \item \strong{month:} Month of the year (integer)
#'   \item \strong{week:} Week number (integer)
#'   \item \strong{yday:} Day of the year (integer)
#'   \item \strong{mday:} Day of the month (integer)
#'   \item \strong{wday:} Day of the week - 1 is monday (integer)
#'   \item \strong{hour:} Hour (integer)
#'   \item \strong{minute:} Minute (integer)
#'   \item \strong{second:} Second (integer)
#'   \item \strong{timeOfDay:} The time of day (factor)
#'   \item \strong{withinWorkingHours:} Is it within working hours (logical)
#'   \item \strong{firstWeekMonth:} Is it the first week of the month (logical)
#'   \item \strong{lastWeekMonth:} Is it the last week of the month (logical)
#'   \item \strong{firstMondayMonth:} Is it the first monday of the month (logical)
#'   \item \strong{lastFridayMonth:} Is it the last friday of the month (logical)
#'   \item \strong{isHoliday:} Is it a national holiday (logical)
#'   \item \strong{isVacation:} Is it a national vacation (logical)
#' }
#'
#' @import data.table magrittr
#'
#' @export
datetime_to_cols <- function(datetime, country = "denmark", suffixes = NULL, return = "data.table", verbose = FALSE){

  winter_holiday_week = 7L
  daysInMonth <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

  daysInMonth <- function(month, year){
    n_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month]
    n_days[month == 2 & ((year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)))] <- 29L
    # if(month == 2 & ((year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)))) n_days <- 29L
    return(n_days)
  }

  country <- match.arg(country, c("denmark"))
  return <- match.arg(return, c("data.table", "data.frame"))

  if(verbose) cat("Commencing datetime_to_cols function", fill = TRUE)

  datetime_objects <- c("POSIXct", "POSIXt")
  if(!(class(datetime)[1] %in% datetime_objects)) stop(paste0("datetime has to be a datetime object - curretly one of: ", paste(datetime_objects, collapse = ", ")))

  if(verbose) cat("Initial datetime wrangle", fill = TRUE)

  datetimeDT <- data.table::data.table(datetime = datetime)

  nNA <- nrow(datetimeDT[is.na(datetime)])
  if(nNA > 0) warning(paste("Found", nNA, "NA values in datetime. Consider removing NA before running the function"))

  datetimeDT[!is.na(datetime), date := data.table::as.IDate(datetime)]
  datetimeDT[!is.na(datetime), time := data.table::as.ITime(datetime)]
  datetimeDT[!is.na(datetime), year := data.table::year(datetime)]
  datetimeDT[!is.na(datetime), quarter := data.table::quarter(datetime)]
  datetimeDT[!is.na(datetime), month := data.table::month(datetime)]
  datetimeDT[!is.na(datetime), week := data.table::week(datetime)]
  datetimeDT[!is.na(datetime), yday := data.table::yday(datetime)]
  datetimeDT[!is.na(datetime), mday := data.table::mday(datetime)]
  datetimeDT[!is.na(datetime), wday := (unclass(data.table::as.IDate(datetime)) + 4L)%%7L]
  datetimeDT[!is.na(datetime), hour := data.table::hour(datetime)]
  datetimeDT[!is.na(datetime), minute := data.table::minute(datetime)]
  datetimeDT[!is.na(datetime), second := data.table::second(datetime)]


  if(verbose) cat("Time of day", fill = TRUE)
  datetimeDT[!is.na(datetime), timeOfDay := time_of_day(hour, country = country)]
  datetimeDT[, timeOfDay :=
               factor(timeOfDay,
                      levels = c("nat", "morgen", "formiddag", "middag", "eftermiddag", "aften"))]


  if(verbose) cat("Is weekend", fill = TRUE)
  datetimeDT[!is.na(datetime), isWeekend := FALSE]
  datetimeDT[wday %in% c(6L, 7L), isWeekend := TRUE]


  if(verbose) cat("Within Working Hours", fill = TRUE)
  datetimeDT[!is.na(datetime), withinWorkingHours := FALSE]
  datetimeDT[hour >= 8L & hour < 17L & isWeekend == FALSE, withinWorkingHours := TRUE]


  if(verbose) cat("First / last of a month", fill = TRUE)
  datetimeDT[!is.na(datetime), firstWeekMonth := FALSE]
  datetimeDT[!is.na(datetime) & mday <= 7, firstWeekMonth := TRUE, by = "month"]

  datetimeDT[!is.na(datetime), lastWeekMonth := FALSE]
  datetimeDT[!is.na(datetime) & mday >= (daysInMonth(month, year)-7), lastWeekMonth := TRUE, by = "month"]

  datetimeDT[!is.na(datetime), firstMondayMonth := FALSE]
  datetimeDT[!is.na(datetime) & wday == 1 & firstWeekMonth == TRUE, firstMondayMonth := TRUE, by = "month"]

  datetimeDT[!is.na(datetime), lastFridayMonth := FALSE]
  datetimeDT[!is.na(datetime) & wday == 5 & lastWeekMonth == TRUE, lastFridayMonth := TRUE, by = "month"]


  if(verbose) cat("Calculating easter", fill = TRUE)
  unique_years <- unique(datetimeDT[!is.na(year)]$year)
  easter <- lapply(unique_years, get_easter)
  easter <- data.table::rbindlist(easter)
  easter[, easter_date := paste0(year, "/", month, "/", day) %>% as.Date()]

  datetimeDT <- merge(datetimeDT, easter[, .(year, easter_date)], by = "year", all.x = TRUE, sort = FALSE)


  if(verbose) cat("Is holiday", fill = TRUE)
  datetimeDT[!is.na(datetime), isHoliday := FALSE]
  datetimeDT[!is.na(datetime) & (
    (mday == 1L & month == 1L) |
      (date == easter_date-3) |
      (date == easter_date-2) |
      (date == easter_date) |
      (date == easter_date+1) |
      (date == easter_date+(3*7)+5) |
      (date == easter_date+(5*7)+4) |
      (date == easter_date+(7*7)) |
      (date == easter_date+(7*7)+1) |
      (mday == 25L & month == 12L) |
      (mday == 26L & month == 12L)

  ), isHoliday := TRUE]


  if(verbose) cat("Is vacation", fill = TRUE)
  datetimeDT[, easter_week := data.table::week(easter_date)]
  datetimeDT[!is.na(datetime), isVacation := FALSE]
  datetimeDT[!is.na(datetime) & (
    isHoliday == TRUE |
    week %in% winter_holiday_week | #Vinterferie
    week == easter_week | #Påskeferie
    week %in% c(29L, 30L, 31L) | #Sommerferie
    week == 42L #Vinterferie
    #Mangler juleferie
  ), isVacation := TRUE]


  datetimeDT[, easter_date := NULL]
  datetimeDT[, easter_week := NULL]

  data.table::setcolorder(datetimeDT, c(names(datetimeDT)[2:4], names(datetimeDT)[1], names(datetimeDT)[5:ncol(datetimeDT)]))

  if(!is.null(suffixes)){
    if(verbose) cat("Adds suffixes", fill = TRUE)
    data.table::setnames(datetimeDT, names(datetimeDT), paste0(suffixes, names(datetimeDT)))
  }

  if(return == "data.frame"){
    if(verbose) cat("Converts to DF", fill = TRUE)
    datetimeDT[!is.na(datetime), date := as.Date(date)]
    datetimeDT <- as.data.frame(datetimeDT)
  }

  return(datetimeDT)

}
