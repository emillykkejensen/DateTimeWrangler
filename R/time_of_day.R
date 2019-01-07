time_of_day <- function(hour, language = "danish"){
  if(length(hour) == 1){
    if(language == "danish"){
      if(hour >= 24L & hour < 6L) return("nat")
      if(hour >= 6L & hour < 9L) return("morgen")
      if(hour >= 9L & hour < 12L) return("formiddag")
      if(hour >= 12L & hour < 15L) return("middag")
      if(hour >= 15L & hour < 18L) return("eftermiddag")
      if(hour >= 18L & hour < 24L) return("aften")
    }
  } else {
    return(sapply(hour, time_of_day, language))
  }
}

is.holiday <- function(date, country = "denmark"){

  if(length(date) == 1){

    mday <- mday(date)
    month <- month(date)
    year <- year(date)

    easter <- get_easter(year)
    easter_date <- as.Date(paste0(year, "/", easter$month, "/", easter$day))

    if(any(
      mday == 1L & month == 1L,
      date == easter_date-3,
      date == easter_date-2,
      date == easter_date,
      date == easter_date+1,
      date == easter_date+(3*7)+5,
      date == easter_date+(5*7)+4,
      date == easter_date+(7*7),
      date == easter_date+(7*7)+1,
      mday == 25L & month == 12L,
      mday == 26L & month == 12L
    )) return(TRUE) else return(FALSE)

  } else {

    return(sapply(date, is.holiday, country))

  }

}

get_easter <- function(year){
  # From: https://github.com/cran/fCalendar/blob/master/R/Easter.R
  a = year%%19
  b = year%/%100
  c = year%%100
  d = b%/%4
  e = b%%4
  f = (b+8)%/%25
  g = (b-f+1)%/%3
  h = (19*a+b-d-g+15)%%30
  i = c%/%4
  k = c%%4
  l = (32+2*e+2*i-h-k)%%7
  m = (a+11*h+22*l)%/%451
  easter.month = as.integer((h+l-7*m+114)%/%31)
  p = (h+l-7*m+114)%%31
  easter.day =as.integer(p+1)
  return(list(month = easter.month, day = easter.day))
}
