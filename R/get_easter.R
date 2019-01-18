get_easter <- function(year){
  # From: https://github.com/cran/fCalendar/blob/master/R/Easter.R
  # https://da.wikipedia.org/wiki/P%C3%A5ske
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
  return(list(year = year, month = easter.month, day = easter.day))
}
