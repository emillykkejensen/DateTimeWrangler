time_of_day <- function(hour, country = "denmark", as.factor = TRUE){
  if(length(hour) == 1){
    if(country == "denmark"){
      if(hour >= 0L & hour < 6L) out <- "nat"
      if(hour >= 6L & hour < 9L) out <- "morgen"
      if(hour >= 9L & hour < 12L) out <- "formiddag"
      if(hour >= 12L & hour < 15L) out <- "middag"
      if(hour >= 15L & hour < 18L) out <- "eftermiddag"
      if(hour >= 18L & hour <= 23L) out <- "aften"
      return(out)
    }
  } else {
    out <- sapply(hour, time_of_day, country, as.factor = FALSE)
    return(out)
  }

}
