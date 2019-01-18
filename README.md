
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/emillykkejensen/DateTimeWrangler.svg?branch=master)](https://travis-ci.org/emillykkejensen/DateTimeWrangler)

DateTimeWrangler
================

This R Package converts a DateTime into diff. value such as day of the week, time of day, day of the year, whether the time and day is during working hours, if it is a holiday or national vacation day, etc.

Installation
------------

You can install DateTimeWrangler from github with:

``` r
# install.packages("devtools")
devtools::install_github("emillykkejensen/DateTimeWrangler")
```

Example
-------

To use DateTimeWrangler simply give it a list of datetime values.

``` r

library(DateTimeWrangler)
#> Loading required package: data.table
#> Loading required package: magrittr

datetimes <- c(as.POSIXct("2018-05-12 12:35:20 CET"),
               as.POSIXct("2018-05-14 10:11:04 CET"),
               as.POSIXct("2018-07-01 04:50:02 CET"),
               as.POSIXct("2018-12-25 18:02:51 CET"))

populated_datetime <- datetime_to_cols(datetimes)

knitr::kable(populated_datetime)
```

| datetime            | date       |      time|  year|  quarter|  month|  week|  yday|  mday|  wday|  hour|  minute|  second| timeOfDay | isWeekend | withinWorkingHours | firstWeekMonth | lastWeekMonth | firstMondayMonth | lastFridayMonth | isHoliday | isVacation |
|:--------------------|:-----------|---------:|-----:|--------:|------:|-----:|-----:|-----:|-----:|-----:|-------:|-------:|:----------|:----------|:-------------------|:---------------|:--------------|:-----------------|:----------------|:----------|:-----------|
| 2018-05-12 12:35:20 | 2018-05-12 |  10:35:20|  2018|        2|      5|    19|   132|    12|     6|    12|      35|      20| middag    | TRUE      | FALSE              | FALSE          | FALSE         | FALSE            | FALSE           | FALSE     | FALSE      |
| 2018-05-14 10:11:04 | 2018-05-14 |  08:11:04|  2018|        2|      5|    20|   134|    14|     1|    10|      11|       4| formiddag | FALSE     | TRUE               | FALSE          | FALSE         | FALSE            | FALSE           | FALSE     | FALSE      |
| 2018-07-01 04:50:02 | 2018-07-01 |  02:50:02|  2018|        3|      7|    27|   182|     1|     0|     4|      50|       2| nat       | FALSE     | FALSE              | TRUE           | FALSE         | FALSE            | FALSE           | FALSE     | FALSE      |
| 2018-12-25 18:02:51 | 2018-12-25 |  17:02:51|  2018|        4|     12|    52|   359|    25|     2|    18|       2|      51| aften     | FALSE     | FALSE              | FALSE          | TRUE          | FALSE            | FALSE           | TRUE      | TRUE       |
