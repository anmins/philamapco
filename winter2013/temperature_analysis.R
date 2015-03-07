#Temperature Analysis

library(data.table)
library(ggplot2)

weather <- data.table(read.csv("~/philamapco/winter2013/snow_data.csv"))

names(weather)

temp.f <- weather[, list(date = DATE, max_temp = TMAX/10, min_temp = TMIN/10)]

#define date related variables
temp.f[, year := substr(date, 1, 4)]
temp.f[, date := as.Date(paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, 8), sep="-"))]

temp.f[, month_num := as.numeric(substr(date, 5, 6))]
temp.f[, winter_year := ifelse(month_num > 7, paste(year, as.numeric(year)+1, sep="-"), paste(as.numeric(year)-1, year, sep="-"))]
temp.f[, winter_year2 := as.numeric(ifelse(month_num > 7, paste(year), paste(as.numeric(year)-1)))]

