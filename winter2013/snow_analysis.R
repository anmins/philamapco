library(data.table)
library(ggplot2)

weather <- data.table(read.csv("~/Downloads/301283.csv"))


weather[, snow_in := SNOW / 25.4]

#3/7 is missing for SNOW
snow <- weather[SNOW != -9999]
snow[, year := substr(DATE, 1, 4)]
snow[, date := as.Date(paste(substr(DATE, 1, 4), substr(DATE, 5, 6), substr(DATE, 7, 8), sep="-"))]

ggplot(snow[year %in% c("2013", "2014")], aes(x = date, y = snow_in, group = year, color = year)) +
  geom_point() +
  geom_line()

#total snowfall by year:

snow[, month_num := as.numeric(substr(DATE, 5, 6))]
snow[, winter_year := ifelse(month_num > 7, paste(year, as.numeric(year)+1, sep="-"), paste(as.numeric(year)-1, year, sep="-"))]
snow[, winter_year2 := as.numeric(ifelse(month_num > 7, paste(year), paste(as.numeric(year)-1)))]

snow[winter_year2 != "1979", list(total_snow_in = sum(snow_in)), by = "winter_year2"]

snow[, time_per := cut(winter_year2, breaks=c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014))]


monthly_means <- snow[, list(mean_snow = mean(snow_in), 
                             mean_temp = mean(TMAX)), by = winter_year2]

snow[, is_2013 := ifelse(winter_year2 == 2014, 1, 0)]

ggplot(snow[winter_year ], )