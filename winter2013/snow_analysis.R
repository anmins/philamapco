#Script for various snow data analyses

library(data.table)
library(ggplot2)
library(plyr)
library(reshape2)
library(lubridate)
library(xtable)

#Read in the weather data
weather <- data.table(read.csv("~/philamapco/winter2013/snow_data.csv"))

#calcualte snowfall and snow depth in inches
weather[, snowfall_in := SNOW / 25.4]
weather[, snow_depth_in := SNWD / 25.4]
weather[, rainfall := PRCP / 25.4]

#format dates
weather[, date := as.Date(ymd(DATE))] #convert the DATE column to a date format
weather[, month := substr(DATE, 5, 6)] #extract the month from the date
weather[, month_num := as.numeric(substr(DATE, 5, 6))] #extract the month as a number from the date
weather[, year := as.numeric(substr(date, 1, 4))] #extract year from date
weather[, md := substr(as.character(date), 6, 10)]
weather[, quarter := quarter(date)]

daily.means <- weather[, list(mean = mean(PRCP), sd = sd(PRCP)/sqrt(nrow(.SD))), by = "md,month,quarter"]
ggplot(daily.means, aes(x = md, y = mean, ymin = mean-sd, ymax = mean+sd, color = month)) +
  geom_point() +
  geom_errorbar() +
  theme_bw() +
  facet_wrap(~ quarter, scales="free_x", nrow = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#july 4th rainfall
j4th.rain <- weather[substr(date, 6, 10) == "05-26", list(year, rainfall)]
ggplot(j4th.rain, aes(x = year, y = rainfall)) +
  geom_bar(stat = "identity") +
  theme_bw()

arrange(weather[, mean(PRCP), by = md], V1, decreasing = TRUE) 

#create variables for the year of the winter, one long, one short
weather[, winter_year := ifelse(month_num > 7, paste(year, as.numeric(year)+1, sep="-"), paste(as.numeric(year)-1, year, sep="-"))]
weather[, winter_year2 := as.numeric(ifelse(month_num > 7, paste(year), paste(as.numeric(year)-1)))]


#total snow fall by year, one line plot
annual_snow <- weather[winter_year2 != 1979, list(total_snow = sum(snowfall_in)), by = "winter_year"]
annual_snow[, location := "philadelphia"]
annual_snow_final <- annual_snow[, list(winter = winter_year, snowfall = round(total_snow), snow_inches = round(total_snow))]

#save the data for a nicer plot
write.csv(annual_snow_final, "~/Documents/D3_Tutorial/annual_snow.csv", row.names=FALSE)

#line plot
ggplot(annual_snow, aes(x = winter_year, y = total_snow, group = location)) +
  geom_line(size = 1.5, color = col.br[1]) + 
  geom_point(size = 4, shape = 19, color = col.br[1]) +
  geom_point(size = 2, shape = 19, color = "white", alpha = 0.5) +
  theme_bw() +
  labs(y = "Total Snow (inches)",
       x = "Winter Year (eg 1995 is 1995-1996",
       title = "Total Snowfall by Year 1980 - 2014")


#calculate the years with the most and least snow
snowiest <- arrange(annual_snow, total_snow)$winter_year[c(30:34)]
least.snowy <- arrange(annual_snow, total_snow)$winter_year[1:5]
#table of snowiest and least snowy
snowfall.table <- arrange(annual_snow[winter_year %in% c(snowiest, least.snowy)], total_snow, decreasing = TRUE)

#output snow as a table
print(xtable(snowfall.table), type = "html")


#cumulative snow plot
snow_winter <- snow[month_num %in% c(11:12, 1:3), 
                    list(snowfall_in, snow_depth_in, year, winter_year2, winter_year,date, 
                         max_temp = (9/5*TMAX/10)+32, min_temp = (9/5 * TMIN/10)+32)]

snow_winter[, fake_date := ifelse(year == winter_year2,
                                  as.Date(paste(2000,substr(date, 5,10),sep="")),
                                  as.Date(paste(2001,substr(date, 5,10),sep="")))]
snow_winter$fake_date <- as.Date(snow_winter$fake_date, origin = "1970-01-01")

snow_winter <- arrange(snow_winter, fake_date)
snow_winter[, cum_snow := cumsum(snowfall_in), by = winter_year2]
snow_winter[, snow_day := ifelse(snowfall_in > 0, 1, 0)] 
  
ggplot(snow_winter[winter_year2 != 1995],
       aes(x = date, y = snow_depth_in)) +
  geom_bar(stat="identity", color = col.br[1], fill = col.br[1]) +
  theme_bw() +
  facet_wrap( ~ winter_year2, scales = "free_x") +
  #geom_point(aes(y = snowfall_in, alpha = factor(snow_day)), color = "black", shape = 18, size = 3) +
  scale_alpha_manual(values = c(0,1))
  

tmp <- snow_winter[winter_year2 != 2013 & !is.na(fake_date), list(mean_cover = mean(snow_cover, na.rm = TRUE)), by = "fake_date"]
tmp$year <- "average"
tmp13 <- snow_winter[winter_year2 == 2013 & !is.na(fake_date), 
                     list(mean_cover = max(snow_cover)), by = "fake_date"]
tmp13$year <- "wy2013"

newthing <- rbindlist(list(tmp, tmp13))

ggplot(newthing, aes(x = fake_date, y = mean_cover, color = year)) + geom_point()


View(tmp13)

#significant snow cover days: 
snow_winter[, snow_cover := ifelse(snow_depth_in > 0, 1, 0)]

arrange(snow_winter[, sum(snow_cover), by = winter_year2], V1)

mean.snow.on.snow <- snow_winter[snowfall_in > 0, list(avg_snow = mean(snowfall_in),
                                                   total_snow = sum(snowfall_in),
                                                   mean_snow = mean(snow_cover)),
                                 by = "winter_year,winter_year2"]

ggplot(mean.snow.on.snow, aes(x = mean_snow, y = total_snow)) + 
  geom_text(label = mean.snow.on.snow$winter_year2)

#total snow vs. days w/ snow





#table: top 5 snowiest years
weather.class <- weather[, list(date = DATE, temp = TMAX, snow_in := )]

#for class....... winter year, inches of snow, mean temperature (actual winter: dec21 to mar21)














