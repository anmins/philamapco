library(data.table)
library(ggplot2)

weather <- data.table(read.csv("~/philamapco/winter2013/snow_data.csv"))


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
snow[, month := substr(DATE, 5, 6)]

snow$month <- ordered(snow$month, levels = c(paste("0", 4:9, sep=""), "10", "11", "12", paste("0", 1:3, sep="")))


monthly_means <- snow[, list(mean_snow = mean(snow_in), 
                             mean_temp = mean(TMAX)), by = "winter_year2,month"]

monthly_means[, is_2013 := ifelse(winter_year2 == 2013, 1, 0)]

ggplot(monthly_means[!(month %in% paste("0", 4:9, sep=""))], 
       aes(x = month, y = mean_snow, group = winter_year2, color = factor(is_2013), size = factor(is_2013), alpha = factor(is_2013))) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_color_manual(values = c("grey70", "red")) +
  scale_size_manual(values = c(1,2)) +
  scale_alpha_manual(values = c(0.3, 1))

#total snow fall by year, one line plot
annual_snow <- snow[winter_year2 != 1979, list(total_snow = sum(snow_in)), by = "winter_year2,winter_year"]
write.csv(annual_snow, "~/philamapco/winter2013/annual_snow.csv")

ggplot(annual_snow, aes(x = winter_year2, y = total_snow)) +
  geom_line(size = 1.5, color = col.br[1]) + 
  geom_point(size = 4, shape = 19, color = col.br[1]) +
  geom_point(size = 2, shape = 19, color = "white", alpha = 0.5) +
  theme_bw() +
  labs(y = "Total Snow (inches)",
       x = "Winter Year (eg 1995 is 1995-1996",
       title = "Total Snowfall by Year 1980 - 2014")
  #geom_text(label = annual_snow$winter_year2)


snowiest <- arrange(annual_snow, total_snow)$winter_year2[30:34]
least.snowy <- arrange(annual_snow, total_snow)$winter_year2[1:5]


#cumulative snow plot
snow_winter <- snow[month_num %in% c(11:12, 1:3), list(snow_in, year, winter_year2, winter_year,date)]

snow_winter[, fake_date := ifelse(year == winter_year2,
                                  as.Date(paste(2000,substr(date, 5,10),sep="")),
                                  as.Date(paste(2001,substr(date, 5,10),sep="")))]
snow_winter$fake_date <- as.Date(snow_winter$fake_date, origin = "1970-01-01")

snow_winter <- arrange(snow_winter, fake_date)
snow_winter[, cum_snow := cumsum(snow_in), by = winter_year2]

ggplot(snow_winter[winter_year2 %in% c(least.snowy,snowiest)], 
       aes(x = fake_date, y = cum_snow, group = winter_year2)) +
  geom_area(fill = col_br[2], alpha = 0.5) +
  geom_line(size = 2, color = col_br[3]) +
  theme_bw() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) +
  facet_wrap( ~ winter_year)


mean.snow.on.snow <- snow_winter[snow_in > 0, list(avg_snow = mean(snow_in),
                                                   total_snow = sum(snow_in)), by = "winter_year,winter_year2"]

ggplot(mean.snow.on.snow, aes(x = avg_snow, y = total_snow)) + 
  geom_text(label = mean.snow.on.snow$winter_year2)

#snow pattern: top 5 snowiest years


#table: top 5 snowiest years



#patterns by decade



library(plyr)




var snowfall = [15.4724409, 25.5118110, 37.9133858, 21.6535433, 16.4960630]
var year = [1980, 1981, 1982, 1983, 1985]




write.csv(data.frame(snowfall = 41:50, year = seq(10, 100, by = 10)), "~/Documents/D3_Tutorial/little.csv",
          row.names = FALSE)











