

#we're going to read in the file of points (made in QGIS) of litter violations
#then merge in the cateogories from Kelly
#then we'll make a static map overlayed with hexbins by violation type.

#we'll use a basemap from the google maps api and we'll use ggplot2 to make the hexbins
#so after exporting the shapefile to a csv in QGIS we can actually do all of this
#without using any shapefiles

#since ther eare 434K records, everything takes time

library(data.table) #this is just my preferred method of handling data, some prefer plyr functions, or just data.frames / etc.
library(ggmap) #load base maps
library(ggplot2) #for plotting the hexbins
library(RColorBrewer) #nice library for mapping and plotting colors
library(classInt) #determining the breaks for shading
library(zoo) #for time series analysis
library(plyr)

#read in the csv w/ violations. Immediately convert to data.table
#data.table package has a csv reader funciton called fread(), and it is very fast
#but it does not do as many checks on the data, so if you are not sure that you have a very
#well formatted csv don't use it (i.e. if you have a column that switches class at somepoint,
#that may throw an error)
litter.violations <- data.table(read.csv("violations_xy.csv"))
#change the name of the violation to "cv_no"
setnames(litter.violations, "Violation1", "cv_no")

#read in Kelly's categories
litter.categories <- data.table(read.csv("cv_codes.csv"))

#merge the two together and overwrite the violations dataset, 
#keeping only the violations that appear in the categories dataset
litter.violations <- merge(litter.violations, litter.categories, by = "cv_no", all = FALSE)

#first get the base map
#determine the center of the map using the data itself
basemap <- get_googlemap(center = c(long = median(litter.violations$X), lat = median(litter.violations$Y)), 
                         maptype="terrain", color="bw", scale = 2, zoom = 11)

#next initialize the graph with the map we chose
ggmap(basemap, width = 10, height = 10) + 
  #then add the hex layer, adjust alpha and binwidth
  geom_hex(aes(x=X, y=Y), data = litter.violations, alpha = 0.9, binwidth = c(0.008,0.008)) +
  #now I don't like the default colors, so I'll choose my own scale, using colorbrewer
  scale_fill_continuous(low = "white", high = brewer.pal(n = 9, name = "Blues")[9])

#we need to play with the breaks since the default is not informative enough
#we'll also make it a facetted map. but this will take longer to generate since there are actually 15 categories
ggmap(basemap, width = 10, height = 10) + 
  #then add the hex layer, adjust alpha and binwidth
  geom_hex(aes(x=X, y=Y), data = litter.violations, alpha = 0.9, binwidth = c(0.008,0.008)) +
  #now I don't like the default colors, so I'll choose my own scale, using colorbrewer
  scale_fill_continuous(low = "white", high = brewer.pal(n = 9, name = "Reds")[9], trans = "log") +
  facet_wrap(~Topic)

#there is a better way to create the fill color breaks manually. some other time...

#we have dates in this data set. What can we do with that? Trends?

#let's summarize and make a line chart over time by violation type

#first create a new variable that is the date with class Date instead of character
#litter.violations[, date := as.Date(Date_Added)]
litter.violations[, date := as.Date(gsub(pattern = "/", "-", as.character(Date_Added)))]

#also get the weekday. why? b/c what does date added mean? if we don't see any on weekends, they I wonder
#if date_added is more for record keeping.
litter.violations[, weekday := weekdays(date)]
litter.violations[, .N / nrow(litter.violations), by = weekday]

#ah ha! only 1% of violations have a date_added on the weeekend. So this date doesn't necessarily
#represent the day that the violation happened

#this means that we need to summarize above the day level, i.e. at the week / month or year level
#there are a lot of ways to summarize at the week level. Since we're spanning years, and there are 
#actually 52.14 weeks in year, using something like "week of the year" would be odd. So I'm going to make
#a variable that summarizes to the nearest Tuesday, and the make the data look as if everythign happened on
#tuesday only, then we can plot the time series

#first figure out what week each day was in
all.days <- seq(min(litter.violations$date), max(litter.violations$date), 1)
litter.violations[, week_no := ceiling(which(all.days == date)/7), by = date]
litter.violations[, week_date := all.days[week_no*7]]

#quickly verify that the scheme worked:
ggplot(litter.violations[, .N, by = "week_no,date"], aes(x = date, y = week_no, size = N)) + geom_point()
ggplot(litter.violations[, length(unique(date)), by = "week_no,week_date"], 
       aes(x = week_date, y = week_no, size = V1)) + geom_point()

#looks good!

#now make some time series plots
ggplot(litter.violations[, .N, by = "week_date,Topic"], aes(x = week_date, y = N)) +
  geom_line() +
  facet_wrap(~ Topic, scales = "free_y")
#this plot is really interesting. what does it mean?????

#let's make it look nicer and add some summary lines.
#the summary lines are kinda meaningless for inferential purposes,
#they're just to make the plot look nicer
#note that you don't want to compare from plot to plot by the Y axis since the axes vary

#first make a summary data.table, then plot that
count.by.topic <- litter.violations[, .N, by = "week_date,Topic"]
ggplot(count.by.topic, aes(x = week_date, y = N)) +
  geom_line(color = "grey50") +
  facet_wrap(~ Topic, scales = "free_y", nrow = 3) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = "Count of Violations by Topic Over Time") +
  geom_smooth(method = "loess", se = F, color = "#CC3366")

#starting in 2011, let's look at the decomposition of each time series for Litter, 
#Recycling, Vechicle and Dumpster (since they have the "nicest" looking data)

#arrange the data.table by date, then we can convert easily to a time series object
count.by.topic <- arrange(count.by.topic, week_date)

decomp.litter    <- stl(ts(count.by.topic[week_date > as.Date("2011-01-01") & Topic == " Litter"]$N)) #ugh there is a space in Litter
decomp.dumpster  <- stl(ts(count.by.topic[week_date > as.Date("2011-01-01") & Topic == "Dumpster"]$N))
decomp.recycling <- stl(ts(count.by.topic[week_date > as.Date("2011-01-01") & Topic == "Recycling"]$N))
decomp.vehicle   <- stl(ts(count.by.topic[week_date > as.Date("2011-01-01") & Topic == "Vehcile"]$N))

#aaannnnddddd they're all aperiodic. boring!







