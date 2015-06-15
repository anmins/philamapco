#Make some decorative maps using R!

#you'll need these libraries
#if they're not installed, use install.packages("package.name")
library(data.table)
library(maptools) #for reading in shapefiles
library(ggplot2)  #for plotting
library(jpeg) #for adding a background image
library(grid) # for adding a background image
library(extrafont) #use custom fonts

#load blank theme for ggplot
source("theme_blank.R")

#change this file path to wherever your fonts are located
#your downloaded fonts may not be in the same place as your
#system fonts (the ones that came w/ your computer)
font_import("~/Library/Fonts/", prompt = FALSE)

#shapefile of the street lines
#you'll need to download this file yourself from the link in the readme
phila.streets <- readShapeLines("data/PhiladelphiaStreetCenterlines201302.shp")

#use fortify to convert the shapefile into a data frame form before plotting
phila.streets.plot <- data.table(fortify(phila.streets, region = "SEG_ID"))

#make points for a bouding box for the map
#poins need to be in PA south state plane coordinates
#convert from lat/long here: http://www.earthpoint.us/StatePlane.aspx
lat.n <- 238510.033
lon.e <- 2702477.912
lat.s <- 230867.075
lon.w <- 2683682.56

#read in the points you want to put on the map
map.points <- fread("data/fake_coordinates.csv")

#read in the background image
img <- readJPEG("data/Chalkboard-background.jpg")

#do the subsetting + plot call in one step
map.plot <- ggplot(phila.streets.plot[lat < lat.n & lat > lat.s & long < lon.e & long > lon.w],
                   aes(x = long, y = lat, group = id)) +
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf) +
  geom_text(data = data.table(x = lon.w, y = lat.n + 1200, group = "a"),
            aes(x = x, y = y, group = group, label = "Blake and Chloe take on Philly"),
            color = "grey90", family = "KGMakesYouStronger", size = 16, alpha = 0.9, hjust = 0) +
  scale_y_continuous(limits = c(lat.s, (lat.n+2000))) +
  geom_line(color = "grey90", alpha = 0.75, size = 1.1) +
  coord_equal() + #very imprtant so that your map doesn't look "streched"
  geom_text(data = map.points, aes(x = long, y = lat, group = NULL, label = rep(sprintf('\u2665'), nrow(map.points))),
            size = 18, color = "#FADBDB") +
  new_theme_empty

#view the map
print(map.plot)

#save the map
ggsave(plot = map.plot, filename = "~/Documents/PhilaMapCo/DecoMapsTutorial/map.jpg")






