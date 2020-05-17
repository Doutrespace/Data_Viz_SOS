### Air Quality
install.packages("openair")
install.packages("xlsx")
install.packages("geojsonio")
install.packages("spData")
install.packages("spDataLarge")
install.packages("tmap")
library(tmap)
library(openair)
library(ggplot2)
library(circlize)
library(mapview)
library(xlsx)
library(leaflet)
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

data("mydata")
#pollution Rose

pollutionRose <- function(mydata, pollutant = "nox", key.footer = pollutant,
                          key.position = "right", key = TRUE,
                          breaks = 6, paddle = FALSE, seg = 0.9, normalise = FALSE,
                          ...) {
  
  ## extra args setup
  extra <- list(...)
  
  ## check to see if two met data sets are being compared.
  ## if so, set pollutant to one of the names
  if ("ws2" %in% names(extra)) {
    pollutant <- extra$ws
    if (missing(breaks)) breaks <- NA
  }
  
  if (is.null(breaks)) breaks <- 6
  
  if (is.numeric(breaks) & length(breaks) == 1) {
    
    ## breaks from the minimum to 90th percentile, which generally gives sensible
    ## spacing for skewed data. Maximum is added later.
    breaks <- unique(pretty(c(
      min(mydata[[pollutant]], na.rm = TRUE),
      quantile(mydata[[pollutant]], probs = 0.9, na.rm = TRUE),
      breaks
    )))
  }
  
  windRose(
    mydata,
    pollutant = pollutant, paddle = paddle, seg = seg,
    key.position = key.position, key.footer = key.footer, key = key,
    breaks = breaks, normalise = normalise, ...
  )
}

plot(pollutionRose(mydata), col.data= "viridis")#color = pollution concentratzin, bars= direction/speed of wind

pollutionRose(mydata, ws= "ws", wd = "wd",
              pollutant = "nox",
              breaks = c(0,50,100,200,350,500),
              auto.text=  FALSE,
              paddle = FALSE,
              annotate= FALSE,
              grid.line = 5,
              key.header = "EAGLE ROSE",
              key = list(labels= c(">0-50",
                                  ">50-100",
                                  ">100-150",
                                  ">150-200",
                                  ">200-250",
                                  ">250-300",
                                  ">350-500")),
              key.footer = expression(paste(NO[x]~"(","ppb",")")),
              key.position= "bottom",
              par.settings=list(axis.line=list(col="lightgray")),
              col= c("#4f4f4f", "#0a7cb9","#f9be00", "#ff7f2f","#d7153a"))

polarPlot(mydata, pollutant = "nox", col.data = "black")

polarAnnulus(mydata, pollutant= "nox")# period= "season")
#####################################AirQ######################################################################
summaryPlot(selectByDate(mydata, year=c(2000,2001)), type = "density",
            avg.time = "hour", period= "months", col.data = "grey")

scatterPlot(mydata, x="nox", y= "no2", z= "wd", type = "season")

timeVariation(mydata, pollutant = "nox", local.tz = "Europe/London")

trendLevel(mydata, pollutant= "nox")#, rotate.axis = c(45,0), border= "black") 

smoothTrend(mydata, pollutant = "nox")

calendarPlot(mydata, pollutant = "no2", year = 2003, annotate = "ws", main = "nox")

windRose(mydata, ws="ws", wd= "wd", paddle = FALSE, type = "season")

pollutionRose(mydata, pollutant = "nox")  #color = pollution concentratzin, bars= direction/speed of wind

polarPlot(mydata, pollutant = "nox", col.data = "black")

polarAnnulus(mydata, pollutant= "nox")# period= "season")

data <- importKCL(site = "kc1", year = c(2004,2005), pollutant = "all", met = TRUE)

summaryPlot(data)

traj <- importTraj(site = "london", year = c(2005), local = NA)

trajPlot(traj, type = "season")

trajPlot(selectByDate(traj,year = 2005, month = 1), group = "day")

newDf <- merge(traj, selectByDate(data, year  = 2005))
trajPlot(selectByDate(newDf, year = 2005, month=1), pollutant = "nox")

############################################################################################################



meanNDVI_HARV_2011 <- read_csv("files/NEON-DS-Met-Time-Series/HARV/NDVI/meanNDVI_HARV_2011.csv")

### new zeeland



tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 
tm_shape(nz) + tm_fill(col = "Land_area")
legend_title = expression("Area (km"^2*")")
map_nza = tm_shape(nz) +
  tm_fill(col = "Land_area", title = legend_title) + tm_borders()

tm_shape(nz) + tm_polygons(col = "Median_income", palette = "BuGn")
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "GnBu")
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "PuBuGn")
tm_shape(nz) + tm_polygons(col = "Median_income", palette = "YlGnBu")




##########################log-scale############################################
graph <- ggplot(df, aes(x = Date, y = meanNDVI, colour = meanNDVI)) +
  geom_point() +
  theme_light()
graph

ggplot(meanNDVI_HARV_2011) +
 aes(x = Date, y = meanNDVI) +
 geom_line(size = 0.88, colour = "#35b779") +
 scale_y_continuous(trans = "atanh") +
 labs(title = "Mean NDVI in one year") +
 theme_light()

ggplot(meanNDVI_HARV_2011) +
 aes(x = Date, y = meanNDVI) +
 geom_line(size = 0.88, colour = "#35b779") +
 scale_y_continuous(trans = "log") +
 labs(title = "Mean NDVI in one year") +
 theme_light()









