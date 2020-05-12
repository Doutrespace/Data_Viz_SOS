### Air Quality
install.packages("openair")
install.packages("xlsx")
library(openair)
library(ggplot2)
library(circlize)
library(mapview)
library(xlsx)
library(leaflet)

data("mydata")

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
