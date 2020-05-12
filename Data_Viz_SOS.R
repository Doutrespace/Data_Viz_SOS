#library
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("sp","textreadr","SSDM","readr","xml2","circlize","mapview","ggplot2","patchwork","usdm","Rcpp","dismo","rgbif","sdm","raster","rlist","getSpatialData","sf","sp","esquisse","list","dplyr","lubridate","rgdal","data.table","devtools","svDialogs","gdalUtils","Rcpp")
ipak(packages)

library(patchwork)
# emperor dataset
emperors <- read.csv("D:/Data_Viz/files/emperors.csv")
emperors <- read_html("https://raw.githubusercontent.com/rfordatasience/tidytuesday/master/data/2019/2019-08-13/emperors.csv", "rb")

emperors %>%
  filter(index >= 1L & index <= 50L) %>%
  ggplot() +
  aes(x = birth_cty, y = killer, colour = cause) +
  geom_tile(size = 1L) +
  labs(x = "Birth - City", y = "Killer", title = "Birth - City & Killer cause relations") +
  scale_color_hue() +
  theme_minimal()

ggplot(emperors) +
  aes(x = birth_cty, y = killer, fill = verif_who, colour = cause) +
  geom_tile(size = 1L) +
  scale_fill_hue() +
  scale_color_hue() +
  labs(x = "Birth - City", y = "Killer", title = "Birth City & Killer cause relations & Data verification") +
  theme_minimal()

#improve pie chart
library(patchwork)

Data <- data.frame(
  percent1994=c(35,25,20,10,5,5),
  percent2014=c(41,21,17,8,4,9),
  music <- c("hard rock","samba","hiphop","reggae","country","classic")
)

pie <- ggplot(data, aes(x="", y= percent1994, fill=music))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  ggtitle("music preferences 1994")

pie2 <- ggplot(data, aes(x="", y= percent2014, fill=music))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  ggtitle("music preferences 2014")

pie + pie2

ggsave("data_viz_piechart_task.jpg", width = 15, height = 7)

# chorddia test


df = data.frame(from = rep(rownames(Data), times = ncol(Data)),
                to = rep(colnames(Data), each = nrow(Data)),
                value = as.vector(Data),
                stringsAsFactors = FALSE)
df


chordDiagram(df)


######################### Where the fun actualy started#################################

load(system.file("extdata", "doodle.RData", package = "circlize"))
circos.par("cell.padding" = c(0, 0, 0, 0))
circos.initialize(letters[1:16], xlim = c(0, 1))

circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  img = img_list[[CELL_META$sector.numeric.index]]
  circos.raster(img, CELL_META$xcenter, CELL_META$ycenter, 
                width = CELL_META$xrange, height = CELL_META$yrange, 
                facing = "bending.inside")
}, track.height = 0.25, bg.border = NA)

circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  img = img_list[[CELL_META$sector.numeric.index + 16]]
  circos.raster(img, CELL_META$xcenter, CELL_META$ycenter, 
                width = CELL_META$xrange, height = CELL_META$yrange, 
                facing = "bending.inside")
}, track.height = 0.25, bg.border = NA)
circos.clear()

