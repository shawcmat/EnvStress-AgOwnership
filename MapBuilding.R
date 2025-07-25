library(gridExtra)
library(ggplot2)
library(sf)
library(ggpubr)
library(raster)
library(sp)
library(viridis)
library(rnaturalearth)
library(patchwork)
library(ggspatial)


wd <- '<Set your data directory here>'

setwd(wd)

load('./Datasets/MasterFile/MainDatav6/MainDatav6.Rdata')

class(LandData)
names(LandData)

countrylist <- unique(LandData$CNTRYNAMEE)

#Template Map: just geometry

base_plotlist <- list()

for (i in countrylist){
  subset <- subset(LandData, CNTRYNAMEE == i)
  yearlist <- sort(unique(subset$SVYYEAR))
  yearpaste <- paste(as.character(yearlist), collapse = ', ')
  subset <- subset(subset, SVYYEAR == max(SVYYEAR))
  subset <- st_as_sf(subset)
  
  
  output <- ggplot(data=subset) + geom_sf(aes()) + ggtitle(label = i, subtitle = yearpaste ) + theme(axis.text.x = element_blank(),
                                                                      axis.text.y = element_blank(),
                                                                      axis.ticks = element_blank(),
                                                                      panel.grid.major = element_blank(),
                                                                      panel.grid.minor = element_blank(),
                                                                      panel.background = element_blank(),
                                                                      plot.title = element_text(size = 12, hjust = 0.5),
                                                                      plot.subtitle = element_text(hjust = 0.5)
  )
  
  base_plotlist[[i]] <- output
}


figure_1 <- ggarrange(plotlist = base_plotlist)

ggsave('base_maps.png', plot = figure_1, path = "./Analysis (Models)/Maps", 
       width = 8, height = 12, units = "in", dpi = "print")

# Maps of Land Ownership Variable

landownership_plotlist <- list()

for (i in countrylist){
  subset <- subset(LandData, CNTRYNAMEE == i)
  subset <- subset(subset, SVYYEAR == max(SVYYEAR))
  subset <- st_as_sf(subset)
  
  output <- ggplot(data=subset) + geom_sf(aes(fill = HCAGONHLND)) + ggtitle(i) + theme(axis.text.x = element_blank(),
                                                                                          axis.text.y = element_blank(),
                                                                                          axis.ticks = element_blank(),
                                                                                          panel.grid.major = element_blank(),
                                                                                       plot.title = element_text(size = 12, hjust = 0.5),
                                                                                          plot.subtitle = element_text(hjust = 0.5))
  output <- output + labs(fill = 'Agricultural Land Ownership (Percent)')
  
  output <- output + scale_fill_viridis_c(option = "viridis", direction = -1)
  
  landownership_plotlist[[i]] <- output
}

figure_2 <- ggarrange(plotlist = landownership_plotlist,
                      legend = 'top', 
                      common.legend=TRUE)

ggsave('landownership_maps.png', plot = figure_2, path = "./Analysis (Models)/Maps", 
       width = 8, height = 12, units = "in", dpi = "print")

#AL
ggsave("./landownership_maps_AL.png", plot = figure_2, width = 8, height = 12, units = "in" )


# Cropland Percent

cropland_percent_plotlist <- list()

for (i in countrylist){
  subset <- subset(LandData, CNTRYNAMEE == i)
  subset <- subset(subset, SVYYEAR == max(SVYYEAR))
  subset <- st_as_sf(subset)
  
  output <- ggplot(data=subset) + geom_sf(aes(fill = percentcrop)) + ggtitle(i) + theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank(),
                                                                                       panel.grid.major = element_blank(),
                                                                                       panel.grid.minor = element_blank(),
                                                                                       panel.background = element_blank(),
                                                                                       plot.title = element_text(size = 12, hjust = 0.5),
                                                                                       plot.subtitle = element_text(hjust = 0.5))
  output <- output + labs(fill = 'Percent Cropland')
  
  output <- output + scale_fill_viridis_c(option = "magma")
  
  cropland_percent_plotlist[[i]] <- output
}

figure_2 <- ggarrange(plotlist = cropland_percent_plotlist,
                      legend = 'top', 
                      common.legend=TRUE)

ggsave('cropland_percent_maps.png', plot = figure_2, path = "./Datasets/MasterFile/Figures", 
       width = 8, height = 12, units = "in", dpi = "print")


# Maps of Growing Season SPEI03

meanspei03_plotlist <- list()

for (i in countrylist){
  subset <- subset(LandData, CNTRYNAMEE == i)
  subset <- subset(subset, SVYYEAR == max(SVYYEAR))
  subset <- st_as_sf(subset)
  
  output <- ggplot(data=subset) + geom_sf(aes(fill = SPEI03_Mean, )) + ggtitle(i) + theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank(),
                                                                                       panel.grid.major = element_blank(),
                                                                                       panel.grid.minor = element_blank(),
                                                                                       panel.background = element_blank(),
                                                                                       plot.title = element_text(size = 12, hjust = 0.5),
                                                                                       plot.subtitle = element_text(hjust = 0.5))
  output <- output + labs(fill = 'Five-year mean growing season SPEI03 previous to most recent survey.')
  
  output <- output + scale_fill_viridis_c(limits = c(-1, 1), option = "magma", direction = -1)
  
  meanspei03_plotlist[[i]] <- output
}

figure_3 <- ggarrange(plotlist = meanspei03_plotlist,
                      legend = 'top', 
                      common.legend=TRUE)

ggsave('meanspei03_maps.png', plot = figure_2, path = "./Datasets/MasterFile/Figures", 
       width = 8, height = 12, units = "in", dpi = "print")

#AL
ggsave("./meanspei03_maps_AL.png", plot = figure_3, width = 8, height = 12, units = "in" )


# Maps of Growing Season SPEI12

meanspei12_plotlist <- list()

for (i in countrylist){
  subset <- subset(LandData, CNTRYNAMEE == i)
  subset <- subset(subset, SVYYEAR == max(SVYYEAR))
  subset <- st_as_sf(subset)
  
  output <- ggplot(data=subset) + geom_sf(aes(fill = SPEI12_Mean, )) + ggtitle(i) + theme(axis.text.x = element_blank(),
                                                                                          axis.text.y = element_blank(),
                                                                                          axis.ticks = element_blank(),
                                                                                          panel.grid.major = element_blank(),
                                                                                          panel.grid.minor = element_blank(),
                                                                                          panel.background = element_blank(),
                                                                                          plot.title = element_text(size = 12, hjust = 0.5),
                                                                                          plot.subtitle = element_text(hjust = 0.5))
  output <- output + labs(fill = 'Mean Growing Season SPEI12')
  
  output <- output + scale_fill_viridis_c(option = "magma")
  
  meanspei12_plotlist[[i]] <- output
}

figure_2 <- ggarrange(plotlist = meanspei12_plotlist,
                      legend = 'top', 
                      common.legend=TRUE)

ggsave('meanspei12_maps.png', plot = figure_2, path = "./Datasets/MasterFile/Figures", 
       width = 8, height = 12, units = "in", dpi = "print")






countrylist <- unique(LandData$CNTRYNAMEE)
sum <- 0

for (i in countrylist){
  subset <- subset(LandData, CNTRYNAMEE == i)
  yearlist <- sort(unique(subset$SVYYEAR))
  yearpaste <- paste(as.character(yearlist), collapse = ', ')
  subset <- subset(subset, SVYYEAR == max(SVYYEAR))
  x <- nrow(subset)
  sum <- sum + x
}
  



#Creating an example map of phenology. 


Afghanistan <- subset(LandData, CNTRYNAMEE == "Afghanistan")

seasons <- raster("./Datasets/Phenology/phenos1_v03.tif")
seasone <- raster("./Datasets/Phenology/phenoe1_v03.tif")

afgstart <- crop(seasons, Afghanistan)
afgend <- crop(seasone, Afghanistan)

afgstart@data@values <- clamp(afgstart@data@values, upper = 108, useValues = FALSE)
afgend@data@values <- clamp(afgend@data@values, upper = 108, useValues = FALSE)

afgstart@data@values <- ifelse(afgstart@data@values <= 36, afgstart@data@values, afgstart@data@values)
afgstart@data@values <- ifelse(afgstart@data@values > 36 & afgstart@data@values <= 72, afgstart@data@values - 36, afgstart@data@values)
afgstart@data@values <- ifelse(afgstart@data@values > 72, afgstart@data@values - 72, afgstart@data@values)

afgend@data@values <- ifelse(afgend@data@values <= 36, afgend@data@values, afgend@data@values)
afgend@data@values <- ifelse(afgend@data@values > 36 & afgend@data@values <= 72, afgend@data@values - 36, afgend@data@values)
afgend@data@values <- ifelse(afgend@data@values > 72, afgend@data@values - 72, afgend@data@values)

afgstart@data@values <- round((afgstart@data@values * 10), digits = 0)
afgend@data@values <- round((afgend@data@values * 10), digits = 0)

afgstart@data@values <- as.numeric(strftime(as.Date(afgstart@data@values, origin = "2015-12-31", tz = "UTC"), "%m"))
afgend@data@values <- as.numeric(strftime(as.Date(afgend@data@values, origin = "2015-12-31", tz = "UTC"), "%m"))


afgstart <- mask(afgstart, Afghanistan)
afgend<- mask(afgend, Afghanistan)

plot(afgstart, axes = F, box= F, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), col = rainbow(11))
plot(Afghanistan$geometry, add = TRUE)

plot(Afghanistan["SOSMonth"], main = "Afghanistan start of season", 
     pal = rainbow(12), nbreaks = 13,
     breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12), key.pos = 4, key.length = .8, key.width = .13)

legend(1, 1.5, legend=c("Jan","Feb", "Mar", "Apr", "May", "Jun"), col=c("black","green","red"), lwd=2,horiz=TRUE)


plot(afgend, axes = F, box = F, breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), col = rainbow(11))
plot(Afghanistan$geometry, add = TRUE)

plot(Afghanistan["EOSMonth"], main = "Afghanistan end of season", 
     pal = rainbow(12), nbreaks = 13,
     breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12), key.pos = 4)


# Create an example map of cropland percent

Afghanistan <- subset(LandData, CNTRYNAMEE == "Afghanistan")

percentcrop <- raster("./Datasets/CroplandPercent/asap_mask_crop_v03.tiff")

percentcrop <- crop(percentcrop, Afghanistan)

percentcrop@data@values <- percentcrop@data@values/2

percentcrop <- mask(percentcrop, Afghanistan)

ramp <- viridis(100, direction = -1, option = "A")

plot(percentcrop, axes = F, box= F, col = ramp)
plot(Afghanistan$geometry, add = TRUE)

percentcrop[is.na(percentcrop)] <- 0

percropmean <- extract(percentcrop, Afghanistan, fun =mean, na.rm=TRUE, df=TRUE)

Afghanistan$percentcrop2 <- percropmean$asap_mask_crop_v03

ramp <- viridis(nrow(Afghanistan), direction = -1, option = "A")

plot(Afghanistan["percentcrop"], nbreaks = 10, breaks = c(0,10,20,30,40,50,60,70,80,90,100), main = "Afghanistan percent cropland", 
     col = ramp, key.pos = 2)

plot(Afghanistan["percentcrop2"], main = "Afghanistan percent cropland (using 0s)", 
     col = ramp, key.pos = 2)



output <- ggplot(data=Afghanistan) + geom_sf(aes(fill = percentcrop, )) + theme(axis.text.x = element_blank(),
                                                                                        axis.text.y = element_blank(),
                                                                                        axis.ticks = element_blank(),
                                                                                        panel.grid.major = element_blank(),
                                                                                        panel.grid.minor = element_blank(),
                                                                                        panel.background = element_blank(),
                                                                                        plot.title = element_text(size = 12, hjust = 0.5),
                                                                                        plot.subtitle = element_text(hjust = 0.5))

output <- output + scale_fill_viridis_c(limits = c(0,100), direction = -1, option = "magma")
output

output2 <- ggplot(data=Afghanistan) + geom_sf(aes(fill = percentcrop2, )) + theme(axis.text.x = element_blank(),
                                                                                       axis.text.y = element_blank(),
                                                                                       axis.ticks = element_blank(),
                                                                                       panel.grid.major = element_blank(),
                                                                                       panel.grid.minor = element_blank(),
                                                                                       panel.background = element_blank(),
                                                                                       plot.title = element_text(size = 12, hjust = 0.5),
                                                                                       plot.subtitle = element_text(hjust = 0.5))
output2 <- output2 + scale_fill_viridis_c(limits = c(0,40), direction = -1, option = 'magma')
output2


