###############################################################################
#Visualisation of wind turbines for the calculation of their  area consumption#
###############################################################################

Sys.setenv(LANG = "en")
pacman::p_load(rio, data.table, tidyverse, tidyr, purrr, magrittr, compare, 
               ggplot2, leaflet, sp, raster, rgdal, htmltools, htmlwidgets,
               tmaptools,maptools, raster, sf, tmap, spatstat)

#import data#
MaStR_Amprion_join_with_georeferences <- read.csv("MaStR_Amprion_join_with_georeferences.csv")
attach(MaStR_Amprion_join_with_georeferences)

#change lat and long to numeric
MaStR_Amprion_join_with_georeferences$l_wgs84 <- gsub(",",".", l_wgs84)  
MaStR_Amprion_join_with_georeferences$b_wgs84 <- gsub(",",".", b_wgs84)
MaStR_Amprion_join_with_georeferences$b_wgs84 <- as.numeric(MaStR_Amprion_join_with_georeferences$b_wgs84)
MaStR_Amprion_join_with_georeferences$l_wgs84 <- as.numeric(MaStR_Amprion_join_with_georeferences$l_wgs84)

#create map
map <- leaflet(data = MaStR_Amprion_join_with_georeferences) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = MaStR_Amprion_join_with_georeferences$l_wgs84,
                   lat = MaStR_Amprion_join_with_georeferences$b_wgs84,
                   color = "#F40707",
                   weight = 2,
                   radius = 1,
                   label = vg_name,
                   popup = ~paste("<h3> Daten der Windkraftanlage</h3>",
                                  "<b>Landkreis (LK):</b>",          lk_name, "<br>",
                                  "<b>Verbandsgemeinde:</b>", vg_name, "<br>",
                                  "<b>EEG-Nr.:</b>", eeg_nr,"<br>",
                                  "<b>Leistung [kW]:</b>", leistung_s, "<br>",
                                  "<b>Nabenhöhe [m]:</b>", nabe, "<br>",
                                  "<b>Rotordurchmesser [m]:</b>", rotor, "<br>",
                                  "<b>Stromertrag 2019 [MWh]:</b>", menge_mwh, "<br>",
                                  "<b>Volllaststunden im LK 2019 [h]:</b>", flh, "<br>" ),
                   )  

map
#save as html file
saveWidget(map, file="area_consumtpion_of_wind_turbines.html")

#check crs
class(MaStR_Amprion_join_with_georeferences)
coordinates(MaStR_Amprion_join_with_georeferences) <- ~ l_wgs84 + b_wgs84
crs(MaStR_Amprion_join_with_georeferences)

#create crs
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs")
crs(MaStR_Amprion_join_with_georeferences) <- WGS84
crs(MaStR_Amprion_join_with_georeferences)

###################
#create shape file#
###################
class(MaStR_Amprion_join_with_georeferences)
writeOGR(MaStR_Amprion_join_with_georeferences, "C:/Users/Dell/Desktop/MasterThesis/Area_consumption_of_wind_turbines", "distance_calc", driver = "ESRI Shapefile")


##############################
#PPA - Point Pattern Analysis#
##############################


######################################
#load data in the ppp and owin format#
######################################
library(sf)
library(maptools)
library(raster)
library(spatstat)

##################################
#load points pattern in sf format#
##################################
wts_sf <- st_read("distance_calc.shp")
crs(wts_sf)

#reproject to flat projection
wts_flat <- st_transform(wts_sf, crs = 6345)
crs(wts_flat)

#create ppp formate of WT's
wts_ppp  <- as.ppp(wts_flat)
plot(wts_ppp)


########################
#load boundaries of RLP#
########################
rlp_sf  <- st_read("Borders_RLP_shape/Landesgrenze_RLP.shp")
crs(rlp_sf)
plot(rlp_sf)

#transform to flat
rlp_flat <- st_transform(rlp_sf, crs = 6345)
plot(rlp_flat)


#create owin format
rlp_owin  <- as.owin(rlp_flat)


#create window of points with borders of rlp and plot
Window(wts_ppp) <- rlp_owin
plot(wts_ppp, cols=rgb(0,0,0,.2), pch=20)













