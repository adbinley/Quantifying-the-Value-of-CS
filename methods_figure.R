library(tidyverse)
library(sf)
library(ggmap)
library(leaflet)

library(googleway)

# apiKey <- 'your_api_key'
# mapKey <- 'your_map_key'

# newYork <- google_geocode(address = "New York", key = apiKey)

# google_map(location = as.numeric(newYork$results$geometry$location), 
#            key = mapKey)

ebd_tnc_data <- read.csv("raw_data/ebd_tnc_combined_clean.csv")

spat_data <- st_as_sf(ebd_tnc_data, coords = c("longitude","latitude"), crs = 4326)

ebd_spat <- spat_data %>%
  filter(set == "eBird")

tnc_spat <- spat_data %>%
  filter(set == "TNC")

study_ext <- st_bbox(tnc_spat)

#leaflet() %>% addProviderTiles(providers$USGS.USImagery)

register_stadiamaps("bba3bbd6-b835-45c4-a587-ff2384a680a4", write = FALSE)
ggmap::register_google(key="AIzaSyAyFsT4CJGWUhUx6z8IDH4H3Oj40MYmR9k")
api_secret <- "AIzaSyAyFsT4CJGWUhUx6z8IDH4H3Oj40MYmR9k"
#CA_map <- get_map(location ='california', source="stamen")
# bounds <- c(left=-122,bottom=35,right=-118,top=40)
# bounds <- c(left=-130,bottom=30,right=-110,top=45)
bounds_zoom <- c(left=-123.5,bottom=33.5,right=-117.5,top=42)
#-124.410607, 32.534231, -114.134458, and 42.009659
CA_map_sat <- get_map(location = bounds_zoom, maptype ="satellite", source="google")
#CA_map_zoom <- get_map(location = bounds_zoom, maptype ="terrain", source="google")


ggmap(CA_map_zoom)

#plot(st_geometry(ebd_spat, add=T))

ext <- st_bbox(spat_data)
CA_map <- get_stadiamap(ext, zoom= 10, maptype = "stamen_terrain")

#CA_map <- get_googlemap("waco texas", zoom = 12, maptype = "satellite")

site_map = ggmap(get_stadiamap(bounds, maptype = "stamen_terrain", zoom = 2))

ggplot()+
  geom_sf(data = ebd_spat, color = "blue", size = 0.1)+
  geom_sf(data = tnc_spat, color = "red", size = 0.1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90))

ebd_tnc_data$Dataset <- ebd_tnc_data$set

ebd_tnc_data$Dataset <- factor(ebd_tnc_data$Dataset, levels=c("eBird","TNC"))

ggmap(CA_map_sat)+
  geom_point(aes(x=longitude, y=latitude, fill = Dataset), data = ebd_tnc_data, alpha = 0.3,size = 0.8, colour="black",pch=21)+
  scale_fill_manual(values=c("#D81B60","#FFC107"))+
  ylab("Latitude")+
  xlab("Longitude")

ggmap(CA_map_sat)+
  geom_point(aes(x=longitude, y=latitude, color = Dataset), data = ebd_tnc_data, alpha = 0.3,size = 0.8)+
  scale_color_manual(values=c("#FFC107","#D81B60"))+
  ylab("Latitude")+
  xlab("Longitude")

ebd_data <- ebd_tnc_data %>%
  filter(Dataset == "eBird")
tnc_data <- ebd_tnc_data %>%
  filter(Dataset == "TNC")

png("data_outputs/results/figures/survey_locations_clean_2.png", height = 12, width = 12, units = "in",res=300)

ggmap(CA_map_sat)+
  geom_point(aes(x=longitude, y=latitude), fill= "#D81B60", data = ebd_data, alpha = 0.5,size = 3, color = "black", pch=21)+
  geom_point(aes(x=longitude, y=latitude), fill = "#FFC107", data = tnc_data, alpha = 0.9,size = 3, color = "black", pch=21)+
  geom_rect(aes(xmin = -122.2116, xmax = -121.3781, ymin = 38.6040, ymax = 39.5573), color = "black", fill = NA, size = 2)+
  #scale_fill_manual(values=c("#D81B60","#FFC107"))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic(base_size = 22, base_family = "sans")
  
dev.off()

#inset map

#bounds_zoom <- c(left=-123.5,bottom=33.5,right=-117.5,top=42)
#bounds_inset <- c(left=-122.2116,bottom=38.6040,right=-121.3781,top=39.5573)
bounds_inset <- c(left=-122.4,bottom=38.2,right=-121,top=39.8)

CA_map_inset <- get_map(location = bounds_inset, maptype ="satellite", source="google")

png("data_outputs/results/figures/survey_locations_inset.png", height = 12, width = 12, units = "in",res=300)

ggmap(CA_map_inset)+
  geom_point(aes(x=longitude, y=latitude), fill= "#D81B60", data = ebd_data, alpha = 0.5,size = 3, color = "black", pch=21)+
  geom_point(aes(x=longitude, y=latitude), fill = "#FFC107", data = tnc_data, alpha = 0.9,size = 3, color = "black", pch=21)+
  #geom_rect(aes(xmin = -122.2116, xmax = -121.3781, ymin = 38.6040, ymax = 39.5573), color = "black", fill = NA, size = 2)+
  #scale_fill_manual(values=c("#D81B60","#FFC107"))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme_classic(base_size = 22, base_family = "sans")

dev.off()

#broader study extent map

bounds_broad <- c(left=-140,bottom=20,right=-70,top=50)

CA_map_broad <- get_map(location = bounds_broad, maptype = "satellite", source="google")

ggmap(CA_map_broad)

png("data_outputs/results/figures/survey_locations_broad.png", height = 12, width = 12, units = "in",res=300)

ggmap(CA_map_broad)

dev.off()




#geom_point(aes(x=longitude, y=latitude), data = tnc_spat, color = "red", size = 0.1)
  


# map_proj <- st_crs(4326)
# ne_state_lines <- read_sf("raw_data/gis-data.gpkg", "ne_state_lines") %>% 
#   st_transform(crs = map_proj) %>% 
#   st_geometry()

#plot(site_map)
