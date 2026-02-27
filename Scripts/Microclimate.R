# generating microclimate map of Holden

# load package

#devtools::install_github("matthewkling/topoclimate.pred")
library(topoclimate.pred)
library(sf)

# read in the DEM for Holden
dem = raster("./Raw.Data/USGS_13_n42w082_20230911.tif")
names(dem) = "elevation"
# project the DEM
dem.proj = projectRaster(dem, crs = "+proj=longlat +datum=NAD83 +no_defs")

# crop to focus on Holden, specifically the natural areas
# read in the natural areas shape file
natareas = st_read("./Raw.Data/HA_Natural_Areas/")
# projected CRS = WGS 84/Pseudo-Mercator

# align the crs between dem and natareas
natareas.trans = natareas %>% 
  st_transform(st_crs(dem.proj))

# get bounding box of natareas
bb = st_bbox(natareas.trans)
bb_ext = extent(bb$xmin, bb$xmax, bb$ymin, bb$ymax)

dem.crop = crop(dem.proj,bb_ext)

# terrain map of natural areas
na.terrain = hillShade(terrain(dem.crop,"slope"), terrain(dem.crop,"aspect"))
plot(na.terrain,col = colorRampPalette(c("black","white"))(50),legend = F)

# get microclimate
clim <- bioclimate(dem.crop, include_inputs = TRUE)
plot(clim, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$high_temp, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$low_temp, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$moisture, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$northness, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$eastness, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$wayard_exposure, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$mTPI, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$macro_bio1, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$macro_bio12, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$macro_bio5, col = viridis::viridis_pal()(50), nrow = 1)
plot(clim$macro_bio6, col = viridis::viridis_pal()(50), nrow = 1)

# overlay the natural areas on the climate maps
library(ggplot2)
crs(natareas.trans)
crs(clim$high_temp)

high.temp.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = high_temp))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "High Temp (C)")+
  ggtitle("Summer Max Temp (1979-2009)")
high.temp.plot
ggsave("./Maps/high.temp.png", plot = high.temp.plot,
       width = 8, height = 6, dpi =300)

low.temp.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = low_temp))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "Low Temp (C)")+
  ggtitle("Winter Min Temp (1979-2009)")
low.temp.plot
ggsave("./Maps/low.temp.png", plot = low.temp.plot,
       width = 8, height = 6, dpi =300)

moisture.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = moisture))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "Moisture(mm)")+
  ggtitle("Annual Precipitation (1979-2009)")
moisture.plot
ggsave("./Maps/moisture.png", plot = moisture.plot,
       width = 8, height = 6, dpi =300)

northness.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = northness))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "Northness")+
  ggtitle("Northness, calculated from slope")
northness.plot
ggsave("./Maps/northness.png", plot = northness.plot,
       width = 8, height = 6, dpi =300)

eastness.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = eastness))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "Eastness")+
  ggtitle("Eastness, calculated from aspect")
eastness.plot
ggsave("./Maps/eastness.png", plot = eastness.plot,
       width = 8, height = 6, dpi =300)3

windward.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = windward_exposure))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "Windward")+
  ggtitle("Windward Exposure")
windward.plot
ggsave("./Maps/windward.png", plot = windward.plot,
       width = 8, height = 6, dpi =300)

mTPI.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = mTPI))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "mTPI")+
  ggtitle("Elevational Position")
mTPI.plot
ggsave("./Maps/mTPI.png", plot = mTPI.plot,
       width = 8, height = 6, dpi =300)

precip.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = macro_bio12))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "bio12")+
  ggtitle("Total Annual Precipitation")
precip.plot
ggsave("./Maps/precip.png", plot = precip.plot,
       width = 8, height = 6, dpi =300)

temp.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = macro_bio1))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "bio1")+
  ggtitle("Mean Annual Temperature")
temp.plot
ggsave("./Maps/temp.png", plot = temp.plot,
       width = 8, height = 6, dpi =300)

max.temp.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = macro_bio5))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "bio5")+
  ggtitle("Max Temp of Warmest Month")
max.temp.plot
ggsave("./Maps/max.temp.png", plot = max.temp.plot,
       width = 8, height = 6, dpi =300)

min.temp.plot = ggplot()+
  geom_raster(data = as.data.frame(clim, xy = TRUE),
              aes(x = x, y = y, fill = macro_bio6))+
  geom_sf(data = natareas.trans, fill = NA,
          color = "black", linewidth = 1)+
  scale_fill_viridis_c(option = "viridis",
                       name = "bio6")+
  ggtitle("Min Temp of Coldest Month")
min.temp.plot
ggsave("./Maps/min.temp.png", plot = min.temp.plot,
       width = 8, height = 6, dpi =300)


#### Extract data for Bri's plots ####

BLD.plots = st_read("./Raw.Data/BLD Forest Monitoring Plot Centers/")

# align the crs between other plots and BLD.plots
BLD.trans = BLD.plots %>% 
  st_transform(st_crs(natareas.trans))

BLD.plot.clim = raster::extract(clim, BLD.trans)
BLD.plot.clim = as.data.frame(BLD.plot.clim)
coords <- st_coordinates(BLD.trans) # -84.36060  41.63333
BLD.plot.clim.local = cbind(BLD.plot.clim,coords)

BLD.sub = BLD.trans[,c(29,30,36)]
BLD.coords = as.data.frame(st_coordinates(BLD.sub))
BLD.sub = cbind(BLD.sub,BLD.coords)

all = merge(BLD.plot.clim.local,BLD.sub)

# subset to plots on the property
all.2 = all %>% 
  filter(Local_Plac %in% c("Arboretum","Baldwin","Pierson Creek","Schoop","Upper Baldwin",
                           "Bole Woods","Stebbins"))
pc = princomp(all.2[,c(3:13)], cor = TRUE)
summary(pc)
pc$loadings
library(ggbiplot)
ggbiplot(pc)


