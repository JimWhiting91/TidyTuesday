# Map Week
lib <- c("cowplot","extrafont","stars","spData","spDataLarge","tmap","leaflet","ggplot2","sf","raster","viridis","geojsonio")
sapply(lib,library,character.only=T)

setwd("~/TidyTuesday/211102_map_week/")

# Fetch the city boundary...
all_bounds <- st_read(dsn = "data/Local_Authority_Districts_(December_2020)_UK_BFC/Local_Authority_Districts_(December_2020)_UK_BFC.shp")
sheffield_bound = subset(all_bounds, LAD20NM=="Sheffield")
plot(sheffield_bound)

# Fetch road data
cycle_routes <- st_read(dsn = "data/sheffield/cycleway_line.shp")
cycle_routes_df <- as.data.frame(cycle_routes)

# # Visualise cycle routes...
# ggplot() +
#   geom_sf(data = cycle_routes)+
#   geom_sf(data=sheffield_bound)

# Match up projections...
sheffield_bound <- st_transform(sheffield_bound,
                                crs(cycle_routes))
# ggplot() +
#   geom_sf(data = sheffield_bound)+
#     geom_sf(data = cycle_routes)

# Fetch our elevation raster and trim to the same borders
sheffield_elevation <- raster("data/sheffield/n53_w002_1arc_v3.tif")
# plot(sheffield_elevation, main = "Sheffield Elevation")

sheffield_elevation2 <- crop(sheffield_elevation, extent(sheffield_bound))
sheffield_elevation_crop <- mask(sheffield_elevation2, sheffield_bound)
# plot(sheffield_elevation_crop, main = "Sheffield Elevation Cropped")

# Convert elevation to a data.frame
sheffield_elevation_df <- as.data.frame(sheffield_elevation2, xy = TRUE)
colnames(sheffield_elevation_df) <- c("x","y","alt")

# # Quickly visualise elevation
# ggplot() +
#   geom_raster(data = sheffield_elevation_df , aes(x = x, y = y, fill = alt))+
#   scale_fill_viridis(option="A")+
#   geom_sf(data = sheffield_bound)

# Calculate slope based on elevation raster...
slope_ras <- terrain(sheffield_elevation, opt=c('slope', 'aspect'), unit='degrees')
# plot(slope_ras)

# Convert to shapefile and merge with cycle routes...
slope_sf <- st_as_sf(st_as_stars(slope_ras))

# Merge slope with cycle routes and filter based on polygon...
cycle_route_merged <- st_join(cycle_routes, slope_sf)
cycle_route_merged_sheff_only <- cycle_route_merged[st_within(cycle_route_merged,sheffield_bound) %>% lengths > 0,]

# Plot slopes
slope_map <- ggplot() +
  geom_raster(data = na.omit(as.data.frame(sheffield_elevation_crop, xy = TRUE)) , aes(x = x, y = y, fill = na.omit(as.data.frame(sheffield_elevation_crop, xy = TRUE))[,3]),show.legend = F)+
  scale_fill_continuous(low="white",high="black")+
  theme_void()+
  theme(plot.title=element_text(size=52,  family = "mono"),
        plot.subtitle=element_text(size=24,  family = "mono"),
        legend.position="bottom",
        legend.title=element_text(size=28,  family = "mono"),
        legend.text=element_text(size=22,  family = "mono",),
        legend.key.width=unit(2,"cm"))+
  geom_sf(data = cycle_route_merged_sheff_only,aes(colour=slope+1))+
  scale_colour_viridis(option="B",trans="log",breaks=c(1,3,8,20))+
  labs(title="Sheffield Cycling",
       subtitle="How much effort?",
       colour=expression(Slope~(degree)))+
  guides(colour = guide_colourbar(title.position="top"))

# Fetch the pub data - OSM
sheffield_pubs <- st_read(dsn = "data/sheffield/sheffield_pubs.shp")

# Calculate distance between pub and cycle route..
pub_dist <- st_distance(cycle_route_merged_sheff_only,sheffield_pubs)
nearest_pub <- apply(pub_dist,1,min)
cycle_route_merged_sheff_only$pub_dist <- nearest_pub

# Plot map of pub dists
pub_map <- ggplot() +
  geom_sf(data=sheffield_bound,colour="gray50")+
  geom_sf(data=sheffield_pubs,size=2)+
  scale_fill_continuous(low="white",high="black")+
  theme_void()+
  theme(plot.subtitle=element_text(size=24,  family = "mono"),
        legend.position="bottom",
        legend.title=element_text(size=28,  family = "mono"),
        legend.text=element_text(size=22,  family = "mono"),
        legend.key.width=unit(2,"cm"))+
  geom_sf(data = cycle_route_merged_sheff_only,aes(colour=pub_dist))+
  scale_colour_viridis(option="B",trans="log10")+
  labs(title="",
       subtitle="How far to the pub?",
       caption = "Sheffield Boundary: Ordnance Survey (UK)\nElevation: USGS\nCycleways: OSM\nPubs: OSM",
       colour="Distance (m)")+
  guides(colour = guide_colourbar(title.position="top"))

# Plot together
pdf("figs/sheffield_maps.pdf", width=12, height=7)
plot_grid(slope_map,pub_map,ncol=2,align="h",axis = "tblr")
dev.off()

png("figs/sheffield_maps.png", width=12, height=7)
plot_grid(slope_map,pub_map,ncol=2,align="h",axis = "tblr")
dev.off()



