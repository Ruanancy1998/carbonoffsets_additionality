
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_states <- ne_states(returnclass = "sf")

#ggplot(data = world_states) +
#  geom_sf() +
#  labs(title = "World Map (States and Provinces)",
#       subtitle = "Data sourced from Natural Earth") +
#  theme_minimal()

# 绘制地图
#ggplot(data = world) +
#  geom_sf() +
#  labs(title = "World Map",
#       subtitle = "Simple world map using sf and ggplot2") +
#  theme_minimal()

#tmap_mode("plot")
#tm_shape(world) +
#  tm_polygons() +
#  tm_layout(title = "World Map using tmap")

#leaflet() %>%
#  addTiles() %>%
#  addPolygons(data = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
#              color = "blue", weight = 1, opacity = 0.7,
#              fillOpacity = 0.5)


#china_city_map <- st_read("/Users/nancy/Desktop/QGIS/我国2020年省、市、县行政区划（地理坐标系）/2020年我国市级行政区划.shp")
#china_border_map <- st_read("/Users/nancy/Desktop/QGIS/我国2020年省、市、县行政区划（地理坐标系）/我国国界线.shp")

#ggplot() +
  # 添加市级行政区划
#  geom_sf(data = china_city_map, fill = "lightblue", color = "gray", alpha = 0.6) +
  # 添加国界线
#  geom_sf(data = china_border_map, color = "black") +
#  labs(
#    title = "China City Map with National Border",
#    subtitle = "2020 City and National Boundary",
#    caption = "Source: Custom Shapefiles"
#  ) +
#  theme_minimal()

