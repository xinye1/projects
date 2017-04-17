# setup and loading packages
pacman::p_load(
  devtools, purrr, dplyr, htmlwidgets, stringr, rvest, xml2,
  htmltools, leaflet, cartogram, maptools, broom, ggplot2, ggmap)
source('./code/process_shapes.R')
source('./code/process_votes.R')


# Before plotting the map, the shape file is checked against the CSV for location matching
names_diff <- setdiff(names_votes, names_shp)
names_diff1 <- setdiff(names_shp, names_votes)

summary(la_shp)
# EPSG:3857 project

la_84 <- spTransform(la_shp, CRS('+init=epsg:4326'))
la_bng <- spTransform(la_shp, CRS('+init=epsg:27700'))
# Transverse Mercator projection aka British National Grid

# Try to understand how the hexagon map is constructed
la_84_f <- fortify(la_84)
b <- bbox(la_1)
b[2, 2] <- 58
b[1, 2] <- 2.5
basemap <- ggmap(
  get_map(location = b,
          source = "stamen",
          maptype = "watercolor",
          crop = T))
basemap + geom_polygon(
  data = la_84_f,
  aes(x = long, y = lat, group = group),
  fill = 'darkorchid',
  alpha = 0.7) + xlab('Longitude') + ylab('Latitude')

# Examine the hexagon map using leaflet
labels2 <- sprintf(
  '<strong>%s</strong>',
  la_84$LAD12NM
) %>% lapply(htmltools::HTML)

leaflet(la_84) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    label = labels2,
    opacity = 0.8, fillColor = 'red',
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))


# Use BNG map to creat new features for NI and Gibralta
# Barrow-in-Furness seems close enough to NI
# This feature will be shifted to the west to become NI!
# Then shift NI south beyond the bottom edge to become Gibrraltar
ind_ref <- which(la_bng$LAD12NM == 'Barrow-in-Furness')
ni <- la_bng@polygons[[ind_ref]]
ni_coors <- ni@Polygons[[1]]@coords

# NI seems around 150,000 meters to the west
# GI 500,000 meters to the sount
ni_shiftx <- -100000
gi_shifty <- -600000

# Shape coordinates
ni_coors[, 1] <- ni_coors[, 1] + ni_shiftx
gi_coors <- ni_coors
gi_coors[, 2] <- ni_coors[, 2] + gi_shifty

# Labpt coordinates
ni_labpt_x <- ni@labpt[1] - ni_shiftx
ni_labpt_y <- ni@labpt[2]
ni_labpt <- c(ni_labpt_x, ni_labpt_y)

gi_labpt_x <- ni@labpt[1] + ni_shiftx
gi_labpt_y <- ni@labpt[2] + gi_shifty
gi_labpt <- c(gi_labpt_x, gi_labpt_y)


# Replace coordiates and ID in ni and gi
ni_poly <- ni
gi_poly <- ni
ni_poly@Polygons[[1]]@coords <- ni_coors
gi_poly@Polygons[[1]]@coords <- gi_coors
ni_poly@Polygons[[1]]@labpt <- ni_labpt
gi_poly@Polygons[[1]]@labpt <- gi_labpt
ni_poly@labpt <- ni_labpt
gi_poly@labpt <- gi_labpt
new_poly_ids <- c('380', '381')
ni_poly@ID <- new_poly_ids[1]
gi_poly@ID <- new_poly_ids[2]


# Construct the new SpatialPolygons
la_bng_polygons <- c(la_bng@polygons, ni_poly, gi_poly)
la_bng_spgs <- SpatialPolygons(la_bng_polygons)
proj4string(la_bng_spgs) <- proj4string(la_bng)
plot(la_bng_spgs)

# Construct the new SpatialPolygonsDataFrame
ni_lad12cn <- raw_votes[raw_votes$Area == 'Northern Ireland', 'Area_Code']
gi_lad12cn <- raw_votes[raw_votes$Area == 'Gibraltar', 'Area_Code']
new_data <- data.frame(
  OBJECTID = c('380', '381'),
  LAD12CD = c(ni_lad12cn, gi_lad12cn),
  LAD12NM = c('Northern Ireland', 'Gibraltar'),
  Shape_Leng = mean(la_bng$Shape_Leng),
  Shape_Area = mean(la_bng$Shape_Area))
la_new_data <- rbind(la_bng@data, new_data)

rownames(la_new_data)[(nrow(la_new_data) - 1):nrow(la_new_data)] <- new_poly_ids
la_bng_new <- SpatialPolygonsDataFrame(la_bng_spgs, la_new_data)


# Fix the last bit of discrepancy in CSV
raw_votes$Area[grepl('glamorgan', raw_votes$Area, ignore.case = T)] <- 'The Vale of Glamorgan'


# Merge the CSV to the shape data
la_bng_new <- la_bng_new %>% merge(raw_votes, by.x = 'LAD12NM', by.y = 'Area')
la_bng_new$Result <- ifelse(la_bng_new$Remain > la_bng_new$Leave, 'Remain', 'Leave') %>% as.factor
cols <- c('#ff6666', '#66ccff')
plot(la_bng_new, col = cols[la_bng_new$Result])


# Before running the following line, if encounter this error
# Error: isTRUE(gpclibPermitStatus()) is not TRUE
# on Windows, make sure Rtools is installed, then install package gpclib
# install.packages('gpclib', type = 'source')
# To verify everything is fine, run
# gpclibPermitStatus()
# gpclibPermit()
# If all True then continue

# ggplot of hexagons ----
la_df <- la_bng_new %>% tidy(region = "LAD12NM")
la_df <- la_df %>%
  left_join(
    data.frame(id = as.character(la_bng_new$LAD12NM),
               Result = as.factor(la_bng_new$Result),
               stringsAsFactors = F))
ggplot() + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
  geom_polygon(data = la_df, aes(x = long, y = lat, group = id, fill = Result))


# Cartogram ----
# install_github("omegahat/Rcartogram")
# install_github('chrisbrunsdon/getcartr', subdir='getcartr')
carto <- cartogram(la_bng_new, 'Valid_Votes')
plot(carto, col = cols[la_bng_new$Result])
png('./output/brexit1.png')
plot(carto, col = cols[test$Result])
dev.off()

carto_df <- carto %>% tidy(region = 'LAD12NM')
carto_df <- carto_df %>%
  left_join(
    data.frame(id = as.character(la_bng_new$LAD12NM),
               Result = as.factor(la_bng_new$Result),
               stringsAsFactors = F))
png('./output/brexit2.png')
ggplot() + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
  geom_polygon(data = carto_df, aes(x = long, y = lat, group = id, fill = Result, colour = Result))
dev.off()



# normal shape ====
la_1_data <- la_1 %>% merge(raw_votes, by.x = 'lad16nm', by.y = 'Area')
la_1_data$Result <- ifelse(la_1_data$Remain > la_1_data$Leave, 'Remain', 'Leave')
# proj4string(auth_data) <- ee_get_const()$wgs84
auth_data@proj4string <- CRS(as.character(NA))
auth_data@proj4string <- ee_get_const()$wgs84
carto_1 <- cartogram(la_1_data, 'Valid_Votes')
plot(carto_1)
carto2 <- carto1 %>% fortify(region = 'Region')
# ggplot()  + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
#   geom_polygon(data = carto1, aes(x = long, y = lat, group = id, fill = as.factor(Result), colour = as.factor(Result)))
# doesn't work because fortifying doesn't work with small shapes?
# Error in RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, drop_lower_td, unaryUnion_if_byid_false,  : 
# TopologyException: Input geom 1 is invalid: Self-intersection at or near point 0.9290865878767145 51.887877493086037 at 0.9290865878767145 51.887877493086037


# Try leaflet ====
epsg4326 <- leafletCRS(
  crsClass = 'L.Proj.CRS',
  code = 'EPSG:4326',
  proj4def = '+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
  resolutions = 2^(16:7))

cols <- c('#b30000', '#0040ff')
labels1 <- sprintf(
  '<strong>%s</strong><br/>Remain: %s (%.0f%%)<br/>Leave: %s (%.0f%%)',
  carto1$lad16nm,
  prettyNum(carto1$Remain, big.mark = ','), 100 * carto1$Remain / carto1$Valid_Votes,
  prettyNum(carto1$Leave, big.mark = ','), 100 * carto1$Leave / carto1$Valid_Votes
) %>% lapply(htmltools::HTML)

leaflet(carto1, options = leafletOptions(crs = epsg4326)) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    label = labels1,
    opacity = 1.0, fillOpacity = 0.5, fillColor = cols[as.factor(carto1$Result)],
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))



# Try leaflet with the hexagon map ====
la_bng_new$Valid_Votes_sq <- la_bng_new$Valid_Votes ^ 2
carto_x <- cartogram(la_bng_new, 'Valid_Votes_sq')
carto_x$fill_op <- abs(carto_x$Pct_Remain - 50) / max(abs(carto_x$Pct_Remain - 50))
labels2 <- sprintf(
  '<strong>%s</strong><br/>Remain: %s (%.0f%%)<br/>Leave: %s (%.0f%%)',
  carto$LAD12NM,
  prettyNum(carto$Remain, big.mark = ','), 100 * carto$Remain / carto$Valid_Votes,
  prettyNum(carto$Leave, big.mark = ','), 100 * carto$Leave / carto$Valid_Votes
) %>% lapply(htmltools::HTML)

leaflet(carto_x, options = leafletOptions(crs = epsg4326)) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    label = labels2,
    opacity = 1.0, fillOpacity = carto_x$fill_op, fillColor = cols[as.factor(carto_x$Result)],
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))


# ggplot the ^2 version
carto_x_df <- carto_x %>% tidy(region = 'LAD12NM')
carto_x_df <- carto_x_df %>%
  left_join(
    data.frame(id = as.character(la_bng_new$LAD12NM),
               Result = as.factor(la_bng_new$Result),
               stringsAsFactors = F))
png('./output/brexit3.png')
ggplot() + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
  geom_polygon(data = carto_x_df, aes(x = long, y = lat, group = id, fill = Result, colour = Result))
dev.off()