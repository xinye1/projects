# Hexagon shape source:
# http://www.arcgis.com/home/item.html?id=593037bc399e460bb7c6c631ceff67b4 - GB Local Authority Hexagon Cartogram (Esri Shapefile)
# http://www.arcgis.com/home/item.html?id=15baaa6fecd54aa4b7250780b6534682 - GB Parliamentary Constituency Hexagon Cartogram (Esri Shapefile)
# https://www.arcgis.com/home/item.html?id=d348614c97264ae19b0311019a5f2276 - Cartogram geoprocessing tool (not used in this project)

pacman::p_load(rgdal)

# Load the hexagon polygon shape file (la for local authority)
la_shp <- readOGR(dsn = './data/GB_Hex_Cartogram_LAs', layer = "GB_Hex_Cartogram_LAs")

# check the shape file
summary(la_shp)
plot(la_shp)
names_shp <- as.character(la_shp$LAD12NM) # for matching the CSV file later

# Alternatively the polygon shape files for the actual geographic shapes can be found on https://data.gov.uk/
# A sample shape file is in data/ folder
la_1 <- readOGR('./data/Local_Authority_Districts_December_2016_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain',
                  layer = 'Local_Authority_Districts_December_2016_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain')