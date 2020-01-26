#***************************************************************************************************
#
#  Create the Geo data (x,y coords) for all parcels in king county in 1999 and current
#
#***************************************************************************************************

 ## Load Libraries

  library(sf)
  library(tidyverse)
  library(kingCoData)

  data_path <- 'c:/dropbox/andy/public/'

 # Set King County Assessor's Projection
  king_county_proj <- paste0('+proj=lcc +lat_0=47 +lon_0=-120.833333333333 ',
                             '+lat_1=47.5 +lat_2=48.7333333333333 +x_0=500000.000000001 +y_0=0 ',
                             '+ellps=GRS80 +units=us-ft +no_defs')

  # load data
  parcel_polygon_1999_path <- file.path(data_path, 'raw', 'parcel_shapefiles_1999',
                                        'parcel.shp')
  poly99_sf <- sf::st_read(parcel_polygon_1999_path)

  parcel_polygon_2020_path <- file.path(data_path, 'raw', 'parcel_shapefiles_2020',
                                        'king_county_parcels__parcel_area.shp')
  poly20_sf <- sf::st_read(parcel_polygon_2020_path)

  # Set CRS
  sf::st_crs(poly99_sf) <- king_county_proj
  poly99_sf <- poly99_sf %>%
    sf::st_transform(4326)

  # Remove invalid polygon
  valid_idx <- sf::st_is_valid(poly99_sf)
  poly99_sf <- poly99_sf %>%
    dplyr::filter(!is.na(valid_idx))

  valid_idx <- sf::st_is_valid(poly20_sf)
  poly20_sf <- poly20_sf %>%
    dplyr::filter(!is.na(valid_idx))

  # Calculate Centroids
  point99_sf <- poly99_sf %>%
    sf::st_centroid()

  point20_sf <- poly20_sf %>%
    sf::st_centroid()

  # Extract centroids as features
  coords99_df <-
    sf::st_coordinates(point99_sf) %>%
    as.data.frame()

  coords20_df <-
    sf::st_coordinates(point20_sf) %>%
    as.data.frame()

  # Conver tto data.frame
  geo99_df <- data.frame(PIN = point99_sf$PIN,
                         latitude = coords99_df$Y,
                         longitude = coords99_df$X)

  geo20_df <- data.frame(PIN = point20_sf$PIN,
                         latitude = coords20_df$Y,
                         longitude = coords20_df$X)

  ## Save Data
  saveRDS(geo99_df, file.path(data_path, 'ready', 'geo_99.rds'))
  saveRDS(geo20_df, file.path(data_path, 'ready', 'geo_new.rds'))

#***************************************************************************************************
#***************************************************************************************************
