
library(tmap)
tmap::tmap_mode('view')

### Census shapefiles


# Get Raw Data
king_boundary_sf <- tidycensus::get_acs(
  geography = "county",
  variables = "B19013_001",
  state = 'WA',
  year = 2020,
  geometry = TRUE) %>%
  dplyr::filter(GEOID == 53033)

king_tracts_sf <- tidycensus::get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = 'WA',
  year = 2020,
  geometry = TRUE) %>%
  dplyr::filter(substr(as.character(GEOID), 1, 5) == '53033')


osmQueryBuilder <- function(ql_obj,
                            qry_bbox,
                            timeout = 240){

  oq <- osmdata::opq(bbox = qry_bbox,
                     timeout = timeout)

  if (is.null(ql_obj$value)){

    oq %>%
      osmdata::add_osm_feature(key = ql_obj$key) ->
      oq

  } else {

    if (is.null(ql_obj$name)){

      oq %>%
        osmdata::add_osm_feature(key = ql_obj$key,
                                 value = ql_obj$value) ->
        oq

    } else {

      if(is.null(ql_obj$exact)) ql_obj$exact <- TRUE
      oq %>%
        osmdata::add_osm_feature(key = ql_obj$key,
                                 value = ql_obj$value) %>%
        osmdata::add_osm_feature(key = 'name:en',
                                 value = ql_obj$name,
                                 value_exact = ql_obj$exact) ->
        oq
    }
  }

  oq
}


king_boundaries <-
  osmQueryBuilder(ql_obj = list(key = 'boundary'),
                  qry_bbox = sf::st_bbox(king_boundary_sf)) %>%
    osmdata::osmdata_sf()


king_water <-
  osmQueryBuilder(ql_obj = list(key = 'water'),
                  qry_bbox = sf::st_bbox(king_boundary_sf)) %>%
  osmdata::osmdata_sf()

water_psf <- king_water$osm_polygons %>%
  dplyr::bind_rows(., king_water$osm_multipolygons)

water_lsf <- king_water$osm_lines
water_xsf <- king_water$osm_points


tm_shape(king_boundary_sf) + tm_borders() +
  tm_shape(water_psf) + tm_fill(col = 'blue') +
  tm_shape(water_lsf) + tm_lines(col = 'red') +
  tm_shape(water_xsf) + tm_dots(col = 'purple')

