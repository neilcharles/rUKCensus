library(googleAnalyticsR)
library(tidyverse)
library(leaflet)
library(sf)
library(mapview)

sf_use_s2(FALSE)

ga_auth()

account_list <- ga_account_list()

all_geo <- account_list %>%
  select(viewId) %>%
  mutate(geo = purrr::map(.x = viewId, .f = ~
                            google_analytics(.x,
                                             date_range = c("2021-01-01", "2022-12-31"),
                                             metrics = "sessions",
                                             dimensions = c("city", "latitude", "longitude"),
                                             filtersExpression = "ga:country==United Kingdom",
                                             max = -1)
                            ))

unique_geo <- all_geo %>%
  tidyr::unnest(geo) %>%
  group_by(city, latitude, longitude) %>%
  summarise() %>%
  ungroup() %>%
  mutate(across(c(latitude, longitude), as.numeric)) %>%
  filter(!(latitude==0 & longitude==0)) %>%
  mutate(ga_location_id = row_number())

usethis::use_data(unique_geo)

ga_points <- gaHelpers::ga_to_points(unique_geo)

ga_points %>%
  leaflet() %>%
  addProviderTiles(provider = providers$OpenStreetMap) %>%
  addCircleMarkers()

#Transform traffic to voronoi
voronoi <- ga_points %>%
  st_union() %>%
  sf::st_voronoi() %>%
  st_collection_extract() %>%
  st_sf() %>%
  st_intersection(select(rUKcensus::shapes_uk_coastline, geometry)) %>%
  st_join(ga_points, join = st_contains) #join with original to get traffic back

voronoi_lsoa <- voronoi[1:10,] %>%
  st_intersection(shapes_lsoa) %>%
  mutate(intersect_area = st_area(.)) %>%
  group_by(geo_id) %>%
  mutate(area_pct = intersect_area / sum(intersect_area)) %>%
  st_drop_geometry() %>%
  select(geo_id, geo_name, ga_location_id, city, area_pct) %>%
  rename(ga_city = city)
