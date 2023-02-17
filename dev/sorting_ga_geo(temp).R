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

# ga_demogs <- google_analytics(ga_id,
#                        date_range = c("2021-01-01", "2021-12-31"),
#                        metrics = "sessions",
#                        dimensions = c("userAgeBracket", "userGender"),
#                        filtersExpression = "ga:country==United Kingdom",
#                        max = -1)
#
# ga_demogs_match <- ga_demogs %>%
#   unite(age_gender, userAgeBracket, userGender) %>%
#   mutate(weight = sessions / sum(sessions)) %>%
#   select(-sessions) %>%
#   crossing(ga_rollup) %>%
#   mutate(sessions_weighted = sessions * weight) %>%
#   arrange(ga_location_id)

#Transform traffic to voronoi
voronoi <- ga_points %>%
  st_union() %>%
  sf::st_voronoi() %>%
  st_collection_extract() %>%
  st_sf() %>%
  st_intersection(rUKcensus::shapes_uk_coastline) %>%
  st_join(ga_points, join = st_contains) #join with original to get traffic back

voronoi %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons()

voronoi_msoa <- voronoi %>%
  st_intersection(shapes_msoa) %>%
  mutate(intersect_area = st_area(.)) %>%
  group_by(geo_id) %>%
  mutate(area_pct = intersect_area / sum(intersect_area)) %>%
  filter(area_pct == max(area_pct)) %>%
  st_drop_geometry() %>%
  group_by(ga_location_id) %>%
  mutate(sessions_weighted = sessions * (population / sum(population))) %>%
  select(geo_id, sessions_weighted)

shapes_traffic <-
  shapes_msoa %>%
  left_join(voronoi_msoa, by = "geo_id")

shapes_traffic %>%
  mutate(sessions_per_000 = sessions_weighted / population * 1000) %>%
  leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(color = "#444444", weight = 1,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", sessions_per_000)(sessions_per_000), n = 10)
