# library(tidyverse)
# library(leaflet)
# library(mapview)
#
# shapes <- read_rds("out/oa_shapes.rds")
#
# census <- read_rds("out/census.rds")
#
# shapes %>%
#   filter(ladnm=="Exeter") %>%
#   left_join(census, by = c("lsoa11cd" = "geography_code")) %>%
#   leaflet::leaflet() %>%
#   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
#   leaflet::addPolygons(weight = 1,
#                        opacity = 0.8,
#                        color = "grey",
#                        fillColor = ~colorQuantile("OrRd", percentage_person_age_18_to_19)(percentage_person_age_18_to_19)) %>%
#   addMiniMap(
#     tiles = providers$CartoDB.Positron,
#     position = 'topright',
#     width = 200, height = 200,
#     toggleDisplay = FALSE,
#     zoomLevelOffset = -6)
#
#
# geo_out_ladcd %>%
#   leaflet::leaflet() %>%
#   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
#   leaflet::addPolygons(weight = 1,
#                        opacity = 0.8,
#                        color = "grey")
#
