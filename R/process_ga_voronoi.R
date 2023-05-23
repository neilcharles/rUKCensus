run_ga_voronoi <- function(){
  sf::sf_use_s2(FALSE)

  googleAnalyticsR::ga_auth()

  account_list <- googleAnalyticsR::ga_account_list()

  all_geo <- account_list %>%
    filter(viewName=="All Web Site Data") %>%
    dplyr::select(viewId) %>%
    dplyr::mutate(geo = purrr::map(.x = viewId, .f = ~
                              googleAnalyticsR::google_analytics(.x,
                                               date_range = c("2021-01-01", "2022-12-31"),
                                               metrics = "sessions",
                                               dimensions = c("city", "latitude", "longitude"),
                                               filtersExpression = "ga:country==United Kingdom",
                                               max = -1)
                              ))

  unique_geo <- all_geo %>%
    tidyr::unnest(geo) %>%
    dplyr::group_by(city, latitude, longitude) %>%
    dplyr::summarise() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(c(latitude, longitude), as.numeric)) %>%
    dplyr::filter(!(latitude==0 & longitude==0)) %>%
    dplyr::mutate(ga_location_id = dplyr::row_number())

  usethis::use_data(unique_geo, overwrite = TRUE)

  ga_points <- gaHelpers::ga_to_points(unique_geo)

  #Transform traffic to voronoi
  ga_voronoi <- ga_points %>%
    sf::st_union() %>%
    sf::st_voronoi() %>%
    sf::st_collection_extract() %>%
    sf::st_sf() %>%
    sf::st_intersection(dplyr::select(shapes_uk_coastline, geometry)) %>%
    sf::st_join(ga_points, join = sf::st_contains) #join with original to get traffic back

  usethis::use_data(ga_voronoi, overwrite = TRUE)

  lsoa_ga_lookup <- ga_voronoi %>%
    sf::st_intersection(shapes_lsoa) %>%
    dplyr::mutate(intersect_area = sf::st_area(.)) %>%
    dplyr::group_by(geo_id) %>%
    dplyr::mutate(area_pct = intersect_area / sum(intersect_area)) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(geo_id, geo_name, ga_location_id, city, area_pct) %>%
    dplyr::rename(ga_city = city) %>%
    dplyr::filter(!is.na(geo_location_id))

  usethis::use_data(lsoa_ga_lookup, overwrite = TRUE)

}
