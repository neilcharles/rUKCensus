process_facts <- function(){

  var_names <- readr::read_csv("inst/extdata/stats/2011/name_match.csv") %>%
    dplyr::mutate(scot_name = janitor::make_clean_names(scot_name),
                  eng_wa_name = janitor::make_clean_names(eng_wa_name),
                  name = janitor::make_clean_names(name)) %>%
    dplyr::filter(include) %>%
    dplyr::select(-include)

  # Load England & Wales ---------------------------------------------------------
  raw_ew <- tibble::tibble(filename = list.files(path = "inst/extdata/stats/2011/england_wales", pattern = ".*.csv", full.names = FALSE)) %>%
    dplyr::mutate(data = purrr::map(.x = filename, .f = ~ readr::read_csv(glue::glue("inst/extdata/stats/2011/england_wales/{.x}"), col_types = readr::cols(.default = "c")))) %>%
    dplyr::mutate(filename = stringr::str_extract(filename, "^[A-Z][A-Z]\\d\\d\\d")) %>%
    dplyr::filter(filename %in% var_names$filename)

  raw_ew_long <- raw_ew %>%
    dplyr::mutate(data2 = purrr::map(
      .x = data,
      .f = ~
        tidyr::pivot_longer(
          janitor::clean_names(.),
          -c(date, geography, `geography_code`, `rural_urban`)
        )
    )) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(data2) %>%
    dplyr::select(-date, -geography, -rural_urban) %>%
    dplyr::rename(geo_id = geography_code,
                  eng_wa_name = name) %>%
    dplyr::left_join(dplyr::select(var_names, filename, eng_wa_name, name)) %>%
    dplyr::select(-eng_wa_name) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(value = stringr::str_replace(value, "^-$", "0")) %>%
    dplyr::mutate(value = stringr::str_replace_all(value, ",","")) %>%
    dplyr::mutate(value = as.numeric(value))


  # Load Scotland ----------------------------------------------------------------
  raw_scot <- tibble::tibble(filename = list.files(path = "inst/extdata/stats/2011/scotland/KS", pattern = "*.csv", full.names = FALSE)) %>%
    dplyr::mutate(data = purrr::map(.x = filename, .f = ~readr::read_csv(glue::glue("inst/extdata/stats/2011/scotland/KS/{.x}"), col_types = readr::cols(.default = "c")) %>%
                                      dplyr::rename(geography_code = ...1))) %>%
    dplyr::mutate(filename = stringr::str_extract(filename, "^[A-Z][A-Z]\\d\\d\\d")) %>%
    dplyr::filter(filename %in% var_names$filename)

  raw_scot_long <- raw_scot %>%
    dplyr::mutate(data2 = purrr::map(
      .x = data,
      .f = ~
        tidyr::pivot_longer(
          janitor::clean_names(.),
          -c(`geography_code`)
        )
    )) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(data2) %>%
    dplyr::rename(geo_id = geography_code,
                  scot_name = name) %>%
    dplyr::left_join(dplyr::select(var_names, filename, scot_name, name)) %>%
    dplyr::select(-scot_name) %>%
    dplyr::filter(!is.na(name))

  #Summarise Scotland (OA to LSOA)
  raw_scot_long_lsoa <- raw_scot_long %>%
    dplyr::left_join(dplyr::summarise(dplyr::group_by(lookup_areas, oa11cd, lsoa11cd)), by = c('geo_id' = 'oa11cd')) %>%
    dplyr::mutate(value = stringr::str_replace(value, "^-$", "0")) %>%
    dplyr::mutate(value = stringr::str_replace_all(value, ",","")) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    dplyr::group_by(filename, lsoa11cd, name) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::rename(geo_id = lsoa11cd) %>%
    dplyr::filter(!is.na(geo_id))

  # Merge England, Wales & Scotland
  raw_all <- raw_ew_long %>%
    dplyr::union_all(raw_scot_long_lsoa) %>%
    dplyr::filter(glue::glue("{filename}{name}") %in% glue::glue("{var_names$filename}{var_names$name}")) %>% #Remove unwanted vars
    dplyr::select(-filename)

  raw_wide <- raw_all %>%
    tidyr::pivot_wider(names_from = "name", values_from = "value")

  # check for NA and identify problem columns
  if(any(is.na(raw_wide))){

    for(i in 1:ncol(raw_wide)){
      if(any(is.na(raw_wide[,i]))) print(i)
    }

    stop("Names didn't match")
  }

  facts_lsoa_2011 <- raw_wide

  usethis::use_data(facts_lsoa_2011, overwrite = TRUE)

  # Run this if output contains list cols
  # test <- raw_all %>%
  #   dplyr::group_by(geo_id, name) %>%
  #   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #   dplyr::filter(n > 1L)

  facts_ga_2011 <- facts_lsoa_2011 %>%
    dplyr::left_join(lsoa_ga_lookup, multiple = "all") %>%
    dplyr::mutate(across(!geo_id & !geo_name & !ga_location_id & !ga_city, ~as.numeric(. * area_pct))) %>%
    dplyr::select(-area_pct, -geo_id, -geo_name) %>%
    dplyr::group_by(ga_location_id, ga_city) %>%
    dplyr::summarise(across(everything(), ~sum(.))) %>%
    dplyr::filter(!is.na(ga_location_id))

  usethis::use_data(facts_ga_2011, overwrite = TRUE)
}

#Segment LSOA level demographics
create_segmentation <- function(){

  #Convert to % of population except for density columns
  pct_all <- facts_lsoa_2011 %>%
    mutate(across(!geo_id &
                    !variable_all_usual_residents,
                  ~./variable_all_usual_residents))

  segmentation_raw <- scale(
    pct_all[3:ncol(pct_all)]
  )

  set.seed(42)
  fit <- kmeans(segmentation_raw, 12, algorithm = "Hartigan", iter.max = 1000)

  # tibble(x = fit$cluster) %>%
  #   group_by(x) %>%
  #   summarise(n = n()) %>%
  #   ggplot2::ggplot(aes(x = x, y = n)) +
  #   geom_col()

  mapdata <- tibble(geo_id = pct_all$geo_id,
                    cluster = fit$cluster)

  colours <- tibble(col = pals::alphabet()) %>%
    mutate(cluster=row_number())

  mapshapes <- rUKcensus::shapes_lsoa %>%
    left_join(mapdata) %>%
    left_join(colours)

  # leaflet(filter(mapshapes, cluster==10)) %>%
  # leaflet::leaflet(mapshapes) %>%
  #   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  #   leaflet::addPolygons(fillColor = ~as.character(col),
  #               popup = ~as.character(cluster),
  #               weight = 0.5,
  #               color = "black")

cluster_names <- tibble::tibble(
  cluster = seq(1:12),
  cluster_name = c(
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "",
  "")
)

clusters_lsoa_2011 <- mapdata %>%
  sf::st_drop_geometry() %>%
  left_join(cluster_names)

usethis::use_data(clusters_lsoa_2011, overwrite = TRUE)

}
