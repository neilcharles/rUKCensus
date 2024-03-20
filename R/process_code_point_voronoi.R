calc_post_sector_voronoi <- function(){
  library(tidyverse)
  library(glue)
  library(sf)
  library(leaflet)
  library(googlesheets4)
  library(nngeo)
  library(bigrquery)

  sf_use_s2(FALSE)

  uk_boundary <- st_read("inst/extdata/shapes/coastline/GBR_adm/GBR_adm1.shp") %>%
    st_transform(crs = 4326) %>%
    filter(NAME_1 != "Northern Ireland") |>
    dplyr::summarise(geometry = st_union(geometry)) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 0.001)

  code_point <- list.files(path = "inst/extdata/lookup/codepo_gb/Data/CSV/", pattern = "*.csv") %>%
    as_tibble() %>%
    mutate(full_path = glue("inst/extdata/lookup/codepo_gb/Data/CSV/{value}")) %>%
    pull(full_path) %>%
    map_df(~read_csv(., col_names = FALSE)) %>%
    rename(
      postcode = X1,
      lon = X3,
      lat = X4
    ) %>%
    filter(!(lat==0 & lon==0)) %>%
    select(postcode, lat, lon)

  #roll up to sector level for faster voronoi
  code_point_points <- code_point %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 27700, agr = "constant") %>%
    st_transform(crs = 4326) %>%
    mutate(post_sector = substr(postcode, 1, nchar(postcode) - 2)) %>%
    group_by(post_sector) %>%
    summarise(geometry = st_union(geometry)) %>% #group points by post sector
    group_by(post_sector) %>%
    summarise(geometry = st_centroid(geometry)) #find sector centroid


  shapes_post_sectors <- code_point_points %>%
    st_union() %>%
    st_voronoi() %>%
    st_collection_extract() %>%
    st_sf() %>%
    st_join(code_point_points, join = st_contains) |>
    st_intersection(uk_boundary)

  code_point_voronoi |>
    leaflet::leaflet() |>
      addPolygons()

  usethis::use_data(shapes_post_sectors)
}
