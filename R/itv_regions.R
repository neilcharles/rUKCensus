itv_regions <- function(){
  library(tidyverse)
  library(sf)

  sf_use_s2(FALSE)

  centroids <- shapes_lsoa |>
    group_by(geo_id) |>
    summarise(geometry = st_centroid(geometry))

  itv_macro <- st_read("inst/extdata/shapes/itv/macro_regions.shp")

  macro_match <- centroids |>
    st_join(itv_macro, st_within)

  macro_match_lookup <- macro_match |>
    st_drop_geometry() |>
    write_rds("data/lsoa_macro_match.rds")

  macro_match_lookup |>
    write_csv("C:/Users/neild/Documents/Sequence/Clients/HSL/segmentation/lsoa_macro_lookup.csv")
}
