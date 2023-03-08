build_shapes <- function() {

  sf::sf_use_s2(FALSE)

  lookup_areas <- readr::read_csv("inst/extdata/lookup/area_names/PCD_OA_LSOA_MSOA_LAD_MAY21_UK_LU.csv") %>% #not in package because huge
    dplyr::mutate(post_town = stringr::str_extract(pcd7, "^\\D+")) %>%
    dplyr::group_by(oa11cd, lsoa11cd, msoa11cd, ladcd, lsoa11nm, msoa11nm, ladnm) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  usethis::use_data(lookup_areas, overwrite = TRUE)

  shapes_lsoa <-
    sf::st_read("inst/extdata/shapes/lsoa/infuse_lsoa_lyr_2011_clipped/infuse_lsoa_lyr_2011_clipped.shp") %>%
    dplyr::filter(substr(geo_code, 1,1)!='N') %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 0.0005) %>%
    dplyr::rename(geo_id = geo_code,
                  geo_name = geo_label) %>%
    dplyr::select(geo_id, geo_name, geometry) %>%
    st_make_valid()

  shapes_lad <-
    sf::st_read("inst/extdata/shapes/la/infuse_dist_lyr_2011_clipped/infuse_dist_lyr_2011_clipped.shp") %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::filter(substr(geo_code, 1,1) %in% c('E', 'W', 'S')) %>%
    dplyr::rename(geo_id = geo_code,
                  geo_name = geo_label) %>%
    dplyr::select(geo_id, geo_name, geometry) %>%
    st_make_valid()


  shapes_msoa <-
    sf::st_read("inst/extdata/shapes/msoa/infuse_msoa_lyr_2011_clipped/infuse_msoa_lyr_2011_clipped.shp") %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::filter(substr(geo_code, 1,1)!='N') %>%
    sf::st_simplify(preserveTopology = TRUE, dTolerance = 0.0005) %>%
    dplyr::rename(geo_id = geo_code,
                  geo_name = geo_label) %>%
    dplyr::select(geo_id, geo_name, geometry) %>%
    st_make_valid()

  shapes_uk_coastline <-
    sf::st_read("inst/extdata/shapes/coastline/merged_manual/merged_manual.shp") %>%
    sf::st_transform(crs = 4326) %>%
    st_make_valid()

  usethis::use_data(shapes_lsoa,
                    shapes_msoa,
                    shapes_uk_coastline, overwrite = TRUE)
}
