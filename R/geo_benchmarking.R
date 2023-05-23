test <- function(){
  googleAnalyticsR::ga_auth()

  account_list <- googleAnalyticsR::ga_account_list()

  all_geo <- account_list %>%
    dplyr::select(accountId, accountName, websiteUrl, viewId) %>%
    dplyr::mutate(geo = purrr::map(.x = viewId, .f = ~
                                     googleAnalyticsR::google_analytics(.x,
                                                                        date_range = c("2021-01-01", "2022-12-31"),
                                                                        metrics = "sessions",
                                                                        dimensions = c("city", "latitude", "longitude"),
                                                                        filtersExpression = "ga:country==United Kingdom",
                                                                        max = -1)
    ))

  write_rds(all_geo, 'geo_benchmarking.RDS')

  included_viewid <- all_geo %>%
    mutate(sessions = purrr::map(.x = geo, .f = ~sum(.x$sessions)),
           nrows = purrr::map(.x = geo, .f = ~length(.x$sessions))) %>%
    select(-geo) %>%
    mutate(across(everything(), unlist)) %>%
    group_by(websiteUrl) %>%
    arrange(-sessions) %>%
    mutate(n = row_number()) %>%
    filter(n==1) %>%
    select(-n) %>%
    filter(nrows >=800) %>%
    pull(viewId)

  ga_latlong_benchmark <- all_geo %>%
    unnest(geo) %>%
    filter(viewId %in% included_viewid) %>%
    group_by(viewId) %>%
    mutate(sessions_pct = sessions / sum(sessions)) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude)) %>%
    filter(!(latitude==0 & longitude==0)) %>%
    group_by(latitude, longitude) %>%
    summarise(sessions_pct = mean(sessions_pct, na.rm = TRUE))

  usethis::use_data(ga_latlong_benchmark)
}
