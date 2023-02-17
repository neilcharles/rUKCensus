ga_single_city_demogs <- function(ga_id, date_min, date_max, city){

  filter <- glue::glue("ga:city=={city}")

  ga <- google_analytics(ga_id,
                   date_range = c(date_min, date_max),
                   metrics = "sessions",
                   dimensions = c("userAgeBracket", "userGender"),
                   filtersExpression = filter,
                   max = -1)

  if(is.null(ga)) return(NA)

  ga %>%
    rename(sessions_demog = sessions) %>%
    unite(age_gender, userAgeBracket, userGender)
}


ga_detailed_demogs <- function(ga_id, date_min, date_max){
  cities_available <-
    google_analytics(ga_id,
                   date_range = c(date_min, date_max),
                   metrics = "sessions",
                   dimensions = c("city"),
                   filtersExpression = "ga:country==United Kingdom",
                   max = -1)

  cities_demogs <- cities_available[1:10,] %>%
    filter(city != "(not set)") %>%
    mutate(demogs = purrr::map(.x = city, .f = ~ga_single_city_demogs(ga_id, date_min, date_max, .x)))

  test <- cities_demogs %>%
    unnest(demogs, names_repair = 'unique') %>%
    group_by(city) %>%
    mutate(demog_pct = sessions_demog / sessions)

}
