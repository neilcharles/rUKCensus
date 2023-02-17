library(tidyverse)
library(glue)
library(sf)
library(leaflet)

var_names <- read_csv("name_match.csv") %>%
  mutate(scot_name = janitor::make_clean_names(scot_name),
         eng_wa_name = janitor::make_clean_names(eng_wa_name),
         name = janitor::make_clean_names(name)) %>%
  filter(include) %>%
  select(-include)

# Load England & Wales ---------------------------------------------------------
raw_ew <- tibble(filename = list.files(path = "england_wales", pattern = ".*.csv", full.names = FALSE)) %>%
  mutate(data = map(.x = filename, .f = ~ read_csv(glue("england_wales/{.x}"), col_types = cols(.default = "c")))) %>%
  mutate(filename = str_extract(filename, "^[A-Z][A-Z]\\d\\d\\d")) %>%
  filter(filename %in% var_names$filename)

raw_ew_long <- raw_ew %>%
  mutate(data2 = map(
    .x = data,
    .f = ~
      pivot_longer(
        janitor::clean_names(.),
      -c(date, geography, `geography_code`, `rural_urban`)
    )
  )) %>%
  select(-data) %>%
  unnest(data2) %>%
  select(-date, -geography, -rural_urban) %>%
  rename(geo_id = geography_code,
         eng_wa_name = name) %>%
  left_join(select(var_names, filename, eng_wa_name, name)) %>%
  select(-eng_wa_name)

# Load Scotland ----------------------------------------------------------------
raw_scot <- tibble(filename = list.files(path = "scotland/KS", pattern = "*.csv", full.names = FALSE)) %>%
  mutate(data = map(.x = filename, .f = ~read_csv(glue("scotland/KS/{.x}"), col_types = cols(.default = "c")) %>%
                      rename(geography_code = ...1))) %>%
  mutate(filename = str_extract(filename, "^[A-Z][A-Z]\\d\\d\\d")) %>%
  filter(filename %in% var_names$filename)

raw_scot_long <- raw_scot %>%
  mutate(data2 = map(
    .x = data,
    .f = ~
      pivot_longer(
        janitor::clean_names(.),
        -c(`geography_code`)
      )
  )) %>%
  select(-data) %>%
  unnest(data2) %>%
  rename(geo_id = geography_code,
         scot_name = name) %>%
  left_join(select(var_names, filename, scot_name, name)) %>%
  select(-scot_name)


# Merge England, Wales & Scotland
raw_all <- raw_ew_long %>%
  union_all(raw_scot_long) %>%
  filter(glue("{filename}{name}") %in% glue("{var_names$filename}{var_names$name}")) %>% #Remove unwanted vars
  select(-filename) %>%
  mutate(value = str_replace(value, "^-$", "0")) %>%
  mutate(value = str_replace_all(value, ",","")) %>%
  mutate(value = as.numeric(value))

raw_wide <- raw_all %>%
  pivot_wider(names_from = "name", values_from = "value")

# check for NA and identify problem columns
if(any(is.na(raw_wide))){

  for(i in 1:ncol(raw_wide)){
    if(any(is.na(raw_wide[,i]))) print(i)
  }

  stop("Names didn't match")
}

# Run this if output contains list cols
# test <- raw_all %>%
#   dplyr::group_by(geo_id, name) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)


#Convert to % of population except for density columns
pct_all <- raw_wide %>%
  mutate(across(!geo_id &
                  !variable_all_usual_residents &
                  !variable_density_number_of_persons_per_hectare,
                  ~./variable_all_usual_residents)) %>%
  write_csv("pct_all.csv")

pct_all <- read_csv("pct_all.csv")


# pca <- prcomp(pct_all[3:ncol(pct_all)], scale. = TRUE)

#Check how many princomps needed
# pca_var <- pca$sdev^2
# pca_var_perc <- round(pca_var/sum(pca_var) * 100, 1)
# barplot(pca_var_perc[1:50], main = "Variation Plot", xlab = "PCs", ylab = "Percentage Variance", ylim = c(0, 100))

segmentation_raw <- scale(
  pct_all[3:ncol(pct_all)]
)

# segmentation_dist <- dist(as_tibble(pca$x[,1:10]), method = "euclidean")

# fit <- kmeans(pca$x[,1:50], 12, algorithm = "MacQueen", iter.max = 1000)
set.seed(42)
fit <- kmeans(segmentation_raw, 12, algorithm = "Hartigan", iter.max = 1000)

tibble(x = fit$cluster) %>%
  group_by(x) %>%
  summarise(n = n()) %>%
  ggplot2::ggplot(aes(x = x, y = n)) +
  geom_col()

mapdata <- tibble(geo_id = pct_all$geo_id,
       cluster = fit$cluster)

colours <- tibble(col = pals::alphabet()) %>%
  mutate(cluster=row_number())


mapshapes <- rUKcensus::shapes_lsoa %>%
  left_join(mapdata) %>%
  left_join(colours)

# leaflet(filter(mapshapes, cluster==10)) %>%
leaflet(mapshapes) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~as.character(col),
              popup = ~as.character(cluster),
              weight = 0.5,
              color = "black")

mapdata %>%
  st_drop_geometry() %>%
  write_csv("clusters.csv")


# 1 Mixed town centre MCR
# 2 Students?
# 3 Nice rural yorkshire dales Dulwich
# 4 Deprived
# 5 Deprived multi-ethnic Rochdale
# 6 One tiny bit of Brighton
# 7 Milton Keynes long distance commuter belt
# 8 Only scotland?
# 9 Rural Wales -
# 10 Mixed urban
# 11
# 12 Central London
# 13
# 14 Middle class Mirfield
# 15








# Test correct number of segments (18)
# Use map_dbl to run many models with varying value of k (centers)
# tot_withinss <- map_dbl(8:20,  function(k){
#   model <- kmeans(x = segmentation_raw, centers = k, iter.max = 500)
#   model$tot.withinss
# })
# # Generate a data frame containing both k and tot_withinss
# elbow_df <- data.frame(
#   k = 8:20,
#   tot_withinss = tot_withinss
# )
#
# # Plot the elbow plot
# ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
#   geom_line() + geom_point()+
#   scale_x_continuous(breaks = 1:10)
