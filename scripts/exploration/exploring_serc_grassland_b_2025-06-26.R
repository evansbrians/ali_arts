
# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source("scripts/functions.R")

tmap_mode("view")

# get and subset data -----------------------------------------------------

stop_data <- 
  read_rds("data/processed/stop_data_serc_scbi.rds") %>% 
  filter(
    transect_id == "serc-grass-b",
    as_date(start) == "2025-06-26",
  ) %>% 
  mutate(
    direction = str_extract(stop_id, "(d|l)$"),
    light_dist =
      case_when(
        str_detect(stop_id, "light") ~ 0,
        str_detect(stop_id, "dark") ~ 100,
        .default = 
          stop_id %>% 
          str_extract("[0-9]*") %>% 
          as.numeric()
      ),
    dark_dist = abs(100 - light_dist),
    .before = start
  ) %>%
  select(!transect_id)

detections <-
  read_rds("data/processed/detections.rds") %>% 
  filter(
    transect_id == "serc-grass-b",
    as_date(time) == "2025-06-26",
  )

node_locations <-
  read_rds("data/processed/node_locations.rds") %>% 
  filter(
    str_detect(location, "serc-grass-b")
  )

bad_elf <- 
  read_rds("data/processed/bad_elf.rds") %>% 
  filter(file == "serc_grassland_b_2025-06-26")

# map of walk and nodes ---------------------------------------------------

tm_basemap("Esri.WorldImagery") +
  bad_elf %>% 
  tm_shape() +
  tm_dots() +
  node_locations %>%
  tm_shape(node_locations) +
  tm_dots(
    fill = "location",
    col = "#000000",
    size = 1
  )

# align detections and stop times -----------------------------------------

stop_rss <- 
  get_stop_rss(stop_data, detections)

# Plot stop RSS by time:

stop_rss %>% 
  ggplot() +
  aes(
    x = time,
    y = rssi,
    col = direction
  ) +
  geom_point() +
  facet_wrap(~ tag)

# Plot stop RSS by distance to light:

stop_rss %>% 
  ggplot() +
  aes(
    x = light_dist,
    y = rssi,
    col = direction
  ) +
  geom_point() +
  facet_wrap(~ tag)

# Plot stop RSS by distance to dark:

stop_rss %>% 
  ggplot() +
  aes(
    x = dark_dist,
    y = rssi,
    col = direction
  ) +
  geom_point() +
  facet_wrap(~ tag)

# with bad elf ------------------------------------------------------------

bad_elf_dist <-
  get_bad_elf_dist(bad_elf, node_locations)

# RSS of detections with distance to the light node:

detections %>% 
  inner_join(
    bad_elf_dist,
    by = "time"
  ) %>% 
  ggplot() +
  aes(
    x = light_dist,
    y = rssi,
    col = tag
  ) +
  geom_point() +
  facet_wrap(~ tag)

# RSS of detections with distance to the dark node:

detections %>% 
  inner_join(
    bad_elf_dist,
    by = "time"
  ) %>% 
  ggplot() +
  aes(
    x = dark_dist,
    y = rssi,
    col = tag
  ) +
  geom_point()  +
  facet_wrap(~ tag)

# RSS at the light node with sampling time:

detections %>% 
  inner_join(
    bad_elf_dist,
    by = "time"
  ) %>% 
  ggplot() +
  aes(
    x = time,
    y = rssi,
    col = tag
  ) +
  geom_point()  +
  facet_wrap(~ tag)

# What time did Ali turn around?

turn_around <- 
  stop_data %>% 
  filter(
    str_detect(stop_id, "^dark")
  ) %>% 
  summarize(
    start = min(start),
    end = max(end)
  )

# Does this line up with the spatial data?

tm_basemap("Esri.WorldImagery") +
  bad_elf %>% 
  tm_shape() +
  tm_dots(fill = "#dcdcdc") +
  bad_elf %>% 
  filter(
    between(
      time, 
      turn_around$start,
      turn_around$end
    )
  ) %>% 
  tm_shape() +
  tm_dots() +
  node_locations %>%
  tm_shape() +
  tm_dots(
    fill = "location",
    size = 0.75
  )


