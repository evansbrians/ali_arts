
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
    transect_id == "serc-forest-b",
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
    transect_id == "serc-forest-b",
    as_date(time) == "2025-06-26",
  )

node_locations <-
  read_rds("data/processed/node_locations.rds") %>% 
  filter(
    str_detect(location, "serc-forest-b")
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

