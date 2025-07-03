
# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source("scripts/functions.R")

source("scripts/preprocess.R")

tmap_mode("view")

# Subset data to target sampling event:

subset_to_sampling_event(
  date = "2025-06-26",
  bad_elf_file = "20250626-130932-0001831-022204",
  transect_id = "serc-grass-a"
) %>% 
  list2env(.GlobalEnv)

# map of walk and nodes ---------------------------------------------------

tm_basemap("Esri.WorldImagery") +
  focal_bad_elf %>% 
  tm_shape() +
  tm_dots() +
  focal_nodes %>%
  tm_shape(focal_nodes) +
  tm_dots(
    fill = "location",
    size = 0.75
  )

# align detections and stop times -----------------------------------------

stop_rss <- 
  get_stop_rss(focal_stops, focal_detections)

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
  get_bad_elf_dist(focal_bad_elf, focal_nodes)

# RSS of detections with distance to the light node:

focal_detections %>% 
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
  geom_point()  +
  facet_wrap(~ tag)

# RSS of detections with distance to the dark node:

focal_detections %>% 
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

focal_detections %>% 
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
  focal_stops %>% 
  filter(
    str_detect(stop_id, "^dark")
  ) %>% 
  summarize(
    start = min(start),
    end = max(end)
  )
  
# Does this line up with the spatial data?

tm_basemap("Esri.WorldImagery") +
  focal_bad_elf %>% 
  tm_shape() +
  tm_dots(fill = "#dcdcdc") +
  focal_bad_elf %>% 
  filter(
    between(
      time, 
      turn_around$start,
      turn_around$end
    )
  ) %>% 
  tm_shape() +
  tm_dots() +
  focal_nodes %>%
  tm_shape(focal_nodes) +
  tm_dots(
    fill = "location",
    size = 0.75
  )
