
# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source("scripts/functions.R")

tmap_mode("view")

# get and subset data -----------------------------------------------------

stop_data <- 
  read_rds("data/processed/stop_data_serc_scbi.rds") %>% 
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
  read_rds("data/processed/detections.rds")

node_locations <-
  read_rds("data/processed/node_locations.rds")

bad_elf <- 
  read_rds("data/processed/bad_elf.rds")