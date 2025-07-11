
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
  )

detections <-
  read_rds("data/processed/detections.rds") %>% 
  mutate(
    tag_height = 
      if_else(
        str_detect(tag, "2"),
        "top",
        "bottom"
      )
  )

1:nrow(stop_data) %>% 
  map_df(
    function(i) {
      times <- 
        stop_data %>% 
        slice(i)
      
      detections %>% 
        filter(
          between(
            time, 
            times$start,
            times$end
          )
        ) %>% 
        mutate(
          stop_id = times$stop_id,
          tag_position = 
            case_when(
              str_detect(stop_id, "d$") & 
                str_detect(tag, "A$") ~ "light",
              str_detect(stop_id, "d$") &
                str_detect(tag, "C$") ~ "dark",
              str_detect(stop_id, "l$") & 
                str_detect(tag, "D$") ~ "light",
              str_detect(stop_id, "l$") &
                str_detect(tag, "B$") ~ "dark",
              .default = "perp"
            )
        )
    }
  )

node_locations <-
  read_rds("data/processed/node_locations.rds")

bad_elf <- 
  read_rds("data/processed/bad_elf.rds")