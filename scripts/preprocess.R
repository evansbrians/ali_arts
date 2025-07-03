# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

source("scripts/functions.R")

# pre-processing tag data -------------------------------------------------

tags <- 
  readxl::read_excel("data/tag_ids.xlsx") %>% 
  
  # Make a shortened tag ID to allow for partial reads:
  
  mutate(
    id = 
      str_sub(
        actual_id,
        start = 1,
        end = 3
      ),
    tag = shortened_id,
    .keep = "none"
  )

# pre-process garmin gps data ---------------------------------------------

# Need to add this from the Garmin!

node_locations <-
  tribble(
    ~ location, ~ long, ~ lat,
    "serc-grass-a-dark", -76.546727, 38.895952,
    "serc-grass-a-light", -76.5466698, 38.896888,
    "serc-grass-b-dark", -76.546887, 38.890811,
    "serc-grass-b-light", -76.547898, 38.891225
  ) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

# pre-process detections (beep_0) data ------------------------------------

detections <-
  list.files("data/detections", pattern = "beep") %>% 
  map_df(
    ~ file.path(
      "data/detections",
      .x
    ) %>% 
      read_csv() %>% 
      mutate(
        node = 
          str_extract(.x, "beep_0_[a-z]*") %>% 
          str_remove("beep_0_"),
        .before = time
      )
  ) %>% 
  filter(
    year(time) == 2025
  ) %>% 
  mutate(
    id = 
      str_sub(
        id,
        start = 1,
        end = 3
      )
  ) %>% 
  inner_join(
    tags,
    by = "id"
  ) %>% 
  select(
    time,
    tag,
    rssi
  )

# pre-process bad elf data ------------------------------------------------

bad_elf <- 
  read_rds("data/gps_bad_elf/bad_elf.rds") %>% 
  mutate(
    time = 
      time %>% 
      force_tzs("America/New_York", "UTC")
  ) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

# pre-process ali's stop and start time data ------------------------------

# SCBI and SERC:

stop_data_serc_scbi <-
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1axx2wAwiSGJcykYA3wGbj6r44el0f2BxfzxL-YVe_jw/edit?usp=sharing",
    sheet = "serc_scbi") %>% 
  janitor::clean_names() %>% 
  set_names(
    names(.) %>% 
      str_extract("^[a-z]*(_id)?")
  )  %>% 
  mutate(
    transect_id,
    stop_id,
    across(
      start:end,
      ~ as_date(date) %>% 
        str_c(
          hms::as_hms(.x),
          sep = " "
        ) %>% 
        
        # Because the hours were off on the watch:
        
        as_datetime() - 3600
    ),
    .keep = "none"
  )

# Observatory:

stop_data_observatory <-
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1axx2wAwiSGJcykYA3wGbj6r44el0f2BxfzxL-YVe_jw/edit?usp=sharing",
    sheet = "observatory") %>% 
  janitor::clean_names() %>% 
  set_names(
    names(.) %>% 
      str_extract("^[a-z]*(_id)?")
  ) %>% 
  mutate(
    stop_id,
    start = arrival,
    end = departure,
    across(
      start:end,
      ~ as_date(date) %>% 
        str_c(
          hms::as_hms(.x),
          sep = " "
        ) %>% 
        
        # Because the hours were off on the watch:
        
        as_datetime() - 3600
    ),
    .keep = "none"
  )
