# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

rm(
  list = ls()
)

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

# Write to file:

write_rds(tags, "data/processed/tags.rds")

# pre-process garmin gps data ---------------------------------------------

# Need to add this from the Garmin!

node_locations <-
  tribble(
    ~ location, ~ long, ~ lat,
    "serc-grass-a-dark", -76.546727, 38.895952,
    "serc-grass-a-light", -76.5466698, 38.896888,
    "serc-grass-b-dark", -76.546887, 38.890811,
    "serc-grass-b-light", -76.547898, 38.891225,
    "serc-forest-a-light", -76.54487, 38.886876,
    "serc-forest-a-dark", -76.544083, 38.886312,
    "scbi-grass-a-dark", -78.16202, 38.89520,
    "scbi-grass-a-light", -78.16101, 38.896888,
    "scbi-grass-b-dark", -78.16561, 38.89600,
    "scbi-grass-b-light", -78.16643, 38.89540,
    "scbi-forest-a-light", -78.15948, 38.89253,
    "scbi-forest-a-dark", -78.16055, 38.89284,
    "sewage", -77.05176, 38.92860,
    "waterfall", -77.05095, 38.92834,
    "kori", -77.05099, 38.92899,
    "den", -77.05160, 38.92822,
    "fence", -77.05061, 38.92796
  ) %>% 
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326
  )

# Write to file:

write_rds(node_locations, "data/processed/node_locations.rds")

# pre-process bad elf data ------------------------------------------------

source("scripts/gpx_to_csv.R")

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

# Write to file:

write_rds(bad_elf, "data/processed/bad_elf.rds")

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
        
        as_datetime()
    ),
    .keep = "none"
  )

# Write to file:

write_rds(stop_data_serc_scbi, "data/processed/stop_data_serc_scbi.rds")

write_rds(stop_data_observatory, "data/processed/stop_data_observatory.rds")

# pre-process detections (beep_0) data ------------------------------------

# Read and start detection processing:

detections_start <-
  list.files("data/detections", pattern = "beep") %>% 
  map_df(
    ~ file.path(
      "data/detections",
      .x
    ) %>% 
      read_csv() %>% 
      mutate(
        node = 
          str_extract(.x, "beep_0_[a-z]*(_the_seq)?") %>% 
          str_remove("beep_0_"),
        .before = time
      )
  ) %>% 
  
  # Get shortened tag_id:
  
  mutate(
    id = 
      str_sub(
        id,
        start = 1,
        end = 3
      )
  ) %>% 
  
  # Get tags detected:
  
  inner_join(
    tags,
    by = "id"
  ) %>% 
  
  # Subset to columns of interest:
  
  select(
    node,
    time,
    tag,
    rssi
  )

# Make a data frame of repaired times:

detections_pre_independence <- 
  detections_start %>% 
  
  # Subset to detections before 4 July:
  
  filter(time <= "2025-07-04") %>% 
  
  # Bring in Tara-calculated node time differences:
  
  left_join(
    googlesheets4::read_sheet(
      "https://docs.google.com/spreadsheets/d/1axx2wAwiSGJcykYA3wGbj6r44el0f2BxfzxL-YVe_jw/edit?usp=sharing",
      sheet = "repair_times"
    ),
    by = "node"
  ) %>% 
  
  # Repair times:
  
  mutate(time = time + time_diff) %>% 
  
  # Subset to days on or after 13 June:
  
  filter(
    as_date(time) >= "2025-06-13"
  )

# Combine repaired times and good times:

detections_combined <- 
  bind_rows(
    detections_pre_independence,
    detections_start %>% 
      filter(time > "2025-07-04")
  )

# Get Ali-recorded stop data across sites:

transect_stops_summary <- 
  stop_data_observatory %>% 
  filter(
    as_date(start) != "2025-06-13"
  ) %>% 
  mutate(
    transect_id = "observatory",
    .before = stop_id
  ) %>% 
  bind_rows(stop_data_serc_scbi) %>% 
  drop_na(start) %>% 
  mutate(
    date = as_date(start),
    .before = start
  ) %>% 
  summarize(
    start = min(start),
    end = max(end),
    .by = c(transect_id, date)
  ) %>% 
  arrange(start)

# Subset detections to sampling events and bring in the transect that was
# being sampled:

detections_by_sampling_event <- 
  1:nrow(transect_stops_summary) %>% 
  map_df(
    \(i) {
      stop_subset <-
        transect_stops_summary %>% 
        slice(i)
      
      detections_subset <- 
        detections_combined %>% 
        filter(
          between(
            time,
            stop_subset$start,
            stop_subset$end
          )
        ) %>% 
        mutate(
          transect_id = stop_subset$transect_id,
          .before = node
        )
      
      # Subset to the appropriate nodes for a given site:
      
      if(
        nrow(detections_subset) > 0 &&
        unique(detections_subset$transect_id) == "observatory"
      ) {
        detections_subset %>% 
          filter(
            !str_detect(node, "light|dark")
          )
      } else {
        detections_subset %>% 
          filter(
            str_detect(node, "light|dark")
          )
      }
    }
  )

# Write to file:

write_rds(detections_by_sampling_event, "data/processed/detections.rds")

