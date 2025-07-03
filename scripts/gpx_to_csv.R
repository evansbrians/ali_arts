# Setup:

library(sf)
library(tidyverse)

# List gpx files:

list.files(
  "data/gps_bad_elf", 
  full.names = TRUE,
  pattern = "gpx$"
) %>% 
  
  # Iterate across each file:
  
  map_df(
    \(path) {
      
      # Read in the data:
      
      st_read(path, layer = "track_points") %>% 
        
        # Add coordinates as columns:
        
        bind_cols(
          st_coordinates(.)
        ) %>% 
        
        # Add file column and select columns of interest:
        
        mutate(
          file = 
            path %>% 
            str_remove("data/gps_bad_elf/") %>% 
            str_remove("\\.gpx"),
          .before = time
        ) %>% 
        select(
          file,
          time,
          long = X,
          lat = Y
        ) %>% 
        
        # Remove the geometry column and convert to tibble:
        
        st_drop_geometry() %>% 
        as_tibble()
    }
  ) %>% 
  
  # Write file:
  
  write_rds("data/gps_bad_elf/bad_elf.rds")