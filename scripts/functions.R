# Make a decimal hours vector from a time ---------------------------------

decimal_hours <-
  function(.time) {
    hour(.time) +
      minute(.time) / 60 +
      second(.time) / 3600
  }

# align ali-reported stops and detections ---------------------------------

get_stop_rss <-
  function(focal_stops, focal_detections) {
    focal_stops %>% 
      pull(stop_id) %>% 
      map_df(
        \(stop_id) {
          
          # Subset stop data to a given stop_id:
          
          stop_subset <- 
            focal_stops %>% 
            filter(stop_id == {{ stop_id }})
          
          # Subset detections to the stop:
          
          focal_detections %>% 
            select(!transect_id) %>% 
            filter(
              between(
                time,
                stop_subset$start,
                stop_subset$end
              )
            ) %>% 
            bind_cols(stop_subset, .)
        }
      )
  }

# align bad elf data and detections ---------------------------------------

get_bad_elf_dist <-
  function(focal_bad_elf, focal_nodes) {
    
    # Calculate distances between bad elf points and nodes:
    
    focal_bad_elf %>% 
      mutate(
        dark_dist = 
          st_distance(
            ., 
            filter(
              focal_nodes, 
              str_detect(location, "dark$")
            )
          ) %>% 
          as.numeric(),
        light_dist = 
          st_distance(
            ., 
            filter(
              focal_nodes, 
              str_detect(location, "light$")
            )
          ) %>% 
          as.numeric()
      ) %>% 
      
      # Make into a tibble and select columns of interest:
      
      st_drop_geometry() %>% 
      as_tibble() %>% 
      select(
        time, 
        dark_dist, 
        light_dist
      )
  }
