# Make a decimal hours vector from a time ---------------------------------

decimal_hours <-
  function(.time) {
    hour(.time) +
      minute(.time)/60 +
      second(.time)/3600
  }

# subset data to a sampling event -----------------------------------------

subset_to_sampling_event <-
  function(
    date,
    bad_elf_file,
    transect_id
  ) {
    list(
      
      # Detections for a given day of sampling:
      
      focal_detections = 
        detections %>% 
        filter(
          str_detect(time, {{ date }})
        ),
      
      # Bad Elf:
      
      focal_bad_elf =
        bad_elf %>% 
        filter(file == {{ bad_elf_file }}),
      
      # Stop data:
      
      focal_stops =
        stop_data_serc_scbi %>% 
        filter(
          str_detect(start, {{ date }}),
          transect_id == {{ transect_id }}
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
        select(!transect_id),
      
      # Node locations:
      
      focal_nodes =
        node_locations %>% 
        filter(
          str_detect(location, transect_id)
        )
    )
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
              str_detect(location,  "dark$")
            )
          ) %>% 
          as.numeric(),
        light_dist = 
          st_distance(
            ., 
            filter(
              focal_nodes, 
              str_detect(location,  "light$")
            )
          ) %>% 
          as.numeric()
      ) %>% 
      
      # Make into a tibble and select columns of interest:
      
      st_drop_geometry() %>% 
      as_tibble() %>% 
      select(time, dark_dist, light_dist)
  }
