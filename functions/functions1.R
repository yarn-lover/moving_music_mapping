
#1: function to load data from multiple codepoint files as one tibble into R
load_postcode_data <- function(postcode_starts){
  
  folder_path <- "data/postcodes_codepoint/codepo_gb/Data/CSV"

    # Initialize an empty list to store data frames
    combined_data <- list()
    
    # Loop through the vector of letters
    for (i in postcode_starts) {
      # Construct the filename (assuming files are named 'letter.csv')
      file_name <- paste0(folder_path, "/", i, ".csv")
      
      # Check if the file exists before reading
      if (file.exists(file_name)) {
        # Read the CSV file into a data frame
        df <- read.csv(file_name, stringsAsFactors = FALSE)
        
        # Append the data frame to the list
        combined_data <- append(combined_data, list(df))
      } else {
        warning(paste("File", file_name, "does not exist"))
      }
    }
    
    # Combine all data frames in the list by row binding them
    final_data <- do.call(rbind, combined_data)
    
    return(final_data)
  }
  

#2: function to extract longitude & latitude from individual full postcodes, and form that into a tibble

construct_full_postcode_info <- function(concert_data, postcode_starts, codepoint_data) {
  # Filter concert data for only the full postcodes that start with any of the values in postcode_starts
  relevant_full_postcodes <- concert_data %>%
    filter(sapply(postcode_full_raw, function(x) any(grepl(paste0("^", postcode_starts), x, ignore.case = TRUE))))
 
  #figure out if all postcodes in concert_data are correct
  if (all(relevant_full_postcodes[[1]] %in% codepoint_data[[1]])) {
    print("All postcodes match")
  } else {
    print("At least one postcode does not match")
  }
  
  #make a dataset containing the codepoint data for the full postcodes that we have in relevant_full_postcodes
  matching_data <- merge(relevant_full_postcodes, codepoint_data, by.x = 1, by.y = 1)

  #convert the coordinates into the right crs
  concert_coords <- matching_data 
    #Create an sf object from the easting and northing columns
  audience_coordinates_sf <- st_as_sf(matching_data, coords = c(17, 18), crs = 27700)  
  
    #Transform to CRS 4326 (WGS84)
  audience_coordinates_sf_4326 <- st_transform(audience_coordinates_sf, crs = 4326)
  
  # Extract the transformed coordinates (latitude and longitude)
  matching_data$longitude <- st_coordinates(audience_coordinates_sf_4326)[, 1]
  matching_data$latitude <- st_coordinates(audience_coordinates_sf_4326)[, 2]
  
  return(matching_data)

}

#3: plot everything: full postcode edition
plot_individual_postcodes<- function(IMD_data, postcodes_sf, concert_coords){
  #define dorchester abbey
  interest_points <- data.frame( #make a new dataframe called interest_points, which will have in it:
    Location = c("Dorchester Abbey"), #it should have a column called location, with dorchester abbey
    lat = c(51.646301), #it should have a column called lat, with these values (latitude - found on google)
    lon = c(-1.167200) #it should have a column called lon, with these values (longitude - found on google)
  )
  
  #turn into a new sf object: I don't fully understand what this means, but R needs us to do this if we want to make the dataset into a map
  interest_points_sf <- st_as_sf(interest_points, coords = c("lon", "lat"), crs = 4326)
  #crs = 4326 states these coordinates we've given it represent longitude & latitude on earth's surface

  
  
  plot_full_postcode <- ggplot()+ 
    geom_sf(data = IMD_data, aes(fill = IMD_Decile), colour = NA)+ #colour the IMD shapes based on IMD value, and don't give them a border
    scale_fill_continuous(name = "Index of Multiple Deprivation, 2019", type = "viridis", direction = -1)+ #IMD fill should be given a name; filled using the colour palette 'viridis', but so that high values are darker and low values are lighter: this is opposite to how it is usually applied, hence use direction = -1
    geom_sf(data=postcodes_sf, colour = "white", alpha=0.1)+ #add postcode sectors on top, in white outline
    
    geom_point(data =  concert_coords, aes(x = longitude, y = latitude), colour = "red", size = 1.5, pch = 19) + #add our points of interest, colouring them by the colour stated in the interest points dataset, with size 3 dots, using the data from the table we made, and getting the coordinates from the coordinates columns we made
    
    geom_sf(data = interest_points_sf, colour = "yellow", size = 1.5, pch = 19) + #add our points of interest, colouring them by the colour stated in the interest points dataset, with size 3 dots, using the data from the table we made (interest_points_sf), and getting the coordinates from the coordinates table we made
    
    
    theme_minimal()+
    
    labs(title = "Oxfordshire", x="", y="") #add a title, and take away the x and y axis names - otherwise it just says X and Y
  
  
  print(plot_full_postcode)
}


#4: full postcodes map, with placenames

plot_individual_postcodes_names<- function(IMD_data, postcodes_sf, concert_coords, bbox_coords, plot_xlim, plot_ylim){
  
  osm_data <- opq(bbox = bbox) %>%
    add_osm_feature(key = "place", value = c("town", "city")) %>%  # Filter for villages, towns, and cities
    osmdata_sf()
  
  # Filter the data to include only places with names and of the specified types
  places_with_names <- osm_data$osm_points[!is.na(osm_data$osm_points$name), ]
  
  places_with_names <- st_transform(places_with_names, crs = 4326)
  
  #define dorchester abbey
  interest_points <- data.frame( #make a new dataframe called interest_points, which will have in it:
    Location = c("Dorchester Abbey"), #it should have a column called location, with dorchester abbey
    lat = c(51.646301), #it should have a column called lat, with these values (latitude - found on google)
    lon = c(-1.167200) #it should have a column called lon, with these values (longitude - found on google)
  )
  
  #turn into a new sf object: I don't fully understand what this means, but R needs us to do this if we want to make the dataset into a map
  interest_points_sf <- st_as_sf(interest_points, coords = c("lon", "lat"), crs = 4326)
  #crs = 4326 states these coordinates we've given it represent longitude & latitude on earth's surface
  
  
  plot_full_postcode <- ggplot()+ 
    geom_sf(data = IMD_data, aes(fill = IMD_Decile), colour = NA)+ #colour the IMD shapes based on IMD value, and don't give them a border
    scale_fill_continuous(name = "Index of Multiple Deprivation, 2019", type = "viridis", direction = -1)+ #IMD fill should be given a name; filled using the colour palette 'viridis', but so that high values are darker and low values are lighter: this is opposite to how it is usually applied, hence use direction = -1
    geom_sf(data=postcodes_sf, colour = "white", alpha=0.1)+ #add postcode sectors on top, in white outline
    
    geom_label(data = places_with_names, 
               aes(x = st_coordinates(geometry)[, 1], 
                   y = st_coordinates(geometry)[, 2], 
                   label = name), 
               size = 3, nudge_y = 0.00005, nudge_x = 0.00005, 
               colour = "black", label.padding = unit(0.1, "lines"), 
               label.size = 0, alpha = 0.7) +
    
    geom_point(data =  concert_coords, aes(x = longitude, y = latitude), colour = "red", size = 1.5, pch = 19) + #add our points of interest, colouring them by the colour stated in the interest points dataset, with size 3 dots, using the data from the table we made, and getting the coordinates from the coordinates columns we made
    
    geom_sf(data = interest_points_sf, colour = "yellow", size = 1.5, pch = 19) + #add our points of interest, colouring them by the colour stated in the interest points dataset, with size 3 dots, using the data from the table we made (interest_points_sf), and getting the coordinates from the coordinates table we made
    
    
    # Adjust for the map's projection and ensure all layers fit properly
    coord_sf(xlim = plot_xlim, ylim = plot_ylim) +  # Set limits to fit Oxfordshire area
    
    theme_minimal()+
    
    labs(title = "Oxfordshire", x="", y="") #add a title, and take away the x and y axis names - otherwise it just says X and Y
  
  
  print(plot_full_postcode)
}
