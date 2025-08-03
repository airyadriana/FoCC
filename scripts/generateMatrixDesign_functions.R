#################
## Description ##
#################
# Author: Airy Peralta
# Date: August 2nd, 2025

# Functions for the Generate Matrix design

###############
## Functions ##
###############

#--Get the file format of the plot area
get_file_format <- function() {
  cat("What format is your garden perimeter file?\n")
  cat("1. KML file\n")
  cat("2. ArcGIS Shapefile\n")
  cat("3. Other format\n")
  
  choice <- readline(prompt = "Enter your choice (1, 2, or 3): ")
  
  format_map <- c("1" = "kml", "2" = "shapefile", "3" = "other")
  
  if (choice %in% names(format_map)) {
    return(format_map[choice])
  } else {
    cat("Invalid choice. Please try again.\n")
    return(get_file_format())  # Recursive call for invalid input
  }
}


#--Load a kml
select_kml_file <- function() {
  cat("Please select your KML file...\n")
  
  # Open file dialog
  file_path <- file.choose()
  
  # Check if file has .kml extension
  if (!grepl("\\.kml$", file_path, ignore.case = TRUE)) {
    warning("Selected file doesn't appear to be a KML file")
  }
  
  
  cat("Transforming to Colorado State Plane Central (US Survey Feet)...\n")
  
  # EPSG:2232 - NAD83 / Colorado Central (ftUS)
  garden_area <- sf::st_read(file_path, quiet = T)
  garden_feet <- st_transform(garden_area, crs = 2232)
  return(garden_feet)
}


#--Get the plant spacing
get_spacing_in_state_plane <- function(garden_area) {
  
  # Transform to feet-based coordinate system
  garden_feet <- transform_to_feet(garden_area)
  garden_2d <- st_zm(garden_feet)
  
  # Area will now be in square feet
  area_sqft <- as.numeric(st_area(garden_2d))
  area_sqm <- area_sqft / 10.764
  
  cat("\n=== Plant Spacing Configuration ===\n")
  cat("Garden area:", round(area_sqft, 0), "square feet")
  cat(" (", round(area_sqm, 0), "square meters)\n")
  
  spacing_feet <- readline(prompt = "How far apart will you be planting each plant? (in feet): ")
  spacing_feet <- as.numeric(spacing_feet)
  
  if (is.na(spacing_feet) || spacing_feet <= 0) {
    cat("Please enter a valid positive number.\n")
    return(get_spacing_in_state_plane(garden_area))
  }
  
  # Calculate density (plants per square foot)
  plants_per_sqft <- 1 / (spacing_feet^2)
  total_plants <- plants_per_sqft * area_sqft
  
  cat("With", spacing_feet, "foot spacing:\n")
  cat("- Plant density:", round(plants_per_sqft, 4), "plants per square foot\n")
  cat("- Estimated total plants needed (assuming that only plants that expand 1 foot are used):", round(total_plants, 0), "\n")
  
  return(list(
    garden_feet = garden_feet,
    spacing_feet = spacing_feet,
    total_plants = round(total_plants, 0)
  ))
}


#-- Get spacing and create a grid
get_spacing_and_create_grid <- function(garden_area) {
  
  # Since garden_area is already in State Plane feet, no transformation needed
  garden_2d <- st_zm(garden_area)
  
  # Area is already in square feet
  area_sqft <- as.numeric(st_area(garden_2d))
  area_sqm <- area_sqft / 10.764
  
  cat("\n=== Plant Spacing Configuration ===\n")
  cat("Garden area:", round(area_sqft, 0), "square feet")
  cat(" (", round(area_sqm, 0), "square meters)\n")
  
  spacing_feet <- readline(prompt = "How far apart will you be planting each plant? (in feet): ")
  spacing_feet <- as.numeric(spacing_feet)
  
  if (is.na(spacing_feet) || spacing_feet <= 0) {
    cat("Please enter a valid positive number.\n")
    return(get_spacing_and_create_grid(garden_area))
  }
  
  # Calculate density (plants per square foot)
  plants_per_sqft <- 1 / (spacing_feet^2)
  total_plants <- plants_per_sqft * area_sqft
  
  cat("With", spacing_feet, "foot spacing:\n")
  cat("- Plant density:", round(plants_per_sqft, 4), "plants per square foot\n")
  cat("- Estimated total plants needed:", round(total_plants, 0), "\n")
  
  # Create the planting grid
  cat("\nCreating planting grid...\n")
  planting_grid <- create_plantGrid(garden_2d, spacing_feet)
  
  cat("Grid created with", nrow(planting_grid), "planting points\n")
  
  return(list(
    garden_area = garden_2d,
    spacing_feet = spacing_feet,
    total_plants_estimated = round(total_plants, 0),
    actual_plant_points = nrow(planting_grid),
    planting_grid = planting_grid
  ))
}

#--Function to create the actual planting grid
# Your original fast function
create_plantGrid <- function(garden_area, spacing_feet) {
  
  cat("Creating", spacing_feet, "x", spacing_feet, "foot squares...\n")
  
  # Create grid using st_make_grid (built-in function - FAST!)
  plantGrid <- st_make_grid(
    garden_area, 
    cellsize = spacing_feet,
    what = "polygons",
    square = TRUE
  )
  
  # Convert to sf object with attributes
  grid_sf <- st_sf(
    square_id = 1:length(plantGrid),
    geometry = plantGrid
  )
  
  # Intersect with garden boundary
  squares_in_garden <- suppressWarnings(st_intersection(grid_sf, garden_area))
  
  # Add area calculations
  squares_in_garden$area_sqft <- as.numeric(st_area(squares_in_garden))
  squares_in_garden$is_full_square <- squares_in_garden$area_sqft >= (spacing_feet^2 * 0.99)
  
  cat("Created", nrow(squares_in_garden), "grid squares\n")
  cat("Full squares:", sum(squares_in_garden$is_full_square), "\n")
  cat("Partial edge squares:", sum(!squares_in_garden$is_full_square), "\n")
  
  return(squares_in_garden)
}


#--Get the plant list from Google Sheets
get_plant_database_url <- function() {
  cat("\n=== Plant Database Setup ===\n")
  cat("Please provide the URL to your Google Sheets plant database.\n")
  cat("Example: https://docs.google.com/spreadsheets/d/1ABC123.../edit#gid=0\n\n")
  
  sheet_url <- readline(prompt = "Enter Google Sheets URL: ")
  
  # Basic validation
  if (sheet_url == "" || !grepl("docs.google.com/spreadsheets", sheet_url)) {
    cat("Invalid URL. Please make sure it's a Google Sheets link.\n")
    return(get_plant_database_url())  # Try again
  }
  
  cat("URL received. Loading plant database...\n")
  return(sheet_url)
}


#--Rotate grid
rotate_grid <- function(plantGrid, 
                        garden_area, 
                        angle_degrees = 45,
                        spacing_feet) {
  
  cat("Rotating grid by", angle_degrees, "degrees...\n")
  
  # Convert degrees to radians
  angle_rad <- angle_degrees * pi / 180
  
  # Get centroid coordinates
  centroid <- st_centroid(garden_area)
  centroid_coords <- st_coordinates(centroid)
  cx <- centroid_coords[1]
  cy <- centroid_coords[2]
  
  # Function to rotate a single polygon
  rotate_polygon <- function(geom) {
    coords <- st_coordinates(geom)[,1:2]
    
    # Rotate each point around centroid
    rotated_coords <- matrix(nrow = nrow(coords), ncol = 2)
    for(i in 1:nrow(coords)) {
      # Translate to origin
      x <- coords[i,1] - cx
      y <- coords[i,2] - cy
      
      # Rotate
      new_x <- x * cos(angle_rad) - y * sin(angle_rad)
      new_y <- x * sin(angle_rad) + y * cos(angle_rad)
      
      # Translate back
      rotated_coords[i,1] <- new_x + cx
      rotated_coords[i,2] <- new_y + cy
    }
    
    return(st_polygon(list(rotated_coords)))
  }
  
  # Apply rotation to all squares
  rotated_geoms <- st_sfc(lapply(st_geometry(plantGrid), rotate_polygon), 
                          crs = st_crs(plantGrid))
  
  # Create new sf object
  rotated_grid <- st_sf(
    square_id = 1:length(rotated_geoms),
    geometry = rotated_geoms
  )
  
  # Intersect with garden boundary
  final_grid <- suppressWarnings(st_intersection(rotated_grid, garden_area))
  
  # Add area calculations
  final_grid$area_sqft <- as.numeric(st_area(final_grid))
  final_grid$is_full_square <- final_grid$area_sqft >= (spacing_feet^2 * 0.99)
  
  cat("Rotated grid created with", nrow(final_grid), "squares\n")
  
  return(final_grid)
}

#--Function to load plant database from Google Sheets
load_plant_database <- function(sheet_url) {
  
  cat("Loading plant database from Google Sheets...\n")
  
  tryCatch({
    
    # Convert Google Sheets URL to CSV export URL
    if (grepl("/edit", sheet_url)) {
      csv_url <- gsub("/edit.*", "/export?format=csv&gid=0", sheet_url)
    } else {
      csv_url <- sheet_url  # Assume it's already a CSV URL
    }
    
    cat("Accessing:", csv_url, "\n")
    
    # Load the data
    plant_db <- read.csv(csv_url, stringsAsFactors = FALSE)
    
    cat("✓ Plant database loaded successfully!\n")
    cat("  Records:", nrow(plant_db), "plants\n")
    cat("  Columns:", paste(names(plant_db), collapse = ", "), "\n")
    
    # Basic validation
    if (nrow(plant_db) == 0) {
      warning("Plant database appears to be empty")
      return(NULL)
    }
    
    # Show first few plants
    cat("\nFirst few plants:\n")
    if ("plant_name" %in% names(plant_db)) {
      cat(paste(head(plant_db$plant_name, 3), collapse = ", "), "...\n")
    } else {
      cat("Preview:", paste(head(plant_db[,1], 3), collapse = ", "), "...\n")
    }
    
    return(plant_db)
    
  }, error = function(e) {
    cat("✗ Error loading plant database:", e$message, "\n")
    cat("\nTroubleshooting tips:\n")
    cat("- Make sure the Google Sheet is shared publicly\n")
    cat("- Check that the URL is complete\n")
    cat("- Verify you have internet connection\n")
    return(NULL)
  })
}


#--Simple mathematical checkerboard pattern - modified for your parameter
assign_true_checkerboard <- function(plantGrid, plant_database) {
  
  grass_plants <- plant_database[plant_database$Vegetation.type == "Grass", ]
  
  # Use the passed plantGrid directly
  grid_squares <- plantGrid
  
  # Add coordinates
  coords <- st_coordinates(st_centroid(grid_squares))
  grid_squares$x_center <- coords[,1]
  grid_squares$y_center <- coords[,2]
  
  # Convert coordinates to grid indices
  min_x <- min(grid_squares$x_center)
  min_y <- min(grid_squares$y_center)
  
  # Estimate spacing from the grid (since we don't have spacing_result here)
  unique_x <- sort(unique(grid_squares$x_center))
  spacing_feet <- min(diff(unique_x)[diff(unique_x) > 0])
  
  # Calculate grid row and column for each square
  grid_squares$grid_col <- round((grid_squares$x_center - min_x) / spacing_feet)
  grid_squares$grid_row <- round((grid_squares$y_center - min_y) / spacing_feet)
  
  # Create checkerboard pattern: grass where (row + col) is even
  grid_squares$is_grass_spot <- (grid_squares$grid_row + grid_squares$grid_col) %% 2 == 0
  
  # Initialize
  grid_squares$plant_type <- "Empty_for_Forbs"
  grid_squares$plant_name <- "Reserved for Forbs"
  grid_squares$common_name <- "Forb Space"
  
  # Assign grass to checkerboard positions
  grass_positions <- which(grid_squares$is_grass_spot)
  
  for (i in 1:length(grass_positions)) {
    pos <- grass_positions[i]
    grass_index <- ((i - 1) %% nrow(grass_plants)) + 1
    
    grid_squares$plant_type[pos] <- "Grass"
    grid_squares$plant_name[pos] <- grass_plants$Botanical.Name[grass_index]
    grid_squares$common_name[pos] <- grass_plants$Common.Name[grass_index]
  }
  
  cat("Created true checkerboard pattern:\n")
  cat("- Grass spots:", sum(grid_squares$is_grass_spot), "\n")
  cat("- Forb spots:", sum(!grid_squares$is_grass_spot), "\n")
  
  return(grid_squares)
}


#--Plot showing different grass species with colors
plot_grass_species <- function(planted_grid) {
  
  # Get unique grass species
  grass_species <- unique(planted_grid$common_name[planted_grid$plant_type == "Grass"])
  
  cat("Grass species found:\n")
  for(i in 1:length(grass_species)) {
    cat(i, ":", grass_species[i], "\n")
  }
  
  # Create color palette for grass species
  grass_colors <- rainbow(length(grass_species))
  names(grass_colors) <- grass_species
  
  # Add forb color
  all_colors <- c(grass_colors, "Forb Space" = "lightgray")
  
  # Create color vector for all squares
  plot_colors <- ifelse(planted_grid$plant_type == "Grass", 
                        grass_colors[planted_grid$common_name],
                        "lightgray")
  
  # Plot
  plot(planted_grid$geometry, 
       col = plot_colors,
       main = "Grass Species Distribution",
       border = "black", 
       lwd = 0.2)
  
  # Add legend
  legend("topright", 
         legend = c(grass_species, "Forb Space"), 
         fill = c(grass_colors, "lightgray"),
         cex = 0.7)
  
  # Print species count
  cat("\nSpecies count:\n")
  species_count <- table(planted_grid$common_name[planted_grid$plant_type == "Grass"])
  print(species_count)
}


#--Function to export purchase list
export_purchase_list <- function(planted_grid, filename = "plant_purchase_list.csv") {
  
  purchase_data <- attr(planted_grid, "purchase_summary")
  
  if(is.null(purchase_data)) {
    cat("No purchase summary found. Run place_shrubs_and_summary() first.\n")
    return()
  }
  
  # Create data frame for export
  grass_df <- data.frame(
    Type = "Grass",
    Species = names(purchase_data$grasses),
    Quantity = unlist(purchase_data$grasses),
    stringsAsFactors = FALSE
  )
  
  shrub_df <- data.frame(
    Type = "Shrub", 
    Species = gsub("Shrub: ", "", names(purchase_data$shrubs)),
    Quantity = unlist(purchase_data$shrubs),
    stringsAsFactors = FALSE
  )
  
  purchase_df <- rbind(grass_df, shrub_df)
  
  write.csv(purchase_df, filename, row.names = FALSE)
  cat("Purchase list exported to:", filename, "\n")
}


#--Function to prompt user for shrub spacing in feet
get_shrub_spacing <- function() {
  cat("\n=== Shrub Spacing Configuration ===\n")
  cat("Shrubs will be placed so they are:\n")
  cat("1. Completely surrounded by grass species\n")
  cat("2. At least X feet apart from each other\n\n")
  
  spacing <- readline(prompt = "Enter minimum distance between shrubs (in feet): ")
  spacing_num <- as.numeric(spacing)
  
  if(is.na(spacing_num) || spacing_num <= 0) {
    cat("Invalid input. Using default 10 feet.\n")
    return(10)
  }
  
  cat("Shrub spacing set to", spacing_num, "feet\n")
  return(spacing_num)
}

#--Modified shrub placement function to work in feet
place_shrubs_center_simple <- function(planted_grid, plant_database, min_shrub_distance_feet = 10) {
  
  shrub_plants <- plant_database[plant_database$Vegetation.type == "Shrub", ]
  
  if(nrow(shrub_plants) == 0) {
    cat("No shrub species found in database\n")
    return(planted_grid)
  }
  
  cat("Found", nrow(shrub_plants), "shrub species\n")
  cat("Minimum shrub spacing:", min_shrub_distance_feet, "feet\n")
  
  # Get coordinates
  coords <- suppressWarnings(st_coordinates(st_centroid(planted_grid)))
  planted_grid$x_center <- coords[,1]
  planted_grid$y_center <- coords[,2]
  
  # Find center of garden
  center_x <- mean(coords[,1])
  center_y <- mean(coords[,2])
  
  cat("Garden center at:", round(center_x, 1), ",", round(center_y, 1), "\n")
  
  # Find forb spaces in center area (within 30% of garden from center)
  all_distances <- sqrt((coords[,1] - center_x)^2 + (coords[,2] - center_y)^2)
  max_distance_from_center <- quantile(all_distances, 0.7)  # Stay within 70% from edge
  
  cat("Limiting shrub placement to within", round(max_distance_from_center, 1), "feet of center\n")
  
  # Get forb spaces in center area only
  center_forb_spaces <- which(
    planted_grid$plant_type == "Empty_for_Forbs" &
      sqrt((planted_grid$x_center - center_x)^2 + (planted_grid$y_center - center_y)^2) <= max_distance_from_center
  )
  
  cat("Found", length(center_forb_spaces), "forb spaces in center area\n")
  
  if(length(center_forb_spaces) == 0) {
    cat("No forb spaces available in center area\n")
    return(planted_grid)
  }
  
  # Sort center forb spaces by distance from center (closest first)
  center_coords <- coords[center_forb_spaces, , drop = FALSE]
  distances_from_center <- sqrt((center_coords[,1] - center_x)^2 + (center_coords[,2] - center_y)^2)
  sorted_center_indices <- center_forb_spaces[order(distances_from_center)]
  
  # Function to check minimum distance from existing shrubs
  check_distance_from_shrubs <- function(candidate_idx, placed_shrub_indices) {
    if(length(placed_shrub_indices) == 0) return(TRUE)
    
    candidate_coords <- c(planted_grid$x_center[candidate_idx], planted_grid$y_center[candidate_idx])
    
    for(shrub_idx in placed_shrub_indices) {
      shrub_coords <- c(planted_grid$x_center[shrub_idx], planted_grid$y_center[shrub_idx])
      distance <- sqrt((candidate_coords[1] - shrub_coords[1])^2 + (candidate_coords[2] - shrub_coords[2])^2)
      
      if(distance < min_shrub_distance_feet) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
  
  placed_shrubs <- c()
  
  cat("\n=== PLACING SHRUBS IN CENTER ===\n")
  
  # Place each shrub
  for(s in 1:nrow(shrub_plants)) {
    shrub_name <- shrub_plants$Common.Name[s]
    cat("Placing shrub", s, ":", shrub_name, "\n")
    
    placed <- FALSE
    
    # Try each center location
    for(candidate_idx in sorted_center_indices) {
      
      # Skip if already occupied
      if(planted_grid$plant_type[candidate_idx] != "Empty_for_Forbs") next
      
      # Check distance from other shrubs
      if(check_distance_from_shrubs(candidate_idx, placed_shrubs)) {
        
        # Place the shrub
        planted_grid$plant_type[candidate_idx] <- "Shrub"
        planted_grid$plant_name[candidate_idx] <- shrub_plants$Botanical.Name[s]
        planted_grid$common_name[candidate_idx] <- paste("Shrub:", shrub_name)
        
        placed_shrubs <- c(placed_shrubs, candidate_idx)
        
        # Calculate distance from center
        distance_from_center <- sqrt((planted_grid$x_center[candidate_idx] - center_x)^2 + 
                                       (planted_grid$y_center[candidate_idx] - center_y)^2)
        
        cat("  ✓ Placed at square", planted_grid$square_id[candidate_idx], 
            "(", round(distance_from_center, 1), "feet from center)\n")
        
        placed <- TRUE
        break
      }
    }
    
    if(!placed) {
      cat("  ✗ Could not place", shrub_name, "- insufficient space with", min_shrub_distance_feet, "foot spacing\n")
    }
  }
  
  cat("\nSuccessfully placed", length(placed_shrubs), "out of", nrow(shrub_plants), "shrubs\n")
  
  # Generate summary
  generate_planting_summary(planted_grid)
  
  return(planted_grid)
}

#--Function to create a clean plot with all species
plot_final_matrix_garden <- function(final_grid) {
  
  # Get all unique species
  grass_species <- unique(final_grid$common_name[final_grid$plant_type == "Grass"])
  shrub_species <- unique(final_grid$common_name[final_grid$plant_type == "Shrub"])
  
  # Clean shrub names (remove "Shrub: " prefix)
  shrub_species_clean <- gsub("Shrub: ", "", shrub_species)
  
  # Create distinct color palettes
  grass_colors <- c("blue", "red", "green", "purple")
  shrub_colors <- c("darkred", "darkgreen")  # Darker colors for shrubs
  forb_color <- "lightgray"
  
  # Create named color vector
  all_species <- c(grass_species, shrub_species, "Forb Space")
  all_colors <- c(grass_colors[1:length(grass_species)], 
                  shrub_colors[1:length(shrub_species)], 
                  forb_color)
  names(all_colors) <- all_species
  
  # Create plot colors for each square
  plot_colors <- character(nrow(final_grid))
  
  for(i in 1:nrow(final_grid)) {
    species_name <- final_grid$common_name[i]
    if(species_name %in% names(all_colors)) {
      plot_colors[i] <- all_colors[species_name]
    } else {
      plot_colors[i] <- "white"  # fallback
    }
  }
  
  # Create the plot
  plot(final_grid$geometry, 
       col = plot_colors,
       main = "Matrix Garden with Shrubs",
       border = "white", 
       lwd = 0.1)
  
  # Create clean legend labels
  legend_labels <- c(grass_species, shrub_species_clean, "Forb Space")
  legend_colors <- c(grass_colors[1:length(grass_species)], 
                     shrub_colors[1:length(shrub_species)], 
                     forb_color)
  
  # Add legend
  legend("topright", 
         legend = legend_labels, 
         fill = legend_colors,
         cex = 0.8,
         bg = "white",
         box.lwd = 1)
  
  # Print summary
  cat("\n=== PLOT SUMMARY ===\n")
  cat("Grass species:", length(grass_species), "\n")
  cat("Shrub species:", length(shrub_species), "\n")
  
  for(species in grass_species) {
    count <- sum(final_grid$common_name == species)
    cat(" -", species, ":", count, "plants\n")
  }
  
  for(species in shrub_species) {
    count <- sum(final_grid$common_name == species)
    clean_name <- gsub("Shrub: ", "", species)
    cat(" -", clean_name, ":", count, "plants\n")
  }
  
  forb_count <- sum(final_grid$plant_type == "Empty_for_Forbs")
  cat(" - Forb spaces:", forb_count, "squares\n")
}

#--Alternative version with better colors
plot_matrix_garden_enhanced <- function(final_grid) {
  
  library(RColorBrewer)
  
  # Get species
  grass_species <- unique(final_grid$common_name[final_grid$plant_type == "Grass"])
  shrub_species <- unique(final_grid$common_name[final_grid$plant_type == "Shrub"])
  
  # Use ColorBrewer for better colors
  if(length(grass_species) <= 4) {
    grass_colors <- brewer.pal(max(3, length(grass_species)), "Set1")[1:length(grass_species)]
  } else {
    grass_colors <- rainbow(length(grass_species))
  }
  
  if(length(shrub_species) <= 2) {
    shrub_colors <- c("darkred", "darkgreen")[1:length(shrub_species)]
  } else {
    shrub_colors <- brewer.pal(max(3, length(shrub_species)), "Dark2")[1:length(shrub_species)]
  }
  
  # Create plot
  plot_colors <- rep("lightgray", nrow(final_grid))
  
  # Assign grass colors
  for(i in 1:length(grass_species)) {
    plot_colors[final_grid$common_name == grass_species[i]] <- grass_colors[i]
  }
  
  # Assign shrub colors
  for(i in 1:length(shrub_species)) {
    plot_colors[final_grid$common_name == shrub_species[i]] <- shrub_colors[i]
  }
  
  # Plot
  plot(final_grid$geometry, 
       col = plot_colors,
       main = "Matrix Garden Design",
       border = "white", 
       lwd = 0.1)
  
  # Clean legend
  legend_labels <- c(grass_species, gsub("Shrub: ", "", shrub_species), "Forb Space")
  legend_colors <- c(grass_colors, shrub_colors, "lightgray")
  
  legend("topright", 
         legend = legend_labels, 
         fill = legend_colors,
         cex = 0.7,
         bg = "white")
}

#--Function to calculate balanced forb requirements
calculate_forb_requirements <- function(final_grid, plant_database) {
  
  # Count available forb spaces
  total_forb_spaces <- sum(final_grid$plant_type == "Empty_for_Forbs")
  cat("=== FORB PLANTING CALCULATION ===\n")
  cat("Total forb spaces available:", total_forb_spaces, "\n\n")
  
  # Get all forb species with tray sizes
  forb_plants <- plant_database[plant_database$Vegetation.type == "Forbs", ]
  
  if(nrow(forb_plants) == 0) {
    cat("No forb species found in database\n")
    return(NULL)
  }
  
  cat("Available forb species:\n")
  for(i in 1:nrow(forb_plants)) {
    cat(i, ":", forb_plants$Common.Name[i], "- Tray size:", forb_plants$Tray.size[i], "plants\n")
  }
  
  num_species <- nrow(forb_plants)
  plants_per_species <- floor(total_forb_spaces / num_species)
  
  cat("\nBalanced distribution:\n")
  cat("Plants per species (equal distribution):", plants_per_species, "\n")
  
  # Calculate tray requirements for each species
  forb_requirements <- data.frame(
    Species = forb_plants$Common.Name,
    Botanical_Name = forb_plants$Botanical.Name,
    Tray_Size = forb_plants$Tray.size,
    Plants_Needed = plants_per_species,
    Trays_Needed = ceiling(plants_per_species / forb_plants$Tray.size),
    Plants_Purchased = ceiling(plants_per_species / forb_plants$Tray.size) * forb_plants$Tray.size,
    Extras = (ceiling(plants_per_species / forb_plants$Tray.size) * forb_plants$Tray.size) - plants_per_species,
    stringsAsFactors = FALSE
  )
  
  cat("\n=== FORB PURCHASE REQUIREMENTS ===\n")
  total_trays <- 0
  total_plants_purchased <- 0
  total_extras <- 0
  
  for(i in 1:nrow(forb_requirements)) {
    cat("\n", forb_requirements$Species[i], ":\n")
    cat("  Tray size:", forb_requirements$Tray_Size[i], "plants per tray\n")
    cat("  Plants needed:", forb_requirements$Plants_Needed[i], "\n")
    cat("  Trays to buy:", forb_requirements$Trays_Needed[i], "\n")
    cat("  Total plants purchased:", forb_requirements$Plants_Purchased[i], "\n")
    cat("  Extra plants:", forb_requirements$Extras[i], "\n")
    
    total_trays <- total_trays + forb_requirements$Trays_Needed[i]
    total_plants_purchased <- total_plants_purchased + forb_requirements$Plants_Purchased[i]
    total_extras <- total_extras + forb_requirements$Extras[i]
  }
  
  # Calculate remaining spaces after balanced distribution
  plants_used <- sum(forb_requirements$Plants_Needed)
  remaining_spaces <- total_forb_spaces - plants_used
  
  cat("\n=== SUMMARY ===\n")
  cat("Total forb spaces:", total_forb_spaces, "\n")
  cat("Plants used (balanced):", plants_used, "\n")
  cat("Remaining spaces:", remaining_spaces, "\n")
  cat("Total trays to purchase:", total_trays, "\n")
  cat("Total forb plants purchased:", total_plants_purchased, "\n")
  cat("Total extra plants:", total_extras, "\n")
  
  if(remaining_spaces > 0) {
    cat("\nNote: You have", remaining_spaces, "extra forb spaces.\n")
    cat("Consider using extra plants or adding more of preferred species.\n")
  }
  
  return(forb_requirements)
}

#--Function to optimize tray purchases (alternative approach)
optimize_forb_trays <- function(final_grid, plant_database, prefer_fewer_trays = TRUE) {
  
  total_forb_spaces <- sum(final_grid$plant_type == "Empty_for_Forbs")
  forb_plants <- plant_database[plant_database$Vegetation.type == "Forbs", ]
  
  cat("=== OPTIMIZED FORB TRAY CALCULATION ===\n")
  cat("Total forb spaces:", total_forb_spaces, "\n")
  cat("Optimization goal:", ifelse(prefer_fewer_trays, "Minimize total trays", "Maximize species balance"), "\n\n")
  
  if(prefer_fewer_trays) {
    # Prioritize species with larger tray sizes to minimize total trays
    forb_plants <- forb_plants[order(-forb_plants$Tray.size), ]
    cat("Species ordered by tray size (largest first):\n")
  } else {
    # Keep original order for balanced approach
    cat("Species in balanced approach:\n")
  }
  
  for(i in 1:nrow(forb_plants)) {
    cat(i, ":", forb_plants$Common.Name[i], "- Tray size:", forb_plants$Tray.size[i], "\n")
  }
  
  # Calculate optimal distribution
  remaining_spaces <- total_forb_spaces
  num_species <- nrow(forb_plants)
  
  forb_allocation <- data.frame(
    Species = forb_plants$Common.Name,
    Tray_Size = forb_plants$Tray.size,
    Trays_Bought = 0,
    Plants_Used = 0,
    stringsAsFactors = FALSE
  )
  
  # Allocate plants
  if(prefer_fewer_trays) {
    # Greedy approach: use largest tray sizes first
    for(i in 1:nrow(forb_plants)) {
      tray_size <- forb_plants$Tray.size[i]
      max_trays <- floor(remaining_spaces / tray_size)
      
      # Try to use at least 1 tray of each species, then distribute remainder
      if(max_trays >= 1) {
        trays_to_use <- max(1, floor(remaining_spaces / (tray_size * (num_species - i + 1))))
        trays_to_use <- min(trays_to_use, max_trays)
        
        forb_allocation$Trays_Bought[i] <- trays_to_use
        forb_allocation$Plants_Used[i] <- min(trays_to_use * tray_size, remaining_spaces)
        remaining_spaces <- remaining_spaces - forb_allocation$Plants_Used[i]
      }
    }
  } else {
    # Balanced approach (same as previous function)
    plants_per_species <- floor(total_forb_spaces / num_species)
    
    for(i in 1:nrow(forb_plants)) {
      tray_size <- forb_plants$Tray.size[i]
      trays_needed <- ceiling(plants_per_species / tray_size)
      
      forb_allocation$Trays_Bought[i] <- trays_needed
      forb_allocation$Plants_Used[i] <- min(plants_per_species, remaining_spaces)
      remaining_spaces <- remaining_spaces - forb_allocation$Plants_Used[i]
    }
  }
  
  # Calculate totals
  forb_allocation$Plants_Purchased <- forb_allocation$Trays_Bought * forb_allocation$Tray_Size
  forb_allocation$Extras <- forb_allocation$Plants_Purchased - forb_allocation$Plants_Used
  
  # Display results
  cat("\n=== OPTIMIZED ALLOCATION ===\n")
  total_trays <- sum(forb_allocation$Trays_Bought)
  total_purchased <- sum(forb_allocation$Plants_Purchased)
  total_used <- sum(forb_allocation$Plants_Used)
  total_extras <- sum(forb_allocation$Extras)
  
  for(i in 1:nrow(forb_allocation)) {
    cat("\n", forb_allocation$Species[i], ":\n")
    cat("  Trays:", forb_allocation$Trays_Bought[i], "x", forb_allocation$Tray_Size[i], "=", forb_allocation$Plants_Purchased[i], "plants\n")
    cat("  Plants used:", forb_allocation$Plants_Used[i], "\n")
    cat("  Extras:", forb_allocation$Extras[i], "\n")
  }
  
  cat("\n=== TOTALS ===\n")
  cat("Total trays:", total_trays, "\n")
  cat("Total plants purchased:", total_purchased, "\n")
  cat("Total plants used:", total_used, "\n")
  cat("Total extras:", total_extras, "\n")
  cat("Unused forb spaces:", total_forb_spaces - total_used, "\n")
  
  return(forb_allocation)
}

#--Function to place forbs based on grouping requirements
place_forbs_by_grouping <- function(final_grid, forb_plan_optimized, plant_database) {
  
  cat("=== PLACING FORBS BY GROUPING REQUIREMENTS ===\n")
  
  # Make a copy of the grid to modify
  planted_grid <- final_grid
  
  # Get forb species info with grouping - handle NA values
  # FIXED: Use "Forbs" (plural)
  forb_info <- merge(forb_plan_optimized, 
                     plant_database[plant_database$Vegetation.type == "Forbs", 
                                    c("Common.Name", "Botanical.Name", "Grouping")], 
                     by.x = "Species", by.y = "Common.Name", all.x = TRUE)
  
  # Replace NA values in Grouping with "Random"
  forb_info$Grouping[is.na(forb_info$Grouping)] <- "Random"
  forb_info$Grouping[forb_info$Grouping == ""] <- "Random"
  
  # Handle missing botanical names
  forb_info$Botanical_Name[is.na(forb_info$Botanical_Name)] <- forb_info$Species[is.na(forb_info$Botanical_Name)]
  forb_info$Botanical_Name[forb_info$Botanical_Name == ""] <- forb_info$Species[forb_info$Botanical_Name == ""]
  
  cat("Forb species grouping requirements:\n")
  for(i in 1:nrow(forb_info)) {
    grouping_text <- ifelse(is.na(forb_info$Grouping[i]) | forb_info$Grouping[i] == "", 
                            "Random", forb_info$Grouping[i])
    cat(forb_info$Species[i], ":", grouping_text, 
        "(", forb_info$Plants_Used[i], "plants )\n")
  }
  
  # Get all available forb spaces
  forb_spaces_indices <- which(planted_grid$plant_type == "Empty_for_Forbs")
  available_spaces <- forb_spaces_indices
  
  cat("\nTotal forb spaces available:", length(available_spaces), "\n")
  
  # Add coordinates for spatial analysis
  coords <- suppressWarnings(st_coordinates(st_centroid(planted_grid)))
  planted_grid$x_center <- coords[,1]
  planted_grid$y_center <- coords[,2]
  
  # Estimate grid spacing
  unique_x <- sort(unique(coords[,1]))
  grid_spacing <- min(diff(unique_x)[diff(unique_x) > 0])
  
  # Function to find nearby forb spaces (not just adjacent)
  find_nearby_forb_spaces <- function(available_indices, min_cluster_size, search_radius_feet = 15) {
    
    if(length(available_indices) < min_cluster_size) return(list())
    
    clusters <- list()
    remaining_indices <- available_indices
    
    attempts <- 0
    max_attempts <- 20
    
    cat("Looking for clusters of", min_cluster_size, "+ spaces within", search_radius_feet, "feet radius\n")
    
    while(length(remaining_indices) >= min_cluster_size && attempts < max_attempts) {
      attempts <- attempts + 1
      
      # Start with a random seed point
      seed_idx <- sample(remaining_indices, 1)
      seed_coords <- c(planted_grid$x_center[seed_idx], planted_grid$y_center[seed_idx])
      
      # Find ALL points within the search radius
      cluster_indices <- c()
      for(idx in remaining_indices) {
        point_coords <- c(planted_grid$x_center[idx], planted_grid$y_center[idx])
        distance_feet <- sqrt((point_coords[1] - seed_coords[1])^2 + (point_coords[2] - seed_coords[2])^2)
        
        if(distance_feet <= search_radius_feet) {
          cluster_indices <- c(cluster_indices, idx)
        }
      }
      
      cat("    Found", length(cluster_indices), "spaces within", search_radius_feet, "feet\n")
      
      # If we found enough spaces in this area, use them
      if(length(cluster_indices) >= min_cluster_size) {
        clusters <- append(clusters, list(cluster_indices), length(clusters))
        remaining_indices <- setdiff(remaining_indices, cluster_indices)
        cat("    ✓ Cluster", length(clusters), "created with", length(cluster_indices), "spaces\n")
      } else {
        # Remove seed point and try a different area
        remaining_indices <- setdiff(remaining_indices, seed_idx)
      }
    }
    
    cat("Found", length(clusters), "clusters total\n")
    return(clusters)
  }
  
  # Function to find small groups (2-10 plants)
  find_small_groups <- function(available_indices, group_size, search_radius_feet = 8) {
    
    groups <- list()
    remaining_indices <- available_indices
    
    attempts <- 0
    max_attempts <- 100
    
    while(length(remaining_indices) >= group_size && attempts < max_attempts) {
      attempts <- attempts + 1
      
      if(length(remaining_indices) == 0) break
      
      seed_idx <- sample(remaining_indices, 1)
      seed_coords <- c(planted_grid$x_center[seed_idx], planted_grid$y_center[seed_idx])
      
      # Find points within smaller radius for small groups
      group_indices <- c()
      for(idx in remaining_indices) {
        point_coords <- c(planted_grid$x_center[idx], planted_grid$y_center[idx])
        distance_feet <- sqrt((point_coords[1] - seed_coords[1])^2 + (point_coords[2] - seed_coords[2])^2)
        
        if(distance_feet <= search_radius_feet) {
          group_indices <- c(group_indices, idx)
        }
      }
      
      # Use the group if it's the right size
      actual_group_size <- min(group_size, length(group_indices))
      if(actual_group_size >= 2) {  # At least 2 for a small group
        final_group <- group_indices[1:actual_group_size]
        groups <- append(groups, list(final_group), length(groups))
        remaining_indices <- setdiff(remaining_indices, final_group)
      } else {
        remaining_indices <- setdiff(remaining_indices, seed_idx)
      }
    }
    
    return(groups)
  }
  
  cat("\n=== PLACING COMMUNAL SPECIES (10+ groups) ===\n")
  
  # Process COMMUNAL species first
  communal_species <- forb_info[!is.na(forb_info$Grouping) & forb_info$Grouping == "Communal", ]
  
  if(nrow(communal_species) > 0) {
    for(i in 1:nrow(communal_species)) {
      species_name <- communal_species$Species[i]
      plants_needed <- communal_species$Plants_Used[i]
      botanical_name <- communal_species$Botanical.Name[i]
      
      cat("Placing", species_name, ":", plants_needed, "plants in communal groups\n")
      
      # Find clusters with larger search radius
      clusters <- find_nearby_forb_spaces(available_spaces, min_cluster_size = 10, search_radius_feet = 20)
      
      plants_placed <- 0
      cluster_count <- 0
      
      for(cluster in clusters) {
        if(plants_placed >= plants_needed) break
        
        cluster_count <- cluster_count + 1
        plants_in_cluster <- min(length(cluster), plants_needed - plants_placed)
        
        # Place plants in this cluster
        for(j in 1:plants_in_cluster) {
          idx <- cluster[j]
          planted_grid$plant_type[idx] <- "Forb"
          planted_grid$plant_name[idx] <- botanical_name
          planted_grid$common_name[idx] <- species_name
          plants_placed <- plants_placed + 1
        }
        
        # Remove used spaces
        available_spaces <- setdiff(available_spaces, cluster[1:plants_in_cluster])
        
        cat("  Cluster", cluster_count, ":", plants_in_cluster, "plants placed\n")
      }
      
      cat("  Total placed:", plants_placed, "/", plants_needed, "\n")
    }
  } else {
    cat("No communal species found\n")
  }
  
  cat("\n=== PLACING SMALL GROUP SPECIES (2-10 groups) ===\n")
  
  # Process SMALL GROUP species
  small_group_species <- forb_info[!is.na(forb_info$Grouping) & forb_info$Grouping == "Small Group", ]
  
  if(nrow(small_group_species) > 0) {
    for(i in 1:nrow(small_group_species)) {
      species_name <- small_group_species$Species[i]
      plants_needed <- small_group_species$Plants_Used[i]
      botanical_name <- small_group_species$Botanical.Name[i]
      
      cat("Placing", species_name, ":", plants_needed, "plants in small groups\n")
      
      # Determine group size (2-10)
      optimal_group_size <- max(2, min(10, ceiling(plants_needed / 4)))
      
      groups <- find_small_groups(available_spaces, optimal_group_size, search_radius_feet = 10)
      
      plants_placed <- 0
      group_count <- 0
      
      for(group in groups) {
        if(plants_placed >= plants_needed) break
        
        group_count <- group_count + 1
        plants_in_group <- min(length(group), plants_needed - plants_placed)
        
        # Place plants in this group
        for(j in 1:plants_in_group) {
          idx <- group[j]
          planted_grid$plant_type[idx] <- "Forb"
          planted_grid$plant_name[idx] <- botanical_name
          planted_grid$common_name[idx] <- species_name
          plants_placed <- plants_placed + 1
        }
        
        # Remove used spaces
        available_spaces <- setdiff(available_spaces, group[1:plants_in_group])
        
        cat("  Group", group_count, ":", plants_in_group, "plants placed\n")
      }
      
      cat("  Total placed:", plants_placed, "/", plants_needed, "\n")
    }
  } else {
    cat("No small group species found\n")
  }
  
  cat("\n=== PLACING RANDOM/OTHER SPECIES ===\n")
  
  # Process remaining species (random placement)
  other_species <- forb_info[is.na(forb_info$Grouping) | 
                               forb_info$Grouping == "Random" | 
                               forb_info$Grouping == "" |
                               !forb_info$Grouping %in% c("Communal", "Small Group"), ]
  
  if(nrow(other_species) > 0) {
    for(i in 1:nrow(other_species)) {
      species_name <- other_species$Species[i]
      plants_needed <- other_species$Plants_Used[i]
      botanical_name <- other_species$Botanical.Name[i]
      
      cat("Placing", species_name, ":", plants_needed, "plants randomly\n")
      
      plants_placed <- 0
      
      # Random placement
      if(plants_needed > 0 && length(available_spaces) > 0) {
        spaces_to_use <- min(plants_needed, length(available_spaces))
        selected_spaces <- sample(available_spaces, spaces_to_use)
        
        for(idx in selected_spaces) {
          planted_grid$plant_type[idx] <- "Forb"
          planted_grid$plant_name[idx] <- botanical_name
          planted_grid$common_name[idx] <- species_name
          plants_placed <- plants_placed + 1
        }
        
        # Remove used spaces
        available_spaces <- setdiff(available_spaces, selected_spaces)
      }
      
      cat("  Total placed:", plants_placed, "/", plants_needed, "\n")
    }
  } else {
    cat("No random placement species found\n")
  }
  
  cat("\n=== PLACEMENT SUMMARY ===\n")
  cat("Remaining empty forb spaces:", length(available_spaces), "\n")
  
  # Summary by species
  for(species in forb_info$Species) {
    placed_count <- sum(planted_grid$common_name == species, na.rm = TRUE)
    needed_count <- forb_info$Plants_Used[forb_info$Species == species]
    cat(species, ":", placed_count, "/", needed_count, "placed\n")
  }
  
  return(planted_grid)
}

#--Function to visualize forb placement
plot_forb_placement <- function(planted_grid) {
  
  # Load required libraries
  library(RColorBrewer)
  library(viridis)
  
  # Get all species by type
  grass_species <- unique(planted_grid$common_name[planted_grid$plant_type == "Grass"])
  shrub_species <- unique(planted_grid$common_name[planted_grid$plant_type == "Shrub"])
  forb_species <- unique(planted_grid$common_name[planted_grid$plant_type == "Forb"])
  
  # Remove NA values if they exist
  grass_species <- grass_species[!is.na(grass_species)]
  shrub_species <- shrub_species[!is.na(shrub_species)]
  forb_species <- forb_species[!is.na(forb_species)]
  
  # Create distinct color palettes for each plant type
  # Grasses: Cool colors (blues, purples)
  if(length(grass_species) > 0) {
    grass_colors <- brewer.pal(min(max(3, length(grass_species)), 9), "Blues")[1:length(grass_species)]
  } else {
    grass_colors <- c()
  }
  
  # Shrubs: Earth tones (browns, dark greens)
  if(length(shrub_species) > 0) {
    shrub_colors <- brewer.pal(min(max(3, length(shrub_species)), 9), "BrBG")[1:length(shrub_species)]
  } else {
    shrub_colors <- c()
  }
  
  # Forbs: Vibrant, diverse colors
  if(length(forb_species) > 0) {
    if(length(forb_species) <= 12) {
      forb_colors <- brewer.pal(min(max(3, length(forb_species)), 12), "Set3")[1:length(forb_species)]
    } else {
      # Use viridis for many species
      forb_colors <- viridis(length(forb_species), option = "plasma")
    }
  } else {
    forb_colors <- c()
  }
  
  # Initialize plot colors
  plot_colors <- rep("white", nrow(planted_grid))  # White for empty spaces
  
  # Create a species-to-color mapping
  species_color_map <- list()
  
  # Assign grass colors
  for(i in seq_along(grass_species)) {
    species_color_map[[grass_species[i]]] <- grass_colors[i]
  }
  
  # Assign shrub colors
  for(i in seq_along(shrub_species)) {
    species_color_map[[shrub_species[i]]] <- shrub_colors[i]
  }
  
  # Assign forb colors
  for(i in seq_along(forb_species)) {
    species_color_map[[forb_species[i]]] <- forb_colors[i]
  }
  
  # Apply colors to plot
  for(i in 1:nrow(planted_grid)) {
    species <- planted_grid$common_name[i]
    if(!is.na(species) && species %in% names(species_color_map)) {
      plot_colors[i] <- species_color_map[[species]]
    }
  }
  
  # Create the plot with better styling
  par(mar = c(1, 1, 3, 1), bg = "white")
  
  plot(planted_grid$geometry, 
       col = plot_colors,
       main = "Complete Matrix Garden with Forbs",
       border = "gray90", 
       lwd = 0.3,
       cex.main = 1.2,
       font.main = 2)
  
  # Create comprehensive legend
  all_species <- c(grass_species, shrub_species, forb_species)
  all_colors <- c(grass_colors, shrub_colors, forb_colors)
  
  # Add "Empty Space" if there are any
  if(any(is.na(planted_grid$common_name) | planted_grid$common_name == "")) {
    all_species <- c(all_species, "Empty Space")
    all_colors <- c(all_colors, "white")
  }
  
  # Create legend with better positioning and styling
  legend("topright", 
         legend = all_species, 
         fill = all_colors,
         border = "gray50",
         cex = 0.7,
         bg = "white",
         box.col = "gray50",
         title = "Plant Species",
         title.adj = 0)
  
  # Add summary statistics as subtitle
  n_grass <- length(grass_species)
  n_shrub <- length(shrub_species)
  n_forb <- length(forb_species)
  
  mtext(paste("Grasses:", n_grass, "| Shrubs:", n_shrub, "| Forbs:", n_forb), 
        side = 3, line = 0.5, cex = 0.8, col = "gray30")
  
  # Return invisible list of species counts for further analysis
  invisible(list(
    grass_count = n_grass,
    shrub_count = n_shrub,
    forb_count = n_forb,
    total_species = n_grass + n_shrub + n_forb,
    color_mapping = species_color_map
  ))
}

#--Helper function for summary
generate_planting_summary <- function(planted_grid, plant_database) {
  cat("\n=== PLANTING SUMMARY ===\n")
  
  grass_count <- table(planted_grid$common_name[planted_grid$plant_type == "Grass"])
  shrub_count <- table(planted_grid$common_name[planted_grid$plant_type == "Shrub"])
  forb_count <- table(planted_grid$common_name[planted_grid$plant_type == "Forb"])
  forb_spaces_remaining <- sum(planted_grid$plant_type == "Empty_for_Forbs")
  
  # Function to calculate tray requirements and cost
  calculate_plant_cost <- function(species_counts, vegetation_type) {
    total_cost <- 0
    total_plants_purchased <- 0
    total_trays <- 0
    
    if(length(species_counts) > 0) {
      for(species in names(species_counts)) {
        plants_needed <- species_counts[species]
        
        # Find plant info in database
        plant_info <- plant_database[plant_database$Common.Name == species & 
                                       plant_database$Vegetation.type == vegetation_type, ]
        
        if(nrow(plant_info) > 0) {
          tray_size <- plant_info$Tray.size[1]
          tray_price <- plant_info$Tray.price.LGV[1]
          
          # Calculate trays needed
          trays_needed <- ceiling(plants_needed / tray_size)
          plants_purchased <- trays_needed * tray_size
          cost <- trays_needed * tray_price
          
          total_trays <- total_trays + trays_needed
          total_plants_purchased <- total_plants_purchased + plants_purchased
          total_cost <- total_cost + cost
          
          cat(species, ":\n")
          cat("  Plants needed:", plants_needed, "\n")
          cat("  Tray size:", tray_size, "plants per tray\n")
          cat("  Trays to buy:", trays_needed, "at $", tray_price, "each\n")
          cat("  Total plants purchased:", plants_purchased, "\n")
          cat("  Cost: $", round(cost, 2), "\n")
          cat("  Extra plants:", plants_purchased - plants_needed, "\n\n")
        } else {
          cat(species, ": Price info not found\n\n")
        }
      }
    }
    
    return(list(
      total_cost = total_cost,
      total_plants_purchased = total_plants_purchased,
      total_trays = total_trays
    ))
  }
  
  cat("\n--- GRASSES TO PURCHASE ---\n")
  total_grasses <- sum(grass_count)
  grass_costs <- calculate_plant_cost(grass_count, "Grass")
  cat("GRASS TOTALS:\n")
  cat("  Plants needed:", total_grasses, "\n")
  cat("  Plants purchased:", grass_costs$total_plants_purchased, "\n")
  cat("  Total trays:", grass_costs$total_trays, "\n")
  cat("  Total cost: $", round(grass_costs$total_cost, 2), "\n\n")
  
  cat("--- FORBS TO PURCHASE ---\n")
  total_forbs <- sum(forb_count)
  if(total_forbs > 0) {
    forb_costs <- calculate_plant_cost(forb_count, "Forbs")
    cat("FORB TOTALS:\n")
    cat("  Plants needed:", total_forbs, "\n")
    cat("  Plants purchased:", forb_costs$total_plants_purchased, "\n")
    cat("  Total trays:", forb_costs$total_trays, "\n")
    cat("  Total cost: $", round(forb_costs$total_cost, 2), "\n\n")
  } else {
    forb_costs <- list(total_cost = 0, total_plants_purchased = 0, total_trays = 0)
    cat("No forbs planted yet\n\n")
  }
  
  cat("--- SHRUBS TO PURCHASE ---\n")
  total_shrubs <- sum(shrub_count)
  if(total_shrubs > 0) {
    shrub_costs <- calculate_plant_cost(shrub_count, "Shrub")
    cat("SHRUB TOTALS:\n")
    cat("  Plants needed:", total_shrubs, "\n")
    cat("  Plants purchased:", shrub_costs$total_plants_purchased, "\n")
    cat("  Total trays:", shrub_costs$total_trays, "\n")
    cat("  Total cost: $", round(shrub_costs$total_cost, 2), "\n\n")
  } else {
    shrub_costs <- list(total_cost = 0, total_plants_purchased = 0, total_trays = 0)
    cat("No shrubs planted\n\n")
  }
  
  cat("--- GARDEN COMPOSITION ---\n")
  total_plants_placed <- total_grasses + total_forbs + total_shrubs
  total_squares <- nrow(planted_grid)
  
  grass_percent <- round(total_grasses / total_plants_placed * 100, 1)
  forb_percent <- round(total_forbs / total_plants_placed * 100, 1)
  shrub_percent <- round(total_shrubs / total_plants_placed * 100, 1)
  
  cat("Planted composition:\n")
  cat("  Grasses:", grass_percent, "% (", total_grasses, "plants)\n")
  cat("  Forbs:", forb_percent, "% (", total_forbs, "plants)\n")
  cat("  Shrubs:", shrub_percent, "% (", total_shrubs, "plants)\n")
  cat("  Empty forb spaces:", forb_spaces_remaining, "squares\n")
  cat("  Total planted:", total_plants_placed, "plants\n")
  cat("  Total squares:", total_squares, "\n")
  
  if(total_grasses > 0 && total_forbs > 0) {
    cat("  Grass:Forb ratio = 1:", round(total_forbs/total_grasses, 2), "\n")
  }
  
  cat("\n--- TOTAL PROJECT COST ---\n")
  grand_total_cost <- grass_costs$total_cost + forb_costs$total_cost + shrub_costs$total_cost
  grand_total_trays <- grass_costs$total_trays + forb_costs$total_trays + shrub_costs$total_trays
  grand_total_plants <- grass_costs$total_plants_purchased + forb_costs$total_plants_purchased + shrub_costs$total_plants_purchased
  
  cat("Total trays to purchase:", grand_total_trays, "\n")
  cat("Total plants purchased:", grand_total_plants, "\n")
  cat("TOTAL PROJECT COST: $", round(grand_total_cost, 2), "\n")
  
  # Create summary data frame for export
  summary_data <- data.frame(
    Category = c("Grasses", "Forbs", "Shrubs", "TOTAL"),
    Plants_Needed = c(total_grasses, total_forbs, total_shrubs, total_plants_placed),
    Plants_Purchased = c(grass_costs$total_plants_purchased, forb_costs$total_plants_purchased, 
                         shrub_costs$total_plants_purchased, grand_total_plants),
    Total_Trays = c(grass_costs$total_trays, forb_costs$total_trays, 
                    shrub_costs$total_trays, grand_total_trays),
    Total_Cost = c(grass_costs$total_cost, forb_costs$total_cost, 
                   shrub_costs$total_cost, grand_total_cost)
  )
  
  # Return summary data invisibly
  invisible(summary_data)
}

# Function to export detailed plant list with costs
export_detailed_plant_list <- function(planted_grid, plant_database, filename = "detailed_plant_order.csv") {
  
  # Get all plant counts
  all_plants <- planted_grid[planted_grid$plant_type %in% c("Grass", "Forb", "Shrub"), ]
  plant_counts <- table(all_plants$common_name, all_plants$plant_type)
  
  # Create detailed list
  detailed_list <- data.frame()
  
  for(plant_type in c("Grass", "Forbs", "Shrub")) {
    if(plant_type %in% colnames(plant_counts)) {
      species_in_type <- plant_counts[plant_counts[, plant_type] > 0, plant_type]
      
      for(species in names(species_in_type)) {
        plants_needed <- species_in_type[species]
        
        plant_info <- plant_database[plant_database$Common.Name == species & 
                                       plant_database$Vegetation.type == plant_type, ]
        
        if(nrow(plant_info) > 0) {
          tray_size <- plant_info$Tray.size[1]
          tray_price <- plant_info$Tray.price.LGV[1]
          trays_needed <- ceiling(plants_needed / tray_size)
          
          detailed_list <- rbind(detailed_list, data.frame(
            Plant_Type = plant_type,
            Common_Name = species,
            Botanical_Name = plant_info$Botanical.Name[1],
            Plants_Needed = plants_needed,
            Tray_Size = tray_size,
            Trays_to_Order = trays_needed,
            Plants_Purchased = trays_needed * tray_size,
            Price_per_Tray = tray_price,
            Total_Cost = trays_needed * tray_price,
            Extra_Plants = (trays_needed * tray_size) - plants_needed
          ))
        }
      }
    }
  }
  
  # Sort by plant type and cost
  detailed_list <- detailed_list[order(detailed_list$Plant_Type, -detailed_list$Total_Cost), ]
  
  write.csv(detailed_list, filename, row.names = FALSE)
  cat("Detailed plant order exported to:", filename, "\n")
  
  return(detailed_list)
}



###################
## Main Function ##
###################
#--Main function structure
design_matrix_garden <- function() {
  # 1. Get file format
  file_format <- get_file_format()
  
  # 2. Load perimeter data
  if(file_format == "kml"){
    garden_area <- select_kml_file() # load the file
    
  } else {
    cat("I need to design this part: line")
  }
  
  spacing_result <- get_spacing_and_create_grid(garden_area) # create a grid
  
  
  # 3. Get data if choosing from a list or Get site conditions to pick plants
  # site_conditions <- get_site_conditions()
  
  # Read from public Google Sheet (no authentication needed)
  # sheet_url <- https://docs.google.com/spreadsheets/d/1edDtXnxyyVSvXM_jNPahPEPfuFWz8Ymgv4RC8ZP557M/edit?usp=sharing
  plant_data <- get_plant_database_url()
  plants <- load_plant_database(plant_data)
  
  
  
  # # 4. Plant selection workflow
  # selected_plants <- select_plants_workflow(site_conditions)
  
  
  planted_grid <- suppressWarnings(assign_true_checkerboard(plantGrid = spacing_result$planting_grid, 
                                                    plant_database = plants)) # Generate planting design check board
  
  plot_grass_species(planted_grid) # plot grid with grass species
  
  shrub_distance_feet <- get_shrub_spacing() # Get user input for shrub spacing in feet
  
  final_grid <- suppressWarnings(
    place_shrubs_center_simple(planted_grid, plants, min_shrub_distance_feet = shrub_distance_feet)
  )# Place shrubs with constraints

  plot_final_matrix_garden(final_grid) # look at plant species
  
  
  # Count forb spaces in final_grid
  forb_spaces_count <- sum(final_grid$plant_type == "Empty_for_Forbs")
  cat("Total forb spaces available:", forb_spaces_count, "\n")
  
  # # Calculate balanced forb requirements
  # forb_plan <- calculate_forb_requirements(final_grid, 
  #                                          plant_database = plants)
  
  # Alternative: Optimize for fewer trays
  forb_plan_optimized <- optimize_forb_trays(final_grid, 
                                             plant_database = plants, 
                                             prefer_fewer_trays = TRUE)
  
  # View the results
  # print(forb_plan) # non-optimized for waste
  print(forb_plan_optimized) #optimized for waste
  
  # Place forbs according to grouping requirements
  complete_garden <- place_forbs_by_grouping(final_grid, 
                                             forb_plan_optimized, 
                                             plant_database = plants)
  
  # Visualize the complete garden
  plot_forb_placement(complete_garden)
  
  # Generate final summary
  generate_planting_summary(complete_garden, plants)
  
  
  
  
  
  
  
  # # Export purchase list
  # export_purchase_list(final_planted_grid, "my_garden_purchase_list.csv")
  # 
  
  
  
  # design <- create_planting_design(garden_area, selected_plants)
  
  return(design)
}






# Create ggplot of the grid
p <- ggplot(spacing_result$planting_grid$geometry) +
  geom_sf(fill = "blue", color = "black", alpha = 0.6, size = 0.1) +
  theme_minimal() +
  ggtitle("Interactive Grid with Plotly")

# Convert to interactive plotly
interactive_plot <- ggplotly(p)
interactive_plot









