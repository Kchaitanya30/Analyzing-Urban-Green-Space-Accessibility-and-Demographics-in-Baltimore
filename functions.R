# Fill in supporting functions in this file


get_isochrone <- function(park_coords, api_key, duration_minutes = 10) {
  ors_isochrones(
    locations = list(park_coords),
    profile = "foot-walking",  # Walking accessibility
    range = duration_minutes * 60,  # Convert minutes to seconds
    api_key = api_key,
    output = "sf"
  )
}


get_coords <- function(park_id, ob_park) {
  # Fiter the park id from the park object
  selected_park <- ob_park[ob_park$OBJECTID == park_id, ]

  # Cross check of its existence
  if (nrow(selected_park) == 0) {
    stop(paste("No park found with OBJECTID:", park_id))
  }

  # Extract the coordinates
  coords <- st_coordinates(selected_park$geometry)

  list(c(coords[1, 1], coords[1, 2]))
}

# Fill in supporting functions in this file

# For parks as the isochrones work on a particular point, and walking profile
#is selected

get_isochrone <- function(park_coords, api_key, duration_minutes = 10) {
  ors_isochrones(
    locations = list(park_coords),
    profile = "foot-walking",  # Walking accessibility
    range = duration_minutes * 60,  # Convert minutes to seconds
    api_key = api_key,
    output = "sf"
  )
}

# To get coordinates of a park by its park id with validation of existence
get_coords <- function(park_id, ob_park) {
  selected_park <- ob_park[ob_park$OBJECTID == park_id, ]
  if (nrow(selected_park) == 0) {
    stop(paste("No park found with OBJECTID:", park_id))
  }
  coords <- st_coordinates(selected_park$geometry)
  list(c(coords[1, 1], coords[1, 2]))
}


# function to create a leaflet map with isochrones, tracts and parks.

create_leafletmap <- function(isochrone_data, tract_data, park_data) {
  leaflet() |>
    addTiles() |>
    addPolygons(
      data = tract_data,
      fill = NA,
      stroke = TRUE,
      color = "red",
      weight = 3,
      popup = ~paste("Tract ID:", GEOID)
    ) |>
    addPolygons(
      data = isochrone_data,
      fillColor = ~ifelse(mins == 5, "blue", "skyblue"), # Colour differentiation for isochrones
      fillOpacity = 0.4,
      color = "black",
      weight = 1,
      popup = ~paste("Walking Time:", mins, "minutes")
    ) |>
    addPolygons(
      data = park_data,
      fillColor = "green",
      fillOpacity = 0.5,
      color = "green",
      weight = 1,
      popup = ~paste("Park ID:", OBJECTID)
    ) |>
    addLegend(
      position = "bottomright",
      colors = c("blue", "skyblue", "green", "red"),
      labels = c("5 Minutes", "10 Minutes", "Park", "Tracts"),
      title = "Legend",
      opacity = 1,
      values = c("5 Minutes", "10 Minutes", "Park", "Tracts")
    )
}


# Function to extract isochrones for vaious profile modes
extract_isochrones <- function(coords, mode, range, interval, api_key) {
  isochrones <- openrouteservice::ors_isochrones(
    locations = coords,
    profile = mode,
    range = range,
    interval = interval,
    api_key = api_key,
    output = "sf"
  )
  isochrones$mode <- mode
  return(isochrones)
}


# Function to create a leaflet map for a selected location
create_BP_leafletmap <- function(isochrone_data, tract_data, park_data, coords, title ) {
  leaflet() |>
    addTiles() |>
    addPolygons(
      data = tract_data,
      fill = NA,
      color = "red",
      weight = 1,
      popup = ~paste("Tract ID:", GEOID)
    ) |>
    addPolygons(
      data = isochrone_data,
      fillColor = ~ifelse(mins == 5, "blue",
                          ifelse(mins == 10, "purple", "skyblue")),
      fillOpacity = 0.5,
      color = "black",
      weight = 1,
      popup = ~paste("Time:", mins)
    ) |>
    addPolygons(
      data = park_data,
      fillColor = "green",
      fillOpacity = 0.6,
      color = "darkgreen",
      weight = 1,
      popup = ~paste("Park ID:", OBJECTID)
    ) |>
    addMarkers(
      lng = coords[1],
      lat = coords[2],
      popup = "Isochrone Center"
    ) |>
    addLegend(
      position = "bottomright",
      colors = c("blue", "purple", "skyblue", "green", "red"),
      labels = c("5 Minutes", "10 Minutes", "15 Minutes", "Parks", "Tracts"),
      title = paste(title, "Isochrones"),
      opacity = 1,
      values =  c("5 Minutes", "10 Minutes", "15 Minutes", "Parks", "Tracts")
    )
}

# funtion to resolve overlaps of isochrones
unoverlap_isochrones <- function(isochrone_data, selected_column) {
  isochrone_data |>
    group_by_at(selected_column) |>
    st_intersection() |>
    ungroup()
}



