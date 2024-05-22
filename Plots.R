



library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(sf)


# Reading in data
ks_dat <- read.csv("./KansasCamera_data.csv") # Camera data

KSextent <- st_read("./KSextent/tl_2019_20_cousub.shp") # Kansas border

# Formatting DateTime
ks_dat$DateTime  <- as.POSIXct(ks_dat$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS") 


# Creating a dataframe for deer occurrences
deer_occurrences <- data.frame(Site = unique(ks_dat$Site))


for (site in unique(ks_dat$Site)) {
  
  # Subsetting data by site
  site_data <- ks_dat[ks_dat$Site == site, ]
  
  if ("Mule Deer" %in% site_data$Common_name & "White-Tailed Deer" %in% site_data$Common_name) {
    
    deer_occurrences$Deer_Occurrence[deer_occurrences$Site == site] <- "Both"
    
  } else if ("Mule Deer" %in% site_data$Common_name) {
    
    deer_occurrences$Deer_Occurrence[deer_occurrences$Site == site] <- "Mule Deer"
    
  } else if ("White-Tailed Deer" %in% site_data$Common_name) {
    
    deer_occurrences$Deer_Occurrence[deer_occurrences$Site == site] <- "White-Tailed Deer"
    
  } else if (!("Mule Deer" %in% site_data$Common_name) & !("White-Tailed Deer" %in% site_data$Common_name)){
    
    deer_occurrences$Deer_Occurrence[deer_occurrences$Site == site] <- "Neither"
  }
  
  # Extracting latitude and longitude from the first occurrence
  first_occurrence <- site_data[1, ]
  deer_occurrences$Latitude[deer_occurrences$Site == site] <- first_occurrence$Latitude
  deer_occurrences$Longitude[deer_occurrences$Site == site] <- first_occurrence$Longitude
}


NROW(deer_occurrences)

# Plotting based on Deer_Occurrence conditions

# Create a color palette
palette <- colorFactor(
  palette = c("purple", "orange", "blue", "black"),  # Adjust colors as needed
  levels = c("White-Tailed Deer", "Mule Deer", "Both", "Neither")
)

# Create a leaflet map centered on Kansas
kansas_map <- leaflet() %>%
  setView(lng = -98.35, lat = 38.5, zoom = 6) %>%
  addTiles()

# Add points to the map based on Deer_Occurrence conditions
kansas_map <- kansas_map %>%
  addCircleMarkers(
    data = deer_occurrences,
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~palette(Deer_Occurrence),
    radius = 5,
    fillOpacity = 0.7,
    popup = ~paste("Site: ", Site, "<br>Deer Occurrence: ", Deer_Occurrence)
  )

# Display the map
kansas_map


# Plot of Study Area

# plots of all sites
ggplot() +
  geom_sf(data = KSextent, fill = "transparent", color = "black") + 
  geom_point(data = deer_occurrences, aes(x = Longitude, y = Latitude), size = 3)



# plots of sites with just White-Tailed Deer
wtd_plot <- subset(deer_occurrences, Deer_Occurrence == "White-Tailed Deer")
NROW(wtd_plot)

ggplot() +
  geom_sf(data = KSextent, fill = "transparent", color = "black") + 
  geom_point(data = wtd_plot, aes(x = Longitude, y = Latitude), size = 3, color = "purple")


# plots of sites with just mule deer
md_plot <- subset(deer_occurrences, Deer_Occurrence == "Mule Deer")
NROW(md_plot)

ggplot() +
  geom_sf(data = KSextent, fill = "transparent", color = "black") + 
  geom_point(data = md_plot, aes(x = Longitude, y = Latitude), size = 3, color = "orange")


# plots of sites with both
both_plot <- subset(deer_occurrences, Deer_Occurrence == "Both")
NROW(both_plot)

ggplot() +
  geom_sf(data = KSextent, fill = "transparent", color = "black") + 
  geom_point(data = both_plot, aes(x = Longitude, y = Latitude), size = 3, color = "blue")



# plots of sites with neither
neither_plot <- subset(deer_occurrences, Deer_Occurrence == "Neither")
NROW(neither_plot)

ggplot() +
  geom_sf(data = KSextent, fill = "transparent", color = "black") + 
  geom_point(data = neither_plot, aes(x = Longitude, y = Latitude), size = 3, color = "black")






# Plotting based on Deer_Occurrence conditions
ggplot() +
  geom_sf(data = KSextent, fill = "transparent", color = "black") +  
  geom_point(data = deer_occurrences, aes(x = Longitude, y = Latitude, color = Deer_Occurrence), size = 3) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("White-Tailed Deer" = "purple",
                                "Mule Deer" = "orange",
                                "Both" = "blue",
                                "Neither" = "black")) +
  theme_minimal()





