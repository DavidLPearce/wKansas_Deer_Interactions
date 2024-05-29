# -------------------------------------------------------
#
#                    Load/install Packages
#
# -------------------------------------------------------


library(ggplot2)
library(dplyr)
library(lubridate)
library(unmarked)
library(AICcmodavg)
library(raster)
library(terra)
library(sp)
library(spatialEco)
library(progress)

set.seed(123)
options(scipen = 999)
setwd(".")

# -------------------------------------------------------
#
#                    Read In data
#
# -------------------------------------------------------


# Reading in data
ks_dat <- read.csv("./KansasCamera_data.csv") # Camera data

# -------------------------------------------------------
#
#                   Data Wrangling
#
# -------------------------------------------------------



# Formatting DateTime
ks_dat$DateTime  <- as.POSIXct(ks_dat$DateTime ,  tryFormats = "%m/%d/%Y %H:%M:%OS", tz="UTC") 


# Adding a column based on year for survey 
ks_dat$Survey <- ifelse(ks_dat$Year == "2018", 1,
                        ifelse(ks_dat$Year == "2019", 2, 
                               ifelse(ks_dat$Year == "2020", 3, NA)))

# Creating a stacked site column
ks_dat$Stack_ID <- paste(ks_dat$Site, ks_dat$Survey, sep = ".")

# Extracting stacked id to create new site IDs
stack_df <- as.data.frame(as.matrix(as.numeric(unique(ks_dat$Stack_ID))))
names(stack_df)[1] <- "Stack_ID"
stack_df <- arrange(stack_df, Stack_ID)

# creating new site IDs
stack_df$New_SiteID <- 1:NROW(stack_df)

# merging new site ID by Stack_ID
ks_dat <- merge(ks_dat, stack_df, by.x = "Stack_ID", by.y = "Stack_ID")


# adding day of year based on set date
ks_dat$SetVisitDate  <- as.POSIXct(ks_dat$SetVisitDate ,  tryFormats = "%m/%d/%Y", tz="UTC") 

ks_dat <- ks_dat %>%
  mutate(dep_doy = yday(SetVisitDate))

# cameras were out for 28 days so adding a retrieve day of year column
ks_dat$ret_doy <- ks_dat$dep_doy + 28

# adding a day of year for when detection occured
ks_dat$Date  <- as.POSIXct(ks_dat$Date ,  tryFormats = "%m/%d/%Y", tz="UTC") 

ks_dat <- ks_dat %>%
  mutate(det_doy = yday(Date))

## ------------------ White-Tailed Deer -------------------------------------

# subsetting the data by white-tailed deer 
wtd_dat <- ks_dat[(which(ks_dat$Common_name == "White-Tailed Deer")),]

# creating a detection matrix for wtd
wtd_det_mat <- matrix(0, nrow = length(unique(ks_dat$New_SiteID)), ncol = 28)

# renaming det_mat columns
colnames(wtd_det_mat) <- 1:28
rownames(wtd_det_mat) <- 1:length(unique(ks_dat$New_SiteID))



# Filling detection matrix with 1 if wtd was detected at the site on deployment day

for (site in 1:length(unique(wtd_dat$New_SiteID))){
  
  site_sub <- wtd_dat[(which(wtd_dat$New_SiteID == site )),] 
  
  # Order the data frame by det_doy
  site_sub <- site_sub %>%
    arrange(det_doy)
  
  for (un_det_day in 1:28){
    
    det_day_sub <- site_sub[(which(site_sub$det_doy - site_sub$dep_doy == un_det_day)),] 
    
    site_id <- as.numeric(unique(det_day_sub$New_SiteID))
    
    # Check if any detections occurred on this day
    if (nrow(det_day_sub) > 0) {
      wtd_det_mat[site_id, un_det_day] <- 1
    }
    
  }
}

# Take a look
print(wtd_det_mat)



## ------------------ Mule Deer -------------------------------------

# subsetting the data by white-tailed deer 
md_dat <- ks_dat[(which(ks_dat$Common_name == "Mule Deer")),]

# creating a detection matrix for wtd
md_det_mat <- matrix(0, nrow = length(unique(ks_dat$New_SiteID)), ncol = 28)

# renaming det_mat columns
colnames(md_det_mat) <- 1:28
rownames(md_det_mat) <- 1:length(unique(ks_dat$New_SiteID))

# Filling detection matrix with 1 if wtd was detected at the site on deployment day
for (site in 1:length(unique(md_dat$New_SiteID))){
  
  site_sub <- md_dat[(which(md_dat$New_SiteID == site )),] 
  
  # Order the data frame by det_doy
  site_sub <- site_sub %>%
    arrange(det_doy)
  
  for (un_det_day in 1:28){
    
    det_day_sub <- site_sub[(which(site_sub$det_doy - site_sub$dep_doy == un_det_day)),] 
    
    site_id <- as.numeric(unique(det_day_sub$New_SiteID))
    
    # Check if any detections occurred on this day
    if (nrow(det_day_sub) > 0) {
      md_det_mat[site_id, un_det_day] <- 1
    }
    
  }
}

# Take a look
print(md_det_mat)


# Creating a list for the det matrices
ylist <- list(Whitetail_deer = wtd_det_mat, mule_deer = md_det_mat)


## ------------------ Covariates -------------------------------------

# detection covariates --------------

doy_cov <- read.csv("./KansasCamera_doy.csv")

# Sort by site and then by date
ordered_indices <- order(doy_cov$Site, doy_cov$Year)

# reorder  data frame
doy_cov <- doy_cov[ordered_indices, ]

# Adding a column based on year for survey 
doy_cov$Survey <- ifelse(doy_cov$Year == "2018", 1,
                         ifelse(doy_cov$Year == "2019", 2, 
                                ifelse(doy_cov$Year == "2020", 3, NA)))


# Creating a stacked site column
doy_cov$Stack_ID <- paste(doy_cov$Site, doy_cov$Survey, sep = ".")




# merging new site ID by Stack_ID
doy_merge_dat <- merge(ks_dat, doy_cov, by.x = "Stack_ID", by.y = "Stack_ID")


# Extracting unique instances of Stack_ID with corresponding Latitude, Longitude, and DaysActive
doy_unique_data <- doy_merge_dat %>%
  distinct(Stack_ID,Site.x, Latitude.x, Longitude.x, DaysActive) %>%
  arrange(Stack_ID)


# # reading in vegetation data
# veg_cov <- read.csv("./KansasCamera_cov.csv")
# 
# # Sort by site and then by date
# ordered_indices <- order(veg_cov$Site, veg_cov$SurveyYear)

# # reorder  data frame
# veg_cov <- veg_cov[ordered_indices, ]
# 
# 
# # Adding a column based on year for survey 
# veg_cov$Survey <- ifelse(veg_cov$SurveyYear == "2018", 1,
#                          ifelse(veg_cov$SurveyYear == "2019", 2, 
#                                 ifelse(veg_cov$SurveyYear == "2020", 3, NA)))
# 
# 
# # Creating a stacked site column
# veg_cov$Stack_ID <- paste(veg_cov$Site, veg_cov$Survey, sep = ".")
# 
# # merging new site ID by Stack_ID
# veg_merge_dat <- merge(ks_dat, veg_cov, by.x = "Stack_ID", by.y = "Stack_ID")
# 
# # Extracting unique instances of Stack_ID with corresponding Latitude, Longitude, and DaysActive
# veg_unique_data <- veg_merge_dat %>%
#   select(Stack_ID,Site.x, VegHeight) %>%
#   distinct() %>%
#   arrange(Stack_ID)

# # Merging doy cov and veg cov into a single dataframe for detection
# # merging new site ID by Stack_ID
# det_cov <- merge(doy_unique_data, veg_unique_data, by.x = "Stack_ID", by.y = "Stack_ID")
# 
# # renaming site, lat, long, and removing site.x.y
# names(det_cov)[2] <- "Site"
# names(det_cov)[3] <- "Latitude"
# names(det_cov)[4] <- "Longitude"
# det_cov <- det_cov[,-6]

##############################
names(doy_unique_data)[2] <- "Site"
names(doy_unique_data)[3] <- "Latitude"
names(doy_unique_data)[4] <- "Longitude"

############################


# landscape covariates -------------- 

landscape_cov_dat <- read.csv("./occu_cov_2650.csv")
#View(landscape_cov_dat)
# combining landscape_cov_dat with det_cov 
site_covs <- merge(landscape_cov_dat, doy_unique_data, by.x = "Site", by.y = "Site")

# checking length
NROW(site_covs)

# creating new siteID
site_covs$New_SiteID <- 1:NROW(site_covs)

# take a look
#View(site_covs)

# distance covariates -------------- 

distance_covs <- read.csv("./distance_covs.csv")

# combining landscape_cov_dat with det_cov 
site_covs <- merge(site_covs, distance_covs, by.x = "Site", by.y = "Site")

# creating a proportion summary of grasslands
site_covs$prpGrassland <- site_covs$PasturePrp + site_covs$CRPPrp + site_covs$SGPPrp + site_covs$MGPPrp + site_covs$TGPPrp
site_covs$GrasslandNP <- site_covs$PastureNP + site_covs$CRPNP + site_covs$SGPNP + site_covs$MGPNP + site_covs$TGPNP





# Load digital elevation model
dem <- rast("D:/KansasGIS/KS_DEM/KS_DEM.tif")


# Convert siteXY to spatial points
site_coords <- SpatialPoints(coords = site_covs[, c("Longitude", "Latitude")], 
                             proj4string = CRS("+proj=longlat +datum=WGS84"))


# Convert the coordinates to a SpatVector object
site_pointvec <- vect(site_coords, type = "points", crs = crs(dem))

# Elevation
elev_values <- terra::extract(dem, site_pointvec)
elev_values <- as.data.frame(elev_values)
elev_values$Site <- site_covs$Site
head(elev_values)

# adding to site_covs
site_covs$Elev <- elev_values$KS_DEM

# Have to subset dem to extract slope, aspect, TRI, and VRM

# progress bar
pb <- progress_bar$new(
  format = "  Progress [:bar] :percent in :elapsed",
  total = NROW(site_covs$New_SiteID),    # Total number of iterations
  width = 60               # Width of the progress bar
)
site=1
# loop to extract
for (site in 1:NROW(site_covs[,'New_SiteID'])) {
  
  # Update the progress bar
  pb$tick()
  
  # subset the site
  site_sub <- site_covs[which(site_covs[,'New_SiteID'] == site),]
  
  # setting projection
  site_sub_coords <- SpatialPoints(coords = site_sub[, c("Longitude", "Latitude")], 
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # buffer the site
  site_buffer <- terra::buffer(site_sub_coords, width = 3000)
  
  # subset DEM
  dem_subset <- terra::crop(dem, site_buffer)

  # get slope
  slope_sub <- terra::terrain(x = dem_subset, v = "slope", unit = "degrees")
  
  # get aspect
  aspect_sub <- terra::terrain(x = dem_subset, v = "aspect")
  
  # get terrain ruggedness index
  tri_sub <- terra::terrain(x = dem_subset, v = "TRI")
  
  # get vector ruggedness
  vrm_sub <- spatialEco::vrm(dem_subset, s = 3)
  
  # Convert the coordinates to a SpatVector object
  site_pointvec <- vect(site_sub_coords, type = "points", crs = crs(dem_spat))
  
  # extract slope
  slope_values <- terra::extract(slope_sub, site_pointvec)
  
  # extract aspect
  aspect_values <- terra::extract(aspect_sub, site_pointvec)
  
  # extract terrain ruggedness index
  tri_values <- terra::extract(tri_sub, site_pointvec)
  
  # extract vector ruggedness values
  vrm_values <- terra::extract(vrm_sub, site_pointvec)
  
  # adding  values to site_covs
  site_covs[site,'Slope'] <- slope_values[,'slope']
  site_covs[site,'Aspect'] <- aspect_values[,'aspect']
  site_covs[site,'TRI'] <- tri_values[,'TRI']
  site_covs[site,'VRM'] <- vrm_values[,'lyr1']
}

# take a look
head(site_covs)
tail(site_covs)

#View(site_covs)

# -------------------------------------------------------
#
#                     Unmarked frame 
#
# -------------------------------------------------------

# creating an unmarked frame

deer_umf <- unmarkedFrameOccuMulti(y = ylist, 
                                   siteCovs = site_covs)


# -------------------------------------------------------
#
#               White-tail Detection Model 
#
# -------------------------------------------------------

# null model
wtd_detfit1 <- occuMulti(data = deer_umf,
                         detformulas = c("~1",
                                         "~1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)

# days active 
wtd_detfit2 <- occuMulti(data = deer_umf,
                         detformulas = c("~scale(DaysActive)", 
                                         "~1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)



# MajorRoadDis
wtd_detfit3 <- occuMulti(data = deer_umf,
                         detformulas = c("~ scale(MajorRoadDist)", 
                                         "~ 1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)

# TownsDist
wtd_detfit4 <- occuMulti(data = deer_umf,
                         detformulas = c("~ scale(TownsDist)", 
                                         "~ 1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)

# TownsDist + MajorRoadDis
wtd_detfit5 <- occuMulti(data = deer_umf,
                         detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist)", 
                                         "~ 1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)

# TownsDist + MajorRoadDis + DaysActive
wtd_detfit6 <- occuMulti(data = deer_umf,
                         detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + scale(DaysActive)", 
                                         "~ 1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)


# Creating a list of model names
wtdDET_model_names <- paste("wtd_detFit", as.character(1:6), sep = "")

# Calculating AIC for the list of models
wtd_det_model_aicc <- aictab(list(wtd_detfit1, wtd_detfit2, wtd_detfit3, wtd_detfit4, wtd_detfit5,
                                  wtd_detfit6), 
                             modnames = wtdDET_model_names)

print(wtd_det_model_aicc)

# Fit 2 was the most supported detection model for whitetail deer

# -------------------------------------------------------
#
#               Mule Deer Detection Model 
#
# -------------------------------------------------------

# null model
md_detfit1 <- occuMulti(data = deer_umf,
                        detformulas = c("~1",
                                        "~1"),
                        stateformulas = c("~1",
                                          "~1",
                                          "~1") 
)

# days active 
md_detfit2 <- occuMulti(data = deer_umf,
                        detformulas = c("~1", 
                                        "~ scale(DaysActive)"),
                        stateformulas = c("~1",
                                          "~1",
                                          "~1") 
)



# MajorRoadDis
md_detfit3 <- occuMulti(data = deer_umf,
                        detformulas = c("~1 ", 
                                        "~ scale(MajorRoadDist)"),
                        stateformulas = c("~1",
                                          "~1",
                                          "~1") 
)

# TownsDist
md_detfit4 <- occuMulti(data = deer_umf,
                        detformulas = c("~1 ", 
                                        "~ scale(TownsDist)"),
                        stateformulas = c("~1",
                                          "~1",
                                          "~1") 
)

# TownsDist + MajorRoadDis
md_detfit5 <- occuMulti(data = deer_umf,
                        detformulas = c("~1 ", 
                                        "~ scale(TownsDist) + scale(MajorRoadDist)"),
                        stateformulas = c("~1",
                                          "~1",
                                          "~1") 
)

# TownsDist + MajorRoadDis + DaysActive
md_detfit6 <- occuMulti(data = deer_umf,
                        detformulas = c("~1", 
                                        "~ scale(TownsDist) + scale(MajorRoadDist) + scale(DaysActive)"),
                        stateformulas = c("~1",
                                          "~1",
                                          "~1") 
)


# Creating a list of model names
md_det_model_names <- paste("detFit", as.character(1:6), sep = "")

# Calculating AIC for the list of models
md_det_model_aicc <- aictab(list(md_detfit1, md_detfit2, md_detfit3, md_detfit4, md_detfit5,
                                 md_detfit6), 
                            modnames = md_det_model_names)

print(md_det_model_aicc)

# Fit 5  was the most supported for mule deer detection

# -------------------------------------------------------
#
#             Cumulative Detection Probability 
#
# -------------------------------------------------------

# now that the covariates that are strongly associated for each species are found
# we can look at the cumulative detection probability for each species to decide how many
# survey occasions we should have

# Best detection model for both species
det_fit <- occuMulti(data = deer_umf,
                     detformulas = c("~ scale(DaysActive)", 
                                     "~ scale(TownsDist) + scale(MajorRoadDist)"),
                     stateformulas = c("~1",
                                       "~1",
                                       "~1"))


# Creating a new data frame with detection covariates
# View(site_covs)
newdata <- site_covs[,c(70 , 77:78)]


# Predicting detection using best detection model
# Whitetail deer
daily_p_wtd <- predict(det_fit, type = "det", species = "Whitetail_deer", newdata = newdata)

# Mule deer
daily_p_md <- predict(det_fit, type = "det", species = "mule_deer", newdata = newdata)

# Extract the predicted probabilities
p_det_wtd <- daily_p_wtd$Predicted
p_det_md <- daily_p_md$Predicted

# Number of survey days
n_days <- 28

# Calculate cumulative detection probability over 28 days
# Cumulative detection probability for a single species over n days is:
# P_cumulative = 1 - (1 - p)^n_days
cumulative_p_wtd <- 1 - cumprod(1 - p_det_wtd[1:n_days])
cumulative_p_md <- 1 - cumprod(1 - p_det_md[1:n_days])

# Create a data frame for plotting
plot_data <- data.frame(
  Day = 1:n_days,
  Cumulative_wTD = cumulative_p_wtd,
  Cumulative_MD = cumulative_p_md
)


# Plot the cumulative detection probabilities
ggplot(plot_data, aes(x = Day)) +
  geom_line(aes(y = Cumulative_wTD, color = "Whitetail Deer"), linewidth = 1) +
  geom_line(aes(y = Cumulative_MD, color = "Mule Deer"), linewidth = 1) +
  labs(y = "Cumulative Detection Probability", title = "") +
  scale_color_manual(name = "Legend", values = c("Whitetail Deer" = "purple", "Mule Deer" = "orange")) +
  scale_x_continuous(breaks = 1:n_days) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA) 
  )

# -------------------------------------------------------
#
#             Adjusting detection matrix 
#
# -------------------------------------------------------

# can see from the cumulative detection probability plot that the detection propability
# asymptotes at 28 days for whitetail and (almost) mule deer

# creating a matrix of 1 survey occasion


# Collapsing the detection matrix to 1 column and filling 1/0 if spp was detected within the 28 days
wtd_det_mat_1occ  <- as.matrix(apply(wtd_det_mat, 1, function(x) as.integer(any(x == 1))))
md_det_mat_1occ  <- as.matrix(apply(md_det_mat, 1, function(x) as.integer(any(x == 1))))

# creating new list
ylist_1survey <- list(Whitetail_deer = wtd_det_mat_1occ, mule_deer = md_det_mat_1occ)
str(ylist_1survey)
# Creating new unmarked frame
deer1occ_umf <- unmarkedFrameOccuMulti(y = ylist_1survey, 
                                       siteCovs = site_covs)


# Function to summarize data for every 14 days
summarize_14_days <- function(matrix_data) {
  num_days <- ncol(matrix_data)
  num_intervals <- ceiling(num_days / 14)
  
  summarized_matrix <- matrix(0, nrow = nrow(matrix_data), ncol = num_intervals)
  
  for (i in 1:num_intervals) {
    start_index <- (i - 1) * 14 + 1
    end_index <- min(i * 14, num_days)
    
    # Check if there is any 1 in the 14-day interval
    summarized_matrix[, i] <- apply(matrix_data[, start_index:end_index], 1, any)
  }
  
  return(summarized_matrix)
}

# Call the function to summarize the data
wtd_det_mat_14days <- summarize_14_days(wtd_det_mat)
md_det_mat_14days <- summarize_14_days(md_det_mat)

# creating new list
ylist_2surveys <- list(Whitetail_deer = wtd_det_mat_14days, mule_deer = md_det_mat_14days)

deer2occ_umf <- unmarkedFrameOccuMulti(y = ylist_2surveys,
                                       siteCovs = site_covs)



# -------------------------------------------------------
#
#      Whitetail Deer Detection Model 2 survey occasion
#
# -------------------------------------------------------


# null model
wtd_2oc_detfit1 <- occuMulti(data = deer2occ_umf,
                             detformulas = c("~1",
                                             "~1"),
                             stateformulas = c("~1",
                                               "~1",
                                               "~1") 
)

# days active 
wtd_2oc_detfit2 <- occuMulti(data = deer2occ_umf,
                             detformulas = c("~ scale(DaysActive)", 
                                             "~ 1"),
                             stateformulas = c("~ 1",
                                               "~ 1",
                                               "~ 1") 
)



# MajorRoadDis
wtd_2oc_detfit3 <- occuMulti(data = deer2occ_umf,
                             detformulas = c("~ scale(MajorRoadDist)", 
                                             "~ 1"),
                             stateformulas = c("~1",
                                               "~1",
                                               "~1") 
)

# TownsDist
wtd_2oc_detfit4 <- occuMulti(data = deer2occ_umf,
                             detformulas = c("~ scale(TownsDist)", 
                                             "~ 1"),
                             stateformulas = c("~1",
                                               "~1",
                                               "~1") 
)

# TownsDist + MajorRoadDis
wtd_2oc_detfit5 <- occuMulti(data = deer2occ_umf,
                             detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist)", 
                                             "~ 1"),
                             stateformulas = c("~1",
                                               "~1",
                                               "~1") 
)

# TownsDist + MajorRoadDis + DaysActive
wtd_2oc_detfit6 <- occuMulti(data = deer2occ_umf,
                             detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + scale(DaysActive)", 
                                             "~ 1"),
                             stateformulas = c("~1",
                                               "~1",
                                               "~1") 
)



# Creating a list of model names
wtd_2oc_DET_model_names <- paste("wtd_2oc_detFit", as.character(1:6), sep = "")

# Calculating AIC for the list of models
wtd_2oc_det_model_aicc <- aictab(list(wtd_2oc_detfit1, wtd_2oc_detfit2, wtd_2oc_detfit3, 
                                      wtd_2oc_detfit4, wtd_2oc_detfit5, wtd_2oc_detfit6),
                                 modnames = wtd_2oc_DET_model_names)

print(wtd_2oc_det_model_aicc)

# Can see that fit 6 has the lowest AIC but fit 2 is the most parsimonious model
# will use fit 2 for whitetail detection

# -------------------------------------------------------
#
#               Mule Deer 2occ Detection Model 
#
# -------------------------------------------------------


# null model
md_2oc_detfit1 <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~1",
                                            "~1"),
                            stateformulas = c("~1",
                                              "~1",
                                              "~1") 
)

# days active 
md_2oc_detfit2 <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~1", 
                                            "~scale(DaysActive)"),
                            stateformulas = c("~1",
                                              "~1",
                                              "~1") 
)



# MajorRoadDis
md_2oc_detfit3 <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~1", 
                                            "~ scale(MajorRoadDist)"),
                            stateformulas = c("~1",
                                              "~1",
                                              "~1") 
)

# TownsDist
md_2oc_detfit4 <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~1 ", 
                                            "~scale(TownsDist)"),
                            stateformulas = c("~1",
                                              "~1",
                                              "~1") 
)

# TownsDist + MajorRoadDis
md_2oc_detfit5 <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~ 1 ", 
                                            "~ scale(TownsDist) + scale(MajorRoadDist)"),
                            stateformulas = c("~1",
                                              "~1",
                                              "~1") 
)

# TownsDist + MajorRoadDis + DaysActive
md_2oc_detfit6 <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~ 1", 
                                            "~ scale(TownsDist) + scale(MajorRoadDist) + scale(DaysActive)"),
                            stateformulas = c("~1",
                                              "~1",
                                              "~1") 
)




# Creating a list of model names
md_2oc_det_model_names <- paste("detFit", as.character(1:6), sep = "")

# Calculating AIC for the list of models
md_2oc_det_model_aicc <- aictab(list(md_2oc_detfit1, md_2oc_detfit2, md_2oc_detfit3, 
                                     md_2oc_detfit4, md_2oc_detfit5, md_2oc_detfit6), 
                                modnames = md_2oc_det_model_names)

print(md_2oc_det_model_aicc)

# Fit 2 was the best model for detecting mule deer


# -------------------------------------------------------
#
#               Whitetail Deer Occupancy Model 
#
# -------------------------------------------------------



# Null model
wtd_2occ_occu_fit1 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~1",
                                                  "~1") 
)


# RowcropPrp
wtd_2occ_occu_fit2 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(RowcropPrp)",
                                                  "~1",
                                                  "~1") 
)

# RowcropPrp + RowcropNP
wtd_2occ_occu_fit3 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(RowcropPrp) + scale(RowcropNP)",
                                                  "~1",
                                                  "~1") 
)


# SandsagePrp
wtd_2occ_occu_fit4 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(SandsagePrp)",
                                                  "~1",
                                                  "~1") 
)

# SandsagePrp + SandsageNP
wtd_2occ_occu_fit5 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(SandsagePrp) + scale(SandsageNP)",
                                                  "~1",
                                                  "~1") 
)


# ForestPrp
wtd_2occ_occu_fit6 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp)",
                                                  "~1",
                                                  "~1") 
)

# ForestPrp + ForestNP
wtd_2occ_occu_fit7 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(ForestNP)",
                                                  "~1",
                                                  "~1") 
)


# ShrublandPrp 
wtd_2occ_occu_fit8 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + ShrublandNP
wtd_2occ_occu_fit9 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(ShrublandNP)",
                                                  "~1",
                                                  "~1") 
)

# prpGrassland
wtd_2occ_occu_fit10 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(prpGrassland)",
                                                   "~1",
                                                   "~1") 
)

# prpGrassland + GrasslandNP
wtd_2occ_occu_fit11 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(prpGrassland) + scale(GrasslandNP)",
                                                   "~1",
                                                   "~1") 
)

# prpGrassland + scale(SandsagePrp)
wtd_2occ_occu_fit12 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(prpGrassland) + scale(SandsagePrp)",
                                                   "~1",
                                                   "~1") 
)

# prpGrassland + scale(SandsagePrp) + GrasslandNP + SandsageNP
wtd_2occ_occu_fit13 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(prpGrassland) + scale(SandsagePrp)+
                                                   scale(GrasslandNP) + scale(SandsageNP)",
                                                   "~1",
                                                   "~1") 
)



# ShrublandPrp + ForestPrp
wtd_2occ_occu_fit14 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP)
wtd_2occ_occu_fit15 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + SandsagePrp
wtd_2occ_occu_fit16 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(SandsagePrp)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + SandsagePrp + scale(ShrublandNP) + scale(SandsageNP)
wtd_2occ_occu_fit17 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(SandsagePrp) + 
                                                  scale(ShrublandNP) + scale(SandsageNP)",
                                                  "~1",
                                                  "~1") 
)


# SandsagePrp + ForestPrp
wtd_2occ_occu_fit18 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(SandsagePrp) + scale(ForestPrp)",
                                                  "~1",
                                                  "~1") 
)



# SandsagePrp + ForestPrp + ForestNP + SandsageNP
wtd_2occ_occu_fit19 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(SandsagePrp) + scale(ForestPrp)+ 
                                                  scale(ForestNP) + scale(SandsageNP)",
                                                  "~1",
                                                  "~1") 
)

# MajorRoadDist
wtd_2occ_occu_fit20 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(MajorRoadDist)",
                                                  "~1",
                                                  "~1") 
)

# TownsDist
wtd_2occ_occu_fit21 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(TownsDist)",
                                                  "~1",
                                                  "~1") 
)

# TownsDist + MajorRoadDist
wtd_2occ_occu_fit22 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(TownsDist) + scale(MajorRoadDist)",
                                                  "~1",
                                                  "~1") 
)

# Elev
wtd_2occ_occu_fit23 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(Elev)",
                                                   "~1",
                                                   "~1") 
)



# Slope
wtd_2occ_occu_fit24 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(Slope)",
                                                  "~1",
                                                  "~1") 
)

# Aspect
wtd_2occ_occu_fit25 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(Aspect)",
                                                  "~1",
                                                  "~1") 
)

# TRI
wtd_2occ_occu_fit26 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(TRI)",
                                                  "~1",
                                                  "~1") 
)

# VRM
wtd_2occ_occu_fit27 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(VRM)",
                                                  "~1",
                                                  "~1") 
)

# VRM + Aspect
wtd_2occ_occu_fit28 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(VRM) + scale(Aspect)",
                                                  "~1",
                                                  "~1") 
)


# VRM + Slope
wtd_2occ_occu_fit29 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(VRM) + scale(Slope)",
                                                  "~1",
                                                  "~1") 
)

# VRM +  Elev
wtd_2occ_occu_fit30 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(VRM) + scale(Elev)",
                                                   "~1",
                                                   "~1") 
)


# Aspect + Slope
wtd_2occ_occu_fit31 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(Aspect) + scale(Slope)",
                                                  "~1",
                                                  "~1") 
)

# Aspect + Elev
wtd_2occ_occu_fit32 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(Aspect) + scale(Elev)",
                                                   "~1",
                                                   "~1") 
)

# Slope + Elev
wtd_2occ_occu_fit33 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(Aspect) + scale(Elev)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Slope)
wtd_2occ_occu_fit34 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Slope)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect)
wtd_2occ_occu_fit35 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(Aspect)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(TRI)
wtd_2occ_occu_fit36 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(TRI)",
                                                   "~1",
                                                   "~1") 
)                                 

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM)
wtd_2occ_occu_fit37 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(VRM)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM) + scale(Aspect)
wtd_2occ_occu_fit38 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                    scale(VRM) + scale(Aspect)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM) + scale(Slope)
wtd_2occ_occu_fit39 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(VRM) + scale(Slope)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM) + scale(Elev)
wtd_2occ_occu_fit40 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(VRM) + scale(Slope)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope)
wtd_2occ_occu_fit41 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(Aspect) + scale(Slope)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Elev)
wtd_2occ_occu_fit42 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(Aspect) + scale(Elev)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev)
wtd_2occ_occu_fit43 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) ",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Slope) + scale(Elev) + scale(VRM)
wtd_2occ_occu_fit44 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM) + scale(TownsDist)
wtd_2occ_occu_fit45 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(TownsDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM) + scale(MajorRoadDist)
wtd_2occ_occu_fit46 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM) + scale(TownsDist + scale(MajorRoadDist)
wtd_2occ_occu_fit47 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(MajorRoadDist) + scale(TownsDist)",
                                                   "~1",
                                                   "~1") 
)



# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
wtd_2occ_occu_fit48 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP)   + scale(Slope) + scale(Elev) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
wtd_2occ_occu_fit49 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                    scale(Slope) + scale(Elev) +scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Elev) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
wtd_2occ_occu_fit50 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Elev)+ scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
wtd_2occ_occu_fit51 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope)+ scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev) + 
# scale(TownsDist) + scale(MajorRoadDist)
wtd_2occ_occu_fit52 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) + 
#  scale(MajorRoadDist)
wtd_2occ_occu_fit53 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev)+ scale(VRM) +
                                                   scale(MajorRoadDist)",
                                                   "~1",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) + 
# scale(TownsDist)
wtd_2occ_occu_fit54 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) +scale(VRM) +
                                                   scale(TownsDist)",
                                                   "~1",
                                                   "~1") 
)


# ShrublandPrp + ShrublandNP + scale(VRM)
wtd_2occ_occu_fit55 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(ForestNP) +
                                                  scale(VRM)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(MajorRoadDist)
wtd_2occ_occu_fit56 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(MajorRoadDist)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(TownsDist)
wtd_2occ_occu_fit57 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~ scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(MajorRoadDist)",
                                                  "~1",
                                                  "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(MajorRoadDist) + scale(TownsDist)
wtd_2occ_occu_fit58 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(MajorRoadDist) + scale(TownsDist)",
                                                  "~1",
                                                  "~1") 
)


# Creating a list of model names
wtd_2occ_occu_model_names <- paste("wtd_2occ_occu_Fit", as.character(1:58), sep = "")

# Calculating AIC for the list of models
wtd_2occ_occu_model_aicc <- aictab(list(wtd_2occ_occu_fit1, wtd_2occ_occu_fit2, wtd_2occ_occu_fit3,
                                        wtd_2occ_occu_fit4, wtd_2occ_occu_fit5, wtd_2occ_occu_fit6,
                                        wtd_2occ_occu_fit7, wtd_2occ_occu_fit8, wtd_2occ_occu_fit9,
                                        wtd_2occ_occu_fit10, wtd_2occ_occu_fit11, wtd_2occ_occu_fit12,
                                        wtd_2occ_occu_fit13, wtd_2occ_occu_fit14, wtd_2occ_occu_fit15,
                                        wtd_2occ_occu_fit16, wtd_2occ_occu_fit17, wtd_2occ_occu_fit18,
                                        wtd_2occ_occu_fit19, wtd_2occ_occu_fit20, wtd_2occ_occu_fit21,
                                        wtd_2occ_occu_fit22, wtd_2occ_occu_fit23, wtd_2occ_occu_fit24,
                                        wtd_2occ_occu_fit25, wtd_2occ_occu_fit26, wtd_2occ_occu_fit27,
                                        wtd_2occ_occu_fit28, wtd_2occ_occu_fit29 , wtd_2occ_occu_fit30,
                                        wtd_2occ_occu_fit31, wtd_2occ_occu_fit32, wtd_2occ_occu_fit33,
                                        wtd_2occ_occu_fit34, wtd_2occ_occu_fit35, wtd_2occ_occu_fit36,
                                        wtd_2occ_occu_fit37, wtd_2occ_occu_fit38, wtd_2occ_occu_fit39,
                                        wtd_2occ_occu_fit40, wtd_2occ_occu_fit41, wtd_2occ_occu_fit42,
                                        wtd_2occ_occu_fit43, wtd_2occ_occu_fit44, wtd_2occ_occu_fit45,
                                        wtd_2occ_occu_fit46, wtd_2occ_occu_fit47, wtd_2occ_occu_fit48,
                                        wtd_2occ_occu_fit49, wtd_2occ_occu_fit50, wtd_2occ_occu_fit51,
                                        wtd_2occ_occu_fit52, wtd_2occ_occu_fit53, wtd_2occ_occu_fit54,
                                        wtd_2occ_occu_fit55, wtd_2occ_occu_fit56, wtd_2occ_occu_fit57,
                                        wtd_2occ_occu_fit58),
                                   modnames = wtd_2occ_occu_model_names)

print(wtd_2occ_occu_model_aicc)

# Fit 44 scale(ShrublandPrp) + scale(ForestPrp) + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM)
# is the best occupancy model for whitetail deer

# -------------------------------------------------------
#
#               Mule Deer Occupancy Model 
#
# -------------------------------------------------------



# Null model
md_2occ_occu_fit1 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~1",
                                                  "~1") 
)


# RowcropPrp
md_2occ_occu_fit2 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(RowcropPrp)",
                                                  "~1") 
)

# RowcropPrp + RowcropNP
md_2occ_occu_fit3 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(RowcropPrp) + scale(RowcropNP)",
                                                  "~1") 
)


# SandsagePrp
md_2occ_occu_fit4 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(SandsagePrp)",
                                                  "~1") 
)

# SandsagePrp + SandsageNP
md_2occ_occu_fit5 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(SandsagePrp) + scale(SandsageNP)",
                                                  "~1") 
)


# ForestPrp
md_2occ_occu_fit6 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ForestPrp)",
                                                  "~1") 
)

# ForestPrp + ForestNP
md_2occ_occu_fit7 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ForestPrp) + scale(ForestNP)",
                                                  "~1") 
)


# ShrublandPrp 
md_2occ_occu_fit8 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ShrublandPrp)",
                                                  "~1") 
)

# ShrublandPrp + ShrublandNP
md_2occ_occu_fit9 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ShrublandPrp) + scale(ShrublandNP)",
                                                  "~1") 
)

# prpGrassland
md_2occ_occu_fit10 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(prpGrassland)",
                                                   "~1") 
)

# prpGrassland + GrasslandNP
md_2occ_occu_fit11 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(prpGrassland) + scale(GrasslandNP)",
                                                   "~1") 
)

# prpGrassland + scale(SandsagePrp)
md_2occ_occu_fit12 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(prpGrassland) + scale(SandsagePrp)",
                                                   "~1") 
)

# prpGrassland + scale(SandsagePrp) + GrasslandNP + SandsageNP
md_2occ_occu_fit13 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(prpGrassland) + scale(SandsagePrp)+
                                                   scale(GrasslandNP) + scale(SandsageNP)",
                                                   "~1") 
)



# ShrublandPrp + ForestPrp
md_2occ_occu_fit14 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP)
md_2occ_occu_fit15 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP)",
                                                   "~1") 
)

# ShrublandPrp + SandsagePrp
md_2occ_occu_fit16 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(SandsagePrp)",
                                                   "~1") 
)

# ShrublandPrp + SandsagePrp + scale(ShrublandNP) + scale(SandsageNP)
md_2occ_occu_fit17 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(SandsagePrp) + 
                                                  scale(ShrublandNP) + scale(SandsageNP)",
                                                   "~1") 
)


# SandsagePrp + ForestPrp
md_2occ_occu_fit18 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(SandsagePrp) + scale(ForestPrp)",
                                                   "~1") 
)



# SandsagePrp + ForestPrp + ForestNP + SandsageNP
md_2occ_occu_fit19 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(SandsagePrp) + scale(ForestPrp)+ 
                                                  scale(ForestNP) + scale(SandsageNP)",
                                                   "~1") 
)

# MajorRoadDist
md_2occ_occu_fit20 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(MajorRoadDist)",
                                                   "~1") 
)

# TownsDist
md_2occ_occu_fit21 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(TownsDist)",
                                                   "~1") 
)

# TownsDist + MajorRoadDist
md_2occ_occu_fit22 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1") 
)
# Elev
md_2occ_occu_fit23 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(Elev)",
                                                   "~1") 
)



# Slope
md_2occ_occu_fit24 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(Slope)",
                                                   "~1") 
)

# Aspect
md_2occ_occu_fit25 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(Aspect)",
                                                   "~1") 
)

# TRI
md_2occ_occu_fit26 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(TRI)",
                                                   "~1") 
)

# VRM
md_2occ_occu_fit27 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(VRM)",
                                                   "~1") 
)

# VRM + Aspect
md_2occ_occu_fit28 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(VRM) + scale(Aspect)",
                                                   "~1") 
)


# VRM + Slope
md_2occ_occu_fit29 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(VRM) + scale(Slope)",
                                                   "~1") 
)

# VRM +  Elev
md_2occ_occu_fit30 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(VRM) + scale(Elev)",
                                                   "~1") 
)


# Aspect + Slope
md_2occ_occu_fit31 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(Aspect) + scale(Slope)",
                                                   "~1") 
)

# Aspect + Elev
md_2occ_occu_fit32 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(Aspect) + scale(Elev)",
                                                   "~1") 
)

# Slope + Elev
md_2occ_occu_fit33 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(Aspect) + scale(Elev)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Slope)
md_2occ_occu_fit34 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Slope)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect)
md_2occ_occu_fit35 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(Aspect)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(TRI)
md_2occ_occu_fit36 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(TRI)",
                                                   "~1") 
)                                 

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM)
md_2occ_occu_fit37 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(VRM)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM) + scale(Aspect)
md_2occ_occu_fit38 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                    scale(VRM) + scale(Aspect)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM) + scale(Slope)
md_2occ_occu_fit39 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(VRM) + scale(Slope)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(VRM) + scale(Elev)
md_2occ_occu_fit40 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(VRM) + scale(Elev)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope)
md_2occ_occu_fit41 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(Aspect) + scale(Slope)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Elev)
md_2occ_occu_fit42 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                   scale(Aspect) + scale(Elev)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev)
md_2occ_occu_fit43 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1 ",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Slope) + scale(Elev) + scale(VRM)
md_2occ_occu_fit44 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM) + scale(TownsDist)
md_2occ_occu_fit45 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(TownsDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM) + scale(MajorRoadDist)
md_2occ_occu_fit46 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Slope) + scale(Elev) + scale(VRM) + scale(TownsDist + scale(MajorRoadDist)
md_2occ_occu_fit47 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) + 
                                                   scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(MajorRoadDist) + scale(TownsDist)",
                                                   "~1") 
)



# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + 
# scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
md_2occ_occu_fit48 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP)   + scale(Slope) + scale(Elev) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
md_2occ_occu_fit49 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                    scale(Slope) + scale(Elev) +scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Elev) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
md_2occ_occu_fit50 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Elev)+ scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(VRM) + 
# scale(TownsDist) + scale(MajorRoadDist)
md_2occ_occu_fit51 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope)+ scale(VRM) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev) + 
# scale(TownsDist) + scale(MajorRoadDist)
md_2occ_occu_fit52 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) +
                                                   scale(TownsDist) + scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) + 
#  scale(MajorRoadDist)
md_2occ_occu_fit53 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev)+ scale(VRM) +
                                                   scale(MajorRoadDist)",
                                                   "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(Aspect) + scale(Slope) + scale(Elev) + scale(VRM) + 
# scale(TownsDist)
md_2occ_occu_fit54 <- occuMulti(data = deer2occ_umf,
                                 detformulas = c("~ scale(DaysActive)", 
                                                 "~ scale(DaysActive)"),
                                 stateformulas = c("~1",
                                                   "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(Aspect) + scale(Slope) + scale(Elev) +scale(VRM) +
                                                   scale(TownsDist)",
                                                   "~1") 
)

# ShrublandPrp + ShrublandNP + scale(VRM)
md_2occ_occu_fit55 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~scale(ShrublandPrp) + scale(ShrublandNP) +
                                                  scale(VRM)",
                                                 "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(MajorRoadDist)
md_2occ_occu_fit56 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(MajorRoadDist)",
                                                  "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(TownsDist)
md_2occ_occu_fit57 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(TownsDist)",
                                                  "~1") 
)

# ShrublandPrp + ForestPrp + scale(ShrublandNP) + scale(ForestNP) + scale(MajorRoadDist) + scale(TownsDist)
md_2occ_occu_fit58 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                                  scale(ShrublandNP) + scale(ForestNP) +
                                                  scale(MajorRoadDist) + scale(TownsDist)",
                                                  "~1") 
)



# Creating a list of model names
md_2occ_occu_model_names <- paste("md_2occ_occu_Fit", as.character(1:58), sep = "")

# Calculating AIC for the list of models
md_2occ_occu_model_aicc <- aictab(list(md_2occ_occu_fit1, md_2occ_occu_fit2, md_2occ_occu_fit3,
                                       md_2occ_occu_fit4, md_2occ_occu_fit5, md_2occ_occu_fit6,
                                       md_2occ_occu_fit7, md_2occ_occu_fit8, md_2occ_occu_fit9,
                                       md_2occ_occu_fit10, md_2occ_occu_fit11, md_2occ_occu_fit12,
                                       md_2occ_occu_fit13, md_2occ_occu_fit14, md_2occ_occu_fit15,
                                       md_2occ_occu_fit16, md_2occ_occu_fit17, md_2occ_occu_fit18,
                                       md_2occ_occu_fit19, md_2occ_occu_fit20, md_2occ_occu_fit21,
                                       md_2occ_occu_fit22, md_2occ_occu_fit23, md_2occ_occu_fit24,
                                       md_2occ_occu_fit25, md_2occ_occu_fit26, md_2occ_occu_fit27,
                                       md_2occ_occu_fit28, md_2occ_occu_fit29 , md_2occ_occu_fit30,
                                       md_2occ_occu_fit31, md_2occ_occu_fit32, md_2occ_occu_fit33,
                                       md_2occ_occu_fit34, md_2occ_occu_fit35, md_2occ_occu_fit36,
                                       md_2occ_occu_fit37, md_2occ_occu_fit38, md_2occ_occu_fit39,
                                       md_2occ_occu_fit40, md_2occ_occu_fit41, md_2occ_occu_fit42,
                                       md_2occ_occu_fit43, md_2occ_occu_fit44, md_2occ_occu_fit45,
                                       md_2occ_occu_fit46, md_2occ_occu_fit47, md_2occ_occu_fit48,
                                       md_2occ_occu_fit49, md_2occ_occu_fit50, md_2occ_occu_fit51,
                                       md_2occ_occu_fit52, md_2occ_occu_fit53, md_2occ_occu_fit54,
                                       md_2occ_occu_fit55, md_2occ_occu_fit56, md_2occ_occu_fit57,
                                       md_2occ_occu_fit58),
                                   modnames = md_2occ_occu_model_names)

print(md_2occ_occu_model_aicc)

# Fit 58 has the lowest AIC but
# fit 57 is the most parsimonious
# scale(ShrublandPrp) + scale(ForestPrp) + scale(ShrublandNP) + scale(ForestNP) +   scale(TownsDist)


# -------------------------------------------------------
#
#               Best Occupancy model
#
# -------------------------------------------------------

# fitting a occupancy model that was the 'best' for both species
occu_fit <- occuMulti(data = deer2occ_umf,
                      detformulas = c("~ scale(DaysActive)", 
                                      "~ scale(DaysActive)"),
                      stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                          scale(ShrublandNP) + scale(ForestNP) + 
                                          scale(Slope) + scale(Elev) + scale(VRM)",
                                        
                                        "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                        scale(ShrublandNP) + scale(ForestNP) + 
                                        scale(TownsDist)",
                                        "~1")) 

# -------------------------------------------------------
#
#               Conditional Occupancy Plots 
#
# -------------------------------------------------------

### species presence/absence

# wtd
wtd_md <- predict(occu_fit, type="state", species="Whitetail_deer", cond="mule_deer")
wtd_no_md <- predict(occu_fit, type="state", species="Whitetail_deer", cond="-mule_deer")

wtd_cond_data <- rbind(wtd_md[1,], wtd_no_md[1,])
wtd_cond_data$md_status <- c("Present","Absent")


plot(1:2, wtd_cond_data$Predicted, ylim=c(0,1), 
     xlim=c(0.5, 2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="Mule Deer status", ylab="Whitetail occupancy and 95% CI")
axis(1, at=1:2, labels=wtd_cond_data$md_status)

# CIs
top <- 0.1
for (i in 1:2){
  segments(i, wtd_cond_data$lower[i], i, wtd_cond_data$upper[i])
  segments(i-top, wtd_cond_data$lower[i], i+top)
  segments(i-top, wtd_cond_data$upper[i], i+top)
}



# md
md_wtd <- predict(occu_fit, type="state", species="mule_deer", cond="Whitetail_deer")
md_no_wtd <- predict(occu_fit, type="state", species="mule_deer", cond="-Whitetail_deer")

md_cond_data <- rbind(md_wtd[1,], md_no_wtd[1,])
md_cond_data$wtd_status <- c("Present","Absent")


plot(1:2, md_cond_data$Predicted, ylim=c(0,0.5), 
     xlim=c(0.5, 2.5), pch=19, cex=1.5, xaxt='n', 
     xlab="Whitetail Deer status", ylab="Mule Deer occupancy and 95% CI")
axis(1, at=1:2, labels=md_cond_data$wtd_status)

# CIs
top <- 0.1
for (i in 1:2){
  segments(i, md_cond_data$lower[i], i, md_cond_data$upper[i])
  segments(i-top, md_cond_data$lower[i], i+top)
  segments(i-top, md_cond_data$upper[i], i+top)
}

# -------------------------------------------------------
#
#               Marginal Occupancy Plots 
#                 i.e., habitat
#
# -------------------------------------------------------


### proportion forest

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
prpforest_range <- range(siteCovs(deer2occ_umf)$ForestPrp)
prpforests_seq <- seq(prpforest_range[1], prpforest_range[2], length.out=100)
prpForest_df <- data.frame(ForestPrp = prpforests_seq)

# occupancy model with just whitetail deer
forest_occu <- occuMulti(data = deer2occ_umf,
                         detformulas = c("~ scale(DaysActive)", 
                                         "~ scale(DaysActive)"),
                         stateformulas = c("~scale(ForestPrp)",
                                           "~scale(ForestPrp)",
                                           "~1")
) 

# white tail deer
occ_prpForest_wtd <- predict(forest_occu, type="state", 
                             species="Whitetail_deer", newdata = prpForest_df)


occ_prpForest_wtd$Species <- "Whitetail deer"
occ_prpForest_wtd$prpForest <- prpforests_seq
head(occ_prpForest_wtd)

# mule deer
occ_prpForest_md <- predict(forest_occu, type="state", 
                            species="mule_deer", newdata = prpForest_df)


occ_prpForest_md$Species <- "Mule Deer"
occ_prpForest_md$prpForest <- prpforests_seq
head(occ_prpForest_md)



# Plotting marginal occupancy in proportion forest
plot(occ_prpForest_wtd$prpForest, occ_prpForest_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="Proportion Forest", ylab="Marginal occupancy")

lines(occ_prpForest_md$prpForest, occ_prpForest_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))





### proportion shrubland

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
ShrublandPrp_range <- range(siteCovs(deer2occ_umf)$ShrublandPrp)
ShrublandPrp_seq <- seq(ShrublandPrp_range[1], ShrublandPrp_range[2], length.out=100)
ShrublandPrp_df <- data.frame(ShrublandPrp = ShrublandPrp_seq)

# occupancy model with just whitetail deer
shrubland_occu <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~ scale(DaysActive)", 
                                            "~ scale(DaysActive)"),
                            stateformulas = c("~scale(ShrublandPrp)",
                                              "~scale(ShrublandPrp)",
                                              "~1")
) 

# whitetail deer
occ_prpShrubland_wtd <- predict(shrubland_occu, type="state", 
                                species="Whitetail_deer", newdata = ShrublandPrp_df)


occ_prpShrubland_wtd$Species <- "Whitetail deer"
occ_prpShrubland_wtd$prpShrubland <- ShrublandPrp_seq
head(occ_prpShrubland_wtd)

# mule deer
occ_prpShrubland_md <- predict(shrubland_occu, type="state", 
                               species="mule_deer", newdata = ShrublandPrp_df)


occ_prpShrubland_md$Species <- "Mule Deer"
occ_prpShrubland_md$prpShrubland <- ShrublandPrp_seq
head(occ_prpShrubland_md)



# Plotting marginal occupancy in proportion shrubland
plot(occ_prpShrubland_wtd$prpShrubland, occ_prpShrubland_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="Proportion Shrubland", ylab="Marginal occupancy")

lines(occ_prpShrubland_md$prpShrubland, occ_prpShrubland_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))




### VRM

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
VRM_range <- range(siteCovs(deer2occ_umf)$VRM)
VRM_seq <- seq(VRM_range[1], VRM_range[2], length.out=100)
VRM_df <- data.frame(VRM = VRM_seq)

# occupancy model 
VRM_occu <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~ scale(DaysActive)", 
                                            "~ scale(DaysActive)"),
                            stateformulas = c("~scale(VRM)",
                                              "~scale(VRM)",
                                              "~1")
) 

# white tail deer
occ_VRM_wtd <- predict(VRM_occu, type="state", 
                                species="Whitetail_deer", newdata = VRM_df)


occ_VRM_wtd$Species <- "Whitetail deer"
occ_VRM_wtd$VRM <- VRM_seq
head(occ_VRM_wtd)

# mule deer
occ_VRM_md <- predict(VRM_occu, type="state", 
                               species="mule_deer", newdata = VRM_df)


occ_VRM_md$Species <- "Mule Deer"
occ_VRM_md$VRM <- VRM_seq
head(occ_VRM_md)



# Plotting marginal occupancy in VRM
plot(occ_VRM_wtd$VRM, occ_VRM_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="VRM", ylab="Marginal occupancy")

lines(occ_VRM_md$VRM, occ_VRM_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))


### TownsDist

TownsDist_range <- range(siteCovs(deer2occ_umf)$TownsDist)
TownsDist_seq <- seq(TownsDist_range[1], TownsDist_range[2], length.out=100)
TownsDist_df <- data.frame(TownsDist = TownsDist_seq)

TownsDist_occu <- occuMulti(data = deer2occ_umf,
                      detformulas = c("~ scale(DaysActive)", 
                                      "~ scale(DaysActive)"),
                      stateformulas = c("~scale(TownsDist)",
                                        "~scale(TownsDist)",
                                        "~1")
) 


# white tail deer
occ_TownsDist_wtd <- predict(TownsDist_occu, type="state", 
                       species="Whitetail_deer", newdata = TownsDist_df)


occ_TownsDist_wtd$Species <- "Whitetail deer"
occ_TownsDist_wtd$TownsDist <- TownsDist_seq
head(occ_TownsDist_wtd)

# mule deer
occ_TownsDist_md <- predict(TownsDist_occu, type="state", 
                      species="mule_deer", newdata = TownsDist_df)


occ_TownsDist_md$Species <- "Mule Deer"
occ_TownsDist_md$TownsDist <- TownsDist_seq
head(occ_TownsDist_md)


plot(occ_TownsDist_wtd$TownsDist, occ_TownsDist_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="TownsDist", ylab="Marginal occupancy")

lines(occ_TownsDist_md$TownsDist, occ_TownsDist_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))



# -------------------------------------------------------
#
#                       Predict 
#
# -------------------------------------------------------


install.packages("lattice")
library(lattice)

occu_fit <- occuMulti(data = deer2occ_umf,
                      detformulas = c("~ scale(DaysActive)", 
                                      "~ scale(DaysActive)"),
                      stateformulas = c("~scale(ShrublandPrp) + scale(ForestPrp) + 
                                          scale(ShrublandNP) + scale(ForestNP) + 
                                          scale(Slope) + scale(Elev) + scale(VRM)",
                                        
                                        "~scale(ShrublandPrp) + scale(ForestPrp) + 
                                        scale(ShrublandNP) + scale(ForestNP) + 
                                        scale(TownsDist)",
                                        "~1")) 

#View(site_covs)
pred_dat <- site_covs[, c(1, 12:13, 41,42, 68:69, 78, 91:92, 95 )]
head(pred_dat)


occuPred <- predict(occu_fit,
                    type = "state",
                    newdata = pred_dat,
                    na.rm = TRUE,
                    inf.rm = TRUE)


levelplot(Predicted ~ pred_dat$Latitude + pred_dat$Longitude, # need to extract values for new sites
          data = occuPred,
          col.regions = rev(terrain.colors(100)),
          at = seq(0,1,length.out=101))




# -------------------------------------------------------
#
#                       Dredge 
#
# -------------------------------------------------------


install.packages("MuMIn")
library(MuMIn)

## umf for wtd
wtd_umf <- unmarkedFrameOccu(y = wtd_det_mat_14days, 
                             siteCovs = site_covs)
## umf for md
md_umf <- unmarkedFrameOccu(y = md_det_mat_14days, 
                            siteCovs = site_covs)

# wtd global model
wtd_global <- occu(formula = 
                  # Detection
                  ~ scale(DaysActive)  
                
                # Occupancy
                ~ scale(RowcropPrp) + scale(FallowPrp) +
                  scale(WaterPrp) + scale(DevelopedPrp) +
                  scale(BarrenPrp) + scale(ForestPrp) +
                  scale(ShrublandPrp) + scale(WetlandsPrp) + 
                  scale(prpGrassland) + scale(RowcropNP)  +
                  scale(FallowNP)  + scale(WaNPrNP)  + 
                  scale(DevelopedNP) + scale(BarrenNP)  +
                  scale(ForestNP) + scale(ShrublandNP)  + 
                  scale(WetlandsNP)  + scale(GrasslandNP) #+
                  # scale(Slope) + scale(Aspect) + 
                  # scale(TRI) + scale(VRM) + scale(Elev)
                ,
                
                data = wtd_umf)

# wtd dredge
wtd_dredged <- dredge(global.model = wtd_global, 
                          evaluate = TRUE,
                          rank = "AIC")

print(wtd_dredged)


