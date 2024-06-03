# -------------------------------------------------------
#
#                    Load/install Packages
#
# -------------------------------------------------------


library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(unmarked)
library(AICcmodavg)
library(raster)
library(terra)
library(sp)
library(spatialEco)
library(progress)
library(psych)

set.seed(123)
options(scipen = 9999)
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
max(ks_dat$DateTime)

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

write.csv(wtd_det_mat, "wtd_det_mat.csv")


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


# reading in vegetation data
veg_cov <- read.csv("./KansasCamera_cov.csv")

# Sort by site and then by date
ordered_indices <- order(veg_cov$Site, veg_cov$SurveyYear)

# reorder  data frame
veg_cov <- veg_cov[ordered_indices, ]

# Adding a column based on year for survey
 veg_cov$Survey <- ifelse(veg_cov$SurveyYear == "2018", 1,
                          ifelse(veg_cov$SurveyYear == "2019", 2,
                                 ifelse(veg_cov$SurveyYear == "2020", 3, NA)))


# Creating a stacked site column
veg_cov$Stack_ID <- paste(veg_cov$Site, veg_cov$Survey, sep = ".")

# Merging doy cov and veg cov into a single dataframe for detection
# merging new site ID by Stack_ID
 det_cov <- merge(doy_unique_data, veg_cov, by.x = "Stack_ID", by.y = "Stack_ID")

# renaming site, lat, long, and removing site.x.y
 names(det_cov)[2] <- "Site"
 names(det_cov)[3] <- "Latitude"
 names(det_cov)[4] <- "Longitude"



# landscape covariates -------------- 

landscape_cov_dat <- read.csv("./occu_cov_2650.csv")
 
# combining landscape_cov_dat with det_cov 
site_merge <- inner_join(ks_dat, landscape_cov_dat, by = "Site")

# extracting values
unique_values <- site_merge %>%
  distinct(Stack_ID, .keep_all = TRUE)

# setting stack id as numeric
unique_values$Stack_ID <- as.numeric(unique_values$Stack_ID)

# ordering unique values
unique_values <- unique_values %>%
  arrange(Stack_ID)

# extracting site covs
#View(unique_values)
site_covs <- unique_values[,c(1, 10:11, 16:17, 21:88)]

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

# merging site_covs and det_covs
covs <- merge(site_covs, det_cov, by = "Stack_ID")

# renaming site, lat, long, and removing extra site, lat and long
names(covs)[2] <- "Site"
names(covs)[4] <- "Latitude"
names(covs)[5] <- "Longitude"
covs <- covs[,-c(99:101, 104:106, 113)] 
head(covs)

# replacing NAs with 0
covs <- covs %>%
  mutate_all(~coalesce(., 0))

# take a look
head(covs)

NROW(unique(colnames(covs)))

# -------------------------------------------------------
#
#                     Unmarked frame 
#
# -------------------------------------------------------

# creating an unmarked frame
deer_umf <- unmarkedFrameOccuMulti(y = ylist,
                                   siteCovs = covs)

# -------------------------------------------------------
#
#         Detection covariates correlation
#
# -------------------------------------------------------

# detection covs are: DaysActive, VegHeight, GrassPct, ForbPct, CropPct, OpenGroundPct, ShrubPct
pairs.panels(covs[c(99, 101:106)],
                         gap = 0,
                         bg = c("blue", "red"),
                         pch = 21, main = "Det Covs Corr")

# can see a strong negative correlation between Vegheight and GrassPCT with opengroundpct
# will not use opengroundpct in models


# -------------------------------------------------------
#
#               White-tail Detection Model 
#
# -------------------------------------------------------

# null model
wtd_detfit1 <- occuMulti(data = deer_umf,
                         detformulas = c(
                                           # Whitetail Deer
                                           "~1",
                                           # Mule Deer
                                           "~1"),
                         stateformulas = c(
                                           # Whitetail Deer
                                           "~1",
                                           # Mule Deer
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



# VegHeight
wtd_detfit3 <- occuMulti(data = deer_umf,
                         detformulas = c("~ scale(VegHeight)", 
                                         "~ 1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)

# DaysActive + VegHeight
wtd_detfit4 <- occuMulti(data = deer_umf,
                         detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                         "~ 1"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)


# DaysActive, VegHeight, GrassPct, ForbPct, CropPct, OpenGroundPct, ShrubPct

# Creating a list of model names
wtdDET_model_names <- paste("wtd_detFit", as.character(1:4), sep = "")

# Calculating AIC for the list of models
wtd_det_model_aicc <- aictab(list(wtd_detfit1, wtd_detfit2, wtd_detfit3, wtd_detfit4), 
                             modnames = wtdDET_model_names)

print(wtd_det_model_aicc)

# Fit 4 (scale(DaysActive) + scale(VegHeight)) was the most supported detection model for whitetail deer

# -------------------------------------------------------
#
#               Mule Deer Detection Model 
#
# -------------------------------------------------------

# null model
md_detfit1 <- occuMulti(data = deer_umf,
                        detformulas = c(
                                          # Whitetail Deer
                                          "~1",
                                          # Mule Deer
                                          "~1"),
                        stateformulas = c(
                                          # Whitetail Deer
                                          "~1",
                                          # Mule Deer
                                          "~1",
                                          "~1") 
)


# days active 
md_detfit2 <- occuMulti(data = deer_umf,
                         detformulas = c("~1", 
                                         "~scale(DaysActive)"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)



# VegHeight
md_detfit3 <- occuMulti(data = deer_umf,
                         detformulas = c("~ 1", 
                                         "~ scale(VegHeight)"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)

# DaysActive + VegHeight
md_detfit4 <- occuMulti(data = deer_umf,
                         detformulas = c("~ 1", 
                                         "~ scale(DaysActive) + scale(VegHeight)"),
                         stateformulas = c("~1",
                                           "~1",
                                           "~1") 
)



# Creating a list of model names
md_det_model_names <- paste("detFit", as.character(1:4), sep = "")

# Calculating AIC for the list of models
md_det_model_aicc <- aictab(list(md_detfit1, md_detfit2, md_detfit3, md_detfit4), 
                            modnames = md_det_model_names)

print(md_det_model_aicc)

# Fit 2 (DaysActive ) was the most supported for mule deer detection

# -------------------------------------------------------
#
#             Cumulative Detection Probability 
#
# -------------------------------------------------------

# Looking into how many survey occasions should be used to fit occupancy models

# Best detection model for both species
det_fit <- occuMulti(data = deer_umf,
                     detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                     "~ scale(DaysActive)"),
                     stateformulas = c("~1",
                                       "~1",
                                       "~1"))


# Creating a new data frame with detection covariates
# View(covs)
newdata <- covs[,c(99, 101)]


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
  geom_smooth(aes(y = Cumulative_wTD, color = "Whitetail Deer"), method = "loess", se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = Cumulative_MD, color = "Mule Deer"), method = "loess", se = FALSE, linewidth = 1) +
  labs(y = "Cumulative Detection Probability\n", x = "\n Survey Day", title = "") +
  scale_color_manual(name = "Legend", values = c("Whitetail Deer" = "purple", "Mule Deer" = "orange")) +
  scale_x_continuous(breaks = 1:n_days) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA) 
  )

# Line at day 14
ggplot(plot_data, aes(x = Day)) +
  geom_smooth(aes(y = Cumulative_wTD, color = "Whitetail Deer"), method = "loess", se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = Cumulative_MD, color = "Mule Deer"), method = "loess", se = FALSE, linewidth = 1) +
  geom_vline(xintercept = 14, linetype = "dashed", color = "Black", linewidth = 1) +
  labs(y = "Cumulative Detection Probability\n", x = "\n Survey Day", title = "") +
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
                                       siteCovs = covs)


# -------------------------------------------------------
#
#             State Covariate Correlation
#
# -------------------------------------------------------

View(covs)

# RowcropPrp FallowPrp PasturePrp CRPPrp SGPPrp MGPPrp TGPPrp SandsagePrp
# RowcropNP FallowNP CRPNP SGPNP MGPNP TGPNP SandsageNP
pairs.panels(covs[c(14:15, 23:27, 43:44, 52:56)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")

plot(covs[,c(19, 48)]) # Forest
pairs.panels(covs[c(19, 48)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")

plot(covs[,c(25, 54)]) # mixed grass
pairs.panels(covs[c(25, 54)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")

# FallowPrp BarrenPrp
# FallowNP BarrenNP
pairs.panels(covs[c(15, 18, 44, 47)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")



# can see some high negative correlation between Rowcrop and SGP & MGP 
# and also between Fallow and SGP & MGP

# ForestPrp ShrublandPrp
# ForestNP ShrublandNP
pairs.panels(covs[c(19:20, 48:49)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")

# proportion and number of patches are highly correlated with each other in the same 
# land cover type


# WindTurbineDist PrdOilWellsDist StreamsDist PondDist RuralRoadDist MajorRoadDist
#  TownsDist IrrigationPivotDist RoadDensity PivotDensity StreamDensity
pairs.panels(covs[c(75:85)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")


# none of the distances are correlated together 
# towndist and roaddensity are corrlated
# streamdist is correlated with stream density and pivot density
# pivot density is correlated with road density
# road density is correlated with irrigationpivot density

# Elev    Slope   Aspect       TRI           VR
pairs.panels(covs[c(94:96, 98)],
             gap = 0,
             bg = c("blue", "red"),
             pch = 21, main = "")

# slope is corrlated with TRI and VRM
# TRI and VRM are correlated 

# -------------------------------------------------------
#
#                       Dredge  ############### DELETE section
#
# -------------------------------------------------------

 
library(MuMIn)

## umf for wtd
wtd_umf <- unmarkedFrameOccu(y = wtd_det_mat_1occ, 
                             siteCovs = covs)
## umf for md
md_umf <- unmarkedFrameOccu(y = md_det_mat_1occ, 
                            siteCovs = covs)

 
#######################################################################
# md global model
md_prp_occu <- occu(formula = 
                       # Detection
                       ~ scale(DaysActive)   
                     
                     # Occupancy
                     ~ scale(CRPPrp) + scale(SGPPrp) +
                       scale(MGPPrp) + scale(TGPPrp) +
                       scale(SandsagePrp)
                     ,
                     
                     data = wtd_umf)

# wtd dredge
md_dredged <- dredge(global.model = md_prp_occu, 
                      evaluate = TRUE,
                      rank = "AIC")

print(md_dredged)



# -------------------------------------------------------
#
#               Whitetail Deer Occupancy Model 
#
# -------------------------------------------------------

#####  Whitetail deer
# scale(Slope) + scale(Elev)
# scale(VRM)
 
 
 

# scale(MajorRoadDist) + scale(TownsDist)
# scale(MajorRoadDist) 
# scale(TownsDist)
# scale(MGPPrp)
# scale(MGPPrp) + scale(SGPPrp)


# Null model
wtd_2occ_occu_fit1 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~1",
                                                  "~1",
                                                  "~1") 
)

# Global model
wtd_2occ_occu_fit2 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) +  scale(BarrenPrp) +
                                                  scale(FallowPrp) + scale(MGPPrp) + 
                                                  scale(SGPPrp) + scale(WindTurbineDist) + 
                                                  scale(PrdOilWellsDist) +  scale(MajorRoadDist) + 
                                                  scale(TownsDist) + scale(Slope) + scale(Elev) ",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(Slope) + scale(Elev)
wtd_2occ_occu_fit3 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(Slope) + 
                                                  scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(BarrenPrp) + scale(Slope) + scale(Elev)
wtd_2occ_occu_fit4 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(BarrenPrp) + 
                                                  scale(Slope) + scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(BarrenPrp) + scale(Slope) + scale(Elev) + scale(MGPPrp)
wtd_2occ_occu_fit5 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~ scale(ForestPrp) + scale(BarrenPrp) + 
                                                  scale(Slope) + scale(Elev) + scale(MGPPrp)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(BarrenPrp) + scale(Slope) + scale(Elev) + scale(MGPPrp) + scale(ShrublandPrp)
wtd_2occ_occu_fit6 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(BarrenPrp) + 
                                                  scale(Slope) + scale(Elev) + scale(MGPPrp) + 
                                                  scale(ShrublandPrp)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(BarrenPrp) + scale(Slope) + scale(Elev) + scale(MGPPrp) + scale(WindTurbineDist) 
wtd_2occ_occu_fit7 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(BarrenPrp) + 
                                                  scale(Slope) + scale(Elev) + scale(MGPPrp) + 
                                                  scale(WindTurbineDist) ",
                                                  "~1",
                                                  "~1") 
)



# scale(ForestPrp) + scale(BarrenPrp) + scale(FallowPrp)
wtd_2occ_occu_fit8 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(BarrenPrp) + 
                                                  scale(FallowPrp) + scale(Slope) + scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(MGPPrp)
wtd_2occ_occu_fit9 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(MGPPrp) +
                                                  scale(Slope) + scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(MGPPrp) + scale(SGPPrp)
wtd_2occ_occu_fit10 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(MGPPrp) +
                                                  scale(SGPPrp) + scale(Slope) + scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(MGPPrp) + scale(SGPPrp) + scale(WindTurbineDist) + scale(PrdOilWellsDist)
wtd_2occ_occu_fit11 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(MGPPrp) +
                                                  scale(SGPPrp) + scale(WindTurbineDist) + 
                                                  scale(PrdOilWellsDist) + scale(Slope) + scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(MGPPrp) + scale(SGPPrp) + scale(WindTurbineDist) 
wtd_2occ_occu_fit12 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(MGPPrp) +
                                                  scale(SGPPrp) + scale(WindTurbineDist) + 
                                                  scale(Slope) + scale(Elev) ",
                                                  "~1",
                                                  "~1") 
)

# scale(ForestPrp) + scale(MGPPrp) + scale(SGPPrp) + scale(PrdOilWellsDist)
wtd_2occ_occu_fit13 <- occuMulti(data = deer2occ_umf,
                                detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                                "~ scale(DaysActive)"),
                                stateformulas = c("~scale(ForestPrp) + scale(MGPPrp) +
                                                  scale(SGPPrp) + scale(PrdOilWellsDist) + 
                                                  scale(Slope) + scale(Elev)",
                                                  "~1",
                                                  "~1") 
)

# Creating a list of model names
wtd_2occ_occu_model_names <- paste("wtd_2occ_occu_Fit", as.character(1:13), sep = "")

# Calculating AIC for the list of models
wtd_2occ_occu_model_aicc <- aictab(list(wtd_2occ_occu_fit1, wtd_2occ_occu_fit2, wtd_2occ_occu_fit3,
                                        wtd_2occ_occu_fit4, wtd_2occ_occu_fit5, wtd_2occ_occu_fit6,
                                        wtd_2occ_occu_fit7, wtd_2occ_occu_fit8, wtd_2occ_occu_fit9,
                                        wtd_2occ_occu_fit10, wtd_2occ_occu_fit11, wtd_2occ_occu_fit12,
                                        wtd_2occ_occu_fit13),
                                   modnames = wtd_2occ_occu_model_names)

print(wtd_2occ_occu_model_aicc)

# Fit 5 scale(ForestPrp) + scale(BarrenPrp) + scale(Slope) + scale(Elev) + scale(MGPPrp)
# is the best occupancy model for whitetail deer




# -------------------------------------------------------
#
#               Mule Deer Occupancy Model 
#
# -------------------------------------------------------

#####  Mule deer

# scale(DevelopedPrp) + scale(MajorRoadDist) + scale(TownsDist)
# scale(IrrigationPivotDist) + scale(PondDist) + scale(PrdOilWellsDist) + scale(WindTurbineDist) + scale(StreamsDist)
# scale(Elev) + scale(Slope) + scale(Aspect)
# scale(Elev) + scale(VRM) + scale(Aspect)
# scale(ShrublandPrp)
# scale(PasturePrp) + scale(BarrenPrp) + scale(FallowPrp)
# scale(MGPPrp)
# scale(MGPPrp) + scale(CRPPrp) + scale(SandsagePrp)


# Null model
md_2occ_occu_fit1 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~1",
                                                 "~1") 
)

# Global model
md_2occ_occu_fit2 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~scale(DevelopedPrp) + scale(MajorRoadDist) + 
                                                 scale(TownsDist) + scale(IrrigationPivotDist) + 
                                                 scale(PondDist) + scale(PrdOilWellsDist) + 
                                                 scale(WindTurbineDist) + scale(StreamsDist) + 
                                                 scale(Elev) + scale(Slope) + scale(Aspect) + 
                                                 scale(ShrublandPrp) + scale(PasturePrp) + 
                                                 scale(BarrenPrp) + scale(FallowPrp) + 
                                                 scale(MGPPrp) + scale(CRPPrp) + scale(SandsagePrp)",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist) + scale(StreamsDist) + scale(MGPPrp) + scale(CRPPrp) + scale(Elev) + scale(Aspect) +
# scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit3 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(StreamsDist) + scale(MGPPrp) + 
                                                 scale(CRPPrp) + scale(Elev) + scale(Aspect) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist) + scale(StreamsDist) + scale(MGPPrp) + scale(Aspect) +
# scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit4 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(StreamsDist) + scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist) + scale(StreamsDist) + scale(MGPPrp) + scale(Aspect) +scale(BarrenPrp)  
md_2occ_occu_fit5 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(StreamsDist) + scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp)  
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist)  + scale(MGPPrp) + scale(Aspect) + scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit6 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist)  + scale(MGPPrp) + scale(Aspect) + scale(BarrenPrp)
md_2occ_occu_fit7 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MGPPrp) + scale(Aspect) + scale(BarrenPrp)
md_2occ_occu_fit7 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + 
                                                 scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(StreamsDist) + scale(MGPPrp) + scale(Aspect) +
# scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit8 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + 
                                                 scale(StreamsDist) + scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist) + scale(StreamsDist) + scale(MGPPrp) + scale(Elev) + scale(Slope) +
# scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit9 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(StreamsDist) + scale(MGPPrp) + 
                                                 scale(Elev) + scale(Slope) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MajorRoadDist) + scale(StreamsDist) + scale(MGPPrp) + scale(Elev) + 
# scale(VRM) + scale(Aspect) + # scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit10 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + scale(MajorRoadDist) + 
                                                 scale(StreamsDist) + scale(MGPPrp) + 
                                                 scale(Elev) + scale(Slope) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)

# scale(ShrublandPrp) + scale(MGPPrp) + scale(Aspect) +
# scale(BarrenPrp) + scale(PasturePrp)
md_2occ_occu_fit11 <- occuMulti(data = deer2occ_umf,
                               detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                               "~ scale(DaysActive)"),
                               stateformulas = c("~1",
                                                 "~ scale(ShrublandPrp) + 
                                                 scale(MGPPrp) + 
                                                 scale(Aspect) +
                                                 scale(BarrenPrp) + scale(PasturePrp)
                                                 ",
                                                 "~1") 
)


# Creating a list of model names
md_2occ_occu_model_names <- paste("md_2occ_occu_Fit", as.character(1:11), sep = "")

# Calculating AIC for the list of models
md_2occ_occu_model_aicc <- aictab(list(md_2occ_occu_fit1, md_2occ_occu_fit2, md_2occ_occu_fit3,
                                       md_2occ_occu_fit4, md_2occ_occu_fit5, md_2occ_occu_fit6,
                                       md_2occ_occu_fit7, md_2occ_occu_fit8, md_2occ_occu_fit9,
                                       md_2occ_occu_fit10, md_2occ_occu_fit11),
                                  modnames = md_2occ_occu_model_names)

print(md_2occ_occu_model_aicc)

# Fit 8 # scale(ShrublandPrp) + scale(StreamsDist) + scale(MGPPrp) + scale(Aspect) +scale(BarrenPrp) + scale(PasturePrp)

# was the best model for mule deer

# -------------------------------------------------------
#
#               Best Occupancy model
#
# -------------------------------------------------------

# fitting a occupancy model that was the 'best' for both species
best_occu_fit <- occuMulti(data = deer2occ_umf,
                      detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                      "~ scale(DaysActive)"),
                      
                      stateformulas = c("~scale(ForestPrp) + scale(BarrenPrp) + 
                                        scale(Slope) + scale(Elev) + scale(MGPPrp)",
                                        
                                        "~scale(ShrublandPrp) + scale(StreamsDist) + 
                                        scale(MGPPrp) + scale(Aspect) + 
                                        scale(BarrenPrp) + scale(PasturePrp)",
                                        "~1")) 

# for beta estimate plot
occu_fit <- occuMulti(data = deer2occ_umf,
                      detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
                                      "~ scale(DaysActive)"),
                      
                      stateformulas = c("~scale(ForestPrp) + scale(BarrenPrp) + 
                                        scale(Slope) + scale(Elev) + scale(MGPPrp)+ 
                                        scale(Aspect) + scale(ShrublandPrp) + 
                                        scale(StreamsDist) + scale(PasturePrp)",
                                        
                                        "~scale(ShrublandPrp) + scale(StreamsDist) + 
                                        scale(MGPPrp) + scale(Aspect) + 
                                        scale(BarrenPrp) + scale(PasturePrp) + 
                                        scale(ForestPrp) + 
                                        scale(Slope) + scale(Elev)",
                                        "~1"))


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Extract occupancy estimates and SEs from the occu_fit object
occupancy_estimates <- summary(occu_fit)$state

# Combine occupancy estimates into one data frame
occupancy_estimates <- data.frame(
  Estimate = occupancy_estimates$Estimate,
  SE = occupancy_estimates$SE,
  z = occupancy_estimates$z,
  P_value = occupancy_estimates$`P(>|z|)`,
  Parameter = rownames(occupancy_estimates)
)

# Add confidence intervals
occupancy_estimates <- occupancy_estimates %>%
  mutate(
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE
  )

# Filter out the intercept
occupancy_estimates <- occupancy_estimates %>%
  filter(!grepl("Intercept", Parameter))

# Separate estimates for mule deer and white-tail deer, and rename parameters
white_tail_estimates <- occupancy_estimates %>%
  filter(grepl("Whitetail_deer", Parameter) & !grepl("Whitetail_deer:mule_deer", Parameter)) %>%
  mutate(Species = "Whitetail deer")

mule_deer_estimates <- occupancy_estimates %>%
  filter(grepl("mule_deer", Parameter) & !grepl("Whitetail_deer:mule_deer", Parameter)) %>%
  mutate(Species = "Mule deer")

# Extract the parameter names without species and "scale()"
white_tail_estimates$Parameter <- gsub("\\[Whitetail_deer\\] scale\\(", "", white_tail_estimates$Parameter)
white_tail_estimates$Parameter <- gsub("\\)", "", white_tail_estimates$Parameter)
mule_deer_estimates$Parameter <- gsub("\\[mule_deer\\] scale\\(", "", mule_deer_estimates$Parameter)
mule_deer_estimates$Parameter <- gsub("\\)", "", mule_deer_estimates$Parameter)

# Combine estimates for plotting
combined_estimates <- rbind(white_tail_estimates, mule_deer_estimates)

# Plot the occupancy estimates using ggplot2 with custom colors and a horizontal line at 0
ggplot(combined_estimates, aes(x = Parameter, y = Estimate, color = Species)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() + # Flip coordinates for better readability
  theme_minimal() +
  scale_color_manual(values = c("Whitetail deer" = "purple", "Mule deer" = "orange")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Add a horizontal line at 0
  labs(title = "Occupancy Beta Estimates and 95% CI",
       x = "Parameter",
       y = "Estimate") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "right",  # Keep legend on the right
        legend.margin = margin(r = 20))  # Remove minor gridlines



 

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

# Plotting
ggplot(wtd_cond_data, aes(x = md_status, y = Predicted)) +
  geom_point(size = 3, shape = 19, color = "purple") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color = "purple") +
  ylim(0, 1) +
  xlab("\nMule Deer status") +
  ylab("Whitetail Deer Conditional Occupancy \n") +
  ggtitle("") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")  # Remove minor gridlines

# md
md_wtd <- predict(occu_fit, type="state", species="mule_deer", cond="Whitetail_deer")
md_no_wtd <- predict(occu_fit, type="state", species="mule_deer", cond="-Whitetail_deer")

md_cond_data <- rbind(md_wtd[1,], md_no_wtd[1,])
md_cond_data$wtd_status <- c("Present","Absent")

# 
# plot(1:2, md_cond_data$Predicted, ylim=c(0,1), 
#      xlim=c(0.5, 2.5), pch=19, cex=1.5, xaxt='n', 
#      xlab="Whitetail Deer status", ylab="Mule Deer occupancy and 95% CI")
# axis(1, at=1:2, labels=md_cond_data$wtd_status)
# 
# # CIs
# top <- 0.1
# for (i in 1:2){
#   segments(i, md_cond_data$lower[i], i, md_cond_data$upper[i])
#   segments(i-top, md_cond_data$lower[i], i+top)
#   segments(i-top, md_cond_data$upper[i], i+top)
# }

# Plotting
ggplot(md_cond_data, aes(x = wtd_status, y = Predicted)) +
  geom_point(size = 3, shape = 19, color = "orange") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = "orange"), width = 0.1) +
  ylim(0, 1) +
  xlab("\nWhitetail Deer status") +
  ylab("Mule Deer Conditional Occupancy \n") +
  ggtitle("") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),
        legend.position = "none")  # Remove minor gridlines

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
                         detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
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
                            detformulas = c("~ scale(DaysActive) + scale(VegHeight)", 
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




### Slope

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
Slope_range <- range(siteCovs(deer2occ_umf)$Slope)
Slope_seq <- seq(Slope_range[1], Slope_range[2], length.out=100)
Slope_df <- data.frame(Slope = Slope_seq)

# occupancy model 
Slope_occu <- occuMulti(data = deer2occ_umf,
                      detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                      "~ scale(DaysActive)"),
                      stateformulas = c("~scale(Slope)",
                                        "~scale(Slope)",
                                        "~1")
) 

# white tail deer
occ_Slope_wtd <- predict(Slope_occu, type="state", 
                       species="Whitetail_deer", newdata = Slope_df)


occ_Slope_wtd$Species <- "Whitetail deer"
occ_Slope_wtd$Slope <- Slope_seq
head(occ_Slope_wtd)

# mule deer
occ_Slope_md <- predict(Slope_occu, type="state", 
                      species="mule_deer", newdata = Slope_df)


occ_Slope_md$Species <- "Mule Deer"
occ_Slope_md$Slope <- Slope_seq
head(occ_Slope_md)



# Plotting marginal occupancy 
plot(occ_Slope_wtd$Slope, occ_Slope_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="Slope", ylab="Marginal occupancy")

lines(occ_Slope_md$Slope, occ_Slope_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))


### Elev

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
Elev_range <- range(siteCovs(deer2occ_umf)$Elev)
Elev_seq <- seq(Elev_range[1], Elev_range[2], length.out=100)
Elev_df <- data.frame(Elev = Elev_seq)

# occupancy model 
Elev_occu <- occuMulti(data = deer2occ_umf,
                        detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                        "~ scale(DaysActive)"),
                        stateformulas = c("~scale(Elev)",
                                          "~scale(Elev)",
                                          "~1")
) 

# white tail deer
occ_Elev_wtd <- predict(Elev_occu, type="state", 
                         species="Whitetail_deer", newdata = Elev_df)


occ_Elev_wtd$Species <- "Whitetail deer"
occ_Elev_wtd$Elev <- Elev_seq
head(occ_Elev_wtd)

# mule deer
occ_Elev_md <- predict(Elev_occu, type="state", 
                        species="mule_deer", newdata = Elev_df)


occ_Elev_md$Species <- "Mule Deer"
occ_Elev_md$Elev <- Elev_seq
head(occ_Elev_md)



# Plotting marginal occupancy 
plot(occ_Elev_wtd$Elev, occ_Elev_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="Elevation", ylab="Marginal occupancy")

lines(occ_Elev_md$Elev, occ_Elev_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))



### Aspect

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
Aspect_range <- range(siteCovs(deer2occ_umf)$Aspect)
Aspect_seq <- seq(Aspect_range[1], Aspect_range[2], length.out=100)
Aspect_df <- data.frame(Elev = Aspect_seq)

# occupancy model 
Aspect_occu <- occuMulti(data = deer2occ_umf,
                       detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                       "~ scale(DaysActive)"),
                       stateformulas = c("~scale(Elev)",
                                         "~scale(Elev)",
                                         "~1")
) 

# white tail deer
occ_Aspect_wtd <- predict(Aspect_occu, type="state", 
                        species="Whitetail_deer", newdata = Aspect_df)


occ_Aspect_wtd$Species <- "Whitetail deer"
occ_Aspect_wtd$Aspect <- Aspect_seq
head(occ_Aspect_wtd)

# mule deer
occ_Aspect_md <- predict(Aspect_occu, type="state", 
                       species="mule_deer", newdata = Aspect_df)


occ_Aspect_md$Species <- "Mule Deer"
occ_Aspect_md$Aspect <- Aspect_seq
head(occ_Aspect_md)



# Plotting marginal occupancy in Aspect
plot(occ_Aspect_wtd$Aspect, occ_Aspect_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="Aspect", ylab="Marginal occupancy")

lines(occ_Aspect_md$Aspect, occ_Aspect_md$Predicted, col='orange', lwd=2)

legend('topright', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))


### BarrenPrp

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
BarrenPrp_range <- range(siteCovs(deer2occ_umf)$BarrenPrp)
BarrenPrp_seq <- seq(BarrenPrp_range[1], BarrenPrp_range[2], length.out=100)
BarrenPrp_df <- data.frame(BarrenPrp = BarrenPrp_seq)

# occupancy model 
BarrenPrp_occu <- occuMulti(data = deer2occ_umf,
                         detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                         "~ scale(DaysActive)"),
                         stateformulas = c("~scale(BarrenPrp)",
                                           "~scale(BarrenPrp)",
                                           "~1")
) 

# white tail deer
occ_BarrenPrp_wtd <- predict(BarrenPrp_occu, type="state", 
                          species="Whitetail_deer", newdata = BarrenPrp_df)


occ_BarrenPrp_wtd$Species <- "Whitetail deer"
occ_BarrenPrp_wtd$BarrenPrp <- BarrenPrp_seq
head(occ_BarrenPrp_wtd)

# mule deer
occ_BarrenPrp_md <- predict(BarrenPrp_occu, type="state", 
                         species="mule_deer", newdata = BarrenPrp_df)


occ_BarrenPrp_md$Species <- "Mule Deer"
occ_BarrenPrp_md$BarrenPrp <- BarrenPrp_seq
head(occ_BarrenPrp_md)



# Plotting marginal occupancy 
plot(occ_BarrenPrp_wtd$BarrenPrp, occ_BarrenPrp_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="Barren Prp", ylab="Marginal occupancy")

lines(occ_BarrenPrp_md$BarrenPrp, occ_BarrenPrp_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))


### MGPPrp

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
MGPPrp_range <- range(siteCovs(deer2occ_umf)$MGPPrp)
MGPPrp_seq <- seq(MGPPrp_range[1], MGPPrp_range[2], length.out=100)
MGPPrp_df <- data.frame(MGPPrp = MGPPrp_seq)

# occupancy model 
MGPPrp_occu <- occuMulti(data = deer2occ_umf,
                            detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                            "~ scale(DaysActive)"),
                            stateformulas = c("~scale(MGPPrp)",
                                              "~scale(MGPPrp)",
                                              "~1")
) 

# white tail deer
occ_MGPPrp_wtd <- predict(MGPPrp_occu, type="state", 
                             species="Whitetail_deer", newdata = MGPPrp_df)


occ_MGPPrp_wtd$Species <- "Whitetail deer"
occ_MGPPrp_wtd$MGPPrp <- MGPPrp_seq
head(occ_MGPPrp_wtd)

# mule deer
occ_MGPPrp_md <- predict(MGPPrp_occu, type="state", 
                            species="mule_deer", newdata = MGPPrp_df)


occ_MGPPrp_md$Species <- "Mule Deer"
occ_MGPPrp_md$MGPPrp <- MGPPrp_seq
head(occ_MGPPrp_md)



# Plotting marginal occupancy 
plot(occ_MGPPrp_wtd$MGPPrp, occ_MGPPrp_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="MGPPrp", ylab="Marginal occupancy")

lines(occ_MGPPrp_md$MGPPrp, occ_MGPPrp_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))



### StreamsDist

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
StreamsDist_range <- range(siteCovs(deer2occ_umf)$StreamsDist)
StreamsDist_seq <- seq(StreamsDist_range[1], StreamsDist_range[2], length.out=100)
StreamsDist_df <- data.frame(StreamsDist = StreamsDist_seq)

# occupancy model 
StreamsDist_occu <- occuMulti(data = deer2occ_umf,
                         detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                         "~ scale(DaysActive)"),
                         stateformulas = c("~scale(StreamsDist)",
                                           "~scale(StreamsDist)",
                                           "~1")
) 

# white tail deer
occ_StreamsDist_wtd <- predict(StreamsDist_occu, type="state", 
                          species="Whitetail_deer", newdata = StreamsDist_df)


occ_StreamsDist_wtd$Species <- "Whitetail deer"
occ_StreamsDist_wtd$StreamsDist <- StreamsDist_seq
head(occ_StreamsDist_wtd)

# mule deer
occ_StreamsDist_md <- predict(StreamsDist_occu, type="state", 
                         species="mule_deer", newdata = StreamsDist_df)


occ_StreamsDist_md$Species <- "Mule Deer"
occ_StreamsDist_md$StreamsDist <- StreamsDist_seq
head(occ_StreamsDist_md)



# Plotting marginal occupancy 
plot(occ_StreamsDist_wtd$StreamsDist, occ_StreamsDist_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="StreamsDist", ylab="Marginal occupancy")

lines(occ_StreamsDist_md$StreamsDist, occ_StreamsDist_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))



### PasturePrp

# Getting the range of the prpForest covariate and creating a sequence along that 
# range for occupancy probability
PasturePrp_range <- range(siteCovs(deer2occ_umf)$PasturePrp)
PasturePrp_seq <- seq(PasturePrp_range[1], PasturePrp_range[2], length.out=100)
PasturePrp_df <- data.frame(PasturePrp = PasturePrp_seq)

# occupancy model 
PasturePrp_occu <- occuMulti(data = deer2occ_umf,
                              detformulas = c("~ scale(DaysActive)+ scale(VegHeight)", 
                                              "~ scale(DaysActive)"),
                              stateformulas = c("~scale(PasturePrp)",
                                                "~scale(PasturePrp)",
                                                "~1")
) 

# white tail deer
occ_PasturePrp_wtd <- predict(PasturePrp_occu, type="state", 
                               species="Whitetail_deer", newdata = PasturePrp_df)


occ_PasturePrp_wtd$Species <- "Whitetail deer"
occ_PasturePrp_wtd$PasturePrp <- PasturePrp_seq
head(occ_PasturePrp_wtd)

# mule deer
occ_PasturePrp_md <- predict(PasturePrp_occu, type="state", 
                              species="mule_deer", newdata = PasturePrp_df)


occ_PasturePrp_md$Species <- "Mule Deer"
occ_PasturePrp_md$PasturePrp <- PasturePrp_seq
head(occ_PasturePrp_md)



# Plotting marginal occupancy 
plot(occ_PasturePrp_wtd$PasturePrp, occ_PasturePrp_wtd$Predicted, type='l', ylim=c(0,1),
     col='purple', lwd=2, xlab="PasturePrp", ylab="Marginal occupancy")

lines(occ_PasturePrp_md$PasturePrp, occ_PasturePrp_md$Predicted, col='orange', lwd=2)

legend('topleft', col=c('purple', 'orange'), lty=1,
       legend=c("Whitetail deer", "Mule Deer"))



