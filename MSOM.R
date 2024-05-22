

library(ggplot2)
library(dplyr)
library(lubridate)
library(unmarked)
library(AICcmodavg)


# Reading in data
ks_dat <- read.csv("./KansasCamera_data.csv") # Camera data

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

# combining landscape_cov_dat with det_cov 
site_covs <- merge(landscape_cov_dat, doy_unique_data, by.x = "Site", by.y = "Site")

# checking length
NROW(site_covs)

# creating new siteID
site_covs$New_SiteID <- 1:NROW(site_covs)

# take a look
View(site_covs)

# distance covariates -------------- 

distance_covs <- read.csv("./distance_covs.csv")

# combining landscape_cov_dat with det_cov 
site_covs <- merge(site_covs, distance_covs, by.x = "Site", by.y = "Site")



## ------------------ Unmarked frame -------------------------------------

# creating an unmarked frame

deer_umf <- unmarkedFrameOccuMulti(y = ylist, 
                                   siteCovs = site_covs)
                                   

# detection model ------------------------

# null model
fit1 <- occuMulti(data = deer_umf,
                  detformulas = c("~1",
                                  "~1"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# days active 
fit2 <- occuMulti(data = deer_umf,
                  detformulas = c("~scale(DaysActive)", 
                                  "~scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# latitude 
fit3 <- occuMulti(data = deer_umf,
                  detformulas = c("~Latitude", 
                                  "~Latitude"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# longitude 
fit4 <- occuMulti(data = deer_umf,
                  detformulas = c("~Longitude", 
                                  "~Longitude"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)


# DaysActive + longitude
fit5 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(DaysActive) + Longitude", 
                                  "~ scale(DaysActive) + Longitude"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# DaysActive + latitude
fit6 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(DaysActive) + Latitude", 
                                  "~ scale(DaysActive) + Latitude"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# DaysActive + latitude + longitude
fit7 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(DaysActive) + Latitude + Longitude", 
                                  "~ DaysActive + Latitude + Longitude"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# MajorRoadDis
fit8 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(MajorRoadDist)", 
                                  "~ scale(MajorRoadDist)"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# TownsDist
fit9 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist)", 
                                  "~ scale(TownsDist)"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# TownsDist + MajorRoadDis
fit10 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist)"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)

# TownsDist + MajorRoadDis + DaysActive
fit11 <- occuMulti(data = deer_umf,
                   detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + scale(DaysActive)", 
                                   "~ scale(TownsDist) + scale(MajorRoadDist) + scale(DaysActive)"),
                   stateformulas = c("~1",
                                     "~1",
                                     "~1") 
)

# TownsDist + MajorRoadDis + Longitude
fit12 <- occuMulti(data = deer_umf,
                   detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude", 
                                   "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude"),
                   stateformulas = c("~1",
                                     "~1",
                                     "~1") 
)

# TownsDist + MajorRoadDis + Longitude + DaysActive
fit13 <- occuMulti(data = deer_umf,
                   detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                   "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                   stateformulas = c("~1",
                                     "~1",
                                     "~1") 
)


# Creating a list of model names
model_names <- paste("Fit", as.character(1:13), sep = "")

# Calculating AIC for the list of models
model_aicc <- aictab(list(fit1,fit2,fit3,fit4,fit5,
                          fit6,fit7,fit8,fit9,fit10,fit11,fit12,fit13), modnames = model_names)

print(model_aicc)


# occupancy model ------------------------

# detection model fit 13 was the most supported 

# Null model
fit1 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~1",
                                    "~1") 
)


# Longitude
fit2 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~Longitude",
                                    "~Longitude") 
)


# RowcropPrp
fit3 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~scale(RowcropPrp)",
                                    "~scale(RowcropPrp)") 
)



# ForestPrp
fit4 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~scale(ForestPrp)",
                                    "~scale(ForestPrp)") 
)

# ShrublandPrp + ForestPrp
fit5 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~scale(ShrublandPrp)",
                                    "~scale(ShrublandPrp)") 
)

# ShrublandPrp + ForestPrp
fit6 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~scale(ShrublandPrp) + scale(ForestPrp)",
                                    "~scale(ShrublandPrp) + scale(ForestPrp)") 
)

# CRPPrp + SGPPrp + MGPPrp + TGPPrp + SandsagePrp
fit7 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~scale(CRPPrp) + scale(SGPPrp) + scale(MGPPrp) + scale(TGPPrp) + scale(SandsagePrp)",
                                    "~scale(CRPPrp) + scale(SGPPrp) + scale(MGPPrp) + scale(TGPPrp) + scale(SandsagePrp)") 
)


# CRPPrp + SGPPrp + MGPPrp + TGPPrp + SandsagePrp
fit7 <- occuMulti(data = deer_umf,
                  detformulas = c("~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)", 
                                  "~ scale(TownsDist) + scale(MajorRoadDist) + Longitude + scale(DaysActive)"),
                  stateformulas = c("~1",
                                    "~scale(CRPPrp) + scale(SGPPrp) + scale(MGPPrp) + scale(TGPPrp) + scale(SandsagePrp)",
                                    "~scale(CRPPrp) + scale(SGPPrp) + scale(MGPPrp) + scale(TGPPrp) + scale(SandsagePrp)") 
)




# Creating a list of model names
model_names <- paste("Fit", as.character(1:7), sep = "")

# Calculating AIC for the list of models
model_aicc <- aictab(list(fit1,fit2,fit3,fit4,fit5,fit6,fit7), modnames = model_names)

print(model_aicc)

