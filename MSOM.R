


library(dplyr)
library(lubridate)
library(unmarked)


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

# Creating a covariate matrix



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

doy_cov_dat <- read.csv("./KansasCamera_doy.csv")

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

# merging new site ID by Stack_ID
veg_merge_dat <- merge(ks_dat, veg_cov, by.x = "Stack_ID", by.y = "Stack_ID")

# Extracting unique instances of Stack_ID with corresponding Latitude, Longitude, and DaysActive
veg_unique_data <- veg_merge_dat %>%
  select(Stack_ID,Site.x, VegHeight) %>%
  distinct() %>%
  arrange(Stack_ID)

# Merging doy cov and veg cov into a single dataframe for detection
# merging new site ID by Stack_ID
det_cov <- merge(doy_unique_data, veg_unique_data, by.x = "Stack_ID", by.y = "Stack_ID")

# renaming site, lat, long, and removing site.x.y
names(det_cov)[2] <- "Site"
names(det_cov)[3] <- "Latitude"
names(det_cov)[4] <- "Longitude"
det_cov <- det_cov[,-6]



# occupancy covariates -------------- 

landscape_cov_dat <- read.csv("./occu_cov_2650.csv")

# combining landscape_cov_dat with det_cov 
site_covs <- merge(landscape_cov_dat, det_cov, by.x = "Site", by.y = "Site")

# checking length
NROW(site_covs)


## ------------------ Unmarked frame -------------------------------------

# creating an unmarked frame

deer_umf <- unmarkedFrameOccuMulti(y = ylist, 
                                   siteCovs = site_covs)
                                   

# detection model ------------------------

# null model
fit1 <- occuMulti(data = deer_umf,
                  detformulas = c("~1","~1"),
                  stateformulas = c("~1","~1","~1") 
                  )

# days active 
fit2 <- occuMulti(data = deer_umf,
                  detformulas = c("~DaysActive", "~DaysActive"),
                  stateformulas = c("~1","~1","~1") 
)

deer_umf@siteCovs
