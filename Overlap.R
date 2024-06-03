
# Load packages
install.packages("activity")
install.packages("overlap")

library(activity) 
library(overlap)



# Reading in data
ks_dat <- read.csv("./KansasCamera_data.csv")


both_dat <- ks_dat[which(ks_dat$Common_name == "White-Tailed Deer" | ks_dat$Common_name == "Mule Deer" ),]

# calculate solar time 
tmp <- solartime ( both_dat$DateTime, # the date time column 
                   both_dat$Latitude,  # Latitude
                   both_dat$Longitude, # Longitude
                   tz=-5,              # an offset in numeric hours to UTC (Alberta is 6 hours behind)
                   format="%m/%d/%Y %H:%M:%OS")


both_dat$Solar <- tmp$solar

# Subsetting data by species - All Years
wtd_dat <- both_dat[which(both_dat$Common_name == "White-Tailed Deer"),]
wtd <- as.vector(as.numeric(wtd_dat$Solar))

md_dat <- both_dat[which(both_dat$Common_name == "Mule Deer" ),]
md <- as.vector(as.numeric(md_dat$Solar))


# getting overlap estimates
dhat4 <- overlapEst(wtd, md, type="Dhat4")

boot <- bootstrap(wtd, md, nb = 1000, type="Dhat4")

deer_overlap_est <- bootCI(dhat4, boot, conf = 0.95)

# Plotting overlap
overlapPlot(wtd, md, xscale = 24, xcenter = "noon", main = "All Years",
            linetype = c(1, 1), linecol = c("purple", "orange"), linewidth = c(2, 2),
            olapcol = "lightgrey", kmax = 3, adjust = 1)

legend("topleft", c("Whitetail", "Mule"), col=c("purple", "orange"), lty=1, lwd=2, bty = 'n')

dhat4
deer_overlap_est

text(x = 2, y = 0.1, 
     labels = bquote(hat(Delta)[4] == "0.886 [0.857, 0.895]"),
     font = 2)

# Subsetting data by species - 2018
wtd_dat_2018 <- both_dat[which(both_dat$Common_name == "White-Tailed Deer" & both_dat$Year == 2018),]
wtd_2018 <- as.vector(as.numeric(wtd_dat_2018$Solar))

md_dat_2018 <- both_dat[which(both_dat$Common_name == "Mule Deer" & both_dat$Year == 2018),]
md_2018 <- as.vector(as.numeric(md_dat_2018$Solar))


# getting overlap estimates
dhat4_2018 <- overlapEst(wtd_2018, md_2018, type="Dhat4")

boot_2018 <- bootstrap(wtd_2018, md_2018, nb = 1000, type="Dhat4")

deer_overlap_est_2018 <- bootCI(dhat4_2018, boot_2018, conf = 0.95)

# Plotting overlap
overlapPlot(wtd_2018, md_2018, xscale = 24, xcenter = "noon", main = "2018",
            linetype = c(1, 1), linecol = c("purple", "orange"), linewidth = c(2, 2),
            olapcol = "lightgrey", kmax = 3, adjust = 1)

legend("topleft", c("Whitetail", "Mule"), col=c("purple", "orange"), lty=1, lwd=2, bty = 'n')

dhat4_2018
deer_overlap_est_2018

text(x = 2, y = 0.095, 
     labels = bquote(hat(Delta)[4] == "0.781 [0.758, 0.818]"),
     font = 2)


# Subsetting data by species - 2019
wtd_dat_2019 <- both_dat[which(both_dat$Common_name == "White-Tailed Deer" & both_dat$Year == 2019),]
wtd_2019 <- as.vector(as.numeric(wtd_dat_2019$Solar))

md_dat_2019 <- both_dat[which(both_dat$Common_name == "Mule Deer" & both_dat$Year == 2019),]
md_2019 <- as.vector(as.numeric(md_dat_2019$Solar))


# getting overlap estimates
dhat4_2019 <- overlapEst(wtd_2019, md_2019, type="Dhat4")

boot_2019 <- bootstrap(wtd_2019, md_2019, nb = 1000, type="Dhat4")

deer_overlap_est_2019 <- bootCI(dhat4_2019, boot_2019, conf = 0.95)

# Plotting overlap
overlapPlot(wtd_2019, md_2019, xscale = 24, xcenter = "noon", main = "2019",
            linetype = c(1, 1), linecol = c("purple", "orange"), linewidth = c(2, 2),
            olapcol = "lightgrey", kmax = 3, adjust = 1)

legend("topleft", c("Whitetail", "Mule"), col=c("purple", "orange"), lty=1, lwd=2, bty = 'n')

dhat4_2019
deer_overlap_est_2019

text(x = 2, y = 0.090, 
     labels = bquote(hat(Delta)[4] == "0.885 [0.859, 0.925]"),
     font = 2)

# Subsetting data by species - 2020
wtd_dat_2020 <- both_dat[which(both_dat$Common_name == "White-Tailed Deer" & both_dat$Year == 2020),]
wtd_2020 <- as.vector(as.numeric(wtd_dat_2020$Solar))

md_dat_2020 <- both_dat[which(both_dat$Common_name == "Mule Deer" & both_dat$Year == 2020),]
md_2020 <- as.vector(as.numeric(md_dat_2020$Solar))


# getting overlap estimates
dhat4_2020 <- overlapEst(wtd_2020, md_2020, type="Dhat4")

boot_2020 <- bootstrap(wtd_2020, md_2020, nb = 1000, type="Dhat4")

deer_overlap_est_2020 <- bootCI(dhat4_2020, boot_2020, conf = 0.95)

# Plotting overlap
overlapPlot(wtd_2020, md_2020, xscale = 24, xcenter = "noon", main = "2020",
            linetype = c(1, 1), linecol = c("purple", "orange"), linewidth = c(2, 2),
            olapcol = "lightgrey", kmax = 3, adjust = 1)

legend("topleft", c("Whitetail", "Mule"), col=c("purple", "orange"), lty=1, lwd=2, bty = 'n')

dhat4_2020
deer_overlap_est_2020

text(x = 2, y = 0.095, 
     labels = bquote(hat(Delta)[4] == "0.823 [0.815, 0.873]"),
     font = 2)


