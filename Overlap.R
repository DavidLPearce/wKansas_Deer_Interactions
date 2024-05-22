
# Load packages
install.packages("activity")
install.packages("overlap")

library(activity) 
library(overlap)



# Reading in data
ks_dat <- read.csv("./KansasCamera_data.csv")


tmp <- deer_occurrences$Site[deer_occurrences$Deer_Occurrence == "Both"]

both_dat <- subset(ks_dat, Site %in% tmp)

both_dat <- both_dat[which(both_dat$Common_name == "White-Tailed Deer" | both_dat$Common_name == "Mule Deer" ),]

# calculate solar time 
tmp <- solartime ( both_dat$DateTime, # the date time column 
                   both_dat$Latitude,  # Latitude
                   both_dat$Longitude, # Longitude
                   tz=-5,              # an offset in numeric hours to UTC (Alberta is 6 hours behind)
                   format="%m/%d/%Y %H:%M:%OS")


both_dat$Solar <- tmp$solar

# Subsetting data by species
wtd_dat <- both_dat[which(both_dat$Common_name == "White-Tailed Deer"),]
wtd <- as.vector(as.numeric(wtd_dat$Solar))

md_dat <- both_dat[which(both_dat$Common_name == "Mule Deer" ),]
md <- as.vector(as.numeric(md_dat$Solar))


# getting overlap estimates
dhat4 <- overlapEst(wtd, md, type="Dhat4")

boot <- bootstrap(wtd, md, nb = 1000, type="Dhat4")

deer_overlap_est <- bootCI(dhat4, boot, conf = 0.95)

# Plotting overlap
overlapPlot(wtd, md, xscale = 24, xcenter = "noon", main = "",
            linetype = c(1, 1), linecol = c("purple", "orange"), linewidth = c(2, 2),
            olapcol = "lightgrey", kmax = 3, adjust = 1)

legend("topleft", c("Whitetail", "Mule"), col=c("purple", "orange"), lty=1, lwd=2, bty = 'n')

text(x = 1, y = 0.1, 
     labels = bquote(hat(Delta)[4] == "0.886 [0.857, 0.895]"),
     font = 2)


