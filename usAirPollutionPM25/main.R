# PM2.5 is particulate matter 2.5 micrometers or less in diameter.
# For comparison, human hair is about 100 micrometers.
# These fine particles are not trapped by nose, mouth or throat.
# Hence, they can get deep into the lungs.
# Dust, smoke, mist, etc. make up PM2.5.

# Reading the data
pollution <- read.csv("data/avgpm25.csv")

# Get more info by viewing the structure
# Data types, how many rows and cols, a peek into values
str(pollution)

# First few rows
head(pollution)

# Last few rows
tail(pollution)

# Sort and see the extreme values
head(sort(pollution$pm25))
tail(sort(pollution$pm25))

# Control how many rows for head and tail
head(sort(pollution$pm25), n = 3)
tail(sort(pollution$pm25),  n = 3)

# Quick info about how data is distributed
# Five-number summary: min, 25th percentile, median, 75th percentile, max
fivenum(pollution$pm25)

# Gives more info including mean
# mean >> median => positively skewed
summary(pollution$pm25)

# Boxplot to show the spread of data
boxplot(pollution$pm25, col = "lightblue")
abline(h = 15) # mark a threshold point

# We analyze high pollution areas: pm25 > 15
# All are in the "west" in country prefixed by "06", which is California
highones <- pollution[pollution$pm25 > 15,]

# Let's plot this county
library(maps)
map("county", "california")
with(highones, points(longitude, latitude))

# Histogram to see full data distribution
hist(pollution$pm25, col = "lightgreen")

# Histogram with user-specified bins
hist(pollution$pm25, 
     breaks = seq(floor(min(pollution$pm25)), 
                  ceiling(max(pollution$pm25)), by = 0.5),
     col = "lightgreen")
hist(pollution$pm25, breaks = 5, col = "pink") # do we see a large spike?

# Rug gives the actual data points
rug(pollution$pm25)

# Mark the median
abline(v = median(pollution$pm25), col = "blue", lwd = 4)

# Analyze categorical data
# This call tell if data is collected uniformly across regions of interest
table(pollution$region)
barplot(table(pollution$region), col = "wheat")

# Let's visualize more than one variable
# y ~ grp: numeric vector y split into groups as given in grp
# More pollution in the east than in west but all high outliers are in the west
boxplot(pm25 ~ region, data = pollution, col = "pink")

# Let's do a histogram by region (two ways of doing this)
hist(pollution$pm25[pollution$region == "east"], col = "green")
hist(subset(pollution, region == "east")$pm25, col = "green")

# Combine two histograms into one plot
# mfrow: no. of rows and cols
# mar: margin
# Distribution is different for east and west regions (skewness)
# Left skewed in the east and right skewed in the west
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
hist(subset(pollution, region == "east")$pm25, 
     col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

# For continuous variables, scatterplot is commonly used
# For graphical parameters, refer to "par"
# lwd: line width
# lty: (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

# Differentiate the regions
with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)
levels(pollution$region) # east (black) and west (red)

# Split into subplots
# We see that highly polluted areas in the west are near the same latitude
par(mfrow = c(1, 2))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

# Same as above but with Lattice
library(lattice)
xyplot(pm25 ~ latitude | region, data = pollution)

# Same as above but with ggplot2
library(ggplot2)
qplot(latitude, pm25, data = pollution, facets = . ~ region)

