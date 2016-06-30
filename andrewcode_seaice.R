##############################################################################
# PART 1: EXPLORE THE DATA
##############################################################################

# Recall the commands summary, dim, length, range, head, hist

# Load libraries (install first, if needed)
library(maps)
library(fields)
library(animation)
library(mapproj)

# Load and attach seaice data
load("/Users/Andrew/Desktop/GSP_Bootcamp2016/Wednesday/Arctic_Sea_Changes_Movie/seaice.RData")
attach(seaice)
# What variables do you have? 
ls(seaice)
summary(seaice)
# conc
# dates
# lat
# lon
# What dimensions are they?
dim(lat)
dim(lon)
dim(conc)
length(dates)
# Get basic information about the dataset


# What do the values mean?
range(lat)
range(lon)
range(conc)
summary(conc)
head(dates)
# Look more closely at conc
hist(conc)


##############################################################################
# PART 2: PLOTS
##############################################################################

# Snapshot of one month
image.plot(lon, lat, conc[,,1])

# Change projection
?mapproject
coord <- mapproject (lon, lat, proj = "azequidistant", orientation = c(90,0,0))           
summary(coord)
lapply(coord, summary)
lapply(coord, length)

# make coord$x and coord$y into matrices
x.mat <- matrix( coord$x, nrow =  152, ncol = 224)
y.mat <- matrix( coord$y, nrow =  152, ncol = 224)

# Snapshot of first month with new projection
image.plot(x.mat, y.mat, conc[,,1])

# Change color scale (blue to white)
?designer.colors
coltab<- designer.colors(col=c("#0D50B2",  "#6BAED6", "#FFFFFF"), x= c( 0,0.5, 1))
image.plot(x.mat,y.mat,conc[,,1], col = coltab)

# Add country borders
map("world", ylim = c(45,90), projection = "azequidistant",
    orientation = c(90,0,0), fill=T, col = "green", lwd = 1, add = TRUE)

# Add title
title(main = paste("Sea Ice Concentration in Nov. 1978"))

# Make a beautiful plot of month 1 (turn off axes and axes labels)
map( "world", ylim = c(55,90), projection = "azequidistant",
     orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
     lwd = 1, bg = "#0D50B2")
image.plot( x.mat, y.mat,conc[,,1], col = coltab, 
            xaxt = "n", yaxt = "n", ann = FALSE, 
            horizontal=TRUE, add=TRUE)
map( "world", ylim = c(55,90), projection = "azequidistant",
     orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
     lwd = 1, bg="#0D50B2", add=TRUE)
title( main = paste("Sea Ice Concentration", dates[1]), col.main = "white")


##############################################################################
# PART 3: MOVIES & ANIMATION
##############################################################################

# Explore animation package. What's the difference between these?
?saveVideo
?saveMovie
?saveGIF
?mapproject

# Write a loop to plot the first 3 months
# HINT: use your beautiful plot code
for (ii in 1:3){
  print(ii)
  map( "world", ylim = c(55,90), projection = "azequidistant",
       orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
       lwd = 1, bg = "#0D50B2")
  image.plot(x.mat, y.mat,conc[,,ii], col = coltab, 
              xaxt = "n", yaxt = "n", ann = FALSE, 
              horizontal=TRUE, add=TRUE)
  map( "world", ylim = c(55,90), projection = "azequidistant",
       orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
       lwd = 1, bg = "#0D50B2", add = TRUE)
  title( main = paste("Sea Ice Concentration", dates[ii]), col.main = "white")
}

# turn that loop into a function
mapimages.f <- function(conc, timeind, xmat, ymat, dates){
  for (ii in timeind){
    print(ii)
    map( "world", ylim = c(55,90), projection = "azequidistant",
         orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
         lwd = 1, bg = "#0D50B2")
    image.plot(x.mat, y.mat,conc[,,ii], col = coltab, 
               xaxt = "n", yaxt = "n", ann = FALSE, 
               horizontal=TRUE, add=TRUE)
    map( "world", ylim = c(55,90), projection = "azequidistant",
         orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
         lwd = 1, bg = "#0D50B2", add = TRUE)
    title( main = paste("Sea Ice Concentration", dates[ii]), col.main = "white")
  }
}

# set your working directory for saving GIF
getwd()
setwd("/Users/Andrew/ImageMagick-7.0.1")

# test it with shorter version--only 12 months worth
saveGIF(mapimages.f(conc,1:12,coord$x,coord$y,dates), 
           interval = .3, movie.name = "ice1_12.gif" )

# make video of full time period
saveGIF( mapimages.f(  ,   ,   ,   ,   ), 
           interval =  , movie.name =   )



# CHALLENGE!!!!





##############################################################################
# PART 4: ADDITIONAL PLOTS
##############################################################################

# Plot spatial mean ice concentration
# Find the mean over time at each point
conc.mn <- apply(conc,c(1,2),mean)
dim(conc.mn)
map( "world", ylim = c(55,90), projection = "azequidistant",
     orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
     lwd = 1, bg="#0D50B2")
image.plot( x.mat, y.mat,conc[,,1], col = coltab, 
            xaxt = "n", yaxt = "n", ann = FALSE, 
            horizontal=TRUE, add=TRUE )
map( "world", ylim = c(55,90), projection = "azequidistant",
     orientation = c(90,0,0), col = "#0C9A61", fill = TRUE,
     lwd = 1, bg ="#0D50B2", add=TRUE)
title( main =  , col.main =  )




# Plot the time series
# replace flags (negative values) with NAs
sum(conc<0)
conc.noneg <- ifelse(conc<0,NA,conc)
dim(conc.noneg)
conc.ts <- apply(conc.noneg,3,mean, na.rm=TRUE)
plot(1:422, conc.ts, type = "l", xlab="time", ylab="mean conc")
# add a trendline using lm
x = 1:422
abline(conc.ts~x, col="red", lwd=3)
