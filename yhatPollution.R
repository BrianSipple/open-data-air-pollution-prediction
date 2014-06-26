setwd("/Users/BrianSipple/Development/R/datasciencecoursera/DataProducts/")

d <- read.csv("./data/annual_all_2013.csv")
str(d) #Inspect variables

# Each row is a value for the pollutant. 
# We want to find the variables for Latitude and Longitude for location, and
# Parameter.Name and Arithmetic.Mean for relevant actual polutant info

sub <- subset(d, Parameter.Name %in% c("PM2.5 - Local Conditions", "Ozone")
              & Pollutant.Standard %in% c("Ozone 8-Hour 2008", "PM25 Annual 2006"),
              c(Longitude, Latitude, Parameter.Name, Arithmetic.Mean))

# We now have the Arithmetic.Mean values for Ozone & PM2.5 at various locations
head(sub)


# Now we want to Find the average of different measurements at each location

# Aggregate over each "Arithmetic.Mean" value
pollavg <- aggregate(sub[, "Arithmetic.Mean"],
                     sub[, c("Longitude", "Latitude", "Parameter.Name")],
                     mean, na.rm=T)

head(pollavg)

#remane x to be the "level" of the pollutant
names(pollavg)[4] = "level"

# Since we subsetted the dataset into just two pollutants,
# we should refactor the dataset so that it only has two levels
pollavg <- transform(pollavg, Parameter.Name = factor(Parameter.Name))

head(pollavg)

# Remove the unprocessed data that we no longer need to optimize yhat modeling when we upload
rm(d, sub)

# Create a new data frame that just has the locations of the monitors so we can calculate
# the distances b/w the point we want a prediction at, and the locations of all the monitors

monitors = data.matrix(pollavg[, c("Longitude", "Latitude")])

################### Now we write our prediction function

# The inuput and output needs to be in the form of a data frame

# We'll input the Latitude, Longitude, and radius. 

# Radius determines how far away we're looking 
# from a given point to find monitors that we will use to average over.
# Monitors that are close by will likely be more related to the actual level of 
# pollution at a given prediction point... but we want to balance that by actaully finding 
# a decent amount of monitors!)

#install.packages("fields")
library(fields)

## Input is data frame with
## lon: longitude
## lat: latitude
## radius: Raduis in miles for finding monitors
pollutant <- function(df) {
        
        #### Calculate distances on the surface of a sphere (i.e., Planet Earth!) by using the "fields" package
        
        x <- data.matrix(df[, c("lon", "lat")])
        r <- df$radius
        
        # Calculate distance between the all input points (in the x matrix) that we want to make predictions on,
        # and all of the monitors in our "monitors" data set
        d <- rdist.earth(monitors, x) 
        
        # Loop over all prediction points and find out which monitors are within the radius distance
        # of our prediction point 
        use <- lapply(seq_len(ncol(d)), function(i) {
                which(d[, i] < r[i])  # which point is less than the radius
        })
        
        # Now that we have the set of indices, we can caluclate the levels of pollution at each location
        
        # Returns 2-level vector (one level for Ozone and one for PM2.5) of means for every prediction location
        levels <- sapply(use, function(idx) {
                with(pollavg[idx, ], tapply(level, Parameter.Name, mean)) # tapply over level, by Parameter.Name, and calculate the mean 
        })
        
        # Convert our "levels" mean vector into a dataframe
        dlevels <- as.data.frame(t(levels)) # take the transpose of "levels" so it looks right
        
        # Return a dataframe that has the input data in one column, and prediction values in another
        data.frame(df, dlevels)
}


########### TESTING ###########

pollutant(data.frame(lon = -122.33, lat = 47.69, radius = 40))  # Lookin' good, Seattle



########### Publish the prediction function to yhat #############

#install.packages("yhatr")
library(yhatr)


## Loads all dependencies required for the function to run
model.require <- function() {
        library(fields)
}


## Takes input data frame as an arugment, in case we 
## need to transform the data in any specific way before it's
## fed to the prediction model
model.transform <- function() {
        df   # Doesn't apply for this algorithm, so we just return the input
}



## Performs the prediction
model.predict <- function() {
        pollutant(df)       
}



## Create a vector for yhat upload containing username, apikey, and env (the website url) 
yhat.config <- c(username = "Bsipple57@gmail.com",
                 apikey = "56bf5f8dbe504c900c77e61522f3f7f3",
                 env = "http://sandbox.yhathq.com/")

yhat.deploy("Pollutant_Predictor")



