#!/usr/bin/env Rscript

# Preprocessing and summarizing data
library(dplyr)

# Load the NCDC 15-year (2000 Jan - 2014 Dec) Livermore (California) Airport daily weather
weather_15yr_data <- read.csv("ncdc_livermore_15yr_weather.csv", stringsAsFactors=FALSE, sep=",")

# convert all column names into lowercase (for convenience)
colnames(weather_15yr_data) <- tolower(names(weather_15yr_data))

# Create a dataframe containing data related only to temperature measurements
all_temps_15yrs <- weather_15yr_data %>%
        select(date,
               tmax, measurement.flag.5, quality.flag.5, source.flag.5,
               tmin, measurement.flag.6, quality.flag.6, source.flag.6)

# Convert date into three columns - year, month, day
# The date in the raw data file is a string in the format YYYYMMDD

# First convert String object to class "Date"
all_dates <- as.Date(as.character(all_temps_15yrs$date), format="%Y%m%d", origin="1970-01-01")

# Extract parts of the date and make three columns
all_dates <- as.POSIXlt(all_dates)      # POSIXlt object is a list of date parts
year <- all_dates$year + 1900           # years is num of years from 1900, therefore adding 1900
month <- all_dates$mon
day <- all_dates$mday

# Replace the date column with 'split' date part columns (i.e., year, month, day columns)
all_temps_15yrs <- subset(all_temps_15yrs, select=-date)    # drop date column
all_temps_15yrs <- cbind(year, month, day, all_temps_15yrs) # Add back the 'split' date column

# Save the data into a file. Data in this file will be used for visualization
write.csv(all_temps_15yrs, file="livermore_15yr_temps.csv", row.names=FALSE)

