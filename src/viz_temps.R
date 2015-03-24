#!/usr/bin/env Rscript

# Preprocessing and summarizing data
library(dplyr)

# Visualization development
library(ggplot2)

# For text graphical object (to add text annotation)
library(grid)
 
# The starting point for data analysis done in this module is
# file livermore_15yr_temps.csv which is created by running load.R. The input to
# load.R is the raw NCDC 15 year weather data file 'ncdc_livermore_15yr_weather.csv'
# from which it extracts only the temperature and related data.

# Load 15 years temperature data
all_temps_15yrs <- read.csv("livermore_15yr_temps.csv", stringsAsFactors=FALSE, sep=",")

# Clean the data: drop records with invalid temp values, and missing or invalid
#  measurement, quality or source flags.
#
#  NOTES: 1. The raw CSV file shows white space for Tmax and Tmin Measurement flags
#            but when read into the dataframe those flags read NA. The NA values are
#            treated as Normal in this analysis
#         2. The raw CSV file shows white space for Tmax and Tmin Quality flags but
#            when read into the dataframe the Tmax Quality flags show up as NA, while Tmin
#            Quality flags show up as " " (white space), as expected. 
all_temps_15yrs <- all_temps_15yrs %>%
        filter(tmax != -9999                &   # drop missing max temp identified by -9999
               tmin != -9999                &   # drop missing min temp identified by -9999
               is.na(measurement.flag.4)    &   # keep tmax data with no special measurement info
               is.na(measurement.flag.5)    &   # keep tmin data with no special measurement info
               is.na(quality.flag.4)        &   # keep tmax data that did not fail quality check
               quality.flag.5 == " "        &   # keep tmin data that did not fail quality check
               source.flag.4 != " "         &   # drop tmax data with no source (blank)
               !is.na(source.flag.4)        &   #  or NA                              
               source.flag.5 != " "         &   # drop tmin data with no source (blank)
               !is.na(source.flag.5))           #  or NA

# Raw data temps are in Celsius degrees to tenths. Convert to Fahrenheit and scale for tenths
all_temps_15yrs <- all_temps_15yrs %>%
        mutate(tmaxF = tmax*0.18 + 32,          # convert to Fahrenheit (F = C*9/5 + 32)
               tminF = tmin*0.18 + 32)          # note: temps are also being converted from tenths
                                                #       to real (normal) values

# Compute the average per year min and max temps
avg_temps_each_year <- all_temps_15yrs %>%
        group_by(year) %>%
        summarise(avg_min = mean(tminF),
                  avg_max = mean(tmaxF)) %>%
        ungroup()

avg_max_per_year <- arrange(avg_temps_each_year, desc(avg_max))
avg_min_per_year <- arrange(avg_temps_each_year, desc(avg_min))

print(avg_max_per_year[,c("year","avg_max")])
print(avg_min_per_year[,c("year","avg_min")])

# create a dataframe that represents 14 years of historical temp data from 2000-2013
Past <- all_temps_15yrs %>%
        group_by(year, month) %>%
        arrange(day) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(newDay = seq(1, length(day))) %>%    # label days as 1:365 (will represent x-axis)
        ungroup() %>%
        filter(year != 2014) %>%                # filter out 2014 data
        group_by(newDay) %>%
        mutate(upper = max(tmaxF),              # identify same day highest max temp from all years
               lower = min(tminF),              # identify same day lowest min temp from all years
               avg_upper = mean(tmaxF),         # compute same day average max temp from all years
               avg_lower = mean(tminF)) %>%     # compute same day average min temp from all years
        ungroup()

# create a dataframe that represents 2014 temperature data
Present <- all_temps_15yrs %>%
        group_by(year, month) %>%
        arrange(day) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(newDay = seq(1, length(day))) %>%    # label days as 1:365 (will represent x-axis)
        ungroup() %>%
        filter(year == 2014)                    # filter out all years except 2014 data

# create dataframe that represents the lowest same-day temperature from years 2000-2013
PastLows <- Past %>%
        group_by(newDay) %>%
        summarise(Pastlow = min(tminF)) # identify lowest same-day temp between 2000 and 2013

# create dataframe that represents the highesit same-day temperature from years 2000-2013
PastHighs <- Past %>%
        group_by(newDay) %>%
        summarise(Pasthigh = max(tmaxF)) # identify highest same-day temps between 2000 and 2013

# create dataframe that identifies days in 2014 when temps were lower than in all previous 14 years
PresentLows <- Present %>%
        left_join(PastLows) %>%         # merge historical lows to 2014 low temp data
        mutate(record = ifelse(tminF<Pastlow, "Y", "N")) %>% # current year was a record low?
        filter(record == "Y")           # filter for 2014 record low days

# create dataframe that identifies days in 2014 when temps were higher than in all previous 14 years
PresentHighs <- Present %>%
        left_join(PastHighs) %>%        # merge historical lows to 2014 low temp data
        mutate(record = ifelse(tmaxF>Pasthigh, "Y", "N")) %>% # current year was a record high?
        filter(record == "Y")           # filter for 2014 record high days

#   **** Prepare to do Visualization ****

# prepare y-axis details

# function: Turn y-axis labels into values with a degree superscript
degree_format <- function(x, ...) {
  parse(text = paste(x, "*degree", sep=""))
}

# create y-axis variable
yaxis_temps <- degree_format(seq(0, 120, by=10))


#   **** Visualization Steps ****
# Step 1: create the canvas for the plot. Also plot the background lowest, highest 2000-2013 temps
p <- ggplot(Past, aes(newDay, tmaxF)) +
        theme(plot.background = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.border = element_blank(),
              panel.background = element_rect(fill = "seashell2"),
              axis.ticks = element_blank(),
              #axis.text = element_blank(),  
              axis.title = element_blank()) +
        geom_linerange(Past,
                       mapping=aes(x=newDay, ymin=lower, ymax=upper),
                       size=0.8, colour = "#CAA586", alpha=.6)
#print(p)


# Step 2: Plot average low and high temps from 2000-2013
p <- p + 
        geom_linerange(Past,
                       mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper),
                       size=0.8,
                       colour = "#A57E69")
#print(p)


# Step 3: Plot 2014 high and low temps
p <- p + 
        geom_linerange(Present, mapping=aes(x=newDay, ymin=tminF, ymax=tmaxF), size=0.8, colour = "#4A2123")
#print(p)


# Step 4: Add the y-axis border and the x-axis grid lines
p <- p + 
        geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1) +
        geom_hline(yintercept = 0, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 10, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 20, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 30, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 40, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 50, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 60, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 70, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 80, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 90, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 100, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 110, colour = "ivory2", linetype=1, size=.1) +
        geom_hline(yintercept = 120, colour = "ivory2", linetype=1, size=.1)
#print(p)


# Step 5: Add vertical gridlines to mark end of each month
p <- p + 
        geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.4) +
        geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.4) 
#print(p)

# Step 6: Add labels to the x and y axes
p <- p +
        coord_cartesian(ylim = c(0,120)) +
        scale_y_continuous(breaks = seq(0,120, by=10), labels = yaxis_temps) +
        scale_x_continuous(expand = c(0, 0), 
                           breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
                           labels = c("January", "February", "March", "April",
                                      "May", "June", "July", "August", "September",
                                      "October", "November", "December"))
#print(p)

# Step 7: Add points to mark the 2014 record high and low temps
p <- p +
        geom_point(data=PresentLows, aes(x=newDay, y=tminF), colour="blue3") +
        geom_point(data=PresentHighs, aes(x=newDay, y=tmaxF), colour="firebrick3")
#print(p)

# Step 8: Add title to plot
p <- p +
        ggtitle("Dublin (California) Weather in 2014") +
        theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="gray30",size=18))
#print(p)

# Step 9: Add explanation text under the plot title

grob1 = grobTree(textGrob("Temperature\n",
                          x=0.02, y=0.92, hjust=0,
                          gp=gpar(col="gray30", fontsize=10, fontface="bold")))
p <- p + annotation_custom(grob1)

grob2 = grobTree(textGrob(paste("Bars represent range between daily high and low temperatures.\n",
                                "Data set includes data from Jan 1, 2000 to December 31, 2014.\n",
                                "Average high temperature for 2014 was 76.9F making it\n",
                                "the warmest in 15 years since 2000.", sep=""),
                          x=0.02, y=0.83, hjust=0,
                          gp=gpar(col="gray30", fontsize=8.5)))

p <- p + annotation_custom(grob2)
#print(p)

# Step 10: Add annotation for points representing the record high 2014 temperatures
#          Note: There were no record lows in 2014, it was the warmest year of the 15-yr period!

grob3 = grobTree(textGrob(paste("In 2014 there were 60 days that were\n",
                                 "hottest since 2000\n",sep=""),
                          x=0.72, y=0.9, hjust=0,
                          gp=gpar(col="firebrick3", fontsize=7)))

p <- p + annotation_custom(grob3)

p <- p +
        annotate("segment", x = 257, xend = 263, y = 99, yend = 108, colour = "firebrick3") 
#print(p)

# Step 11: Add legend to explain difference between the different data point layers

p <- p +
        annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "#CAA586", size=3) +
        annotate("segment", x = 181, xend = 181, y = 11, yend = 19, colour = "#A57E69", size=3) +
        annotate("segment", x = 181, xend = 181, y = 13, yend = 22, colour = "#4A2123", size=2) +
        annotate("segment", x = 177, xend = 179, y = 18.7, yend = 18.7, colour = "#A57E69", size=.5) +
        annotate("segment", x = 177, xend = 179, y = 11.2, yend = 11.2, colour = "#A57E69", size=.5) +
        annotate("segment", x = 177, xend = 177, y = 11.2, yend = 18.7, colour = "#A57E69", size=.5) +
        annotate("segment", x = 183, xend = 185, y = 13.25, yend = 13.25, colour = "#4A2123", size=.3) +
        annotate("segment", x = 183, xend = 185, y = 21.75, yend = 21.75, colour = "#4A2123", size=.3) +
        annotate("text", x = 165, y = 14.75, label = "NORMAL RANGE", size=2.1, colour="gray30") +
        annotate("text", x = 170, y = 25, label = "RECORD HIGH", size=2.1, colour="gray30") +
        annotate("text", x = 170, y = 5, label = "RECORD LOW", size=2.1, colour="gray30") +
        annotate("text", x = 195, y = 21.75, label = "ACTUAL HIGH", size=2.1, colour="gray30") +
        annotate("text", x = 195, y = 13.25, label = "ACTUAL LOW", size=2.1, colour="gray30")

print(p)

#ggsave(file="dublin2014temps.svg", plot=last_plot())



