# Calendar Heat Map
# Using Baltimore City Open Data
# René F. Najera, DrPH
# Adapted from here: http://www.r-graph-gallery.com/284-calendar-heatmap/
# Also adapted from here: https://www.johnmackintosh.com/2016-11-27-calendar-heatmaps/

setwd("~/Dropbox/RProjects/Calendar-Heat-Map")

# Install packages as needed
install.packages("ggExtra")
# Install libraries as needed
library(ggplot2) 
library(lubridate) 
library(tidyr) 
library(ggExtra)
library(dplyr)
library(plyr)

# Bring in the data
all_crimes <- read.csv("data/2016_crimes.csv")
shootings <- all_crimes %>% filter(Description == "SHOOTING")
homicides <- all_crimes %>% filter(Description == "HOMICIDE")

###############################################################################

# Create the counts column for each day
homicides <- homicides %>%
  group_by(CrimeDate) %>%
  dplyr::summarize(Value=n()) # Total number of homicides by day
homicides$type <- as.character("Homicide")

shootings <- shootings %>%
  group_by(CrimeDate) %>%
  dplyr::summarize(Value=n()) # Total number of shootings by day
shootings$type <- as.character("Shooting")

# Bring them together
crimes <- rbind(shootings, homicides)

# Clean up the data
crimes$CrimeDate <-  as.Date(crimes$CrimeDate, format="%m/%d/%Y")
crimes$year <- year(crimes$CrimeDate)
crimes$dow = wday(crimes$CrimeDate,label=TRUE) 
crimes$dow = with(crimes, factor(dow, levels = rev(levels(dow)))) 
crimes$week = week(crimes$CrimeDate) 
crimes$weeks = format(crimes$CrimeDate, "%W") # was originally "%Y/%W" 
crimes$weeks = factor(crimes$weeks, levels = unique(crimes$weeks)) # use weekStart as a calendar style plot 
crimes$weekStart = crimes$CrimeDate - as.POSIXlt(crimes$CrimeDate)$wday
crimes$months <-  months(crimes$CrimeDate)
crimes$type <- as.factor(crimes$type) 
crimes$type <- with(crimes, factor(type, levels = rev(levels(type))))

heatmap <- ggplot(crimes,aes(x=weekStart, y=dow, fill=Value)) + 
geom_tile(colour="blue",size=.1) + 
  scale_fill_gradient(high="Red",low= "Yellow") + 
  guides(fill=guide_legend(title="Scale")) + 
  scale_x_date(date_breaks = "1 week",date_labels="%d-%b-%y") + 
  theme_minimal(base_size = 10) + 
  removeGrid() +
  rotateTextX() + 
  ggtitle("Heat Map of Shootings and Homicides by Week and Weekday in Baltimore, 2017",subtitle = "Using Open Data from Data.BaltimoreCity.Gov") + 
  labs(x="Week Beginning", y="Day of the Week") + 
  theme(
    plot.title=element_text(hjust=0), 
    axis.ticks=element_blank(),
    axis.text=element_text(size=7),
    legend.title=element_text(size=8),
    legend.text=element_text(size=6),
    legend.position="right") +
  geom_text(data = crimes, aes(weekStart,dow,label=Value,fontface="bold"),colour="black",size=2.5) +
  facet_wrap(~type,nrow =2 )

heatmap

###############################################################################

# Edit the data
homicides2 <- filter(crimes, type == "Homicide")
homicides2 <- group_by(homicides2, dow, months, type)
homicides2 <- dplyr::summarize(homicides2, Value=n())

shootings2 <- filter(crimes, type == "Shooting")
shootings2 <- group_by(shootings2, dow, months, type)
shootings2 <- dplyr::summarize(shootings2, Value=n())

crimes2 <- rbind(homicides2, shootings2)

# Make the Heat Map
heatmap2 <- ggplot(crimes2,aes(x=months, y=dow, fill=Value)) + 
  geom_tile(colour="blue",size=.1) + 
  scale_fill_gradient(high="Red",low= "Yellow") + 
  guides(fill=guide_legend(title="Scale")) + 
  #scale_x_date(date_breaks = "1 week",date_labels="%d-%b-%y") + 
  theme_minimal(base_size = 10) + 
  removeGrid() +
  rotateTextX() + 
  ggtitle("Heat Map of Shootings and Homicides by Month and Weekday in Baltimore, 2017",subtitle = "Using Open Data from Data.BaltimoreCity.Gov") + 
  labs(x="Month", y="Day of the Week") + 
  theme(
    plot.title=element_text(hjust=0), 
    axis.ticks=element_blank(),
    axis.text=element_text(size=7),
    legend.title=element_text(size=8),
    legend.text=element_text(size=6),
    legend.position="right") +
  geom_text(data = crimes2, aes(months,dow,label=Value),colour="black",size=2.5) +
  facet_wrap(~type,nrow =2 )

heatmap2

###############################################################################

# Heat Map of Homicides by Day of Week for 2016
# Edit the data to get the counts per day
shootings <- all_crimes %>% filter(Description == "SHOOTING")
homicides <- all_crimes %>% filter(Description == "HOMICIDE")

crime_year_s <- shootings
crime_year_s$CrimeDate <-  as.Date(crime_year_s$CrimeDate, format="%m/%d/%Y")
crime_year_s$dow = wday(crime_year_s$CrimeDate,label=TRUE) 
crime_year_s$dow = with(crime_year_s, factor(dow, levels = rev(levels(dow)))) 
crime_year_s <- crime_year_s %>%
  group_by(dow) %>%
  dplyr::summarize(Value=n()) # Total number of shootings by day
crime_year_s$type <- as.character("Shooting")
crime_year_s$year <- 2017

crime_year_h <- homicides
crime_year_h$CrimeDate <-  as.Date(crime_year_h$CrimeDate, format="%m/%d/%Y")
crime_year_h$dow = wday(crime_year_h$CrimeDate,label=TRUE) 
crime_year_h$dow = with(crime_year_h, factor(dow, levels = rev(levels(dow)))) 
crime_year_h <- crime_year_h %>%
  group_by(dow) %>%
  dplyr::summarize(Value=n()) # Total number of shootings by day
crime_year_h$type <- as.character("Homicide")
crime_year_h$year <- 2017

crime_year <- rbind(crime_year_s, crime_year_h)

# Create the heat map
heatmap3 <- ggplot(crime_year,aes(x=year, y=dow, fill=Value)) + 
  geom_tile(colour="blue",size=.1) + 
  scale_fill_gradient(high="Red",low= "Yellow") + 
  guides(fill=guide_legend(title="Scale")) + 
  theme_minimal(base_size = 10) + 
  removeGrid() +
  rotateTextX() + 
  ggtitle("Heat Map of Shootings and Homicides by Weekday in Baltimore, 2017",subtitle = "Using Open Data from Data.BaltimoreCity.Gov") + 
  labs(x="2016", y="Day of the Week") + 
  theme(
    plot.title=element_text(hjust=0), 
    axis.ticks=element_blank(),
    axis.text=element_text(size=7),
    legend.title=element_text(size=8),
    axis.text.x = element_blank(),
    legend.text=element_text(size=6),
    legend.position="right") +
  geom_text(data = crime_year, aes(year,dow,label=Value,fontface="bold"),colour="black",size=2.5) +
  facet_wrap(~type,nrow =2 )

heatmap3 # Year, Day

heatmap2 # Year, Month, Day

heatmap # Year, Month, Week, Day