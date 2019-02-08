# Analysis script: compute values and create graphics of interest
library("dplyr")
library("ggplot2")
library("lubridate")
library("tidyr")
library("ggmap")

# Load in your data
eviction <- read.csv("data/Eviction_Notices.csv",stringsAsFactors = FALSE)
# Compute some values of interest and store them in variables for the report

# How many evictions were there?
num_evictions <- nrow(eviction)
num_features <- ncol(eviction)
num_features
# Create a table (data frame) of evictions by zip code (sort descending)
by_zip <- eviction%>%
 group_by(Eviction.Notice.Source.Zipcode)%>%
  count()%>%#count the frequencies
  arrange(-n)%>%
  ungroup()%>%
  top_n(10,wt = n)

# Create a plot of the number of evictions each month in the dataset
by_month <- eviction%>%
  mutate(date = as.Date(File.Date,format = "%m/%d/%y"))%>%
  mutate(month = floor_date(date,unit = "month"))%>%
  group_by(month)%>%
  count()
View(by_month)
# Store plot in a variable
ggplot(data = by_month) + 
  geom_line(mapping = aes(x = month,y = n)) + 
  labs(x = "date", y = "Number of Evictions", title = "Evi over time")
# Map evictions in 2017 

# Format the lat/long variables, filter to 2017

# Create a maptile background

# Add a layer of points on top of the map tiles
