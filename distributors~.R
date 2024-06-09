
getwd()
setwd("/Users/azimyunusalparslan/Desktop")
dist <- read.csv("dist.csv")
head(dist)
sum(is.na(dist))

# Load necessary libraries
library(ggplot2)
library(dplyr)


ggplot(dist, aes(x = Year, y = Gross.Revenue, color = X.Distributor)) +
   geom_line(aes(group = X.Distributor)) +
   geom_point() +
   labs(title = "Total Gross Revenue Over the Years by Distributor", 
        x = "Year", 
        y = "Total Gross Revenue") +
   theme_minimal()

# Plot number of movies over the years
ggplot(dist, aes(x = Year, y = Films.Distributed, color = X.Distributor)) +
   geom_line(aes(group = X.Distributor)) +
   geom_point() +
   labs(title = "Number of Movies Released Over the Years by Distributor", 
        x = "Year", 
        y = "Number of Movies") +
   theme_minimal()

# Plot average gross revenue per film over the years
ggplot(dist, aes(x = Year, y = Revenue.per.Film, color = X.Distributor)) +
   geom_line(aes(group = X.Distributor)) +
   geom_point() +
   labs(title = "Average Gross Revenue Per Film Over the Years by Distributor", 
        x = "Year", 
        y = "Average Gross Revenue Per Film") +
   theme_minimal()

ggplot(dist, aes(x = Year, y = Gross.Revenue, color = X.Distributor)) +
   geom_line(aes(group = X.Distributor)) +
   geom_point() +
   labs(title = "Total Gross Revenue Over the Years by Distributor", 
        x = "Year", 
        y = "Total Gross Revenue") +
   theme_minimal()

