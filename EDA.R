# Import the necessary libraries

libs <- c("ggplot2", "dplyr", "readr")
lapply(libs, require, character.only = TRUE)

# Import the data sets

kg_movies <- read.csv("Databases/movies.csv")
kg_netflix <- read.csv("Databases/netflix_titles.csv")
kg_prime <- read.csv("Databases/amazon_prime_titles.csv")

tmdb_movies <- read.csv("Databases/tmdb_5000_movies.csv")
tmdb_credits <- read.csv("Databases/tmdb_5000_credits.csv")

# The tmdb_credits data frame contains movie_id, title, cast, and crew columns.
# The cast and crew columns are dictionaries containing multiple entries.
# They can be considered datasets on their own.
# The tmdb_credits data frame on the other hand has a lots of different columns
# mostly in a structured format. Some of the variables like keywords contain
# multiple values in a dictionary like structure

# The kaggle movies data set is just beauty to the eye
# Kaggle Netflix has some columns with multiple values such as cast and listed in
# Kaggle Amazon Prime has the same structure as the Netflix data set

# Start the EDA process
colnames(tmdb_movies)
colnames(kg_prime)
