# Import the necessary libraries

libs <- c("ggplot2", "dplyr", "readr")
lapply(libs, require, character.only = TRUE)

# Import the data sets

# kg_movies <- read.csv("Databases/movies.csv")
# kg_netflix <- read.csv("Databases/netflix_titles.csv")
# kg_prime <- read.csv("Databases/amazon_prime_titles.csv")
# 
# tmdb_movies <- read.csv("Databases/tmdb_5000_movies.csv")
# tmdb_credits <- read.csv("Databases/tmdb_5000_credits.csv")
# 
# movies_data <- read.csv("Databases/movie_data.csv")
# 
# dirs <- read.csv("Databases/movies_1970_2018.csv")

kg <- read.csv("Databases/Kaggle_movies.csv")

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

head(tmdb_movies[, "production_countries"])

head(tmdb_movies)


# create a column in dataset to group countries to continents
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Finland", "France", "Georgia", "Greece", "Germany",
                  "Hungary", "Iceland", "Ireland", "Italy", "Malta", "Netherlands", "Norway", "Poland", "Portugal",
                  "Republic of Macedonia", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                  "United Kingdom", "West Germany", "Yugoslavia")

others <- c("Afghanistan", "Australia", "Bahamas", "Cambodia", "Cameroon", "Canada", "Chile", "China", "Colombia",
            "Dominican Republic", "Egypt", "Hong Kong", "India", "Indonesia", "Iran", "Israel", "Japan", "Kenya")

kg <- kg[kg$country != "", ]

d <- kg %>% mutate(
  region = case_when(
    country %in% eu_countries ~ "Europe",
    country == "United States" ~ "USA",
    T ~ "Others"
  )
)

table(d$region)[1]

# Filter only Action, Drama, Comedy, Horror, Animation
usa_grp <- d %>% 
  filter(genre %in% c("Action", "Drama", "Comedy", "Horror", "Animation") & region == "USA") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(d$region)[3],
            count = n())

eu_grp <- d %>% 
  filter(genre %in% c("Action", "Drama", "Comedy", "Horror", "Animation") & region == "Europe") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(d$region)[1],
            count = n())

others_grp <- d %>% 
  filter(genre %in% c("Action", "Drama", "Comedy", "Horror", "Animation") & region == "Others") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(d$region)[2],
            count = n())

grp <- rbind(usa_grp, eu_grp, others_grp)

ggplot(grp, aes(x=region, y=prop, fill=genre)) +
  geom_bar(stat="identity", position="dodge")

# Conduct a chisquare test for this contingency table
# Take the first genre
# d <- d %>% 
#   mutate(main_genre = strsplit(genres, "\\|"))
# unique(d$genres)


##### genre profit over years 
p <- d %>%
  # filter(genre %in% genre_list) %>% 
  mutate(profit = (gross - budget) / budget * 100)


high_profit_genre_list <- p[p$profit %in% sort(p$profit, decreasing = T)[1:100], "genre"]
table(high_profit_genre_list)

profit <- p %>% 
  group_by(genre) %>% 
  summarize(avg_profit = mean(profit, na.rm =T)) %>% 
  arrange(desc(avg_profit)) # Take top for and plot on a time series

ggplot()
# These are the highest profiting genres on average
top_genre_list <- c("Horror", "Family", "Thriller", "Animation")
profit <- p %>% 
  filter(genre %in% genre_list) %>% 
  group_by(year, genre) %>% 
  summarize(avg_profit_perc = mean(profit, na.rm = T))

p_time_series <- ggplot(profit, aes(x=year, y=avg_profit_perc, color=genre)) +
  geom_line() +
  geom_point()

d %>% 
  group_by(genre,year) %>%
  summarize(avg_gross = mean(gross)) %>%
  ggplot(mapping = aes(x = year, y = avg_gross, color=genre)) +
  geom_point() + 
  geom_line() +
  ylab("Average Gross Revenue (in US Dollars)") +
  ggtitle("Gross Revenue Over Time")
