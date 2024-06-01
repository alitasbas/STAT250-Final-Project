# Import the necessary libraries

libs <- c("ggplot2", "dplyr", "readr", "magrittr", "jsonlite")
lapply(libs, require, character.only = TRUE)

# Import the data sets
tmdb <- read.csv("Databases/tmdb_5000_movies.csv")

kg <- read.csv("Databases/Kaggle_movies.csv")

# The tmdb_credits data frame contains movie_id, title, cast, and crew columns.
# The cast and crew columns are dictionaries containing multiple entries.
# They can be considered datasets on their own.
# The tmdb_credits data frame on the other hand has a lots of different columns
# mostly in a structured format. Some of the variables like keywords contain
# multiple values in a dictionary like structure

# The movies data set is just beauty to the eye

# Start the EDA process
colnames(tmdb)
colnames(kg)

head(tmdb[, "production_countries"])

head(tmdb)

# Are the genre distributions different across regions?
# create a region column to group countries to based on regions
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Finland", "France", "Georgia", "Greece", "Germany",
                  "Hungary", "Iceland", "Ireland", "Italy", "Malta", "Netherlands", "Norway", "Poland", "Portugal",
                  "Republic of Macedonia", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                  "United Kingdom", "West Germany", "Yugoslavia")

others <- c("Afghanistan", "Australia", "Bahamas", "Cambodia", "Cameroon", "Canada", "Chile", "China", "Colombia",
            "Dominican Republic", "Egypt", "Hong Kong", "India", "Indonesia", "Iran", "Israel", "Japan", "Kenya")

kg <- kg[kg$country != "", ]

kg <- kg %>% mutate(
  region = case_when(
    country %in% eu_countries ~ "Europe",
    country == "United States" ~ "USA",
    T ~ "Others"
  )
)

table(kg$region)

# To construct a side-by-side bar chart, extract the distributions 
# Filter only Action, Drama, Comedy, Horror, Animation
acdh_genres <- c("Action", "Drama", "Comedy", "Horror", "Animation")

usa_grp <- kg %>% 
  filter(genre %in% acdh_genres & region == "USA") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(kg$region)[3],
            count = n())

eu_grp <- kg %>% 
  filter(genre %in% acdh_genres & region == "Europe") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(kg$region)[1],
            count = n())

others_grp <- kg %>% 
  filter(genre %in% acdh_genres & region == "Others") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(kg$region)[2],
            count = n())

props_grp <- rbind(usa_grp, eu_grp, others_grp)

genre_region_props <- ggplot(props_grp, aes(x=region, y=prop, fill=genre)) +
  geom_bar(stat="identity", position="dodge")

genre_region_props

# Conduct a chisquare test for this contingency table
# Take the first genre
# kg <- kg %>% 
#   mutate(main_genre = strsplit(genres, "\\|"))
# unique(kg$genres)



# 2- Genre profit over years TMDB
# summary(tmdb[, c("budget", "revenue")])
# nrow(tmdb[tmdb$budget==0, ])
# nrow(tmdb[tmdb$revenue==0, ])
# # As movies can't be produced for free, remove all observations with budget=0.
# # Additionally, some budget entries are in millions. Take budget greater than 10000.
# tmdb %<>% 
#   filter(budget > 10000)
# 
# # There are some flops and wrong values for revenues. Get rid of them, too.
# tmdb %<>% 
#   filter(revenue > 10000)
# 
# tmdb %<>%
#   mutate(profit = (revenue - budget) / budget * 100)
# 
# profit <- tmdb %>% 
#   group_by(genres) %>% 
#   summarize(avg_profit = mean(profit, na.rm =T)) %>% 
#   arrange(desc(avg_profit)) # Take top for and plot on a time series
# # These are the highest profiting genres on average
# top_genre_list <- c("Horror", "Family", "Thriller", "Animation")
# 
# # Currently, the genre column is in JSON format. We want to extract the first genre.
# extract_first_name <- function(json_str) {
#   genre_list <- fromJSON(json_str)  # Convert JSON string to list
#   return(genre_list[[1]]$name)  # Extract the name of the first genre
# }
# 
# # Apply the function to the column
# tmdb$first_genre_name <- sapply(tmdb$genres, extract_first_name)


# 2- Genre profit over years KG
summary(kg[, c("budget", "gross")])
nrow(kg[kg$budget==0, ])
nrow(kg[kg$gross==0, ])
# As movies can't be produced for free, remove all observations with budget=0.
# Additionally, some budget entries are in millions. Take budget greater than 10000.
kg %<>% 
  filter(budget > 10000)

# There are some flops and wrong values for revenues. Get rid of them, too.
kg %<>% 
  filter(gross > 10000)

kg %<>%
  mutate(profit = (gross - budget) / budget * 100)
# There are some outliers. We decided to increase outlier tolerance as they are true
# but extreme values. Remove the 3 highest profits.
summary(kg$profit)

kg %<>%
  filter(profit < sort(kg$profit, decreasing = T)[3])

genre_profits <- kg %>% 
  group_by(genre) %>% 
  summarize(avg_profit = mean(profit, na.rm =T)) %>% 
  arrange(desc(avg_profit)) # Take top four and plot on a time series

genre_profits

# Family seems unusually high
kg %>% 
  filter(genre=="Animation") %>% 
  arrange(profit)

# Our sample size is extremely small with an extreme observation
# Also, we did not consider thriller because of its small sample size.

# These are the highest viable profiting genres on average
top_genre_list <- c("Horror", "Drama", "Comedy", "Animation")

# Construct a time-series plot
profit_df <- kg %>% 
  filter(genre %in% top_genre_list) %>% 
  group_by(year, genre) %>% 
  summarize(avg_profit_perc = mean(profit, na.rm = T))

profit_time_series <- ggplot(profit_df, aes(x=year, y=avg_profit_perc, color=genre)) +
  geom_line() +
  geom_point()

profit_time_series # Comment on our findings

# The horror genre seems extremely profitable
# To observe the relationship among the others better, plot without horror

profit_time_series_no_horror <- profit_df %>% 
  filter(genre != "Horror") %>% 
  ggplot(aes(x=year, y=avg_profit_perc, color=genre)) +
  geom_line() +
  geom_point()

profit_time_series_no_horror # Comment on our findings


