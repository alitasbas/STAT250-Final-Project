# Import the necessary libraries

libs <- c("ggplot2", "dplyr", "readr", "magrittr", "jsonlite", "stringr", "tidyverse", "corrplot", "car")
lapply(libs, require, character.only = TRUE)

# Import the data sets

mvs <- read.csv("Databases/movies.csv")

# The tmdb_credits data frame contains movie_id, title, cast, and crew columns.
# The cast and crew columns are dictionaries containing multiple entries.
# They can be considered datasets on their own.
# The tmdb_credits data frame on the other hand has a lots of different columns
# mostly in a structured format. Some of the variables like keywords contain
# multiple values in a dictionary like structure

# The movies data set is just beauty to the eye

# Start the EDA process
colnames(mvs)
colSums(is.na(mvs))


# Let's start our analysis from the consumer side. How have the genre distributions changed
# over time. It is probably shaped according to the viewers.

# mvs %>% group_by(year, genre) %>% 
#   summarize(count = n(),
#             prop = n()) %>% 
#   ggplot(aes(x=year, y = prop)) +
#   geom_line()


genre_counts <- mvs %>%
  group_by(year, genre) %>%
  summarize(count = n(), .groups = 'drop')

# Calculate the total number of movies per year
yearly_counts <- genre_counts %>%
  group_by(year) %>%
  summarize(total = sum(count), .groups = 'drop')

# Merge the two datasets and calculate the proportion
genre_proportions <- genre_counts %>%
  left_join(yearly_counts, by = "year") %>%
  mutate(proportion = count / total) %>%
  select(year, genre, proportion)

p <- genre_proportions %>% 
  ggplot(aes(x = year, y = proportion, color = genre)) +
  geom_line() +
  labs(title = "Proportion of Movie Genres Over Time", 
       x = "Year", 
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1")+
  theme_minimal()

ggsave(filename = "All_Genre_Proportions_Over_Time.png", plot = p)

# There are some genres which are very rare. Get rid of them
per_year_movie_counts <- genre_counts %>% group_by(genre) %>% 
  summarize(mean_count_per_year = mean(count, na.rm = T)) %>% 
  arrange(desc(mean_count_per_year)) # Take the genres that have an average of more than 5

genres_of_interest <- per_year_movie_counts %>% 
  filter(mean_count_per_year > 5) %>% 
  pull(genre)

# Visualization for these genres
# Line Plot
p <- genre_proportions %>% 
  filter(genre %in% genres_of_interest) %>% 
  ggplot(aes(x=year, y = proportion, color = genre)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Proportion of Movie Genres Over Time") +
  scale_fill_brewer(palette = "Set1") +
  theme_light()

ggsave(filename = "Main_Genre_Proportions_Over_Time.png", plot = p)

# Stacked Area Plot
p <- genre_proportions %>% 
  filter(genre %in% genres_of_interest) %>% 
  ggplot(aes(x=year, y = proportion, fill = genre)) +
  geom_area(linewidth = 1.6) +
  labs("Proportion of Movie Genres Over Time") +
  scale_fill_brewer(palette = "Set1") +
  theme_light()

ggsave(filename = "genre stacked plot.png", plot = p)

# Faceted Area Plot
p <- genre_proportions %>% 
  filter(genre %in% genres_of_interest) %>% 
  ggplot(aes(x=year, y = proportion, fill = genre)) +
  geom_area() +
  theme_light() +
  facet_wrap(~genre)

ggsave(filename = "faceted genre plots.png", plot = p)

# It seems interesting...


# 1.2- Are the genre distributions different across regions?
# create a region column to group countries to based on regions
eu_countries <- c("Austria", "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Finland", "France", "Georgia", "Greece", "Germany",
                  "Hungary", "Iceland", "Ireland", "Italy", "Malta", "Netherlands", "Norway", "Poland", "Portugal",
                  "Republic of Macedonia", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                  "United Kingdom", "West Germany", "Yugoslavia")

others <- c("Afghanistan", "Australia", "Bahamas", "Cambodia", "Cameroon", "Canada", "Chile", "China", "Colombia",
            "Dominican Republic", "Egypt", "Hong Kong", "India", "Indonesia", "Iran", "Israel", "Japan", "Kenya")

mvs <- mvs[mvs$country != "", ]

mvs <- mvs %>% mutate(
  region = case_when(
    country %in% eu_countries ~ "Europe",
    country == "United States" ~ "USA",
    T ~ "Others"
  )
)

table(mvs$region)

# To construct a side-by-side bar chart, extract the distributions 
# Filter only Action, Drama, Comedy, Horror, Animation
acdh_genres <- c("Action", "Drama", "Comedy", "Horror", "Animation")

usa_grp <- mvs %>% 
  filter(genre %in% acdh_genres & region == "USA") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(mvs$region)[3],
            count = n())

eu_grp <- mvs %>% 
  filter(genre %in% acdh_genres & region == "Europe") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(mvs$region)[1],
            count = n())

others_grp <- mvs %>% 
  filter(genre %in% acdh_genres & region == "Others") %>% 
  group_by(region, genre) %>% 
  summarize(prop = n() / table(mvs$region)[2],
            count = n())

props_grp <- rbind(usa_grp, eu_grp, others_grp)

p <- props_grp %>% ggplot(aes(x=region, y=prop, fill=genre)) +
  geom_bar(stat="identity", position="dodge") + 
  labs(title = "Proportion of Movie Genres by Region", 
       x = "Region", 
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggsave(filename = "Genre over Regions Bar.png", plot = p)

# Conduct a chisquare test for this contingency table
# Are their differences in culture and region regarding genre preference

genre_contingency_table <- props_grp %>% 
  select(region, genre, count) %>% 
  pivot_wider(names_from = genre, values_from = count)

chisq_result <- chisq.test(as.matrix(genre_contingency_table[, 2:6])) # Very Significant



# 2- Genre profit over years KG
summary(mvs[, c("budget", "gross")])
nrow(mvs[mvs$budget==0, ])
nrow(mvs[mvs$gross==0, ])
# As movies can't be produced for free, remove all observations with budget=0.
# Additionally, some budget entries are in millions. Take budget greater than 10000.
mvs %<>% 
  filter(budget > 10000)

# There are some flops and wrong values for revenues. Get rid of them, too.
mvs %<>% 
  filter(gross > 10000)

mvs %<>%
  mutate(profit = (gross - budget) / budget * 100)
# There are some outliers. We decided to increase outlier tolerance as they are true
# but extreme values. Remove the 3 highest profits.
summary(mvs$profit)

mvs %<>%
  filter(profit < sort(mvs$profit, decreasing = T)[3])

genre_profits <- mvs %>% 
  group_by(genre) %>% 
  summarize(avg_profit = mean(profit, na.rm =T)) %>% 
  arrange(desc(avg_profit)) # Take top four and plot on a time series

genre_profits

# Family seems unusually high
mvs %>% 
  filter(genre=="Family") %>% 
  arrange(profit)

# Our sample size is extremely small with an extreme observation
# Also, we did not consider thriller because of its small sample size.

# These are the highest viable profiting genres on average
top_genre_list <- c("Horror", "Drama", "Comedy", "Animation")

# Construct a time-series plot
profit_df <- mvs %>% 
  filter(genre %in% top_genre_list) %>% 
  group_by(year, genre) %>% 
  summarize(avg_profit_perc = mean(profit, na.rm = T))

profit_time_series <- ggplot(profit_df, aes(x=year, y=avg_profit_perc, color=genre)) +
  geom_line(linewidth=1) +
  scale_color_brewer(palette = "Set1")

ggsave(filename = "Profit Percentage of main genres.png", plot = profit_time_series)

profit_time_series # Comment on our findings

# The horror genre seems extremely profitable
# To observe the relationship among the others better, plot without horror

profit_time_series_no_horror <- profit_df %>% 
  filter(genre != "Horror") %>% 
  ggplot(aes(x=year, y=avg_profit_perc, color=genre)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Average Profit Percentage Over Time by Genre (Excluding Horror)", 
       x = "Year", 
       y = "Average Profit Percentage") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

ggsave(filename = "Profit Percentage of main genres wihtout horror.png", plot = profit_time_series_no_horror)

profit_time_series_no_horror # Comment on our findings


#####################################################################
# 3- Compare most successful directors

sum(is.na(mvs))
## colnames(cinema)
mvs %>% 
  filter(genre == "Drama") %>% 
  nrow()
## Extract most profitable 15 directors for action genre.
action_directors <- mvs %>% 
  filter(genre=="Action") %>% 
  group_by(director) %>% 
  summarise(total_profit =sum(profit,na.rm = TRUE)) %>% 
  arrange(desc(total_profit)) %>% 
  slice_head(n=15)

## Extract most profitable 15 directors for drama genre.

drama_directors <- mvs %>%
  filter(genre == "Drama") %>%
  group_by(director) %>%
  summarise(total_profit = sum(profit, na.rm = TRUE)) %>%
  arrange(desc(total_profit)) %>%
  slice_head(n = 15)

## Combine most profitable directors and their profits in drama genre.
## There are giants like Clint Eastwood, Steven Spielberg, James Cameron.
top_drama_directors <- drama_directors$director
drama_profits <- mvs %>%
  filter(director %in% top_drama_directors & genre == "Drama") %>%
  arrange(desc(profit)) %>% 
  select(director, profit)
drama_profits

## Combine most profitable directors and their profits in action genre.
## There are giants like J.J Abrams, Michael Bay and James Cameron.
top_action_directors <- action_directors$director
action_profits <- mvs %>%
  filter(director %in% top_action_directors & genre == "Action") %>%
  arrange(desc(profit)) %>% 
  select(director, profit)
action_profits
# Combine the profits data for the t-test
combined_profits <- rbind(
  data.frame(director = drama_profits$director, profit = drama_profits$profit, genre = "Drama"),
  data.frame(director = action_profits$director, profit = action_profits$profit, genre = "Action")
)

# Print combined profits data for verification
print(combined_profits)

# Separate the profits by genre
drama_profit_values <- combined_profits %>%
  filter(genre == "Drama") %>%
  pull(profit)
action_profit_values <- combined_profits %>%
  filter(genre == "Action") %>%
  pull(profit)
drama_profit_values
action_profit_values



t_test_result <- t.test(drama_profit_values, action_profit_values, alternative = "greater")
t_test_result # Successfull drama movies have higher profits than action movies 


## p - value is 0.004181
## based on a 95% confidence interval, since p-value < 0.05, there is a significant difference between 
## the mean profits of the most successful 15 directors in the Action and Drama genres.


########################################################################
# 4- Let's analyze the top 10 most successful movies for the past 40 years

top10blockbusters <- read.csv("Databases/blockbuster-top_ten_movies_per_year_DFE.csv")

head(top10blockbusters)
str(top10blockbusters)

# alter worldwide_gross to numeric
top10blockbusters$worldwide_gross <- as.numeric(gsub("[\\$,]", "", top10blockbusters$worldwide_gross))

# group year and get the top 10 movies by worldwide gross for each year
top10blockbusters <- top10blockbusters %>%
  group_by(year) %>%
  top_n(10, worldwide_gross)


# the average worldwide gross
average_gross_per_year <- top10blockbusters %>%
  group_by(year) %>%
  summarise(average_gross = mean(worldwide_gross))


# plotting gross(profit!) of top 10 films for every year 

# Plot the regression
p <- ggplot(average_gross_per_year, aes(x = year, y = average_gross / 1e6)) +
  geom_smooth(color = "blue", size = 1, method = "lm", se = F) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Gross of Top 10 Movies Worldwide by Year",
       subtitle = "Data represents the average gross of the top 10 highest-grossing movies each year",
       x = "Year",
       y = "Average Gross (in million $)") +
  theme_minimal()

ggsave(filename = "Gross of BlockBusters over time correlation.png", plot = p)


p <- ggplot(average_gross_per_year, aes(x = year, y = average_gross / 1e6)) +
  geom_point(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Gross of Top 10 Movies Worldwide by Year",
       subtitle = "Data represents the average gross of the top 10 highest-grossing movies each year",
       x = "Year",
       y = "Average Gross (in million $)") +
  theme_minimal()

ggsave(filename = "Gross of BlockBusters over time.png", plot = p)

# Is there a significant trend in the average gross of the top 10 movies worldwide over the years?


# linear regression model
linear_model <- lm(average_gross ~ year, data = average_gross_per_year)
summary(linear_model)

#intercept: -4.456e+10
#Both coefficients have p-values < 2e-16, which are highly significant, indicating strong evidence against the null hypothesis.
#p-value: < 2.2e-16
#The p-value associated with the F-statistic is extremely low, indicating that the model is highly significant.
#The model is statistically significant, indicating a strong relationship between the year and the average gross of the top 10 movies.
#The high R-squared value indicates that the model explains a large proportion of the variance in the average gross, suggesting a strong fit.

#normally distrubuted

par(mfrow = c(2, 2))
plot(linear_model)


##Non-Linearity: The slight curve in the Residuals vs Fitted plot suggests that a simple linear model might not be the best fit. 
##A polynomial regression or another non-linear model might capture the relationship better.
#Normality of Residuals: The normality assumption is reasonably satisfied, though there are some deviations at the extremes.
#Homoscedasticity: There is a slight indication of heteroscedasticity, but it is not severe.
#Influential Points: Points 35 and 38 could be influential and should be examined further.



# normality of residuals
shapiro.test(linear_model$residuals)


# launch ANOVA test to compare the models

######################################################

# 5- Production company analysis

dist <- read.csv("Databases/Movie_Distributors_1995-2019.csv")
head(dist)
sum(is.na(dist))

dist %<>% 
  mutate(Revenue.per.Film = Revenue.per.Film / 1000000,
         Gross.Revenue = Gross.Revenue / 1000000)

# Plot number of movies over the years
p <- ggplot(dist, aes(x = Year, y = Films.Distributed, color = X.Distributor)) +
  geom_line(aes(group = X.Distributor)) +
  geom_point() +
  labs(title = "Number of Movies Released Over the Years by Distributor", 
       x = "Year", 
       y = "Number of Movies") +
  theme_minimal()

ggsave(filename = "Total movie per distributor.png", plot = p)

# Plot average gross revenue per film over the years
p <- ggplot(dist, aes(x = Year, y = Revenue.per.Film, color = X.Distributor)) +
  geom_line(aes(group = X.Distributor)) +
  geom_point() +
  labs(title = "Average Gross Revenue Per Film Over the Years by Distributor", 
       x = "Year", 
       y = "Average Gross Revenue Per Film (in Millions)") +
  theme_minimal()

ggsave(filename = "Average gross per film each distributor.png", plot = p)

# Plot the total gross revenue over the years
p <- ggplot(dist, aes(x = Year, y = Gross.Revenue, color = X.Distributor)) +
  geom_line(aes(group = X.Distributor)) +
  geom_point() +
  labs(title = "Total Gross Revenue Over the Years by Distributor", 
       x = "Year", 
       y = "Total Gross Revenue (in Millions)") +
  theme_minimal()

ggsave(filename = "Total Gross each distributor.png", plot = p)


# Create a Walt Disney only dataframe
walt_disney_movies <- mvs %>%
  filter(str_detect(company,"Walt")) %>% 
  select(name, genre, year, score, budget, gross, profit)

table(walt_disney_movies$year)

# group every 5 years after 1995 togethter
walt_disney_movies %<>% 
  mutate(gross = gross / 1000000) %>% 
  filter(year >= 1995) %>% 
  mutate(year_cat = cut(year, breaks=c(1994, 2000, 2005, 2010, 2015, 2020),
                        labels = c("1995-2000", "2001-2005",
                                   "2006-2010", "2011-2015", "2016-2020")))

all_movies_year_cat_mean <- mvs %>% 
  mutate(gross = gross / 1000000) %>% 
  filter(year >= 1995) %>% 
  mutate(year_cat = cut(year, breaks=c(1994, 2000, 2005, 2010, 2015, 2021),
                        labels = c("1995-2000", "2001-2005",
                                   "2006-2010", "2011-2015", "2016-2020"))) %>% 
  group_by(year_cat) %>% 
  summarize(mean_gross = mean(gross, na.rm = T), .groups = "drop") %>% 
  mutate(company = "All")

walt_disney_year_cat_mean <- walt_disney_movies %>% 
  group_by(year_cat) %>% 
  summarize(mean_gross = mean(gross, na.rm = T), .groups = "drop") %>% 
  mutate(company = "Walt Disney")
  
grouped <- rbind(walt_disney_year_cat_mean, all_movies_year_cat_mean)

# grouped$year_cat <- factor(grouped$year_cat, levels = c(""))

p <- ggplot(grouped, aes(x=year_cat, y=mean_gross, fill=company)) +
  geom_col(position = "dodge") +
  labs(title = "Comparing Walt Disney to the market",
       x = "Year",
       y = "Mean Gross (in millions)")

ggsave(filename = "Walt vs Industry Bar graph.png", plot = p)

# After the agreement with Lucas Studios in 2009  Warner Bros dominated the industry
top_50_gross <- mvs %>% 
  filter(year >= 2009) %>% 
  arrange(desc(gross)) %>% 
  slice_head(n=50)

table(top_50_gross$company)

# Out of the 50 top grossing movies 10 + 2 + 7 + 2 = 21 were produced by Walt Disney firms


# Lets us test weather Walt Disney really did dominate the industry after 2009
sort(table(mvs$company), decreasing = T)[1:15]

# filter for the movies after the deal with Marvel and Lucasfilms
prime_walt_disney_gross <- mvs %>% 
  filter(year > 2009 & (company == "Lucasfilm" | str_detect(company, "Marvel") | str_detect(company, "Walt"))) %>% 
  select(gross)

wd_mean <- mean(prime_walt_disney_gross$gross)

post_2009_movies_gross <- mvs %>% 
  filter(year > 2009) %>% 
  select(gross)

post_mean <- mean(post_2009_movies_gross$gross)

test <- t.test(prime_walt_disney_gross$gross, mu = post_mean, alternative = "greater") # EZ reject
# Walt Disney is the father of the industry
