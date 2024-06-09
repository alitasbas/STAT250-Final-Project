setwd("/Users/azimyunusalparslan")
getwd()
cinema <- read.csv("moviesfinal.csv")
head(cinema)



library(dplyr)
## We are interested about profit of the movies so i add a profit column.
cinema  <- cinema %>% 
   mutate(profit = gross - budget)
is.na(cinema)
na.omit(cinema)
sum(is.na(cinema))
colSums(is.na(cinema))
cinema_clean <- cinema %>%
   filter(!is.na(profit) & !is.na(gross) & !is.na(budget))

sum(is.na(cinema_clean)) ## still 1
cinema_clean <- cinema_clean %>% 
   filter(!is.na(runtime))
sum(is.na(cinema_clean))
##colnames(cinema)
cinema_clean %>% 
   filter(genre == "Drama") %>% 
   nrow()
##Extract most profitable 15 directors for action genre.
action_directors <- cinema_clean %>% 
   filter(genre=="Action") %>% 
   group_by(director) %>% 
   summarise(total_profit =sum(profit,na.rm = TRUE)) %>% 
   arrange(desc(total_profit)) %>% 
   slice_head(n=15)

##Extract most profitable 15 directors for drama genre.

drama_directors <- cinema_clean %>%
   filter(genre == "Drama") %>%
   group_by(director) %>%
   summarise(total_profit = sum(profit, na.rm = TRUE)) %>%
   arrange(desc(total_profit)) %>%
   slice_head(n = 15)

## Combine most profitable directors and their profits in drama genre.
## There are giants like Clint Eastwood, Steven Spielberg, James Cameron.
top_drama_directors <- drama_directors$director
drama_profits <- cinema_clean %>%
   filter(director %in% top_drama_directors & genre == "Drama") %>%
   select(director, profit) 
drama_profits

## Combine most profitable directors and their profits in action genre.
## There are giants like J.J Abrams, Michael Bay and James Cameron.
top_action_directors <- action_directors$director
action_profits <- cinema_clean %>%
   filter(director %in% top_action_directors & genre == "Action") %>%
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



t_test_result <- t.test(drama_profit_values, action_profit_values)
t_test_result



## p - value is 0.004181
##  based on a 95% confidence interval, since p-value < 0.05, there is a significant difference between 
##  the mean profits of the most successful 15 directors in the Action and Drama genres.
