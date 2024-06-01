cinema <- read.csv("moviesfinal.csv")
head(cinema)
is.na(cinema)
na.omit(cinema)

## We are interested about profit of the movies so i add a profit column.
cinema  <- cinema %>% 
   mutate(profit = gross - budget)
#There are some NA values.


##colnames(cinema)
cinema %>% 
   filter(genre == "Drama") %>% 
   nrow()
##Extract most profitable 5 directors for action genre.
action_directors <- cinema %>% 
   filter(genre=="Action") %>% 
   group_by(director) %>% 
   summarise(total_profit =sum(profit,na.rm = TRUE)) %>% 
   arrange(desc(total_profit)) %>% 
   slice_head(n=5)

##Extract most profitable 5 directors for drama genre.

drama_directors <- cinema %>%
   filter(genre == "Drama") %>%
   group_by(director) %>%
   summarise(total_profit = sum(profit, na.rm = TRUE)) %>%
   arrange(desc(total_profit)) %>%
   slice_head(n = 5)

## Combine most profitable directors and their profits in drama genre.
## There are stars like Clint Eastwood, Steven Spielberg, James Cameron.
top_drama_directors <- drama_directors$director
drama_profits <- cinema %>%
   filter(director %in% top_drama_directors) %>%
   select(director, profit) 
drama_profits

## Combine most profitable directors and their profits in action genre.
## There are giants like J.J Abrams, Michael Bay and James Cameron.
top_action_directors <- action_directors$director
action_profits <- cinema %>%
   filter(director %in% top_action_directors) %>%
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
drama_profit_values
action_profit_values
action_profit_values <- combined_profits %>%
   filter(genre == "Action") %>%
   pull(profit)


t_test_result <- t.test(drama_profit_values, action_profit_values)
t_test_result



## p - value is 0.004181
##  based on a 95% confidence interval, since p-value < 0.05, there is a significant difference between 
##  the mean profits of the most successful 5 directors in the Action and Drama genres.
