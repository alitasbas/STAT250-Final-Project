mov <- read.csv("Databases/Kaggle_movies.csv")
# movies <- read.csv("movies_1970_2018.csv")


#observe data
str(mov)
na.omit(mov)
head(mov)

#we will work on profit so create profit coulmn
mov <- mov %>% 
  mutate(profit = gross - budget)
head(mov)

mov <- mov %>% 
  mutate(profit_percentage = profit/budget * 100)



#find out 10 films have highest profit

top_10_profits <- mov %>%
  group_by(year) %>%
  arrange(year, desc(profit)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(name, year, profit, profit_percentage)

print(top_10_profits)






#Calculate the average profit for each genre
avg_profit_by_genre <- mov %>%
  group_by(genre) %>%
  summarize(avg_profit = mean(profit, na.rm = TRUE))
            
print(avg_profit_by_genre)

#By using bar plot, Visualize the average profit by genre
ggplot(head(avg_profit_by_genre, 7), aes(x = reorder(genre, -avg_profit), y = avg_profit)) +
  geom_bar(stat = "identity", fill = "deeppink2") +
  labs(title = "Average Profit by Genre", x = "Genre", y = "Average Profit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#We'll perform a two-sample t-test to compare the profit percentages of the top two genres (Horror and Action)


#H0: There is no! difference in the average profit percentage between Horror and Action movies.
#H1: There is a difference in the average profit percentage between Horror and Action movies.



horror_action <- mov %>%
  filter(genre %in% c("Horror", "Action"))

# Separate the profit percentage for the two genres
horror_profit_percentage <- horror_action %>%
  filter(genre == "Horror") %>%
  pull(profit_percentage)

action_profit_percentage <- horror_action %>%
  filter(genre == "Action") %>%
  pull(profit_percentage)

#conducting two sample t-test
t_test_result <- t.test(horror_profit_percentage, action_profit_percentage, alternative = "two.sided")


print(t_test_result)




# Interpret the results wrt. p-value...

if (t_test_result$p.value < 0.05) {
  cat("We reject the null hypothesis. There is a significant difference in the average profit percentage between Horror and Action movies.\n")
} else {
  cat("We fail to reject the null hypothesis. There is no significant difference in the average profit percentage between Horror and Action movies.\n")
}


# t = 1.3868, df = 253.01, p-value = 0.1667
























