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

ggplot(average_gross_per_year, aes(x = year, y = average_gross / 1e6)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Gross of Top 10 Movies Worldwide by Year",
       subtitle = "Data represents the average gross of the top 10 highest-grossing movies each year",
       x = "Year",
       y = "Average Gross (in million $)") +
  theme_minimal()



#Is there a significant trend in the average gross of the top 10 movies worldwide over the years?


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



# Plot the polynomial regression
ggplot(average_gross_per_year, aes(x = year, y = average_gross / 1e6)) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue", size = 1) +
  labs(title = "Polynomial Regression: Average Gross of Top 10 Movies Worldwide by Year",
       subtitle = "Data represents the average gross of the top 10 highest-grossing movies each year",
       x = "Year",
       y = "Average Gross (in million $)") +
  theme_minimal()


# launch ANOVA test to compare the models
anova_result <- anova(linear_model, polynomial_model)

print(anova_result)







