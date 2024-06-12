df = read.csv("movies.csv")
df <- df %>% mutate(profit = gross - budget)
df$genre <- as.factor(df$genre)
df$company = as.factor(df$company)
model <- lm(profit ~ budget + genre + company + runtime + score, data = df)
summary(model)

#########################################################################

model_summary = summary(model)
coefficients_summary <- coef(model_summary)

# Extract the p-values
p_values <- coefficients_summary[, "Pr(>|t|)"]

# Select predictors with p-values higher than 0.05
non_significant_predictors <- names(p_values[p_values > 0.05])

# Handle variables with spaces or special characters
significant_predictors <- sapply(significant_predictors, function(x) {
  if (grepl("[[:punct:][:space:]]", x)) {
    paste0("`", x, "`")
  } else {
    x
  }
})


df["company"] <- sapply(df["company"], function(x) {
  if (grepl("[[:punct:][:space:]]", x)) {
    paste0("`", x, "`")
  } else {
    x
  }
})



# Construct the new formula excluding non-significant predictors
significant_predictors <- setdiff(names(p_values), non_significant_predictors)
new_formula <- as.formula(paste("profit ~", paste(significant_predictors[-1], collapse = " + ")))

# Fit the updated model with only significant predictors
updated_model <- lm(new_formula, data = df)

# Print the summary of the updated model
summary(updated_model)


