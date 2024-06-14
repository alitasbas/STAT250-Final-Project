df = read.csv("Databases/Kaggle_movies.csv")
df %<>% 
  filter(budget > 10000)

top_companies <- names(sort(table(df$company), decreasing = T)[1:10])
df %<>% 
  select(score, budget, genre, company, runtime, rating, year) %>% 
  mutate(company = ifelse(company %in% top_companies, company, "other"))

df$genre <- as.factor(df$genre)
df$company = as.factor(df$company)
df$rating <- as.factor(df$rating)

model <- lm(score ~ budget + genre + company + runtime + rating + year, data = df)
summary(model)

#########################################################################

# observe that rating has almost no effect on score
# Some genres have insignificant effect, too
# Out of the companies with most movies, only Walt Disney and Paramount have a sig
# effect on score

df = read.csv("Databases/Kaggle_movies.csv")
df %<>% 
  filter(budget > 10000 & !is.na(runtime) & !is.na(score))

insig_genres <- c("Adventure", "Family", "Fantasy", "Mystery", "Romance", "Sci-Fi", "Thriller", "Western")

df %<>% 
  select(score, budget, genre, company, runtime, year) %>% 
  mutate(company = ifelse(str_detect(company, "Walt"), company, "other"),
         genre = ifelse(genre %in% insig_genres, "other", genre))

model <- lm(score ~ budget + genre + company + runtime + year, data = df)
model_summary = summary(model)
coefficients_summary <- coef(model_summary)

ress <- resid(model)

# Assumptions
numeric_data <- df[, c("budget", "runtime", "year")]
cor_matrix <- cor(numeric_data)
print(cor_matrix)

vif(model) # No Multicollinearity

# Normality of score
shapiro.test(df[sample(nrow(df), 4999), "score"]) # Not Normal

ggplot(df, aes(x=score)) +
  geom_density()

# Transformations
tf <- df %>% 
  mutate(z_score = (score - mean(score, na.rm=T)) / sd(score, na.rm = T) ) %>% 
  slice_sample(n = 4999)

ggplot(tf, aes(x=z_score)) +
  geom_density() # Eliminate outliers

tf %<>% 
  filter(z_score < 2.5 & z_score > -2.5)

shapiro.test(tf$z_score) # There is an improvement 

# T2
tf2 <- df %>% 
  mutate(z_score = score^(1/4) ) %>% 
  slice_sample(n = 4999)

ggplot(tf2, aes(x=z_score)) +
  geom_histogram() # Eliminate outliers

tf2 %<>% 
  filter(z_score < 1.74 & z_score > 1.45)

shapiro.test(tf2$score) # There is an improvement 

#####
model_tf <- lm(z_score ~ budget + genre + company + runtime + year, data = tf)
model_tf2 <- lm(z_score ~ budget + genre + company + runtime + year, data = tf2)

# Plot the model
par(mfrow=c(2,2))
plot(model_tf)
plot(model_tf2)

# Independency

durbinWatsonTest(model_tf)
durbinWatsonTest(model_tf2)

summary(model_tf)
summary(model_tf2) # resid standard error is EXTREMELY low (partly due to different response unit)

##### You can predict usin tf2

# new_film <- data.frame(budget = 150000000, genre = "Animation", company = "Walt Disney Pictures", runtime = 108, year = 2016)

# predict(model_tf, newdata = new_film) * sd(tf$score, na.rm = T) + mean(tf$score, na.rm=T)
# DO NOT FORGET TO APPLY THE INVERSE TRANSFORMATION TO GET A MEANINGFUL OUTPUT

# cikan deger karekok karekok olcak yani ^4 alinca mantikli cikmasi lazim
