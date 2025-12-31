############################################################
# R-Based Analysis of Travel Reviews (TripAdvisor Dataset)
# Focus: EDA, Correlation, Hypothesis Testing, Clustering,
# Logistic Regression for Beach Lovers
############################################################

## 1. Setup ----
library(tidyverse)
library(corrplot)
library(cluster)
library(factoextra)
library(car)

set.seed(123)

## 2. Load Data ----
data = read.csv(
  "data/Final Project/tripadvisor_review.csv",
  stringsAsFactors = FALSE
)

# Rename columns
names(data) = c(
  "user_id", "art_galleries", "dance_clubs", "juice_bars",
  "restaurants", "museums", "resorts", "parks",
  "beaches", "theaters", "religious"
)

rating_cols = names(data)[-1]

# Descriptive Statistics ----
summary(data[rating_cols])

ratings_long = data %>%
  pivot_longer(cols = all_of(rating_cols),
               names_to = "category",
               values_to = "rating")

ggplot(ratings_long, aes(rating)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  facet_wrap(~category, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Ratings by Category")

# Correlation Analysis ----
cor_matrix = cor(data[rating_cols])

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Heatmap of Travel Categories",
  mar = c(0,0,2,0)
)

# Hypothesis Testing (Two-Sample t-tests) ----

# H1: Beaches vs Art Galleries
t.test(data$beaches, data$art_galleries, paired = TRUE)

# H2: Beaches vs Religious Institutions
t.test(data$beaches, data$religious, paired = TRUE)

# Normality Check (Q-Q Plots) ----
qq_plots = lapply(rating_cols, function(col) {
  ggplot(data, aes(sample = .data[[col]])) +
    stat_qq() +
    stat_qq_line(color = "red") +
    theme_minimal() +
    labs(title = paste("Q-Q Plot:", col))
})

gridExtra::grid.arrange(grobs = qq_plots, ncol = 3)

# Clustering Analysis (K-Means) ----
ratings_scaled = scale(data[rating_cols])

kmeans_model = kmeans(ratings_scaled, centers = 3, nstart = 25)
data$cluster = factor(kmeans_model$cluster)

cluster_profiles = data %>%
  group_by(cluster) %>%
  summarise(across(all_of(rating_cols), mean), .groups = "drop")

print(cluster_profiles)

# Logistic Regression: Beach Lovers

beach_logit_model = glm(
  Is_Beach_Lover ~ 
    art_galleries +
    dance_clubs +
    juice_bars +
    restaurants +
    museums +
    resorts +
    parks +
    theaters +
    religious,
  family = "binomial",
  data = dataset
)

# Model Summary
summary(beach_logit_model)



# Odds ratios for interpretation
odds_ratios = exp(cbind(
  OR = coef(beach_logit),
  confint(beach_logit)
))

print(round(odds_ratios, 3))



