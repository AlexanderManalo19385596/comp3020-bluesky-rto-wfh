# 03_hypothesis_testing.R
#
# Research Question:
# Is there an association between which topic a post discusses (RTO vs WFH)
# and the sentiment (positive/negative/neutral) expressed in that post?
#
# H0: Topic (RTO/WFH) and sentiment are independent - knowing which topic
#     a post is about tells you nothing about its sentiment.
# H1: Topic and sentiment are NOT independent - there is an association
#     between them.
#
# Test: Chi-squared test of independence (categorical vs categorical),
# as covered in COMP3020's hypothesis testing content.

library(dplyr)
library(tidytext)
library(tidyr)

posts_clean <- readRDS("data/posts_clean.rds")

# Break posts into words, then score each word as positive/negative
sentiment_scores <- posts_clean |>
  mutate(post_id = row_number()) |>
  select(post_id, group, text) |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(post_id, group, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(overall_sentiment = case_when(
    positive > negative ~ "Positive",
    negative > positive ~ "Negative",
    TRUE ~ "Neutral"
  ))

# Build the contingency table: group (RTO/WFH) vs sentiment
X <- table(sentiment_scores$group, sentiment_scores$overall_sentiment)
X

# Check expected counts 
chisq.test(X)$expected

# Run the chi-squared test
result <- chisq.test(X)
result

# --- Decision ---
# Compare result$p.value to 0.05 (or your chosen alpha) and write the
# conclusion 
# p < 0.05 -> reject H0, evidence of association between topic and sentiment
# p >= 0.05 -> fail to reject H0, no evidence of association