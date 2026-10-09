# 08_sentiment_analysis.R
#
# Research question: How do sentiment categories compare between the return-to-office
# and remote-work search groups? (Tested as: is sentiment associated with the search group?)
#
# H0: Search group (RTO/WFH) and sentiment (negative/neutral/positive) are independent.
# H1: Search group and sentiment are not independent (there is an association).
#
# Test: chi-squared test of independence (two categorical variables).
#
# Uses the same 727 English-labelled posts as 05_text_analysis.R and 07_clustering.R.
# Run 05_text_analysis.R first: it creates outputs/text/analysis_input.rds.
#
# Main analysis: the query words (return, office, remote, work, home, rto, wfh,
# remotework) and "trump" are removed from the sentiment dictionary before scoring.
# The Bing dictionary counts "work" and "trump" as positive. Many WFH posts contain
# "remote work", so leaving "work" in could make WFH posts look
# positive before anyone says anything.
#
# Robustness checks: (1) without removing those words, (2) all 1,137 cleaned posts
# (the original 03_hypothesis_testing.R analysis), (3) with job adverts removed.
#
# Outputs are saved to outputs/sentiment/. Object names start with sent_ so that
# sourcing this file inside the report does not overwrite other sections' objects.

library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

dir.create("outputs/sentiment", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Data: the English-labelled posts from the text analysis
# ---------------------------------------------------------------------------

sent_input <- readRDS("outputs/text/analysis_input.rds")

# clean_text has URLs and @mentions removed (done in 05_text_analysis.R)
sent_english <- sent_input$posts |>
  transmute(uri, group, text = clean_text)

if (nrow(sent_english) != 727) {
  warning("Expected 727 English-labelled posts but found ", nrow(sent_english),
          ". Check that 05_text_analysis.R is the final version.")
}

# Words removed from the dictionary in the main analysis
sent_drop_words <- c(sent_input$topic_words, "trump")

posts_clean <- readRDS("data/posts_clean.rds")

# Post dates (Bluesky's indexed time) so we can report time coverage by group
date_col <- intersect(c("indexed_at", "created_at"), names(posts_clean))[1]
if (is.na(date_col)) stop("No date column found in data/posts_clean.rds")

sent_dates <- data.frame(
  uri = posts_clean$uri,
  post_date = as.Date(substr(as.character(posts_clean[[date_col]]), 1, 10))
)

sent_date_range <- sent_english |>
  left_join(sent_dates, by = "uri") |>
  group_by(group) |>
  summarise(
    first_post = min(post_date, na.rm = TRUE),
    last_post  = max(post_date, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# Sentiment scoring
# ---------------------------------------------------------------------------
# Each word is matched to the Bing lexicon (positive or negative). A post is
# Positive if it has more positive than negative words, Negative if more negative
# than positive, and Neutral if the counts are equal. Posts with no lexicon word
# cannot be scored and are left out of the test.

sent_score <- function(df, drop_words = character(0)) {
  lexicon <- get_sentiments("bing") |> filter(!word %in% drop_words)
  df |>
    select(uri, group, text) |>
    unnest_tokens(word, text) |>
    inner_join(lexicon, by = "word") |>
    count(uri, group, sentiment) |>
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
    mutate(overall_sentiment = case_when(
      positive > negative ~ "Positive",
      negative > positive ~ "Negative",
      TRUE ~ "Neutral"
    ))
}

# Which of the removed words does the Bing dictionary actually score, and how often?
sent_drop_counts <- sent_english |>
  unnest_tokens(word, text) |>
  filter(word %in% sent_drop_words) |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(group, word, sentiment, name = "occurrences")

# ---------------------------------------------------------------------------
# Main analysis: 727 English-labelled posts, query words and "trump" removed
# ---------------------------------------------------------------------------

sent_scored <- sent_score(sent_english, sent_drop_words)

# Contingency table: search group (rows) by sentiment (columns)
sent_X <- table(sent_scored$group, sent_scored$overall_sentiment)

sent_test <- chisq.test(sent_X)

# Simulated p-value, as shown in the lecture slides
set.seed(3020)
sent_sim <- chisq.test(sent_X, simulate.p.value = TRUE, B = 10000)

# Effect size: Cramer's V (0 = no association, 1 = perfect association)
sent_V <- sqrt(unname(sent_test$statistic) / (sum(sent_X) * (min(dim(sent_X)) - 1)))

sent_pct_table <- round(100 * prop.table(sent_X, 1), 1)   # row percentages

sent_coverage <- sent_english |>
  count(group, name = "english_posts") |>
  left_join(count(sent_scored, group, name = "scored_posts"), by = "group") |>
  mutate(
    unscored = english_posts - scored_posts,
    percent_scored = round(100 * scored_posts / english_posts, 1)
  )

# Which lexicon words drive each sentiment class?
sent_top_words <- sent_english |>
  unnest_tokens(word, text) |>
  inner_join(get_sentiments("bing") |> filter(!word %in% sent_drop_words), by = "word") |>
  count(group, sentiment, word, sort = TRUE) |>
  group_by(group, sentiment) |>
  slice_head(n = 5) |>
  summarise(top_words = paste0(word, " (", n, ")", collapse = ", "), .groups = "drop")

# ---------------------------------------------------------------------------
# Robustness check 1: same posts, but without removing the query words / "trump"
# ---------------------------------------------------------------------------

sent_scored_raw <- sent_score(sent_english)
sent_X_raw <- table(sent_scored_raw$group, sent_scored_raw$overall_sentiment)
sent_test_raw <- chisq.test(sent_X_raw)
sent_pct_raw <- round(100 * prop.table(sent_X_raw, 1), 1)

# ---------------------------------------------------------------------------
# Robustness check 2: all 1,137 cleaned posts (original 03 analysis, no removal)
# ---------------------------------------------------------------------------

sent_scored_all <- sent_score(posts_clean)
sent_X_all <- table(sent_scored_all$group, sent_scored_all$overall_sentiment)
sent_test_all <- chisq.test(sent_X_all)
sent_pct_all <- round(100 * prop.table(sent_X_all, 1), 1)

# Should match the earlier result: RTO 203/54/171, WFH 36/73/412
if (!all(as.vector(sent_X_all) == c(203, 36, 54, 73, 171, 412))) {
  warning("All-posts table differs from the earlier 03 result (203/54/171 and 36/73/412).")
}

# ---------------------------------------------------------------------------
# Robustness check 3: remove job adverts (main scoring rule otherwise unchanged)
# ---------------------------------------------------------------------------
# Job adverts use positive words and are not opinions about work arrangements.
# This is a keyword rule, not a manual label, so it is only a check.

sent_job_pattern <- paste0(
  "\\b(hiring|apply now|apply here|apply today|vacanc(y|ies)|recruit(ing|er|ment)?|",
  "careers?|remotejobs|job (opening|openings|alert|alerts|posting|board|listing|listings)|salary)\\b"
)

sent_english$job_ad_like <- str_detect(str_to_lower(sent_english$text), sent_job_pattern)

# Note: the new column is called job_ads so the percentage uses the logical column
sent_job_share <- sent_english |>
  group_by(group) |>
  summarise(
    posts = n(),
    job_ads = sum(job_ad_like),
    percent = round(100 * mean(job_ad_like), 1),
    .groups = "drop"
  )

sent_scored_nojobs <- sent_score(filter(sent_english, !job_ad_like), sent_drop_words)
sent_X_nojobs <- table(sent_scored_nojobs$group, sent_scored_nojobs$overall_sentiment)
sent_test_nojobs <- chisq.test(sent_X_nojobs)
sent_pct_nojobs <- round(100 * prop.table(sent_X_nojobs, 1), 1)

# ---------------------------------------------------------------------------
# Figure and saved tables
# ---------------------------------------------------------------------------

sent_plot_df <- as.data.frame(sent_pct_table, stringsAsFactors = FALSE)
names(sent_plot_df) <- c("group", "sentiment", "percent")
sent_plot_df$sentiment <- factor(sent_plot_df$sentiment,
                                 levels = c("Negative", "Neutral", "Positive"))

sent_plot <- ggplot(sent_plot_df, aes(x = group, y = percent, fill = sentiment)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(percent, "%"), colour = sentiment),
    position = position_stack(vjust = 0.5), size = 3.5
  ) +
  scale_fill_manual(values = c(Negative = "#C2410C", Neutral = "#8A94A0", Positive = "#0A8F83")) +
  scale_colour_manual(values = c(Negative = "white", Neutral = "black", Positive = "white"),
                      guide = "none") +
  labs(
    title = "Sentiment of English-labelled search results",
    subtitle = "Share of scored posts; query words and \"trump\" not scored",
    x = NULL, y = "% of scored posts", fill = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/sentiment/sentiment_by_group.png", sent_plot,
       width = 6.5, height = 4.5, dpi = 300, bg = "white")

write.csv(as.data.frame.matrix(sent_X), "outputs/sentiment/contingency_table.csv")
write.csv(as.data.frame.matrix(round(sent_test$stdres, 2)), "outputs/sentiment/standardised_residuals.csv")
write.csv(sent_coverage, "outputs/sentiment/coverage.csv", row.names = FALSE)
write.csv(sent_job_share, "outputs/sentiment/job_ad_share.csv", row.names = FALSE)
write.csv(sent_top_words, "outputs/sentiment/top_lexicon_words.csv", row.names = FALSE)
write.csv(sent_drop_counts, "outputs/sentiment/removed_word_counts.csv", row.names = FALSE)
write.csv(as.data.frame.matrix(sent_X_raw), "outputs/sentiment/contingency_table_unadjusted.csv")
writeLines(capture.output(sessionInfo()), "outputs/sentiment/session_info.txt")

# ---------------------------------------------------------------------------
# Small helpers used by the report text
# ---------------------------------------------------------------------------

sent_pct <- function(x) paste0(format(round(x, 1), nsmall = 1), "%")

sent_p <- function(p) {
  if (p < 0.001) "p < .001" else paste0("p = ", format(round(p, 3), nsmall = 3))
}

# Print the key results when run on its own
print(sent_drop_counts)
print(sent_coverage)
print(sent_X)
print(sent_pct_table)
print(sent_test)
print(sent_sim)
print(round(sent_test$stdres, 2))
print(sent_V)
print(sent_top_words)
print(sent_X_raw)
print(sent_pct_raw)
print(sent_test_raw)
print(sent_X_all)
print(sent_test_all)
print(sent_job_share)
print(sent_X_nojobs)
print(sent_pct_nojobs)
print(sent_test_nojobs)