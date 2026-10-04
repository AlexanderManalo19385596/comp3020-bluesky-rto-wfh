# What words and themes characterise RTO versus WFH discussions on Bluesky?

posts <- readRDS("data/posts_clean.rds")

# Load analysis packages
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)

# Recreate the same sample in the same order
set.seed(3020)

sample_posts <- posts |>
  group_by(group) |>
  slice_sample(n = 10) |>
  ungroup() |>
  select(uri, group, text)

# Save the sample and our relevance review
sample_review <- sample_posts |> mutate(review = "Keep: workplace")
sample_review$review[c(1, 3, 6, 10)] <- "Exclude: political office"
sample_review$review[7] <- "Unclear: link only"
sample_review$review[8] <- "Workplace: Swedish text"
sample_review$review[13] <- "Link only: no prose to analyse"

dir.create("outputs", showWarnings = FALSE)

write.csv(sample_review, "outputs/sample_relevance_review.csv", row.names = FALSE)

# Prepare all posts for relevance and language review
full_review <- posts |>
  select(uri, group, text) |> left_join(sample_review |> select(uri, review), by = "uri") |>
  mutate(review = coalesce(review, "Not reviewed"), language = "", notes = "")

# Avoid overwriting any manual review work
review_path <- "outputs/full_relevance_review.csv"

if (!file.exists(review_path)) {
  write.csv(full_review, review_path, row.names = FALSE)
}

# Preliminary analysis: relevance and language review is incomplete
words <- posts |>
  select(uri, group, text) |>
  mutate(text = str_remove_all(text, regex("https?://\\S+|www\\.\\S+", ignore_case = TRUE))) |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word") |>
  filter(str_detect(word, "^\\p{L}+$"))

# Count word occurrences within each group
word_counts <- words |>
  count(group, word, sort = TRUE)

top_words <- word_counts |>
  group_by(group) |>
  slice_max(n, n = 15, with_ties = FALSE) |>
  ungroup()

# Draw the chart
word_plot <- ggplot(top_words, aes(x = reorder_within(word, n, group), y = n, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~group, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Most frequent words in collected RTO and WFH posts",
    subtitle = "Preliminary: relevance and language review incomplete",
    x = NULL,
    y = "Word occurrences"
  ) +
  theme_minimal()

print(word_plot)

ggsave("outputs/preliminary_word_frequency.png", word_plot, width = 10, height = 6, dpi = 300)

# Exclude topic-defining words for this second chart only
topic_words <- c("return", "office", "remote", "work", "home", "rto", "wfh", "remotework")

# Count each word once per post
post_word_rates <- words |>
  filter(!word %in% topic_words) |>
  distinct(uri, group, word) |>
  count(group, word, name = "posts_with_word") |>
  left_join(posts |> count(group, name = "total_posts"), by = "group") |>
  mutate(percent = 100 * posts_with_word / total_posts)

top_rates <- post_word_rates |>
  group_by(group) |>
  slice_max(percent, n = 15, with_ties = FALSE) |>
  ungroup()

rate_plot <- ggplot(
  top_rates,
  aes(x = reorder_within(word, percent, group), y = percent, fill = group)
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~group, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Common words beyond the search terms",
    subtitle = "Preliminary: relevance and language review incomplete",
    x = NULL,
    y = "Percentage of posts mentioning the word"
  ) +
  theme_minimal()

print(rate_plot)

ggsave(
  "outputs/preliminary_post_word_percentages.png",
  rate_plot,
  width = 12,
  height = 7,
  dpi = 300
)

# Save the underlying results for the report
write.csv(word_counts, "outputs/preliminary_word_counts.csv", row.names = FALSE)

write.csv(post_word_rates, "outputs/preliminary_post_word_percentages.csv", row.names = FALSE)

