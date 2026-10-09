# RQ: How does vocabulary differ between English-labelled RTO/WFH search results?
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
dir.create("outputs/text", recursive = TRUE, showWarnings = FALSE)

# Language metadata is supplied by Bluesky; missing/mixed tags are not English-only.
posts <- readRDS("data/posts_clean.rds")
language_codes <- lapply(posts$langs, function(x) tolower(unlist(x)))
audit <- posts |> select(uri, group, text)
audit$language <- vapply(language_codes, paste, collapse = ";", FUN.VALUE = "")
audit$english <- vapply(language_codes, function(x) {
  length(x) > 0 && all(grepl("^en(-[a-z]+)?$", x))
}, logical(1))
# Remove URLs (also bare domains/short links) and account mentions before tokenising.
url_pattern <- regex("https?://\\S+|www\\.\\S+|\\b[a-z0-9][a-z0-9.-]*\\.[a-z]{2,}(?:[/?#]\\S*)?", TRUE)
audit$clean_text <- audit$text |>
  str_remove_all(url_pattern) |>
  str_remove_all("@[[:alnum:]_.-]+") |>
  str_replace_all("[’‘]", "'")
words <- audit |> filter(english) |> select(uri, group, clean_text) |>
  unnest_tokens(word, clean_text) |>
  anti_join(stop_words, by = "word") |>
  filter(str_detect(word, "^[a-z]+$"), nchar(word) >= 2)
audit <- audit |> mutate(
  usable = english & uri %in% words$uri,
  decision = case_when(!english & language == "" ~ "Missing language tag",
    !english ~ "Non-English or mixed language tags",
    !usable ~ "No retained English word tokens", TRUE ~ "Included"))
text_posts <- audit |> filter(usable)
text_counts <- audit |> count(group, decision, name = "posts")
word_counts <- words |> count(group, word, sort = TRUE)
# These are search-result groups, not verified opinions or a relevance-screened corpus.
topic_words <- c("return", "office", "remote", "work", "home", "rto", "wfh", "remotework")
post_word_rates <- words |> filter(!word %in% topic_words) |>
  distinct(uri, group, word) |> count(group, word, name = "posts_with_word") |>
  left_join(text_posts |> count(group, name = "total_posts"), by = "group") |>
  mutate(percent = 100 * posts_with_word / total_posts)

# Both figures share a numeric scale across groups; top 15 terms are chosen per group.
plot_words <- function(data, metric, title, axis_label) {
  top <- data |> mutate(value = .data[[metric]]) |> group_by(group) |>
    arrange(desc(value), word, .by_group = TRUE) |> slice_head(n = 15) |> ungroup()
  ggplot(top, aes(reorder_within(word, value, group), value, fill = group)) +
    geom_col(show.legend = FALSE) + facet_wrap(~group, scales = "free_y") +
    scale_x_reordered() + coord_flip() + theme_minimal(base_size = 12) +
    scale_fill_manual(values = c(RTO = "#CC6677", WFH = "#228899")) +
    labs(title = title, subtitle = "English-labelled search results; relevance not fully screened",
         x = NULL, y = axis_label)
}
word_plot <- plot_words(word_counts, "n", "Frequent words in RTO and WFH search results", "Word occurrences")
rate_plot <- plot_words(post_word_rates, "percent", "Vocabulary beyond the topic terms", "Percentage of usable posts")
print(word_plot)
print(rate_plot)
ggsave("outputs/text/word_frequency.png", word_plot, width = 11, height = 7, dpi = 300, bg = "white")
ggsave("outputs/text/post_word_percentages.png", rate_plot, width = 11, height = 7, dpi = 300, bg = "white")
write.csv(audit, "outputs/text/inclusion_audit.csv", row.names = FALSE)
write.csv(text_counts, "outputs/text/sample_counts.csv", row.names = FALSE)
write.csv(word_counts, "outputs/text/word_counts.csv", row.names = FALSE)
write.csv(post_word_rates, "outputs/text/post_word_percentages.csv", row.names = FALSE)
saveRDS(list(posts = text_posts, words = words, topic_words = topic_words), "outputs/text/analysis_input.rds")
writeLines(capture.output(sessionInfo()), "outputs/text/session_info.txt")
print(text_counts)
