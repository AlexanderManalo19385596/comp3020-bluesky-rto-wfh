# RQ: Do English-labelled search results contain distinct vocabulary clusters?
# Run 05_text_analysis.R first. Module 6: TF-IDF, k-means and elbow analysis.
library(dplyr)
library(tidytext)
library(SnowballC)
library(ggplot2)
dir.create("outputs/clustering", recursive = TRUE, showWarnings = FALSE)
a <- readRDS("outputs/text/analysis_input.rds")
# Remove query-defining words and stem English words; keep terms in >=3 posts.
counts <- a$words |> filter(!word %in% a$topic_words) |>
  mutate(term = wordStem(word, language = "english")) |> count(uri, term, name = "n")
vocabulary <- counts |> count(term, name = "documents") |> filter(documents >= 3)
counts <- counts |> semi_join(vocabulary, by = "term")
M <- unclass(xtabs(n ~ uri + term, counts))
# TF = within-post term proportion; IDF = log(N / document frequency).
T <- sweep(M / rowSums(M), 2, log(nrow(M) / colSums(M > 0)), "*")
stopifnot(all(rowSums(T^2) > 0), all(is.finite(T)))
X <- T / sqrt(rowSums(T^2))
# Ordinary k-means uses squared Euclidean distance on unit-length TF-IDF rows.
# Pairwise squared Euclidean distance / 2 equals cosine dissimilarity here.
# This is NOT spherical k-means: centroids are not constrained to unit length.
fits <- lapply(1:10, function(k) {
  set.seed(3020 + k)
  kmeans(X, centers = k, nstart = 30, iter.max = 100)
})
elbow <- data.frame(k = 1:10, within_ss = sapply(fits, function(f) f$tot.withinss))
elbow$reduction <- c(NA, -diff(elbow$within_ss))
# No decisive elbow: k=5 separates repeated referral/news families mixed at k=3.
# This is an interpretable exploratory cut, not evidence of five natural themes.
chosen_k <- 5
candidate_summary <- bind_rows(lapply(2:10, function(k) {
  bind_rows(lapply(1:k, function(j) {
    idx <- which(fits[[k]]$cluster == j)
    groups <- a$posts$group[match(rownames(X)[idx], a$posts$uri)]
    terms <- names(sort(colMeans(T[idx, , drop = FALSE]), decreasing = TRUE))[1:8]
    data.frame(k = k, cluster = j, posts = length(idx), RTO = sum(groups == "RTO"),
               WFH = sum(groups == "WFH"), top_stems = paste(terms, collapse = ", "))
  }))
}))
fit <- fits[[chosen_k]]
# Relabel in decreasing size so C1 is always the largest cluster.
cluster <- match(fit$cluster, order(fit$size, decreasing = TRUE))
assignments <- a$posts[match(rownames(X), a$posts$uri), c("uri", "group", "text")]
assignments$cluster <- cluster
centres <- do.call(rbind, lapply(1:chosen_k, function(j) colMeans(X[cluster == j, , drop = FALSE])))
assignments$distance_to_centre <- rowSums((X - centres[cluster, ])^2)
top_terms <- bind_rows(lapply(1:chosen_k, function(j) {
  data.frame(cluster = j, term = colnames(T), weight = colMeans(T[cluster == j, , drop = FALSE])) |>
    arrange(desc(weight), term) |> slice_head(n = 10)
}))
examples <- assignments |> group_by(cluster) |>
  arrange(distance_to_centre, uri, .by_group = TRUE) |> slice_head(n = 5) |> ungroup()
cluster_summary <- assignments |> count(cluster, group) |> group_by(cluster) |>
  mutate(cluster_size = sum(n), percent = 100 * n / cluster_size) |> ungroup()
# Diagnose identical processed text within clusters; different URIs remain separate posts.
repetition <- a$words |> group_by(uri) |> summarise(signature = paste(word, collapse = " ")) |>
  inner_join(assignments |> select(uri, cluster), by = "uri") |> group_by(cluster) |>
  summarise(posts = n(), unique_token_sequences = n_distinct(signature), .groups = "drop")
# PCA is for display only. Clusters were fitted in all retained term dimensions.
pca <- prcomp(X, center = TRUE, scale. = FALSE, rank. = 2)
variance <- 100 * pca$sdev^2 / sum(pca$sdev^2)
coords <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], cluster = factor(cluster))
elbow_plot <- ggplot(elbow, aes(k, within_ss)) + geom_line() + geom_point() +
  geom_vline(xintercept = chosen_k, linetype = "dashed", colour = "#CC6677") +
  scale_x_continuous(breaks = 1:10) + theme_minimal(base_size = 12) +
  labs(title = "Cluster-count comparison", subtitle = "No decisive elbow; k = 5 used as an exploratory summary",
       x = "Number of clusters", y = "Within-cluster sum of squares")
cluster_plot <- ggplot(coords, aes(PC1, PC2, colour = cluster)) +
  geom_point(alpha = 0.55, size = 1.7) + theme_minimal(base_size = 12) +
  labs(title = "Vocabulary clusters in English-labelled search results",
       subtitle = "PCA display only; overlapping points may represent repeated text",
       x = sprintf("PC1 (%.1f%% of variance)", variance[1]),
       y = sprintf("PC2 (%.1f%% of variance)", variance[2]), colour = "Cluster")
term_plot <- ggplot(top_terms, aes(reorder_within(term, weight, cluster), weight)) +
  geom_col(fill = "#228899") + facet_wrap(~cluster, scales = "free_y") +
  scale_x_reordered() + coord_flip() + theme_minimal(base_size = 12) +
  labs(title = "Terms characterising each cluster", x = NULL, y = "Mean TF-IDF weight",
       subtitle = "Stemmed terms; mean weights calculated within each cluster")
for (name in c("elbow", "cluster", "term")) {
  figure <- get(paste0(name, "_plot"))
  print(figure)
  ggsave(paste0("outputs/clustering/", name, ".png"), figure, width = 11, height = 7, dpi = 300, bg = "white")
}
excluded <- a$posts |> filter(!uri %in% assignments$uri) |>
  select(uri, group, text) |> mutate(reason = "No terms after topic-word and document-frequency filters")
metrics <- data.frame(posts = nrow(X), terms = ncol(X), k = chosen_k,
  excluded_after_text = nrow(excluded), explained_between_percent = 100 * fit$betweenss / fit$totss,
  pc1_percent = variance[1], pc2_percent = variance[2])
# Save all result tables using the same output convention.
results <- list(assignments = assignments, top_terms = top_terms, examples = examples,
  cluster_summary = cluster_summary, elbow = elbow, repetition = repetition,
  candidate_summary = candidate_summary, excluded_posts = excluded, metrics = metrics)
for (name in names(results)) write.csv(results[[name]],
  paste0("outputs/clustering/", name, ".csv"), row.names = FALSE)
saveRDS(list(fit = fit, cluster = cluster, uris = rownames(X), terms = colnames(X)),
        "outputs/clustering/model.rds")
writeLines(capture.output(sessionInfo()), "outputs/clustering/session_info.txt")
print(metrics)
print(cluster_summary)
