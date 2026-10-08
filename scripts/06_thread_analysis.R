library(atrrr)

# Use the completed collection
thread_run_dir <- "data/network_threads/20260930T062807Z"

thread_tables <- list()

for (i in 1:3) {
  saved <- readRDS(
    file.path(thread_run_dir, paste0("thread_", i, "_raw.rds"))
  )
  
  # Verify the saved response matches the requested conversation
  stopifnot(
    identical(saved$response$thread$post$uri, saved$request$uri)
  )
  
  # Turn the saved response into a table
  parsed <- atrrr:::parse_threads(saved$response)
  
  parsed$conversation <- i
  parsed$seed_uri <- saved$request$uri
  parsed$collected_at_utc <- saved$collected_at_utc
  parsed$selection_reason <- saved$selection_reason
  
  thread_tables[[i]] <- parsed
}

# Preserve conversation membership, then create a unique-post table
thread_memberships <- dplyr::bind_rows(thread_tables)

thread_posts <- thread_memberships[
  !duplicated(thread_memberships$uri),
]

# Include parent text to help interpret short replies
thread_posts$parent_text <- thread_posts$text[
  match(thread_posts$in_reply_to, thread_posts$uri)
]

print(nrow(thread_posts))

if (interactive()) {
  View(thread_posts[
    , c("conversation", "text", "parent_text")
  ])
}

library(igraph)

# Extract account IDs from post identifiers
thread_posts$author_id <- sub(
  "^at://([^/]+)/.*$", "\\1", thread_posts$uri
)

# Find each reply's parent within the collected posts
parent_row <- match(
  thread_posts$in_reply_to,
  thread_posts$uri
)

thread_reply_events <- data.frame(
  from = thread_posts$author_id,
  to = thread_posts$author_id[parent_row],
  source_post_uri = thread_posts$uri,
  parent_post_uri = thread_posts$in_reply_to
)

# Keep connections where both posts were retrieved
thread_reply_events <- thread_reply_events[
  !is.na(thread_reply_events$to),
]

# Remove self-replies and repeated account pairs from the graph
thread_edges <- unique(
  thread_reply_events[
    thread_reply_events$from != thread_reply_events$to,
    c("from", "to")
  ]
)

thread_graph <- graph_from_data_frame(
  thread_edges,
  directed = TRUE,
  vertices = data.frame(
    name = unique(thread_posts$author_id)
  )
)

print(c(
  accounts = vcount(thread_graph),
  connections = ecount(thread_graph)
))

# Identify connected groups, ignoring arrow direction
thread_components <- components(thread_graph, mode = "weak")

group_colours <- hcl.colors(
  thread_components$no,
  palette = "Dark 3"
)

# Create a reproducible layout
set.seed(3020)
thread_layout <- layout_with_fr(thread_graph)

plot(
  thread_graph,
  layout = thread_layout,
  vertex.label = NA,
  vertex.size = 5,
  vertex.color = group_colours[thread_components$membership],
  vertex.frame.color = NA,
  edge.color = "grey50",
  edge.arrow.size = 0.3,
  main = "Reply network across three selected conversations"
)

legend(
  "topleft",
  legend = paste("Connected group", seq_len(thread_components$no)),
  col = group_colours,
  pch = 19,
  bty = "n",
  cex = 0.7
)

print(sort(thread_components$csize, decreasing = TRUE))

thread_summary <- data.frame(
  posts = nrow(thread_posts),
  accounts = vcount(thread_graph),
  connections = ecount(thread_graph),
  density = round(edge_density(thread_graph, loops = FALSE), 4),
  isolated_accounts = sum(degree(thread_graph, mode = "all") == 0),
  connected_groups = thread_components$no,
  largest_group = max(thread_components$csize)
)

print(thread_summary)

thread_centrality <- data.frame(
  account = thread_posts$author_handle[
    match(V(thread_graph)$name, thread_posts$author_id)
  ],
  in_degree = degree(thread_graph, mode = "in"),
  betweenness = betweenness(
    thread_graph,
    directed = TRUE,
    normalized = FALSE
  ),
  row.names = NULL
)

# Show the three accounts receiving replies from the most accounts
reply_ranking <- thread_centrality[
  order(-thread_centrality$in_degree, thread_centrality$account),
]

print(head(reply_ranking, 3))

# Show accounts with positive betweenness
between_ranking <- thread_centrality[
  order(-thread_centrality$betweenness, thread_centrality$account),
]

print(between_ranking[between_ranking$betweenness > 0, ])
print(head(reply_ranking, 3))

largest_group <- which.max(thread_components$csize)

largest_graph <- induced_subgraph(
  thread_graph,
  vids = which(thread_components$membership == largest_group)
)

set.seed(3020)

plot(
  largest_graph,
  layout = layout_with_fr(largest_graph),
  vertex.label = NA,
  vertex.size = 4 + 2 * sqrt(degree(largest_graph, mode = "in")),
  vertex.color = "steelblue",
  vertex.frame.color = NA,
  edge.color = "grey50",
  edge.arrow.size = 0.4,
  main = "Largest conversation: isolation and wellbeing"
)

print(c(
  accounts = vcount(largest_graph),
  connections = ecount(largest_graph)
))

dir.create(
  "outputs/network",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  thread_summary,
  "outputs/network/network_summary.csv",
  row.names = FALSE
)

write.csv(
  reply_ranking,
  "outputs/network/account_centrality.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Save the full network
png(
  "outputs/network/full_reply_network.png",
  width = 8, height = 7, units = "in", res = 180
)

plot(
  thread_graph,
  layout = thread_layout,
  vertex.label = NA,
  vertex.size = 5,
  vertex.color = group_colours[thread_components$membership],
  vertex.frame.color = NA,
  edge.color = "grey50",
  edge.arrow.size = 0.3,
  main = "Reply network across three selected conversations"
)

legend(
  "topleft",
  legend = paste("Connected group", seq_len(thread_components$no)),
  col = group_colours,
  pch = 19,
  bty = "n",
  cex = 0.7
)

dev.off()

# Save the largest connected group
png(
  "outputs/network/largest_conversation.png",
  width = 8, height = 7, units = "in", res = 180
)

set.seed(3020)

plot(
  largest_graph,
  layout = layout_with_fr(largest_graph),
  vertex.label = NA,
  vertex.size = 4 + 2 * sqrt(degree(largest_graph, mode = "in")),
  vertex.color = "steelblue",
  vertex.frame.color = NA,
  edge.color = "grey50",
  edge.arrow.size = 0.4,
  main = "Largest conversation: isolation and wellbeing"
)

dev.off()

review_file <- "outputs/network/thread_relevance_review.csv"

if (file.exists(review_file)) {
  thread_review <- read.csv(
    review_file, as.is = TRUE, fileEncoding = "UTF-8"
  )
} else {
  thread_review <- thread_posts[, c(
    "conversation", "uri", "author_handle",
    "text", "parent_text", "selection_reason"
  )]
  thread_review$review <- ""
  thread_review$notes <- ""
  
  write.csv(
    thread_review, review_file,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
}

print(nrow(thread_review))
if (interactive()) {
  View(thread_review)
}
