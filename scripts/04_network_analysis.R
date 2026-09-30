posts <- readRDS("data/posts_clean.rds")

replies <- posts[
  !is.na(posts$in_reply_to) &
    nzchar(posts$in_reply_to),
]

nrow(replies)

# Extract each account's ID from its post link
get_account <- function(uri) {
  sub("^at://([^/]+)/.*$", "\\1", uri)
}

# Each connection goes from the replying account to the parent account
reply_events <- data.frame(
  from = get_account(replies$uri),
  to = get_account(replies$in_reply_to)
)

# Remove replies an account made to itself
reply_events <- reply_events[
  reply_events$from != reply_events$to,
]

# Keep one connection per directed pair of accounts
edges <- unique(reply_events)

nrow(edges)

library(igraph)

# Include authors from the collected posts and accounts they replied to
nodes <- data.frame(
  name = unique(c(get_account(posts$uri), edges$to))
)

# Build the reply network
reply_graph <- graph_from_data_frame(
  d = edges,
  directed = TRUE,
  vertices = nodes
)

# Check the network's size
vcount(reply_graph)
ecount(reply_graph)

# Identify accounts with at least one retained reply connection
has_connection <- degree(reply_graph, mode = "all") > 0

# Keep the layout reproducible
set.seed(3020)

plot(
  reply_graph,
  layout = layout_with_fr(reply_graph),
  vertex.label = NA,
  vertex.size = ifelse(has_connection, 3, 1.5),
  vertex.color = ifelse(has_connection, "steelblue", "grey80"),
  vertex.frame.color = NA,
  edge.color = "grey50",
  edge.arrow.size = 0.25,
  main = "Preliminary Bluesky reply network"
)

legend(
  "topleft",
  legend = c("Has a reply connection", "No reply connection observed"),
  col = c("steelblue", "grey80"),
  pch = 19,
  bty = "n",
  cex = 0.7
)

# Find connected groups, ignoring arrow direction for this check
network_components <- components(reply_graph, mode = "weak")

network_summary <- data.frame(
  accounts = vcount(reply_graph),
  connections = ecount(reply_graph),
  isolated_accounts = sum(degree(reply_graph) == 0),
  connected_groups = network_components$no,
  largest_group = max(network_components$csize)
)

print(network_summary)