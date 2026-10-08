# Find posts with active reply conversations
posts <- readRDS("data/posts_clean.rds")

candidate_rows <- order(
  posts$reply_count,
  decreasing = TRUE,
  na.last = NA
)

network_candidates <- head(
  posts[
    candidate_rows,
    c("uri", "text", "reply_count", "group")
  ],
  20
)

View(network_candidates)

# Select specific posts using their permanent identifiers
seed_uris <- c(
  "at://did:plc:dzezcmpb3fhcpns4n4xm4ur5/app.bsky.feed.post/3mvibhqyix42y",
  "at://did:plc:6hit5rjdbmlnscmuk4bbtmuc/app.bsky.feed.post/3mwee54fx722l",
  "at://did:plc:ufjfrrqerpv5flbvmsdkthyg/app.bsky.feed.post/3mwbdvngx7223"
)

# Check that all selected posts exist in the saved data
stopifnot(all(seed_uris %in% posts$uri))

seed_posts <- posts[match(seed_uris, posts$uri), ]

seed_posts$selection_reason <- c(
  "Workplace isolation and mental health",
  "Return-to-office and housing costs",
  "Remote work and job competition"
)

View(seed_posts[
  , c("text", "reply_count", "selection_reason")
])

library(atrrr)

# Request the selected post and two levels of replies
thread_request <- list(
  uri = as.character(seed_uris[1]),
  depth = 2L,
  parentHeight = 0L,
  .return = "json"
)

collected_at <- format(
  Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
)

first_thread <- do.call(
  atrrr:::app_bsky_feed_get_post_thread,
  thread_request
)

# Preserve this collection in its own dated folder
thread_run_dir <- file.path(
  "data", "network_threads",
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
)

dir.create(thread_run_dir, recursive = TRUE)

saveRDS(
  list(
    request = thread_request,
    collected_at_utc = collected_at,
    selection_reason = seed_posts$selection_reason[1],
    response = first_thread
  ),
  file.path(thread_run_dir, "thread_1_raw.rds")
)

# Confirm that the response belongs to the requested post
stopifnot(
  identical(first_thread$thread$post$uri, seed_uris[1])
)

# Count the first level of returned reply entries
print(length(first_thread$thread$replies))

# Collect the other two selected conversations
for (i in 2:3) {
  
  next_request <- thread_request
  next_request$uri <- as.character(seed_uris[i])
  
  collected_at <- format(
    Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  )
  
  thread_response <- do.call(
    atrrr:::app_bsky_feed_get_post_thread,
    next_request
  )
  
  saveRDS(
    list(
      request = next_request,
      collected_at_utc = collected_at,
      selection_reason = seed_posts$selection_reason[i],
      response = thread_response
    ),
    file.path(thread_run_dir, paste0("thread_", i, "_raw.rds"))
  )
  
  stopifnot(
    identical(thread_response$thread$post$uri, seed_uris[i])
  )
  
  message(
    "Conversation ", i, ": ",
    length(thread_response$thread$replies),
    " direct reply entries"
  )
}