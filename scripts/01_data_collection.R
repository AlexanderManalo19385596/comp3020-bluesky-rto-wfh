library(atrrr)

# Run this once per session if not already authenticated:
# auth("your-handle.bsky.social")

posts_rto <- search_post('"return to office"', limit = 500, sort = "latest")
posts_wfh <- search_post('"remote work"', limit = 500, sort = "latest")

nrow(posts_rto)
nrow(posts_wfh)

saveRDS(posts_rto, "data/posts_rto_raw.rds")
saveRDS(posts_wfh, "data/posts_wfh_raw.rds")