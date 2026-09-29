library(dplyr)
library(stringr)

# Load raw data collected in 01_data_collection.R
posts_rto <- readRDS("data/posts_rto_raw.rds") |> mutate(group = "RTO")
posts_wfh <- readRDS("data/posts_wfh_raw.rds") |> mutate(group = "WFH")

posts <- bind_rows(posts_rto, posts_wfh)

# --- Remove unsuitable records ---
posts_clean <- posts |>
  filter(!is.na(text)) |>              # remove missing text
  filter(str_length(text) > 10) |>     # remove near-empty posts (e.g. "no!!!!")
  distinct(uri, .keep_all = TRUE)      # remove accidental duplicates

# Document how much was removed 
nrow(posts)        # before cleaning
nrow(posts_clean)  # after cleaning

# Save the cleaned combined dataset for everyone to use downstream
saveRDS(posts_clean, "data/posts_clean.rds")