# COMP3020 Social Web Analytics Project

Comparing Return-to-Office vs Work-from-Home discussion on Bluesky.

## Data collection

Run `scripts/01_data_collection.R` for keyword collection. This requires your own Bluesky app password. Data is saved in `data/`.

## Network analysis

1. Open `GroupProject.Rproj` in RStudio.
2. Run this in the console:

```r
source("scripts/06_thread_analysis.R")
```

3. Open `network_section.Rmd` and click **Knit**.

Required packages: `atrrr` (version 0.2.0), `dplyr`, `igraph`, `knitr` and `rmarkdown`. PDF knitting also needs Pandoc and LaTeX.

The analysis uses the three saved files in `data/network_threads/20260930T062807Z/`, so no Bluesky login is needed.

Expected results: **66 posts, 62 accounts and 60 connections**, with three connected groups of **32, 19 and 11 accounts**.

Tables and graphs are saved in `outputs/network/`. Keep `thread_relevance_review.csv` in that folder because it contains our content labels.