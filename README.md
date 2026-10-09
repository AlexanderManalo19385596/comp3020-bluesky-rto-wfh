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

## Text analysis and clustering

These sections use the saved `data/posts_clean.rds`; no Bluesky login or new collection is required.

1. Open `GroupProject.Rproj` in RStudio.
2. Install required packages once:

```r
install.packages(c("dplyr", "tidyr", "tidytext", "stringr", "ggplot2", "SnowballC", "knitr", "rmarkdown"))
```

3. Run in order from the project root:

```r
source("scripts/05_text_analysis.R")
source("scripts/07_clustering.R")
rmarkdown::render("text_clustering_section.Rmd", output_format = "html_document")
```

The standalone HTML explains the methods, results and limitations, and includes the analysis code. `Report_draft.Rmd` includes the same sections through `report_sections/`. Run both scripts before knitting either report. The combined report reads the saved network tables and figures; regenerate those separately with `06_thread_analysis.R` when needed. PDF knitting requires LaTeX. The previously committed `Report_draft.pdf` predates these additions and is not an updated final report.

**Scope:** English-only is implemented using Bluesky language tags, including regional variants. Missing, mixed and non-English tags are excluded; language is not independently verified. Of 1,137 saved posts, 751 pass the tag rule and 727 have usable tokens (371 RTO, 356 WFH). There is no full topic-relevance screening; results describe keyword-search groups, not a verified workplace-only corpus.

The clustering stage uses 700 posts and 811 stems after additional vocabulary filtering. K-means on unit-length TF-IDF vectors produces an exploratory five-cluster summary (642, 21, 16, 11 and 10 posts). There is no decisive elbow and separation is weak; smaller clusters largely reflect repeated content. The report explains the selection, examples and PCA limitations.

Final outputs are in `outputs/text/` and `outputs/clustering/`, including per-post exclusions, tables, PNG figures and session information. The old `outputs/preliminary_*` files and relevance-review templates are historical drafts, not the inputs for the updated charts. The scripts preserve these review files and do not claim the review is complete.

**Still required for the group submission:** complete the hypothesis-test section and overall discussion/conclusion, check consistency of samples across analyses, finish references and poster, and render/check the final PDF. The existing hypothesis-testing script still reads all 1,137 posts; it has not automatically adopted the text analysis's English-only subset.
