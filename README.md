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

## Clustering

Text analysis and clustering compare the vocabulary of RTO and WFH posts using the saved `data/posts_clean.rds`. Posts were filtered using English language tags to keep the analysis focused on English-language discussion. This language filter does not establish whether a post is relevant to workplace arrangements.

1. Open `GroupProject.Rproj` in RStudio.
2. Install the required packages once:

```r
install.packages(c("dplyr", "tidyr", "tidytext", "stringr", "ggplot2", "SnowballC", "knitr", "rmarkdown"))
```

3. Run the analysis from the project root:

```r
source("scripts/05_text_analysis.R")
source("scripts/07_clustering.R")
```

The text analysis produces word-frequency and post-percentage charts. Clustering uses TF-IDF, word stemming and k-means, with an elbow comparison and a PCA visualisation. Methods, cluster interpretation and limitations are included in `text_clustering_section.Rmd`.

Tables and figures are saved in `outputs/text/` and `outputs/clustering/`. No Bluesky login is required to analyse the saved data.

To render the text analysis and clustering report:

```r
rmarkdown::render("text_clustering_section.Rmd", output_format = "html_document")
```

For PDF output, use `output_format = "pdf_document"`. PDF rendering requires Pandoc and LaTeX.

## Combined report

Open `GroupProject.Rproj` in RStudio, then open `Report_draft.Rmd` and click **Knit**.

The report reads the saved analysis outputs. PDF rendering requires `knitr`, `rmarkdown`, Pandoc and a LaTeX installation.