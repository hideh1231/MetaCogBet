# Implicit Manifestation of Prospective Metacognition in Betting Choices Enhances its Efficiency Compared to Explicit Expression

This repository contains the code and data for the paper "[Implicit Manifestation of Prospective Metacognition in Betting Choices Enhances its Efficiency Compared to Explicit Expression](https://doi.org/10.3389/fnhum.2025.1490530)" by Hidekazu Nagamura, Hiroshi Ohnishi, Kohta I. Kobayasi, and Shoko Yuki.

## Reproducing the results

To reproduce the results in the paper, follow these steps:

1. Clone the Repository

``` shell
git clone git@github.com:hideh1231/MetaCogBet.git
cd MetaCogBet
```

2. Install Dependencies

The dependencies are managed using renv. To install all necessary packages, execute the following command in R:

``` r
renv::restore()
```

If this does not successfully install the dependencies, you can manually install them with:

``` r
renv::install(c("targets","jagstargets","tarchetypes","tidyverse","future","bayesplot","here","ggpubr","ggpp","ggrain","ggdist","see","patchwork","abind","brms","stats","rstatix","mc2d","parallel","crew","Hmisc","ggnewscale","RColorBrewer","dlookr","readxl","rjags"))
```

3. Run Analysis

Execute each code chunk in the `targets.Rmd` file. This will perform the analyses and generate the figures included in the paper.

### Data

Raw data used in the paper are available in the `data/` directory as CSV files.

The files located in the `csv/` directory are formatted specifically for statistical analyses using JASP. JAASP files are available in the `jasp/` directory.
