# PNGRemittanceNetworks
Data and code repository for "Remittance Networks Shape Intensification of Small-Scale Natural Resource Use"
<br>
(\*title subject to change)

## Repository structure

- **final_df.csv**: Final dataset including household characteristics, network metrics, and fishing commercialization indices.

- **Combine_2018_2026_ Data.R**:Code to clean and combine household data across years

- **SummarizeNetworkData.R**: Code to construct social networks (financial and fish flows) and compute centrality measures (indegree, outdegree, betweenness, and external ties).

- **FishingCommercializationIndex.R**: Code to compute fishing commercialization indices, including market orientation, capital access, gear sophistication, and pelagic fishing focus.

- **DAGS.R**:  Code for making DAG figures. Also includes 'daggity' style code for use on https://www.dagitty.net/

- **DescriptiveStatsOutcomes.R**: Code to generates descriptive statistics and visualizations to characterize changes in household outcomes, fishing commercialization, network structure, and livelihood strategies between 2018 and 2026.

- **StatisticalAnalysis.R**: Code to estimate the relationship between remittance network structure and fishing commercialization outcomes using Bayesian panel models.

## Prerequisites
- R ≥ 4.6

- Packages required:
Run the following code in R (RStudio or the R console):

```{r}
packages <- c("tidyverse", "readxl", "igraph", "ggraph", "patchwork", "GGally", "dplyr", "ggplot2", "brms", "purrr", tidyr", ""tibble", "tidybayes", "ggdist", "ggeffects", "patchwork")
install.packages(setdiff(packages, rownames(installed.packages())))Show more lines
```

## Reproducibility

The dataset provided in this repository is the final cleaned and processed dataset used for analysis. Raw data and intermediate processing files are not included for confidentiality reasons. 
 
The Combine_2018_2026_ Data.R, FishingCommercializationIndex.R and SummarizeNetworkData.R scripts are provided for transparency and documentation purposes only and are not expected to be fully reproducible using the files included in this repository.
 
The statistical analysis scripts (StatisticalAnalysis.R and DescriptiveStatsOutcomes.R) will run with the provided dataset.


# Citation

If you use this code or dataset in your work, please cite:

Matt Clark, Michele L. Barnes, Amelia C. Meier, John Ben, Wilda Hungito, Bing Lin, Natalia Rivas Escobar, Jaida Damstra, Joshua E. Cinner. (2026).
Remittance Networks, Fishing Commercialization, and Household Panel Analysis.
GitHub repository.
