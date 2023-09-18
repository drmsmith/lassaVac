# Generates indexed input files for Lassa fever R code
# Same value used for each year within a simulation.

rm(list=ls())

library(readr)
library(readxl)
library(tibble)
library(dplyr)

n_years <- 10
n_sims <- 99

# Load original spillover data and extract details
df <- read_csv("LASV/spillover_data_v2.csv", show_col_types = FALSE)
details <- df[,1:6]

# Load new centile data and append details
centiles <- read_excel("LASV/incidence_ADM_1_SampleDF_Centiles.xlsx")
centiles <- centiles %>%
  left_join(details, by = c("ID" = "rID"))

# Generate files
for (i in 0:(n_sims-1)) {
  job <- i
  df_job <<- details # catchment details
  centiles2 <- centiles %>%
    filter(Centile == (job+1))
  for (y in (1:n_years)) {
    df_job2 <<- centiles2$Sampled_Value # spillover estimate (same value used for each year)
    df_job <<- bind_cols(df_job,df_job2)
  }
  
  filename <- paste0("LASV/input", i, ".csv")
  write.csv(df_job, file=filename, row.names=FALSE)
}

