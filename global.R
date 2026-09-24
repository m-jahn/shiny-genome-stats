#
# LOADING LIBRARIES
# ***********************************************
library(shiny)
# library(httr)
library(DT)
library(shinyWidgets)
library(tibble)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(shinythemes)
library(shinyTree)
library(configr)
library(stringr)
library(colorspace)
library(forcats)

# LOADING EXTERNAL FUNCTIONS AND DATA
# ***********************************************
excludes <- c("scripts/prefetch_genomes.R")
for (Rfile in setdiff(list.files("scripts", full.names = TRUE), excludes)) {
  source(Rfile)
}

# import genome/proteome data
data_dir <- "data/"
df_summary <- read_tsv(file.path(data_dir, "genome_summary.tsv"), col_types = cols())
df_features <- read_tsv(file.path(data_dir, "genome_features.tsv"), col_types = cols())
df_sequences <- read_tsv(file.path(data_dir, "genome_sequences.tsv"), col_types = cols())

# parse seq_lengths list column correctly
df_sequences <- df_sequences %>%
  mutate(seq_lengths_top10 = str_remove_all(seq_lengths_top10, "c\\(|\\)") %>%
    str_split(", ") %>%
    sapply(as.numeric)) %>%
  pull(seq_lengths_top10)

# load corresponding YAML configuration file
config <- configr::read.config("config/config.yml")

# list of all available genomes
list_genomes <- df_summary %>%
  dplyr::select(organism.organism_name, accession) %>%
  deframe()

# list of preselected genomes (from config file)
list_selected <- df_summary %>%
  dplyr::filter(accession %in% unlist(config$data$accession)) %>%
  dplyr::select(organism.organism_name, accession) %>%
  deframe()
