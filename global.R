#
# LOADING LIBRARIES
# ***********************************************
library(shiny)
library(httr)
library(jsonlite)
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
library(rentrez)
library(KEGGREST)


# LOADING EXTERNAL FUNCTIONS AND DATA
# ***********************************************
for (Rfile in list.files("R", full.names = TRUE)) {
  source(Rfile)
}

# load corresponding YAML configuration file
data_dir <- "data/"
config <- configr::read.config("config/config.yml")
list_genomes <- unlist(config$data$genomes)
