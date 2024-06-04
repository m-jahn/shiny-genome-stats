#
# LOADING LIBRARIES
# ***********************************************
library(shiny)
library(httr)
library(jsonlite)
library(DT)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(shinythemes)
library(shinyTree)
library(configr)
library(stringr)


# LOADING EXTERNAL FUNCTIONS AND DATA
# ***********************************************
for (Rfile in list.files("R", full.names = TRUE)) {
  source(Rfile)
}

# load corresponding YAML configuration file
data_dir <- "data/"
data_config <- configr::read.config("config/config.yml")
list_genomes <- unlist(data_config$data$genomes)
