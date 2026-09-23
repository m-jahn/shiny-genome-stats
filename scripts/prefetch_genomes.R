#!/usr/bin/env Rscript

# import required libraries
suppressPackageStartupMessages({
  library(jsonlite)
  library(tidyverse)
  library(Biostrings)
  library(rtracklayer)
  library(argparse)
})

# set args here, optionally parse from CLI at future implementation
parse_args <- function() {
  parser <- ArgumentParser()
  parser$add_argument("--input", default = file.path("data", "genomes.tsv"))
  parser$add_argument("--output_dir", default = "data")
  parser$add_argument("--accession_column", default = "assemblyId")
  parser$add_argument("--limit", type = "integer", default = 10)
  parser$add_argument("--datasets_bin", default = "datasets")
  parser$add_argument("--overwrite", action = "store_true")
  args <- parser$parse_args()
  message("Parsed arguments: ", paste(names(args), args, sep = "=", collapse = ", "))
  return(args)
}

# Keep intermediate tables stable across files by reading them with a fixed schema.
# Columns that may vary by source are read as character and normalized later.
read_tsv_consistent <- function(path) {
  readr::read_tsv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
}

normalize_column_types <- function(df, numeric_columns = character(), integer_columns = character(), date_columns = character()) {
  if (!is.null(df) && nrow(df) > 0) {
    if (length(numeric_columns)) {
      numeric_columns <- intersect(numeric_columns, names(df))
      df <- df %>% mutate(across(all_of(numeric_columns), ~ readr::parse_number(as.character(.x))))
    }
    if (length(integer_columns)) {
      integer_columns <- intersect(integer_columns, names(df))
      df <- df %>% mutate(across(all_of(integer_columns), ~ as.integer(readr::parse_number(as.character(.x)))))
    }
    if (length(date_columns)) {
      date_columns <- intersect(date_columns, names(df))
      df <- df %>% mutate(across(all_of(date_columns), ~ as.Date(.x)))
    }
  }
  df
}

# Summarise basic statistics of a FASTA file containing genome sequences
summarise_fasta <- function(fasta_file) {
  if (is.na(fasta_file) || !file.exists(fasta_file)) {
    warning("Could not locate a FASTA file in the downloaded genome package.")
    return(NULL)
  }
  # import genome seq and determine basic features
  DNAStringSet <- Biostrings::readDNAStringSet(fasta_file)
  df_fasta <- tibble(
    seq_count = length(DNAStringSet),
    seq_lengths = list(Biostrings::width(DNAStringSet)),
    seq_names = list(names(DNAStringSet)),
    total_length = sum(Biostrings::width(DNAStringSet), na.rm = TRUE),
    gc_content = letterFrequency(DNAStringSet, letters = c("G", "C"), as.prob = TRUE) %>%
      rowSums() %>% mean(na.rm = TRUE),
    gc_skew = ((
      letterFrequency(DNAStringSet, letters = c("G"), as.prob = TRUE) %>% rowSums() %>% mean(na.rm = TRUE) -
        letterFrequency(DNAStringSet, letters = c("C"), as.prob = TRUE) %>% rowSums() %>% mean(na.rm = TRUE)) /
      gc_content
    )
  )
  df_fasta
}

# Summarise basic statistics of a GFF3 file containing genome features
summarise_gff <- function(gff_file) {
  if (is.na(gff_file) || !file.exists(gff_file)) {
    warning("Could not locate a GFF3 file in the downloaded genome package.")
    return(NULL)
  }
  # import GFF3 file and summarise features by type
  features <- rtracklayer::import(gff_file, format = "gff3") %>%
    as_tibble() %>%
    group_by(type) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = type, values_from = c(count))

  if (!nrow(features)) {
    return(NULL)
  } else {
    return(features)
  }
}

# Fetch genome data from NCBI using the datasets command-line tool
fetch_ncbi_genome <- function(accession, temp_dir, datasets_bin = "datasets") {
  package_zip <- file.path(temp_dir, paste0(accession, ".zip"))
  args <- c(
    "download", "genome", "accession", accession,
    "--include", "genome,gff3",
    "--filename", package_zip
  )
  ncbi_out <- system2(datasets_bin, args, stdout = TRUE, stderr = TRUE)
  if (!str_detect(tail(ncbi_out, 1), "Validating package files")) {
    warning(
      paste(c(
        "NCBI datasets download failed with message:",
        ncbi_out
      ), collapse = "\n")
    )
    return(NULL)
  }

  # unzip the downloaded genome info
  unzip_dir <- file.path(temp_dir, accession)
  dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(package_zip, exdir = unzip_dir)

  # if fetching of data succeeded, we also fetch genome reports in JSON format
  args <- c("summary", "genome", "accession", accession, "--report", "genome", "--as-json-lines")
  system2(datasets_bin, args, stdout = TRUE, stderr = TRUE) %>%
    jsonlite::fromJSON(simplifyVector = TRUE, flatten = TRUE) %>%
    unlist() %>%
    enframe(name = "name", value = "value") %>%
    filter(!str_detect(name, "assembly_info.comments|assembly_info.biosample.attributes")) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    mutate(
      across(c(ends_with("_date")), ~ as.Date(.x, optional = TRUE)),
      across(c(ends_with("_version")), as.character)
    ) %>%
    write_tsv(file.path(temp_dir, accession, "assembly_report.tsv"))

  # returned the extracted files and assembly report
  data_dir <- file.path(unzip_dir, "ncbi_dataset", "data", accession)
  list(
    assembly_report = file.path(temp_dir, accession, "assembly_report.tsv"),
    gff_file = list.files(data_dir, pattern = "\\.gff3?$", full.names = TRUE)[1],
    fasta_file = list.files(data_dir, pattern = "\\.fna$", full.names = TRUE)[1]
  )
}

# Main function to orchestrate the genome fetching and summarisation process
main <- function() {
  # parse arguments
  opts <- parse_args()
  if (!dir.exists(opts$output_dir)) {
    dir.create(opts$output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # import list of representative bacterial genomes (source: fastgenomics server)
  df_genomes <- read_tsv(opts$input, show_col_types = FALSE)

  if (!is.na(opts$limit)) {
    df_genomes <- df_genomes %>% dplyr::slice(1:opts$limit)
  }

  if (!nrow(df_genomes)) {
    stop("No genomes to process after applying the current filters.")
  }

  # loop through each genome in the list using the assembly ID and
  # fetch genome seq and annotation from NCBI
  df_fasta <- tibble()
  df_gff <- tibble()
  df_assembly <- tibble()

  for (i in seq_len(nrow(df_genomes))) {
    row <- df_genomes[i, , drop = FALSE]
    accession <- row$assemblyId
    message("[", i, "/", nrow(df_genomes), "] fetching ", accession)

    # create a temp dir and download the genome data
    temp_dir <- file.path(opts$output_dir, "ncbi")
    if (!dir.exists(file.path(temp_dir, accession))) {
      ncbi_result <- fetch_ncbi_genome(accession, temp_dir, opts$datasets_bin)
    } else {
      ncbi_result <- list(
        fasta_file = list.files(file.path(temp_dir, accession, "ncbi_dataset", "data", accession), pattern = "\\.fna$", full.names = TRUE)[1],
        gff_file = list.files(file.path(temp_dir, accession, "ncbi_dataset", "data", accession), pattern = "\\.gff3?$", full.names = TRUE)[1],
        assembly_report = file.path(temp_dir, accession, "assembly_report.tsv")
      )
    }

    # import and summarise the downloaded fasta and gff3 files
    df_fasta <- bind_rows(df_fasta, summarise_fasta(ncbi_result$fasta_file) %>% mutate(accession = accession))
    df_gff <- bind_rows(df_gff, summarise_gff(ncbi_result$gff_file) %>% mutate(accession = accession))
    df_assembly <- bind_rows(df_assembly, read_tsv_consistent(ncbi_result$assembly_report))
  }

  # export the summary tables to the output directory
  write_tsv(df_assembly, file.path(opts$output_dir, "genome_summary.tsv"))
  write_tsv(df_fasta, file.path(opts$output_dir, "genome_sequences.tsv"))
  write_tsv(df_gff, file.path(opts$output_dir, "genome_features.tsv"))

  message("Wrote genome summary tables to: ", opts$output_dir)
  message("- FASTA summary: genome_sequences.tsv")
  message("- GFF3 summary: genome_features.tsv")
  message("- ASSEMBLY summary: genome_summary.tsv")
}

main()
