#!/usr/bin/env Rscript

# Calculate CDS length distributions and codon usage statistics per genome accession.

suppressPackageStartupMessages({
  library(tidyverse)
  library(Biostrings)
  library(rtracklayer)
  library(argparse)
})

parse_args <- function() {
  parser <- ArgumentParser()
  parser$add_argument("--input", default = file.path("data", "ncbi"))
  parser$add_argument("--output_dir", default = "data")
  parser$add_argument("--bins", type = "integer", default = 25)
  parser$add_argument("--max_codons", type = "integer", default = 1000)
  parser$add_argument("--limit", type = "integer", default = 0)
  parser$add_argument("--accession_pattern", default = "^(GCA|GCF)_[0-9]+\\.[0-9]+$")
  args <- parser$parse_args()
  message("Parsed arguments: ", paste(names(args), args, sep = "=", collapse = ", "))
  return(args)
}

read_organism_lookup <- function(output_dir) {
  candidates <- c(
    file.path(output_dir, "genome_features.tsv"),
    file.path(output_dir, "genome_sequences.tsv"),
    file.path(output_dir, "genome_summary.tsv")
  )

  for (table_path in candidates) {
    if (!file.exists(table_path)) {
      next
    }
    df <- readr::read_tsv(table_path, show_col_types = FALSE)
    if (all(c("accession", "organism") %in% names(df))) {
      return(df %>% dplyr::select(accession, organism) %>% distinct())
    }
  }
  stop("No suitable organism lookup table found in output_dir: ", output_dir)
}

list_accessions <- function(input, accession_pattern) {
  list.files(input, full.names = FALSE) %>%
    stringr::str_subset(accession_pattern) %>%
    sort()
}

build_file_index <- function(input, accession) {
  accession_dir <- file.path(input, accession, "ncbi_dataset", "data", accession)
  gff_file <- file.path(accession_dir, "genomic.gff")
  fna_file <- list.files(accession_dir, pattern = "_genomic\\.fna(\\.gz)?$", full.names = TRUE)
  fna_file <- if (length(fna_file) > 0) fna_file[[1]] else NA_character_
  list(gff = gff_file, fna = fna_file)
}

extract_cds_ranges <- function(gff_file) {
  gff <- rtracklayer::import(gff_file)
  type_col <- as.character(S4Vectors::mcols(gff)[["type"]])
  cds <- gff[!is.na(type_col) & type_col == "CDS"]
  cds
}

extract_cds_sequences <- function(dna, cds) {
  seq_lens <- setNames(as.integer(Biostrings::width(dna)), names(dna))
  seqname_chr <- as.character(GenomicRanges::seqnames(cds))
  cds_start <- as.integer(IRanges::start(cds))
  cds_end <- as.integer(IRanges::end(cds))

  valid <- seqname_chr %in% names(seq_lens) &
    !is.na(cds_start) & !is.na(cds_end) &
    cds_start >= 1 & cds_end >= cds_start &
    cds_end <= seq_lens[seqname_chr]

  cds_valid <- cds[valid]
  if (!length(cds_valid)) {
    return(list(seqs = DNAStringSet(), cds = cds_valid, dropped = sum(!valid)))
  }

  seqname_chr <- as.character(GenomicRanges::seqnames(cds_valid))
  cds_start <- as.integer(IRanges::start(cds_valid))
  cds_end <- as.integer(IRanges::end(cds_valid))

  seq_list <- mapply(
    FUN = function(chr, start_pos, end_pos) {
      Biostrings::subseq(dna[[chr]], start = start_pos, end = end_pos)
    },
    chr = seqname_chr,
    start_pos = cds_start,
    end_pos = cds_end,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  cds_seqs <- Biostrings::DNAStringSet(seq_list)
  minus <- as.vector(strand(cds_valid)) == "-"
  if (any(minus)) {
    cds_seqs[minus] <- Biostrings::reverseComplement(cds_seqs[minus])
  }

  list(seqs = cds_seqs, cds = cds_valid, dropped = sum(!valid))
}

codons_from_seq <- function(seq_char) {
  seq_char <- toupper(seq_char)
  usable_nt <- nchar(seq_char) - (nchar(seq_char) %% 3)
  if (usable_nt < 3) {
    return(list(start = NA_character_, stop = NA_character_, internal = character(0)))
  }
  codons <- substring(seq_char, seq(1, usable_nt, by = 3), seq(3, usable_nt, by = 3))
  internal <- if (length(codons) > 2) codons[2:(length(codons) - 1)] else character(0)
  list(start = codons[[1]], stop = codons[[length(codons)]], internal = internal)
}

summarize_width_distribution <- function(codon_lengths, bins, max_codons) {
  breaks <- seq(0, max_codons, length.out = bins + 1)
  main_bins <- cut(codon_lengths,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE
  )
  overflow_label <- paste0(">", max_codons)
  bin_label <- ifelse(is.na(main_bins), overflow_label, as.character(main_bins))

  base_bins <- tibble(
    bin = c(levels(main_bins), overflow_label)
  )

  tibble(bin = bin_label) %>%
    count(bin, name = "count") %>%
    right_join(base_bins, by = "bin") %>%
    mutate(count = replace_na(count, 0L)) %>%
    mutate(
      proportion = {
        total <- sum(count)
        if (total > 0) count / total else rep(0, n())
      },
      bin = factor(bin, levels = base_bins$bin)
    ) %>%
    arrange(bin) %>%
    mutate(bin = as.character(bin))
}

summarize_codon_bias <- function(internal_codons) {
  genetic_code <- Biostrings::GENETIC_CODE
  sense_codons <- names(genetic_code)[genetic_code != "*"]
  aa_map <- genetic_code[sense_codons]

  codon_counts <- tibble(codon = internal_codons) %>%
    filter(!is.na(codon), codon %in% sense_codons) %>%
    count(codon, name = "count")

  tibble(codon = sense_codons, aa = unname(aa_map)) %>%
    left_join(codon_counts, by = "codon") %>%
    mutate(count = replace_na(count, 0L)) %>%
    group_by(aa) %>%
    mutate(
      aa_total = sum(count),
      n_synonymous = n(),
      expected = if_else(aa_total > 0, aa_total / n_synonymous, NA_real_),
      rscu = if_else(!is.na(expected) & expected > 0, count / expected, NA_real_),
      synonymous_fraction = if_else(aa_total > 0, count / aa_total, NA_real_),
      preferred = if_else(aa_total > 0 & !is.na(rscu), rscu == max(rscu, na.rm = TRUE), FALSE)
    ) %>%
    ungroup()
}

main <- function() {
  args <- parse_args()
  dir.create(args$output_dir, showWarnings = FALSE, recursive = TRUE)

  accessions <- list_accessions(args$input, args$accession_pattern)
  if (!length(accessions)) {
    stop("No accession directories found under: ", args$input)
  }

  if (args$limit > 0) {
    accessions <- head(accessions, args$limit)
  }

  organism_lookup <- read_organism_lookup(args$output_dir)

  width_out <- vector("list", length(accessions))
  start_out <- vector("list", length(accessions))
  stop_out <- vector("list", length(accessions))
  bias_out <- vector("list", length(accessions))

  for (i in seq_along(accessions)) {
    accession <- accessions[[i]]
    files <- build_file_index(args$input, accession)

    if (!file.exists(files$gff) || is.na(files$fna) || !file.exists(files$fna)) {
      message("[", i, "/", length(accessions), "] skipped ", accession, " (missing gff/fna)")
      next
    }

    message("[", i, "/", length(accessions), "] processing ", accession)

    cds <- tryCatch(extract_cds_ranges(files$gff), error = function(e) NULL)
    if (is.null(cds) || length(cds) == 0) {
      message("  no CDS features found")
      next
    }

    dna <- Biostrings::readDNAStringSet(files$fna)
    names(dna) <- stringr::word(names(dna), 1)
    extraction <- tryCatch(extract_cds_sequences(dna, cds), error = function(e) NULL)
    if (is.null(extraction) || length(extraction$seqs) == 0) {
      message("  unable to extract CDS sequences")
      next
    }
    cds_seqs <- extraction$seqs
    cds <- extraction$cds
    if (!is.null(extraction$dropped) && extraction$dropped > 0) {
      message("  dropped ", extraction$dropped, " CDS ranges with invalid coordinates")
    }

    organism <- organism_lookup %>%
      filter(accession == .env$accession) %>%
      pull(organism)
    organism <- if (length(organism) > 0) organism[[1]] else accession

    codon_lengths <- floor(Biostrings::width(cds_seqs) / 3)
    width_stats <- summarize_width_distribution(codon_lengths, bins = args$bins, max_codons = args$max_codons) %>%
      mutate(accession = accession, organism = organism, .before = 1)

    codon_lists <- lapply(as.character(cds_seqs), codons_from_seq)
    start_codons <- vapply(codon_lists, `[[`, character(1), "start")
    stop_codons <- vapply(codon_lists, `[[`, character(1), "stop")
    internal_codons <- unlist(lapply(codon_lists, `[[`, "internal"), use.names = FALSE)

    start_stats <- tibble(codon = start_codons) %>%
      filter(!is.na(codon)) %>%
      count(codon, name = "count") %>%
      mutate(proportion = count / sum(count)) %>%
      arrange(desc(count)) %>%
      mutate(accession = accession, organism = organism, .before = 1)

    stop_stats <- tibble(codon = stop_codons) %>%
      filter(!is.na(codon)) %>%
      mutate(codon = if_else(codon %in% c("TAA", "TAG", "TGA"), codon, "OTHER")) %>%
      count(codon, name = "count") %>%
      mutate(proportion = count / sum(count)) %>%
      arrange(desc(count)) %>%
      mutate(accession = accession, organism = organism, .before = 1)

    bias_stats <- summarize_codon_bias(internal_codons) %>%
      arrange(aa, desc(rscu), codon) %>%
      mutate(accession = accession, organism = organism, .before = 1)

    width_out[[i]] <- width_stats
    start_out[[i]] <- start_stats
    stop_out[[i]] <- stop_stats
    bias_out[[i]] <- bias_stats
  }

  width_tbl <- bind_rows(width_out)
  start_tbl <- bind_rows(start_out)
  stop_tbl <- bind_rows(stop_out)
  bias_tbl <- bind_rows(bias_out)

  readr::write_tsv(width_tbl, file.path(args$output_dir, "gene_wise_cds_width_distribution.tsv"))
  readr::write_tsv(start_tbl, file.path(args$output_dir, "gene_wise_start_codon_frequency.tsv"))
  readr::write_tsv(stop_tbl, file.path(args$output_dir, "gene_wise_stop_codon_frequency.tsv"))
  readr::write_tsv(bias_tbl, file.path(args$output_dir, "gene_wise_codon_bias.tsv"))

  message("Wrote outputs to: ", args$output_dir)
}

main()