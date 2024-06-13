get_ncbi_genome <- function(taxid) {
  # get genome(s) from tax id
  query_tax <- entrez_link(
    dbfrom = "taxonomy",
    id = taxid,
    db = "genome"
  )

  # try higher tax level if search fails
  if (!length(query_tax$links)) {
    query_tax2 <- entrez_link(
      dbfrom = "taxonomy",
      id = taxid,
      db = "taxonomy"
    )
    query_tax <- entrez_link(
      dbfrom = "taxonomy",
      id = query_tax2$links$taxonomy_taxonomy_species,
      db = "genome"
    )
  }

  # get genome summary
  query_genome <- entrez_summary(
    db = "genome",
    id = query_tax$links$taxonomy_genome
  )

  # get nuccore links from assembly id
  query_nuccore <- entrez_link(
    dbfrom = "assembly",
    id = query_genome$assemblyid,
    db = "nuccore"
  )

  # retrieve sequence summary from nuccore links
  query_result <- entrez_fetch(
    db = "nuccore",
    id = query_nuccore$links$assembly_nuccore_refseq,
    rettype = "xml",
    parsed = TRUE
  ) %>%
    XML::xmlToDataFrame() %>%
    filter(str_detect(GBSeq_locus, "^N[CZ]\\_[A-Z0-9]+$")) %>%
    as_tibble() %>%
    rename_with(~ str_remove(.x, "GBSeq\\_")) %>%
    select(locus:taxonomy) %>%
    mutate(across(matches("length"), as.numeric))

  # # OPTIONAL: retrieve gene-wise summary or sequence
  # query_genes <- entrez_fetch(
  #   "nuccore",
  #   id = query_nuccore$links$assembly_nuccore_refseq,
  #   rettype = "fasta_cds_na", # feature tableonly: ft
  #   retmode = "text"
  # )
  return(query_result)
}
