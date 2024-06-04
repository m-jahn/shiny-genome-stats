format_uniprot <- function(df) {
  df %>%
    rename_with(tolower) %>%
    rename(
      uniprot_ID = entry,
      uniprot_name = `entry name`,
      locus_tag = `gene names (ordered locus)`,
      gene_name_long = `gene names`,
      protein = `protein names`,
      gene_ontology_ids = `gene ontology ids`
    )
}
