format_uniprot <- function(df) {
  df %>%
    rename_with(tolower) %>%
    rename(
      uniprot_ID = entry,
      uniprot_name = `entry name`,
      locus_tag = `gene names (ordered locus)`,
      gene_name_long = `gene names`,
      protein = `protein names`,
      gene_ontology_ids = `gene ontology ids`,
      localization = `subcellular location [cc]`
    ) %>%
    mutate(organism = str_remove_all(organism, "\\(|\\)|strain ")) %>%
    mutate(localization = str_remove_all(
      localization,
      "SUBCELLULAR LOCATION: | \\{ECO:.*\\}") %>% tolower()
    ) %>%
    mutate(
      localization = case_when(
        .default = "unknown",
        str_detect(localization, "cytoplasm|cytosol") ~ "cytoplasm",
        str_detect(localization, "cell (inner )?membrane") ~ "inner membrane",
        str_detect(localization, "(cell )?outer membrane") ~ "outer membrane",
        str_detect(localization, "periplasm") ~ "periplasm",
        str_detect(localization, "flagellum|fimbr") ~ "flagellum",
        str_detect(localization, "secreted") ~ "secreted"
      )
    )
}
