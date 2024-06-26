format_uniprot <- function(df) {
  df %>%
    rename_with(tolower) %>%
    rename(
      uniprot_ID = entry,
      uniprot_name = `entry name`,
      locus_tag = `gene names (ordered locus)`,
      gene_name_long = `gene names`,
      protein = `protein names`,
      go_ids = `gene ontology ids`,
      go_bp = `gene ontology (biological process)`,
      localization = `subcellular location [cc]`,
      organism_id = `organism (id)`
    ) %>%
    mutate(organism = str_remove_all(organism, "\\(|\\)|strain ")) %>%
    filter(organism == names(sort(table(organism), decreasing = TRUE))[1]) %>%
    mutate(localization = str_remove_all(
      localization,
      "SUBCELLULAR LOCATION: | \\{ECO:.*\\}") %>% tolower()
    ) %>%
    mutate(kegg = str_remove_all(kegg, "\\;.*")) %>%
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
    ) %>%
    filter(!is.na(locus_tag))
}
