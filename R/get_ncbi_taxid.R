get_ncbi_taxid <- function(search_string, max_taxa = 100) {
  messages <- NULL
  suffix <- "[Strain]"
  query <- entrez_search(db = "taxonomy", paste0(search_string, suffix))

  if (length(query$ids)) {
    query_rank <- entrez_summary(db = "taxonomy", query$ids)$rank
  } else {
    return(NULL)
  }

  if (query_rank %in% c("strain", "no rank", "")) {
    query_ids <- query$ids
  } else {
    query_ids <- entrez_link(
      dbfrom = "taxonomy",
      id = query$ids,
      db = "taxonomy")
    query_ids <- query_ids$links$taxonomy_taxonomy_child
  }

  if (length(query_ids)) {
    if (length(query_ids) > max_taxa) {
      messages <- c(messages, paste0("Retrieved >", max_taxa, " taxa from NCBI"))
      query_ids <- sort(as.numeric(query_ids))[1:max_taxa] %>%
        as.character()
    } else {
      messages <- c(messages, paste0("Retrieved ", length(query_ids), " taxa from NCBI"))
    }
    query_items <- entrez_fetch(
      db = "taxonomy",
      id = query_ids,
      rettype = "xml",
      parsed = TRUE
    )
  } else {
    query_items <- NULL
  }

  if (is.null(query_items)) {
    return(NULL)
  } else {
    query_table <- query_items %>%
      XML::xmlToDataFrame() %>%
      as_tibble() %>%
      filter(Rank %in% c("strain", "no rank")) %>%
      select(ScientificName, TaxId) %>%
      mutate(ScientificName = str_remove_all(ScientificName, "'")) %>%
      arrange(as.numeric(TaxId)) %>%
      filter(!duplicated(TaxId)) %>%
      tibble::deframe()
  }

  if (length(query_table)) {
    return(list(result = query_table, messages = messages))
  } else {
    return(NULL)
  }
}
