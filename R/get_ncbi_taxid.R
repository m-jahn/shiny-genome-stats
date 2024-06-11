get_ncbi_taxid <- function(search_string) {
  query <- entrez_search(db = "taxonomy", search_string)

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
    return(query_table)
  } else {
    return(NULL)
  }
}
