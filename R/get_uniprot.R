get_uniprot <- function(query, reviewed = NULL) {
  if (!is.na(as.numeric(query))) {
    query_type <- "taxonomy_id"
  } else {
    query_type <- "organism_name"
    query <- gsub(" ", "%2C", query)
  }
  if (!is.null(reviewed)) {
    if (reviewed) {
      reviewed <- "+AND+%28reviewed%3Atrue%29"
    } else {
      reviewed <- "+AND+%28reviewed%3Afalse%29"
    }
  } else {
    reviewed <- "%20"
  }
  uniprot_url <- paste0(
    "https://rest.uniprot.org/uniprotkb/stream?compressed=false&fields=accession",
    "%2Creviewed%2Cid%2Cprotein_name%2Cgene_names%2Corganism_name%2Corganism_id",
    "%2Cxref_refseq_full%2Clength%2Cgene_oln%2Ccc_pathway%2Cgo_id%2Cgo_p%2C",
    "cc_subcellular_location&format=tsv&query=%28%28", query_type, "%3A",
    query, "%29%29", reviewed
  )
  server_error = simpleError("")
  df_uniprot <- tryCatch({
    response <- GET(uniprot_url)
    if (status_code(response) == 200) {
      df <- read_tsv(uniprot_url, show_col_types = FALSE)
      return(df)
    } else {
      return(NULL)
    }
  }, error = function(server_error) {
    warning("Uniprot server not available")
    return(NULL)
  })
  if (nrow(df_uniprot) > 10000) {
    stop(
      paste(
        "Size of retrieved result table from Uniprot exceeds 10,000 rows;",
        "Please use more soecific strain name or taxonomy ID.",
        sep = "\n"
      )
    )
  }
}

