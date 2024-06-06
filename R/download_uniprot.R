# script to automatically download genome annotation from Uniprot
# for selected microbes
#
# information on how to retrieve gene annotation from Uniprot:
# https://www.uniprot.org/help/api_queries
#
# information about which columns can be selected:
# got to uniprot.org, search for proteins, click download, create API call
#
if (!length(list.files("data/"))) {
  config <- configr::read.config("config/config.yml")
  list_genomes <- unlist(config$data$genomes)

  for (taxid in list_genomes) {
    if (taxid %in% c(224308)) revwd <- TRUE else revwd <- NULL
    df <- get_uniprot(taxid, reviewed = revwd)
    df <- format_uniprot(df)
    if (taxid == 99287) {
      df <- filter(df, str_detect(organism, "LT2"))
    }
    if (taxid == 301447) {
      df <- filter(df, organism == "Streptococcus pyogenes serotype M1")
    }
    if (taxid == 381666) {
      df <- filter(df, !is.na(locus_tag))
    }
    write_tsv(df, paste0("data/", taxid, ".tsv"))
  }
}

