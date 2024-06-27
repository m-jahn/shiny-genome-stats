# script to automatically download genome annotation from Uniprot and NCBI
# for selected microbes
#
# information on how to retrieve gene annotation from Uniprot:
# https://www.uniprot.org/help/api_queries
#
# information about which columns can be selected:
# got to uniprot.org, search for proteins, click download, create API call
#
# information on possible queries on MCBI databases using
# r-entrez:
# https://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.T._valid_values_of__retmode_and/
#
if (!length(list.files("data/"))) {
  config <- configr::read.config("config/config.yml")
  list_genomes <- unlist(config$data$genomes)
  for (taxid in list_genomes[9]) {
    if (taxid %in% c(224308)) revwd <- TRUE else revwd <- NULL
    df <- get_uniprot(taxid, reviewed = revwd)
    df <- format_uniprot(df)
    df_summary <- get_ncbi_genome(taxid)
    kegg_org_id <- na.omit(unique(str_extract(df$kegg, "^[a-z]{3}")))
    if (taxid == 100226) { kegg_org_id <- "sco" }
    df_kegg <- get_kegg_pathways(id = kegg_org_id[1])
    df_kegg$organism <- df$organism[1]
    write_tsv(df, paste0("data/", taxid, ".tsv"))
    write_tsv(df_summary, paste0("data/", taxid, "_summary.tsv"))
    write_tsv(df_kegg, paste0("data/", taxid, "_kegg.tsv"))
    message(paste0("downloaded genome annotation for taxid: ", taxid))
  }
}
