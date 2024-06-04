#
# SHINY SERVER
# ***********************************************
server <- function(input, output, session) {

  # DATA SELECTION AND INPUT
  # ---------------------------------------------
  # reactive variable that holds genome list
  datalist <- reactiveValues()
  for (genome in names(list_genomes)) {
    datalist[[genome]] <- list_genomes[genome]
  }

  # reactive variable that just collects newest status posts
  status_list <- reactiveValues()

  # function that adds new data from input button
  observeEvent(input$UserAddGenome, {
    if (!(is.na(input$UserDataGet) | input$UserDataGet == "")) {
      df <- get_uniprot(input$UserDataGet)
      if (nrow(df)) {
        org_name <- substr(names(table(df$Organism))[1], 1, 50)
        file_name <- str_replace_all(org_name, "[[:punct:]]", "_")
        write_tsv(df, paste0("data/", file_name, ".tsv"))
        datalist[[org_name]] <- file_name
        status_list[["latest"]] <- paste0(
          "Downloaded genome data for organism '", org_name, "' from Uniprot"
        )
      }
    }
  })

  # reactive field for user selection of data
  output$DataChoice <- renderUI({
    selectInput(
      "UserDataChoice",
      "Select Microbial Genome:",
      choices = names(datalist),
      selected = names(datalist)[1:3],
      multiple = TRUE,
      selectize = TRUE
    )
  })

  # reactive field for free text input
  output$DataGet <- renderUI({
    textInput(
      "UserDataGet",
      "Find Microbial Genome:",
      value = ""
    )
  })

  # reactive function to import selected data
  df_selected_genomes <- reactive({
    req(input$UserDataChoice)
    list_df <- lapply(input$UserDataChoice, function(id) {
      df_file <- paste0(data_dir, unname(datalist[[id]]), ".tsv")
      df <- read_tsv(df_file, show_col_types = FALSE)
      df <- format_uniprot(df)
    })
    df <- bind_rows(list_df, .id = "genome")
    return(df)
  })

  # display status for newly added data
  output$AddStatus <- renderText(
    if (input$UserAddGenome == "") {
      "Search for uniprot tax ID ('224308') or\nstrain name ('Bacillus subtilis 168')"
    } else {
      status_list[["latest"]]
    }
  )

  # DATA OUTPUT AND PLOTTING
  # ---------------------------------------------
  #
  # output: display summary statistics
  output$genomeSummary <- renderDataTable({
    df_uniprot <- df_selected_genomes()
    if (is.null(df_uniprot) || nrow(df_uniprot) == 0) {
      return()
    } else {
      df_summary <- df_uniprot %>%
        group_by(organism) %>%
        summarize(
          `all proteins` = n(),
          `hypothetical proteins` = sum(str_detect(protein, "[Uu]nknown|[Hh]ypothetical|[Uu]ncharacteri")),
          `putative proteins` = sum(str_detect(protein, "[Pp]utative")),
          pathways = length(unique(pathway)),
          #`GO terms` = str_gene_ontology_ids
          .groups = "drop"
        )
      datatable(df_summary)
    }
  })
}
