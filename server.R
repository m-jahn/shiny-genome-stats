#
# SHINY SERVER
# ***********************************************
server <- function(input, output, session) {
  # DATA SELECTION AND INPUT
  # ---------------------------------------------
  # reactive variable that holds genome list
  list_data <- reactiveValues()
  for (genome in names(list_genomes)) {
    list_data[[genome]] <- list_genomes[genome]
  }

  list_data_selected <- reactiveValues()
  for (genome in names(list_selected)) {
    list_data_selected[[genome]] <- list_genomes[genome]
  }

  # reactive field for user selection of data
  output$DataChoice <- renderUI({
    selectInput(
      "UserDataChoice",
      "Select Microbial Genome",
      choices = names(list_data),
      selected = names(list_data_selected),
      multiple = TRUE,
      selectize = TRUE
    )
  })

  # reactive function to select summary data
  df_summary_selected <- reactive({
    df_summary_selected <- df_summary %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_summary_selected)
  })

  # reactive function to select genome feature data
  df_features_selected <- reactive({
    df_features_selected <- df_features %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_features_selected)
  })

  # reactive function to select genome sequence data
  df_sequences_selected <- reactive({
    df_sequences_selected <- df_sequences %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_sequences_selected)
  })

  # reactive function to import kegg pathways
  # df_kegg <- reactive({
  #   list_df <- lapply(input$UserDataChoice, function(id) {
  #     df_file <- paste0(data_dir, unname(list_data[[id]]), "_kegg.tsv")
  #     if (file.access(df_file) == 0) {
  #       df <- read_tsv(df_file, show_col_types = FALSE)
  #     } else {
  #       df <- NULL
  #     }
  #   })
  #   df <- bind_rows(list_df, .id = "genome")
  #   return(df)
  # })

  # PLOTTING OPTIONS
  # ---------------------------------------------
  #
  # reactive value that holds theme
  current_theme <- reactive({
    switch(input$UserTheme,
      custom = custom_theme(),
      `ggplot default` = theme(),
      `ggplot dark` = theme_dark(),
      `ggplot light` = theme_light(),
      `ggplot bw` = theme_bw()
    )
  })

  # choice of aggregation function
  aggregation <- function(x) {
    if (input$UserFrequency == "relative") {
      x / sum(x)
    } else if (input$UserFrequency == "absolute") {
      x
    } else {
      stop()
    }
  }

  # apply log or lin transformation to orig data
  logfun <- function(x) {
    if (input$UserLogY == "linear") {
      x
    } else if (input$UserLogY == "log 2") {
      log2(x)
    } else if (input$UserLogY == "log 10") {
      log10(x)
    } else {
      log(x)
    }
  }

  # function to abbreviate a genus / species name
  abbreviate_org <- function(name) {
    str_split(name, " ") %>%
      sapply(function(x) {
        paste0(str_sub(x[1], 1, 1), ". ", paste(x[-1], collapse = " ")) %>%
        str_sub(1, 25)
      })
  }

  # reactive value for color palettes
  current_palette <- reactive({
    palettes(pal = input$UserColorPalette)
  })

  # dynamic user inputs
  output$UserTheme <- renderUI({
    selectInput(
      "UserTheme",
      "Theme",
      config$plot$theme,
      selected = config$plot$theme[1]
    )
  })

  output$UserGrouping <- renderUI({
    selectInput(
      "UserGrouping",
      "Grouping",
      config$plot$grouping,
      selected = config$plot$grouping[1]
    )
  })

  # DATA OUTPUT AND PLOTTING
  # ---------------------------------------------
  #
  # OUTPUT 1: SUMMARY STATISTICS
  output$genomeSummary <- renderDataTable({
    df <- df_features_selected() %>%
      dplyr::select(organism, region, gene, CDS) %>%
      mutate(organism = abbreviate_org(organism))
    if (is.null(df) || nrow(df) == 0) {
      return()
    } else {
      datatable(df, options = list(dom = "t"))
    }
  })

  # OUTPUT 2: BARCHART WITH CATEGORIES
  output$categories.ui <- renderUI({
    plotOutput("categories", height = "330px", width = "100%")
  })

  output$categories <- renderPlot(res = 96, {
    df <- df_selected_genomes() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      group_by(organism) %>%
      mutate(category = case_when(
        str_detect(protein, "[Uu]nknown|[Hh]ypothetical|[Uu]ncharacteri") ~ "unknown",
        str_detect(protein, "[Pp]utative") ~ "putative",
        str_detect(protein, "[Rr]egulator") ~ "regulation",
        str_detect(protein, "[Tt]ransport") ~ "transport",
        reviewed == "reviewed" ~ "reviewed function",
        TRUE ~ "other function"
      )) %>%
      count(category) %>%
      mutate(vars = factor(
        category,
        c(
          "reviewed function", "other function", "regulation", "transport",
          "putative", "unknown"
        )
      )) %>%
      arrange(as.numeric(vars))
    plot <- do.call(
      input$UserTypeAnno, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "", 1
      )
    )
    print(plot)
  })


  # OUTPUT 3: LOCALIZATION
  output$localization.ui <- renderUI({
    plotOutput("localization", height = "450px", width = "100%")
  })

  output$localization <- renderPlot(res = 96, {
    df <- df_selected_genomes() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      group_by(organism) %>%
      count(localization) %>%
      mutate(localization = factor(
        localization,
        c(
          "cytoplasm", "periplasm", "inner membrane", "outer membrane",
          "flagellum", "secreted", "unknown"
        )
      )) %>%
      arrange(as.numeric(localization)) %>%
      rename(vars = localization)
    plot <- do.call(
      input$UserTypeLocal, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "", 2
      )
    )
    print(plot)
  })

  # OUTPUT 4: HISTOGRAM WITH PROTEIN LENGTHS
  output$protein_length.ui <- renderUI({
    plotOutput("protein_length", height = "450px", width = "100%")
  })

  output$protein_length <- renderPlot(res = 96, {
    plot <- df_selected_genomes() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      ggplot(aes(x = length)) +
      geom_histogram(
        fill = current_palette()[1],
        color = "white",
        bins = 25
      ) +
      lims(x = c(0, input$UserMaxLength)) +
      facet_wrap(~organism, nrow = 2) +
      labs(x = "", y = "") +
      current_theme()
    print(plot)
  })

  # OUTPUT 5: KEGG PATHWAYS
  output$kegg.ui <- renderUI({
    plotOutput("kegg", height = "450px", width = "100%")
  })

  output$kegg <- renderPlot(res = 96, {
    df <- df_kegg() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      mutate(top = kegg_pathway %in% names(sort(table(kegg_pathway), decreasing = TRUE))[1:(input$UserTopPathways - 1)]) %>%
      mutate(kegg_pathway = ifelse(top, kegg_pathway, "other")) %>%
      group_by(organism) %>%
      count(kegg_pathway) %>%
      group_by(kegg_pathway) %>%
      mutate(total_count = sum(n)) %>%
      arrange(desc(total_count)) %>%
      group_by(organism) %>%
      rename(vars = kegg_pathway) %>%
      mutate(vars = fct_inorder(substr(vars, 1, 20)))
    plot <- plot <- do.call(
      input$UserTypeKegg, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "Top 20 KEGG pathways by number of proteins", 1
      )
    )
    print(plot)
  })

  # OUTPUT 6: GO TERMS
  output$goterms.ui <- renderUI({
    plotOutput("goterms", height = "450px", width = "100%")
  })

  output$goterms <- renderPlot(res = 96, {
    df <- df_selected_genomes() %>%
      select(organism, go_bp) %>%
      mutate(
        organism = abbreviate_org(organism),
        go_bp = str_remove_all(go_bp, " \\[GO\\:[0-9]+\\]")
      ) %>%
      separate_longer_delim(go_bp, "; ") %>%
      mutate(top = go_bp %in% names(
        sort(table(go_bp), decreasing = TRUE)
      )[1:input$UserTopBioProcess]) %>%
      mutate(go_bp = ifelse(top, go_bp, "other")) %>%
      group_by(organism) %>%
      count(go_bp) %>%
      group_by(go_bp) %>%
      mutate(total_count = sum(n)) %>%
      arrange(desc(total_count)) %>%
      group_by(organism) %>%
      filter(!is.na(go_bp), go_bp != "other") %>%
      rename(vars = go_bp) %>%
      mutate(vars = fct_inorder(substr(vars, 1, 20)))
    plot <- do.call(
      input$UserTypeGO, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "Top 20 GO-BP terms by number of proteins", 1
      )
    )
    print(plot)
  })

  # OUTPUT 7: GENOME STATS
  output$genome_info.ui <- renderUI({
    plotOutput("genome_info", height = "250px", width = "100%")
  })

  output$genome_info <- renderPlot(res = 96, {
    df <- df_genome_summary() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      group_by(organism) %>%
      arrange(desc(length)) %>%
      mutate(
        n = length / 10^6,
        topology = factor(topology, c("circular", "linear")),
        rank = as.factor(seq_along(n))
      ) %>%
      arrange(length) %>%
      mutate(vars = fct_inorder(locus))
    if (input$UserTypeGenome == "barchart") {
      plot <- do.call(
        input$UserTypeGenome,
        list(
          df, input, aggregation, current_theme(),
          current_palette(), "rank", "Chromosomes and plasmids [Mbases]", 1
        )
      ) +
        coord_flip() +
        facet_wrap(~organism, nrow = 1, scales = "free_y") +
        theme(axis.text.x = element_text(), legend.position = "none")
    }
    if (input$UserTypeGenome == "piechart") {
      plot <- do.call(
        input$UserTypeGenome,
        list(
          df, input, aggregation, current_theme(),
          current_palette(), "rank", "Chromosomes and plasmids [Mbases]", 1
        )
      ) +
        theme(legend.position = "none")
    }
    print(plot)
  })
}
