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

  # ALL REACTIVE DATA SELECTIONS
  # select summary data
  df_summary_selected <- reactive({
    df_summary_selected <- df_summary %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_summary_selected)
  })

  # select genome feature data
  df_features_selected <- reactive({
    df_features_selected <- df_features %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_features_selected)
  })

  # select genome sequence data
  df_sequences_selected <- reactive({
    df_sequences_selected <- df_sequences %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_sequences_selected)
  })

  # select gene-wise-stats
  df_cds_width_selected <- reactive({
    df_cds_width_selected <- df_cds_width %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_cds_width_selected)
  })

  # select codon bias data
  df_codon_bias_selected <- reactive({
    df_codon_bias_selected <- df_codon_bias %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_codon_bias_selected)
  })

  # select start codon frequency data
  df_startcodons_selected <- reactive({
    df_startcodons_selected <- df_start_codons %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_startcodons_selected)
  })

  # select stop codon frequency data
  df_stopcodons_selected <- reactive({
    df_stopcodons_selected <- df_stop_codons %>%
      dplyr::filter(accession %in% list_genomes[input$UserDataChoice])
    return(df_stopcodons_selected)
  })

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
      x / sum(x, na.rm = TRUE)
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
    abbr <- str_split(name, " ") %>%
      sapply(function(x) {
        paste0(str_sub(x[1], 1, 1), ". ", paste(x[-1], collapse = " ")) %>%
          str_sub(1, 25)
      })
    # mark duplicate organism names
    duplicated_names <- duplicated(abbr)
    if (any(duplicated_names)) {
      abbr[duplicated_names] <- paste0(abbr[duplicated_names], " (", seq_len(sum(duplicated_names)), ")")
    }
    return(abbr)
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

  # OUTPUT 2: GENOME STATS
  output$genome_info.ui <- renderUI({
    plotOutput("genome_info", height = "420px", width = "100%")
  })

  output$genome_info <- renderPlot(res = 96, {
    df <- df_sequences_selected() %>%
      dplyr::select(organism, seq_count, total_length, gc_content, gc_skew) %>%
      distinct() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      pivot_longer(
        cols = c(seq_count, total_length, gc_content, gc_skew),
        names_to = "metric",
        values_to = "value"
      ) %>%
      mutate(
        metric = factor(
          metric,
          levels = c("seq_count", "total_length", "gc_content", "gc_skew"),
          labels = c("Sequence count", "Total length [Mb]", "GC content [%]", "GC skew")
        ),
        value = case_when(
          metric == "Total length [Mb]" ~ value / 10^6,
          metric == "GC content [%]" ~ value * 100,
          TRUE ~ value
        )
      )

    if (is.null(df) || nrow(df) == 0) {
      return()
    }

    plot <- ggplot(df, aes(x = organism, y = value)) +
      geom_col(fill = current_palette()[1], color = "white") +
      facet_wrap(~metric, scales = "free_y", nrow = 2) +
      labs(x = "", y = "") +
      current_theme() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
        strip.text = element_text(face = "bold")
      )

    print(plot)
  })


  # OUTPUT 3: GENOME REGIONS
  output$genome_regions.ui <- renderUI({
    plotOutput("genome_regions", height = "420px", width = "100%")
  })

  output$genome_regions <- renderPlot(res = 96, {
    df <- df_sequences_selected() %>%
      dplyr::select(organism, seq_lengths_top10) %>%
      mutate(organism = abbreviate_org(organism)) %>%
      unnest(cols = seq_lengths_top10) %>%
      rename(n = seq_lengths_top10) %>%
      group_by(organism) %>%
      mutate(vars = factor(seq_along(n)))

    plot <- do.call(
      input$UserTypeGenomeRegions, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "Chromosomes (contigs)", input$UserNRows, input$UserNCols
      )
    )
    print(plot + theme(legend.position = "none"))
  })


  # OUTPUT 4: GENOME FEATURES
  output$genome_features.ui <- renderUI({
    plotOutput("genome_features", height = "420px", width = "100%")
  })

  output$genome_features <- renderPlot(res = 96, {
    df <- df_features_selected() %>%
      dplyr::select(organism, gene, CDS, pseudogene, rRNA, tRNA) %>%
      distinct() %>%
      mutate(organism = abbreviate_org(organism)) %>%
      pivot_longer(
        cols = c(gene, CDS, pseudogene, rRNA, tRNA),
        names_to = "vars",
        values_to = "n"
      ) %>%
      mutate(
        n = replace_na(n, 0),
        vars = factor(
          vars,
          levels = c("gene", "CDS", "pseudogene", "rRNA", "tRNA"),
          labels = c("Genes", "CDS", "Pseudogenes", "rRNAs", "tRNAs")
        )
      )

    if (is.null(df) || nrow(df) == 0) {
      return()
    }

    plot <- do.call(
      input$UserTypeGenomeFeatures, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "Genome feature counts", input$UserNRows, input$UserNCols
      )
    )
    print(plot)
  })


  # OUTPUT 5: HISTOGRAM WITH PROTEIN LENGTHS
  output$protein_length.ui <- renderUI({
    plotOutput("protein_length", height = "420px", width = "100%")
  })

  output$protein_length <- renderPlot(res = 96, {
    df <- df_cds_width_selected() %>%
      mutate(vars = as.factor(bin_center), n = count)

    plot <- do.call(
      input$UserTypeGenes, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "Protein length (0-1,000 aa)", input$UserNRows, input$UserNCols
      )
    ) +
      theme(legend.position = "none")
    print(plot)
  })


  # OUTPUT 6: START CODON FREQUENCY
  output$startcodons.ui <- renderUI({
    plotOutput("startcodons", height = "420px", width = "100%")
  })

  output$startcodons <- renderPlot(res = 96, {
    df <- df_startcodons_selected() %>%
      filter(codon %in% names(sort(table(codon), decreasing = TRUE)[1:5])) %>%
      mutate(vars = fct_reorder(codon, count, .desc = TRUE), n = count)

    plot <- do.call(
      input$UserTypeStartcodons, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "", input$UserNRows, input$UserNCols
      )
    )
    print(plot)
  })


  # OUTPUT 7: STOP CODON FREQUENCY
  output$stopcodons.ui <- renderUI({
    plotOutput("stopcodons", height = "420px", width = "100%")
  })

  output$stopcodons <- renderPlot(res = 96, {
    df <- df_stopcodons_selected() %>%
      filter(codon %in% names(sort(table(codon), decreasing = TRUE)[1:5])) %>%
      mutate(vars = fct_reorder(codon, count, .desc = TRUE), n = count)

    plot <- do.call(
      input$UserTypeStopcodons, list(
        df, input, aggregation, current_theme(),
        current_palette(), "vars", "", input$UserNRows, input$UserNCols
      )
    )
    print(plot)
  })
}
