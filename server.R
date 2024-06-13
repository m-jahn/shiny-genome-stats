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
  for (genome in names(list_genomes)[1:4]) {
    list_data_selected[[genome]] <- list_genomes[genome]
  }

  # reactive variable that holds genomes to download
  list_download <- reactiveValues()

  # reactive variable that just collects newest status posts
  list_status <- reactiveValues()

  # function that searches new genomes from input button
  observeEvent(input$UserSearchGenome, {
    for (taxname in names(list_download)) {
      list_download[[taxname]] <- NULL
    }
    if (!(is.na(input$UserDataSearch) | input$UserDataSearch == "")) {
      taxa <- get_ncbi_taxid(input$UserDataSearch)
      if (length(taxa) > 100) {
        max_taxa <- 100
        list_status[["latest"]] <- paste0("Retrieved >", max_taxa, " taxa from NCBI")
      } else {
        max_taxa <- length(taxa)
        list_status[["latest"]] <- paste0("Retrieved ", length(taxa), " taxa from NCBI")
      }
      for (taxname in names(taxa)[1:max_taxa]) {
        list_download[[taxname]] <- taxa[taxname]
      }
    }
  })

  # reactive field for genome search
  output$DataSearch <- renderUI({
    textInput(
      "UserDataSearch",
      "Find Microbial Genome",
      value = "Vibrio cholerae O1 biovar El Tor"
    )
  })

  # reactive field to display searched genomes
  output$DataSelection <- renderUI({
    selectInput(
      "UserDataSelection",
      "Add Microbial Genome",
      choices = names(list_download),
      selected = names(list_download)[1],
      multiple = FALSE,
      selectize = TRUE
    )
  })

  # function to add a new genome from uniprot query
  observeEvent(input$UserAddGenome, {
    req(input$UserDataSelection)
    taxid <- list_download[[input$UserDataSelection]]
    time_fetch <- system.time({
      df <- get_uniprot(taxid)
      df <- format_uniprot(df)
      df_summary <- get_ncbi_genome(taxid)
    })
    if (!(is.null(df) | nrow(df) < 1)) {
      write_tsv(df, paste0("data/", taxid, ".tsv"))
      list_data[[names(taxid)]] <- taxid
      list_data_selected[[names(taxid)]] <- taxid
      list_status[["latest"]] <- paste0(
        "Downloaded genome in ",
        round(time_fetch[3]), " sec"
      )
    }
    if (!(is.null(df_summary) | nrow(df_summary) < 1)) {
      write_tsv(df_summary, paste0("data/", taxid, "_summary.tsv"))
    }
  })

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

  # reactive function to import selected data
  df_selected_genomes <- reactive({
    list_df <- lapply(input$UserDataChoice, function(id) {
      df_file <- paste0(data_dir, unname(list_data[[id]]), ".tsv")
      df <- read_tsv(df_file, show_col_types = FALSE)
    })
    df <- bind_rows(list_df, .id = "genome")
    return(df)
  })

  # reactive function to import genome summary
  df_genome_summary <- reactive({
    list_df <- lapply(input$UserDataChoice, function(id) {
      df_file <- paste0(data_dir, unname(list_data[[id]]), "_summary.tsv")
      if (file.access(df_file) == 0) {
        df <- read_tsv(df_file, show_col_types = FALSE)
      } else {
        df <- NULL
      }
    })
    df <- bind_rows(list_df, .id = "genome")
    return(df)
  })

  # display status for newly added data
  output$AddStatus <- renderText(
    if (input$UserSearchGenome == 0) {
      ""
    } else {
      list_status[["latest"]]
    }
  )

  # PLOTTING OPTIONS
  # ---------------------------------------------
  #
  # reactive value that holds theme
  current_theme <- reactive({
    switch(
      input$UserTheme,
      custom = custom_theme(),
      `ggplot default` = theme(),
      `ggplot dark` = theme_dark(),
      `ggplot light` = theme_light(),
      `ggplot bw` = theme_bw()
    )
  })

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
      selected = config$plot$theme[1])
  })

  output$UserGrouping <- renderUI({
    selectInput(
      "UserGrouping",
      "Grouping",
      config$plot$grouping,
      selected = config$plot$grouping[1])
  })

  # DATA OUTPUT AND PLOTTING
  # ---------------------------------------------
  #
  # OUTPUT 1: SUMMARY STATISTICS
  output$genomeSummary <- renderDataTable({
    df_uniprot <- df_selected_genomes()
    if (is.null(df_uniprot) || nrow(df_uniprot) == 0) {
      return()
    } else {
      df_summary <- df_uniprot %>%
        mutate(organism = substr(organism, 1, 25)) %>%
        group_by(.data[[input$UserGrouping]]) %>%
        summarize(
          all = n(),
          hypothetical = sum(str_detect(protein, "[Uu]nknown|[Hh]ypothetical|[Uu]ncharacteri"))
        ) %>%
        mutate(`%` = round(hypothetical/all*100))
      datatable(df_summary, options = list(dom = 't'))
    }
  })

  # OUTPUT 2: BARCHART WITH CATEGORIES
  output$categories.ui <- renderUI({
    plotOutput("categories", height = "330px", width = "100%")
  })

  output$categories <- renderPlot(res = 96, {
    df <- df_selected_genomes() %>%
      mutate(organism = substr(organism, 1, 25)) %>%
      group_by(organism) %>%
      summarize(
        `all` = n(),
        reviewed = sum(reviewed == "reviewed"),
        `hypothetical` = sum(
          str_detect(protein, "[Uu]nknown|[Hh]ypothetical|[Uu]ncharacteri")
        ),
        `putative` = sum(str_detect(protein, "[Pp]utative")),
        transport = sum(str_detect(protein, "[TT]ransporter")),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = !matches("organism"),
        names_to = "proteins",
        values_to = "count"
      ) %>%
      mutate(proteins = forcats::fct_inorder(proteins))
    plot <- df %>%
      ggplot(aes(x = proteins, y = count, fill = proteins)) +
      geom_col(color = "white") +
      geom_text(
        aes(label = count, color = proteins),
        size = 2.5, nudge_y = max(df$count) * 0.03
      ) +
      facet_wrap( ~ organism, nrow = 1) +
      labs(x = "", y = "") +
      current_theme() +
      theme(
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm")
      ) +
      scale_fill_manual(values = current_palette()) +
      scale_color_manual(values = current_palette())
    print(plot)
  })


  # OUTPUT 3: BARCHART WITH LOCALIZATION
  output$localization.ui <- renderUI({
    plotOutput("localization", height = "450px", width = "100%")
  })

  output$localization <- renderPlot(res = 96, {
    plot <- df_selected_genomes() %>%
      mutate(organism = substr(organism, 1, 25)) %>%
      group_by(organism) %>%
      count(localization) %>%
      arrange(desc(n)) %>%
      mutate(
        fraction = n/sum(n),
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1))
      ) %>%
      ggplot(aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = localization)) +
      geom_rect(color = "white") +
      coord_polar(theta = "y") +
      lims(x = c(0, 4)) +
      facet_wrap( ~ organism, nrow = 2) +
      labs(x = "", y = "") +
      current_theme() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm")
      ) +
      scale_fill_manual(values = current_palette())
    print(plot)
  })

  # OUTPUT 4: HISTOGRAM WITH PROTEIN LENGTHS
  output$protein_length.ui <- renderUI({
    plotOutput("protein_length", height = "450px", width = "100%")
  })

  output$protein_length <- renderPlot(res = 96, {
    plot <- df_selected_genomes() %>%
      mutate(organism = substr(organism, 1, 25)) %>%
      ggplot(aes(x = length)) +
      geom_histogram(
        fill = current_palette()[1],
        color = "white",
        bins = 25
      ) +
      lims(x = c(0, input$UserMaxLength)) +
      facet_wrap( ~ organism, nrow = 2) +
      labs(x = "", y = "") +
      current_theme()
    print(plot)
  })

  # OUTPUT 5: PATHWAYS
  output$pathways.ui <- renderUI({
    plotOutput("pathways", height = "450px", width = "100%")
  })

  output$pathways <- renderPlot(res = 96, {
    plot <- df_selected_genomes() %>%
      select(organism, go_bp) %>%
      mutate(
        organism = substr(organism, 1, 25),
        go_bp = str_remove_all(go_bp, " \\[GO\\:[0-9]+\\]")
      ) %>%
      separate_longer_delim(go_bp, "; ") %>%
      mutate(top = go_bp %in% names(
        sort(table(go_bp), decreasing = TRUE)
      )[1:input$UserTopBioProcess]) %>%
      mutate(go_bp = ifelse(top, go_bp, "other")) %>%
      group_by(organism) %>%
      count(go_bp) %>%
      filter(!is.na(go_bp), go_bp != "other") %>%
      mutate(
        go_bp = fct_inorder(substr(go_bp, 1, 20)),
        fraction = n/sum(n),
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1))
      ) %>%
      ggplot(aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax, fill = go_bp)) +
      geom_rect(color = "white") +
      coord_polar(theta = "y") +
      lims(x = c(0, 4)) +
      facet_wrap( ~ organism, nrow = 1) +
      labs(x = "", y = "", subtitle = "Top 20 GO-BP terms by number of proteins") +
      current_theme() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm")
      ) +
      scale_fill_manual(values = colorRampPalette(current_palette())(
        input$UserTopBioProcess))
    print(plot)
  })

  # OUTPUT 6: BARCHART WITH GENOME INFO
  output$genome_info.ui <- renderUI({
    plotOutput("genome_info", height = "250px", width = "100%")
  })

  output$genome_info <- renderPlot(res = 96, {
    plot <- df_genome_summary() %>%
      mutate(organism = substr(organism, 1, 25)) %>%
      group_by(organism) %>%
      arrange(length) %>%
      mutate(
        locus = fct_inorder(locus),
        topology = factor(topology, c("circular", "linear"))
      ) %>%
      ggplot(aes(x = length/10^6, y = locus, fill = topology)) +
      geom_col(color = "white") +
      geom_text(
        aes(label = round(length/10^6, 1), color = topology),
        size = 2.5, nudge_x = max(df_genome_summary()$length)/10^7.2
      ) +
      facet_wrap( ~ organism, nrow = 1, scales = "free_y") +
      labs(x = "", y = "", subtitle = "Chromosomes and plasmids, Mbases") +
      current_theme() +
      theme(
        legend.position = "bottom",
        legend.key.size = unit(0.4, "cm")
      ) +
      scale_fill_manual(values = current_palette()) +
      scale_color_manual(values = current_palette())
    print(plot)
  })

}
