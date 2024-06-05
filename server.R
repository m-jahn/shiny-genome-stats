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
      "Select Microbial Genome",
      choices = names(datalist),
      selected = names(datalist)[1:4],
      multiple = TRUE,
      selectize = TRUE
    )
  })

  # reactive field for free text input
  output$DataGet <- renderUI({
    textInput(
      "UserDataGet",
      "Find Microbial Genome",
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
    if (input$UserAddGenome == "" | is.na(input$UserAddGenome)) {
      "Search for uniprot tax ID ('224308') or\nstrain name ('Bacillus subtilis 168')"
    } else {
      status_list[["latest"]]
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
          `all` = n(),
          reviewed = sum(reviewed == "reviewed"),
          `hypothetical` = sum(str_detect(protein, "[Uu]nknown|[Hh]ypothetical|[Uu]ncharacteri")),
          `putative` = sum(str_detect(protein, "[Pp]utative")),
          transport = sum(str_detect(protein, "[TT]ransporter")),
          .groups = "drop"
        )
      datatable(df_summary)
    }
  })

  # OUTPUT 2: BARCHART WITH CATEGORIES
  output$categories.ui <- renderUI({
    plotOutput("categories", height = "330px", width = "100%")
  })

  output$categories <- renderPlot(res = 96, {
    plot <- df_selected_genomes() %>%
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
      mutate(proteins = forcats::fct_inorder(proteins)) %>%
      ggplot(aes(x = proteins, y = count, fill = proteins)) +
      geom_col(color = "white") +
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
    plotOutput("localization", height = "450px", width = "100%", )
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
      coord_polar(theta="y") +
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
      lims(x = c(0, 1000)) +
      facet_wrap( ~ organism, nrow = 2) +
      labs(x = "", y = "") +
      current_theme()
    print(plot)
  })

}
