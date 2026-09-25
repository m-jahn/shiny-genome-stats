#
# SHINY UI
# ***********************************************
# Define user interface for application
ui <- navbarPage(
  # Title on NavBar Header
  title = "Shiny Genome Stats - comparative statistics about bacterial genomes",

  # Use one of different shiny themes
  theme = shinytheme("cosmo"),

  # Main tab
  tabPanel(
    "App",

    # Sidebar
    sidebarLayout(
      position = c("left", "right"),
      fluid = TRUE,
      sidebarPanel(
        position = "left",
        width = 4,

        # SELECT DATA
        # -------------------
        # select data file
        h4("DATA OPTIONS"),
        fluidRow(
          column(
            width = 12,
            uiOutput("DataChoice")
          )
        ),

        # SELECT PLOT OPTIONS
        # -------------------
        hr(),
        h4("PLOT OPTIONS"),
        fluidRow(
          column(
            width = 4,
            uiOutput("UserTheme")
          ),
          column(
            width = 4,
            selectInput(
              "UserColorPalette",
              "Palette",
              choices = c(
                "ggplot", "rainbow", "hawaii", "sunset", "batlow", "terrain",
                "dark_mint", "viridis", "plasma", "purple_yellow", "yellow_green",
                "yellow_red", "pink_yellow"
              ),
              selected = "hawaii"
            )
          ),
          column(
            width = 4,
            uiOutput("UserGrouping")
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              "UserFrequency",
              "Frequency",
              choices = c("relative", "absolute"),
              selected = "absolute"
            )
          ),
          column(
            width = 4,
            selectInput(
              "UserTypeGenomeRegions",
              "Genome Regions",
              choices = c("piechart", "barchart"),
              selected = "piechart"
            )
          ),
          column(
            width = 4,
            selectInput(
              "UserTypeGenomeFeatures",
              "Genome Features",
              choices = c("piechart", "barchart"),
              selected = "barchart"
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              "UserTypeGenes",
              "Genes",
              choices = c("piechart", "barchart"),
              selected = "barchart"
            )
          ),
          column(
            width = 4,
            selectInput(
              "UserTypeStartcodons",
              "Start Codons",
              choices = c("piechart", "barchart"),
              selected = "barchart"
            )
          ),
          column(
            width = 4,
            selectInput(
              "UserTypeStopcodons",
              "Stop Codons",
              choices = c("piechart", "barchart"),
              selected = "barchart"
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            sliderInput(
              "UserNRows",
              "Number of rows",
              min = 0,
              max = 20,
              value = 2,
              step = 1
            )
          ),
          column(
            width = 6,
            sliderInput(
              "UserNCols",
              "Number of columns",
              min = 0,
              max = 20,
              value = 5,
              step = 1
            )
          )
        ),

        # SUMMARY TABLE
        # -------------------
        hr(),
        h4("SUMMARY TABLE"),
        DTOutput("genomeSummary"),

        # HELP BOX
        # -------------------
        hr(),
        fluidRow(
          helpbox(width = 12)
        )
      ),

      # MAIN PLOT AREA
      mainPanel(
        column(
          width = 12,
          wellPanel(
            h4("GENOME ORGANIZATION"),
            uiOutput("genome_info.ui")
          ),
          wellPanel(
            h4("GENOME REGIONS"),
            uiOutput("genome_regions.ui"),
            h4("GENOME FEATURES"),
            uiOutput("genome_features.ui")
          ),
          wellPanel(
            h4("GENES"),
            uiOutput("protein_length.ui"),
            h4("START CODONS"),
            uiOutput("startcodons.ui"),
            h4("STOP CODONS"),
            uiOutput("stopcodons.ui")
          )
        )
      )
    )
  ),

  # THE ABOUT PAGE
  tabPanel(
    "About",

    # help and info box
    fluidRow(
      helpbox(width = 6),
      methbox(width = 6)
    ),
    fluidRow(
      fundbox(width = 6)
    )
  )
)
