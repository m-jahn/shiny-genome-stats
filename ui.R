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
            width = 6,
            uiOutput("DataChoice")
          ),
          column(
            width = 6,
            uiOutput("DataSearch"),
            actionButton(
              "UserSearchGenome",
              "Search",
              class = "btn-success"
            ),
            br(),
            br(),
            uiOutput("DataSelection"),
            actionButton(
              "UserAddGenome",
              " Add ",
              class = "btn-success"
            ),
            br(),
            br(),
            p(strong('Status')),
            uiOutput("AddStatus"),
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
              choices = c("ggplot", "rainbow", "hawaii", "sunset", "batlow", "terrain",
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
            sliderInput(
              "UserMaxLength",
              "Max length in aa",
              min = 0,
              max = 2000,
              value = 1000,
              step = 100
            )
          ),
          column(
            width = 4,
            sliderInput(
              "UserTopPathways",
              "Top pathways",
              min = 0,
              max = 30,
              value = 20,
              step = 1
            )
          ),
          column(
            width = 4,
            sliderInput(
              "UserTopBioProcess",
              "Top GO terms",
              min = 0,
              max = 30,
              value = 20,
              step = 1
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              "UserFrequency",
              "Frequency",
              choices = c("relative", "absolute"),
              selected = "relative"
            )
          ),
          column(
            width = 4,
            selectInput(
              "UserPlotType",
              "Plot type",
              choices = c("piechart", "barchart"),
              selected = "piechart"
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
            h4("ANNOTATED PROTEINS"),
            uiOutput("categories.ui")
          ),
          wellPanel(
            fluidRow(
              column(
                width = 6,
                h4("LOCALIZATION"),
                uiOutput("localization.ui")
              ),
              column(
                width = 6,
                h4("LENGTH (AA)"),
                uiOutput("protein_length.ui")
              )
            )
          ),
          wellPanel(
            fluidRow(
              column(
                width = 12,
                h4("KEGG PATHWAYS"),
                uiOutput("kegg.ui")
              )
            )
          ),
          wellPanel(
            fluidRow(
              column(
                width = 12,
                h4("GENE ONTOLOGY - BIOLOGICAL FUNCTION"),
                uiOutput("goterms.ui")
              )
            )
          ),
          wellPanel(
            fluidRow(
              column(
                width = 12,
                h4("GENOMIC FEATURES"),
                uiOutput("genome_info.ui")
              )
            )
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
