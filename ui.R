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

        fluidRow(column(
          width = 6,
          uiOutput("DataChoice")
        ), column(
          width = 6,
          uiOutput("DataGet"),
          actionButton(
            "UserAddGenome",
            "Add",
            class = "btn-success"
          ),
          br(),
          uiOutput("AddStatus")
        )),

        # SELECT PLOT OPTIONS
        # -------------------
        hr(),
        h4("PLOT OPTIONS")
      ),
      mainPanel(DTOutput("genomeSummary"))
    )
  ),

  # THE ABOUT PAGE
  tabPanel(
    "About",

    # help and info box
    column(
      width = 4,
      #helpbox(width = 12),
      #fundbox(width = 12),), column(width = 8,
    )
  )
)
