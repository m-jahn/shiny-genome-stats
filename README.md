# Shiny Genome Stats

R Shiny app to show basic statistics and features of microbial genomes

### Getting started

If you want to *run ShinyLib locally*, you need to have R (optionally also Rstudio) and some of its libraries installed:

- `shiny`
- `httr`
- `jsonlite`
- `DT`
- `shinyWidgets`
- `tidyr`
- `dplyr`
- `readr`
- `ggplot2`
- `shinythemes`
- `shinyTree`
- `configr`
- `stringr`

Open `global.R`, `server.R` or `ui.R` in RStudio and push the `Run App` button in Rstudio, done!
You can also run the app from R console, just call `runApp("path-to-ShinyApp")`.

### Input data

- to be added

### Structure

ShinyLib consists of a set of R scripts that determine the functionality.

- `global.R` loads packages, data sets, and `.yml` configuration files
- `server.R` contains the main body of functions. The server obtains input parameters from the GUI and adjusts the graphical output accordingly (changes charts on the fly)
- `ui.R` The GUI contains the interactive modules such as sliders and check boxes
- `R/<helper_functions>.R` - additional functions loaded when necessary, for example for data formatting and plotting

### Author(s)

- Dr. Michael Jahn
  - Affiliation: [Max-Planck-Unit for the Science of Pathogens](https://www.mpusp.mpg.de/) (MPUSP), Berlin, Germany
  - ORCID profile: https://orcid.org/0000-0002-3913-153X
  - github page: https://github.com/m-jahn

