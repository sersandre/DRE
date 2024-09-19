# Falls es an dieser Stelle Fehlermeldungen gibt, müssen die Packages mit
# install.packages installiert werden
library(shiny)
# Dashboard
library(shinydashboard)
# tidyverse
library(tidyverse)
# DT-Tabellen in shiny
library(DT)
# plotly-Plots
library(plotly)
# wird für source_directory benötigt
library(R.utils)
# Import von .xls- und .xlsx-Dateien
library(readxl)
# Import von .csv-Dateien
library(readr)
# Bearbeiten von Strings
library(stringr)
# Objektorientiertes System; z.B. TabBox ist ein R6-Objekt
library(R6)
# UI
library(shinyWidgets)
# Tooltips
library(shinyBS)
# weibulltools v. 2.0.0
library(weibulltools)
# R dictionary implementation
library(dict)

# Source source_directory.R
source("./modules/predefined/source_directory.R", encoding = "UTF-8")

# Nutze source_directory, um gesamten Ordner zu sourcen; setze verbose = FALSE,
# um keine Mitteilungen in der Konsole zu sehen
source_directory(
  "./modules", encoding = "UTF-8", modifiedOnly = FALSE, chdir = TRUE, 
  verbose = TRUE, envir = globalenv()
)

# Erzeuge einen Viewer, in dem Plots und Tabellen in einzelnen Tabs dargestellt
# werden können
viewer <- TabBox$new(
  id = "viewer",
  title = "Viewer",
  width = 12
)

# Scrollen in zu breiten DT-Tabellen
options(DT.options = list(scrollX = TRUE))

ui <- div(
  tags$head(
    # Include custom css styles
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  dashboardPage(
    dashboardHeader(
      title = "DRE-App"
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          text = "Import",
          tabName = "import"
        ),
        menuItem(
          text = "Auswertungen",
          menuSubItem(
            text = "Drahtauswahl",
            tabName = "drahtauswahl"
          ),
          menuSubItem(
            text = "Raffungstest",
            tabName = "raffungstest"
          )
        )
      )
    ),
    dashboardBody(
      fluidRow(
        column(
          width = 6,
          tabItems(
            tabItem(
              tabName = "import",
              box(
                title = "Import",
                width = 12,
                excel_csv_file_input_ui(
                  id = "id_excel_csv_file_input"
                )
              )
            ),
            tabItem(
              tabName = "drahtauswahl",
              box(
                title = "Drahtauswahl",
                width = 12,
                drahtauswahl_ui(
                  id = "id_drahtauswahl"
                )
              )
            ),
            tabItem(
              tabName = "raffungstest",
              box(
                title = "Raffungstest",
                width = 12,
                raffungstest_ui(
                  id = "id_raffungstest"
                )
              )
            )
          )
        ),
        column(
          width = 6,
          # Container, in dem die Inhalte des Viewers dargestellt werden
          viewer$tabBox()
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Verknüpfe Viewer mit der session
  viewer$set_session(session)
  
  # Erzeuge eine Liste, die allen Modulen als Argument übergeben wird
  .values <- list(
    data_storage = ObjectStorage$new(),
    viewer = viewer 
  )
  
  # Führe die jeweilige module_server Funktion aus: 
  excel_csv_file_input_server(
    id = "id_excel_csv_file_input",
    .values = .values
  )
  
  drahtauswahl_server(
    id = "id_drahtauswahl", 
    .values = .values
  )
  
  raffungstest_server(
    id = "id_raffungstest", 
    .values = .values
  )
}

# Erzeuge die App
shinyApp(ui, server)