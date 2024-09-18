drahtauswahl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    actionButtonQW(
      inputId = ns("add_histogram"),
      label = NULL,
      icon = icon("chart-area"),
      tooltip = "Öffne Histogramm"
    )
  )
}

drahtauswahl_server <- function(id, .values) {
  
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      data_r <- reactive({
        data_selector_return$data_r()
      })
      
      # Beispiel: Anfang
      histogram_r <- reactive({
        ggplot(data = mtcars, mapping = aes(x = mpg)) +
          geom_histogram(bins = nclass.Sturges(mpg)) +
          theme_bw()
      })
      
      observeEvent(input$add_histogram, {
        plot_output_id <- paste0("histogram_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Histogram:", data_selector_return$name_r()),
            value = paste0("histogram_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlot({
            histogram_r()
          })
        }
      })
      
      # Beispiel: Ende
      
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}