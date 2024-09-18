raffungstest_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    )
  )
}

raffungstest_server <- function(id, .values) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      data_r <- reactive({
        data_selector_return$data_r()
      })
      
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}