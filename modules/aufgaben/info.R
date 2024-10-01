info_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    dataTableOutput(ns("summary_table"))
  )
}
info_server <- function(id, .values) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$summary_table <- renderDataTable({})
    }
  )
}