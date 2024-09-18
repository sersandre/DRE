data_name_text_input_ui <- function(id, label, value) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::textInput(
      inputId = ns("name"),
      label = label,
      value = value,
      width = "100%"
    ),
    shiny::uiOutput(
      outputId = ns("validation")
    )
  )
}

data_name_text_input_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      is_empty_r <- shiny::reactive(
        is.null(input$name) || is.na(input$name) || input$name == ""
      )
      
      has_wrong_chars_r <- shiny::reactive({
        !str_detect(input$name, "^[A-Za-z0-9\\-\\s]*$")
      })
      
      output$validation <- shiny::renderUI({
        shiny::validate(
          shiny::need(!is_empty_r(), "Der Name darf nicht leer sein!"),
          shiny::need(!has_wrong_chars_r(), "Der Name darf nur A-Z, a-z, 0-9, '-' und ' ' enthalten!")
        )
        
        NULL
      })
      
      error_r <- shiny::reactive({
        is_empty_r() || has_wrong_chars_r()
      })
      
      return_list <- list(
        name_r = shiny::reactive(input$name),
        error_r = error_r
      )
    }
  )
}