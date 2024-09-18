excel_csv_file_input_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      id = ns("file-input-container"),
      fileInput(
        inputId = ns("file"),
        label = "Lade eine Datei hoch:",
        placeholder = "Klicke hier."
      )
    ),
    uiOutput(
      outputId = ns("names_data")
    ),
    uiOutput(
      outputId = ns("add_preview")
    ),
    uiOutput(
      outputId = ns("add_data")
    )
  )
}

excel_csv_file_input_server <- function(id, .values) {
  
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      envir <- new.env()
      envir$data_name_returns <- list()
      
      file_rv <- reactiveVal(NULL)
      
      observeEvent(input$file, {
        file_rv(input$file)
      })
      
      file_r <- reactive({
        shiny::req(file_rv())
      })
      
      data_name_text_input_counter_rv <- reactiveVal(0)
      
      output$names_data <- renderUI({
        if (!file_type_not_supported_r()) {
          ui <- map(seq_len(sheets_r()), function(i) {
            if (i > data_name_text_input_counter_rv()) {
              data_name_text_input_counter_rv(
                data_name_text_input_counter_rv() + 1
              )
              
              envir$data_name_returns[[i]] <- data_name_text_input_server(
                id = "data_name_text_input" %_% i,
                .values = .values
              )
            }
            
            data_name_text_input_ui(
              id = ns("data_name_text_input" %_% i),
              label = paste0("Name des ", i, ". Tabellenblattes"),
              value = excel_sheets(file_r()$datapath)[[i]]
            )
          }) 
        } else {
          ui <- NULL
        }
        ui
      })
      
      output$add_preview <- renderUI({
        if (!file_type_not_supported_r()) {
          actionButtonQW(
            inputId = ns("add_preview"),
            label = "Vorschau"
          )
        }
      })
      
      output$add_data <- renderUI({
        if (!error_r()) {
          ui <- actionButtonQW(
            inputId = ns("add_data"),
            label = "Füge Datensatz hinzu."
          )
        } else {
          ui <- uiOutput(
            outputId = ns("ui_error")
          )
        }
      })
      
      file_type_r <- reactive({
        path <- file_r()$datapath
        split_path <- str_split(path, pattern = "\\.")
        # Extrahiere Dateiendung
        split_path[[1]][length(split_path[[1]])]
      })
      
      sheets_r <- reactive({
        if (file_type_r() == "xlsx" || file_type_r() == "xls") {
          len <- length(excel_sheets(file_r()$datapath))
        } else {
          len <- 1 
        }
        
        len
      })
      
      file_type_not_supported_r <- reactive({
        !(file_type_r() %in% c("xlsx", "xls", "csv"))
      })
      
      data_name_error_r <- reactive({
        error <- purrr::map_lgl(seq_len(sheets_r()), function(i) {
          envir$data_name_returns[[i]]$error_r()
        })
        
        any(error)
      })
      
      data_names_r <- reactive({
        if (!file_type_not_supported_r()) {
          map_chr(seq_len(sheets_r()), function(i) {
            envir$data_name_returns[[i]]$name_r()
          })
        } else {
          character()
        }
      })
      
      name_in_use_r <- reactive({
        any(data_names_r() %in% .values$data_storage$get_names())
      })
      
      which_names_in_use <- reactive({
        data_names_r()[which(data_names_r() %in% .values$data_storage$get_names())]
      })
      
      error_r <- reactive({
        file_type_not_supported_r() ||
          data_name_error_r() ||
          name_in_use_r()
      })
      
      data_preview_r <- reactive({
        if (!file_type_not_supported_r()) {
          data <- list()
          type <- file_type_r()
          
          if (type == "xlsx" || type == "xls") {
            for (i in 1:sheets_r()) {
              data[[i]] <- read_excel(
                path = file_r()$datapath,
                sheet = i,
                col_names = TRUE
              )
            }
            data <- map(seq_len(sheets_r()), function(sheet) {
              read_excel(
                path = file_r()$datapath,
                sheet = sheet,
                col_names = TRUE
              )
            })
          } else if (type == "csv") {
            data[[envir$data_name_returns[[1]]$name_r()]] <- read_csv2(
              file = file_r()$datapath
            )
          }
          
          return(data)
        }
      })
      
      data_r <- reactive({
        if (!error_r()) {
          data <- list()
          type <- file_type_r()
          if (type == "xlsx" || type == "xls") {
            for (i in seq_len(sheets_r())) {
              name <- envir$data_name_returns[[i]]$name_r()
              
              data[[name]] <- read_excel(
                path = file_r()$datapath,
                sheet = i,
                col_names = TRUE
              )
            }
          } else if (type == "csv") {
            data[[envir$data_name_returns[[1]]$name_r()]] <- read_csv2(
              file = file_r()$datapath
            )
          }
          
          return(data)
        }
      })
      
      observeEvent(input$add_preview, {
        .values$viewer$append_tab(
          tab = tabPanel(
            title = "Vorschau",
            value = "preview",
            uiOutput(
              outputId = ns("select_preview_sheet")
            ),
            dataTableOutput(
              outputId = ns("preview_data")
            )
          )
        )
      })
      
      output$select_preview_sheet <- renderUI({
        if (sheets_r() > 1) {
          selectInput(
            inputId = ns("select_preview_sheet"),
            label = "Wähle das anzuzeigende Tabellenblatt aus:",
            choices = seq_len(sheets_r())
          )
        }
      })
      
      output$preview_data <- renderDataTable({
        if (sheets_r() > 1) {
          data_preview_r()[[as.numeric(req(input$select_preview_sheet))]]
        } else {
          data_preview_r()[[1]]
        }
      })
      
      output$ui_error <- renderUI({
        if (file_type_not_supported_r()) {
          not_supported <- paste0(
            "Dateiendung .", file_type_r(), " wird nicht untersützt. "
          )
        } else {
          not_supported <- NULL
        }
        
        if (name_in_use_r()) {
          name_in_use <- paste0(
            "Es existieren bereits Datensätze mit den Namen ", 
            paste(which_names_in_use(), collapse = ", "), ". "
          )
        } else {
          name_in_use <- NULL
        }
        
        ui <- tagList(
          not_supported,
          name_in_use
        )
      })
      
      observeEvent(input$add_data, {
        # Setze fileInput zurück
        removeUI(
          selector = paste0("#", ns("file-input-container"), " *"),
          multiple = TRUE
        )
        
        insertUI(
          selector = paste0("#", ns("file-input-container")),
          where = "afterBegin",
          ui = fileInput(
            inputId = ns("file"),
            label = "Lade eine Datei hoch:",
            placeholder = "Klicke hier."
          )
        )
        
        data <- data_r()
        walk(seq_len(sheets_r()), function(i) {
          name <- envir$data_name_returns[[i]]$name_r()
          
          object <- DataObject$new(
            name = name,
            value = data[[i]]
          )
          
          .values$data$add_object(
            object
          )
          
          showNotification(
            ui = paste0("Datensatz '", name, "' erfolgreich hinzugefügt!"),
            type = "message"
          )
        })
        
        # Reset file_rv
        file_rv(NULL)
      })
    }
  )
}