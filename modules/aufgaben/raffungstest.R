raffungstest_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    checkboxInput(
      ns("combine_plots"), 
      "Weibull und Wöhlerlinie gemeinsam anzeigen", 
      value = FALSE
    ),
    actionButtonQW(
      inputId = ns("add_weibull_to_viewer"),
      label = "Weibull Gerade anzeigen",
      icon = icon("chart-line")
    ),
    actionButtonQW(
      inputId = ns("add_woehler_to_viewer"),
      label = "Wöhlerlinie anzeigen",
      icon = icon("chart-line")
    ),
    actionButtonQW(
      inputId = ns("add_combined_to_viewer"),
      label = "Kombinierte Plots anzeigen",
      icon = icon("chart-line")
    ),
    
    br(), br(),
    
    # Weibull plot output
    plotlyOutput(ns("weibull_plot")),
    
    # Wöhler plot output
    plotlyOutput(ns("woehler_plot")),
    
    # Checkboxes to show/hide tables
    checkboxInput(ns("show_summary_table"), "Weibull-Parameter Tabelle anzeigen", value = FALSE),
    checkboxInput(ns("show_raffungsfaktoren_table"), "Raffungsfaktoren Tabelle anzeigen", value = FALSE),
    checkboxInput(ns("show_probability_table"), "Wahrscheinlichkeitstabelle anzeigen", value = FALSE),
    
    # Output containers for the tables (hidden by default)
    conditionalPanel(
      condition = paste0("input['", ns("show_summary_table"), "'] == true"),
      dataTableOutput(ns("summary_table"))
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("show_raffungsfaktoren_table"), "'] == true"),
      dataTableOutput(ns("raffungsfaktoren_table"))
    ),
    
    conditionalPanel(
      condition = paste0("input['", ns("show_probability_table"), "'] == true"),
      dataTableOutput(ns("probability_table"))
    )
  )
}

# Server-Logik für Raffungstest
raffungstest_server <- function(id, .values) {
  
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      # Reaktive Daten für den Winkel 45°
      data_45_r <- reactive({
        data <- data_selector_return$data_r()
        data[data$Winkel == 45, ]
      })
      
      # Reaktive Daten für den Winkel 90°
      data_90_r <- reactive({
        data <- data_selector_return$data_r()
        data[data$Winkel == 90, ]
      })
      
      # Reaktive Daten für den Winkel 180°
      data_180_r <- reactive({
        data <- data_selector_return$data_r()
        data[data$Winkel == 180, ]
      })
      
      # Weibull-Parameter für die Winkel 45°, 90°, 180°
      weibull_params_r <- reactive({
        data_combined <- rbind(data_45_r(), data_90_r(), data_180_r())
        angles <- unique(data_combined$Winkel)
        params <- list()
        
        for (angle in angles) {
          refined_data <- data_combined[data_combined$Winkel == angle, ]
          rel_data <- reliability_data(
            x = refined_data$Biegungsanzahl,
            status = refined_data$Status
          )
          weibull_params <- ml_estimation(x = rel_data, distribution = "weibull")
          params[[as.character(angle)]] <- weibull_params
        }
        
        return(params)
      })
      
      # Weibull-Wahrscheinlichkeitsplot für alle Winkel
      weibull_plot_r <- reactive({
        data_combined <- rbind(data_45_r(), data_90_r(), data_180_r())
        angles <- unique(data_combined$Winkel)
        plot_data <- data.frame()
        params <- weibull_params_r()
        est_list <- list()
        
        for (angle in angles) {
          refined_data <- data_combined[data_combined$Winkel == angle, ]
          rel_data <- reliability_data(
            x = refined_data$Biegungsanzahl,
            status = refined_data$Status
          )
          median_data <- estimate_cdf(x = rel_data, methods = "mr")
          plot_data <- rbind(plot_data, median_data)
          est_list[[as.character(angle)]] <- params[[as.character(angle)]]
        }
        
        # Basisplot erstellen
        class(est_list) <- c("wt_model_estimation_list")
        base_plot <- plot_prob(
          x = plot_data,
          distribution = "weibull",
          title_main = "Weibull - Wahrscheinlichkeitsnetz für alle Winkel",
          title_x = "Biegungsanzahl",
          title_y = "Ausfallwahrscheinlichkeit in %",
          title_trace = "Ausfälle",
          plot_method = "plotly"
        ) %>%
          plot_mod(x = est_list)
        
        return(base_plot)
      })
      
      # Erstellung des Wöhler-Plots mit den Quantilen
      woehler_plot_r <- reactive({
        
        data_combined <- rbind(data_45_r(), data_90_r(), data_180_r())
        params <- weibull_params_r()
        
        # Berechnung der Quantile für die Winkel
        quantiles_5 <- c(
          predict_quantile(p = 0.05, dist_params = params[["45"]]$coefficients, distribution = "weibull"),
          predict_quantile(p = 0.05, dist_params = params[["90"]]$coefficients, distribution = "weibull"),
          predict_quantile(p = 0.05, dist_params = params[["180"]]$coefficients, distribution = "weibull")
        )
        
        quantiles_50 <- c(
          predict_quantile(p = 0.50, dist_params = params[["45"]]$coefficients, distribution = "weibull"),
          predict_quantile(p = 0.50, dist_params = params[["90"]]$coefficients, distribution = "weibull"),
          predict_quantile(p = 0.50, dist_params = params[["180"]]$coefficients, distribution = "weibull")
        )
        
        quantiles_95 <- c(
          predict_quantile(p = 0.95, dist_params = params[["45"]]$coefficients, distribution = "weibull"),
          predict_quantile(p = 0.95, dist_params = params[["90"]]$coefficients, distribution = "weibull"),
          predict_quantile(p = 0.95, dist_params = params[["180"]]$coefficients, distribution = "weibull")
        )
        
        # Plot für Wöhlerlinie erstellen
        woehler_plot <- plot_woehler_layout(
          x = data_combined$Biegungsanzahl,
          y = data_combined$Winkel,
          title_main = "Wöhlerlinie für alle Winkel",
          title_x = "Biegungsanzahl",
          title_y = "Belastung",
          title_trace = "Winkel",
          plot_method = "plotly"
        ) %>%
          add_trace(
            x = quantiles_5,
            y = c(45, 90, 180),
            type = "scatter",
            mode = "markers+lines",
            name = "5% Ausfallwkt.",
            line = list(color = "red", dash = "dot"),
            marker = list(color = "red", size = 10)
          ) %>%
          add_trace(
            x = quantiles_50,
            y = c(45, 90, 180),
            type = "scatter",
            mode = "markers+lines",
            name = "50% Ausfallwkt.",
            line = list(color = "green", dash = "solid"),
            marker = list(color = "green", size = 10)
          ) %>%
          add_trace(
            x = quantiles_95,
            y = c(45, 90, 180),
            type = "scatter",
            mode = "markers+lines",
            name = "95% Ausfallwkt.",
            line = list(color = "blue", dash = "dot"),
            marker = list(color = "blue", size = 10)
          ) %>%
          layout(
            yaxis = list(type = "log", title = "Belastung")
          )
        
        return(woehler_plot)
      })
      
      # Kombiniertes Darstellen von Weibull- und Wöhler-Plot
      combined_plot_r <- reactive({
        if (input$combine_plots) {
          subplot(
            weibull_plot_r(), 
            woehler_plot_r(), 
            nrows = 2, 
            shareX = TRUE
          ) %>%
            layout(title = "Weibull und Wöhlerlinie kombiniert")
        } else {
          return(NULL)
        }
      })
      
      # Darstellung des Weibull-Plots
      output$weibull_plot <- renderPlotly({
        if (input$combine_plots) {
          combined_plot_r()
        } else {
          weibull_plot_r()
        }
      })
      
      # Darstellung des Wöhler-Plots
      output$woehler_plot <- renderPlotly({
        if (!input$combine_plots) {
          woehler_plot_r()
        }
      })
      
      # Weibull Gerade im Viewer anzeigen
      observeEvent(input$add_weibull_to_viewer, {
        plot_output_id <- paste0("weibull_viewer_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Weibull Gerade:", data_selector_return$name_r()),
            value = paste0("weibull_viewer_", data_selector_return$name_r()),
            plotlyOutput(ns(plot_output_id), height = "600px", width = "100%")
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlotly({
            weibull_plot_r()
          })
        }
      })
      
      # Wöhler Linie im Viewer anzeigen
      observeEvent(input$add_woehler_to_viewer, {
        plot_output_id <- paste0("woehler_viewer_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Wöhler Linie:", data_selector_return$name_r()),
            value = paste0("woehler_viewer_", data_selector_return$name_r()),
            plotlyOutput(ns(plot_output_id), height = "600px", width = "100%")
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlotly({
            woehler_plot_r()
          })
        }
      })
      
      # Kombinierte Plots im Viewer anzeigen
      observeEvent(input$add_combined_to_viewer, {
        plot_output_id <- paste0("combined_viewer_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Kombinierte Plots:", data_selector_return$name_r()),
            value = paste0("combined_viewer_", data_selector_return$name_r()),
            plotlyOutput(ns(plot_output_id), height = "600px", width = "100%")
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlotly({
            combined_plot_r()
          })
        }
      })
      
      # Tabelle mit Weibull-Parametern für alle Winkel
      output$summary_table <- renderDataTable({
        params <- weibull_params_r()
        datatable(data.frame(
          Winkel = c("45°", "90°", "180°"),
          T_Shape = c(params[["45"]]$shape_scale_coefficients[1], 
                      params[["90"]]$shape_scale_coefficients[1], 
                      params[["180"]]$shape_scale_coefficients[1]),
          b_Scale = c(params[["45"]]$shape_scale_coefficients[2], 
                      params[["90"]]$shape_scale_coefficients[2], 
                      params[["180"]]$shape_scale_coefficients[2])
        ))
      })
      
      # Raffungsfaktoren-Tabelle
      output$raffungsfaktoren_table <- renderDataTable({
        params <- weibull_params_r()
        data.frame(
          Verhältnis = c("T45/T90", "T45/T180", "T90/T180"),
          Raffungsfaktor = c(
            params[["45"]]$shape_scale_coefficients[1] / params[["90"]]$shape_scale_coefficients[1],
            params[["45"]]$shape_scale_coefficients[1] / params[["180"]]$shape_scale_coefficients[1],
            params[["90"]]$shape_scale_coefficients[1] / params[["180"]]$shape_scale_coefficients[1]
          )
        )
      })
      
      # Wahrscheinlichkeitstabelle für alle Winkel
      output$probability_table <- renderDataTable({
        # Wahrscheinlichkeitsschätzungen für 45°
        rel_data_45 <- reliability_data(
          x = data_45_r()$Biegungsanzahl,
          status = data_45_r()$Status
        )
        prob_data_45 <- estimate_cdf(x = rel_data_45, methods = "mr")
        
        # Wahrscheinlichkeitsschätzungen für 90°
        rel_data_90 <- reliability_data(
          x = data_90_r()$Biegungsanzahl,
          status = data_90_r()$Status
        )
        prob_data_90 <- estimate_cdf(x = rel_data_90, methods = "mr")
        
        # Wahrscheinlichkeitsschätzungen für 180°
        rel_data_180 <- reliability_data(
          x = data_180_r()$Biegungsanzahl,
          status = data_180_r()$Status
        )
        prob_data_180 <- estimate_cdf(x = rel_data_180, methods = "mr")
        
        # Kombinierte Wahrscheinlichkeitstabelle für alle Winkel
        datatable(rbind(
          data.frame(Winkel = "45°", prob_data_45),
          data.frame(Winkel = "90°", prob_data_90),
          data.frame(Winkel = "180°", prob_data_180)
        ))
      })
      
      # Logik für den Daten-Selektor
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}
