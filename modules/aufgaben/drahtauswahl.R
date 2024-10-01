drahtauswahl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    #actionButtonQW(
    #  inputId = ns("add_histogram"),
    #  label = NULL,
    #  icon = icon("chart-area"),
    #  tooltip = "Öffne Histogramm"
    #),
    actionButtonQW(
      inputId = ns("add_weibullgerade"),
      label = NULL,
      icon = icon("chart-area"),
      tooltip = "Weibullgerade anzeigen"
    ),
    dataTableOutput(ns("probability_table")),
    dataTableOutput(ns("summary_table")),
    dataTableOutput(ns("param_table")),
    checkboxInput(ns("show_data"), "Auswertungsdaten anzeigen", FALSE),
    checkboxInput(ns("show_params"), "Weibullparameter anzeigen", FALSE)
    
  )
}
# Funktion zur Aufbereitung der Rohdaten
refine_data <- function(raw_data, material) {
  refined_data <- raw_data[raw_data$Material == material, ] %>%
    select(c('Biegungsanzahl', 'Status', 'Material'))
  
  rel_data <- reliability_data(
    x = refined_data$Biegungsanzahl,
    status = refined_data$Status
  )
  
  return (rel_data)
}
# Funktion zur Beerchnung der weibull_parameter mittels Rangregression
compute_weibull_params <- function(rel_data, model) {
  median_rank_data <- estimate_cdf(
    x = rel_data,
    methods = model
  )
  
  weibull_params <- rank_regression(
    x = median_rank_data,
    distribution = "weibull"
  )
  return (weibull_params)
}
# Hilfsfunktion zum Hinzufügen weiterer Punkte in Weibullplot
generate_probability_plot <- function(
    plot,
    rel_data,
    model,
    material) {
  median_rank_data <- estimate_cdf(
    x = rel_data,
    methods = model
  )
  plot_ret <- add_trace(
    p = plot,
    x = median_rank_data$x,
    y = median_rank_data$prob,
    type = "scatter",
    name = sprintf("Ausfälle %s", material),
    yaxis = "y2"
  )
  return (plot_ret)
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
      
      # Erstellen des Weibullplots für alle Materialien im geladenen Datensatz
      observeEvent(input$add_weibullgerade, {
        drahtauswahl_raw <- data_r()
        materials <- unique(drahtauswahl_raw$Material)
        plot_data <- data.frame()
        params <- dict()
        est_list <- list()
        names <- list()
        sample <- materials[1]
        
        # Parameterschätzung und Rangregression für alle gegebenen Materialien
        for (material in materials) {
          refined_data <- refine_data(drahtauswahl_raw, material)
          weibull_params <- compute_weibull_params(refined_data, "mr")
          median_data <- estimate_cdf(
            x = refined_data,
            methods = "mr"
          )
          plot_data <- rbind(plot_data, median_data)
          params[[material]] <- weibull_params
          est_list[[material]] <- weibull_params
        }
        # Erstellen des Basis-Weibullplots
        class(est_list) <- c("wt_model_estimation_list")
        base_plot <- plot_prob(
          x = plot_data,
          distribution = "weibull",
          title_main = "Weibull - Wahrscheinlichkeitsnetz",
          title_x = "Biegungsanzahl",
          title_y = "Ausfallwahrscheinlichkeit in %",
          title_trace = "Ausfälle",
          plot_method = "plotly"
        ) %>%
          plot_mod(
            x = est_list,
            title_trace = "Ausgleichsgerade"
          )
          # Einfügen einer Überlagerten y-Achse für korrektes alignment aller Punkte
        plot_output_id <- paste0("weibullplot_", data_selector_return$name_r())
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Weibullplot:", data_selector_return$name_r()),
            value = paste0("weibullplot_", data_selector_return$name_r()),
            plotlyOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        # Rendering des erstellten Weibullplots
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlotly({
            base_plot
          })
        }
      })
      # Rendering der Auswertungstablle mit Wahrscheinlichkeitsschätzungen
      output$summary_table <- renderDataTable({
        
        if(input$show_data){
          drahtauswahl_raw <- data_r()
          materials <- unique(drahtauswahl_raw$Material)
          plot_data <- dict()
          params <- dict()
          sample <- materials[1]
        
          for (material in materials){
            refined_data <- refine_data(drahtauswahl_raw, material)
            prob_data <- estimate_cdf(refined_data, methods = "mr")
            weibull_params <- compute_weibull_params(refined_data, "mr")
            plot_data[[material]] <- prob_data
            params[[material]] <- weibull_params
          }
          sample_frame <- data.frame(Material = sample, plot_data = plot_data[[sample]])
          
          for (i in 2:length(materials)) {
            material <- materials[i]
            new_frame <- data.frame(Material = material, plot_data = plot_data[[material]])
            sample_frame <- rbind(sample_frame, new_frame)
          }
          print(sample_frame)
          datatable(sample_frame)
        }
      })
      # Rendering der Tabelle mit Weibullparametern und median Lebensdauern
      output$param_table <- renderDataTable({
        if (input$show_params){
          drahtauswahl_raw <- data_r()
          materials <- unique(drahtauswahl_raw$Material)
          params <- dict()
          sample <- materials[1]
          
          for (material in materials){
            refined_data <- refine_data(drahtauswahl_raw, material)
            prob_data <- estimate_cdf(refined_data, methods = "mr")
            weibull_params <- compute_weibull_params(refined_data, "mr")
            params[[material]] <- weibull_params
          }
          sample_t <- params[[sample]]$shape_scale_coefficients[1]
          sample_b <- params[[sample]]$shape_scale_coefficients[2]
          sample_lifetime <- predict_quantile(
            p = 0.5, 
            dist_params = params[[sample]]$coefficients,
            distribution = "weibull"
          )
          sample_params <- data.frame(Material = sample,
                                      param_T = sample_t,
                                      param_b = sample_b,
                                      median_lifetime = sample_lifetime)
          
          for (i in 2:length(materials)) {
            material <- materials[i]
            param_t <- params[[material]]$shape_scale_coefficients[1]
            param_b <- params[[material]]$shape_scale_coefficients[2]
            lifetime <- predict_quantile(
              p = 0.5,
              dist_params = params[[material]]$coefficients,
              distribution = "weibull"
            )
            
            new_params <- data.frame(Material = material,
                                     param_T = param_t,
                                     param_b = param_b,
                                     median_lifetime = lifetime)
            sample_params <- rbind(sample_params, new_params)
          }
          datatable(sample_params)
        }
      })
      
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}