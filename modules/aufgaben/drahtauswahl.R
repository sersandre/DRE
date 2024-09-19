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
    ),
    actionButtonQW(
      inputId = ns("add_weibullgerade"),
      label = NULL,
      icon = icon("chart-area"),
      tooltip = "Weibullgerade anzeigen"
    )
  )
}
refine_data <- function(raw_data, material) {
  refined_data <- raw_data[raw_data$Material == material, ] %>%
    select(c('Biegungsanzahl', 'Status', 'Material'))
  
  rel_data <- reliability_data(
    x = refined_data$Biegungsanzahl,
    status = refined_data$Status
  )
  
  return (rel_data)
}
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
      
      ## Erstellen des Weibullplots für alle Materialien im geladenen Datensatz
      observeEvent(input$add_weibullgerade, {
        drahtauswahl_raw <- data_r()
        materials <- unique(drahtauswahl_raw$Material)
        plot_data <- dict()
        params <- dict()
        est_list <- list()
        names <- list()
        sample <- materials[1]
        
        for (material in materials) {
          refined_data <- refine_data(drahtauswahl_raw, material)
          weibull_params <- compute_weibull_params(refined_data, "mr")
          plot_data[[material]] <- refined_data
          params[[material]] <- weibull_params
          est_list[[material]] <- weibull_params
        }
        ## create basis plotly plot
        class(est_list) <- c("wt_model_estimation_list")
        median_rank <- estimate_cdf(
          x = plot_data[[sample]],
          methods = "mr"
        )
        base_plot <- plot_prob(
          x = median_rank,
          distribution = "weibull",
          title_main = "Weibull - Wahrscheinlichkeitsnetz",
          title_x = "Biegungsanzahl",
          title_y = "Ausfallwahrscheinlichkeit in %",
          title_trace = "Ausfälle Al",
          plot_method = "plotly"
        ) %>%
          plot_mod(
            x = est_list
          ) %>%
          layout(yaxis2 = list(overlaying = "y",
                               showgrid = FALSE,
                               side = "right",
                               showline = FALSE,
                               zeroline = FALSE,
                               showticklabels = FALSE))
        for (i in 2:length(materials)) {
          material <- materials[i]
          base_plot <- generate_probability_plot(base_plot,
                                                 plot_data[[material]],
                                                 "mr",
                                                 material
                                                 )
        }
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
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlotly({
            base_plot
          })
        }
      })
      
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}