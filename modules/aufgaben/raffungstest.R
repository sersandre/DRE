raffungstest_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    checkboxInput(ns("combine_plots"), "Wahrscheinlichkeitsnetz und Wöhlerlinie anzeigen", FALSE),
    plotOutput(ns("weibull_plot")),
    plotOutput(ns("woehler_plot")), 
    dataTableOutput(ns("summary_table")),
    dataTableOutput(ns("raffungsfaktoren_table")),
    dataTableOutput(ns("probability_table"))
  )
}

raffungstest_server <- function(id, .values) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      # Reaktive Daten für alle Biegewinkel
      data_45_r <- reactive({
        data <- data_selector_return$data_r()
        data[data$Winkel == 45, ]
      })
      
      data_90_r <- reactive({
        data <- data_selector_return$data_r()
        data[data$Winkel == 90, ]
      })
      
      data_180_r <- reactive({
        data <- data_selector_return$data_r()
        data[data$Winkel == 180, ]
      })
      
      # Weibull-Parameter für jeden Winkel berechnen
      weibull_params_45_r <- reactive({
        rel_data_45 <- reliability_data(
          x = data_45_r()$Biegungsanzahl,
          status = data_45_r()$Status
        )
        ml_estimation(x = rel_data_45, distribution = "weibull")
      })
      
      weibull_params_90_r <- reactive({
        rel_data_90 <- reliability_data(
          x = data_90_r()$Biegungsanzahl,
          status = data_90_r()$Status
        )
        ml_estimation(x = rel_data_90, distribution = "weibull")
      })
      
      weibull_params_180_r <- reactive({
        rel_data_180 <- reliability_data(
          x = data_180_r()$Biegungsanzahl,
          status = data_180_r()$Status
        )
        ml_estimation(x = rel_data_180, distribution = "weibull")
      })
      
      # Median-Lebensdauer für jeden Winkel berechnen
      median_lifetime_45_r <- reactive({
        predict_quantile(
          p = 0.5,
          dist_params = weibull_params_45_r()$coefficients,
          distribution = "weibull"
        )
      })
      
      median_lifetime_90_r <- reactive({
        predict_quantile(
          p = 0.5,
          dist_params = weibull_params_90_r()$coefficients,
          distribution = "weibull"
        )
      })
      
      median_lifetime_180_r <- reactive({
        predict_quantile(
          p = 0.5,
          dist_params = weibull_params_180_r()$coefficients,
          distribution = "weibull"
        )
      })
      
      # Weibull-Wahrscheinlichkeitsplot erstellen (für alle Winkel)
      weibull_plot_r <- reactive({
        plot <- NULL
        
        # Weibull Plot für 45°
        rel_data_45 <- reliability_data(
          x = data_45_r()$Biegungsanzahl,
          status = data_45_r()$Status
        )
        median_rank_data_45 <- estimate_cdf(x = rel_data_45, methods = "mr")
        weibull_params_45 <- weibull_params_45_r()
        
        plot <- plot_prob(
          x = median_rank_data_45,
          title_main = "Weibull - Ausfallwahrscheinlichkeiten für alle Winkel",
          title_x = "Biegungsanzahl",
          title_y = "Ausfallwahrscheinlichkeit in %",
          title_trace = "45°"
        ) %>%
          plot_mod(
            x = weibull_params_45,
            title_trace = "Winkel 45°"
          )
        
        # Weibull Plot für 90° hinzufügen
        rel_data_90 <- reliability_data(
          x = data_90_r()$Biegungsanzahl,
          status = data_90_r()$Status
        )
        median_rank_data_90 <- estimate_cdf(x = rel_data_90, methods = "mr")
        weibull_params_90 <- weibull_params_90_r()
        
        plot <- plot %>%
          add_trace(
            x = median_rank_data_90$x,
            y = median_rank_data_90$prob,
            type = "scatter",
            name = "90°"
          ) %>%
          plot_mod(
            x = weibull_params_90,
            title_trace = "Winkel 90°"
          )
        
        # Weibull Plot für 180° hinzufügen
        rel_data_180 <- reliability_data(
          x = data_180_r()$Biegungsanzahl,
          status = data_180_r()$Status
        )
        median_rank_data_180 <- estimate_cdf(x = rel_data_180, methods = "mr")
        weibull_params_180 <- weibull_params_180_r()
        
        plot <- plot %>%
          add_trace(
            x = median_rank_data_180$x,
            y = median_rank_data_180$prob,
            type = "scatter",
            name = "180°"
          ) %>%
          plot_mod(
            x = weibull_params_180,
            title_trace = "Winkel 180°"
          )
        
        plot
      })
      
      # Wöhler-Linie erstellen
      woehler_plot_r <- reactive({
        ggplot(data = rbind(data_45_r(), data_90_r(), data_180_r()), aes(x = Biegungsanzahl, y = Status)) +
          geom_point() +
          labs(title = "Wöhler-Linie", x = "Biegungsanzahl", y = "Belastung") +
          theme_minimal()
      })
      
      # Weibull-Plot und Wöhlerlinie zusammen anzeigen, falls angekreuzt
      combined_plot_r <- reactive({
        if (input$combine_plots) {
          plot <- weibull_plot_r() +
            geom_line(data = rbind(data_45_r(), data_90_r(), data_180_r()), aes(x = Biegungsanzahl, y = Status), color = "blue")
          return(plot)
        } else {
          return(NULL)
        }
      })
      
      # Weibull-Plot in der UI anzeigen (für alle Winkel)
      output$weibull_plot <- renderPlot({
        weibull_plot_r()
      })
      
      # Wöhler-Linie in der UI anzeigen
      output$woehler_plot <- renderPlot({
        if (!input$combine_plots) {
          woehler_plot_r()
        } else {
          combined_plot_r()
        }
      })
      
      # Zusammenfassungstabelle für Weibull-Parameter aller Winkel
      output$summary_table <- renderDataTable({
        params_45 <- weibull_params_45_r()$shape_scale_coefficients
        params_90 <- weibull_params_90_r()$shape_scale_coefficients
        params_180 <- weibull_params_180_r()$shape_scale_coefficients
        median_life_45 <- median_lifetime_45_r()
        median_life_90 <- median_lifetime_90_r()
        median_life_180 <- median_lifetime_180_r()
        datatable(data.frame(
          Winkel = c("45°", "90°", "180°"),
          T_Shape = c(params_45[1], params_90[1], params_180[1]),
          b_Scale = c(params_45[2], params_90[2], params_180[2]),
          Median_Lebensdauer = c(median_life_45, median_life_90, median_life_180)
        ))
      })
      
      # Tabelle der Wahrscheinlichkeitsschätzungen für alle Winkel
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
        
        # Kombinierte Tabelle
        datatable(rbind(
          data.frame(Winkel = "45°", prob_data_45),
          data.frame(Winkel = "90°", prob_data_90),
          data.frame(Winkel = "180°", prob_data_180)
        ))
      })
      
      # Berechnung und Anzeige der Raffungsfaktoren (T45/T90, T45/T180, T90/T180)
      output$raffungsfaktoren_table <- renderDataTable({
        T45 <- weibull_params_45_r()$shape_scale_coefficients[1]  # Shape-Parameter für 45°
        T90 <- weibull_params_90_r()$shape_scale_coefficients[1]  # Shape-Parameter für 90°
        T180 <- weibull_params_180_r()$shape_scale_coefficients[1]  # Shape-Parameter für 180°
        data.frame(
          Verhältnis = c("T45/T90", "T45/T180", "T90/T180"),
          Raffungsfaktor = c(T45 / T90, T45 / T180, T90 / T180)
        )
      })
      
      # Daten-Selektor Logik
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}
