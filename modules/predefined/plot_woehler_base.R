# Mit dieser Funktion wird das Layout des Wöhlerplots erzeugt. 
# Sie enthält die korrekt formatierten Achsen und alle durch die Versuche 
# aufgenommenen Datenpunkte.

# Funktionsargumente: 
## x: Vektor des Lebensdauermerkmals
## y: Vektor des Belastungsmerkmals
## title_main: Charakter, der den Plot Titel setzt 
## title_x: Charakter, der den Titel der x-Achse setzt
## title_y: Charakter, der den Titel der y-Achse setzt
## title_trace: Charakter, der den Titel der Legende setzt
## plot_method: "plotly" oder "ggplot2"

plot_woehler_layout <- function(x,
                                y,
                                title_main = "Wöhlerlinie",
                                title_x = "Lebensdauermerkmal",
                                title_y = "Belastungsmerkmal",
                                title_trace = "Belastung",
                                plot_method = c("plotly", "ggplot2")
) {
  
  plot_method <- match.arg(plot_method)
  
  x_s <- x[!is.na(x) == T]
  y_s <- y[!is.na(x) == T]
  
  # Weibull Layout anwenden: 
  p_wb_layout <- weibulltools:::plot_layout(
    x = x_s, 
    distribution = "weibull", 
    title_main = title_main, 
    title_x = title_x, 
    title_y = title_y, 
    plot_method = plot_method
  )
  
  # Werte der y-Achse müssen der Belastung entsprechen: 
  y_vals <- unique(y_s)
  
  # Weibull Layout hins. der y-Achse manipulieren: 
  if (plot_method == "plotly") {
    p_woehler_layout <- p_wb_layout %>% 
      plotly::layout(
        yaxis = list(
          tickvals = y_vals, 
          ticktext = y_vals
        )
      )
  } else {
    p_woehler_layout <- p_wb_layout + 
      ggplot2::scale_y_continuous(
        breaks = y_vals, 
        labels = y_vals, 
        minor_breaks = FALSE
      )
  }
    
  # Datenpunkte eintragen: 
  if (plot_method == "plotly") {
    p_woehler_pts <- p_woehler_layout %>% 
      plotly::add_trace(
        x = ~x_s, 
        y = ~y_s, 
        type = "scatter",
        mode = "markers", 
        name = paste0(title_trace, ": ", as.character(y_s)),  
        hoverinfo = "text",
        color = ~as.character(y_s), # wichtig für die Farbordnung (gleich weibulltools)
        colors = "Set2", 
        text = ~paste(
          paste("<br>", paste0(title_x, ":")), x_s,
          paste("<br>", paste0(title_y, ":")), y_s
        )
      )
      
  } else {
    p_woehler_pts <- p_woehler_layout + 
      ggplot2::geom_point(
        data = dplyr::tibble(x = x_s, y = y_s, color = as.character(y_s)), 
        mapping = aes(x = x, y = y, color = color)
      ) + 
      ggplot2::labs(color = title_trace)
  }
    
  p_woehler_pts
}