#' Create a tabbed box
#'
#' Create a \code{\link[shinydashboard]{tabBox}} using an R6 object. The object
#' contains methods for creating the tabBox, inserting and removing \code{
#' \link[shiny]{tabPanel}} elements and the ability to expand the usual tabBox
#' with an additional action button which closes a tab.
#'
#' Instantiate a new tabBox_R6 object with \code{tabBox_R6$new(id,
#' selected = NULL, title = "Viewer", width = 6, height = NULL,
#' side = c("left", "right"))}.
#'
#' @usage
#' NULL
#'
#' @format
#' NULL
#'
#' @return
#' Public methods and fields are accessible using the '$' operator.
#' \item{append_tab(tab, select = FALSE, closeable = TRUE)}{Append the
#' \code{\link[shiny]{tabPanel}} \code{tab} to the tabBox. If \code{tab} should
#' be selected upon being inserted use \code{select = TRUE}. If the \code{tab}
#' should not be closeable via an \code{\link[shiny]{actionButton}} use
#' \code{closeable = FALSE}.}
#' \item{get(what)}{Get the value of the private element with name \code{what}.}
#' \item{insert_tab(tab, target, position = c("before", "after"),
#' select = FALSE, closeable = TRUE)}{Insert the \code{\link[shiny]{tabPanel}}
#' \code{tab} to the tabBox. See \code{\link[shiny]{insertTab}} for details
#' regarding \code{target} and \code{position}. If \code{tab} should be selected
#' upon being inserted use \code{select = TRUE}. If the \code{tab}
#' should not be closeable via an \code{\link[shiny]{actionButton}} use
#' \code{closeable = FALSE}.}
#' \item{prepend_tab(tab, select = FALSE, closeable = TRUE)}{Prepend the
#' \code{\link[shiny]{tabPanel}} \code{tab} to the tabBox. If \code{tab} should
#' be selected upon being inserted use \code{select = TRUE}. If the \code{tab}
#' should not be closeable via an \code{\link[shiny]{actionButton}} use
#' \code{closeable = FALSE}.}
#' \item{remove_tab(target)}{Remove the tab with value \code{target}.}
#' \item{set_session}{Set the session for the tabBox. This method is separated
#' from the instantiation because you will usually want to use the tabBox
#' already in the ui when there is no session present yet. Therefore the session
#' has to be manually in the server function.}
#' \item{tabBox()}{Return the HTML that builds the \code{\link[shinydashboard]{
#' tabBox}}.}
#' Use \code{get()} without any arguments to see the names of all private
#' fields and methods.
#'
#' @export
TabBox <- R6Class(
  "tabBox_R6",
  public = list(
    initialize = function(id, title = "Viewer",
                          width = 6, height = NULL, side = c("left", "right")) {
      private$id <- id
      private$title <- title
      private$width <- width
      private$height <- height
      private$side <- match.arg(side)
    },
    
    # Wird nur benötigt, wenn man mehrere Plots, die sich genau gleich verhalten
    # erzeugen möchte, ein Anwendungsfall fällt mir zurzeit nicht ein
    appendPlot = function(plot_reactive, title, select = TRUE,
                          closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      unique_id <- shiny:::createUniqueId()
      private$session$output[[unique_id]] <- renderPlot({
        return(plot_reactive())
      })
      data_value <- title %_% private$tabCounter
      tab <- tabPanel(
        title = title,
        plotOutput(
          outputId = private$session$ns(unique_id)
        ),
        value = data_value
      )
      shiny::appendTab(
        inputId = private$id,
        tab = tab,
        select = select,
        session = private$session
      )
      if (closeable) private$createActionButton(data_value)
      invisible(self)
    },
    
    append_tab = function(tab, select = TRUE, closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      data_value <- tab$attribs[["data-value"]]
      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        private$tab_values <- c(private$tab_values, data_value)
        shiny::appendTab(
          inputId = private$id,
          tab = tab,
          select = select,
          session = private$session
        )
        if (closeable) private$createActionButton(data_value)
      }
      invisible(self)
    },
    
    get = function(what) {
      if (missing(what)) return(names(private))
      private[[what]]
    },
    
    insert_tab = function(tab, target, position = c("before", "after"),
                          select = FALSE, closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      data_value <- tab$attribs[["data-value"]]
      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        private$tab_values <- c(private$tab_values, data_value)
        shiny::insertTab(
          inputId = private$id,
          tab = tab,
          target = target,
          position = match.arg(position),
          select = select,
          session = private$session
        )
        if (closeable) private$createActionButton(tab)
      }
      invisible(self)
    },
    
    is_open = function(value) {
      value %in% private$open_tab_values
    },
    
    is_value = function(value) {
      value %in% private$tab_values
    },
    
    prepend_tab = function(tab, select = FALSE, closeable = TRUE) {
      private$tabCounter <- private$tabCounter + 1
      data_value <- tab$attribs[["data-value"]]
      if (data_value %in% private$open_tab_values) {
        updateTabsetPanel(
          session = private$session,
          inputId = private$id,
          selected = data_value
        )
      } else {
        private$open_tab_values <- c(private$open_tab_values, data_value)
        private$tab_values <- c(private$tab_values, data_value)
        shiny::prependTab(
          inputId = private$id,
          tab = tab,
          target = target,
          select = select,
          session = private$session
        )
        if (closeable) private$createActionButton(tab)
      }
      invisible(self)
    },
    
    remove_tab = function(target) {
      index <- which(private$open_tab_values == target)
      private$open_tab_values <- private$open_tab_values[-index]
      shiny::removeTab(
        inputId = private$id,
        target = target,
        session = private$session
      )
      invisible(self)
    },
    
    set_session = function(session) {
      private$session <- session
    },
    
    tabBox = function(collapsible = FALSE) {
      if (!private$once) {
        if (!collapsible) {
          ui <- shinydashboard::tabBox(
            id = private$id,
            title = private$title,
            width = private$width,
            height = private$height,
            side = private$side
          )
        } else {
          ui <- shinydashboard::box(
            title = private$title,
            collapsible = TRUE,
            width = private$width,
            height = private$height,
            shinydashboard::tabBox(
              id = private$id,
              width = 12,
              side = private$side
            )
          )
          ui$children[[1]]$children[[2]]$children[[1]]$attribs$class <- paste(
            ui$children[[1]]$children[[2]]$children[[1]]$attribs$class,
            "collapsible-tab-box"
          )
        }
        private$once <- TRUE
        return(ui)
      }
      else print("tabBox has been already created.")
    }
  ),
  private = list(
    id = NULL,
    title = "Viewer",
    width = 6,
    height = NULL,
    side = "left",
    once = FALSE,
    session = NULL,
    tabCounter = 0,
    open_tab_values = character(),
    tab_values = character(),
    
    createActionButton = function(data_value) {
      closeId <- private$id %_% private$tabCounter
      div_button <- div(
        class = "div-btn-close",
        actionButton(
          inputId = closeId,
          label = NULL,
          icon = icon("window-close")
        )
      )
      selector <- paste0("#", private$id, " li a[data-value=\"", data_value, "\"]")
      insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = div_button
      )
      observeEvent(private$session$input[[closeId]], {
        self$remove_tab(target = data_value)
      }, domain = private$session)
    }
  )
)