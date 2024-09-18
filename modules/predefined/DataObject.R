DataObject <- R6Class(
  classname = "DataObject",
  public = list(
    initialize = function(name, value) {
      private$name <- reactiveVal(name)
      private$value <- reactiveVal(value)
    },
    
    get_name = function() {
      private$name()
    },
    
    get_value = function() {
      private$value()
    },
    
    set_name = function(name) {
      private$name(name)
    },
    
    set_value = function(value) {
      private$value(value)
    }
  ),
  private = list(
    name = NULL,
    value = NULL
  )
)