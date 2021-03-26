# MODULE UI ----
variablesUI <- function(id) {
  ns <- NS(id)
  
  tags$div(
    id=paste0("var", id),
    fluidRow(
      box(
        uiOutput(ns('variable')),
        uiOutput(ns('value'))
        
      )
    )
  )
}