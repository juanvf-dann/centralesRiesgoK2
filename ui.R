ui <- dashboardPage(
  skin = "yellow",
  
  dashboardHeader(),
  
  dashboardSidebar(
    collapsed = T,
    disable = T
  ),
  dashboardBody(
    useShinyalert(),
    textInput("NSolicitud","Numero de solicitud"),
    
    
    fluidPage( 
      tabBox(
        title = "Ingreso información centrales de riesgo",
        id = "BoxCentralesRiesgo",
        side = "left",
        selected = "ingreso",
        width = "90%",
        tabPanel("ingreso",
                 icon = icon("users"),
                 actionBttn(
                   inputId = "addBtnEntidad",
                   label = NULL,
                   style = "simple",
                   color = "success",
                   icon = icon("plus")
                 ), 
                 br(),
                 hr(),
                 tags$div(id="placeholder"),
                 hr(),
                 div(
                   style="text-align:center;",
                   actionBttn(
                     inputId = "guardar",
                     label = "Guardar",
                     style = "simple",
                     color = "warning",
                     icon = icon("save")
                   )
                 )
        ),
        
        tabPanel("Tabla resultante",
                 value = "TablaResultante",
                 icon = icon("table"),
                 verbatimTextOutput("tabla_final"),
                 strong(p(icon("exclamation-triangle")," La información que se muestra aquí, será consultada desde K2", class="alert alert-warning"))
        )
        
      )
      
    )
    
    
  )
)
