# MODULE UI ----
LHSchoices <- c("X1", "X2", "X3", "X4")



#------------------------------------------------------------------------------#
# MODULE SERVER ----
# MODULE SERVER ----

variablesServer <- function(input, output, session){
  ns = session$ns
  
  output$variable <- renderUI({
    selectInput(
      inputId = ns("variable"),
      label = paste0("Variable ", strsplit(x = ns(""), split = "-")),
      choices = c("Choose" = "", LHSchoices)
    )
  })
  
  output$value <- renderUI({
    numericInput(
      inputId = ns('value'),
      label = paste0("Value ", strsplit(x = ns(""), split = "-")),
      value = NULL
    )
  })
  
  output$NuevaEntidad <- renderUI({
    pickerInput(
      inputId = ns(NuevaEntidad),
      label = "Entidad",
      choices = tabla.entidades$ENTIDAD,
      selected = NULL,
      options = pickerOptions(
        liveSearch = T,
        dropdownAlignRight = F
      )
      
    )
  })
}

fluidRow(
  box(status = "primary",
      width = "65%",
      collapsible = TRUE,
      solidHeader =T,
      title = p(paste0("Entidad #",id_add),tags$span("      "),
                actionButton(remove_id, "Remove", icon = icon("trash-alt"), class = "btn-xs", title = "Update")
      )
      # selectInput(paste0("clienteConsulta_NuevaEntidad_", id_add), "Entidad",
      #             c("Option 1", "Option 2", "Option 3")), 
      flowLayout(
        numericInput(paste0("clienteConsulta_CupoAprobado_", id_add),"Cupo aprobado",value = NA, min=0),
        numericInput(paste0("clienteConsulta_SaldoActual_", id_add),"Saldo actual",value = NA, min=0),
        numericInput(paste0("clienteConsulta_score_", id_add),"Score",value = NA,width = "50%", min=0,max=1000),
        numericInput(paste0("clienteConsulta_valorGarantias_", id_add),"valor garantías",value = NA, min=0),
        numericInput(paste0("clienteConsulta_SaldoDann_", id_add),"Saldo en Dann Regional",value = NA, min=0)
      ),
      radioGroupButtons(
        inputId = paste0("clienteConsulta_CCATII_",id_add),
        label = "Calificacón Trimestre II",
        choices = c("AA","A" ,"BB","B", "CC","C", "INC"),
        status = "primary",
        justified = T,
        checkIcon = list(
          yes = icon("ok", lib = "glyphicon"))
      ),
      radioGroupButtons(
        inputId = paste0("clienteConsulta_CCATI_",id_add),
        label = "Calificacón Trimestre I",
        choices = c("AA","A" ,"BB","B", "CC","C", "INC"),
        status = "primary",
        justified = T,
        checkIcon = list(
          yes = icon("ok", lib = "glyphicon"))
      ),
      radioGroupButtons(
        inputId = paste0("clienteConsulta_CCAActual_",id_add),
        label = "Calificacón Actual",
        choices = c("AA","A" ,"BB","B", "CC","C", "INC"),
        status = "primary",
        justified = T,
        checkIcon = list(
          yes = icon("ok",lib = "glyphicon")
        )
      ),
      flowLayout(
        div(
          strong(p("Castigado como deudor")),
          div(style="text-align:center;",
              prettyToggle(
                inputId = paste0("clienteConsulta_KDeudor_", id_add),
                label_on = "Sí",
                icon_on = icon("exclamation-triangle"),
                status_on = "danger",
                status_off = "success",
                label_off = "No",
                icon_off = icon("check"),
                shape = c("round"),
                bigger = T
              )
          )
          
        )
        ,
        div(
          strong(p("Castigado como codeudor")),
          div(style="text-align:center;",
              prettyToggle(
                inputId = paste0("clienteConsulta_KCodeudor_", id_add),
                label_on = "Sí", 
                icon_on = icon("exclamation-triangle"),
                status_on = "danger",
                status_off = "success", 
                label_off = "No",
                icon_off = icon("check"),
                shape = c("round"),
                bigger = T
              )
          )
        )
      )
      # 
      # textInput(paste0("TXT_",id_add),"Texto" ),
      # textInput(paste0("saldo_",id_add),"Saldo" ),
  )
)

