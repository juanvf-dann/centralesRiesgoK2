
# Server Logic
server <- function(input, output, session) {
  
  observe({
    if( exists("tabla.variables.2") ) {
      remove(tabla.variables.2)
    }
  },
  autoDestroy = TRUE,)
  
  # Observer
  observeEvent(input$addBtnEntidad, {
    # make ids for the plot, the remove button, and the element to remove
    if(input$addBtnEntidad==1){
      ids.df <<- data.frame(
        ID.ADD    = NA,
        ID.REMOVE = NA,
        ID.ELE    = NA
      )[0,]
    }
    
    
    id_add <- paste0(input$addBtnEntidad)
    remove_id <- paste0("remove_", id_add)
    ele_id <- paste0("ele_", id_add)
    ids.df[id_add,] <<-
      c(
        ID.ADD    = id_add,
        ID.REMOVE = remove_id,
        ID.ELE    = ele_id
      )
    # insert all of the ui at once inside a larger UI element (div) with a id value
    insertUI(
      selector = '#placeholder', 
      where = "beforeEnd",
      ui= tags$div(
        id = ele_id,
        fluidRow(
          box(status = "warning",
              width = "65%",
              collapsible = TRUE,
              solidHeader =T,
              title = p(paste0("Entidad #",id_add),tags$span("      "),
                        actionButton(remove_id, "Remove", icon = icon("trash-alt"), class = "btn-xs", title = "Update")
              ),
              pickerInput(
                inputId = paste0("clienteConsulta_NuevaEntidad_", id_add),
                label = "Entidad",
                choices = tabla.entidades$ENTIDAD,
                selected = NULL,
                options = pickerOptions(
                  liveSearch = T,
                  dropdownAlignRight = F
                )
                
              ),
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
                choices = escala.calificacion_homologada,
                status = "primary",
                justified = T,
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"))
              ),
              radioGroupButtons(
                inputId = paste0("clienteConsulta_CCATI_",id_add),
                label = "Calificacón Trimestre I",
                choices = escala.calificacion_homologada,
                status = "primary",
                justified = T,
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"))
              ),
              radioGroupButtons(
                inputId = paste0("clienteConsulta_CCAActual_",id_add),
                label = "Calificacón Actual",
                choices = escala.calificacion_homologada[1:5],
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
        # actionButton(, "Remove")
      )
    )
    
    observeEvent(input[[remove_id]], {
      
      ids.df <<- ids.df %>% filter(ID.REMOVE != remove_id)
      # 
      # print(ids.df)
      # 
      
      removeUI(
        selector = paste0("#", ele_id)
      )
    })
    
    print(ids.df)
  })
  
  
  # 
  # if (input$add_elements == "Element1") {
  #   output[[id_add]] <- renderPlot({
  #     plot(data[,1],data[,2])
  #   })
  # } else if (input$add_elements == "Element2") {
  #   output[[id_add]] <- renderPlot({
  #     plot(data[,1],data[,4])
  #   })
  # }
  
  ## Remove Elements ###
  # when the specific remove button is clicked, remove the larger UI element containing the plot and button
  
  
  
  observeEvent(input$guardar,{
    req(input$NSolicitud)

    if(exists("ids.df")){
      
      test.df <- dbGetQuery(con.centralesRiesgo,
                            paste0("SELECT SOLICITUD FROM TABLA_REPORTES_CENTRALES_RIESGO WHERE SOLICITUD =",input$NSolicitud)
      )
      
      print(test.df)
      if(dim(test.df)[1]==0){
        
        if(dim(ids.df)[1]>0){
          print(ids.df)

          # variables <-c("TXT_","saldo_")
          variables <- c(
            "clienteConsulta_NuevaEntidad_",
            "clienteConsulta_CupoAprobado_",
            "clienteConsulta_SaldoActual_",
            "clienteConsulta_CCATII_",
            "clienteConsulta_CCATI_",
            "clienteConsulta_CCAActual_",
            "clienteConsulta_score_",
            "clienteConsulta_KDeudor_",
            "clienteConsulta_KCodeudor_",
            "clienteConsulta_SaldoDann_"
          )
          
          tabla.variables <-
            expand.grid(ID = ids.df$ID.ADD,VAR=variables) %>%
            mutate(VARIABLE = paste0("input$",VAR,ID)) #%>%
          # mutate(
          #   VALOR = Vectorize(evaluar.fn)(VARIABLE)
          # )
          #
          x<- c()
          # message(
          for(k in 1:dim(tabla.variables)[1])
            x[k] <- eval(parse(text=tabla.variables$VARIABLE[k]))
          
          tabla.variables$VALOR <- x
          
          
          
          
          
          
          print(tabla.variables)
          # paste0(
          #   "===========================\n",
          #   "ADD IDS = ",paste(id_add,collapse = " "),"\n",
          #   "RMV IDS = ",paste(remove_id,collapse = " "),"\n",
          #   "===========================\n"
          # )
          # )
          
          message("==============================")
          
          
          tabla.variables.2 <-
            tabla.variables %>% mutate(VAR = gsub("clienteConsulta_|_","",VAR)) %>%
            select(VAR,VALOR,ID) %>%
            spread(VAR,VALOR) %>%
            mutate(
              SOLICITUD = input$NSolicitud,
              KDeudor   = case_when(as.logical(KDeudor)   ~ "SÍ",TRUE ~ "NO"),
              KCodeudor = case_when(as.logical(KCodeudor) ~ "SÍ",TRUE ~ "NO")
            ) %>%
            select(
              SOLICITUD,
              ID,
              NuevaEntidad,
              CupoAprobado,
              SaldoActual,
              CCATII,
              CCATI,
              CCAActual,
              score,
              KDeudor,
              KCodeudor,
              SaldoDann
            )
          
          
          
          
          dbWriteTable(con.centralesRiesgo,"TABLA_REPORTES_CENTRALES_RIESGO",tabla.variables.2,append = TRUE)
          
          
          if( exists("tabla.variables.2") ) {
            output$tabla_final <- renderPrint({
              tabla.variables.2
            })
          }
          shinyalert(
            "Información guardada",
            paste0(
              "El cliente con número de solicitud:\n << ",
             input$NSolicitud,
              " >>\nfue guardado con éxito."
            ),
            type="success"
          )
          
          updateTabsetPanel(session, "BoxCentralesRiesgo",
                            selected = "TablaResultante")
          
        }else{
          shinyalert(
            "No hay información",
            paste0(
              "Por favor ingrese una o más entidades de centrales de riesgo"
            ),
            type="error"
          )
        }
        
        
        
      }else{
        
        shinyalert(
          "Cliente ya existe",
          paste0(
            "con el cliente con número de solicitud: ",
            input$NSolicitud,
            ", ya se hizo el proceso que está llevando a cabo."
          ),
          type="error",
        )
      }
      



      
    }


    
  })
  
  
  
}

