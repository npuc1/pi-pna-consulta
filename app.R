library(shiny)
library(dplyr)
library(plotly)
library(xlsx)
library(here)
library(tidyverse)
library(shinyjs)
library(DT)
library(spsComps)
library(shinythemes)
library(data.table)
library(d3treeR)
library(treemap)

base1 <- read.xlsx("base_expandida.xlsx", 1) 
base2 <- read.xlsx("Ejes objetivos.xlsx", 1)
mergedf <- merge(x = base1, y = base2, by = "No.Línea.de.Acción")

mergedf <- mergedf %>% 
  relocate(No.Línea.de.Acción, .after = No.Estrategia)


base_acciones<- mergedf

base_expandida <- mergedf %>% 
  filter(contiene_accion == 1) %>% 
  mutate(lab_est = str_c("Estrategia ", No.Estrategia)) # labels para treeMap

base_reporte <- mergedf

estrategias <- c("Todas", "1.1", "1.2", "2.1", "3.1", "3.2", "4.1", "4.2", "5.1", "6.1", "7.1", "7.2", "8.1", "9.1", "9.2", "10.1", "11.1", "11.2", "12.1", "12.2", "12.3", "13.1", "14.1", "15.1", "15.2", "16.1", "16.2", "17.1", "17.2", "18.1", "18.2", "19.1", "19.2", "19.3", "20.1", "20.2", "21.1", "22.1", "23.1", "23.2", "24.1", "24.2", "25.1", "25.2", "26.1", "27.1", "28.1", "29.1", "29.2", "30.1", "30.2", "30.3", "31.1", "31.2", "32.1", "33.1", "34.1", "35.1", "36.1", "36.2", "37.1", "37.2", "38.1", "39.1", "40.1")
objetivos<-c("Todos", "1", "2", "3", "4", "5", "6","7", "8", "9", "10")
ejes <- c("Todos","1", "2", "3", "4")

# info buttons

infoBtn <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question", style = "color: white;"),
               size = "",
               class = 'btn action-button btn-info btn-xs shiny-bound-input',
               style = "background-color: gray; border-color: gray; color: white; border-radius: 50%; height: 16px; width: 16px; padding: 0;"
  )
}

infoBtn2 <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question", style = "color: white;"),
               size = "",
               class = 'btn action-button btn-info btn-xs shiny-bound-input',
               style = "background-color: gray; border-color: gray; color: white; border-radius: 50%; height: 22px; width: 22px; padding: 0;"
  )
}

ui <- navbarPage("Tablero de Implementación - Consulta",
                 useShinyjs(),
                 tabPanel("Consulta gráfica",
                          spsDepend("pop-tip"),
                          fluidPage(
                            theme = shinytheme("paper"),
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                selectInput("filtro_act_tree", "Actores:", c("Todos", sort(unique(base_acciones$Actores)))),
                                selectInput("filtro_pla_tree", "Plazos", c("Todos", "Corto", "Mediano", "Largo")),
                                infoBtn2('notWorking') %>% 
                                  bsTooltip("La gráfica cuenta el número de acciones únicas reportadas por los actores, por lo que los totales pueden variar si el actor reportó la misma acción en diferentes líneas.",
                                            opacity = 0.7)
                              ),
                              mainPanel(
                                width = 9,
                                tabPanel("Consulta gráfica", htmlOutput("treeMap"))
                              )
                            )
                          )),
                 tabPanel("Consulta tabla",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                selectInput("filter7", "Estrategia:", estrategias),
                                uiOutput("conditionalFilter")
                              ),
                              mainPanel(
                                width = 9,
                                h4(strong("Texto de la estrategia:")),
                                textOutput("textoEstrategia"),
                                hr(),
                                tabsetPanel(
                                  tabPanel("Acciones reportadas", dataTableOutput("dataTableInst")),
                                  tabPanel("Base por acción", dataTableOutput("previewTable"))
                                ),
                                downloadButton("downloadTable", "Descargar datos de tabla"),
                                hr()
                              )
                            )
                          ))
)


# Define server logic

server <- function(input, output, session) {
  
  # Treemap output
  
  observeEvent(eventExpr = {
    
    input$filtro_act_tree
    input$filtro_pla_tree
    
  }, {
    
    if(input$filtro_act_tree == "Todos" & input$filtro_pla_tree == "Todos") {
      
      output$treeMap <- renderUI({
        d3tree2(treemap(base_expandida,
                        index = c("Eje", "Actores", "lab_est", "No.Estrategia"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))
        ),
        rootname = "Acciones reportadas")
      })
      
    } else if(input$filtro_act_tree != "Todos" & input$filtro_pla_tree == "Todos") {
      
      output$treeMap <- renderUI({
        
        validate(
          need(nrow(base_expandida %>% 
                      filter(Actores == input$filtro_act_tree)) > 0, 
               paste(input$filtro_act_tree, "no reportó acciones."))
        )
        
        d3tree2(treemap(base_expandida %>% 
                          filter(Actores == input$filtro_act_tree),
                        index = c("Eje", "Actores", "lab_est", "No.Estrategia"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))
        ),
        rootname = "Acciones reportadas")
      })
      
    } else if(input$filtro_act_tree == "Todos" & input$filtro_pla_tree != "Todos") {
      
      
      output$treeMap <- renderUI({
        d3tree2(treemap(base_expandida %>% 
                          filter(Plazo == input$filtro_pla_tree),
                        index = c("Eje", "Actores", "lab_est", "No.Estrategia"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))
        ),
        rootname = "Acciones reportadas")
      })
      
    } else {
      
      output$treeMap <- renderUI({
        
        validate(
          need(nrow(base_expandida %>% 
                      filter(Plazo == input$filtro_pla_tree) %>% 
                      filter(Actores == input$filtro_act_tree)) > 0, 
               paste(input$filtro_act_tree, "no reportó acciones de", tolower(input$filtro_pla_tree), "plazo."))
        )
        
        
        d3tree2(treemap(base_expandida %>% 
                          filter(Plazo == input$filtro_pla_tree) %>% 
                          filter(Actores == input$filtro_act_tree),
                        index = c("Eje", "Actores", "lab_est", "No.Estrategia"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))
        ),
        rootname = "Acciones reportadas")
      })
      
    }
    
  })
  
  # Table data
  
  groupedData <- reactive({
    
    if(input$filter7 == "Todas") {
      
      return(base_reporte %>% 
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         extra = sum(as.integer(Extra)),
                         lineas_esp = length(unique(Línea_inst[Extra == 0])),
                         lineas_rep = length(unique(Línea_inst[contiene_accion == 1 & Extra == 0])),
                         Cobertura = ifelse(
                           min(Extra) == 1,
                           "N/A",
                           str_c(as.character(round(length(unique(Línea_inst[contiene_accion == 1 & Extra == 0])) /length(unique(Línea_inst[Extra == 0]))*100, 2)), "%")
                         )))
      
    } else if (input$filtroLA == "Todas") {
      
      return(base_reporte %>% 
               filter(No.Estrategia == input$filter7) %>% 
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         extra = sum(as.integer(Extra)),
                         lineas_esp = length(unique(Línea_inst[Extra == 0])),
                         lineas_rep = length(unique(Línea_inst[contiene_accion == 1 & Extra == 0])),
                         Cobertura = ifelse(
                           min(Extra) == 1,
                           "N/A",
                           str_c(as.character(round(length(unique(Línea_inst[contiene_accion == 1 & Extra == 0])) /length(unique(Línea_inst[Extra == 0]))*100, 2)), "%")
                         )))
      
    } else {
      
      return(base_reporte %>% 
               filter(No.Estrategia == input$filter7) %>% 
               filter(No.Línea.de.Acción == input$filtroLA) %>% 
               group_by(Actores) %>% 
               summarise(acc_tot = sum(contiene_accion),
                         extra = sum(as.integer(Extra)),
                         lineas_esp = length(unique(Línea_inst[Extra == 0])),
                         lineas_rep = length(unique(Línea_inst[contiene_accion == 1 & Extra == 0])),
                         Cobertura = ifelse(
                           min(Extra) == 1,
                           "N/A",
                           str_c(as.character(round(length(unique(Línea_inst[contiene_accion == 1 & Extra == 0])) /length(unique(Línea_inst[Extra == 0]))*100, 2)), "%")
                         )))
      
    }
    
  })
  
  txtEstrategia <- reactive({
    
    if(input$filter7 == "Todas") return("Seleccione una estrategia") else {
      
      est <- base_reporte %>% 
        filter(No.Estrategia == input$filter7)
      
      return(est$texto.estrategia[1])
      
    }
    
  })
  
  # Output texto estrategia
  
  output$textoEstrategia <- renderText({
    txtEstrategia()
  })
  
  # Filtro dinámico
  
  output$conditionalFilter <- renderUI({
    
    selectInput("filtroLA", "Línea de acción", c("Seleccione estrategia"))
    
  })
  
  observeEvent(input$filter7, {
    
    if(input$filter7 == "Todas") {
      
      updateSelectInput(session, "filtroLA", choices = c("Seleccione estrategia"))
      
    } else {
      
      opcionesLA <- base_reporte %>% 
        mutate(LA_string = str_detect(No.Línea.de.Acción, str_c("^", input$filter7))) %>% 
        filter(LA_string == TRUE) %>% 
        pull(No.Línea.de.Acción) %>% 
        unique()
      
      updateSelectInput(session, "filtroLA", choices = c("Todas", opcionesLA)) 
      
    }
  })
  
  # output tabla instituciones
  
  output$dataTableInst <- renderDataTable({
    
    cobertura_texto <- tags$span(
      "Cobertura", 
      infoBtn('notWorking') %>% 
        bsTooltip("El porcentaje de líneas de acción donde se contempla la participación del actor en el PI-PNA en las que este reportó por lo menos una acción",
                  opacity = 0.7)
    ) %>% 
      as.character()
    
    extra_texto <- tags$span(
      "Acciones adicionales", 
      infoBtn('notWorking') %>% 
        bsTooltip("Número de acciones reportadas en líneas de acción adicionales a las contempladas para el actor en el PI-PNA",
                  opacity = 0.7)
    ) %>% 
      as.character()
    
    lineas_esp_texto <- tags$span(
      "Líneas de acción donde participa", 
      infoBtn('notWorking') %>% 
        bsTooltip("De acuerdo con en el PI-PNA",
                  opacity = 0.7)
    ) %>% 
      as.character()
    
    lineas_rep_texto <- tags$span(
      "Líneas de acción reportadas", 
      infoBtn('notWorking') %>% 
        bsTooltip("Número de líneas de acción donde el actor participa en las que reportó por lo menos una acción",
                  opacity = 0.7)
    ) %>% 
      as.character()
    
    acc_tot_texto <- tags$span(
      "Acciones totales", 
      infoBtn('notWorking') %>% 
        bsTooltip("Número de acciones individuales reportadas por el actor a lo largo de todas las líneas de acción",
                  opacity = 0.7)
    ) %>% 
      as.character()
    
    datatable((groupedData()) %>% 
                rename(!!cobertura_texto:=Cobertura,
                       !!extra_texto:=extra,
                       !!lineas_esp_texto:=lineas_esp,
                       !!lineas_rep_texto:=lineas_rep,
                       !!acc_tot_texto:=acc_tot),
              rownames = TRUE,
              selection = 'none',
              escape = FALSE,
              options = list(language = list(url = "https://cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json"),
                             columnDefs = list(list(className = 'dt-center', targets = 0:6))))
  })
  
  # output tabla lista acciones
  
  previewData <- reactive({
    
    if(input$filter7 == "Todas") {
      
      return(base_expandida %>%
               select(c(4, 5, 10:15)))
      
    } else if(input$filtroLA == "Todas") {
      
      return(base_expandida %>% 
               filter(No.Estrategia == input$filter7) %>%
               select(c(4, 5, 10:15))
      )
      
    } else {
      
      return(base_expandida %>% 
               filter(No.Estrategia == input$filter7) %>%
               filter(No.Línea.de.Acción == input$filtroLA) %>% 
               select(c(4, 5, 10:15)))
      
    }
  })
  
  base_expandida = rename(base_expandida, c(`Línea de acción`=Línea.de.Acción,
                                            `Se coordina con`=Se.coordina.con,
                                            `Acción reportada`=Acción.reportada,
                                            `Fecha de inicio`=Fecha.de.inicio,
                                            `Fecha de término`=Fecha.de.término))
  output$previewTable <- renderDataTable({
    previewData()
  }, options = list(pageLength = 5,
                    language = list(url = "https://cdn.datatables.net/plug-ins/1.10.21/i18n/Spanish.json")))
  
  # Download table data
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("est_", input$filter7, "_LA_", input$filtroLA, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(previewData(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  
}

# Run the app
shinyApp(ui = ui, server = server)