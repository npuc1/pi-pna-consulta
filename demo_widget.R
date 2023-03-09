library(shiny)
library(treemap)
library(d3treeR)
library(tidyverse)
library(xlsx)
library(spsComps)

base_expandida <- read.xlsx("base_expandida.xlsx", 1) %>% 
  filter(contiene_accion == 1) %>% 
  mutate(lab_est = str_c("Estrategia ", No.Estrategia))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("filtro_act_tree", "Actor", choices = c("Todos", unique(base_expandida$Actores))),
      selectInput("filtro_pla_tree", "Plazo", choices = c("Todos", "Corto", "Mediano", "Largo"))),
  mainPanel(htmlOutput("treeMap")))
)

server <- function(input, output) {
  
  
  observeEvent(eventExpr = {
    
    input$filtro_act_tree
    input$filtro_pla_tree
    
  }, {
    
    if(input$filtro_act_tree == "Todos" & input$filtro_pla_tree == "Todos") {
      
      output$treeMap <- renderUI({
        d3tree2(treemap(base_expandida,
                        index = c("Plazo", "Actores", "lab_est", "No.Estrategia"),
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
        d3tree2(treemap(base_expandida %>% 
                          filter(Actores == input$filtro_act_tree),
                        index = c("Plazo", "Actores", "lab_est", "No.Estrategia"),
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
                        index = c("Plazo", "Actores", "lab_est", "No.Estrategia"),
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
                        index = c("Plazo", "Actores", "lab_est", "No.Estrategia"),
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
}

shinyApp(ui, server)
