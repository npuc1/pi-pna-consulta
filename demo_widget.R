library(shiny)
library(treemap)
library(d3treeR)
library(tidyverse)
library(xlsx)
library(spsComps)

base_expandida <- read.xlsx("base_expandida.xlsx", 1) %>% 
  filter(contiene_accion == 1) %>% 
  mutate(lab_est = str_c("Estrategia ", No.Estrategia)) %>% 
  mutate(lab_ver = "Ver acciones")

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
                        index = c("Plazo", "Actores", "lab_est", "lab_ver"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))),
                rootname = "Acciones reportadas")
      })
      
    } else if(input$filtro_act_tree != "Todos" & input$filtro_pla_tree == "Todos") {
      
      output$treeMap <- renderUI({
        d3tree2(treemap(base_expandida %>% 
                          filter(Actores == input$filtro_act_tree),
                        index = c("Plazo", "Actores", "lab_est", "lab_ver"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))),
                rootname = "Acciones reportadas")
      })
      
    } else if(input$filtro_act_tree == "Todos" & input$filtro_pla_tree != "Todos") {
      
      output$treeMap <- renderUI({
        d3tree2(treemap(base_expandida %>% 
                          filter(Plazo == input$filtro_pla_tree),
                        index = c("Plazo", "Actores", "lab_est", "lab_ver"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))),
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
                        index = c("Plazo", "Actores", "lab_est", "lab_ver"),
                        vSize = "contiene_accion",
                        type = "index",
                        palette = "Set2",
                        align.labels = list(
                          c("center", "center"), 
                          c("right", "bottom"))),
                rootname = "Acciones reportadas")
      })
      
    }
    
  })
}

shinyApp(ui, server)


######### minimal example

library(shiny)
library(d3treeR)
library(htmlwidgets)
library(treemap)

df <- data.frame(
  level.a = c("1", "2", "3", "4", "1", "2"),
  level.b = c("1.1", "2.1", "3.1", "4.1", "1.2", "2.2"),
  value = c(1, 1, 1, 1, 1, 1),
  details = "abc", "def", "ghs", "ers", "sas", "ert"
)

ui <- fluidPage(
  d3tree2Output("tree")
)

server <- function(input, output) {
  
  output$tree <- renderD3tree2({
    d3tree2(treemap(df,
                    index = c("level.a", "level.b"),
                    vSize = "value",
                    type = "index",
                    palette = "Set2"
    ),
    rootname = "Root")
  })
  
}

shinyApp(ui, server)

