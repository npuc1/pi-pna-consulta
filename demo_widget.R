library(shiny)
library(treemap)
library(d3treeR)
library(tidyverse)
library(xlsx)

ui <- fluidPage(
  htmlOutput("my_widget")
)

server <- function(input, output) {
  
  base_expandida <- read.xlsx("base_expandida.xlsx", 1) %>% 
    filter(contiene_accion == 1)
  
  output$my_widget <- renderUI({
    d3tree2(treemap(base_expandida,
                    index = c("Plazo", "Actores", "Estrategia", "Acción.reportada"),
                    vSize = "contiene_accion",
                    type = "index",
                    palette = "Set2",
                    align.labels = list(
                      c("center", "center"), 
                      c("right", "bottom")
                    )  
    ),
    rootname = "Acciones reportadas")
  })
}

shinyApp(ui, server)
