library(shiny)

ui <- fluidPage(
  htmlOutput("my_widget")
)

server <- function(input, output) {
  
  group <- c(rep("group-1", 4),
             rep("group-2", 2),
             rep("group-3", 3))
  
  subgroup <- paste("subgroup" , 
                    c(1, 2, 3, 4, 1, 2, 1, 2, 3), sep="-")
  
  value <- c(13, 5, 22, 12, 11, 7, 3, 1, 23)
  
  data <- data.frame(group, subgroup, value)
  
  p <- treemap(data,
               index=c("group","subgroup"),
               vSize="value",
               type="index",
               palette = "Set2",
               bg.labels=c("white"),
               align.labels=list(
                 c("center", "center"), 
                 c("right", "bottom")
               )  
  )            
  
  output$my_widget <- renderUI({
    d3tree2(p,
            rootname = "General")
  })
}

shinyApp(ui, server)
