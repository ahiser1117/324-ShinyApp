library(shiny)

ui <- fluidPage(
  navbarPage(
    title = 'Team PHD',
    tabPanel('Project Description'),
    tabPanel('Design Process'),
    tabPanel('Graduate Program Finder'),
    tabPanel('Acknowledgements and References'),
    tabPanel('Reflection')
  )
)

server <- function(input, output){
  
}
  
shinyApp(ui = ui, server = server)
