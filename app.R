library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

fiftystatesCAN <- read.csv("/Users/leemingi/Documents/324-ShinyApp/fiftystatesCAN.csv")
BigTop100 <- read.csv("/Users/leemingi/Documents/324-ShinyApp/gradData.csv")

ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  # ),
  
  navbarPage(
    title = 'Team PHD',theme = shinytheme("united"),
    tabPanel("Graduate Program Finder",
             HTML("<h1><center><b>Graduate School</b> Finder</center></h1>"),
             br(), br(),br(), br()),

    tabPanel("Program Comparison",
             br(), br(),br(), br()),
    
    tabPanel("Data Visualization",
             br(), br(),br(), br()),

    navbarMenu("More",
               tabPanel('Project Description'),
               tabPanel('Design Process'),
               tabPanel('Acknowledgements and References'),
               tabPanel('Reflection'))
  )
  
)

server <- function(input, output){

}
  
shinyApp(ui = ui, server = server)
