library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(maps)

fiftystatesCAN <- read.csv("/Users/leemingi/Documents/324-ShinyApp/fiftystatesCAN.csv")
gradData <- read.csv("/Users/leemingi/Documents/324-ShinyApp/gradData.csv")

ui <- fluidPage(

  navbarPage(
    title = 'Team PHD',theme = shinytheme("united"),
    tabPanel("Graduate Program Finder",
             HTML("<h1><center><b>Graduate School</b> Finder</center></h1>"),
             br(), br(),br(), br(),
             
             ## Sidebar
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Program Filter"),
                 fluidRow(column(3,
                                 checkboxGroupInput(inputId = "FieldFinder",
                                                    label = "Select Field(s):",
                                                    choices = c("Computer Science" = "C", "Physics" = "P"),
                                                    selected = c("C","P")),
                                 
                                 checkboxGroupInput(inputId = "DegreeFinder",
                                                    label = "Select Degree(s):",
                                                    choices = c("Master", "PHD"),
                                                    selected = c("Master","PHD"))
                 ),
                 column(6, offset = 2,
                        checkboxGroupInput(inputId = "RegionFinder",
                                           label = "Select Region(s):",
                                           choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific"),
                                           selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific"))
                        )
                 ),

                 hr(),
                 sliderInput(inputId = "Program Length",
                             label = "Select Program Length",
                             min = 1,
                             max = 10,
                             value = c(1,6),
                             width = "220px"),
                 hr(),
                 
                 checkboxGroupInput(inputId = "Requirement",
                                    label = "Select Requirement(s):",
                                    choices = c("GRE", "Online Application", "Letters of Application", "Transcript", "CV or Resume", "IELTS", "TOEFL"),
                                    selected = c("GRE", "Online Application", "Letters of Application", "Transcript", "CV or Resume", "IELTS", "TOEFL")),
                                    
                 sliderInput(inputId = "TuiotionFinder",
                             label = "Tuition per Year ($)",
                             min = 0,
                             max = 100000,
                             value = c(0,100000),
                             width = "220px"),
                 sliderInput(inputId = "DeadlineFinder",
                             label = "Select Deadline",
                             min = as.Date(Sys.Date()),
                             max = as.Date("2022-08-01","%Y-%m-%d"),
                             value=c(as.Date(Sys.Date()),as.Date("2022-08-01")),
                             timeFormat="%Y-%m-%d")
               ),
               
               mainPanel(
                 withSpinner(plotOutput(outputId = "scatterplotFinder"))
                  )
             )),
                 

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

server <- function(input, output, session){
  gradData_finder <- reactive({
    req(input$RegionFinder)
    filter(Region %in% input$RegionFinder)
    
  })
  
  fiftystatesCAN_Finder <- reactive({
    req(input$RegionFinder)
    filter(fiftystatesCAN, GeoRegion %in% input$RegionFinder)
  })
  
  
  output$scatterplotFinder <- renderPlot({
    input$RegionFinder
    isolate({
        ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          theme_void() +
          ggtitle("No programs fit selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
    })
  })
  
}
  
shinyApp(ui = ui, server = server)
