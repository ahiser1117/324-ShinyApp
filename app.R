library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(plotly)
library(maps)

fiftystatesCAN <- read.csv("fiftystatesCAN.csv")
gradData <- read.csv("gradData1.csv")

ui <- fluidPage(

  navbarPage(
    title = 'Team PHD',theme = shinytheme("united"),
    tabPanel("Graduate Program Finder",
             HTML("<h1><center><b>Graduate School</b> Finder</cexnter></h1>"),
             br(), br(),br(), br(),
             
             ## Sidebar
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Program Filter"),
                 fluidRow(column(3,
                                 checkboxGroupInput(inputId = "FieldFinder",
                                                    label = "Select Field(s):",
                                                    choices = c("Computer Science", "Physics"),
                                                    selected =  c("Computer Science", "Physics")),
                                 
                                 checkboxGroupInput(inputId = "DegreeFinder",
                                                    label = "Select Degree(s):",
                                                    choices = c("Master", "Phd"),
                                                    selected = c("Master","Phd"))
                 ),
                 column(6, offset = 2,
                        checkboxGroupInput(inputId = "RegionFinder",
                                           label = "Select Region(s):",
                                           choices = c("New England" = "NewEngland", "Mid Atlantic" = "MidAtlantic", "Mid West" = "MidWest", "South", "West", "South West" = "SouthWest", "Pacific"),
                                           selected = c("NewEngland", "MidAtlantic", "MidWest", "South", "West", "SouthWest", "Pacific"))
                        )
                 ),

                 hr(),
                 sliderInput(inputId = "LengthFinder",
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
                 withSpinner(plotlyOutput(outputId = "scatterplotFinder")),
                 hr(),
                 br(),
                 fluidRow(                
                   dataTableOutput('table')))
             )
    ),


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
    req(input$FieldFinder)
    req(input$DegreeFinder)
    req(input$LengthFinder)
    
    
    filter(gradData, Region %in% input$RegionFinder) %>%
    filter(Field %in% input$FieldFinder) %>%
    filter(Degree %in% input$DegreeFinder) %>%
    filter(ProgramLength >= input$LengthFinder[1], ProgramLength <= input$LengthFinder[2])
      
    
  })
  
  fiftystatesCAN_Finder <- reactive({
    req(input$RegionFinder)
    filter(fiftystatesCAN, GeoRegion %in% input$RegionFinder)
  })
  
  
  
  
  output$scatterplotFinder <- renderPlotly({
    input$FieldFinder
    input$RegionFinder
    input$DegreeFinder
    input$LengthFinder
    
    isolate({
      if (length(gradData_finder()$Address) == 0) {
        p <- ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          theme_void() +
          ggtitle("No programs fit selected characteristics. \nPlease modify selections.") +
          theme(plot.title = element_text(face = "bold", color = "#FF8D1E", size = 20))
        ggplotly(p)
      }
      else {
        p <-    ggplot() +
          geom_polygon(data = fiftystatesCAN_Finder(), aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
          coord_quickmap() +
          guides(fill = FALSE) +
          geom_point(data = gradData_finder(), aes(text=Name, x = lon, y = lat, color = Field, shape=Degree), size = 2, alpha = 0.5) +
          theme_void() +
          labs(color = "Field") +
          {if(length(input$FieldFinder) <= 1) scale_color_manual(guide = "none", values = c("Computer Science" = "#1E90FF", "Physics" = "#FF8D1E"))} +
          {if(length(input$FieldFinder) > 1)
            scale_color_manual(values = c("Computer Science" = "blue", "Physics" = "red"))} +
          {if(length(input$DegreeFinder) <= 1) scale_shape_manual(guide = "none", values = c("Master" = "circle", "Phd" = "triangle"))} +
          {if(length(input$DegreeFinder) > 1)
            scale_shape_manual(values = c("Master" = "circle", "Phd" = "triangle"))}
        
        
        ggplotly(p,tooltip = c("Name","Field"))
        
        
      }
      
    })
  })
  
  
  user_clickFinder <- reactiveValues()
  reactive({
    user_clickFinder$DT <- data.frame(matrix(0, ncol = ncol(gradData), nrow = 1))
    names(user_clickFinder$DT) <- colnames(gradData)
  })
  
  
  observeEvent(input$click_plotFinder, {
    add_row <-     nearPoints(gradData_finder(), input$click_plotFinder, xvar = "lon", yvar = "lat", threshold = 5)
    user_clickFinder$DT <- rbind(add_row, user_clickFinder$DT)
  })

  
  brushFinder <- reactive({
    req(length(user_clickFinder$DT) > 1)
    user_clickFinder$DT
  })
  
    
  output$table <- renderDataTable({
    gradData})
  
  
}
  
shinyApp(ui = ui, server = server)
