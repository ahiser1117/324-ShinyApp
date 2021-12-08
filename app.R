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
gradData <- read.csv("gradData.csv")

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
                                                    choices = c("Computer Science", "Data Science", "Machine Learning","Computational Science and Engineering", "Electrical Engineering"),
                                                    selected =  c("Computer Science", "Data Science", "Machine Learning","Computational Science and Engineering", "Electrical Engineering")),
                                 
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
                 
                 sliderInput(inputId = "TuitionFinder",
                             label = "Tuition per Year ($)",
                             min = 0,
                             max = 99999,
                             value = c(0,99999),
                             width = "220px"),
                 
                 hr(),
                
                 sliderInput(inputId = "DeadlineFinder",
                             label = "Select Deadline",
                             min = as.Date("2021-08-01","%Y-%m-%d"),
                             max = as.Date("2022-08-01","%Y-%m-%d"),
                             value=c(as.Date("2021-08-01"),as.Date("2022-08-01")),
                             timeFormat="%Y-%m-%d")
               ),
               
               mainPanel(
                 withSpinner(plotlyOutput(outputId = "scatterplotFinder")),
                 hr(),
                 htmlOutput("info"),
                 hr(),
                 br(),
                 fluidRow(                
                   dataTableOutput('table')))
             )
    ),

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
    req(input$TuitionFinder)
    req(input$DeadlineFinder)
    
    
    filter(gradData, Region %in% input$RegionFinder) %>%
    filter(Field %in% input$FieldFinder) %>%
    filter(Degree %in% input$DegreeFinder) %>%
    filter(ProgramLength >= input$LengthFinder[1], ProgramLength <= input$LengthFinder[2]) %>%
    filter(Tuition >= input$TuitionFinder[1], Tuition <= input$TuitionFinder[2]) %>%
    filter(as.Date(Deadlines) >= input$DeadlineFinder[1], as.Date(Deadlines) <= input$DeadlineFinder[2])
    
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
    input$TuitionFinder
    input$DeadlineFinder
    
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
          geom_point(data = gradData_finder(), aes(text=paste(Name,
                                                              '<br>',Field, '/' , Degree,
                                                              '<br>Tuition($): ',Tuition,
                                                              '<br>Application Fee($): ',ApplicationFee,
                                                              '<br>Deadline: ', Deadlines,
                                                              '<br>Program Length: ', ProgramLength, "years",
                                                              '<br>GRE: ', GRE, "/ Online Apply: ", ApplyOnline, "/ Recommendation Letters #: ", RecLetters,
                                                              '<br>Personal Statement: ', PersonalStatement, "/ Transcipt: ", Transcipt, "/ Resume or CV: ", ResumeCV,
                                                              '<br>IELTS: ', IELTS, "/ TOEFL: ", TOEFL, "/ ELP: ", ELP
                                                              ),
                                                   x = lon, y = lat, color = Field, 
                                                   shape=Degree), size = 2, alpha = 0.5) +
          theme_void() +
          labs(color = "Field") +
          {if(length(input$FieldFinder) <= 1) scale_color_manual(guide = "none", values = c("Computer Science" = "red", "Data Science" = "orange", "Machine Learning" = "black","Computational Science and Engineering" = "green", "Electrical Engineering" = "blue"))} +
          {if(length(input$FieldFinder) > 1)
            scale_color_manual(values = c("Computer Science" = "red", "Data Science" = "orange", "Machine Learning" = "black","Computational Science and Engineering" = "green", "Electrical Engineering" = "blue"))} +
          {if(length(input$DegreeFinder) <= 1) scale_shape_manual(guide = "none", values = c("Master" = "circle", "Phd" = "star"))} +
          {if(length(input$DegreeFinder) > 1)
            scale_shape_manual(values = c("Master" = "circle", "Phd" = "star"))}
          
        ggplotly(p,tooltip = "text",source = "Plot1")%>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
        
        
        
      }
      
    })
  })
  
  
  
  output$info <- renderUI({
    d <- event_data("plotly_click", source = "Plot1")
    
    if (is.null(d)) {
      "Click Point to See Detailed Information"
    } else {
      str1 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(22)])
      str2 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(23)])
      str3 <- a(gradData_finder()[gradData_finder()$lon==d$x,][,c(24)], href=gradData_finder()[gradData_finder()$lon==d$x,][,c(24)])
      str4 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(25)])
      str5 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(26)])
      HTML(paste("University Detail: \n",str1, "\n",
                 tags$img(src = str5), "\n",
                 "Program Detail: \n",str2,"\n", 
                 "Link to Faculty: \n",str3, "\n", 
                 "Professors: \n",str4, "\n",  
                 "\n", sep = '<br//>'))
    }
  })
    
  output$table <- renderDataTable({
    gradData[c(1,2,3,4,8,9,10,11,12)]})
  
  
}
  
shinyApp(ui = ui, server = server)
