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
    title = 'Team PhD',theme = shinytheme("united"),
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
                                                    choices = c("Computer Science", "Data Science", "Machine Learning","Computational Science and Engineering", "Electrical Engineering"),
                                                    selected =  c("Computer Science", "Data Science", "Machine Learning","Computational Science and Engineering", "Electrical Engineering")),
                                 
                                 checkboxGroupInput(inputId = "DegreeFinder",
                                                    label = "Select Degree(s):",
                                                    choices = c("Master", "PhD"),
                                                    selected = c("Master","PhD"))
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

    navbarMenu("More",
               tabPanel('Project Description',
                        includeMarkdown("ProductDocument.md")
                        ),
               tabPanel('Design Process',
                        HTML("<h1><center><b>Project Scope</b></center></h1>"),
                        includeMarkdown("Projectscope.md"),
                        br(), br(),br(), br(),
                        HTML("<h1><center><b>Process Map</b></center></h1>"),
                        img(src='processmap.png', style="display: block; margin-left: auto; margin-right: auto;", width = "80%", height = "80%"),
                        br(), br(),br(), br(),
                        HTML("<h1><center><b>Initial App Wireframe</b></center></h1>"),
                        img(src='wireframe1.png', style="display: block; margin-left: auto; margin-right: auto;", width = "80%", height = "80%"),
                        img(src='wireframe2.png', style="display: block; margin-left: auto; margin-right: auto;", width = "80%", height = "80%"),
                        img(src='wireframe3.png', style="display: block; margin-left: auto; margin-right: auto;", width = "80%", height = "80%")
                        ),
               
               tabPanel('Acknowledgements and References',
                        HTML("<h1>Acknowledgements and References</h1> <br> 
                             <h4>We thank to Professor Fernanda Eliott for her guidance, technical assistance, valuable resources, and feedback in this project.<br><br> 
                             Also, we thank our mentor David Lin for sharing his experience and providing advice throughout the project.<br><br><br>
                             
                             Code Reference:<br>
                             NCAA Swimming Team Finder for Incoming College Athletes by Greg Pilgrimname (https://shiny.rstudio.com/gallery/ncaa-swim-team-finder.html)<br><br>
                             Navbar-example from Shiny Gallery (https://shiny.rstudio.com/gallery/navbar-example.html)<br><br>
                             Update Input Demo (https://shiny.rstudio.com/gallery/update-input-demo.html)<br><br><br>
                             
                             R Packages:<br>
                             We used ggplot2 and plotly to make an interactive map. First, we made a US map with geom_polygon() and geom_point() functions in ggplot2. Then, we made the plot interactive with ggplotly() in the plotly package. Also, we used package shinythemes to set a theme for our application. When filtering out rows, we used filter() function in dplyr package. Lastly, we used the DT package to show datatable in our app.
                            
                             
                             </h4>"
                             ),
                        
                        ),
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
                                                              '<br>Program Length: ', ProgramLength, "years"

                                                              ),
                                                   x = lon, y = lat, color = Field, 
                                                   shape=Degree), size = 2, alpha = 0.5) +
          theme_void() +
          labs(color = "Field") +
          {if(length(input$FieldFinder) <= 1) scale_color_manual(guide = "none", values = c("Computer Science" = "red", "Data Science" = "orange", "Machine Learning" = "black","Computational Science and Engineering" = "green", "Electrical Engineering" = "blue"))} +
          {if(length(input$FieldFinder) > 1)
            scale_color_manual(values = c("Computer Science" = "red", "Data Science" = "orange", "Machine Learning" = "black","Computational Science and Engineering" = "green", "Electrical Engineering" = "blue"))} +
          {if(length(input$DegreeFinder) <= 1) scale_shape_manual(guide = "none", values = c("Master" = "circle", "PhD" = "star"))} +
          {if(length(input$DegreeFinder) > 1)
            scale_shape_manual(values = c("Master" = "circle", "PhD" = "star"))}
          
        ggplotly(p,tooltip = "text",source = "Plot1")%>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
        
        
        
      }
      
    })
  })
  
  
  
  output$info <- renderUI({
    d <- event_data("plotly_click", source = "Plot1")
    
    if (is.null(d)) {
      "Click a Point to See Detailed Information"
    } else {
      str1 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(22)])
      str2 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(23)])
      str3 <- a(gradData_finder()[gradData_finder()$lon==d$x,][,c(24)], href=gradData_finder()[gradData_finder()$lon==d$x,][,c(24)])
      str4 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(25)])
      str5 <- paste(gradData_finder()[gradData_finder()$lon==d$x,][,c(26)])
      str6<- paste("GRE: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(13)])
      str7<- paste( "Online Apply: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(14)])
      str8<- paste("Recommendation Letters #: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(15)])
      str9<- paste("Personal Statement: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(16)])
      str10<- paste("Transcipt: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(17)])
      str11<- paste("Resume or CV: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(18)])
      str12<- paste("IELTS: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(19)])
      str13<- paste("TOEFL: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(20)])
      str14<- paste("ELP: ", gradData_finder()[gradData_finder()$lon==d$x,][,c(21)])

      
      HTML(paste("University Detail: \n",str1, "\n",
                 tags$img(src = str5, width = "80%", height = "80%"), "\n",
                 "Program Detail: \n",str2,"\n", 
                 "Link to Faculty: \n",str3, "\n", 
                 "Professors: \n",str4, "\n",  
                 "Application Requirements: \n",
                 str6,
                 str7,
                 str8,
                 str9,
                 str10,
                 str11,
                 str12,
                 str13,
                 str14,
                 "\n", sep = '<br//>'))
    }
  })
    
  output$table <- renderDataTable({
    gradData[c(1,2,3,4,8,9,10,11,12)]})
  
  
}
  
shinyApp(ui = ui, server = server)
