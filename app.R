library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
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
                        includeMarkdown("ProjectScope.md"),
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
                             )
                        
                        ),
               tabPanel('Reflection',
                        HTML("<h1>Reflection</h1> <br> 
                             <h4>Our group's workflow organization was to separate into two teams based on the two main tasks we have: collecting the information and building the app. For the first task, we largely underestimated the time to collect information about different graduate programs online, so we have to cut the numbers of graduate programs featured in our app. However, this also proves the potential of this app. Hours could be saved for a user by finding information in our app instead of wasting them sifting through graduate program websites. And for the second task, we spent a lot of time making the map more helpful to users. And for the other group, we also had the problem that some of the features we originally designed could not be accomplished. For example, we planned to do a clickable map. We originally planned that when you click the icon on the map, the app will jump to another detail page about the school. But now we decided to put the detail page below the map after clicking the map. Our process has been that the two groups meet and talk about the process every Wednesday and it has been working to keep us productive and connected as a team. We would talk about what we did for the last week and make a plan for the future week. Meanwhile, the project scope is updated every week so that we can keep up with what others are doing and have the deadlines in mind. <br><br>
                             During the development, we solved multiple problems. In the beginning, slide bars are not working fine. The slide bar for deadlines could not have a correct start date. However, after hours of research, we fixed the problem and now the default start date would be the date today so that users would not see any programs that have already passed or are not applicable. <br><br>
                             The main structure of our project contains two parts, the searching tools, and the information section. Since our application wants to offer a selection of refined resources, our selection section offers multiple ways to filter the information, including the deadline, tuition fee, application requirements, etc. Meanwhile, the information section would contain brief details about the university, the program, and a picture of the campus. On the bottom, our application would offer the featured professors from the program and the contact information of which. If someone wants to know more about the program, the link to the official program website would also be listed. <br><br>
                             In the future, we want to add the comparison feature. When multiple programs are selected, the user could go into a comparison tab and the features of the programs would be listed in a table so that the user could compare them easily. If time is available, we would also make the detailed information be shown on another pop-up page when the icon on the map is clicked.<br><br>
                             One thing that our group is proud of is the information we gathered during such a short period. The user can directly view the location of each program, select the programs they want based on multiple choices. After choosing the program, the user could find detailed, filtered information about it. Using the link we provide, the user can go straight to the application page and apply for the program. 
<br>

                             </h4>"
                        )))
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
