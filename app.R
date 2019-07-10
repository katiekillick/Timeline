library(shiny)
library(timevis)


options <- list(min="01/01/2010", max="01/01/2020", zoomMin=86400000, type="point")


#'Load the CSV file content into variables
#'----------------------------------------
locationsCsv <- read.csv('www/csv/locationList.csv')
dataCsv <- read.csv('www/csv/timelineData.csv')

locationIds <- locationsCsv$x
#locationNames <- locationsCsv$y

#choices = list()


#'Add the Notification Types and Css Classes
#'------------------------------------------
notificationTypes <- c("a", "b", "c", "d", "e", "f", "g")
notificationCss <- c("a", "b", "c", "d", "e", "f", "g")
grouprs <- data.frame(id = 1:7, content= notificationTypes, className = notificationCss)


cols <- c('id', 'num', 'content', 'start', 'group', 'title', 'location', 'location_name', 'group_content', 'attach')
sdata <- dataCsv#[cols]


types <- notificationTypes # as.character(unique(grouprs$content))

ui <- fluidPage(
  

  tags$head( ),
  
  tags$body(,
    
   
    
    tags$div( class="container",
              
        sidebarLayout(
          sidebarPanel(
            selectInput("location", "Location", locationIds), #locationNames),
            checkboxGroupInput("type", "Type", notificationTypes)#, notificationTypes)
          ),
          
          mainPanel(
            
            fluidRow(
              tags$p('Select location and type of information in the sidebar. Zoom with the mouse roller and move timeline by clicking and dragging. Click on an item to see full details.')),

            fluidRow(
              tags$h3(textOutput("location")),
              timevisOutput("timeline"),
              tags$h3(textOutput("enq")),
              tags$h4(textOutput("location_name")),
              tags$p(textOutput("start_date")),
              tags$div(class = "report-details", htmlOutput("entry"))
            )
          )))
    
  ),
  
  tags$footer(class = "footer", 
      tags$div(class = "container",
         tags$span(class = "text-muted", 
             HTML(paste('x', format(Sys.Date(), "%Y"))))
      )
  )

)

server <- function(input, output, session) {
  
  locationData <- reactive({ sdata[sdata$location == as.character(input$location) & sdata$group_content %in% as.character(input$type), ,drop = F] })

  entryData <- reactive({ enc2native( gsub( "\\r\\n", "<br />", as.character( sdata[sdata$id == input$timeline_selected, 10] ) ) ) })
  
  locName <- reactive({ sdata[sdata$id == input$timeline_selected, 8] })
  enqData <- reactive({ sdata[sdata$id == input$timeline_selected, 2] })
  startDate <- reactive({ sdata[sdata$id == input$timeline_selected, 4] })

  output$location <- renderText(input$location)
  output$timeline <- renderTimevis({ timevis(locationData(), groups = grouprs, options = options) })
  
  output$location_name <- renderText(as.character(locName()))
  output$enq <- renderText(as.character(enqData()))
  output$start_date <- renderText(as.character(startDate()))
  
  output$entry <- renderUI(HTML(  as.character(entryData())))
  
}

shinyApp(ui = ui, server = server)
