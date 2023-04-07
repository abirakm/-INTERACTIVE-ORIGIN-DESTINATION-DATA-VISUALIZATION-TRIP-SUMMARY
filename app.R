library(shiny)
library(leaflet)
library(rgdal)
library(data.table)
library(semantic.dashboard)
library(dplyr)
library(bit64)
#library(ggplot2)
library(plotly)


folder <- getwd()
ShapeFile <- readOGR(dsn = "/srv/custom-library/data",'OUATS_AirSage_TAZ')
ShapeFile$TAZ_ID <- as.numeric(as.character(ShapeFile$TAZ_ID))

ShapeFile@data$COUNTY <- NULL
ShapeFile@data$Int_Ext <- NULL
ShapeFile@data$OUATS_2040 <- NULL
#ShapeFile <- ShapeFile[ShapeFile@data$TAZ_ID < 4000,]
DataWD <- fread(paste0("/srv/custom-library/data/trip_leg_matrix_2014_04_", "WD","_DDP.csv"), header = T, stringsAsFactors = F)
DataWE <- fread(paste0("/srv/custom-library/data/trip_leg_matrix_2014_04_", "WE","_DDP.csv"), header = T, stringsAsFactors = F)

TODTABLE <- data.frame(Hour = c("H00:H01",	"H01:H02",	"H02:H03",	"H03:H04",	"H04:H05",	"H05:H06",	"H06:H07",	"H07:H08",	"H08:H09",	"H09:H10",	"H10:H11",	"H11:H12",	"H12:H13",	"H13:H14",	"H14:H15",	"H15:H16",	"H16:H17",	"H17:H18",	"H18:H19",	"H19:H20",	"H20:H21",	"H21:H22",	"H22:H23",	"H23:H24"),TOD = c("NT","NT","NT","NT","NT","NT","AM","AM","AM","AM","MD","MD","MD","MD","MD","PM","PM","PM","PM","NT","NT","NT","NT","NT"))
setDT(TODTABLE)

TripPurpose <- data.table("Trip Purpose"= c("Home to Work" ,	"Work to Home",	"Home to Other",	"Other to Home",	"Other to Work",	"Work to Other",	"Home to Home",	"Other to Other",	"Work to Work"),
                          "PurposeShort" =  c("HW" ,	"WH",	"HO",	"OH",	"OW",	"WO",	"HH",	"OO",	"WW"))


#Data <- fread(paste0("./data/trip_leg_matrix_2014_04_", as.character("WD"),"_DDP.csv"), header = T, stringsAsFactors = F)

csstags <- 

ui <- dashboard_page(
  theme = 'cerulean',
  dashboard_header(strong(h2("Interactive Origin Destination Data Visualization Tool-Module 3"))),
  dashboardSidebar(disable = T),
  dashboardBody(column(8,
                       box(title = 'Selection Pane : Click on the Map to Select a TAZ',color = 'blue',ribbon = F,title_side = 'top',
                      textInput("SelectedTazGG","Selected TAZ for Grpahs",value = "Please select a TAZ from the Map",width = '100%'),
                      selectInput("date","Day Type:", choices = c("Average Weekday" = "WD", "Average Weekend" = "WE")),
                      tags$style(type = "text/css", "#DistMap {height: calc(100vh - 200px) !important;}"),
                      tags$div(title="Select a TAZ to get the distribution graphs to the right panel",leafletOutput("DistMap"))
  )),
  column(8,
         box(title = 'Daily Trip Distribution',color = 'green',ribbon = F,title_side = 'top',
             tags$style(type = "text/css", "#Hourly {height: calc((100vh - 100px)/3) !important;}"),
             plotlyOutput("Hourly"),
             tags$style(type = "text/css", "#Sbscrb {height: calc((100vh - 100px)/3) !important;}"),
             plotlyOutput("Sbscrb"),
             tags$style(type = "text/css", "#Purpose {height: calc((100vh - 100px)/3) !important;}"),
             plotlyOutput("Purpose")
         )
  ),
  tags$head(includeScript("google-analytics.js")),disable = T,
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tags$head(tags$style(
    "
    .ui.grid>* {
    padding: 0.5rem;
    }
    .ui.grid.container {
    width: 100%!important;
    }
    .ui.segment, .ui.segments .segment {
    font-size: 0.8rem;
    }
    
    .right.icon.menu{
    margin-left: 0 !important;
    width: 100%;
    align-items: center;
    justify-content: center;
    color: white;
    }
    .ui.orange.label , .ui.blue.label,.ui.violet.label,.ui.green.label,.ui.purple.label {
    padding: .5rem 1rem;
    }
    
    @media print {
    .ui.grid>* {
    padding: 0.5rem;
    }
    .ui.grid.container {
    width: 100%!important;
    }
    .ui.segment, .ui.segments .segment {
    font-size: 0.8rem;
    }
    
    .right.icon.menu{
    margin-left: 0 !important;
    width: 100%;
    align-items: center;
    justify-content: center;
    color: white;
    }
    .ui.orange.label , .ui.blue.label,.ui.violet.label,.ui.green.label,.ui.purple.label {
    padding: .5rem 1rem;
    }
    }
    
    "))
  
  )
  
  
)


server <- function(input,output,session){

    output$DistMap <- renderLeaflet({
    ShapeFile$Random <-  sample(30, size = nrow(ShapeFile), replace = TRUE)
    Qpal <- colorNumeric(palette = "Set1",domain = ShapeFile$Random)
    leaflet(ShapeFile)%>% addProviderTiles("Esri.WorldStreetMap")%>%
      addPolygons(stroke = T, fillOpacity = 0.8,fill = T,opacity = 1,weight = .5, smoothFactor = 0.5,color=~Qpal(ShapeFile$Random),layerId = ~TAZ_ID)
  }) 
  
 
 
  
  
  observeEvent(input$DistMap_shape_click,{
    leafletProxy("DistMap") %>% clearPopups()
    event <- input$DistMap_shape_click
    if (is.null(event))return()
    SelectedArea <- ShapeFile[ShapeFile$TAZ_ID == event$id,]
    
    proxy <- leafletProxy("DistMap")
    proxy%>%removeShape(layerId = "Selected")%>%removeShape("SelectLine")%>% addPolylines(data = SelectedArea,weight = 5,color = "Black",layerId = "SelectLine")%>%
      addPolygons(data = SelectedArea,fillColor = "Black",smoothFactor = .5,layerId = "Selected",opacity = 1)

    isolate({
      updateTextInput(session,"SelectedTazGG",value = event$id)
      
    })
    
    
  })

  GraphData <- reactive({
    IDatevalue <- as.character(input$date)
    if(IDatevalue == "WD"){
      DataWD
    }else if(IDatevalue == "WE"){
      DataWE
    }else{
      Day <- substr(IDatevalue,nchar(IDatevalue)-1,nchar(IDatevalue))  
      fread(paste0("./data/trip_leg_matrix_2014_04_", Day,"_DDP.csv"), header = T, stringsAsFactors = F)
    }
   
    
  })
  
  output$Hourly<- renderPlotly({
    if(input$SelectedTazGG !="Please select a TAZ from the Map"){
      SelectedTAZ <<- input$SelectedTazGG
      Data <<- GraphData()
      DataOR <- Data[as.character(Data$Origin_Zone) == SelectedTAZ,]
      DataOR <- merge(DataOR,TODTABLE,by.x = "Time_of_Day", by.y =  "Hour", all.x =T)
      
      DataDS <- Data[as.character(Data$Destination_Zone) == SelectedTAZ,]
      DataDS <- merge(DataDS,TODTABLE,by.x = "Time_of_Day", by.y =  "Hour", all.x =T)
      
      
      #Hourly Plot
      
      DataORTime <- group_by(DataOR,Time_of_Day)
      DataORTime <- summarise(DataORTime, "Number of Trips" = sum(Count))
      DataORTime$Legend <- "From Selected TAZ"
      
      
      DataDSTime <- group_by(DataDS,Time_of_Day)
      DataDSTime <- summarise(DataDSTime, "Number of Trips" = sum(Count))
      DataDSTime$Legend <- "To Selected TAZ"
      
      DataComb <- rbind(DataORTime,DataDSTime)
      DataComb$Hour <- paste0(substr(DataComb$Time_of_Day,2,3),":00")
      
      DataComb <- group_by(DataComb,Legend)
      
      plot_ly(data = DataComb) %>% 
        add_lines(x =~Hour,y = ~`Number of Trips`,color =~Legend, colors = c('red','blue')) %>% 
        layout(yaxis = list(title = 'Number of Trips'),
               legend = list(orientation = 'h',
                             x =0,y=1.2),
               font = list( size =10)
               )
      
      
    }
    
  })
  
  
  
  
  output$Sbscrb<- renderPlotly({
    if(input$SelectedTazGG !="Please select a TAZ from the Map"){
      SelectedTAZ <- input$SelectedTazGG
      Data <- GraphData()
      DataOR <- Data[as.character(Data$Origin_Zone) == SelectedTAZ,]
      DataOR <- merge(DataOR,TODTABLE,by.x = "Time_of_Day", by.y =  "Hour", all.x =T)
      
      DataDS <- Data[as.character(Data$Destination_Zone) == SelectedTAZ,]
      DataDS <- merge(DataDS,TODTABLE,by.x = "Time_of_Day", by.y =  "Hour", all.x =T)
      
      
      #Hourly Plot
      
      DataORSbcrb  <- group_by(DataOR,Subscriber_Class)
      DataORSbcrb  <- summarise(DataORSbcrb , "Number of Trips" = sum(Count))
      DataORSbcrb$Legend <- "From Selected TAZ"
      
      DataDSSbcrb  <- group_by(DataDS,Subscriber_Class)
      DataDSSbcrb  <- summarise(DataDSSbcrb , "Number of Trips" = sum(Count))
      DataDSSbcrb$Legend <- "To Selected TAZ"
      
      
      DataComb <- rbind(DataORSbcrb,DataDSSbcrb)
      DataComb$Subscriber_Class <- gsub(" ","\n",DataComb$Subscriber_Class)
      
      
      DataComb <- group_by(DataComb,Legend)
      
      plot_ly(data = DataComb) %>% 
        add_bars(x =~Subscriber_Class,y = ~`Number of Trips`,color =~Legend, colors = c('red','blue')) %>% 
        layout(yaxis = list(title = 'Number of Trips'),
               xaxis = list(title = ' Subscriber Class'),
               legend = list(orientation = 'h',
                             x =0,y=1.2),
               font = list( size =10)
        )
    }

    
  })
  
  output$Purpose <- renderPlotly({
    if(input$SelectedTazGG !="Please select a TAZ from the Map"){
      SelectedTAZ <- input$SelectedTazGG
      Data <- GraphData()
      DataOR <- Data[as.character(Data$Origin_Zone) == SelectedTAZ,]
      DataOR <- merge(DataOR,TODTABLE,by.x = "Time_of_Day", by.y =  "Hour", all.x =T)
      
      DataDS <- Data[as.character(Data$Destination_Zone) == SelectedTAZ,]
      DataDS <- merge(DataDS,TODTABLE,by.x = "Time_of_Day", by.y =  "Hour", all.x =T)
      
      
      #Hourly Plot
      
      DataORPurpose  <- group_by(DataOR,Purpose)
      DataORPurpose  <- summarise(DataORPurpose  , "Number of Trips" = sum(Count))
      DataORPurpose$Legend <- "From Selected TAZ"
      
      DataDSPurpose   <- group_by(DataDS,Purpose)
      DataDSPurpose   <- summarise(DataDSPurpose  , "Number of Trips" = sum(Count))
      DataDSPurpose$Legend <- "To Selected TAZ"
      
      
      DataComb <- rbind(DataORPurpose ,DataDSPurpose )
      DataComb <- merge(DataComb,TripPurpose, by.x = "Purpose", by.y = "PurposeShort", all.x =T)
      
      
      DataComb$`Trip Purpose` <- gsub(" ","\n",DataComb$`Trip Purpose`)
      
      
      DataComb <- group_by(DataComb,Legend)
      
      plot_ly(data = DataComb) %>% 
        add_bars(x =~`Trip Purpose`,y = ~`Number of Trips`,color =~Legend, colors = c('red','blue')) %>% 
        layout(yaxis = list(title = 'Number of Trips'),
               xaxis = list(title = ' Trip Purpose'),
               legend = list(orientation = 'h',
                             x =0,y=1.2),
               font = list( size =10)
        )
      
      
    }
    

    
  })
  
  session$onSessionEnded(function() {
    stopApp()
  }) 
  
}

shinyApp(ui, server)