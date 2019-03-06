library(shiny)
library(rdrop2)
library(tidyverse)
library(sf)
library(sp)
library(leaflet)

drop_auth()   ## Login to the PAWS dropbox folder


countries <- drop_dir('PAWS/ALL') %>% filter(.tag == 'folder') %>% pull(name)

#locations <- drop_dir(paste0('PAWS/ALL/',input$country)) %>% filter(.tag == 'folder') %>% pull(name)

ui <- fluidPage(
  
  # Application title
  titlePanel("PAWS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('country','Select Country', choices = countries),
      uiOutput('location_ui'),
      actionButton('check','Update Map'),
      conditionalPanel('length(input.location) == 1',uiOutput('year_ui')),
      uiOutput('speciesUI')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #dataTableOutput("test")
      leafletOutput('species_map')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$location_ui <- renderUI({
    locations <- drop_dir(paste0('PAWS/ALL/',input$country)) %>% filter(.tag == 'folder') %>% pull(name)
    selectInput('location','Select Location',choices = locations, multiple = T, selected = locations[1])
  })
  
  output$year_ui <- renderUI({
    validate(need(input$check > 0, 'Press Button'))
    if(length(input$location) == 1){
      years <- drop_dir(paste0('PAWS/ALL/',input$country,'/',input$location)) %>% filter(.tag == 'folder') %>% pull(name)
      selectInput('year','Select year',choices = years)
    }
  })
  
  datafiles <- function(){
    input$check
    isolate(
    drop_dir(paste0('PAWS/All/',input$country)) %>%
      filter(name %in% input$location) %>% 
      rowwise %>% 
      do(drop_dir(.$path_lower)) %>% 
      filter(.tag == 'folder') %>% 
      rowwise %>% 
      do(drop_dir(.$path_lower)) %>% 
      filter(.tag == 'file') 
    )
  }
  

  trap_info <- reactive({
      do.call(bind_rows,lapply(datafiles() %>% 
                                 filter(grepl('trap_info',path_lower)) %>% 
                                 pull(path_display), drop_read_csv)) %>% 
      select(Station, Latitude, Longitude, Area)
  })
  
  captures <- reactive({
    do.call(bind_rows,lapply(datafiles() %>% 
                               filter(grepl('species',path_lower)) %>% 
                               pull(path_display), drop_read_csv)) %>% 
      select(Station,Camera,Species,Directory, FileName) %>% 
      mutate(date = sub('.*__ *(.*?) *__.*','\\1',FileName)) %>%
      mutate(link = paste0('<a href=',gsub(' ','%20',paste0('https://www.dropbox.com/home',gsub("^.*\\Trust)", "", Directory))),'?preview=',gsub(' ','+',FileName),'>',date,'</a>')) %>%
      group_by(Station,Species) %>% summarise(n = n(), link = paste(link, collapse = '<br/>'))
  })
  
  output$speciesUI <- renderUI({
    validate(need(input$check > 0, ''))
    selectInput('species','Select Species:',choices = unique(captures()$Species))
  })
  

  output$species_map <- renderLeaflet({
    validate(need(input$check > 0, ''))
    dat <- left_join(trap_info(),captures() %>% filter(Species == input$species))
    dat$n <- as.numeric(ifelse(is.na(dat$n),0,dat$n))
    dat$link <- ifelse(is.na(dat$link),'',dat$link)
    dat$link <- paste(paste0('<b>',dat$Area,'</b>'),dat$Station,dat$link,sep = '<br/>')
    pal <- colorNumeric('Reds',domain = dat$n, na.color = 'grey')
    leaflet(dat) %>% 
      addTiles(group = 'OSM') %>%
      addProviderTiles(providers$Esri.DeLorme, group = 'Terrain') %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = 'Sattelite') %>% 
      addCircleMarkers(lng = ~Longitude,lat = ~Latitude, fill = T,fillColor = ~pal(n), stroke = T,color = 'black', fillOpacity = .8, radius = 4, weight = 1, popup = ~paste(link)) %>% 
      addLegend('bottomright',pal = pal,values = ~n,title = 'Captures') %>% 
      addLayersControl(baseGroups = c('OSM','Terrain','Sattelite'))
    
    
  })

}


shinyApp(ui = ui, server = server)

