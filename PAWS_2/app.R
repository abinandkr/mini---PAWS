library(shiny)
library(rdrop2)
library(tidyverse)
library(sf)
library(sp)
library(leaflet)
library(shinycssloaders)



drop_auth(rdstoken = "tokenfile.RDS")

getFolders <- function(path,type){
  drop_dir(path) %>% filter(.tag == type) %>% select(name,path_display)
}

files <- drop_dir('PAWS/ALL') %>% 
  filter(.tag == 'folder') %>% 
  select(name,path_display) %>% 
  group_by(name) %>% 
  rename(Country = name) %>% 
  nest() %>% 
  mutate(location = map(data, ~getFolders(.$path_display,'folder'))) %>% 
  unnest(location) %>% 
  rename(Location = name) %>%
  group_by(Country,Location) %>% 
  nest() %>% 
  mutate(Year = map(data, ~getFolders(.$path_display,'folder'))) %>% 
  unnest(Year) %>% 
  rename(Year = name) %>% 
  group_by(Country,Location,Year) %>% 
  nest() %>% 
  mutate(Files = map(data, ~getFolders(.$path_display,'file'))) %>% 
  unnest(Files) %>% 
  rename(Files = name)


ui <- fluidPage(
  
  # Application title
  titlePanel("PAWS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('country','Select Country', choices = unique(files$Country)),
      uiOutput('location_ui'),
      conditionalPanel('length(input.location) <= 1',uiOutput('year_ui')),
      actionButton('check','Update Map'),
      uiOutput('speciesUI')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      withSpinner(leafletOutput('species_map'),type = 6)
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$location_ui <- renderUI({
    locations <- files %>% filter(Country == input$country) %>% pull(Location)
    selectInput('location','Select Location',choices = unique(locations), multiple = T, selected = locations[1])
  })
  
  output$year_ui <- renderUI({
    if(length(input$location) == 1){
      years <- files %>% filter(Country == input$country, Location == input$location) %>% pull(Year)
      selectInput('year','Select year',choices = unique(years))
    }
  })
  
  dropdat <- reactiveValues()
  
  observeEvent(input$check,{
      datafiles <- drop_dir(paste0('PAWS/All/',input$country)) %>%
        filter(name %in% input$location) %>% 
        rowwise %>% 
        do(drop_dir(.$path_lower)) %>% 
        filter(.tag == 'folder') %>% 
        rowwise %>% 
        do(drop_dir(.$path_lower)) %>% 
        filter(.tag == 'file') 
      
      dropdat$trap_info <- do.call(bind_rows,lapply(datafiles %>% 
                                 filter(grepl('trap_info',path_lower)) %>% 
                                 pull(path_display), drop_read_csv)) %>% 
        select(Station, Latitude, Longitude, Area)
      
      dropdat$captures <- do.call(bind_rows,lapply(datafiles %>% 
                                                  filter(grepl('species',path_lower)) %>% 
                                                  pull(path_display), drop_read_csv)) %>% 
        select(Station,Camera,Species,Directory, FileName) %>% 
        mutate(date = sub('.*__ *(.*?) *__.*','\\1',FileName)) %>%
        mutate(link = paste0('<a href=',gsub(' ','%20',paste0('https://www.dropbox.com/home',gsub("^.*\\Trust)", "", Directory))),'?preview=',gsub(' ','+',FileName),' target = "_blank">',date,'</a>')) %>%
        group_by(Station,Species) %>% summarise(n = n(), link = paste(link, collapse = '<br/>'))

      file.remove(paste0(tempdir(),'/',list.files(tempdir())))
  })
  
  output$speciesUI <- renderUI({
    validate(need(is.null(dropdat$captures) == F, ''))
    selectInput('species','Select Species:',choices = unique(dropdat$captures$Species))
  })
  
  output$species_map <- renderLeaflet({
    validate(need(is.null(dropdat$captures) == F & is.null(input$species) == F, ''))
    dat <- left_join(dropdat$trap_info,dropdat$captures %>% filter(Species == input$species))
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

