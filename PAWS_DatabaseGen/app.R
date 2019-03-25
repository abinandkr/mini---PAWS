library(shiny)
library(camtrapR)
library(lubridate)
library(PBSmapping)
library(rdrop2)
library(tidyverse)
library(shinyalert)
library(secr)


srcdir <- getwd()
#Authorise dropbox login
drop_auth()
current.folder <- paste0('C:/Users/',Sys.getenv('USERNAME'),'/Dropbox (Snow Leopard Trust)/PAWS/All/')

ui <- fluidPage(
  
  # Application title
  titlePanel("PAWS"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style(type="text/css", "
                           #loadmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #4986CF;
                           z-index: 105;
                           }
                           ")),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Working on it...",id="loadmessage")),
      helpText('This app has been made exclusively for PAWS work on the SLT remote server for streamlining camera trap analysis. Please make sure you have read the ReadMe file before continuing.'),
      selectInput('country','Select Country:', choices = list.files(current.folder)),
      uiOutput('loc_ui'),
      uiOutput('year_ui'),
      actionButton('setdir','Set Directory'),
      helpText('In case of any bugs or suggestions please email them to abinand@ncf-india.org')
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput('message'),
      useShinyalert(),
      htmlOutput('trapcheckmsg'),
      htmlOutput('trapcolcheckmsg'),
      uiOutput('template_ui'),
      htmlOutput('trap_fold_checkmsg'),
      htmlOutput('mismatch_override_ui'),
      HTML('</br>'),
      uiOutput('create_spcap_button'),
      HTML('</br>'),
      htmlOutput('create_spcap_text'),
      htmlOutput('spcap_complete'),
      uiOutput('create_slcap_button'),
      HTML('</br>'),
      htmlOutput('create_slcap_text'),
      htmlOutput('slcap_complete'),
      uiOutput('reportButton')
      
    )
    )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  checks <- reactiveValues(flow = 0,temp.trig = 0,mismatch = 0) 
  sp_overwrite <- reactiveValues(exists = 0,confirm = FALSE)
  sl_overwrite <- reactiveValues(exists = 0,confirm = FALSE)
  output$loc_ui <- renderUI({
    selectInput('location','Select Location:', choices = list.files(paste(current.folder,input$country,sep = '/')))
  })
  
  output$year_ui <- renderUI({
    selectInput('year','Select Year:', choices = list.files(paste(current.folder,input$country,input$location,sep = '/')))
  }) 
  
  setdir <- eventReactive(input$setdir,({
    setwd(paste(current.folder,input$country,input$location,input$year,sep = '/'))
    files <- rbind(file.info(list.files(getwd())))
    folds <- files %>% filter(isdir == TRUE)
    checks$flow <- 0
    checks$temp.trig <- 0
    checks$mismatch <- 0
    sp_overwrite$exists <- 0
    sp_overwrite$confirm <- FALSE
    sl_overwrite$exists <- 0
    sl_overwrite$confirm <- FALSE
    paste0('Directory set to: ',getwd(),'</br></br>','There are ', nrow(folds), ' folders','</br>')
  }))
  
  output$message <- renderText({setdir()})
  
  trapfile <- reactive({     
    setdir()
    list.files(pattern = "*trap_info*.csv")
  })
  
  trapfilecheck <- reactive({
    setdir()
    if(length(trapfile()) > 1){msg1 <- paste0('<font color=\"#FF0000\">','Multiple trap information file found. Please remove dupliacates and rerun','</font')}
    if(length(trapfile()) == 1){
      msg1 <- paste0('Trap information file found : ',trapfile())
      isolate(checks$flow <- 1)
    }
    else{
      checks$temp.trig <- 1
      msg1 <- paste0('<font color=\"#FF0000\">','No trap information file found. Please ensure the file name contains the term "file_info" or download the template, add the data and upload to the directory','</font')
    }
    paste0('</br>',msg1)
  })
  
  output$trapcheckmsg <- renderText({trapfilecheck()})
  
  trapdat <- reactiveValues()
  
  observe({
    if(checks$flow == 1 ){
      ct_dat <- read.csv(trapfile())
      Y<-ct_dat$Latitude 
      X<-ct_dat$Longitude
      latlong<-cbind(X,Y)
      attr(latlong, "projection") <- "LL"
      attr(latlong, "zone") <- NA
      coord.utm <- convUL(latlong, km=F)
      ct_dat$utm_x<-coord.utm$X
      ct_dat$utm_y<-coord.utm$Y
      if(is.null(ct_dat$Setup_date) == T & is.null(ct_dat$Setup_day) == F){
        ct_dat$Setup_date <- as.Date(paste(ct_dat$Setup_year,ct_dat$Setup_month,ct_dat$Setup_day,sep ='-'))
      }
      if (is.null(ct_dat$Setup_date) == F){
        ct_dat$Setup_date <- parse_date_time(ct_dat$Setup_date, orders = c("%d/%m/Y", "%d-%m-Y", "Y-%m-%d"))
      }
      if(is.null(ct_dat$Retrieval_date) == T & is.null(ct_dat$Retrieval_day) == F){
        ct_dat$Retrieval_date <- as.Date(paste(ct_dat$Retrieval_year,ct_dat$Retrieval_month,ct_dat$Retrieval_day,sep ='-'))
      }
      if (is.null(ct_dat$Retrieval_date) == F){
        ct_dat$Retrieval_date <- parse_date_time(ct_dat$Retrieval_date, orders = c("%d/%m/Y", "%d-%m-Y", "Y-%m-%d"))
      }
      trapdat$data <- ct_dat
    }
  })
  
  output$trapcolcheckmsg <- renderText({
    if(checks$flow == 1){
      reqfields <- c('Area','Station','Latitude','Longitude','Setup_date','Retrieval_date')
      if(all(reqfields %in% colnames(trapdat$data))==F){
        miscol <- reqfields[!reqfields %in% colnames(trapdat$data)]
        msg1 <- paste0('<font color=\"#FF0000\">','"',miscol, '" column is either missing or has to be renamed','</font>')
      }else{
        msg1 <- 'All required fields present'
        isolate(checks$flow <- 2)
      }
      paste0('</br>',msg1)
    }
  })
  
  output$template_ui <- renderUI({
    if(checks$temp.trig == 1){
      downloadButton('template','Download template')
    }
  })
  
  output$template <- downloadHandler(
    filename = function() {
      paste('template',input$country,input$location,input$year,'trap_info.csv', sep='_')
    },
    content = function(con) {
      files <- rbind(file.info(list.files(getwd())))
      files$name <- rownames(files)
      folds <- files %>% filter(isdir == TRUE)
      dat <- data.frame(matrix(ncol = 10,nrow = nrow(folds)))
      colnames(dat) <- c('Area','Station','Latitude','Longitude','Setup_day','Setup_month','Setup_year','Retrieval_day','Retrieval_month','Retrieval_year')
      dat$Station <- as.character(folds$name)
      dat$Area <- input$location
      write.csv(dat, con, row.names = F)
    })
  
  output$trap_fold_checkmsg <- renderText({
    validate(need(checks$flow >= 2,''))
    files <- rbind(file.info(list.files(getwd())))
    files$name <- rownames(files)
    fldrs <- files %>% filter(isdir == TRUE) %>% pull(name)
    stations <- trapdat$data$Station
    stat_miss <- stations[!stations %in% fldrs]
    fldr_miss <- fldrs[!fldrs %in% stations]
    
    if(length(stat_miss) == 0 & length(fldr_miss) == 0){
      msg <- 'Folders and trap locations match'
      if(checks$flow <3){isolate(checks$flow <- 3)}
    }else{
      msg1 <- '<font color=\"#FF0000\">There is a mismatch in the folder names and trap locations. Please review spellings and delete extra white spaces.</font>'
      msg2 <- ifelse(length(stat_miss) == 0,'</br>',paste('The following Station names in the trap_info files are unmatched:',paste0('<b>',paste(stat_miss, collapse = '</br>'),'</b>'),sep = '</br>'))
      msg3 <- ifelse(length(fldr_miss) == 0,'</br>',paste('The following folder names in the directory are unmatched:',paste0('<b>',paste(fldr_miss, collapse = '</br>'),'</b>'),sep = '</br>'))
      msg <- paste(msg1,msg2,msg3,sep = '</br>')
      isolate(checks$mismatch <- 1)
    }
    paste0('</br>',msg)
  })
  
  output$mismatch_override_ui <- renderUI({
    if(checks$mismatch == 1){
      actionButton('mismatch_override','Continue Anyway')
    }
  })
  
  observeEvent(input$mismatch_override,{
    checks$flow <- 3
  })
  
  output$create_spcap_button <- renderUI({
    if(checks$flow >= 3){
      actionButton('create_spcap','Create Species Capture File')
    }
  })
  
  observeEvent(input$create_spcap,{
    if(file.exists(paste('Species',input$country,input$location,input$year,'csv', sep = '.'))){
      shinyalert(title = 'Overwrite',text ='The species capture file already exists, would you like to overwrite it?',showConfirmButton = T,showCancelButton = T,
                 callbackR = function(x){sp_overwrite$confirm <- x},confirmButtonText = 'Overwrite',cancelButtonText = 'Skip')
      isolate(sp_overwrite$exists <- 1)
    }else{
      isolate(sp_overwrite$exists <- 2) 
    }
    isolate(checks$flow <- 4)
  })
  
  
  output$create_spcap_text <- renderText({
    validate(need(sp_overwrite$confirm == TRUE | sp_overwrite$exists == 2,''))
    'Creating Species Capture Database...'
  })
  
  
  
  output$spcap_complete <- renderText({
    validate(need(sp_overwrite$confirm == TRUE | sp_overwrite$exists == 2,''))
    if(checks$flow == 4){
      setdir()
      withCallingHandlers({species <- recordTable(inDir= getwd(),
                                                  IDfrom = "metadata",
                                                  cameraID = "filename",
                                                  camerasIndependent = TRUE,
                                                  exclude = c("Unidentified","Chukar","Snowcock","Tibetan Snowcock","Chukar Patridge","Chukar Partridge","SLK","Other Birds","Rodent","People", "empty","Livestock"),
                                                  minDeltaTime = 60,
                                                  deltaTimeComparedTo = "lastIndependentRecord",
                                                  timeZone = "Asia/Kolkata",
                                                  stationCol = "Station",
                                                  writecsv = FALSE,
                                                  outDir = getwd(),
                                                  metadataHierarchyDelimitor = "|",
                                                  metadataSpeciesTag = "Species",
                                                  removeDuplicateRecords = TRUE)
      }, 
      warning = function(m) {
        showNotification(m$message,duration = 15)
      })
      write.csv(species, paste("Species", input$country, input$location, input$year, "csv", sep="."))
      isolate(checks$flow <- 5)
    }
    msg <- paste0('Species capture database created in directory as : ',paste("Species", input$country, input$location, input$year, "csv", sep="."))
    return(paste0('</br>',msg))
  })
  
  observe({
    if(checks$flow == 4 & sp_overwrite$confirm == FALSE & sp_overwrite$exists == 1){
      isolate(checks$flow <- 5)
    }
  })
  
  
  output$create_slcap_button <- renderUI({
    if(checks$flow >= 5){
      actionButton('create_slcap','Create Snow Leopard Capture File')
    }
  })
  
  species <- reactive({
    if(checks$flow >= 5){
      setdir()
      read.csv(paste0(getwd(),'/',paste("Species", input$country, input$location, input$year, "csv", sep=".")))
    }
  })
  
  
  observeEvent(input$create_slcap,{
    
    if(drop_exists(paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,sep = '/'))){
      shinyalert(title = 'Overwrite',text ='The snow leopard individual capture file already exists, would you like to overwrite it?',showConfirmButton = T,showCancelButton = T,
                 callbackR = function(x){sl_overwrite$confirm <- x},confirmButtonText = 'Overwrite',cancelButtonText = 'Skip')
      isolate(sl_overwrite$exists <- 1)
    }else{
      isolate(sl_overwrite$exists <- 2) 
    }
    isolate(checks$flow <- 6)
  })
  
  
  output$create_slcap_text <- renderText({
    validate(need(sl_overwrite$confirm == TRUE | sl_overwrite$exists == 2,''))
    'Creating Snow leopard Capture Database...'
  })
  
  output$slcap_complete <- renderText({
    validate(need(sl_overwrite$confirm == TRUE | sl_overwrite$exists == 2,''))
    if(checks$flow == 6){
      setdir()
      getSpeciesImages(species = "Snow leopard",
                       recordTable= species(),
                       speciesCol = "Species",
                       stationCol = "Station",
                       outDir= getwd(),
                       createStationSubfolders = FALSE,
                       IDfrom = "Species",
                       metadataSpeciesTag = "Snow leopard",
                       metadataHierarchyDelimitor = "|")
      
      individuals <- recordTableIndividual(inDir = paste0(getwd(), "/Snow leopard"),
                                           minDeltaTime = 60, deltaTimeComparedTo = "lastIndependentRecord", 
                                           hasStationFolders = FALSE, IDfrom = "metadata", writecsv = FALSE, 
                                           outDir = getwd(), 
                                           metadataHierarchyDelimitor = "|", metadataIDTag = "Individuals", 
                                           timeZone = "Asia/Kolkata", removeDuplicateRecords =  TRUE)
      
      
      write.csv(individuals, paste("individuals", input$country, input$location, input$year, "csv", sep="."))
      
      if(drop_exists(paste('PAWS/Snow Leopard Individuals',input$country,sep = '/')) == F) drop_create(paste('PAWS/Snow Leopard Individuals',input$country,sep = '/'))
      if(drop_exists(paste('PAWS/Snow Leopard Individuals',input$country,input$location,sep = '/')) == F) drop_create(paste('PAWS/Snow Leopard Individuals',input$country,input$location,sep = '/'))
      if(drop_exists(paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,sep = '/')) == T) {
        drop_delete(paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,sep = '/'))
        drop_create(paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,sep = '/'))
      } else {(drop_create(paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,sep = '/')))}
      drop_move(paste('PAWS/All',input$country,input$location,input$year,'Snow leopard',sep = '/'),paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,'Snow leopard',sep = '/'))
      drop_move(paste('PAWS/All',input$country,input$location,input$year,paste("individuals", input$country, input$location, input$year, "csv", sep="."),sep = '/'),paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,paste("individuals", input$country, input$location, input$year, "csv", sep="."),sep = '/'))
    }
    isolate(checks$flow <- 7)
    msg <- paste0('Snow leopard individual capture database created as ',paste("individuals", input$country, input$location, input$year, "csv", sep="."))
    return(paste0('</br>',msg))
  })
  
  observe({
    if(checks$flow == 6 & sl_overwrite$confirm == FALSE & sl_overwrite$exists == 1){
      isolate(checks$flow <- 7)
    }
  })
  
  sldat <- reactiveValues()
  
  observe({
    if(checks$flow ==7){
      ind <- drop_read_csv(paste('PAWS/Snow Leopard Individuals',input$country,input$location,input$year,paste("individuals", input$country, input$location, input$year, "csv", sep="."),sep = '/'))
      sldat$data <- ind
    }
  })
  
  output$reportButton <- renderUI({
    if(checks$flow == 7){
      downloadButton('report','Download Report')
    }
  })
  
  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      
      tempReport <- file.path(srcdir, "report.Rmd")
      
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(species = species(),ct_dat = trapdat$data, individuals = sldat$data, place = input$location, year = input$year)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



