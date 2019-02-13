##Install and load packages CamtrapR, lubridate, PBSmapping
#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("camtrapR")
#install.packages("lubridate")
#install.packages("PBSmapping")
#rm(list =ls())
library(camtrapR)
library(lubridate)
library(PBSmapping)

#Paste parent folder containing secr database into parentfolder1
parent.folder1<-"C:/Users/user1/Documents/Dropbox (Snow Leopard Trust)/PAWS/All/"


#Paste parent folder containing secr individuals database into parentfolder2
parent.folder2<- "C:/Users/user1/Documents/Dropbox (Snow Leopard Trust)/PAWS/Snow Leopard Individuals/"

#view the country list within secr database
list.files(parent.folder1)

#Enter the country within quotes
country <- "India"

#View the locations within country
list.files(paste0(parent.folder1, country, sep="/"))

#Enter in location/site to be analyzed within quotes
location<- "Bhaga"

#View years within locations
list.files(paste0(parent.folder1, country, sep="/", location))

#Enter in year within quotes
year<- "2018"

#set this as working directory
directory1<-paste0(parent.folder1, country, sep="/", location, sep="/", year, sep="/")
setwd(directory1)

#check the working directory
getwd()

#list the trap information file
ctfile<-list.files(pattern = "*trap_info*.csv")
ctfile

#read the trap_info file
ct_dat<-read.csv(ctfile, header=T)
head(ct_dat)

#change setup and retrieval dates to yyyy-mm-dd format
ct_dat$Setup_date <- as.Date(paste(ct_dat$Setup_year,ct_dat$Setup_month,ct_dat$Setup_day,sep = '-'))
ct_dat$Retrieval_date <- as.Date(paste(ct_dat$Retrieval_year,ct_dat$Retrieval_month,ct_dat$Retrieval_day,sep = '-'))
head(ct_dat)

#To paste location and year from the trap_info file name. requires stringr package.
#a<-regexpr(pattern="[0-9]", ctfile)
#location<-str_sub(ctfile, start=1, end=a-2)
#year<-str_sub(ctfile, start=a, end=a+3)
#paste("species", location, year, sep = ".")

#snippet to connvert Lat Long to utm_x and utm_y
Y<-ct_dat$Latitude 
X<-ct_dat$Longitude
latlong<-cbind(X,Y)

#specify projection and zone attributes
attr(latlong, "projection") <- "LL"
attr(latlong, "zone") <- NA

#converts to utm coordinates, requires PBSmapping
coord.utm <- convUL(latlong, km=F)
head(coord.utm)

#add utm column to the ct_dat object
ct_dat$utm_x<-coord.utm$X
ct_dat$utm_y<-coord.utm$Y
head(ct_dat)

#Save the utm columns to trap_info file if required
#list.files(pattern = "*trap_info*.csv")
#write.csv(ct_dat, list.files(pattern = "*trap_info*.csv"))

#create species database using function recordTable
species <-  recordTable(inDir= getwd(),
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
View(species)

#save the species file in the working directory
write.csv(species, paste("species", location, year, "csv", sep="."))

#set the new working directory to site within individuals folder structure
directory2<- paste0(parent.folder2, country, sep= "/", location, sep= "/", year, sep="/")
setwd(directory2)
getwd()

#create a Snow leopard folder within the new working directory
getSpeciesImages(species = "Snow leopard",
                 recordTable= species,
                 speciesCol = "Species",
                 stationCol = "Station",
                 outDir= getwd(),
                 createStationSubfolders = FALSE,
                 IDfrom = "Species",
                 metadataSpeciesTag = "Snow leopard",
                 metadataHierarchyDelimitor = "|")

#create a record table for snow leopard individuals in the working directory
individuals <- recordTableIndividual(inDir = paste0(getwd(), "/Snow leopard"),
                                     minDeltaTime = 60, deltaTimeComparedTo = "lastIndependentRecord", 
                                     hasStationFolders = FALSE, IDfrom = "metadata", writecsv = FALSE, 
                                     outDir = getwd(), 
                                     metadataHierarchyDelimitor = "|", metadataIDTag = "Individuals", 
                                     timeZone = "Asia/Kolkata", removeDuplicateRecords =  TRUE)

#remove unidentified individuals
individuals<-individuals[!(individuals$Individual %in% c("unidentified", "Unidentified", "Tabo 7", "Tabo 8", "Tabo 9" )), ]
View(individuals)

#save individuals into a csv file
write.csv(individuals, paste("individuals", location, year, "csv", sep="."))

#generate species detection histories for occupancy analyses
detectionMaps(CTtable = ct_dat, recordTable = species, Xcol = "utm_x", Ycol = "utm_y", stationCol = "Station",
              speciesCol = "Species", richnessPlot = TRUE, speciesPlots = TRUE, addLegend = TRUE,  printLabels = FALSE,
              writePNG = F, plotR=T, plotDirectory = FALSE)

#create histogram of single species activity
activityHistogram(recordTable=species, 
                  allSpecies = TRUE, 
                  speciesCol = "Species", 
                  recordDateTimeCol = "DateTimeOriginal", 
                  recordDateTimeFormat = "%Y-%m-%d %H:%M:%S")

#create a report on camera trap survey and species detections
Report<-surveyReport(recordTable = species, CTtable =  ct_dat, speciesCol = "Species", stationCol = "Station", setupCol = "Setup_date",
                     retrievalCol = "Retrieval_date", CTDateFormat = "%Y-%m-%d",  CTHasProblems = FALSE, recordDateTimeCol = "DateTimeOriginal",
                     recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", Xcol = "utm_x", Ycol = "utm_y", makezip = FALSE)

Report

