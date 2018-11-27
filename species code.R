library(camtrapR)
library(secr)

setwd()
getwd()

ctfile<-list.files(pattern = "*trap_info*.csv")

ct_dat<-read.csv(ctfile, header=T)
head(ct_dat)

fixDateTimeOriginal(inDir = getwd(), recursive = TRUE) 

imageRename(inDir = getwd(), 
            outDir = NULL, 
            hasCameraFolders = FALSE, keepCameraSubfolders = FALSE, copyImages = TRUE, writecsv = TRUE)

recordTable(inDir= getwd(),
            IDfrom = "metadata",
            cameraID = "filename",
            camerasIndependent = TRUE,
            exclude = c("Unidentified","Chukar","Snowcock","Tibetan Snowcock","Chukar Patridge","Chukar Partridge","SLK","Other Birds","Rodent","People", "empty","Livestock"),
            minDeltaTime = 60,
            deltaTimeComparedTo = "lastIndependentRecord",
            timeZone = "Asia/Kolkata",
            stationCol = "Station",
            writecsv = TRUE,
            outDir = getwd(),
            metadataHierarchyDelimitor = "|",
            metadataSpeciesTag = "Species",
            removeDuplicateRecords = TRUE)