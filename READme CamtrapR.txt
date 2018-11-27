1. load packages camtrapR and secr using library()

Usage:			       library(camtrapR)
                               library(secr)

2. set up working directory to camera trap folder location using setwd(). 
Usage: 			       setwd("C:/User/Documents/Pin Valley")

3. get the current working directory using getwd(), without any arguments.
Usage: 			       getwd()
 
4. find camera trap csv file (saved as object ctfile) using list.files() [it finds those csv files that contain the words *trap_info*]
Usage:			       ctfile<-list.files(pattern = "*trap_info*.csv")

5. read ctfile, make sure there is a unique trap_info csv in reach location folder
Usage: 			       ct_dat<-read.csv(ctfile, header=T)

6. run this command for new data to fix errors in data time for camtrapR
Usage:   		       fixDateTimeOriginal(inDir = getwd(), recursive = TRUE) 

7. renames images according to location folder names. enter the out directory (outDir), where renamed folders need to be saved. It should be different from in directory (inDir). The argument copyImages must be kept as TRUE.

Usage:                         imageRename(inDir = getwd(), 
            		       outDir = NULL, #change the outDir, the rest remains the same
            		       hasCameraFolders = FALSE, keepCameraSubfolders = FALSE, copyImages = TRUE, writecsv = TRUE)

8. create a species record table from camera trap images using recordtable(). The function can be used without any changes. 
Usage:			       recordTable(inDir= getwd(),
                               IDfrom = "metadata",
                               cameraID = "filename",
                               camerasIndependent = TRUE,
                               exclude = c("Unidentified","Chukar","Snowcock","SLK","Pika","Other Birds","Rodent","People", "empty"),
                               minDeltaTime = 60,
                               deltaTimeComparedTo = "lastIndependentRecord",
                               timeZone = "Asia/Kolkata",
                               stationCol = "Station",
                               writecsv = TRUE,
                               outDir = getwd(),
                               metadataHierarchyDelimitor = "|",
                               metadataSpeciesTag = "Species",
                               removeDuplicateRecords = TRUE)

# this function creates a species record table and saves it in the working directory as recordTable_date.

Arguments for recordTable:

inDir - Directory containing station folders. can be obtained using getwd()

IDfrom - Read species ID from image metadata ("metadata") 

cameraID - Where should the function look for camera IDs: "filename". "filename" requires images renamed with imageRename.

camerasIndependent - If TRUE, species records are considered to be independent between cameras at a station.

exclude - Vector of species names to be excluded from the record table.

minDeltaTime - Time difference between records of the same species at the same station to be considered independent (in minutes)

deltaTimeComparedTo - For two records to be considered independent, must the second one be at least minDeltaTime minutes after the last independent record of the same species ("lastIndependentRecord"), or minDeltaTime minutes after the last record ("lastRecord")?

timeZone - Input your time zome. Must be an argument of OlsonNames

stationCol - Name of the camera trap station column. Assumes "Station" if undefined.

writecsv - Should the record table be saved as a .csv?

outDir - Directory to save csv to. If NULL and writecsv = TRUE, recordTable will be written to inDir.

metadataHierarchyDelimitor - The character delimiting hierarchy levels in image metadata tags in field "HierarchicalSubject". Either "|" or ":".

metadataSpeciesTag - In custom image metadata, the species ID tag name.

removeDuplicateRecords - If there are several records of the same species at the same station (also same camera if cameraID is defined) at exactly the same time, show only one?

returnFileNamesMissingTags - If species are assigned with metadata and images are not tagged, return a few file names of these images as a message?
