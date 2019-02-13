##Install and load packages CamtrapR, stringr and secr
options(repos = c(CRAN = "http://cran.rstudio.com"))

#install.packages("rgdal")

library(camtrapR)
library(secr)
library(rgdal)

#Paste both the parent folders containing secr database and individuals folder respectively
parent.folder1<-"C:/Users/user1/Documents/Camera trap SECR database/"

parent.folder2<- "C:/Users/user1/Documents/Camera trap Individuals database/"

#view the country list within secr database
list.files(parent.folder1)

#Enter the country within quotes
country <- "India"

#View the locations within country
list.files(paste0(parent.folder1, country, sep="/"))

#Enter in location/site to be analyzed within quotes
location<- "USL"

#View years within locations
list.files(paste0(parent.folder1, country, sep="/", location))

#Enter in year within quotes
year<- "2017"

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
View(ct_dat)

#change setup and retrieval dates to yyyy-mm-dd format
ct_dat$Setup_date <- parse_date_time(ct_dat$Setup_date, orders = c("%d/%m/Y", "%d-%m-Y", "Y-%m-%d"))
ct_dat$Retrieval_date <- parse_date_time(ct_dat$Retrieval_date, orders = c("%d/%m/Y", "%d-%m-Y", "Y-%m-%d"))
View(ct_dat)

#a<-regexpr(pattern="[0-9]", ctfile)
#location<-str_sub(ctfile, start=1, end=a-2)
#year<-str_sub(ctfile, start=a, end=a+3)
#paste("species", location, year, sep = ".")

#read species csv file
species<- read.csv(paste("species", location, year, "csv", sep="."))

#Create camera operatibility matrix using setup and retrieval dates
CamMatrix<-cameraOperation(CTtable = ct_dat, stationCol = "Station", setupCol = "Setup_date", retrievalCol = "Retrieval_date", 
                           byCamera = FALSE, allCamsOn = TRUE, dateFormat = "%Y-%m-%d", hasProblems = F, writecsv = FALSE, 
                           outDir = getwd())

#change dirctory to individuals database
directory2<- paste0(parent.folder2, country, sep= "/", location, sep= "/", year, sep="/")
setwd(directory2)
getwd()

#read individuals csv file
individuals<- read.csv(paste("individuals", location, year, "csv", sep="."))
head(individuals)

#create capture/detection histories
input_secr<-spatialDetectionHistory(recordTableIndividual =  individuals, species = "Snow leopard", camOp = CamMatrix, CTtable = ct_dat, output = "binary",
                                    stationCol = "Station", speciesCol = "Species", Xcol = "utm_x", Ycol = "utm_y", individualCol = "Individual", 
                                    recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", occasionLength = 1, occasionStartTime = 12, 
                                    day1 = "survey", includeEffort = TRUE, binaryEffort = TRUE, timeZone = "Asia/Kolkata")
#summary of detection history
summary(input_secr, terse = TRUE)

#create arbitrary mask around the traps
mask <- make.mask(traps(input_secr), buffer = 28000, type = 'trapbuffer')

#estimate snow leopard population density by model fitting
fit<-secr.fit(input_secr, mask = mask, model = g0~1, start = list(D=.00002, g0= 0.02, sigma= 25000), trace = FALSE)
#esa.plot(fit, ylim=c(0,0.006), xlim=c(0,4000))
predict(fit)
print(fit)

#read the shapefile to read into the habitat mask
poly <- readOGR(dsn= "C:/Users/user1/Documents/Camera trap Individuals database/India", layer="HP_3200_5200UTM")
plot(poly)

#check for suggested buffer distance around trap locations
suggested.buffer<-suggest.buffer(input_secr)
suggested.buffer

#create habitat mask
habitat_mask <- make.mask(traps(input_secr), buffer = 28000, type = "trapbuffer", poly = poly, poly.habitat = TRUE)
#plot(habitat_mask, ppoly = FALSE, col = "grey", pch = 16, cex = 0.6, add = T)
#plot(traps(input_secr), add = T)


#secr analysis with habitat mask 
fit2<-secr.fit(input_secr, mask = habitat_mask, model = g0~1, trace = FALSE)
predict(fit2)
print(fit2)

#extract sigma from using the predict function
sigma<-predict(fit2)$estimate[3]
sigma

#set the new buffer to be 3 times sigma
new.buffer <- 3*sigma
new.buffer

# redo secr analysis
habitat_mask <- make.mask(traps(input_secr), buffer = new.buffer, type = "trapbuffer", poly = poly, poly.habitat = TRUE)

#secr analysis (maximizing likelihood)
fit3<- secr.fit(input_secr, mask = habitat_mask, model = g0~1, trace = FALSE)
print(fit3)
