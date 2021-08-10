## If a package is installed, it will be loaded. If any 
## are not, the missing package(s) will be installed 
## from CRAN and then loaded.

## First specify the packages of interest
packages = c("tidyverse", "dplyr",
             "data.table", "lubridate", "zoo", "magrittr")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

require(tidyverse)
require(dplyr)
require(data.table)
require(lubridate)
require(zoo)
require(magrittr)

read.empatica.eda <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(Timestamp=NA, EDA=raw$V1[3:length(raw$V1)])
  start <- as.POSIXct(start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$EDA) 
  data$Timestamp <- timestamps
  data
}
read.empatica.acc <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(X=raw$V1[3:length(raw$V1)]/64.0,Y=raw$V2[3:length(raw$V2)]/64.0,Z=raw$V3[3:length(raw$V3)]/64.0)
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$X) 
  data$Timestamp <- timestamps
  data
}
read.empatica.bvp <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  data <- data.frame(BVP=raw$V1[3:length(raw$V1)])
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$BVP) 
  data$Timestamp <- timestamps
  data
}
read.empatica.temp <- function(file) {
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  sample_rate <- as.numeric(raw$V1[2])
  temperatureF <- (1.8)*(raw$V1[3:length(raw$V1)]) + 32.0
  data <- data.frame(TEMP=temperatureF)
  start <- as.POSIXct(x = start_s, origin = "1970-01-01")
  dt <- as.difftime(as.character(1.0/sample_rate),format = "%OS")
  timestamps <- seq(from = start, by=dt , along.with = data$TEMP) 
  data$Timestamp <- timestamps
  data
}


data_dir <- "" #Directory set up: 1 Folder for each participant containing all zipped Empatica E4 files
participant_E4 <-  list.files(data_dir, full.names = TRUE) #Make a list of the folder with all X number of participant folders containing zip files.
eda <- c() #Electrodermal Activity
acc <- c() #Accelerometer Data
temp <- c() #Skin Temperature
bvp <- c() #Bloo Volume Pulse
rawdf <- c() # Raw data compiler

# Raw data extractor: For each participant folder, look inside each one by one and pull out...
extractor <- function(participant_E4) {
  {
 
   extract_file <- "EDA.csv"  ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
  list.files(participant_E4, full.names=TRUE) %>% 
    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
    map_df(function(x) {
      td <- tempdir()
      read.empatica.eda(unzip(x, extract_file, exdir=td))
    }) -> eda
  
  extract_file <- "ACC.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
  list.files(participant_E4, full.names=TRUE) %>% 
    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
    map_df(function(x) {
      td <- tempdir()
      read.empatica.acc(unzip(x, extract_file, exdir=td))
    }) -> acc
  
  extract_file <- "TEMP.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
  list.files(participant_E4, full.names=TRUE) %>% 
    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
    map_df(function(x) {
      td <- tempdir()
      read.empatica.temp(unzip(x, extract_file, exdir=td))
    }) -> temp
  
  extract_file <- "BVP.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
  list.files(participant_E4, full.names=TRUE) %>% 
    keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
    map_df(function(x) {
      td <- tempdir()
      read.empatica.bvp(unzip(x, extract_file, exdir=td))
    }) -> bvp
  
  

}

list(eda, bvp, acc, temp)
}

rawdf <- lapply(participant_E4, extractor)

# Filter to only include hours where there is a daylio risk factor.
clean_bvp <- c() #Need to condense BVP to match sampling
clean_acc <- c() #Need to condense ACC to match sampling rate

ACC <- c()
BVP <- c()
time_bvp <- c()
time_acc <- c()

for (i in 1:length(rawdf)){
  ## BVP
  # Unlike EDA and TEMP, BVP is sampled at 64 times per second. 64 Hz
  BVP <- rollapply(rawdf[[i]][[2]]$BVP, width = 16, by = 16, FUN = mean, na.rm = TRUE, align = "left")
  time_bvp = as.data.frame(rawdf[[i]][[2]]$Timestamp)
  time_bvp = time_bvp[seq(1, nrow(time_bvp), 16), ]
  clean_bvp[[i]] <- data.frame(Timestamp = time_bvp[1:length(BVP)], BVP)
  
}
for (i in 1:length(rawdf)){
  ## ACC
  # convert to data frame 
  ACC <- data.frame("X" = rawdf[[i]][[3]]$X, "Y" = rawdf[[i]][[3]]$Y, "Z" = rawdf[[i]][[3]]$Z)
  # convert to g force
  ACC <- ACC %>% mutate(g = sqrt((X * X) + (Y * Y) + (Z * Z)))
  # ACC is sampled at 32 times per second, rollapply a 40 second window with a 10 second step.
  ACC <- rollapply(ACC$g, width = 8, by = 8, FUN = mean, na.rm = TRUE, align = "left")
  
  # Take every 320th row from the Timestamp variable of ACC, this will be the corresponding timestamp for the ACC window.
  time_acc = as.data.frame(rawdf[[i]][[3]]$Timestamp)
  time_acc = time_acc[seq(1, nrow(time_acc), 8), ]
  
  # ACC is averaged from the left, meaning that there will be missing values for the final few observations in the ACC dataset.
  # Just cut the time data set down to bind the columns together.
  clean_acc[[i]] = data.frame(Timestamp = time_acc[1:length(ACC)], g = ACC)
  
}


final <- c()
base_clean <- c()
for (i in 1:length(rawdf)){
  base_clean <- setDT(rawdf[[i]][[1]]) # Index all metrics to EDA time stamps.
  # First match acceleration to EDA.
  setDT(base_clean)[, g := setkey(setDT(clean_acc[[i]]), Timestamp)[base_clean, g, roll="nearest"]]
  # Now lets combine BVP with the final data set by matching observations with the closest time.
  setDT(base_clean)[, BVP := setkey(setDT(clean_bvp[[i]]), Timestamp)[base_clean, BVP, roll="nearest"]]
  # Now lets combine TEMP with the final data set by matching observations with the closest time.
  setDT(base_clean)[, TEMP := setkey(setDT(rawdf[[i]][[4]]), Timestamp)[base_clean, TEMP, roll="nearest"]]
  
  final[[i]] <- base_clean
}



final <- final %>% mutate(Participant = Participant + 8)

