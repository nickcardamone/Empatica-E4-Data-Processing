#Notes
#Tutorial for RHRV Usage: https://www.youtube.com/watch?v=PoHx3d067PI

#Load libraries
#library(RHRV)
#library(XML)
require(tidyverse)
require(plyr)
require(zoo)
require(data.table)

#Direct to folder of E4 zip-folders for a specific participant
data_dir <- ""

# Read functions, found on internet so don't ask me.
read.empatica.ibi <- function(file){
  raw <- read.csv(file,header = F)
  start_s <- raw$V1[1]
  start <- as.POSIXct(x = start_s,origin = "1970-01-01")
  dt <- as.difftime(raw$V1[2:length(raw$V1)],units = "secs")
  timestamps <- start+dt
  ibi <- as.double(as.character(raw$V2[2:length(raw$V2)]) )
  data <- data.frame(Timestamp=timestamps, IBI=ibi)
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

extract_file <- "IBI.csv"  ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
list.files(data_dir, full.names=TRUE) %>% 
  keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
  map_df(function(x) {
    td <- tempdir()
    read.empatica.ibi(unzip(x, extract_file, exdir=td))
  }) -> combined_ibi


extract_file <- "ACC.csv" ### LOOK FOR ALL FILES WITH THIS AFTER UNZIPPING ZIP FOLDERS IN DIRECTORY AND COMBINE
list.files(data_dir, full.names=TRUE) %>% 
  keep(~any(grepl(sprintf("^%s$", extract_file), unzip(., list=TRUE)$Name))) %>% 
  map_df(function(x) {
    td <- tempdir()
    read.empatica.acc(unzip(x, extract_file, exdir=td))
  }) -> combined_acc

## (1) Identifying missing data... is the time gap between to inter-beat intervals the same as the time associated with the inter-beat interval?
combined_ibi <- combined_ibi %>% mutate(diff = as.numeric(Timestamp - lag(Timestamp)))

## (2) If there is a streak of uninterrupted data we want to identify it...
data <- combined_ibi %>% mutate(Streak = if_else(as.numeric(IBI) == as.numeric(diff), "yes", "no"))

##  (3) Count the length of the streak so we have a data window
## (4) Generate a run-length type group id for each streak and 
## (5) Identify unrealistic data by flagging any data that's more than 20% longer or shorter than the previous.
data <- data %>% mutate(count = sequence(rle(as.character(data$Streak))$lengths),
                        rlid = rleid(Streak),
                        artifact = if_else((IBI * (1.20)) < lead(IBI) & lead(Streak) != "no" | (IBI * (0.80)) > lead(IBI) & lead(Streak) != "no", "yes", "no"))

## Filter out artifacts
data2 <- data %>% filter(artifact == "no") %>% select(Timestamp, IBI)

## Re-run (1) and (2) to account for the time gaps introduced by removing artifacts
data2 <- data2 %>% mutate(diff = as.numeric(Timestamp - lag(Timestamp)),
                          Streak = if_else(as.numeric(IBI) == as.numeric(diff), "yes", "no"))
## Re run (3) and (4) to get gold-standard, artifact-removed, uninteruppted data windows for analysis.
data2 <- data2 %>% mutate(
  count = sequence(rle(as.character(data2$Streak))$lengths),
  rlid = rleid(Streak))

## Along the count column, identify a group on the notion that a group doesn't stop if the difference in count is positive...
## Then find the maximum of that value. I have no clue why cumsum() works, it just does.
data3 <- subset(data2, count == ave(count, cumsum(c(TRUE, diff(count) < 0)),  FUN = max)) 
## Only keep data where the window size (count) is greater than 300 seconds (5 minutes)
data3 <- data3 %>% filter(count > 180)
## These data islands are now discrete targets for analysis and can be paired back up with data set 2 based on their identifier, rlid.
islands <- as.factor(data3$rlid)
data2$rlid <- as.factor(data2$rlid)
data4 <- data2 %>% filter(rlid %in% islands)

## Now here's the HRV formula: we're using a time-based metric, RMSSD (Root Mean of Squared Successive Differences)
# Find the value of successive differences only on the data islands.
data4$rlid <- as.factor(data4$rlid)

data4$difference <- ddply(data4, .(rlid), summarise, 
                          difference=c(NA, rollapply(IBI, width=2, FUN = diff)))$difference
# Square it
data4$difference <- (data4$difference)^2
# Take the mean of 30 second windows
data4$mean_diff <- ddply(data4, .(rlid), summarise, 
                         mean_diff= rollapply(difference, width=30, FUN = mean, fill = NA))$mean_diff
# Take the square of that
data4$rmssd <- sqrt(data4$mean_diff)

## MATCH acceleration reading to HRV reading based on closest time. 
## This will take a LONG time if your dataset is large. 

# Convert to g force
ACC <- combined_acc %>% mutate(g = sqrt((X * X) + (Y * Y) + (Z * Z)))
# ACC is sampled at 32 times per second, rollapply a 40 second window with a 10 second step.
ACC <- rollapply(ACC$g, width = 1280, by = 320, FUN = mean, na.rm = TRUE, align = "left")
TIME_ACC = as.data.frame(combined_acc$Timestamp)
TIME_ACC = TIME_ACC[seq(1, nrow(TIME_ACC), 320), ]
ACC = data.frame(TIME_ACC[1:length(ACC)], ACC)

d <- function(x,y) abs(x-y) # define the distance function
idx <- sapply(data4$Timestamp, function(x) which.min( d(x,ACC$TIME_ACC.1.length.ACC..) )) # find matches
final <- data.frame(data4,ACC[idx, 2, drop=FALSE])

# We can now also take the mean by data island.

data5 <- final %>% drop_na() %>% dplyr::group_by(rlid) %>% dplyr::summarise(Time = mean(Timestamp),
                                                                            avg_rmssd = mean(rmssd),
                                                                            window_size = max(count),
                                                                            movement = mean(ACC),
                                                                            sd_rmssd = sd(rmssd))

data5$hour <- hour(data5$Time)
data5$day <- lubridate::day(data5$Time)
## Left 1 hour in between windows, toss out in final analysis, 3 7-hour windows
data5$analysis_period <- if_else(15 > data5$hour &  data5$hour > 8, "morning/mid-day", 
                                 if_else(23 > data5$hour &  data5$hour > 16, "afternoon/evening", 
                                         if_else(7 > data5$hour &  data5$hour > 0, "sleep", "other")))
# By day, by analysis window extract the HRV measurement with the lowest movement (in g-force). 
df <- data5 %>% filter(analysis_period != "other") %>% group_by(day, analysis_period) %>% slice(which.min(movement))

p <- ggplot(df, aes(x = Time, y = avg_rmssd, color = analysis_period)) +geom_hline(color = "orange", alpha = 0.5, yintercept = mean(df$avg_rmssd)) + geom_segment(aes(x = Time, xend = Time, y = 0, yend = avg_rmssd), color = "grey") + geom_point(size = 5, alpha = 0.5) + theme_light()

ggplotly(p, dynamicTicks = T)
