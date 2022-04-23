setwd("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project3")
getwd()

# LIBRARIES========================================================================================================================
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(data.table)

# READ FILES=======================================================================================================================
# taxi_data <- read.table(file="C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project3/taxi_trips.tsv", 
#                         sep = "\t", header = TRUE, quote = "\"")

taxi_data <- fread("C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project3/taxi_trips.tsv", 
                   sep = "\t", header = TRUE, quote = "\"", 
                   select = c('Trip Start Timestamp', 'Trip Seconds', 'Trip Miles', 'Pickup Community Area', 'Dropoff Community Area', 'Company')
                   )

# Basic stats
str(taxi_data)
summary(taxi_data)
dim(taxi_data)

# PRE-PROCESSING===================================================================================================================

# rename columns
taxi_data <- taxi_data %>%
  rename(timestamp = `Trip Start Timestamp`,
         sec = `Trip Seconds`,
         miles = `Trip Miles`,
         pickup = `Pickup Community Area`,
         dropoff = `Dropoff Community Area`,
         company = `Company`)


# ===FILTER VALUES===

# remove all trips less than 0.5 miles and more than 100 miles
taxi_data <- filter(taxi_data, miles > 0.5 & miles < 100)
# remove all trips less than 60 seconds and greater than 5 hours
taxi_data <- filter(taxi_data, sec > 60 & sec < 18000)


# ===COMPANY NAMES===

# Cab company names
print(unique(taxi_data$company))

# Remove all non-alphabetical characters
taxi_data$company <- str_replace_all(taxi_data$company, "[^[:alpha:]]", " ")
# Remove leading and trailing spaces in each new word
taxi_data$company <- trimws(taxi_data$company, which = c("both"))

# Change company names
taxi_data <- taxi_data %>%
  mutate(company = recode(company, `Medallion Management Corp` = '312 Medallion Management Corp', 
                          `Star Taxi` = 'Five Star Taxi', 
                          `Seven Taxi` =  'Twentyfour Seven Taxi', 
                          `Star Taxi` = 'STAR Taxi',
                          `Sun Taxi` = 'SUN Taxi',
                          `Checker Taxi Affiliation` = 'CHECKER Taxi Affiliation',
                          `Chicago Taxicab` = 'CHICAGO Taxicab'
                          ))


# Create unique company code for each company
taxi_data$company_code <- as.integer(factor(taxi_data$company))

# Create dataframe for company and id
companies <- distinct(taxi_data, company_code, .keep_all = TRUE)
companies <- companies[, c("company", "company_code")]
# Output companies file
write.csv(companies,"C:/Users/aranga22/Downloads/Academics/Sem 2/424 Visual Data/Projects/424_Project3/ext\\companies.csv", row.names = FALSE)


# ===NULL VALUES===
# Check null values
lapply(taxi_data,
       function(x) { 
         length(which(is.na(x)))
         }
       )
# Fill null values
taxi_data$pickup[is.na(taxi_data$pickup)] <- 0
taxi_data$dropoff[is.na(taxi_data$dropoff)] <- 0

# convert timestamp to POSIX Format
taxi_data <- taxi_data %>%
  mutate(timestamp = mdy_hms(timestamp))
# Extract invidual date and time components from timestamp
taxi_data <- taxi_data %>%
  mutate(year = year(timestamp),
         month = month(timestamp),
         day = day(timestamp),
         hour = hour(timestamp)
  )
# create new timestamp consisting of 1 hour intervals
taxi_data$timestamp <- with(taxi_data, ymd_h(paste(year, month, day, hour, sep= ' ')))

# Create week day column
taxi_data$week_day <- lubridate::wday(taxi_data$timestamp, label=TRUE)

# Add Month name
name_of_month <- function(taxi_data){
  taxi_data$month_name[taxi_data$month==1] <- 'Jan'
  taxi_data$month_name[taxi_data$month==2] <- 'Feb'
  taxi_data$month_name[taxi_data$month==3] <- 'Mar'
  taxi_data$month_name[taxi_data$month==4] <- 'Apr'
  taxi_data$month_name[taxi_data$month==5] <- 'May'
  taxi_data$month_name[taxi_data$month==6] <- 'Jun'
  taxi_data$month_name[taxi_data$month==7] <- 'Jul'
  taxi_data$month_name[taxi_data$month==8] <- 'Aug'
  taxi_data$month_name[taxi_data$month==9] <- 'Sep'
  taxi_data$month_name[taxi_data$month==10] <- 'Oct'
  taxi_data$month_name[taxi_data$month==11] <- 'Nov'
  taxi_data$month_name[taxi_data$month==12] <- 'Dec'

  return(taxi_data)

}
taxi_data <- name_of_month(taxi_data)
# Create date column by concatenating year, month, day
taxi_data$date <- with(taxi_data, ymd(paste(year, month, day, sep= ' ')))

taxi_data <- subset(taxi_data, select = -c(timestamp, year, month, day))

str(taxi_data)



# # Write data to .tsv file
# write.table(final_data, file='final_data.tsv', quote=FALSE, sep='\t')
# 
# Output final data file into chunks
no_of_chunks <- 50
f <- ceiling(1:nrow(taxi_data) / nrow(taxi_data) * 35)
res <- split(taxi_data, f)
map2(res, paste0("part_", names(res), ".csv"), write.csv)



# 
# 
# 
# daily_sigma <- function(day, data){
#   print(day)
#   print(class(data$date))
#   #print(data$date %in% day)
#   print(data[data$date == as.Date(day, origin = "1964-10-22"), ])
# }
# 
# # Groupby by date
# # daily_frame <- aggregate(data$pickup, by=list(data$date), NROW)
# dates <- unique(taxi_data$date)
# print(class(dates))
# rides <- sapply(dates, function(day) daily_sigma(day, taxi_data))
# daily_frame <- data.frame(dates, rides)
# daily_frame
# date
# lund = "2019-02-13"
# nrow(subset(taxi_data, date == lund))
# class(date)
# str(taxi_data)
# 
# 
# hourly_sigma <- function(h, data){
#   print(nrow(subset(data, time == h)))
# }
# 
# time <- unique(taxi_data$time)
# rides <- sapply(time, function(h) hourly_sigma(h, taxi_data))
# hourly_frame <- data.frame(time, rides)
# item = "01:00:00"
# print(taxi_data[taxi_data$time == hour,])
# 
# taxi_data %>% 
#   filter(time == item)
# 
# #as.POSIXct("2016-07-01 01:00:00", tz="UTC")
# subset(taxi_data, pickup > 0 & dropoff > 0)
# data
# taxi_data
# 
# memory.limit(24000)
months_shit <- taxi_data %>% 
  count(month_name)

colnames(months_shit)
