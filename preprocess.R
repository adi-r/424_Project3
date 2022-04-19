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

# Extract Capital letters to generate codes for each company 
taxi_data$code <- gsub("[^A-Z]","", taxi_data$company)


# ===NULL VALUES===
# Check null values
lapply(taxi_data,
       function(x) { 
         length(which(is.na(x)))
         }
       )
# Fill null values
taxi_data$pickup[is.na(taxi_data$pickup)] <- -1
taxi_data$dropoff[is.na(taxi_data$dropoff)] <- -1


# ===TIME DATA===
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
taxi_data$newdate <- with(taxi_data, ymd_h(paste(year, month, day, hour, sep= ' ')))
# Create week day column
taxi_data$week_day <- lubridate::wday(taxi_data$timestamp, label=TRUE)

# Add Month name
name_of_month <- function(df){
  df$month_name[df$month==1] <- 'Jan'
  df$month_name[df$month==2] <- 'Feb'
  df$month_name[df$month==3] <- 'Mar'
  df$month_name[df$month==4] <- 'Apr'
  df$month_name[df$month==5] <- 'May'
  df$month_name[df$month==6] <- 'Jun'
  df$month_name[df$month==7] <- 'Jul'
  df$month_name[df$month==8] <- 'Aug'
  df$month_name[df$month==9] <- 'Sep'
  df$month_name[df$month==10] <- 'Oct'
  df$month_name[df$month==11] <- 'Nov'
  df$month_name[df$month==12] <- 'Dec'
  
  return(df)
  
}
taxi_data <- name_of_month(taxi_data)


# Remove unnecessary columns and rename accordingly
final_data <- subset(taxi_data, select = -c(timestamp))
final_data <- final_data %>%
  rename(timestamp = newdate)

str(final_data)

# # Write data to .tsv file
# write.table(final_data, file='final_data.tsv', quote=FALSE, sep='\t')

# Output final data file into chunks
no_of_chunks <- 50
f <- ceiling(1:nrow(final_data) / nrow(final_data) * 35)
res <- split(taxi_data, f)
map2(res, paste0("part_", names(res), ".csv"), write.csv)
