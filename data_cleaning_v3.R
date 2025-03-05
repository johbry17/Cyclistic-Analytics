#=====================
# Any oddities commented out are...
# ...legacy steps from converting the Google template to the current version
#=====================

library(tidyverse)  #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

#=====================
# COLLECT DATA
#=====================
# # Upload Divvy datasets (csv files) here

# read files into separate df's (m01 - m12)
read_cyclistic_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)  # Format as 01, 02, ..., 12
    file_path <- paste0("resources/data/", year, "/", year, month, "-divvy-tripdata.csv")
    
    if (file.exists(file_path)) {
      assign(paste0("m", month), read_csv(file_path), envir = .GlobalEnv)
    } else {
      message("File not found: ", file_path)
    }
  }
}

read_cyclistic_data(2024)

# check_na_summary <- function(df, month) {
#   paste(
#     month, "started_at NA's:", sum(is.na(df$started_at)), "|",
#     "ended_at NA's:", sum(is.na(df$ended_at)), "|",
#     "date NA's:", sum(is.na(df$date)), "|",
#     "ride_length NA's:", sum(is.na(df$ride_length)), "|",
#     "hour NA's:", sum(is.na(df$hour)), "|",
#     "season NA's:", sum(is.na(df$season))
#   )
# }
# 
# # Run for m01 to m12
# months <- paste0("m", sprintf("%02d", 1:12))
# na_summaries <- sapply(months, function(m) check_na_summary(get(m), m))
# 
# # Print results
# cat(na_summaries, sep="\n")

#======================================================
# CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# helper function to set season
get_season <- function(month) {
  if (month %in% c("12", "01", "02")) {
    return("Winter")
  } else if (month %in% c("03", "04", "05")) {
    return("Spring")
  } else if (month %in% c("06", "07", "08")) {
    return("Summer")
  } else if (month %in% c("09", "10", "11")) {
    return("Fall")
  }
}

# clean data for m01-m12
clean_cyclistic_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    df_name <- paste0("m", month)
    cleaned_name <- paste0("m", month, "_cleaned")
    
    if (exists(df_name)) {
      df <- get(df_name)
      
      # Explicitly format dates, hope it resolves NA issue
      # df$started_at <- as.POSIXct(df$started_at, format="%Y-%m-%d %H:%M:%S")
      # df$ended_at <- as.POSIXct(df$ended_at, format="%Y-%m-%d %H:%M:%S")
      
      # add date-related columns
      df$date <- as.Date(df$started_at)
      df$month <- format(df$date, "%m")
      df$day <- format(df$date, "%d")
      df$year <- format(df$date, "%Y")
      df$day_of_week <- format(df$date, "%A")
      df$season <- factor(sapply(df$month, get_season), levels = c("Winter", "Spring", "Summer", "Fall"))
      df$hour <- format(as.POSIXct(df$started_at, format="%Y-%m-%d %H:%M:%S"), "%H")
      
      # calculate ride length in minutes
      df$ride_length <- as.numeric(difftime(df$ended_at, df$started_at, units = "mins"))
      
      # remove invalid rows
      df <- df %>%
        filter(!(start_station_name == "HQ QR" | ride_length < 0))
      
      # assign cleaned data back to global environment
      assign(cleaned_name, df, envir = .GlobalEnv)
    }
  }
}

clean_cyclistic_data(2024)

# check_na_summary <- function(df, month) {
#   paste(
#     month, "started_at NA's:", sum(is.na(df$started_at)), "|",
#     "ended_at NA's:", sum(is.na(df$ended_at)), "|",
#     "date NA's:", sum(is.na(df$date)), "|",
#     "ride_length NA's:", sum(is.na(df$ride_length)), "|",
#     "hour NA's:", sum(is.na(df$hour)), "|",
#     "season NA's:", sum(is.na(df$season))
#   )
# }
# 
# # Run for m01 to m12
# months <- paste0("m", sprintf("%02d", 1:12), "_cleaned")
# na_summaries_cleaned <- sapply(months, function(m) check_na_summary(get(m), m))
# 
# # Print results
# cat(na_summaries_cleaned, sep="\n")

#=================================================
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or presentation software
write_cleaned_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    df_name <- paste0("m", month, "_cleaned")
    
    if (exists(df_name)) {
      df_cleaned <- get(df_name)
      
      # # Modify this section with actual cleaning steps
      # df_cleaned <- df_cleaned %>%
      #   mutate(started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"))
      
      output_file <- paste0("resources/data/", year, "/", year, month, "-divvy-tripdata_cleaned.csv")
      write_csv(df_cleaned, output_file)
    }
  }
}

write_cleaned_data(2024)

# dump memory
rm(list = ls(pattern = "^m\\d{2}(_cleaned)?$"))
gc()
