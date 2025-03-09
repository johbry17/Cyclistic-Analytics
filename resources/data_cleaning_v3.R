#=====================
# Any oddities commented out are...
# ...legacy steps from converting the Google template to the current version
# Note that these functions work for any given year.
# Divvy switched to these columns in 2020. As of early 2025, this works for any year,
# although the first three months of 2020 are in one file, Q12020, which'd be a problem
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

# reads files into separate df's (m01 - m12)
read_cyclistic_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)  # Format as 01, 02, ..., 12
    file_path <- paste0("data/", year, "/", year, month, "-divvy-tripdata.csv")
    
    if (file.exists(file_path)) {
      assign(paste0("m", month), read_csv(file_path), envir = .GlobalEnv)
    } else {
      message("File not found: ", file_path)
    }
  }
}

# call read function
read_cyclistic_data(2024)

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

# cleans data for m01-m12
clean_cyclistic_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    df_name <- paste0("m", month)
    cleaned_name <- paste0("m", month, "_cleaned")
    
    if (exists(df_name)) {
      df <- get(df_name)
      
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
      # remove negative ride_lengths, part of company maintenance. No more "HQ QR" stations
      df <- df %>% filter(ride_length >= 0)
      
      # assign cleaned data back to global environment
      assign(cleaned_name, df, envir = .GlobalEnv)
    }
  }
}

# call clean function
clean_cyclistic_data(2024)

#=================================================
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# creates csv files to visualize with
write_cleaned_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    df_name <- paste0("m", month, "_cleaned")
    
    if (exists(df_name)) {
      df_cleaned <- get(df_name)
      
      output_file <- paste0("data/", year, "/", year, month, "-divvy-tripdata_cleaned.csv")
      write_csv(df_cleaned, output_file)
    }
  }
}

# call write function
write_cleaned_data(2024)

# dump memory
rm(list = ls(pattern = "^m\\d{2}(_cleaned)?$"))
gc()
