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

# Function to read files into separate variables (m01 - m12)
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

# function to clean / process each df
# clean_data <- function(df) {
# 
#   # Consolidate the member_casual column
#   # df <- df %>% mutate(member_casual = recode(member_casual, "Subscriber" = "member", "Customer" = "casual"))
#   
#   # Add date-related columns
#   df$date <- as.Date(df$started_at)
#   df$month <- format(as.Date(df$date), "%m")
#   df$day <- format(as.Date(df$date), "%d")
#   df$year <- format(as.Date(df$date), "%Y")
#   df$day_of_week <- format(as.Date(df$date), "%A")
#   df$season <- factor(sapply(df$month, get_season), levels = c("Winter", "Spring", "Summer", "Fall")) # sort seasons
#   df$hour <- format(as.POSIXct(df$started_at, format="%Y-%m-%d %H:%M:%S"), "%H")
#   
#   # Add ride_length column, converting data to minutes
#   # 2019 was in mins, 2020 in secs - combining to all_trips above must've forced calculations to seconds
#   # df$started_at <- as.POSIXct(df$started_at, format="%Y-%m-%d %H:%M:%S")
#   # df$ended_at <- as.POSIXct(df$ended_at, format="%Y-%m-%d %H:%M:%S")
#   df$ride_length <- difftime(df$ended_at, df$started_at, units = "mins")
#   
#   # Convert ride_length to numeric
#   df$ride_length <- as.numeric(as.character(df$ride_length))
#   
#   # Remove rows with start_station_name "HQ QR" or negative ride_length
#   df <- df[!(df$start_station_name == "HQ QR" | df$ride_length < 0),]
# 
#   return(df)
# }


# Function to clean data for m01-m12
clean_cyclistic_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    df_name <- paste0("m", month)
    
    if (exists(df_name)) {
      df <- get(df_name)
      
      # Add date-related columns
      df$date <- as.Date(df$started_at)
      df$month <- format(df$date, "%m")
      df$day <- format(df$date, "%d")
      df$year <- format(df$date, "%Y")
      df$day_of_week <- format(df$date, "%A")
      df$season <- factor(sapply(df$month, get_season), levels = c("Winter", "Spring", "Summer", "Fall"))
      df$hour <- format(as.POSIXct(df$started_at, format="%Y-%m-%d %H:%M:%S"), "%H")
      
      # Calculate ride length in minutes
      df$ride_length <- as.numeric(difftime(df$ended_at, df$started_at, units = "mins"))
      
      # Remove invalid rows
      df <- df[!(df$start_station_name == "HQ QR" | df$ride_length < 0),]
      
      # Assign cleaned data back to global environment
      assign(df_name, df, envir = .GlobalEnv)
    }
  }
}

clean_cyclistic_data(2024)

#=================================================
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or presentation software
write_cleaned_data <- function(year) {
  for (i in 1:12) {
    month <- sprintf("%02d", i)
    df_name <- paste0("m", month)
    
    if (exists(df_name)) {
      df_cleaned <- get(df_name)
      
      # Modify this section with actual cleaning steps
      df_cleaned <- df_cleaned %>%
        mutate(started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"))
      
      output_file <- paste0("resources/data/", year, "/", year, month, "-divvy-tripdata_cleaned.csv")
      write_csv(df_cleaned, output_file)
    }
  }
}

write_cleaned_data(2024)

