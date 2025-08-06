# Cyclistic Analytics  
<span style="font-size:1.1em; color:#555;">Google Data Analytics Capstone</span>

*What drives a casual rider to become a member? An R-powered look into bikeshare behavior.*

🔗 [Final Report (2024)](https://johbry17.github.io/Cyclistic-Analytics/summary_2024.html)  
🔗 [Final Report (2019)](https://johbry17.github.io/Cyclistic-Analytics/summary_2019.html)  
📊 [EDA (2024)](https://johbry17.github.io/Cyclistic-Analytics/scratch_pad_2024.html)  
📊 [EDA (2019)](https://johbry17.github.io/Cyclistic-Analytics/scratch_pad_2019.html)

## Table of Contents

- [Project Overview](#project-overview)
- [Features](#features)
- [Tools & Technologies](#tools--technologies)
- [Usage](#usage)
- [Gallery](#gallery)
- [Certificate](#certificate)
- [References](#references)
- [Licenses](#licenses)
- [Acknowledgements](#acknowledgements)
- [Author](#author)

## Project Overview

**Cyclistic** is a hypothetical bikeshare company in Chicago. This project investigates differences between casual riders and annual members to support a key business goal: increase member conversions.

Using public data provided by Divvy (via Lyft), the analysis:
- Cleans and preprocesses raw CSVs from 2024 and 2019
- Explores ride patterns by time, location, and bike type
- Visualizes data with `ggplot2`, `leaflet`, and R Markdown
- Provides strategic recommendations based on real insights

Originally built as the final project for the [Google Data Analytics Certificate](https://www.coursera.org/professional-certificates/google-data-analytics), it follows the six-phase analytics process:

![Six Phases of Data Analytics](./resources/images/six_phases_of_data_analytics.png)

## Features

- 📄 R Markdown reports knitted to interactive HTML
- 🗺️ Interactive Leaflet maps showing ride density by user type
- 📈 Time-of-day usage patterns (hourly ride histograms)
- 📆 Weekday vs. weekend trends by user type
- 📊 Bike type and ride volume by month
- 🧭 Year-over-year comparisons to spot post-pandemic trends
- 📌 Top stations mapped for casuals vs. members
- 💡 Strategic recommendations for converting casual riders

## Tools & Technologies

- **Language:** R, R Markdown
- **Packages:** tidyverse, sf, ggspatial, leaflet, leaflet.extras, fontawesome
- **Cleaning Scripts:** `data_cleaning_v2.R`, `data_cleaning_v3.R`
- **Deployment:** GitHub Pages (HTML reports)

## Usage

1. View the final summary reports:
   - [2024 Summary](https://johbry17.github.io/Cyclistic-Analytics/summary_2024.html)
   - [2019 Summary](https://johbry17.github.io/Cyclistic-Analytics/summary_2019.html)

2. Explore the exploratory data analysis:
   - [2024 EDA](https://johbry17.github.io/Cyclistic-Analytics/scratch_pad_2024.html)
   - [2019 EDA](https://johbry17.github.io/Cyclistic-Analytics/scratch_pad_2019.html)

3. Run cleaning scripts:
   - Download trip data from [Divvy Data Portal](https://divvy-tripdata.s3.amazonaws.com/index.html)
   - Use provided R scripts to clean and process the data locally:
     - `data_cleaning_v2.R` for 2019
     - `data_cleaning_v3.R` for 2024

## Gallery

Time & Day Plots:

![Rides per Hour, by Member Type](./resources/images/rides_hour_2024.png)

![Rides per Hour and Day of Week, by Member Type](./resources/images/rides_per_hour_and_day_of_week_2024.png)

Map Visualizations:

![2019 comparison map](./resources/images/2019_comparison_map.png)

![2024 casual stations map](./resources/images/leaflet_casual_2024.png)

![2024 member stations map](./resources/images/leaflet_member_2024.png)

Bike Type by Month:

![2024 Rides per Month by Bike and Member Type](./resources/images/bike_type_month_2024.png)

## Certificate

Final capstone project for [Google Data Analytics Professional Certificate](https://www.coursera.org/professional-certificates/google-data-analytics)

![Google Data Analytics Certificate](./resources/images/google_data_analytics_certificate.png)

## References

Divvy and Lyft provided the [data](https://divvy-tripdata.s3.amazonaws.com/index.html) to Google and Coursera.

## Licenses

- MIT License © 2025 Bryan Johns
- Data provided under the [Divvy Data License Agreement](https://divvybikes.com/data-license-agreement)

## Acknowledgements

Thanks to:
- Google and Coursera for the learning platform
- Divvy, Lyft, and Motivate International Inc. for providing the data
- Everyone working on sustainable mobility solutions

## Author

Bryan Johns, March 2025  
[bryan.johns.official@gmail.com](mailto:bryan.johns.official@gmail.com) | [LinkedIn](https://www.linkedin.com/in/b-johns/) | [GitHub](https://github.com/johbry17) | [Portfolio](https://johbry17.github.io/portfolio/index.html)


![AI logo](./resources/images/Logo_AI.jpeg)
*AI generated mock logo*