# *****************************************************************************
# Title:        weather.R
# Description:  Gets current weather data using airport codes for all NFL 
#               cities.  Makes table of basic weather info relevant to 
#               football games.
# Input:        todays_date = date we want to check weather for
# Author:       Kelsey Schuster
# *****************************************************************************

# Set your working directory
#setwd()


# Install and load necessary packages
if (!require("weatherData")) {
  install.packages("weatherData")
}
library(weatherData)


# ============================================================================
# INPUT:

# Date of interest in YYYY-MM-DD form (cannot be date in future, so if 
#    interested in game conditions, need to check the day of)
todays_date <- "2015-09-18"
# ============================================================================



# Supply team names 
nfl_teams <- c("Cardinals", "Falcons", "Panthers", "Bears", "Cowboys", "Lions",
               "Packers", "Vikings", "Saints", "Giants", "Eagles",
               "49ers", "Seahawks", "Rams", "Buccaneers", "Redskins",
               "Ravens", "Bills", "Bengals", "Browns", "Broncos", "Texans",
               "Colts", "Jaguars", "Chiefs", "Dolphins", "Patriots", "Jets",
               "Raiders", "Steelers", "Chargers", "Titans")

# Supply corresponding airport code for each team
weather_codes <- c("KGEU", "KATL", "KCLT", "KMDW", "KGKY", "KDET", 
                   "KGRB", "KMSP", "KMSY", "KEWR", "KPHL",
                   "KSJC", "KSEA", "KSTL", "KTPA", "KDCA",
                   "KBWI", "KBUF", "KLUK", "KCLE", "KDEN", "KHOU",
                   "KIND", "KJAX", "KMKC", "KMIA", "KBOS", "KEWR",
                   "KOAK", "KPIT", "KSAN", "KBNA")


# Make data frame of information
weather_df <- data.frame("Team" = nfl_teams, "Code" = weather_codes, "TempF" = 0,
                         "WindMPH" = 0, "Conditions" = "0")
class(weather_df$Conditions) <- "character"


# Populate data frame with current (last row) weather information
for (i in 1:nrow(weather_df)) {
  
  # Get all weather info
  info <- getDetailedWeather(weather_df[i, "Code"], 
                             date = todays_date, 
                             opt_temperature_columns = FALSE, 
                             opt_all_columns = TRUE)
  last_row <- nrow(info)
  
  # Get temperature in deg F
  weather_df[i, "TempF"] <- as.numeric(info[last_row, "TemperatureF"])
  
  # Get wind speed in mph
  weather_df[i, "WindMPH"] <- info[last_row, "Wind_SpeedMPH"]
  if (weather_df[i, "WindMPH"] == "Calm") {
    weather_df[i, "WindMPH"] <- 0
  }
  
  # Get general weather conditions
  weather_df[i, "Conditions"] <- as.character(info[last_row, "Conditions"])
}

# Fix column classes
weather_df$WindMPH <- as.numeric(weather_df$WindMPH)
weather_df$Conditions <- as.factor(weather_df$Conditions)


weather_df 

