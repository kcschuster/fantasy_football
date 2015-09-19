# *****************************************************************************
# Title:        nfl_schedule.R
# Description:  Imports NFL schedule from saved .csv file (downloaded from
#               "https://excelfantasyfootball.wordpress.com/2015/04/26/
#               nfl-2015-schedule-free-excel-spreadsheet/").  Creates table of
#               game data for each team, including whether the team was home
#               or away.
# Input:        "nfl-2015-schedule.csv" downloaded from above link.
# Output:       "nfl_schedule.csv" = file with teams + opponents for each week,
#               including home/away/bye info
# Author:       Kelsey Schuster
# *****************************************************************************


# INPUT:
# ============================================================================
# Set your working directory
#setwd("~/")
# ============================================================================



# Install and load required libraries
if (!require("XML")) {
  install.packages("XML")
}
library(XML)
if (!require("stringr")) {
  install.packages("stringr")
}
library(stringr)



# ============================================================================
# Import schedule file (assumed to be already saved in working directory)
# ============================================================================

# Set schedule file
# downloaded from "https://excelfantasyfootball.wordpress.com/2015/04/26/
#   nfl-2015-schedule-free-excel-spreadsheet/"
schedule_file <- "nfl-2015-schedule.csv"


# import schedule and eliminate extra info
import_df <- read.csv(file = schedule_file, stringsAsFactors = FALSE)
import_df$Home <- substr(import_df$Home, 2, length(import_df$Home))
import_df$Day <- NULL
import_df$Date <- NULL
import_df$Time <- NULL


# Set up new schedule data frame - organize
schedule <- data.frame("Team" = sort(unique(import_df$Visitor)), "Week1" = "0", 
                       "Home1" = -1, "Week2" = "0", "Home2" = -1, "Week3" = "0",
                       "Home3" = -1, "Week4" = "0", "Home4" = -1, "Week5" = "0",
                       "Home5" = -1, "Week6" = "0", "Home6" = -1, "Week7" = "0",
                       "Home7" = -1, "Week8" = "0", "Home8" = -1, "Week9" = "0",
                       "Home9" = -1, "Week10" = "0", "Home10" = -1, "Week11" = "0",
                       "Home11" = -1, "Week12" = "0", "Home12" = -1, "Week13" = "0",
                       "Home13" = -1, "Week14" = "0", "Home14" = -1, "Week15" = "0",
                       "Home15" = -1, "Week16" = "0", "Home16" = -1, "Week17" = "0",
                       "Home17" = -1)


# ============================================================================
# Populate data frame with schedule info
# ============================================================================

# Organize imported data into new schedule for each week and team
for (week in 1:17) {
  
  # Want opponents in string form, not factors yet
  schedule[ , sprintf("Week%s", week)] <- as.character(schedule[ , sprintf("Week%s", week)])
  
  # Go through all the teams
  for (team in 1:nrow(schedule)) {
    
    # Find team's game for that week, get opponent
    vis <- import_df[import_df$Week == sprintf("WEEK %s", week) & 
                       (import_df$Visitor == schedule[team, ]$Team | 
                          import_df$Home == schedule[team, ]$Team), ]$Visitor
    home <- import_df[import_df$Week == sprintf("WEEK %s", week) & 
                        (import_df$Visitor == schedule[team, ]$Team | 
                           import_df$Home == schedule[team, ]$Team), ]$Home
    
    # If team is on bye, couldn't find name, set home = -1
    if (length(vis) == 0) {
      schedule[team, sprintf("Week%s", week)] <- "NA"
      schedule[team, sprintf("Home%s", week)] <- -1
    }
    
    # Check if team is the visitor or the home team, fill in data
    else if (vis == schedule[team, ]$Team) {
      schedule[team, sprintf("Week%s", week)] <- home
      schedule[team, sprintf("Home%s", week)] <- 0
    } else {
      schedule[team, sprintf("Week%s", week)] <- vis
      schedule[team, sprintf("Home%s", week)] <- 1
    }
  }
}

# Free up space
rm(import_df)



# OUTPUT:
# ============================================================================
# Print schedule data to file
#   home = 0 (away), home = 1 (home), home = -1 (bye week, no game)
write.csv(schedule, file = "nfl_schedule.csv")
# ============================================================================
