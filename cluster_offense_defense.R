# *****************************************************************************
# cluster_offense_defense.R
# Pulls NFL team cumulative offensive and defensive stats from online source.
# Clusters similar offenses/defenses based on stats.
# Creates table of offenses/defenses with their determined cluster values.
# Keeps data from each week (not in place yet).

# To-do: figure out optimal clustering, get data storage working week to week
# *****************************************************************************


# Install and load required libraries
if (!require("XML")) {
  install.packages("XML")
}
library(XML)
if (!require("cluster")) {
  install.packages("cluster")
}
library(cluster)
if (!require("stringr")) {
  install.packages("stringr")
}
library(stringr)


# Set working directory
#setwd()


# Import and print team defense data from current 2015 season
url_defense <- "http://sports.yahoo.com/nfl/stats/byteam?group=Defense&cat=Total&conference=NFL&year=season_2015&sort=1130&old_category=Total&old_group=Defense"
defense_df <- readHTMLTable(url_defense, which = 7, stringsAsFactors = FALSE)
defense_df <- defense_df[ , c(1, 2, seq(from = 3, to = ncol(defense_df), by = 2))]
#write.csv(defense_df, file = "def_2014.csv")

# Cluster defenses into groups based on stats
pam.def <- pam(defense_df[3:ncol(defense_df)], 1)
defense_df$Cluster <- pam.def$clustering
team_summary <- data.frame("Team" = defense_df$Team, 
                           "DEF Cluster" = defense_df$Cluster)

# Import and print team offense data from current 2015 season
url_offense <- "http://sports.yahoo.com/nfl/stats/byteam?group=Offense&cat=Total&conference=NFL&year=season_2015&sort=530&old_category=Total&old_group=Offense"
offense_df <- readHTMLTable(url_offense, which = 7, stringsAsFactors = FALSE) 
offense_df <- offense_df[ , seq(from = 1, to = ncol(offense_df), by = 2)]

# Cluster offenses into groups based on stats
pam.off <- pam(offense_df[3:ncol(offense_df)], 1)
offense_df$Cluster <- pam.off$clustering
team_summary$OFF.Cluster <- offense_df$Cluster

# Fix team names so compatible with data in other tables
team_summary$Team <- word(as.character(team_summary$Team), -1)



