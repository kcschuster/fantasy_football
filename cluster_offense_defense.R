# *****************************************************************************
# cluster_offense_defense.R
# Pulls NFL team cumulative offensive and defensive stats from online source.
# Clusters similar offenses/defenses based on stats.
# Creates table of offenses/defenses with their determined cluster values.
# Keeps data from each week (not in place yet).

# To-do: determine optimal clustering, get data storage working week to week
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
setwd("~/Dropbox/Data_Projects/My_Projects/My_Fantasy_Football/fantasy_football")


# Set desired number of clusters
defClusts <- 5
offClusts <- 5


# =============================================================================
# Import and cluster team defense data
# =============================================================================

# Import and print team defense data from current 2015 season
url_defense <- "http://sports.yahoo.com/nfl/stats/byteam?group=Defense&cat=Total&conference=NFL&year=season_2015&sort=1130&old_category=Total&old_group=Defense"
defense_df <- readHTMLTable(url_defense, which = 7, 
                            stringsAsFactors = FALSE)
defense_df <- defense_df[ , c(1, seq(from = 3, to = ncol(defense_df), by = 2))]

# Remove "N/A" values and ensure values are numeric for clustering
for (i in 1:ncol(defense_df)) {
  defense_df[which(defense_df[ , i] == "N/A"), i] <- 0
}
cols <- 2:ncol(defense_df)
defense_df[ , cols] <- as.numeric(as.character(unlist(defense_df[ ,cols])))

# k-means clustering of defense into groups based on stats
set.seed(2)
km.out <- kmeans(defense_df[5:ncol(defense_df)], defClusts, 50)
defense_df$Cluster <- km.out$cluster
team_summary <- data.frame("Team" = defense_df$Team,
                           "DEF Cluster" = as.factor(defense_df$Cluster))


# =============================================================================
# Import and cluster team offense data
# =============================================================================

# Import and print team offense data from current 2015 season
url_offense <- "http://sports.yahoo.com/nfl/stats/byteam?group=Offense&cat=Total&conference=NFL&year=season_2015&sort=530&old_category=Total&old_group=Offense"
offense_df <- readHTMLTable(url_offense, which = 7, stringsAsFactors = FALSE) 
offense_df <- offense_df[ , seq(from = 1, to = ncol(offense_df), by = 2)]

# Remove "N/A" values and ensure values are numeric for clustering 
for (i in 1:ncol(offense_df)) {
  offense_df[which(offense_df[ , i] == "N/A"), i] <- 0
}
cols <- 2:(ncol(offense_df) - 1)
offense_df[ , cols] <- as.numeric(as.character(unlist(offense_df[ ,cols])))

# k-means clustering of offense into groups based on stats (no TOP)
set.seed(2)
km.out <- kmeans(offense_df[3:ncol(offense_df) - 1], offClusts, 50)
offense_df$Cluster <- km.out$cluster
team_summary$OFF.Cluster <- as.factor(offense_df$Cluster)

# Fix team names so compatible with data in other tables
team_summary$Team <- word(as.character(team_summary$Team), -1)

