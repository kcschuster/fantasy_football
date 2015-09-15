# *****************************************************************************
# depth_chart.R
# Pulls current depth chart info for NFL teams from online source.
# Creates table of all QB, RB, WR1/WR2, TE, K players on NFL rosters with team,
#   position, and depth info.
# *****************************************************************************


# Install and load required libraries
if (!require("XML")) {
  install.packages("XML")
}
library(XML)

# Set working directory



# Set depth chart url
url_depth_nfc <- "http://www.foxsports.com/fantasy/football/commissioner/Players/DepthCharts.aspx?nflLeague=2&position=-99"
url_depth_afc <- "http://www.foxsports.com/fantasy/football/commissioner/Players/DepthCharts.aspx?nflLeague=1&position=-99"

# Read depth chart info
depth_nfc <- readHTMLTable(url_depth_nfc, stringsAsFactors = FALSE)
depth_afc <- readHTMLTable(url_depth_afc, stringsAsFactors = FALSE)

# NFC team names
nfc_abbrevs <- c("Ari", "Atl", "Car", "Chi", "Dal", "Det", "GB", "Min", "NO",
                 "NYG", "Phi", "SF", "Sea", "StL", "TB", "Was")
nfc_teams <- c("Cardinals", "Falcons", "Panthers", "Bears", "Cowboys", "Lions",
               "Packers", "Vikings", "Saints", "Giants", "Eagles",
               "49ers", "Seahawks", "Rams", "Buccaneers", "Redskins")
nfc_cities <- c("Arizona", "Atlanta", "Carolina", "Chicago", "Dallas", "Detroit", 
                "Green Bay", "Minnesota", "New Orleans", "New York", "Philadelphia",
                "San Francisco", "Seattle", "St. Louis", "Tampa Bay", "Washington")
nfc_divisions <- c("W", "S", "S", "N", "E", "N", "N", "N", "S", "E", "E", 
                   "W", "W", "W", "S", "E")
afc_abbrevs <- c("Bal", "Buf", "Cin", "Cle", "Den", "Hou", "Ind", "Jax", "KC",
                 "Mia", "NE", "NYJ", "Oak", "Pit", "SD", "Ten")
afc_teams <- c("Ravens", "Bills", "Bengals", "Browns", "Broncos", "Texans",
               "Colts", "Jaguars", "Chiefs", "Dolphins", "Patriots", "Jets",
               "Raiders", "Steelers", "Chargers", "Titans")
afc_cities <- c("Baltimore", "Buffalo", "Cincinnati", "Cleveland", "Denver",
                "Houston", "Indianapolis", "Jacksonville", "Kansas City", 
                "Miami", "New England", "New York", "Oakland", "Pittsburgh", 
                "San Diego", "Tennessee")
afc_divisions <- c("N", "E", "N", "N", "W", "S", "S", "S", "W", "E", "E",
                   "E", "W", "N", "W", "S")

# Organize data into data frame
depth_chart <- data.frame()
for (i in 1:16) {
  depth_chart <- rbind(depth_chart, data.frame("Player" = depth_nfc[[i]]$Player, 
                             "Position" = depth_nfc[[i]]$Pos,
                             "Team" = nfc_teams[i],
                             "Abbrev" = nfc_abbrevs[i],
                             "City" = nfc_cities[i],
                             "Conference" = "NFC",
                             "Division" = nfc_divisions[i]))
}
for (i in 1:16) {
  depth_chart <- rbind(depth_chart, data.frame("Player" = depth_afc[[i]]$Player, 
                             "Position" = depth_afc[[i]]$Pos, 
                             "Team" = afc_teams[i],
                             "Abbrev" = afc_abbrevs[i],
                             "City" = afc_cities[i],
                             "Conference" = "AFC",
                             "Division" = afc_divisions[i]))
}
rm(depth_nfc)
rm(depth_afc)

# Fill in missing positions
for (i in 1:nrow(depth_chart)) {
  if (depth_chart[i, "Position"] == "") {
    depth_chart[i, "Position"] <- depth_chart[i-1, "Position"]
  }
}

# Assign depth value by checking for 1st, 2nd, etc. occurrence of position
prev <- "dummy"
depth_chart$Depth <- 0
k <- 0
for (i in 1:nrow(depth_chart)) {
  pos <- depth_chart[i, "Position"]
  if (pos != prev) {
    k <- 1
  } else {
    k <- k + 1
  }
  depth_chart[i, "Depth"] <- k
  prev <- depth_chart[i, "Position"]
}

# Function to return position on depth chart for specific player
get_depth_chart <- function(player) {
  c(as.character(depth_chart[which(depth_chart$Player == player), "Position"]),
    as.character(depth_chart[which(depth_chart$Player == player), "Depth"]))
}


# =============================================================================
# Example of use
# =============================================================================

get_depth_chart("Carson Palmer")
get_depth_chart("James Jones")
get_depth_chart("Eddie Lacy")

