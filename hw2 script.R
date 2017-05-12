# Homework 2 script 

library(tidyverse)

gaz_raw <- read_delim("CA_Features_20170401.txt", delim = "|")
View(gaz_raw)

?select
gaz <- select(gaz_raw, FEATURE_ID, FEATURE_NAME, FEATURE_CLASS, STATE_ALPHA, COUNTY_NAME, PRIM_LAT_DEC, PRIM_LONG_DEC, SOURCE_LAT_DEC, SOURCE_LONG_DEC, ELEV_IN_M, MAP_NAME, DATE_CREATED, DATE_EDITED)
View(gaz)

spec(gaz)
gaz$FEATURE_ID <- parse_character(gaz$FEATURE_ID)
gaz$PRIM_LAT_DEC <- parse_double(gaz$PRIM_LAT_DEC, na = "0")
gaz$PRIM_LONG_DEC <- parse_double(gaz$PRIM_LONG_DEC, na = "0")
gaz$SOURCE_LAT_DEC <- parse_double(gaz$SOURCE_LAT_DEC)
gaz$SOURCE_LONG_DEC <- parse_double(gaz$SOURCE_LONG_DEC)
gaz$DATE_CREATED <- parse_date(gaz$DATE_CREATED, "%Y-%m-%d")
gaz$DATE_EDITED <- parse_date(gaz$DATE_EDITED, "%m/%d/%Y")

gaz %>% 
  filter(!is.na(PRIM_LAT_DEC)) %>% 
  filter(!is.na(PRIM_LONG_DEC)) %>%
  filter(STATE_ALPHA == "CA")
View(gaz)

write_delim(gaz, "gaz", delim = "|")

fn <- sort(table(gaz$FEATURE_NAME), decreasing = TRUE)
View(fn)
# Church of Christ, 228 

fc <- sort(table(gaz$FEATURE_CLASS), decreasing = FALSE)
View(fc)
# Isthmus and Sea, 1 

counties <- group_by(gaz, COUNTY_NAME)

locations <- summarise(counties, minlat = min(PRIM_LAT_DEC), maxlat = max(PRIM_LAT_DEC), minlong = min(PRIM_LONG_DEC), maxlong = max(PRIM_LONG_DEC))

locations$meanlat <- ""
locations$meanlong <- ""
locations$meanlat <- mean(locations$minlat + locations$maxlat)
locations$meanlong <- mean(locations$minlong + locations$maxlong)

View(locations)

features <- read_csv("Class_Code_Definitions.csv")
View(features)
features <- features[,-2]
colnames(features) <- c("FEATURE_CLASS", "type")
features$type <- ""
features$type <- ifelse(features$Class %in% c("Airport", "Bridge", "Building", "Canal", "Cemetery", "Census", "Church", "Civil", "Crossing", "Dam", "Harbor", "Hospital", "Levee", "Locale", "Military", "Mine", "Oilfield", "Populated Place", "Post Office", "Reservoir", "School", "Tower", "Trail", "Tunnel", "Well"), "man-made", "natural")

types <- left_join(gaz, features)
View(types)
natural <- filter(types, type == "natural")
natural
# 46,312 values 
manmade <- filter(types, type == "man-made")
manmade
# 75,739 values 
total <- 46312 + 75739
total
# 122,051
natfrac <- 46312/122051
natfrac
# 37.94% 
manfrac <- 75739/122051
manfrac
# 62.05% 