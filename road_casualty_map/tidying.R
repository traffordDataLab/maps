## Loading and tidying STATS19 data ##

## ----- load the R packages
library(tidyverse) ; library(lubridate) ; library(forcats) ; library(sf)

## ----- read the accident and casualty data from source (TfGM)
accident <- read_csv("http://odata.tfgm.com/opendata/downloads/STATS19AccDataForGMServers.csv") 
casualty <- read_csv("http://odata.tfgm.com/opendata/downloads/STATS19CasDataForGMServers.csv")

## ----- merge the accident and casualty dataframes
casualties <- merge(casualty, accident, by = "Accident Index")
rm(accident, casualty)

## ----- recode and relabel variables
# date
casualties$date <- dmy(casualties$OutputDate)
# month
casualties$month <- month(casualties$date, label = TRUE)
# day
casualties$day <- wday(casualties$date, label = TRUE)
# hour
casualties$hour <- hour(casualties$OutputTime)
# mode of travel
casualties <- mutate(casualties, 
                     mode = ifelse(CasTypeCode == 0, "Pedestrian",
                                   ifelse(CasTypeCode == 1, "Pedal Cycle",
                                          ifelse(CasTypeCode %in% c(2, 3, 4, 5), "Powered 2 Wheeler",
                                                 ifelse(CasTypeCode == 9, "Car",
                                                        ifelse(CasTypeCode == 8, "Taxi",
                                                               ifelse(CasTypeCode == 11, "Bus or Coach",
                                                                      ifelse(CasTypeCode %in% c(19, 20, 21), "Goods Vehicle", "Other Vehicle"))))))),
                     mode = factor(mode))
# casualty severity
casualties <- mutate(casualties, severity = ifelse(CasualtySeverity == 1, "Fatal", ifelse(CasualtySeverity == 2, "Serious", "Slight")))
# light conditions
casualties <- mutate(casualties, light = ifelse(LightingCondition == 1, "Daylight", "Dark"))
# sex
casualties <- mutate(casualties, sex = ifelse(Sex == 1, "Male", ifelse(Sex == 2, "Female", "Not known")))
# ageband                     
casualties <- mutate(casualties, ageband = factor(AgeBandOfCasualty),
                     ageband = fct_recode(ageband, 
                                          "0-5" = "1",
                                          "6-10" = "2",
                                          "11-15" = "3",
                                          "16-20" = "4",
                                          "21-25" = "5",
                                          "26-35" = "6",
                                          "36-45" = "7",
                                          "46-55" = "8",
                                          "56-65" = "9",
                                          "66-75" = "10",
                                          "Over 75" = "11"))

# local authority 
casualties <- mutate(casualties, lad16nm = factor(LocalAuthority),
                     lad16nm = fct_recode(lad16nm, 
                                          "Bolton" = "100",
                                          "Bury" = "101",
                                          "Manchester" = "102",
                                          "Oldham" = "104",
                                          "Rochdale" = "106",
                                          "Salford" = "107",
                                          "Stockport" = "109",
                                          "Tameside" = "110",
                                          "Trafford" = "112",
                                          "Wigan" = "114"))

# add a text variable
casualties_desc <- function(row)
  with(as.list(row), paste0("At ", gsub('.{3}$', '', OutputTime), " on ", format.Date(date, "%A %d %B %Y"),
                            " a ", tolower(severity),
                            " collision occured involving ", (NumberVehicles), " vehicle(s) and ",
                            (CasualtyNumber), " casualtie(s)."))
strs <- apply(casualties, 1, casualties_desc)
names(strs) <- NULL
casualties$text <- strs

## ----- select and rename variables
casualties <- select(casualties, AREFNO = `Accident Index`,
       date, month, day, hour, mode, severity, light, sex, ageband, lad16nm, text,
       easting = Easting, northing = Northing)

## ----- extract coordinates and column bind
coords <- st_as_sf(casualties, coords = c("easting","northing")) %>% 
  st_set_crs(27700) %>% 
  st_transform(4326) %>% 
  st_coordinates()
casualties <- cbind(casualties, coords) %>% 
  rename(long = X, lat = Y)

## ----- write dataframe to a CSV
write_csv(casualties, "casualties_2016.csv")
