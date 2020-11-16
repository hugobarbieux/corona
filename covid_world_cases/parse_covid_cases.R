# You can either download the dataset at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# Save it in a .csv
# The data are updated everyday, check the website every morning and repeat the operations

# Import dataset in a variable

covidworldcases <- read.csv("COVID-19-geographic-disbtribution-worldwide-2020-11-15.csv")

View(covidworldcases)

##### OR BETTER #####
# Script for downloading the Excel file into "R" software
# These libraries are necessary

library(readxl)
library(httr)
library(dplyr)
library(ggplot2)
library(ggthemes)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into "R"

covidworldcases <- read_excel(tf)

View(covidworldcases)

# Optional - Examine the data

dim(covidworldcases)
names(covidworldcases)
nrow(covidworldcases)
ncol(covidworldcases)
head(covidworldcases)
tail(covidworldcases)
summary(covidworldcases)

##########

France <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "France")
France <- France [c(1,5,7)]
France <- France[order(as.Date(France$dateRep, format="%d/%m/%y")),]
France[,"cum_sum"] <- cumsum(France$cases)
France <- subset(France, cum_sum > 0)

Germany <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "Germany")
Germany <- Germany [c(1,5,7)]
Germany <- Germany[order(as.Date(Germany$dateRep, format="%d/%m/%y")),]
Germany[,"cum_sum"] <- cumsum(Germany$cases)
Germany <- subset(Germany, cum_sum > 0)

Europe <- rbind(France, Germany)

Europe %>%
  ggplot(aes(x = dateRep, y = cum_sum, color = countriesAndTerritories)) +
  geom_line(size = 1.5, alpha = 0.8) +
  labs(title = "Crise du coronavirus",
       subtitle = "Nrb de contaminations en Europe",
       x = "Date",
       y = "Nbr de cas",
       color = "Pays") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  scale_color_brewer(palette = "Set1")




##########
# Improve the dataset by importing list of country with region and ISO-2 code
##### ! SINCE THEN THE DATASET HAS BEEN UPGRADED WITH MORE COLUMNS SUCH AS ISO-2 CODE ANS POPULATION ! #####

countrieslist <- read.csv("countrieslist.csv")
View(countrieslist)

# Keep only useful columns
countrieslistsomecolumns <- countrieslist [c(2, 6)]
View(countrieslistsomecolumns)

# Join the two data frames by ISO-2 codes to correspond country names with the region
covidworldcasesplusregion <- full_join(countrieslistsomecolumns, covidworldcases, by = "geoId")
View(covidworldcasesplusregion)

##### AFRICA #####

# Keep only African countries
africa <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"region" == "Africa")

# See what African countries are more impacted by the virus
deathsinafrica <- tapply(africa$"deaths", africa$"countriesAndTerritories", sum)
deathsinafrica <- deathsinafrica[order(-deathsinafrica)]
View(deathsinafrica)

##########

# Generate a sort of pivot table, apply a sum to the Deaths column and aggregate it by the Countries column
# Put the "pivot table" in a new variable
deathsbycountry <- tapply(covidworldcases$"deaths", covidworldcases$"countriesAndTerritories", sum)

# Order it
deathsbycountry <- deathsbycountry[order(-deathsbycountry)]
View(deathsbycountry)

# Select only the 50 first rows
deathsbycountry <- deathsbycountry[c(1:50)]

# Create a bar chart with "pivot" values
barplot(height = deathsbycountry)

# Export "pivot" to use it with any visualisation program
write.csv(casesbycountry, file='cases_by_country.csv')
write.csv(deathsbycountry, file = "deaths_by_country.csv")



##### OBSOLETE CODE #####

# Reformat date
France$dateRep <- as.Date(France$dateRep, format = "%d/%m/%y")
class(France$dateRep)

# Subset the dataset by country
# Keep useful columns
# Make cummulative count
# Keep only values greater than 0 (to begin the day the first death is recorded)

Algeria <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "Algeria")
Algeria <- Algeria [c(3,8,9)]
Algeria <- Algeria [order(Algeria$dateRep),]
Algeria <- cumsum(Algeria[, 2])
Algeria <- subset(Algeria, Algeria > 0)
View(Algeria)
write.csv(Algeria, file = "Algeria.csv")

Egypt <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "Egypt")
Egypt <- Egypt [c(3,8,9)]
Egypt <- Egypt[order(Egypt$dateRep),]
Egypt <- cumsum(Egypt[, 2])
Egypt <- subset(Egypt, Egypt > 0)
View(Egypt)
write.csv(Egypt, file = "Egypt.csv")

China <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "China")
China <- China [c(3,8,9)]
China <- China[order(China$dateRep),]
China <- cumsum(China[, 2])
China <- subset(China, China > 0)
write.csv(China, file="China.csv")

Italy <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "Italy")
Italy <- Italy [c(3,8,9)]
Italy <- Italy[order(Italy$dateRep),]
Italy <- cumsum(Italy[, 2])
Italy <- subset(Italy, Italy > 0)
write.csv(Italy, file="Italy.csv")

France <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "France")
View(France)
France <- France [c(3,8,9)]
France <- France[order(France$dateRep),]
France <- cumsum(France[, 2])
France <- subset(France, France > 0)
write.csv(France, file="France.csv")

United_Kingdom <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "United_Kingdom")
United_Kingdom <- United_Kingdom [c(3,8,9)]
United_Kingdom <- United_Kingdom[order(United_Kingdom$dateRep),]
United_Kingdom <- cumsum(United_Kingdom[, 2])
United_Kingdom <- subset(United_Kingdom, United_Kingdom > 0)
write.csv(United_Kingdom, file="United_Kingdom.csv")

United_States <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "United_States_of_America")
United_States <- United_States [c(3,8,9)]
United_States <- United_States[order(United_States$dateRep),]
United_States <- cumsum(United_States[, 2])
United_States <- subset(United_States, United_States > 0)
write.csv(United_States, file="United_States.csv")

Spain <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "Spain")
Spain <- Spain [c(3,8,9)]
Spain <- Spain[order(Spain$dateRep),]
Spain <- cumsum(Spain[, 2])
Spain <- subset(Spain, Spain > 0)
write.csv(Spain, file="Spain.csv")

Germany <- subset(covidworldcasesplusregion, covidworldcasesplusregion$"countriesAndTerritories" == "Germany")
Germany <- Germany [c(3,8,9)]
Germany <- Germany[order(Germany$dateRep),]
Germany <- cumsum(Germany[, 2])
Germany <- subset(Germany, Germany > 0)
write.csv(Germany, file="Germany.csv")

##########

ThreeCountries <- rbind(Italy, France, United_Kingdom)
View(ThreeCountries)
write.csv(ThreeCountries, file="threecountries.csv")

##########

