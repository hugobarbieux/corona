# You can either download the dataset at https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# Save it in a .csv
# The data are updated everyday, check the website every morning and repeat the operations

# Import dataset in a variable

covidworldcases <- read.csv("COVID-19-geographic-disbtribution-worldwide-2020-11-15.csv")

View(covidworldcases)

##### OR BETTER #####
# Script for downloading the Excel file into "R" software
# These libraries are necessary

install.packages("dplyr")
install.packages("ggplot2")
install.packages("gganimate")
install.packages("transformr")
install.packages("gifski")

library(readxl)
library(httr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(transformr)
library(gifski)

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

# Subset the dataset by country
# Keep useful columns
# Format date column as date and order from latest to newest
# Calculate cummulative count of cases in a new variable
# Keep only values greater than 99 (to start since 100 cases reported)

# How to reformat date
# France$dateRep <- as.Date(France$dateRep, format = "%d/%m/%y")
# Verify
# class(France$dateRep)

France <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "France")
France <- France [c(1,5,7)]
France <- France[order(as.Date(France$dateRep, format="%d/%m/%y")),]
France[,"cum_sum"] <- cumsum(France$cases)
France <- subset(France, cum_sum > 99)

Germany <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "Germany")
Germany <- Germany [c(1,5,7)]
Germany <- Germany[order(as.Date(Germany$dateRep, format="%d/%m/%y")),]
Germany[,"cum_sum"] <- cumsum(Germany$cases)
Germany <- subset(Germany, cum_sum > 99)

Italy <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "Italy")
Italy <- Italy [c(1,5,7)]
Italy <- Italy[order(as.Date(Italy$dateRep, format="%d/%m/%y")),]
Italy[,"cum_sum"] <- cumsum(Italy$cases)
Italy <- subset(Italy, cum_sum > 99)

Belgium <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "Belgium")
Belgium <- Belgium [c(1,5,7)]
Belgium <- Belgium[order(as.Date(Belgium$dateRep, format="%d/%m/%y")),]
Belgium[,"cum_sum"] <- cumsum(Belgium$cases)
Belgium <- subset(Belgium, cum_sum > 99)

United_Kingdom <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "United_Kingdom")
United_Kingdom <- United_Kingdom [c(1,5,7)]
United_Kingdom <- United_Kingdom[order(as.Date(United_Kingdom$dateRep, format="%d/%m/%y")),]
United_Kingdom[,"cum_sum"] <- cumsum(United_Kingdom$cases)
United_Kingdom <- subset(United_Kingdom, cum_sum > 99)

Spain <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "Spain")
Spain <- Spain [c(1,5,7)]
Spain <- Spain[order(as.Date(Spain$dateRep, format="%d/%m/%y")),]
Spain[,"cum_sum"] <- cumsum(Spain$cases)
Spain <- subset(Spain, cum_sum > 99)

Sweden <- subset(covidworldcases, covidworldcases$"countriesAndTerritories" == "Sweden")
Sweden <- Sweden [c(1,5,7)]
Sweden <- Sweden[order(as.Date(Sweden$dateRep, format="%d/%m/%y")),]
Sweden[,"cum_sum"] <- cumsum(Sweden$cases)
Sweden <- subset(Sweden, cum_sum > 99)

# Gather data in a single variable
Europe <- rbind(France, Germany, Italy, Belgium, United_Kingdom, Spain, Sweden)

# Make a line graph

graph1 = Europe %>%
  #mutate(isFrance = (countriesAndTerritories == "France")) %>%
  ggplot(aes(x = dateRep, y = cum_sum, color = countriesAndTerritories)) +
  geom_line(size = 1.5, alpha = 0.8) +
  #geom_line(aes(linetype = isFrance), size = 1.5, alpha = 0.8) +
  labs(title = "Coronavirus pandemic",
       subtitle = "Contamination numbers accross Europe since 100 cases reported",
       x = "Date",
       y = "Number of cases",
       color = "Countries") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  #scale_linetype_manual(values = c("dashed", "solid"), guide = "none") +
  scale_color_brewer(palette = "Set1") +
  geom_point()

graph1

# Animate it

graph1.animation = graph1 +
  transition_reveal(dateRep) +
  view_follow(fixed_y = TRUE)

graph1.animation

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 7, end_pause = 60, res = 100)
anim_save("corona_graph.gif")
#save_animation(graph1.animation, "corona_graph.gif")
 

##### OBSOLETE CODE #####

# Improve the dataset by importing list of country with region and ISO-2 code

##### SINCE THE DATASET HAS BEEN UPGRADED WITH MORE COLUMNS SUCH AS ISO-2 CODE ANS POPULATION, THIS STEP IS NOT ANYMORE USEFUL #####

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
