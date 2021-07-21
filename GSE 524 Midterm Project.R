options(scipen=999)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape)


library(gapminder)
install.packages("rust")
library(rust)

# get the US population from 2010 - 2020 
getwd()
setwd("/Users/shreyak/Desktop/GSE 524 - R")
filep <- "population_total.csv"
file <- read_csv(filep)
frameWorks <- data.frame(gather(file, key = "year", value="population", -country))



import_gapminder <- function(filep) {
  file <- read_csv(filep)
  frameWorks <- data.frame(gather(file, key = "year", value="value", -country))
  subSt <- substr(basename(filep), 1, nchar(basename(filep))-4)
  names(frameWorks)[3] <- subSt
  return(frameWorks)
};

filepath <- list.files(path = "/Users/shreyak/Desktop/GSE 524 - R", pattern ="\\.csv$")

finalFrame <- filepath %>%
  map(.f = import_gapminder)

dataFrame <- merge_recurse(finalFrame)
head(dataFrame)

colnames(dataFrame)
names(dataFrame)[names(dataFrame) == "expenditure_per_student_primary_percent_of_gdp_per_person"] <- "Expenditure"
names(dataFrame)[names(dataFrame) == "population_total"] <- "Population"
names(dataFrame)[names(dataFrame) == "country"] <- "Country"
names(dataFrame)[names(dataFrame) == "year"] <- "Year"
names(dataFrame)[names(dataFrame) == "ratio_of_girls_to_boys_in_primary_and_secondary_education_perc"] <- "Gender_Ratio"
names(dataFrame)[names(dataFrame) == "literacy_rate_youth_total_percent_of_people_ages_15_24"] <- "Literacy_Ratio"
names(dataFrame)[names(dataFrame) == "income_per_person_long_series"] <- "Income"
names(dataFrame)[names(dataFrame) == "primary_completion_rate_total_percent_of_relevant_age_group"] <- "Primary_Comp"
head(dataFrame)

sub <- subset(dataFrame, Population >= 73900000)
finalPop <- sub[sub$Year >= "1990" & sub$Year <= "2015",]

head(finalPop)


this <- finalPop %>%
  filter(Country %in% c("United States", "China", "Pakistan", "Russia", "India", "Indonesia"))

finalPop <- transform(finalPop, Year = as.integer(Year))
# PLOT NUMBER 1: POPULATION + LITERACY RATE

g <- ggplot(finalPop, aes(Income, Primary_Comp, color = Country))

yoyo <- g + geom_point() +
  labs(
       y="Primary School Completion", 
       x="Income", 
       title="Primary School Completion as a result of Income")

yoyo + transition_time(Year) +
  labs(title = 'Year: {frame_along}') 


ggplot(this, aes(x = Literacy_Ratio, y = Population, color = Country)) +
  geom_point() +
  scale_color_viridis_d() + 
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Literacy Rate AGES 15 - 64", y = "Population") +
  geom_text(aes(Literacy_Ratio, Population, label = Country), subset = (this$Literacy_Ratio > 0.98 | this$Population > ))


  
library(ggplot2)
theme_set(theme_bw())
library(gapminder)
library(gifski)
library(gganimate)

getwd()
setwd("/Users/shreyak/Desktop/GSE 524 - R")

str(finalPop)
finalPop <- transform(finalPop, Year = as.integer(Year))

# omg in this plot here you can see how each country was given a color as well as a plot for population vs labor participation 
# it would be interesting to see how these countries change over time 


ploot <- ggplot(finalPop, aes(x = Year, y = Income, color = Country)) +
  geom_point() +
  scale_color_viridis_d() + 
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "Year", y = "Literacy Ratio") 
  
ploot + transition_time(Year)




# gganimate plot of increasing income across 6 countries over the past 60 years

this <- finalPop %>%
  filter(Country %in% c("United States", "China", "Pakistan", "Russia", "India", "Indonesia"))

p <- this %>% 
  ggplot(aes(x = Year, y = Expenditure, color = Country, group = Country)) + 
  geom_path() + 
  geom_point() + 
  facet_wrap(~ Country) + 
  expand_limits(y=c(0.0, 50.0)) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_along}') +
  transition_reveal(along = Year) +
  ease_aes('linear')




getwd()
setwd("/Users/shreyak/Desktop/GSE 524 - R")
filepp <- "cross-country-literacy-rates.csv"
filepppp <- read_csv(filepp)

colnames(filepppp)
names(filepppp)[names(filepppp) == "Entity"] <- "Country"
names(filepppp)[names(filepppp) == "Literacy rates (World Bank, CIA World Factbook, and other sources)"] <- "Literacy_Rates"

library(ggplot2)
theme_set(theme_bw())
library(gapminder)
library(gifski)
library(gganimate)


ya <- filepppp[filepppp$Year >= "1950" & filepppp$Year <= "2010",]

globalLit <- ya %>% 
  ggplot(aes(x = Year, y = Literacy_Rates, color = Country, group = continent)) + 
  geom_path() + 
  geom_point() + 
  facet_wrap(~ Country) + 
  expand_limits(y=c(0.0, 50.0)) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_along}') +
  transition_reveal(along = Year) +
  ease_aes('linear')









