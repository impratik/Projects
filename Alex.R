
# Project Info ------------------------------------------------------------
# Date: 21-06-2021
# Data Set Link - https://ourworldindata.org/covid-deaths
# Project By Pratik Jadhav


# Loading library ---------------------------------------------------------
library(tidyverse)
library(dplyr)
library(skimr)
library(lubridate)
library(openxlsx)
library(gt)
library(hrbrthemes)
library(viridis)

# Loading Data ------------------------------------------------------------

CovidDeaths <- read.csv('~/WORKING DIR/data/covid/CovidDeaths.csv', stringsAsFactors = TRUE)
CovidVaccinations <- read.csv('~/WORKING DIR/data/covid/CovidVaccinations.csv', stringsAsFactors = TRUE)

head(CovidDeaths)
head(CovidVaccinations)
skim(CovidDeaths)

CovidDeaths[is.na(CovidDeaths)] <- 0
CovidVaccinations[is.na(CovidVaccinations)] <- 0
CovidDeaths
# Data Manipulation -------------------------------------------------------

# Select data that we are going to be using

CovidDeaths$date <- dmy(CovidDeaths$date)
skim(CovidDeaths)

CovidDeaths %>% select(location, date , total_cases, new_cases, total_deaths, population) %>% 
  filter(CovidDeaths$continent != "") %>%  
  arrange(location, date)

# Total Cases vs Total Deaths 

CovidDeaths %>% select(location, date , total_cases, total_deaths) %>% 
  filter(CovidDeaths$continent != "") %>%  
  mutate(DeathPercentage = (total_cases / total_cases)*100 ) %>% 
  filter(location == "India")  %>% 
  arrange(location, date)

# Total Cases Vs Population

CovidDeaths %>% select(location, date , total_cases, population) %>% 
  filter(CovidDeaths$continent != "") %>% 
  mutate(PopulationInfected = (total_cases / population)*100 ) %>% 
  filter(location == "India")  %>% 
  arrange(location, date)

# Countries with Highest infection rate as compare to population

CovidDeaths %>% 
  mutate(HighestInfectionCount = max(which(total_cases > 0)), PercentPopulationInfected = max((total_cases / population), na.rm = TRUE)*100) %>%  
  # filter(location == "India")  %>% 
  select(location, date , population ,HighestInfectionCount, PercentPopulationInfected) %>% 
  group_by(location, population) %>% 
  arrange(location, date) %>% summary()

CovidDeaths %>% select(location, date, population, total_cases) %>%  
  mutate(HighestInfectionCount = max(which(total_cases > 0)), PercentPopulationInfected = which.max(total_cases / population) * 100) %>% 
  group_by(location, population) %>% 
  arrange(location , date) %>%  summary()



summary(CovidDeaths)
skim(CovidDeaths)  
glimpse(CovidDeaths)

CovidDeaths %>%  
  mutate(abc = which.max(total_cases > 0)) %>%
  mutate(nms = (which.max(total_cases > 0) / population) * 100 )%>% 
  group_by(location, population) %>%  glimpse()


CovidDeaths  %>%  group_by(location, population) %>%  summarise(HigheshInfectionCount = max(total_cases), PercentPopulationInfected = max(total_cases/population) * 100 )

# final code
CovidDeaths %>%  select(location , population , total_cases, date) %>% filter(CovidDeaths$continent != "") %>% 
  group_by(location, population) %>% 
  summarise(HigheshInfectionCount = max(total_cases), 
            PercentPopulationInfected = max(total_cases/population) * 100 ) %>% 
  arrange(desc(PercentPopulationInfected))

# Showing countries with highest Death count per population

CovidDeaths %>%  select(location , continent, total_deaths) %>% filter(CovidDeaths$continent != "") %>% 
  group_by(location) %>%
  summarise(TotalDeathCount = max(total_deaths, na.rm = T)) %>% 
  arrange(desc(TotalDeathCount))

# showing continent with highest death count per population
CovidDeaths %>%  select(location , continent, total_deaths) %>% filter(CovidDeaths$continent == "") %>% 
  group_by(location) %>%
  summarise(TotalDeathCount = max(total_deaths, na.rm = T)) %>% 
  arrange(desc(TotalDeathCount))
  
# Global Data on each day
CovidDeaths %>% select(date,total_cases, total_deaths, new_cases, new_deaths, continent) %>%  group_by(date) %>% 
  filter(continent != "") %>% summarise( total_cases = sum(new_cases, na.rm = T),
                            total_deaths = sum(total_deaths, na.rm = T),
                            DeathPercentage = (sum(new_deaths,na.rm = T)/sum(new_cases,na.rm = T)) * 100)

# GLobal Data total

table1 <- CovidDeaths %>% select(total_cases, total_deaths, new_cases, new_deaths, continent) %>%
  filter(continent != "") %>% summarise( total_cases = sum(new_cases, na.rm = T),
                                         total_deaths = sum(total_deaths, na.rm = T),
                                         DeathPercentage = (sum(new_deaths,na.rm = T)/sum(new_cases,na.rm = T)) * 100)
table1
write_csv(table1,'~/Final Data/table1.csv')



# Joining two dataset ----------------------------------------------------


head(CovidVaccinations)
skim(CovidVaccinations)
CovidVaccinations$date <- dmy(CovidVaccinations$date)


# Total population vs Vaccination

df <- inner_join(CovidDeaths, CovidVaccinations, by = c("location", "date"))

df %>%  select(continent.x, location , date, population , new_vaccinations) %>% 
  filter(continent.x!= "") %>% 
  mutate( new = new_vaccinations + lag(new_vaccinations,default =0)) %>%  group_by(location, date) %>%  View()

df %>%  select(continent.x, location , date, population , new_vaccinations) %>% 
  filter(continent.x!= "") %>% 
  group_by(location,date) %>% mutate( total = new_vaccinations  )


df %>%   select(continent.x, location , date, population , new_vaccinations) %>% 
  filter(continent.x!= "") %>% group_by(location, date) %>% 
  mutate(total = sum(new_vaccinations)) %>%  View()



df %>%   select(continent.x, location , date, population , new_vaccinations) %>% 
  filter(continent.x!= "") %>% 
  mutate(total = ave(new_vaccinations, location , FUN = sum)) %>%  View()

df %>%  select(continent.x , location, date, population , new_vaccinations) %>% 
  filter(continent.x != "") %>% 
  group_by(location) %>% 
  mutate(total = sum(as.integer(new_vaccinations), na.rm = T)) %>%  group_by(date) %>% 
  View()

  
# final code 
df %>%  select(continent.x , location, date, population , new_vaccinations) %>% 
  filter(continent.x != "") %>% 
  group_by(location) %>% 
  mutate(total = cumsum(replace_na(new_vaccinations,0))) %>%
  View()

# CTE alternative
# final code 
df %>%  select(continent.x , location, date, population , new_vaccinations) %>% 
  filter(continent.x != "") %>% 
  group_by(location) %>% 
  mutate(RollingPeopleVaccinated = cumsum(replace_na(new_vaccinations,0))) %>%
  mutate(total  = RollingPeopleVaccinated/population * 100)


# creating table data -----------------------------------------------------

# 1..... 

table1 <- CovidDeaths %>% select(total_cases, total_deaths, new_cases, new_deaths, continent) %>%
  filter(continent != "") %>% summarise( total_cases = sum(new_cases, na.rm = T),
                                         total_deaths = sum(new_deaths, na.rm = T),
                                         DeathPercentage = (sum(new_deaths,na.rm = T)/sum(new_cases,na.rm = T)) * 100)
table1

write_csv(table1,'~/Final Data/table1.csv')

# 2...........
`%!in%` = Negate(`%in%`)

table2 <- CovidDeaths %>% select(total_cases, total_deaths, new_cases, new_deaths, continent, location) %>%
  filter(continent == "" , location %!in% c('World', 'European Union', 'International') ) %>% group_by(location) %>% 
  summarise( TotalDeathCount = sum(new_deaths, na.rm = T))

table2

write.xlsx(table2, '~/Final Data/table2.xlsx')

#3..............

table3 <- df %>%  select(location, population, total_cases) %>%  group_by(location, population) %>% 
  summarise(HighestInfectionCount = max(total_cases) , PercentPopulationInfected =max((total_cases/population)) * 100) %>% 
  arrange(desc(PercentPopulationInfected))

table3[is.na(table3)] <-0 

write.csv(table3,'~/Final Data/table3.csv')

#4...................

table4 <- df %>%  select(location, population,date, total_cases) %>%  group_by(location, population, date) %>% 
  summarise(HighestInfectionCount = max(total_cases) , PercentPopulationInfected =max((total_cases/population)) * 100) %>% 
  arrange(desc(PercentPopulationInfected))

table4[is.na(table4)] <-0 

write.csv(table4,'~/Final Data/table4.csv')



# Data Plotting -----------------------------------------------------------

table2 %>% ggplot(aes(location, TotalDeathCount)) + geom_col()


table2 %>% 
  arrange(TotalDeathCount) %>% 
  mutate(location = factor(location, levels = c("Europe", "South America", "North America", "Asia" , "Africa" , "Oceania")))%>% 
  ggplot(aes(location, TotalDeathCount)) +
  geom_col(fill = "steelblue") + 
  xlab("Continent") + 
  ylab("Total Death Count") +
  theme_light() +
  ggtitle("Total Deaths Per Continent")
  
table4 %>% filter( grepl('India|United States|United Kingdom|China|Russia', location)) %>%  
  ggplot(aes(date, PercentPopulationInfected)) + geom_point(color = "")
  

table4 %>% filter( str_detect(location,'India|United States|United Kingdom|China|Russia')) %>%  
  ggplot(aes(date, PercentPopulationInfected , color = location )) + geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Percent Population Infected") +
  theme_ipsum() +
  ylab("month")
