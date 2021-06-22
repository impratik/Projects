
# Project Info ------------------------------------------------------------
# Date: 21-06-2021
# Data Set Link - https://ourworldindata.org/covid-deaths
# Project By Pratik Jadhav


# Loading library ---------------------------------------------------------
library(tidyverse)
library(dplyr)
library(skimr)
library(lubridate)

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

CovidDeaths %>% select(total_cases, total_deaths, new_cases, new_deaths, continent) %>%
  filter(continent != "") %>% summarise( total_cases = sum(new_cases, na.rm = T),
                                         total_deaths = sum(total_deaths, na.rm = T),
                                         DeathPercentage = (sum(new_deaths,na.rm = T)/sum(new_cases,na.rm = T)) * 100)

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
  group_by(location,date) %>% mutate( total = new_vaccinations + )


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
  mutate(total  = RollingPeopleVaccinated/population * 100) %>%  View()

