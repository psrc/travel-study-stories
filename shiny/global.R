library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)

# base.dir <- "C:/Users/CLam/Desktop/travel-study-stories/shiny"

# source functions
source('travel_crosstab.R')

# source data
pers.dt <- fread('person.csv')
trip.dt <- fread('trip.csv')

variables.lu <- read.xlsx('variables.xlsx') %>% as.data.table
vars.cat <- unique(variables.lu$Category)


