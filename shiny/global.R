library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)

# source functions
source('travel_crosstab.R')

# source data
pers.dt <- fread('person.csv')
trip.dt <- fread('trip.csv')

variables.lu <- read.xlsx('variables.xlsx') %>% as.data.table
vars.cat <- unique(variables.lu$Category)

values.lu <- read.xlsx('variables_values.xlsx') %>% as.data.table


