library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)

# base.dir <- "C:/Users/CLam/Desktop/travel-study-stories/travel_crosstab"

# source functions
source('travel_crosstab.R')

# source data
survey <- fread('person_2017.csv')

person_variables <- read.xlsx('person_variables.xlsx')
# person_variables <- read.xlsx(file.path(base.dir, 'person_variables.xlsx'))
