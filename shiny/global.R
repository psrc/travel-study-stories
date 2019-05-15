library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)
library(plotly)
library(shinyjs)

# source functions
source('travel_crosstab.R')

# source data
pers.dt <- fread('person.csv', encoding = 'UTF-8')
trip.dt <- fread('trip.csv', encoding = 'UTF-8')

variables.lu <- read.xlsx('variables.xlsx') %>% as.data.table
vars.cat <- unique(variables.lu$Category)

values.lu <- read.xlsx('variables_values.xlsx') %>% as.data.table

# master list
dtype.choice <- c("Share" ="share",
                  "Estimate" = "estimate",
                  "Number of Households" = "N_HH",
                  "Shares with Margin of Error" = "share_with_MOE",
                  "Margin of Error (Share)" = "MOE",
                  "Sample Count" = "sample_count")

# xtab sublist
dtype.choice.xtab <- dtype.choice[c(1:2, 4, 6)]

# stab sublist
dtype.choice.stab <- dtype.choice[c(1:2, 5:6)]
dtype.choice.stab.vis <- dtype.choice[c(1:2, 4, 6)]

