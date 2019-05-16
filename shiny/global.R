library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)
library(plotly)
library(shinyjs)

# wrkdir <- "C:/Users/CLam/Desktop/travel-study-stories/shiny" # local
# wrkdir <- "/home/shiny/apps/travel-study-stories/shiny" # shiny

# source(file.path(wrkdir, 'travel_crosstab.R'))
# source(file.path(wrkdir, 'functions_plot.R'))
# pers.dt <- fread(file.path(wrkdir, 'person.csv'), encoding = 'UTF-8')
# trip.dt <- fread(file.path(wrkdir, 'trip.csv'), encoding = 'UTF-8')
# variables.lu <- read.xlsx(file.path(wrkdir, 'variables.xlsx')) %>% as.data.table
# values.lu <- read.xlsx(file.path(wrkdir, 'variables_values.xlsx')) %>% as.data.table
# 
# 
# vars.cat <- unique(variables.lu$Category)

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

