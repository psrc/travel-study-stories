library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)
library(plotly)
library(shinyjs)

# local
# wrkdir <- "C:/Users/CLam/Desktop/travel-study-stories/shiny"
# data.dir <- "C:/Users/CLam/Desktop/travel-study-stories/data"

# shiny server
wrkdir <- "/home/shiny/apps/travel-study-stories/shiny"
data.dir <- "/home/shiny/apps/travel-study-stories/data"

source(file.path(wrkdir, 'travel_crosstab.R'))
source(file.path(wrkdir, 'functions_plot.R'))

pers.dt <- fread(file.path(data.dir, 'person.csv'), encoding = 'UTF-8')
trip.dt <- fread(file.path(data.dir, 'trip.csv'), encoding = 'UTF-8')
variables.lu <- fread(file.path(data.dir, 'variables.csv'))
values.lu <- fread(file.path(data.dir, 'variables_values.csv'))


vars.cat <- unique(variables.lu$Category)

# master list
# dtype.choice <- c("Share" ="share",
#                   "Estimate" = "estimate",
#                   "Number of Households" = "N_HH",
#                   "Shares with Margin of Error" = "share_with_MOE",
#                   "Margin of Error (Share)" = "MOE",
#                   "Sample Count" = "sample_count")
# 
# # xtab sublist
# dtype.choice.xtab <- dtype.choice[c(1:2, 4, 6)]
# 
# # stab sublist
# dtype.choice.stab <- dtype.choice[c(1:2, 5:6)]
# dtype.choice.stab.vis <- dtype.choice[c(1:2, 4, 6)]

# test master list
dtype.choice <- c("Share" ="share",
                  "Estimate" = "estimate",
                  "Margin of Error (Estimate)" = "estMOE",
                  "Estimate with Margin of Error" = "estimate_with_MOE",
                  "Number of Households" = "N_HH",
                  "Shares with Margin of Error" = "share_with_MOE",
                  "Margin of Error (Share)" = "MOE",
                  "Sample Count" = "sample_count")

# xtab sublist
dtype.choice.xtab <- dtype.choice[c(1:2, 6, 4, 8)]
col.headers <- c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")

# stab sublist
# dtype.choice.stab <- dtype.choice[c(1:2, 7:8)]
# dtype.choice.stab.vis <- dtype.choice[c(1:2, 6, 8)]
