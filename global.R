library(config)
library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)
library(plotly)
library(shinyjs)
library(odbc)
library(DBI)
library(here)
library(RSQLite)
library(psrc.travelsurvey)
library(markdown)
library(magrittr)
library(conflicted)


source('travel_crosstab.R')
source('functions_plot.R')


conflict_prefer("layout", "plotly")
conflict_prefer("get", "base")

hhts.datasets <- c('2017/2019','2021')

missing_codes <- c('Missing: Technical Error', 'Missing: Non-response', 'Missing: Skip logic', 'Missing: Skip Logic', 'Children or missing')

dbtable.household <- "h"
dbtable.person <- "p"
dbtable.trip <- "t"

dbtable.variables <- "[HHSurvey.variable_metadata]"
dbtable.values <- "[HHSurvey.v_value_metadata]"

table_names <- list("Household" = list("table_name"=dbtable.household),
                    "Person" = list("table_name"=dbtable.person),
                    "Trip" = list("table_name" = dbtable.trip))
z <- 1.645 # 90% CI


## Read from Elmer


read.dt <- function(astring, type =c('table_name', 'sqlquery')) {
  sqllite_connection <- dbConnect(RSQLite::SQLite(), 'hh_survey.db')
  if (type == 'table_name') {
    dtelm <- dbReadTable(sqllite_connection, SQL(astring))
  } else {
    dtelm <- dbGetQuery(sqllite_connection, SQL(astring))
  }
  dbDisconnect(sqllite_connection)
  setDT(dtelm)
}

variables.lu <- read.dt(dbtable.variables, 'table_name')
variables.lu <- variables.lu %>% select(-"levels") %>% na.omit()%>%setDT()
variables.lu <- variables.lu[order(category_order, variable_name)]
values.lu <- read.dt(dbtable.values, 'table_name')
values.lu<- values.lu[order(value_order)]

readme.dt <- read.xlsx('readme.xlsx', colNames = T, skipEmptyRows = F)

vars.cat <- unique(variables.lu$category)

# master list
dtype.choice <- c("Share" ="share",
                  "Total" = "estimate",
                  "Margin of Error (Total)" = "estMOE",
                  "Total with Margin of Error" = "estimate_with_MOE",
                  "Share with Margin of Error" = "share_with_MOE",
                  "Margin of Error (Share)" = "MOE",
                  "Sample Count" = "sample_count",
                  "Median" = "median",
                  "Median with Margin of Error" = "median_with_MOE")

# xtab sublist: dimensions
dtype.choice.xtab <- dtype.choice[c(1:2, 5, 4, 7)]
col.headers <- c("sample_count", "estimate", "estMOE", "share", "MOE")

# xtab sublist: facts
dtype.choice.xtab.facts <- dtype.choice[c(9, 8, 7)]
col.headers.facts <-  c("median", "MOE", "sample_count")

# we assume a 50% probability to maximize the MOE
p_MOE <- 0.5
# stab sublist
# change to named indices
dtype.choice.stab <- dtype.choice[c(1:2, 6, 3,7)]
dtype.choice.stab.vis <- dtype.choice[c(1:2, 5, 4,7)]

min_float <- 0
max_float <- 200
hist_breaks<- c(0,1,3,5,10,20,30,45,60,180)
hist_breaks_labels<-c('0 to 1', '1 to 3', '3 to 5', '5 to 10', '10 to 20', '20 to 30', '30 to 45', '45 to 60', '60 to 180')
hist_breaks_num_trips<-c(-.01,0,2,4,6,8,10,12,14,16,18,20,100)
hist_breaks_num_trips_labels<-c('0', '1-2', '3-4', '5-6', '7-8', '9-10', '11-12', '13-14', '14-16', '17-18', '19-20', '20-100')

