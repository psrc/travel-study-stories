library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(DT)
library(openxlsx)
library(plotly)
library(shinyjs)
library(data.table)
library(odbc)
library(DBI)


# local
# wrkdir <- "C:/Users/clam/Desktop/travel-study-stories/shiny"
wrkdir <- "C:/Users/SChildress/Documents/GitHub/travel-study-stories_on_github_elmer/shiny"

# shiny server
#wrkdir <- "/home/shiny/apps/travel-study-stories/shiny"

source(file.path(wrkdir, 'travel_crosstab.R'))
source(file.path(wrkdir, 'functions_plot.R'))

missing_codes <- c('Missing: Technical Error', 'Missing: Non-response', 'Missing: Skip logic')

dbtable.household <- "HHSurvey.v_households_2017"
dbtable.day <- "HHSurvey.v_day_2017"
dbtable.vehicle <- "HHSurvey.v_vehicle_2017"
dbtable.person <- "HHSurvey.v_persons_2017"
dbtable.trip <- "HHSurvey.v_trips_2017"
dbtable.variables <- "HHSurvey.DataExplorerVariables2017"
dbtable.values <- "HHSurvey.vDataExplorerValues2017"

hh_weight_name <- 'hh_wt_revised'
hh_day_weight_name <-'hh_day_wt_revised'
trip_weight_name <- 'trip_weight_revised'

table_names <- list("Household" = list("weight_name" = hh_weight_name, "table_name" = dbtable.household),
                    "Day" = list("weight_name" = hh_day_weight_name , "table_name" = dbtable.day),
                    "Vehicle" = list("weight_name" = hh_weight_name, "table_name" =dbtable.vehicle),
                    "Person" = list("weight_name" = hh_weight_name , "table_name" = dbtable.person), 
                    "Trip" = list("weight_name" = trip_weight_name, "table_name" = dbtable.trip))


## Read from Elmer

db.connect <- function() {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\COHO",
                                database = "Elmer",
                                trusted_connection = "yes"
  )
}

read.dt <- function(astring, type =c('tablename', 'sqlquery')) {
  elmer_connection <- db.connect()
  if (type == 'tablename') {
    dtelm <- dbReadTable(elmer_connection, SQL(astring))
  } else {
    dtelm <- dbGetQuery(elmer_connection, SQL(astring))
  }
  dbDisconnect(elmer_connection)
  setDT(dtelm)
}

variables.lu <- read.dt(dbtable.variables, 'tablename')
variables.lu <- na.omit(variables.lu)
variables.lu <- variables.lu[order(CategoryOrder, VariableName)]
values.lu <- read.dt(dbtable.values, 'tablename')
values.lu<- values.lu[order(ValueOrder)]

readme.dt <- read.xlsx(file.path(wrkdir, 'readme.xlsx'), colNames = T, skipEmptyRows = F)

vars.cat <- unique(variables.lu$Category)

# master list
dtype.choice <- c("Share" ="share",
                  "Total" = "estimate",
                  "Margin of Error (Total)" = "estMOE",
                  "Total with Margin of Error" = "estimate_with_MOE",
                  "Number of Households" = "N_HH",
                  "Share with Margin of Error" = "share_with_MOE",
                  "Margin of Error (Share)" = "MOE",
                  "Sample Count" = "sample_count",
                  "Mean" = "mean",
                  "Mean with Margin of Error" = "mean_with_MOE")

# xtab sublist: dimensions
dtype.choice.xtab <- dtype.choice[c(1:2, 6, 4, 8)]
col.headers <- c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")

# xtab sublist: facts
dtype.choice.xtab.facts <- dtype.choice[c(9, 10, 8)]
col.headers.facts <-  c("mean", "MOE", "sample_count", "N_HH")

# stab sublist
dtype.choice.stab <- dtype.choice[c(1:2, 7, 3, 8)]
dtype.choice.stab.vis <- dtype.choice[c(1:2, 6, 4, 8)]

min_float <- 0
max_float <- 200
hist_breaks<- c(0,1,3,5,10,20,30,45,60,180)
hist_breaks_labels<-c('0 to 1', '1 to 3', '3 to 5', '5 to 10', '10 to 20', '20 to 30', '30 to 45', '45 to 60', '60 to 180')
hist_breaks_num_trips<-c(-.01,0,2,4,6,8,10,12,14,16,18,20,100)
hist_breaks_num_trips_labels<-c('0', '1-2', '3-4', '5-6', '7-8', '9-10', '11-12', '13-14', '14-16', '17-18', '19-20', '20-100')