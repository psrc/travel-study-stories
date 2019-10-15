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

wrkdir <- "C:/Users/SChildress/Documents/GitHub/travel-study-stories/shiny"
data.dir <- "C:/Users/SChildress/Documents/GitHub/travel-study-stories/data"
#wrkdir <- "C:/Users/CLam/Desktop/travel-study-stories/shiny"
#data.dir <- "C:/Users/CLam/Desktop/travel-study-stories/data"
# shiny server
#wrkdir <- "/home/shiny/apps/travel-study-stories/shiny"
#data.dir <- "/home/shiny/apps/travel-study-stories/data"

source(file.path(wrkdir, 'travel_crosstab.R'))
source(file.path(wrkdir, 'functions_plot.R'))


working.dbtable.household <- "HHSurvey.vHousehold2017"
working.dbtable.person <- "HHSurvey.vHouseholdPerson2017"
working.dbtable.trip <- "HHSurvey.vHouseholdPersonTrip2017"
working.dbtable.variables <- "HHSurvey.DataExplorerVariables"


## Read from Elmer

db.connect <- function() {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\COHO",
                                database = "Elmer",
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(atable) {
  elmer_connection <- db.connect()
  dtelm <- dbReadTable(elmer_connection, SQL(atable))
  dbDisconnect(elmer_connection)
  setDT(dtelm)
}


household.dt <- read.dt(working.dbtable.household)
pers.dt <-  read.dt(working.dbtable.person)
trip.dt <- read.dt(working.dbtable.trip)
variables.lu <- read.dt(working.dbtable.variables)
values.lu <- fread(file.path(data.dir, 'variables_values_pascal.csv'))
readme.dt <- read.xlsx(file.path(data.dir, 'readme.xlsx'), colNames = T, skipEmptyRows = F)

vars.cat <- unique(variables.lu$Category)

# master list
dtype.choice <- c("Share" ="share",
                  "Total" = "estimate",
                  "Margin of Error (Total)" = "estMOE",
                  "Total with Margin of Error" = "estimate_with_MOE",
                  "Number of Households" = "N_HH",
                  "Share with Margin of Error" = "share_with_MOE",
                  "Margin of Error (Share)" = "MOE",
                  "Sample Count" = "sample_count")

# xtab sublist
dtype.choice.xtab <- dtype.choice[c(1:2, 6, 4, 8)]
col.headers <- c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")

# stab sublist
dtype.choice.stab <- dtype.choice[c(1:2, 7, 3, 8)]
dtype.choice.stab.vis <- dtype.choice[c(1:2, 6, 4, 8)]
