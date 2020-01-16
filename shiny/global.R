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
#wrkdir <- "C:/Users/clam/Desktop/travel-study-stories/shiny"
wrkdir <- "C:/Users/SChildress/Documents/GitHub/travel-study-stories_on_github_elmer/shiny"

# shiny server
#wrkdir <- "/home/shiny/apps/travel-study-stories/shiny"

source(file.path(wrkdir, 'travel_crosstab.R'))
source(file.path(wrkdir, 'functions_plot.R'))


dbtable.household <- "HHSurvey.v_households_2017"
dbtable.person <- "HHSurvey.v_persons_2017"
dbtable.trip <- "HHSurvey.v_trips_2017"
dbtable.variables <- "HHSurvey.DataExplorerVariables2017"
dbtable.values <- "HHSurvey.vDataExplorerValues2017"


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
                  "Sample Count" = "sample_count")

# xtab sublist
dtype.choice.xtab <- dtype.choice[c(1:2, 6, 4, 8)]
col.headers <- c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")

# stab sublist
dtype.choice.stab <- dtype.choice[c(1:2, 7, 3, 8)]
dtype.choice.stab.vis <- dtype.choice[c(1:2, 6, 4, 8)]

min_float <- -.01
max_float <- 200
hist_breaks<- c(-.01,0,1,3,5,10,20,30,45,60,180)