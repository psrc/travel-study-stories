# This script cross-checks for variables in variables.xlsx that are not compiled in variables_values.xlsx

# variables_values::Field = raw column names from source household survey data
# variables::Variables = column names in processed household survey data for this app. Equivalent to variables_values::Label

library(data.table)
library(openxlsx)
library(tidyverse)

this.dir <- getwd()
dir <- 'C:/Users/CLam/Desktop/travel-study-stories/shiny'
setwd(dir)

variables.lu <- read.xlsx('variables.xlsx') %>% as.data.table
values.lu <- read.xlsx('variables_values.xlsx') %>% as.data.table

fields.with.values <- values.lu[, .(Label), by = Label][['Label']]
fields <- variables.lu[['Variables']]

nonlisted <- sort(setdiff(fields, fields.with.values))

variables.lu[Variables %in% nonlisted]

setwd(this.dir)