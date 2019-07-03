library(data.table)
library(openxlsx)
library(tidyverse)

this.dir <- getwd()
dir <- 'C:/Users/CLam/Desktop/travel-study-stories/shiny'
setwd(dir)

# pers.dt <- fread('person.csv')
trip.dt <- fread('trip.csv')

cols <- c("age_category")

vlist <- NULL
for (col in cols) {
  dt <- unique(trip.dt[, ..col])
  dt[, `:=` (Label = col, Field = NULL, Variable = NULL)]
  setnames(dt, col, "Value")
  vlist[[col]] <- dt
}

values.lu <- rbindlist(vlist, use.names = T)

