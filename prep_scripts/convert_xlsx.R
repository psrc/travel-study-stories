library(openxlsx)
library(data.table)
library(magrittr)

# wrkdir <- "C:/Users/CLam/Desktop/travel-study-stories/shiny" # local
wrkdir <- "J:/Projects/Surveys/HHTravel/Survey2017/Data/travel_crosstab/for_shiny_app"
outdir <- "C:/Users/CLam/Desktop/travel-study-stories/data"

variables.lu <- read.xlsx(file.path(wrkdir, 'variables.xlsx')) %>% as.data.table
# values.lu <- read.xlsx(file.path(wrkdir, 'variables_values.xlsx')) %>% as.data.table

fwrite(variables.lu, file.path(outdir, "variables.csv"))
# fwrite(values.lu, file.path(outdir, "variables_values.csv"))
