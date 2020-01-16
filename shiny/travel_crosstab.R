# The R version of travel_crosstab.py

library(data.table)
library(tidyverse)




# create_cross_tab_with_weights
cross_tab <- function(table, var1, var2, wt_field, type) {
  # z <- 1.96 # 95% CI
  z <- 1.645 # 90% CI

  print("reading in data")

  cols <- c(var1, var2)
  print(type)
  if (type == "dimension") {
    setkeyv(table, cols)
    table[table==""]<- NA
    table <- na.omit(table, cols = cols)
    table<-table[!is.na(get(wt_field))]
    raw <- table[, .(sample_count = .N), by = cols] 
    N_hh <- table[, .(hhid = uniqueN(hhid)), by = var1]
    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = cols]
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field, by = var1]
    setnames(expanded, wt_field, "estimate")
    expanded <- merge(expanded, expanded_tot, by = var1)
    expanded[, share := estimate/get(eval(wt_field))]
    expanded <- merge(expanded, N_hh, by = var1)
    expanded[, ("in") := (share*(1-share))/hhid][, MOE := z*sqrt(get("in"))][, N_HH := hhid]
    expanded$estMOE= expanded$MOE*expanded[[wt_field]]
    crosstab <- merge(raw, expanded, by = cols)
    crosstab <- dcast.data.table(crosstab, 
                                 get(eval(var1)) ~ get(eval(var2)), 
                                 value.var = c('sample_count', 'estimate', 'estMOE','share', 'MOE', 'N_HH'))
  } else if (type == "fact") {
    cols = c(var1, var2, wt_field)
    var_weights <- table[, cols, with = FALSE]
    var_weights <- na.omit(var_weights)
    print(var_weights)
    var_weights<-var_weights[eval(parse(text=var2))>min_float]
    var_weights<-var_weights[eval(parse(text=var2))<max_float]
    var_weights[, weighted_total := get(eval((wt_field)))*get(eval((var2)))]
    expanded <- var_weights[, lapply(.SD, sum), .SDcols = "weighted_total", by = var1][order(get(eval(var1)))]
    expanded_tot <- var_weights[, lapply(.SD, sum), .SDcols = wt_field, by = var1]
    expanded_moe <- var_weights[, lapply(.SD, function(x) z*sd(x)/sqrt(length(x))), .SDcols = var2, by = var1][order(get(eval(var1)))]
    setnames(expanded_moe, var2, 'MOE')
    expanded <- merge(expanded, expanded_tot, by = var1)
    print(expanded)
    expanded <- merge(expanded, expanded_moe, by = var1)
    expanded[, mean := weighted_total/get(eval(wt_field))]
    crosstab <- expanded
    print(crosstab)
  }
 
  return(crosstab)
}

simple_table <- function(table, var, wt_field, type) {
  z <- 1.645
  


  if (type == "dimension") {
    setkeyv(table, var)
    table[table==""]<- NA
    table <- na.omit(table, cols = var)
    raw <- table[, .(sample_count = .N), by = var]
    N_hh <- table[, .(hhid = uniqueN(hhid)), by = var]
    table<-table[!is.na(get(wt_field))]
    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = var]
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field][[eval(wt_field)]]
    print(expanded_tot)
    setnames(expanded, wt_field, "estimate")
    expanded[, share := estimate/eval(expanded_tot)]
    expanded <- merge(expanded, N_hh, by = var)
    expanded[, ("in") := (share*(1-share))/hhid][, MOE := z*sqrt(get("in"))][, N_HH := hhid]
    expanded$total <- sum(expanded$estimate)
    expanded$estMOE = expanded$MOE * expanded$total
    s_table <- merge(raw, expanded, by = var)
  
  }
  else if(type == "fact") {
    # rework this because really the cuts are just acting as the variables
    # I think this can have the same logic as the code above.
    setkeyv(table, var)
    table[table==""]<- NA
    cols<- c(var, wt_field)
    table <- na.omit(table)
    if(var == 'weighted_trip_count'){
      breaks<- hist_breaks_num_trips
    }
    else{
      table <- table[eval(parse(text=var))>min_float]
      table <- table[eval(parse(text=var))<max_float]
      breaks<- hist_breaks
    }
    
    var_breaks <- table[, cuts := cut(eval(parse(text=var)),breaks, order_result=TRUE, dig.lab=1)]
    # to do: find a way to pull out this hard code

    
    N_hh <-table[,.(hhid = uniqueN(hhid)), by = cuts]
    raw <- table[, .(sample_count = .N), by = cuts]
    var_cut <-var_breaks[, lapply(.SD, sum), .SDcols = wt_field, by = cuts]
    setnames(var_cut, wt_field, "estimate")
    print(var_cut)
    var_cut$total <- sum(var_cut$estimate)
    var_cut[, share := estimate/total]
    var_cut<- merge(var_cut, N_hh, by = 'cuts')
    var_cut[, ("in") := (share*(1-share))/hhid][, MOE := z*sqrt(get("in"))][, N_HH := hhid]
    var_cut$estMOE = var_cut$MOE * var_cut$total
    var_cut<- merge(raw, var_cut, by = 'cuts')
    s_table<-setnames(var_cut, 'cuts',var)
  }
  
return(s_table)  
}



