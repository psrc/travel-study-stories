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
  }
 
  return(crosstab)
}

simple_table <- function(table, var, wt_field, type = c("total")) {
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
    setkeyv(table, var)
    table[table==""]<- NA
    cols<- c(var, wt_field)
    var_weights <- table[, cols, with = FALSE]
    var_weights <- na.omit(var_weights)
    var_weights <- var_weights[eval(parse(text=var))>min_float]
    var_weights <- var_weights[eval(parse(text=var))<max_float]
    #breaks <- quantile(unlist(var_weights[,var, with =FALSE]), probs=seq(0,1,quantile_break))
    
    breaks<- hist_breaks
    var_breaks <- var_weights[, cuts := cut(eval(parse(text=var)),breaks, order_result=TRUE, dig.lab=1)]
    var_cut <-var_breaks[, lapply(.SD, sum), .SDcols = wt_field, by = cuts]
    var_mean<-weighted.mean(var_weights[, var, with =FALSE], var_weights[,wt_field, with=FALSE])
    s_table<-c(var_cut,var_mean)
    print(s_table)
  }
  
return(s_table)  
}



