# The R version of travel_crosstab.py

library(data.table)
library(tidyverse)




# create_cross_tab_with_weights
cross_tab <- function(table, var1, var2, wt_field, type) {
  # z <- 1.96 # 95% CI
 

  print("reading in data")

  cols <- c(var1, var2)

  if (type == "dimension") {
    setkeyv(table, cols)
    table[table==""]<- NA
    for(missing in missing_codes){
       table<- subset(table, get(var1) != missing)
       table<- subset(table, get(var2) != missing)
     }    
    table <- na.omit(table, cols = cols)
    table<-table[!is.na(get(wt_field))]
    raw <- table[, .(sample_count = .N), by = cols] 
    N_hh <- table[, .(household_id = uniqueN(household_id)), by = var1]
    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = cols]
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field, by = var1]
    setnames(expanded, wt_field, "estimate")
    expanded <- merge(expanded, expanded_tot, by = var1)
    expanded[, share := estimate/get(eval(wt_field))]
    expanded <- merge(expanded, N_hh, by = var1)
    expanded[,p_col :=p_MOE] 
    expanded[, ("in") := (p_col*(1-p_col))/household_id][, MOE := z*sqrt(get("in"))][, N_HH := household_id]
    expanded$estMOE= expanded$MOE*expanded[[wt_field]]
    crosstab <- merge(raw, expanded, by = cols)
    crosstab <- dcast.data.table(crosstab, 
                                 get(eval(var1)) ~ get(eval(var2)), 
                                 value.var = c('sample_count', 'estimate', 'estMOE','share', 'MOE', 'N_HH'))
    
  } else if (type == "fact") {
    cols = c(var1, var2, 'household_id', wt_field)
    var_weights <- table[, cols, with = FALSE]
    for(missing in missing_codes){
      var_weights<- subset(var_weights, get(var1) != missing)
      var_weights<- subset(var_weights, get(var2) != missing)
    }  
    var_weights <- na.omit(var_weights)
    raw <- var_weights[, .(sample_count = .N), by = var1] 
    N_hh <- var_weights[, .(household_id = uniqueN(household_id)), by = var1]
    var_weights<-var_weights[eval(parse(text=var2))>min_float]
    var_weights<-var_weights[eval(parse(text=var2))<max_float]
    var_weights[, weighted_total := get(eval((wt_field)))*get(eval((var2)))]
    expanded <- var_weights[, lapply(.SD, sum), .SDcols = "weighted_total", by = var1][order(get(eval(var1)))]
    expanded_tot <- var_weights[, lapply(.SD, sum), .SDcols = wt_field, by = var1]
    expanded_moe <- var_weights[, lapply(.SD, function(x) z*sd(x)/sqrt(length(x))), .SDcols = var2, by = var1][order(get(eval(var1)))]
    setnames(expanded_moe, var2, 'MOE')
    expanded <- merge(expanded, expanded_tot, by = var1)
    expanded <- merge(expanded, expanded_moe, by = var1)
    expanded[, mean := weighted_total/get(eval(wt_field))]
    N_hh <- merge(raw, N_hh, by = var1)
    expanded <- merge(expanded, N_hh, by = var1)
    setnames(expanded, var1, 'var1')
    setnames(expanded, 'household_id', 'N_HH')
    crosstab <- expanded
    print(crosstab)
  }
 
  return(crosstab)
}

simple_table <- function(table, var, wt_field, type) {
  


  if (type == "dimension") {
    setkeyv(table, var)
    table[table==""]<- NA
    for(missing in missing_codes){
      table<- subset(table, get(var) != missing)
    }
    table <- na.omit(table, cols = var)
    raw <- table[, .(sample_count = .N), by = var]
    N_hh <- table[, .(household_id = uniqueN(household_id))]
    table<-table[!is.na(get(wt_field))]
    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = var]
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field][[eval(wt_field)]]
    expanded[,'household_id':=N_hh[['household_id']][1]]
    setnames(expanded, wt_field, "estimate")
    expanded[, share := estimate/eval(expanded_tot)]
    expanded[,p_col :=p_MOE]   
    expanded[, ("in") := (p_col*(1-p_col))/household_id][, MOE := z*sqrt(get("in"))][, N_HH := household_id]
    expanded$total <- sum(expanded$estimate)
    expanded$estMOE = expanded$MOE * expanded$total
    s_table <- merge(raw, expanded, by = var)
  
  }
  else if(type == "fact") {
    # rework this because really the cuts are just acting as the variables
    # I think this can have the same logic as the code above.
    setkeyv(table, var)
    table[table==""]<- NA
    for(missing in missing_codes){
      table<- subset(table, get(var) != missing)
    }
    cols<- c(var, wt_field)
    table <- na.omit(table)
    if(var == 'weighted_trip_count'){
      breaks<- hist_breaks_num_trips
      hist_labels <- hist_breaks_num_trips_labels
    }
    else{
      table <- table[eval(parse(text=var))>min_float]
      table <- table[eval(parse(text=var))<max_float]
      breaks<- hist_breaks
      hist_labels<- hist_breaks_labels
    }
    
    var_breaks <- table[, cuts := cut(eval(parse(text=var)),breaks,labels=hist_labels, order_result=TRUE,)]
    # to do: find a way to pull out this hard code

    
    N_hh <-table[,.(household_id = uniqueN(household_id)), by = cuts]
    raw <- table[, .(sample_count = .N), by = cuts]
    var_cut <-var_breaks[, lapply(.SD, sum), .SDcols = wt_field, by = cuts]
    setnames(var_cut, wt_field, "estimate")
    var_cut$total <- sum(var_cut$estimate)
    var_cut[, share := estimate/total]
    var_cut<- merge(var_cut, N_hh, by = 'cuts')
    var_cut[, ("in") := (share*(1-share))/household_id][, MOE := z*sqrt(get("in"))][, N_HH := household_id]
    var_cut$estMOE = var_cut$MOE * var_cut$total
    var_cut<- merge(raw, var_cut, by = 'cuts')
    s_table<-setnames(var_cut, 'cuts',var)
  }
  
return(s_table)  
}




