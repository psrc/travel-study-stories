# The R version of travel_crosstab.py

library(data.table)
library(tidyverse)


# create_cross_tab_with_weights
cross_tab <- function(table, var1, var2, wt_field, type = c("total", "mean")) {
  # z <- 1.96 # 95% CI
  z <- 1.645 # 90% CI
  
  print(var1)
  print(var2)
  print(wt_field)
  print("reading in data")

  cols <- c(var1, var2)
  
  if (type == "total") {
    setkeyv(table, cols)
    table[table==""]<- NA
    table <- na.omit(table, cols = cols)
    raw <- table[, .(sample_count = .N), by = cols] 
    N_hh <- table[, .(hhid = uniqueN(hhid)), by = var1]
    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = cols]
    expanded_moe <- table[, lapply(.SD, sd), .SDcols = wt_field, by = cols]
    names(expanded_moe)[names(expanded_moe)==wt_field]<- "SD_Total"
    expanded_moe$'MOE_Total'<-expanded_moe$'SD_Total' * z
    expanded_moe <- expanded_moe[,'SD_Total' := NULL]
    expanded <- merge(expanded, expanded_moe, by = cols)
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field, by = var1]
    setnames(expanded, wt_field, "estimate")
    expanded <- merge(expanded, expanded_tot, by = var1)
    expanded[, share := estimate/get(eval(wt_field))]
    expanded <- merge(expanded, N_hh, by = var1)
    expanded[, ("in") := (share*(1-share))/hhid][, MOE := z*sqrt(get("in"))][, N_HH := hhid]
    crosstab <- merge(raw, expanded, by = cols)
    print(crosstab)
    # crosstab output column names will differ from python output
    crosstab <- dcast.data.table(crosstab, 
                                 get(eval(var1)) ~ get(eval(var2)), 
                                 value.var = c('sample_count', 'estimate', 'MOE_Total','share', 'MOE', 'N_HH'))
  } else if (type == "mean") {
    tbl <- table[, (var2) := as.numeric(get(eval(var2)))][!is.na(get(eval(var2))), ][get(eval(var2)) != 0, ][get(eval(var2)) < 100, ]
    tbl[, weighted_total := get(eval(wt_field))*get(eval(var2))]
    expanded <- tbl[, lapply(.SD, sum), .SDcols = "weighted_total", by = var1][order(get(eval(var1)))]
    expanded_tot <- tbl[, lapply(.SD, sum), .SDcols = wt_field, by = var1]
    expanded_moe <- tbl[, lapply(.SD, function(x) sd(x)/sqrt(length(x))), .SDcols = var2, by = var1][order(get(eval(var1)))]
    print(expanded)
    print(expanded_moe)
    expanded <- merge(expanded, expanded_tot, by = var1)
    expanded <- merge(expanded, expanded_moe, by = var1)
    expanded[, mean := weighted_total/get(eval(wt_field))]
    crosstab <- expanded
  }
 
  return(crosstab)
}

simple_table <- function(table, var, wt_field, type = c("total")) {
  z <- 1.96
  
  print(var)

  if (type == "total") {
    setkeyv(table, var)
    table[table==""]<- NA
    table <- na.omit(table, cols = var)
    raw <- table[, .(sample_count = .N), by = var]
    N_hh <- table[, .(hhid = uniqueN(hhid)), by = var]
    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = var]
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field][[eval(wt_field)]]
    setnames(expanded, wt_field, "estimate")
    expanded[, share := estimate/eval(expanded_tot)]
    expanded <- merge(expanded, N_hh, by = var)
    expanded[, ("in") := (share*(1-share))/hhid][, MOE := z*sqrt(get("in"))][, N_HH := hhid]
    s_table <- merge(raw, expanded, by = var)
  }
  
return(s_table)  
}



