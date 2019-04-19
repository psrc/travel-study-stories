function(input, output, session) {


# Functions ---------------------------------------------------------------

  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = c("sample_count", "estimate", "share", "MOE", "N_HH")) {
    cols <- c("var1", str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }

# Crosstab Generator ------------------------------------------------------
  
  # return values associated with category selected
  output$ui_xtab_xcol <- renderUI({
    t <- variables.lu[Category %in% input$xtab_xcat, ]
    vars.raw <- as.list(t$Variables)
    vars.list <- setNames(vars.raw, as.list(t$Name))
    selectInput('xtab_xcol',
                'Variable', 
                width = '75%',
                vars.list)
  })
  
  # return values associated with category selected
  output$ui_xtab_ycol <- renderUI({
    t <- variables.lu[Category %in% input$xtab_ycat, ]
    vars.raw <- as.list(t$Variables)
    vars.list <- setNames(vars.raw, as.list(t$Name))
    selectInput('xtab_ycol',
                'Variable', 
                width = '75%',
                vars.list)
  })
  
  xtabTableType <- eventReactive(input$xtab_go, {
    select.vars <- variables.lu[Variables %in% c(input$xtab_xcol, input$xtab_ycol), ]
    tables <- unique(select.vars$Table)
    ifelse(tables == "Person", res <- "Person", res <- "Trip")
    return(res)
  })
  
  observeEvent(input$xtab_go, {
    table.type <- xtabTableType()
    if (table.type == "Person") {
      survey <- pers.dt
      wt_field <- 'hh_wt_revised'
    } else {
      survey <- trip.dt
      wt_field <- 'trip_weight_revised'
    }
    type <- 'total'
    crosstab <-cross_tab(survey, input$xtab_xcol, input$xtab_ycol, wt_field, type)
    col.headers<- c("sample_count", "estimate", "share", "MOE", "N_HH")
    dt.list <- list(xtab.col.subset(crosstab, "sample_count"),
                    xtab.col.subset(crosstab, "estimate"),
                    xtab.col.subset(crosstab, "share"),
                    xtab.col.subset(crosstab, "MOE"),
                    xtab.col.subset(crosstab, "N_HH"))
    names(dt.list) <- col.headers

    output$xtab_table_sample_count <- renderDT(dt.list$sample_count, options = list(bFilter=0))
    output$xtab_table_estimate <- renderDT(dt.list$estimate, options = list(bFilter=0))
    output$xtab_table_share <- renderDT(dt.list$share, options = list(bFilter=0))
    output$xtab_table_N_HH <- renderDT(dt.list$N_HH, options = list(bFilter=0))
    output$xtab_table_MOE <- renderDT(dt.list$MOE, options = list(bFilter=0))
  })
  
}