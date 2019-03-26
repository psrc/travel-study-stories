function(input, output, session) {


# Functions ---------------------------------------------------------------

  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = c("sample_count", "estimate", "share", "MOE", "N_HH")) {
    cols <- c("var1", str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }

# Crosstab Generator ------------------------------------------------------

    
  observeEvent(input$xtab_go,{
    type <- 'total'
    wt_field <- 'hh_wt_revised'
    crosstab <-cross_tab(survey, input$xtab_xcol, input$xtab_ycol, wt_field, type)
    # browser()
    col.headers<- c("sample_count", "estimate", "share", "MOE", "N_HH")
    dt.list <- list(xtab.col.subset(crosstab, "sample_count"),
                    xtab.col.subset(crosstab, "estimate"),
                    xtab.col.subset(crosstab, "share"),
                    xtab.col.subset(crosstab, "N_HH"),
                    xtab.col.subset(crosstab, "MOE"))
    names(dt.list) <- col.headers
    # output$xtab_table <- renderDT(crosstab, 
    #                               options = list(bFilter=0))
    output$xtab_table_sample_count <- renderDT(dt.list$sample_count, options = list(bFilter=0))
    output$xtab_table_estimate <- renderDT(dt.list$estimate, options = list(bFilter=0))
    output$xtab_table_share <- renderDT(dt.list$share, options = list(bFilter=0))
    output$xtab_table_N_HH <- renderDT(dt.list$N_HH, options = list(bFilter=0))
    output$xtab_table_MOE <- renderDT(dt.list$MOE, options = list(bFilter=0))
    
  })
  
}