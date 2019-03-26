function(input, output, session) {


# Crosstab Generator ------------------------------------------------------

    
  observeEvent(input$xtab_go,{
    type <- 'total'
    wt_field <- 'hh_wt_revised'
    crosstab <-cross_tab(survey, input$xtab_xcol, input$xtab_ycol, wt_field, type)
    output$xtab_table <- renderDT(crosstab, 
                                  options = list(bFilter=0))
  })
  
}