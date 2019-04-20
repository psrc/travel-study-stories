function(input, output, session) {


# Functions ---------------------------------------------------------------

  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = c("sample_count", "estimate", "share", "MOE", "N_HH")) {
    cols <- c(varsXAlias(), str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }
  
  
# Crosstab Generator ------------------------------------------------------
  
  # variable X alias list
  varsListX <- reactive({
    t <- variables.lu[Category %in% input$xtab_xcat, ]
    vars.raw <- as.list(t$Variables)
    vars.list <- setNames(vars.raw, as.list(t$Name))
  })
  
  # variable Y alias list
  varsListY <- reactive({
    t <- variables.lu[Category %in% input$xtab_ycat, ]
    vars.raw <- as.list(t$Variables)
    vars.list <- setNames(vars.raw, as.list(t$Name))
  })
  
  # variable X alias
  varsXAlias <- reactive({
    xvar.alias <- variables.lu[Variables %in% input$xtab_xcol, .(Name)]
    xvar.alias$Name
  })
  
  # return values associated with category selected
  output$ui_xtab_xcol <- renderUI({
    selectInput('xtab_xcol',
                'Variable', 
                width = '75%',
                varsListX())
  })
  
  # return values associated with category selected
  output$ui_xtab_ycol <- renderUI({
    selectInput('xtab_ycol',
                'Variable', 
                width = '75%',
                varsListY())
  })
  
  xtabTableType <- eventReactive(input$xtab_go, {
    select.vars <- variables.lu[Variables %in% c(input$xtab_xcol, input$xtab_ycol), ]
    tables <- unique(select.vars$Table)
    ifelse(tables == "Person", res <- "Person", res <- "Trip")
    return(res)
  })
  
  # return list of tables subsetted by value types
  xtabTable <- eventReactive(input$xtab_go, {
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
      # test.varsListX <- varsListX()
      # test.varsXAlias <- varsXAlias()
      # browser()
      setnames(crosstab, "var1", varsXAlias())
      col.headers <- c("sample_count", "estimate", "share", "MOE", "N_HH")
      xtab.crosstab <- partial(xtab.col.subset, table = crosstab)
      dt.list <- map(as.list(col.headers), xtab.crosstab)
      names(dt.list) <- col.headers
      return(dt.list)
  })
  
  output$xtab_download <- downloadHandler(
    filename = function() {
      paste("hh_survey_crosstab.xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(xtabTable(), file)
    }
  )
  
  output$xtab_table_share <- renderDT({
    dt <- xtabTable()$share
    DT::datatable(dt,
                  options = list(bFilter=0)) %>%
      formatPercentage(colnames(dt)[2:length(colnames(dt))], 1)
  })
  
  output$xtab_table_estimate <- renderDT({
    dt <- xtabTable()$estimate
    DT::datatable(dt, 
                  options = list(bFilter=0)) %>%
      formatRound(colnames(dt)[2:length(colnames(dt))], 0)
  })
  
  output$xtab_table_N_HH <- renderDT({
    dt <- xtabTable()$N_HH
    DT::datatable(dt, options = list(bFilter=0)) %>%
      formatRound(colnames(dt)[2:length(colnames(dt))], 0)
  })

  output$xtab_table_MOE <- renderDT({
    DT::datatable(xtabTable()$MOE, options = list(bFilter=0))%>%
      formatPercentage(colnames(dt)[2:length(colnames(dt))], 2)
  })
  
  output$xtab_table_sample_count <- renderDT({
    dt <- xtabTable()$sample_count
    DT::datatable(dt, options = list(bFilter=0)) %>%
      formatRound(colnames(dt)[2:length(colnames(dt))], 0)
  })
  
 
  
}