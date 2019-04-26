
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
  varsXAlias <- eventReactive(input$xtab_go, {
    xvar.alias <- variables.lu[Variables %in% input$xtab_xcol, .(Name)]
    xvar.alias$Name
  })
  
  # variable Y alias
  varsYAlias <- eventReactive(input$xtab_go, {
    yvar.alias <- variables.lu[Variables %in% input$xtab_ycol, .(Name)]
    yvar.alias$Name
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
                varsListY(),
                selected = varsListY()[[2]])
  })
  
  xtabXValues <- eventReactive(input$xtab_go, {
    dt <- values.lu[Label %in% input$xtab_xcol, ] # return dt
  })
  
  xtabYValues <- eventReactive(input$xtab_go, {
    dt <- values.lu[Label %in% input$xtab_ycol, ]
    v <- as.vector(dt$Value) # return vector
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
      xvals <- xtabXValues()[, .(Variable, Value)]

      crosstab[, var1.sort := factor(var1, levels = xvals$Value)]
      crosstab <- crosstab[order(var1.sort)][, var1.sort := NULL]
      
      setnames(crosstab, "var1", varsXAlias())
      col.headers <- c("sample_count", "estimate", "share", "MOE", "N_HH")
      xtab.crosstab <- partial(xtab.col.subset, table = crosstab)
      dt.list <- map(as.list(col.headers), xtab.crosstab)
      names(dt.list) <- col.headers
      return(dt.list)
  })
  
  # clean xtabTable()
  xtabTableClean <- reactive({
    dt.list <- xtabTable()
    yv <- xtabYValues()
    xa <- varsXAlias()
    
    col.headers <- c("sample_count", "estimate", "share", "MOE", "N_HH")
    col.headers <- lapply(col.headers, function(x) paste0(x, "_")) %>% unlist
    regex <- paste(col.headers, collapse = "|")
    # browser()
    for (i in 1:length(dt.list)) {
      new.colnames <- str_extract(colnames(dt.list[[i]])[2:length(colnames(dt.list[[i]]))], paste0("(?<=", regex, ").+"))
      
      if (any(is.na(new.colnames))) { # if contains any NA columns
        nonna.new.colnames <- str_subset(new.colnames, ".")
        new.colnames[is.na(new.colnames)] <- "No Response"
        setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
        if (!is.null(yv)) {
          yv.subset <- yv[yv %in% nonna.new.colnames] # are all yv vals accounted for in new.colnames excluding 'No Response'
          setcolorder(dt.list[[i]], c(xa, "No Response", yv.subset))
        }
      } else {
        setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
        if (!is.null(yv)) {
          yv.subset <- yv[yv %in% new.colnames] # are all yv vals accounted for in new.colnames
          setcolorder(dt.list[[i]], c(xa, yv.subset))
        }
      }
    }
   return(dt.list)
  })
  
  xtabVisTable <- reactive({
    dt.list <- xtabTableClean()
    # browser()
    visdt.list <- NULL
    for (i in 1:length(dt.list)) {
      idcol <- varsXAlias()
      msrcols <- colnames(dt.list[[i]])[!(colnames(dt.list[[i]]) %in% idcol)]
      varcol <- "value"
      t <- melt.data.table(dt.list[[i]], id.vars = idcol, measure.vars = msrcols, variable.name = "value", value.name = "result")
      t[, type := names(dt.list[i])]
      visdt.list[[names(dt.list[i])]]<- t
    }
    return(visdt.list)
  }) 
  
# Crosstab Generator Rendering --------------------------------------------
  
  output$xtab_tbl <- renderDT({
    dttype <- input$xtab_dtype_rbtns
    dt <- xtabTableClean()[[dttype]]

    if (dttype == 'share') {
      DT::datatable(dt,
                    options = list(bFilter=0)) %>%
        formatPercentage(colnames(dt)[2:length(colnames(dt))], 1)
    } else if (dttype == 'estimate') {
      DT::datatable(dt, 
                    options = list(bFilter=0)) %>%
        formatRound(colnames(dt)[2:length(colnames(dt))], 0)
    } else if (dttype == 'N_HH') {
      DT::datatable(dt, options = list(bFilter=0)) %>%
        formatRound(colnames(dt)[2:length(colnames(dt))], 0)
    } else if (dttype == 'MOE') {
      DT::datatable(dt, options = list(bFilter=0))%>%
        formatPercentage(colnames(dt)[2:length(colnames(dt))], 2)
    } else if (dttype == 'sample_count') {
      DT::datatable(dt, options = list(bFilter=0)) %>%
        formatRound(colnames(dt)[2:length(colnames(dt))], 0)
    }
  })

  # output$ui_xtab_vis <- renderUI(input$xtab_go, {
  #   tabPanel("Graph",
  #            br(),
  #            plotOutput("xtab_vis"))
  # })
  
  output$xtab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_", varsXAlias(), "_by_", varsYAlias(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(xtabTableClean(), file)
    }
  )
}