
function(input, output, session) {
  

# Functions ---------------------------------------------------------------

  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = c("sample_count", "estimate", "share", "MOE", "N_HH")) {
    cols <- c(varsXAlias(), str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }
  
  xtab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel, ylabel) {
    f <- list(family = "Lato")
    
    if (format == "percent") {
      yscale <- scales::percent
    } else if (format == "nominal") {
      yscale <- scales::comma
    }
    
    g <- ggplot(table, 
                aes_string(x = "value", 
                           y = "result", 
                           group = colnames(table)[1], 
                           fill = colnames(table)[1])) +
      geom_col(position = position_dodge(preserve = "single")) +
      theme_minimal() +
      labs(fill = xlabel,
           x = ylabel,
           y = NULL) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_y_continuous(labels = yscale) +
      theme(axis.title.x = element_text(margin = margin(t=30)),
            axis.title.y = element_text(margin = margin(r=20)))
    
    p <- ggplotly(g) %>% layout(font = f)
  }
  
  stab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel) {
    f <- list(family = "Lato")
    
    if (format == "percent") {
      yscale <- scales::percent
    } else if (format == "nominal") {
      yscale <- scales::comma
    }
    
    g <- ggplot(table, 
                aes_string(x = "value", 
                           y = "result", 
                           fill = colnames(table)[1])) +
      geom_col(position = position_dodge(preserve = "single")) +
      theme_minimal() +
      labs(fill = NULL,
           x = xlabel,
           y = NULL) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_y_continuous(labels = yscale) +
      theme(axis.title.x = element_text(margin = margin(t=30)),
            axis.title.y = element_text(margin = margin(r=20)),
            legend.position = 'none')
    
    p <- ggplotly(g) %>% layout(font = f)
  }
  
  # a generic container for crosstab tables
  dt.container <- function(atable, xvaralias, yvaralias) {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 2, xvaralias),
          th(class = 'dt-center', colspan = (ncol(atable)-1), yvaralias)
        ), # end tr
        tr(
          lapply(colnames(atable)[2:(ncol(atable))], th)
        ) # end tr
      ) # end thead
    ) # end table
    ) # end withTags
  }
  

# Crosstab Generator Selection --------------------------------------------


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
                # width = '75%',
                varsListX())
  })
  
  # return values associated with category selected
  output$ui_xtab_ycol <- renderUI({
    selectInput('xtab_ycol',
                'Variable', 
                # width = '75%',
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

  
# Crosstab Generator data wrangling ---------------------------------------
  
  
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

# Crosstab Generator Visuals ----------------------------------------------

  
  xtabVisTable <- reactive({
    dt.list <- xtabTableClean()
    xvals <- xtabXValues()[, .(Variable, Value)]
    
    visdt.list <- NULL
    for (i in 1:length(dt.list)) {
      idcol <- varsXAlias()
      msrcols <- colnames(dt.list[[i]])[!(colnames(dt.list[[i]]) %in% idcol)]
      varcol <- "value"
      t <- melt.data.table(dt.list[[i]], id.vars = idcol, measure.vars = msrcols, variable.name = "value", value.name = "result")
      t[, type := names(dt.list[i])]
      setnames(t, idcol, "group")
      if (nrow(xvals) != 0) {
        t[, group := factor(group, levels = xvals$Value)][, group := fct_explicit_na(group, "No Response")]
        t <- t[order(group)]
      }
      visdt.list[[names(dt.list[i])]]<- t
    }
    return(visdt.list)
  }) 
  
  output$xtab_vis <- renderPlotly({
    xlabel <- varsXAlias() # first dim
    ylabel <- varsYAlias() # second dim
    dttype <- input$xtab_dtype_rbtns
    dt <- xtabVisTable()[[dttype]]
    
    if (dttype == 'share') {
      p <- xtab.plot.bar(dt, "percent", xlabel, ylabel)
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      p <- xtab.plot.bar(dt, "nominal", xlabel, ylabel)
      return(p)
    } else {
      return(NULL)
    }
    
  })
  
# Crosstab Generator Table Rendering --------------------------------------------
  
  
  output$xtab_tbl <- renderDT({
    dttype <- input$xtab_dtype_rbtns
    dt <- xtabTableClean()[[dttype]]
    sketch <- dt.container(dt, varsXAlias(), varsYAlias())

    if (dttype == 'share') {
      DT::datatable(dt,
                    container = sketch,
                    rownames = FALSE,
                    options = list(bFilter=0
                                   )) %>%
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
  

# Crosstab Generator Download ---------------------------------------------


  output$xtab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_", varsXAlias(), "_by_", varsYAlias(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(xtabTableClean(), file)
    }
  )
  
  # Simple Table ------------------------------------------------------------
  
  
  # variable X alias
  stab.varsXAlias <- eventReactive(input$stab_go, {
    xvar.alias <- variables.lu[Variables %in% input$stab_xcol, .(Name)]
    xvar.alias$Name
  })
  
  # variable X alias list
  stab.varsListX <- reactive({
    t <- variables.lu[Category %in% input$stab_xcat, ]
    vars.raw <- as.list(t$Variables)
    vars.list <- setNames(vars.raw, as.list(t$Name))
  })
  
  output$ui_stab_xcol <- renderUI({
    selectInput('stab_xcol',
                'Variable', 
                # width = '75%',
                stab.varsListX())
  })
  
  stabXValues <- eventReactive(input$stab_go, {
    dt <- values.lu[Label %in% input$stab_xcol, ] # return dt
  })

# Simple Table Data Wrangling ---------------------------------------------

  
  stabTableType <- eventReactive(input$stab_go, {
    select.vars <- variables.lu[Variables %in% c(input$stab_xcol), ]
    tables <- unique(select.vars$Table)
    ifelse(tables == "Person", res <- "Person", res <- "Trip")
    return(res)
  })
  
  # return list of tables subsetted by value types
  stabTable <- eventReactive(input$stab_go, {
    table.type <- stabTableType()
    if (table.type == "Person") {
      survey <- pers.dt
      wt_field <- 'hh_wt_revised'
    } else {
      survey <- trip.dt
      wt_field <- 'trip_weight_revised'
    }
    type <- 'total'
    
    simtable <- simple_table(survey, input$stab_xcol, wt_field, type)
    xvals <- stabXValues()[, .(Variable, Value)]

    simtable[, var1.sort := factor(get(input$stab_xcol), levels = xvals$Value)]
    simtable <- simtable[order(var1.sort)][, var1.sort := NULL]
    
    dtypes <- dtype.choice[!(dtype.choice %in% "N_HH")] # remove N_HH
    selcols <- c(stab.varsXAlias(), names(dtypes))
    setnames(simtable, c(input$stab_xcol, dtypes), selcols)
    setcolorder(simtable, selcols)
    dt <- simtable[, ..selcols]
  })
  
  output$stab_tbl <- renderDT({
    dt <- stabTable()

    fmt.per <- names(dtype.choice[dtype.choice %in% c('share', 'MOE')])
    fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])
    DT::datatable(dt,
                  options = list(bFilter=0, 
                                 pageLength = 6,
                                 autoWidth = TRUE,
                                 columnDefs = list(list(width = '280px', targets = c(2:ncol(dt)))))) %>%
      formatPercentage(fmt.per, 1) %>%
      formatRound(fmt.num, 0)
  })

# Simple Table Visuals -----------------------------------------------------

  
  stabVisTable <- reactive({
    dt <- stabTable()
    idvar <- stab.varsXAlias()
    xvals <- stabXValues()[, .(Variable, Value)]
    
    cols <- c('Sample Count')
    dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    msr.vars <- names(dtype.choice[names(dtype.choice) %in% colnames(dt)])
    t <- melt.data.table(dt, id.vars = idvar, measure.vars = msr.vars, variable.name = "type", value.name = "result")
    setnames(t, idvar, "value")
    
    if (nrow(xvals) != 0) {
      t[, value := factor(value, levels = xvals$Value)][, value := fct_explicit_na(value, "No Response")]
      t <- t[order(value)]
    }
    
    return(t)
  })
  
  output$stab_vis <- renderPlotly({
    xlabel <- stab.varsXAlias() # first dim
    dttype <- input$stab_dtype_rbtns
    selection <- names(dtype.choice[dtype.choice %in% dttype])
    dt <- stabVisTable()[type %in% selection, ]
    
    if (dttype == 'share') {
      p <- stab.plot.bar(dt, "percent", xlabel)
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      p <- stab.plot.bar(dt, "nominal", xlabel)
      return(p)
    } else {
      return(NULL)
    }
  })


# Simple Table Download ---------------------------------------------------

  
  output$stab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_", stab.varsXAlias(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(stabTable(), file)
    }
  )
  
}



