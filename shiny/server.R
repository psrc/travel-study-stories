
function(input, output, session) {
  

# Functions ---------------------------------------------------------------

  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = c("sample_count", "estimate", "share", "MOE", "N_HH")) {
    cols <- c(varsXAlias(), str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }
  
  plot.format.nums <- function(format = c("percent", "nominal")) {
    if (format == "percent") {
      yscale <- scales::percent
    } else if (format == "nominal") {
      yscale <- scales::comma
    }
    return(yscale)
  }
  
  xtab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel, ylabel, dttype.label) {
    f <- list(family = "Lato")
    yscale <- plot.format.nums(format)
    
    g <- ggplot(table, 
                aes(x = value,
                    y = result,
                    group = get(colnames(table)[1]),
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), group,
                                 paste0('<br>', ylabel, ':'), value,
                                 paste0('<br>', dttype.label, ':'), yscale(result))
                    )
                ) +
      geom_col(position = position_dodge(preserve = "single")) +
      theme_minimal() +
      labs(fill = xlabel,
           x = ylabel,
           y = NULL) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_y_continuous(labels = yscale) +
      theme(axis.title.x = element_text(margin = margin(t=30)),
            axis.title.y = element_text(margin = margin(r=20)))
    
    p <- ggplotly(g, tooltip = "text") %>% layout(font = f)
  }
  
  xtab.plot.bar.pivot <- function(table, format = c("percent", "nominal"), xlabel, ylabel, dttype.label) {
    f <- list(family = "Lato")
    yscale <- plot.format.nums(format)
    
    g <- ggplot(table, 
                aes(x = value,
                    y = result,
                    group = get(colnames(table)[1]),
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), group,
                                 paste0('<br>', ylabel, ':'), value,
                                 paste0('<br>', dttype.label, ':'), yscale(result))
                )
    ) +
      geom_col(position = position_dodge(preserve = "single")) +
      theme_minimal() +
      labs(fill = xlabel,
           x = ylabel,
           y = NULL) +
      scale_y_continuous(labels = yscale) +
      theme(axis.title.x = element_text(margin = margin(t=30)),
            axis.title.y = element_text(margin = margin(r=20))) +
      coord_flip()
    
    p <- ggplotly(g, tooltip = "text") %>% layout(font = f)
  }
  
  stab.plot.bar <- function(table, format = c("percent", "nominal"), xlabel) {
    f <- list(family = "Lato")
    yscale <- plot.format.nums(format)
    
    g <- ggplot(table,
                aes(x = value,
                    y = result,
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), value,
                                paste0('<br>', type, ':'), yscale(result))
                    )
                ) +
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
    
    p <- ggplotly(g, tooltip = "text") %>% layout(font = f)
  }
  
  stab.plot.bar2 <- function(table, format = c("percent", "nominal"), xlabel) {
    f <- list(family = "Lato")
    yscale <- plot.format.nums(format)
    
    g <- ggplot(table, 
                aes(x = value, 
                    y = result, 
                    fill = get(colnames(table)[1]),
                    text = paste(paste0(xlabel,':'), value,
                                 paste0('<br>', type, ':'), yscale(result))
                    )
                ) +
      geom_col(position = position_dodge(preserve = "single")) +
      theme_minimal() +
      labs(fill = NULL,
           x = xlabel,
           y = NULL) +
      scale_y_continuous(labels = yscale) +
      theme(axis.title.x = element_text(margin = margin(t=30)),
            axis.text.x = element_blank(),
            axis.title.y = element_text(margin = margin(r=20))#,
      )
    
    p <- ggplotly(g, tooltip = "text") %>% layout(font = f)
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

  # show/hide vars definition
  observe({
    onclick("xtabXtoggleAdvanced",
            toggle(id = "xtabXAdvanced", anim = TRUE))  
    onclick("xtabYtoggleAdvanced",
            toggle(id = "xtabYAdvanced", anim = TRUE))
    
  })
  
  output$xtab_xcol_det <- renderText({
    xvar.det <- variables.lu[Variables %in% input$xtab_xcol, .(Detail)]
    xvar.det$Detail
  })
  
  output$xtab_ycol_det <- renderText({
    yvar.det <- variables.lu[Variables %in% input$xtab_ycol, .(Detail)]
    yvar.det$Detail
  })
  
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
                varsListX())
  })
  
  # return values associated with category selected
  output$ui_xtab_ycol <- renderUI({
    selectInput('xtab_ycol',
                'Variable', 
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
    
    # evaluates for NA columns & rows, and excludes it
    for (i in 1:length(dt.list)) {
      dt.list[[i]] <- dt.list[[i]][!(get(eval(xa)) %in% "")]

      new.colnames <- str_extract(colnames(dt.list[[i]])[2:length(colnames(dt.list[[i]]))], paste0("(?<=", regex, ").+")) #includes blank

      if (any(is.na(new.colnames))) { # if contains any NA columns
        nonna.new.colnames <- str_subset(new.colnames, ".")
        setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
        keep.cols <- colnames(dt.list[[i]])[!is.na(colnames(dt.list[[i]]))]
        dt.list[[i]] <- dt.list[[i]][, ..keep.cols]

        if (length(yv) != 0) {
          yv.subset <- yv[yv %in% nonna.new.colnames] # only account for yv vals that exist in dt
          setcolorder(dt.list[[i]], c(xa, yv.subset))
        }
      } else {
        setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
        if (!is.null(yv)) {
          yv.subset <- yv[yv %in% new.colnames] # are all yv vals accounted for in new.colnames
          setcolorder(dt.list[[i]], c(xa, yv.subset))
        }
      }
    }

    # # evaluate for blank values in first dim
    # xvals <- dt.list[[1]][, ..xa]
    # blanks <- nrow(xvals[get(eval(xa)) %in% ""])
    # 
    # # evaluates for NA columns, naming it as No Response, and excludes it
    # for (i in 1:length(dt.list)) {
    #   if (blanks >= 1) dt.list[[i]][get(eval(xa)) %in% "", (xa) := "No Response"]
    #   dt.list[[i]] <- dt.list[[i]][!(get(eval(xa)) %in% "No Response")]
    #   
    #   new.colnames <- str_extract(colnames(dt.list[[i]])[2:length(colnames(dt.list[[i]]))], paste0("(?<=", regex, ").+"))
    #   
    #   if (any(is.na(new.colnames))) { # if contains any NA columns
    #     nonna.new.colnames <- str_subset(new.colnames, ".")
    #     new.colnames[is.na(new.colnames)] <- "No Response"
    #     setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
    #    
    #     if (length(yv) != 0) {
    #       yv.subset <- yv[yv %in% nonna.new.colnames] # are all yv vals accounted for in new.colnames excluding 'No Response'
    #       setcolorder(dt.list[[i]], c(xa, "No Response", yv.subset))
    #       setcolorder(dt.list[[i]], c(xa, yv.subset))
    #     }
    #     keep.cols <- colnames(dt.list[[i]])[!(colnames(dt.list[[i]]) %in% "No Response")]
    #     dt.list[[i]] <- dt.list[[i]][, ..keep.cols]
    #   
    #   } else {
    #     setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
    #     if (!is.null(yv)) {
    #       yv.subset <- yv[yv %in% new.colnames] # are all yv vals accounted for in new.colnames
    #       setcolorder(dt.list[[i]], c(xa, yv.subset))
    #     }
    #   }
    # }
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
    dttype.label <- names(dtype.choice[dtype.choice == dttype])
    dt <- xtabVisTable()[[dttype]]
 
    l <- length(unique(dt$value))
  
    if (dttype == 'share') {
      ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "percent", xlabel, ylabel, dttype.label), p <- xtab.plot.bar(dt, "percent", xlabel, ylabel, dttype.label))
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "nominal", xlabel, ylabel, dttype.label), p <- xtab.plot.bar(dt, "nominal", xlabel, ylabel, dttype.label))
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
  
  # show/hide vars definition
  observe({
    onclick("stabXtoggleAdvanced",
            toggle(id = "stabXAdvanced", anim = TRUE))  
  })
  
  output$stab_xcol_det <- renderText({
    xvar.det <- variables.lu[Variables %in% input$stab_xcol, .(Detail)]
    xvar.det$Detail
  })
  
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
    
    xa <- stab.varsXAlias()
    
    simtable <- simple_table(survey, input$stab_xcol, wt_field, type)
    xvals <- stabXValues()[, .(Variable, Value)]

    simtable[, var1.sort := factor(get(input$stab_xcol), levels = xvals$Value)]
    simtable <- simtable[order(var1.sort)][, var1.sort := NULL]
   
    dtypes <- dtype.choice[!(dtype.choice %in% "N_HH")] # remove N_HH
    selcols <- c(xa, names(dtypes))
    setnames(simtable, c(input$stab_xcol, dtypes), selcols)
    setcolorder(simtable, selcols)

    # # evaluate for blank values 
    # xvals <- simtable[, ..xa]
    # blanks <- nrow(xvals[get(eval(xa)) %in% ""])
    # if (blanks >= 1) simtable[get(eval(xa)) %in% "", (xa) := "No Response"]
    
    dt <- simtable[!(get(eval(xa)) %in% "")][, ..selcols]
  })
  
  output$stab_tbl <- renderDT({
    dt <- stabTable()

    fmt.per <- names(dtype.choice[dtype.choice %in% c('share', 'MOE')])
    fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])
    DT::datatable(dt,
                  options = list(bFilter=0, 
                                 pageLength = 6,
                                 autoWidth = FALSE,
                                 columnDefs = list(list(width = '100px', targets = c(2:ncol(dt))))
                                 )
                  ) %>%
      formatPercentage(fmt.per, 1) %>%
      formatRound(fmt.num, 0)
  })

# Simple Table Visuals -----------------------------------------------------

  
  stabVisTable <- reactive({
    dt <- stabTable()
    idvar <- stab.varsXAlias()
    xvals <- stabXValues()[, .(Variable, Value)]
    
    cols <- names(dtype.choice[dtype.choice %in% c("sample_count")])
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

    l <- length(stabXValues()$Value) 
    if(l == 0) l <- length(unique(dt$value)) # evaluate if values are not in lookup (length 0)
    
    if (dttype == 'share') {
      ifelse(l < 10, p <- stab.plot.bar(dt, "percent", xlabel), p <- stab.plot.bar2(dt, "percent", xlabel))
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      ifelse(l < 10, p <- stab.plot.bar(dt, "nominal", xlabel), p <- stab.plot.bar2(dt, "nominal", xlabel))
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



