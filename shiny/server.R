
function(input, output, session) {
  

# Functions ---------------------------------------------------------------
  
  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = c("sample_count", "estimate", "share", "MOE", "N_HH")) {
    cols <- c(varsXAlias(), str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }
  
  xtab.join.samplecnt <- function(xtabcleandt, dttype, varsXAlias) {
    dt.data <- xtabcleandt[[dttype]]
    dt.sort.rows <- dt.data[[varsXAlias]]
    dt.style <- xtabcleandt[['sample_count']]
    colnames(dt.style)[2:length(colnames(dt.style))] <- paste0(colnames(dt.style)[2:length(colnames(dt.style))], "_sc")
    dt <- merge(dt.data, dt.style, by = varsXAlias)
    dt[, var1.sort := factor(get(varsXAlias), levels = dt.sort.rows)]
    dt <- dt[order(var1.sort)][, var1.sort := NULL]
  }
  
  xtab.shareMOE.join.samplecnt <- function(xtabcleanshareMOEdt, xtabcleandt, dttype, varsXAlias) {
    dt.data <- xtabcleanshareMOEdt
    dt.style <- xtabcleandt[['sample_count']]
    dt.sort.rows <- dt.data[[varsXAlias]]
    
    idx <- 2:ncol(dt.style)
    colnames(dt.style)[idx] <- paste0(letters[1:(ncol(dt.style)-1)], "_", colnames(dt.style)[idx], "_sc")
    new.cols <- paste0(colnames(dt.style)[idx], "2")
    cols <- colnames(dt.style)[idx]
    col2order <- sort(c(cols, new.cols))
    dt.style[, (new.cols) := mapply(function(x) replicate(1, .SD[[x]]), cols, SIMPLIFY = F)]
    setcolorder(dt.style, c(varsXAlias, col2order))
    dt <- merge(dt.data, dt.style, by = varsXAlias)
    
    dt[, var1.sort := factor(get(varsXAlias), levels = dt.sort.rows)]
    dt <- dt[order(var1.sort)][, var1.sort := NULL]
  } 
  
  xtab.create.DT <- function(atable, moe = c(TRUE, FALSE), acontainer, indices2hide, maxyvals, sc.cols) {
    ltgrey <- '#bdbdc3'
    if (moe == TRUE) {
      defs <- list(list(className = "dt-head-center dt-center", targets = "_all"),# DT CRAN hack
                   list(visible = F, targets = indices2hide)) # DT's column index starts at 0 not 1
    } else {
      defs <- list(list(visible = F, targets = indices2hide)) # DT's column index starts at 0 not 1
    }

     DT::datatable(atable,
                   container = acontainer,
                   rownames = FALSE,
                   options = list(bFilter=0,
                                  columnDefs = defs) # DT's column index starts at 0 not 1
                   ) %>%
      formatStyle(columns = 2:maxyvals,
                  valueColumns = sc.cols, 
                  color = styleInterval(c(30), c(ltgrey, 'black')))
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
  
  dt.container.dtstyle <- function(atable, xvaralias, yvaralias) {
    sc.cols <- str_subset(colnames(atable), "_sc")
    num.disp.cols <- ncol(atable) - length(sc.cols)
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 2, xvaralias),
          th(class = 'dt-center', colspan = (num.disp.cols-1), yvaralias)
        ), # end tr
        tr(
          lapply(colnames(atable)[2:num.disp.cols], th)
        ) # end tr
      ) # end thead
    ) # end table
    ) # end withTags
  }
  
  # a container for shares with margin of error
  dt.container.ShareMOE <- function(atable, xvaralias, yvaralias) {
    exc.cols <- str_subset(colnames(atable), paste(xvaralias, "_MOE", sep = "|"))
    yval.labels <- setdiff(colnames(atable), exc.cols)
    
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 3, xvaralias),
          th(class = 'dt-center', colspan = (ncol(atable)-1), yvaralias)
        ), # end tr
        tr(
          lapply(yval.labels, function(x) th(class = 'dt-center', colspan = 2, x))
        ), # end tr
        tr(
          lapply(rep(c("Share", "Margin of Error"), (ncol(atable)-1)/2), function(x) th(style = "font-size:12px", x))
        ) # end tr
      ) # end thead
    ) # end table
    ) # end withTags
  }
  
  dt.container.ShareMOE.dtstyle <- function(atable, xvaralias, yvaralias) {
    exc.cols <- str_subset(colnames(atable), paste(xvaralias, "_MOE|_sc.*", sep = "|"))
    yval.labels <- setdiff(colnames(atable), exc.cols)
    
    sc.cols <- str_subset(colnames(atable), "_sc.*")
    num.disp.cols <- ncol(atable) - length(sc.cols)
    
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(class = 'dt-center', rowspan = 3, xvaralias),
          th(class = 'dt-center', colspan = (num.disp.cols-1), yvaralias)
        ), # end tr
        tr(
          lapply(yval.labels, function(x) th(class = 'dt-center', colspan = 2, x))
        ), # end tr
        tr(
          lapply(rep(c("Share", "Margin of Error"), (num.disp.cols-1)/2), function(x) th(style = "font-size:12px", x))
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

      new.colnames <- str_extract(colnames(dt.list[[i]])[2:length(colnames(dt.list[[i]]))], paste0("(?<=", regex, ").+")) # includes blank

      if (any(is.na(new.colnames))) { # if contains any NA columns
        nonna.new.colnames <- str_subset(new.colnames, ".")
        setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames)) # blank becomes NA
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
   return(dt.list)
  })
  
  # create separate table of shares alongside margin of errors
  xtabTableClean.ShareMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(Variable, Value)]
    dt.s <- xtabTableClean()[['share']]
    dt.m <- xtabTableClean()[['MOE']]
    dtcols <- colnames(dt.s)[2:ncol(dt.s)]
    
    cols.order <- c()
    for (acol in dtcols) {
      moe.col <- paste0(acol, "_MOE")
      cols.order <- append(cols.order, c(acol, moe.col))
    }
    
    colnames(dt.m)[2:ncol(dt.m)] <- paste0(colnames(dt.m)[2:ncol(dt.m)], "_MOE")
    dt.sm <- merge(dt.s, dt.m, by = xa)
    
    dt.sm[, var1.sort := factor(get(eval(xa)), levels = xvals$Value)]
    dt.sm <- dt.sm[order(var1.sort)][, var1.sort := NULL]
    order.colnames <- c(xa, cols.order)
    dt.sm <- dt.sm[, ..order.colnames]
  })
  
  xtabTableClean.DT.ShareMOE <- reactive({
    # dt <- xtabTableClean.ShareMOE()
    t <- copy(xtabTableClean.ShareMOE())
    
    moe.cols <- str_subset(colnames(t), "_MOE$")
    t[, (moe.cols) := lapply(.SD, function(x) round(x*100, 1)), .SDcols = moe.cols]
    t[, (moe.cols) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = moe.cols]
    
    for(j in seq_along(t)){
      set(t, i = which(t[[j]] == "+/-NA%"), j=j, value="")
    }
    return(t)
  })
  
# Crosstab Generator Visuals ----------------------------------------------

  
  xtabVisTable.ShareMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(Variable, Value)]
    dt.s <- xtabTableClean()[['share']]
    dt.m <- xtabTableClean()[['MOE']]

    msrcols <- colnames(dt.s)[!(colnames(dt.s) %in% xa)]
    dts <- melt.data.table(dt.s, id.vars = xa, measure.vars = msrcols, variable.name = "value", value.name = "result")
    dtm <- melt.data.table(dt.m, id.vars = xa, measure.vars = msrcols, variable.name = "value", value.name = "result_moe")
    dt <- merge(dts, dtm, by = c(xa, "value"))
    setnames(dt, xa, "group")
    
    if (nrow(xvals) != 0) {
      dt[, group := factor(group, levels = xvals$Value)][, group := fct_explicit_na(group, "No Response")]
      dt <- dt[order(group)]
    } else {
      dt[, group := factor(group)]
    }
    return(dt)
  })
  
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
      } else {
        t[, group := factor(group)]
      }
      visdt.list[[names(dt.list[i])]]<- t
    }
    return(visdt.list)
  }) 
  
  output$xtab_vis <- renderPlotly({
    xlabel <- varsXAlias() # first dim
    ylabel <- varsYAlias() # second dim
    dttype <- input$xtab_dtype_rbtns
    dttype.label <- names(dtype.choice.xtab[dtype.choice.xtab == dttype])

    if (dttype %in% c("sample_count", "estimate", "share", "MOE", "N_HH")) {
      dt <- xtabVisTable()[[dttype]]
    } else {
      dt <- xtabVisTable.ShareMOE()
    }

    l <- length(unique(dt$value))

    if (dttype == 'share') {
      ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "percent", xlabel, ylabel, dttype.label), p <- xtab.plot.bar(dt, "percent", xlabel, ylabel, dttype.label))
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "nominal", xlabel, ylabel, dttype.label), p <- xtab.plot.bar(dt, "nominal", xlabel, ylabel, dttype.label))
      return(p)
    } else if (dttype %in% c('share_with_MOE')) {
      ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "percent", xlabel, ylabel), p <- xtab.plot.bar.moe(dt, "percent", xlabel, ylabel))
      return(p)
    } else {
      return(NULL)
    }

  })
  
# Crosstab Generator Table Rendering --------------------------------------------
 
  
  output$xtab_tbl <- DT::renderDataTable({

    dttype <- input$xtab_dtype_rbtns
    if (dttype %in% c("sample_count", "estimate", "share", "MOE", "N_HH")) {
      # dt <- xtabTableClean()[[dttype]] #og
      dt <- xtab.join.samplecnt(xtabTableClean(), dttype, varsXAlias()) 
      sc.cols <- str_which(colnames(dt), "_sc")
      sc.idx <- sc.cols - 1
      disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
    } else {
      # dt <- xtabTableClean.DT.ShareMOE() #og
      dt <- xtab.shareMOE.join.samplecnt(xtabTableClean.DT.ShareMOE(), xtabTableClean(), dttype, varsXAlias())
      moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_MOE")
      sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
      cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
      sc.cols <- str_which(colnames(dt), "_sc.*")
      sc.idx <- sc.cols - 1
      disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
    }
    
    # sketch <- dt.container(dt, varsXAlias(), varsYAlias())
    # sketch.exp <- dt.container.ShareMOE(dt, varsXAlias(), varsYAlias())
    sketch.dtstyle <- dt.container.dtstyle(dt, varsXAlias(), varsYAlias())
    sketch.dtstyle.exp <- dt.container.ShareMOE.dtstyle(dt, varsXAlias(), varsYAlias())
    
    if (dttype == 'share') {
      # DT::datatable(dt,
      #               container = sketch,
      #               rownames = FALSE,
      #               options = list(bFilter=0
      #                              )) %>%
      #   formatPercentage(colnames(dt)[2:length(colnames(dt.data))], 1)
      xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
        formatPercentage(colnames(dt)[2:disp.col.max], 1)
    } else if (dttype == 'estimate') {
      xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
        formatRound(colnames(dt)[2:disp.col.max], 0)
    } else if (dttype == 'sample_count') {
      xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
        formatRound(colnames(dt)[2:disp.col.max], 0)
    } else if (dttype == 'share_with_MOE') {
      # moe.cols <- str_subset(colnames(dt)[2:ncol(dt)], "_MOE")
      # cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], moe.cols)
      # DT::datatable(dt,
      #               container = sketch.exp,
      #               rownames = FALSE,
      #               options = list(bFilter=0,
      #                              autoWidth = FALSE,
      #                              columnDefs = list(list(className = "dt-head-center dt-center", targets = "_all")) # DT CRAN hack
      #               )
      #               ) %>%
      #   formatPercentage(cols.fmt, 1)
      
      xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
        formatPercentage(cols.fmt, 1)
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
   
    dtypes <- dtype.choice.stab 
    selcols <- c(xa, names(dtypes))
    setnames(simtable, c(input$stab_xcol, dtypes), selcols)
    setcolorder(simtable, selcols)
    
    dt <- simtable[!(get(eval(xa)) %in% "")][, ..selcols]
  })
  
  # clean Margin of Error column and column reorder
  stabTable.DT <- reactive({
    xa <- stab.varsXAlias()
    dt <- copy(stabTable())
   
    col <- names(dtype.choice[dtype.choice %in% "MOE"])
    dt[, (col) := lapply(.SD, function(x) round(x*100, 1)), .SDcols = col]
    dt[, (col) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = col]
    new.colorder <- c(xa, names(dtype.choice[dtype.choice %in% c("share")]), col, names(dtype.choice[dtype.choice %in% c("estimate", "sample_count")]))
    setcolorder(dt,  new.colorder)
    return(dt)
  })
  
  output$stab_tbl <- DT::renderDataTable({
    dt <- stabTable.DT()

    fmt.per <- names(dtype.choice[dtype.choice %in% c('share')])
    fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])
    DT::datatable(dt,
                  options = list(bFilter=0, 
                                 pageLength = 6,
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))))
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
    } else {
      t[, value := factor(value)]
    }
    
    return(t)
  })
  
  stabVisTable.shareMOE <- reactive({
    types <- names(dtype.choice[dtype.choice %in% c('share', 'MOE')])
    dt <- stabVisTable()[type %in% types]
    t <- dcast.data.table(dt, value ~ type, value.var = "result")
    setnames(t, str_subset(colnames(t), paste(types, collapse = "|")), c("result", "result_moe"))
    return(t)
  })
  
  output$stab_vis <- renderPlotly({
    xlabel <- stab.varsXAlias() # first dim
    dttype <- input$stab_dtype_rbtns
    selection <- names(dtype.choice[dtype.choice %in% dttype])
    
    if (dttype %in% c("sample_count", "estimate", "share", "MOE", "N_HH")) {
      dt <- stabVisTable()[type %in% selection, ]
    } else {
      dt <- stabVisTable.shareMOE()
    }

    l <- length(stabXValues()$Value) 
    if(l == 0) l <- length(unique(dt$value)) # evaluate if values are not in lookup (length 0)
    
    if (dttype == 'share') {
      ifelse(l < 10, p <- stab.plot.bar(dt, "percent", xlabel), p <- stab.plot.bar2(dt, "percent", xlabel))
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      ifelse(l < 10, p <- stab.plot.bar(dt, "nominal", xlabel), p <- stab.plot.bar2(dt, "nominal", xlabel))
      return(p)
    } else if (dttype %in% c('share_with_MOE')) {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "percent", xlabel), p <- stab.plot.bar2.moe(dt, "nominal", xlabel))
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



