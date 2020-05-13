
function(input, output, session) {
  

# Functions ---------------------------------------------------------------
  
  # Column Subset Crosstab Generator
  xtab.col.subset <- function(table, colstring = col.headers) {
    cols <- c(varsXAlias(), str_subset(colnames(table), paste0("^", colstring)))
    table[, ..cols]
  }
  
  # xtab.edit.samplecnt <- function(dt, thresholdcnt) {
  #   # dt <- copy(xtabstyletable)
  #   edit.ind <- as.data.table(which(dt < thresholdcnt, arr.ind = T))
  #   edit.rows <- unique(edit.ind$row)
  #   dt[edit.rows, (colnames(dt)[2:length(colnames(dt))]) := 1]
  # }
  
  xtab.join.samplecnt <- function(xtabcleandt, dttype, varsXAlias) {
    dt.data <- xtabcleandt[[dttype]]
    dt.sort.rows <- dt.data[[varsXAlias]]
    dt.style <- copy(xtabcleandt[['sample_count']])

    # alter dt.style, update rows containing < 30
    # dt.style <- xtab.edit.samplecnt(dt.style, 30)
    
    colnames(dt.style)[2:length(colnames(dt.style))] <- paste0(colnames(dt.style)[2:length(colnames(dt.style))], "_sc")
    dt <- merge(dt.data, dt.style, by = varsXAlias)
    dt[, var1.sort := factor(get(varsXAlias), levels = dt.sort.rows)]
    dt <- dt[order(var1.sort)][, var1.sort := NULL]
  }
  
  xtab.tblMOE.join.samplecnt <- function(xtabcleantblMOEdt, xtabcleandt, dttype, varsXAlias) {
    dt.data <- xtabcleantblMOEdt
    dt.style <- copy(xtabcleandt[['sample_count']])
    dt.sort.rows <- dt.data[[varsXAlias]]
    
    idx <- 2:ncol(dt.style)
    colnames(dt.style)[idx] <- paste0(letters[1:(ncol(dt.style)-1)], "_", colnames(dt.style)[idx], "_sc")
    new.cols <- paste0(colnames(dt.style)[idx], "2")
    cols <- colnames(dt.style)[idx]
    col2order <- sort(c(cols, new.cols))
    dt.style[, (new.cols) := mapply(function(x) replicate(1, .SD[[x]]), cols, SIMPLIFY = F)]
    setcolorder(dt.style, c(varsXAlias, col2order))

    # alter dt.style, update rows containing < 30
    # dt.style <- xtab.edit.samplecnt(dt.style, 30)
    
    dt <- merge(dt.data, dt.style, by = varsXAlias)
    
    dt[, var1.sort := factor(get(varsXAlias), levels = dt.sort.rows)]
    dt <- dt[order(var1.sort)][, var1.sort := NULL]
  } 
  
  xtab.create.DT <- function(atable, moe = c(TRUE, FALSE), acontainer, indices2hide, maxyvals, sc.cols) {
    colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
    if (moe == TRUE) {
      defs <- list(list(className = "dt-head-center dt-center", targets = "_all"),# DT CRAN hack
                   list(visible = F, targets = indices2hide)) # DT's column index starts at 0 not 1
    } else {
      defs <- list(list(visible = F, targets = indices2hide)) # DT's column index starts at 0 not 1
    }

     DT::datatable(atable,
                   # caption = acaption,
                   container = acontainer,
                   rownames = FALSE,
                   options = list(bFilter=0,
                                  columnDefs = defs) # DT's column index starts at 0 not 1
                   ) %>%
      formatStyle(columns = 2:maxyvals,
                  valueColumns = sc.cols, 
                  color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey)))
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
  
  dt.container.tblMOE.dtstyle <- function(atable, xvaralias, yvaralias, tbltype = c("share", "estimate", "mean")) {
    # ifelse(tbltype == "share", tbltype <- "Share", tbltype <- "Total")
    if (tbltype == "share") {
      tbltype <- "Share"
    } else if (tbltype == "estimate") {
      tbltype <- "Total"
    } else {
      tbltype <- "Mean"
    }
    
    exc.cols <- str_subset(colnames(atable), paste(xvaralias, "_MOE|_sc.*", sep = "|"))
    yval.labels <- setdiff(colnames(atable), exc.cols)
    
    sc.cols <- str_subset(colnames(atable), "_sc.*")
    num.disp.cols <- ncol(atable) - length(sc.cols)
    
    if (tbltype == "Share" |tbltype == "Total") {
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
            lapply(rep(c(tbltype, "Margin of Error"), (num.disp.cols-1)/2), function(x) th(style = "font-size:12px", x))
          ) # end tr
        ) # end thead
      ) # end table
      ) # end withTags
    } else {
      htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', rowspan = 2, xvaralias),
            th(class = 'dt-center', colspan = (num.disp.cols-1), yvaralias)
          ), # end tr
          # tr(
          #   lapply(yval.labels, function(x) th(class = 'dt-center', colspan = 2, x))
          # ), # end tr
          tr(
            lapply(rep(c(tbltype, "Margin of Error"), (num.disp.cols-1)/2), function(x) th(style = "font-size:12px", x))
          ) # end tr
        ) # end thead
      ) # end table
      ) # end withTags
    }
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
    xvar.det <- variables.lu[variable %in% input$xtab_xcol, .(detail)]
    unique(xvar.det$detail)
  })

  output$xtab_ycol_det <- renderText({
    yvar.det <- variables.lu[variable %in% input$xtab_ycol, .(detail)]
    unique(yvar.det$detail)
  })
  
  # variable X alias list
  varsListX <- reactive({
    t <- variables.lu[category %in% input$xtab_xcat & dtype != 'fact', ]
    vars.raw <- as.list(unique(t$variable))
    vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))
  })
  
  # variable Y alias list
  varsListY <- reactive({
    t <- variables.lu[category %in% input$xtab_ycat, ]
    vars.raw <- as.list(unique(t$variable))
    vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))
  })
  
  # variable X alias
  varsXAlias <- eventReactive(input$xtab_go, {
    xvar.alias <- variables.lu[variable %in% input$xtab_xcol, .(variable_name)]
    unique(xvar.alias$variable_name)
  })
  
  # variable Y alias
  varsYAlias <- eventReactive(input$xtab_go, {
    yvar.alias <- variables.lu[variable %in% input$xtab_ycol, .(variable_name)]
    unique(yvar.alias$variable_name)
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
    dt <- values.lu[variable %in% input$xtab_xcol, ][order(value_order)] # return dt
  })
  
  xtabYValues <- eventReactive(input$xtab_go, {
    dt <- values.lu[variable %in% input$xtab_ycol, ][order(value_order)]
    v <- as.vector(dt$value_text) # return vector
  })
  
  xtabCaption <- eventReactive(input$xtab_go, {
    if (input$xtab_fltr_sea == T) {
      cap <- "Seattle results"
    } else {
      cap <- "Regional results"
    }
    return(cap)
  })
  
  output$ui_xtab_res_type_title <- renderUI({
    h4(xtabCaption())
  })

  
# Crosstab Generator data wrangling ---------------------------------------
  
  
  xtabTableType <- eventReactive(input$xtab_go, {
    select.vars <- variables.lu[variable %in% c(input$xtab_xcol, input$xtab_ycol), ]
    tables <- as.vector(unique(select.vars$table_name))
    dtypes <- as.vector(unique(select.vars$dtype))

    if('Trip' %in% tables){
      res<-'Trip'
    } else if('Person' %in% tables){
      res<-'Person'
    }else{
      res<-'Household'
    }
    

    if('fact' %in% dtypes){
      type<- 'fact'
    }
    else{
      type<-'dimension'
    }
    
    return(list(Res=res, Type=type))
    } )

  # return list of tables subsetted by value types
  xtabTable <- eventReactive(input$xtab_go, {
      table.type<- xtabTableType()$Res
      wt_field<- table_names[[table.type]]$weight_name
      
      if(input$xtab_xcol=='weighted_trip_count' || input$xtab_ycol =='weighted_trip_count'){
        # use a special weight here because trip counts are a weird case
        wt_field <-hh_day_weight_name
      }

      sql.query <- paste("SELECT seattle_home, hhid,", input$xtab_xcol,",", input$xtab_ycol, ",", wt_field, "FROM", table_names[[table.type]]$table_name)
      survey <- read.dt(sql.query, 'sqlquery')

      type <- xtabTableType()$Type
      
      if (input$xtab_fltr_sea == T) survey <- survey[seattle_home == 'Home in Seattle',]

      crosstab <-cross_tab(survey, input$xtab_xcol, input$xtab_ycol, wt_field, type)
      xvals <- xtabXValues()[, .(value_order, value_text)]
    
      crosstab <- merge(crosstab, xvals, by.x='var1', by.y='value_text')
      setorder(crosstab, value_order)
        
      setnames(crosstab, "var1", varsXAlias(), skip_absent=TRUE)
  
      xtab.crosstab <- partial(xtab.col.subset, table = crosstab)
      
      if (type == 'dimension') {
        column.headers <- col.headers
      } else if (type == 'fact') {
        column.headers <- col.headers.facts
      }
      
      dt.list <- map(as.list(column.headers), xtab.crosstab)
      names(dt.list) <- column.headers

      return(dt.list)
  })
  
  # clean xtabTable()
  xtabTableClean <- reactive({
    dt.list <- xtabTable()
    # yv <- xtabYValues()
    xa <- varsXAlias()
    # col.headers <- lapply(col.headers, function(x) paste0(x, "_")) %>% unlist
    # regex <- paste(col.headers, collapse = "|")
    
    if (xtabTableType()$Type == 'dimension') {
      yv <- xtabYValues()
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
    } else if (xtabTableType()$Type == 'fact') {
      new.colnames.fact <- c("Mean" = "mean", "Sample Count" = "sample_count", "Number of Households" = "N_HH") 
      for (i in 1:length(dt.list)) {
        # set colnames for mean, sample count, number of households field
        if (names(dt.list[i]) %in% new.colnames.fact) {
          setnames(dt.list[[i]],
                   names(dt.list[i]),
                   names(new.colnames.fact[new.colnames.fact %in% names(dt.list[i])]), skip_absent = TRUE)
        } else {
          next
        }
      }
    }

   return(dt.list)
  })
  
  create.table.joining.moe <- function(valuetable, moetable, xalias, xvalues) {
    # This is function is for Dimension related tables
    
    dtcols <- colnames(valuetable)[2:ncol(valuetable)]
    cols.order <- c()
    for (acol in dtcols) {
      moe.col <- paste0(acol, "_MOE")
      cols.order <- append(cols.order, c(acol, moe.col))
    }
    colnames(moetable)[2:ncol(moetable)] <- paste0(colnames(moetable)[2:ncol(moetable)], "_MOE")
    dt.sm <- merge(valuetable, moetable, by = xalias)
    dt.sm[, var1.sort := factor(get(eval(xalias)), levels = xvalues$value_text)]
    dt.sm <- dt.sm[order(var1.sort)][, var1.sort := NULL]
    order.colnames <- c(xalias, cols.order)
    dt.sm <- dt.sm[, ..order.colnames]
  }
  
  # create separate table of shares alongside margin of errors
  xtabTableClean.ShareMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]
    dt.s <- xtabTableClean()[['share']]
    dt.m <- xtabTableClean()[['MOE']]
    dt.sm <- create.table.joining.moe(dt.s, dt.m, xa, xvals)
  })
  
  # create separate table of estimates alongside margin of errors
  xtabTableClean.EstMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]
    dt.s <- xtabTableClean()[['estimate']]
    dt.m <- xtabTableClean()[['estMOE']]
    dt.sm <- create.table.joining.moe(dt.s, dt.m, xa, xvals)
  })
  
  # create separate table of mean (for fact related tables) alongside margin of errors
  xtabTableClean.MeanMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]

    dt.s <- xtabTableClean()[['mean']]
    dt.m <- xtabTableClean()[['MOE']]
    dt <- merge(dt.s, dt.m, by = xa)
    dt[, var1.sort := factor(get(eval(xa)), levels = xvals$value_text)]
    dt.sm <- dt[order(var1.sort)][, var1.sort := NULL]
  })
  
  xtabTableClean.DT.MeanMOE <- reactive({
    t <- copy(xtabTableClean.MeanMOE())

    t[, MOE := lapply(.SD, function(x) prettyNum(round(x, 2), big.mark = ",", preserve.width = "none")), .SDcols = 'MOE']
    t[, MOE := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = 'MOE']
  })
  
  xtabTableClean.DT.ShareMOE <- reactive({
    t <- copy(xtabTableClean.ShareMOE())
    
    moe.cols <- str_subset(colnames(t), "_MOE$")
    t[, (moe.cols) := lapply(.SD, function(x) round(x*100, 1)), .SDcols = moe.cols]
    t[, (moe.cols) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = moe.cols]
    
    for(j in seq_along(t)){
      set(t, i = which(t[[j]] == "+/-NA%"), j=j, value="")
    }
    return(t)
  })
  
  xtabTableClean.DT.EstMOE <- reactive({
    t <- copy(xtabTableClean.EstMOE())

    moe.cols <- str_subset(colnames(t), "_MOE$")
    t[, (moe.cols) := lapply(.SD, function(x) prettyNum(round(x, 0), big.mark = ",", preserve.width = "none")), .SDcols = moe.cols]
    t[, (moe.cols) := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = moe.cols]
    
    for(j in seq_along(t)){
      set(t, i = which(t[[j]] == "+/-NA"), j=j, value="")
    }
    return(t)
  })
  
# Crosstab Generator Visuals ----------------------------------------------
  
  
  create.table.vistable.moe <- function(valuetable, moetable, xalias, xvalues) {
    msrcols <- colnames(valuetable)[!(colnames(valuetable) %in% xalias)]
    dts <- melt.data.table(valuetable, id.vars = xalias, measure.vars = msrcols, variable.name = "value", value.name = "result")
    
    if (xtabTableType()$Type == 'dimension') {
      dtm <- melt.data.table(moetable, id.vars = xalias, measure.vars = msrcols, variable.name = "value", value.name = "result_moe")
      dt <- merge(dts, dtm, by = c(xalias, "value"))
      setnames(dt, xalias, "group")
    } else {
      dt <- merge(dts, moetable, by = c(xalias))
      setnames(dt, c(xalias, 'MOE'), c("group", "result_moe"))
    }
    
    if (nrow(xvalues) != 0) {
      dt[, group := factor(group, levels = xvalues$value_text)][, group := fct_explicit_na(group, "No Response")]
      dt <- dt[order(group)]
    } else {
      dt[, group := factor(group)]
    }
    return(dt)
  }
  
  xtabVisTable.EstMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]
    dt.s <- xtabTableClean()[['estimate']]
    dt.m <- xtabTableClean()[['estMOE']]
    dt <- create.table.vistable.moe(dt.s, dt.m, xa, xvals)
  })
  
  xtabVisTable.ShareMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]
    dt.s <- xtabTableClean()[['share']]
    dt.m <- xtabTableClean()[['MOE']]
    dt <- create.table.vistable.moe(dt.s, dt.m, xa, xvals)
  })
  
  xtabVisTable.meanMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]
    dt.s <- xtabTableClean()[['mean']]
    dt.m <- xtabTableClean()[['MOE']]
    dt <- create.table.vistable.moe(dt.s, dt.m, xa, xvals)
  })

  xtabVisTable <- reactive({
    dt.list <- xtabTableClean()
    xvals <- xtabXValues()[, .(value_order, value_text)]

    visdt.list <- NULL
    for (i in 1:length(dt.list)) {
      idcol <- varsXAlias()
      msrcols <- colnames(dt.list[[i]])[!(colnames(dt.list[[i]]) %in% idcol)]
      varcol <- "value"
      t <- melt.data.table(dt.list[[i]], id.vars = idcol, measure.vars = msrcols, variable.name = "value", value.name = "result")
      t[, type := names(dt.list[i])]
      setnames(t, idcol, "group")
      if (nrow(xvals) != 0) {
        t[, group := factor(group, levels = xvals$value_text)][, group := fct_explicit_na(group, "No Response")]
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
    geog.caption <- xtabCaption()

    if (xtabTableType()$Type == 'dimension') {
      if (is.null(input$xtab_dtype_rbtns)) return(NULL)
      dttype <- input$xtab_dtype_rbtns
      dttype.label <- names(dtype.choice.xtab[dtype.choice.xtab == dttype])
  
      if (dttype %in% c("sample_count", "estimate", "share", "MOE", "N_HH")) {
        dt <- xtabVisTable()[[dttype]]
      } else {
        if (dttype == "share_with_MOE") dt <- xtabVisTable.ShareMOE()
        if (dttype == "estimate_with_MOE") dt <- xtabVisTable.EstMOE()
      }
  
      l <- length(unique(dt$value))
  
      if (dttype == 'share') {
        ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "percent", xlabel, ylabel, dttype.label, geog.caption), p <- xtab.plot.bar(dt, "percent", xlabel, ylabel, dttype.label, geog.caption))
        return(p)
      } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
        ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption), p <- xtab.plot.bar(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption))
        return(p)
      } else if (dttype %in% c('share_with_MOE')) {
        ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "percent", xlabel, ylabel, geog.caption), p <- xtab.plot.bar.moe(dt, "percent", xlabel, ylabel, geog.caption))
        return(p)
      } else if (dttype %in% c('estimate_with_MOE')) {
        ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "nominal", xlabel, ylabel, geog.caption), p <- xtab.plot.bar.moe(dt, "nominal", xlabel, ylabel, geog.caption))
        return(p)
      } else {
        return(NULL)
      }
    } else { # if xtabTableType()$Type == 'fact'
      if (is.null(input$xtab_dtype_rbtns_fact)) return(NULL)
      dttype <- input$xtab_dtype_rbtns_fact
      dttype.label <- names(dtype.choice.xtab.facts[dtype.choice.xtab.facts == dttype])
      
      if (dttype %in% c("sample_count", "mean", "MOE", "N_HH")) {
        dt <- xtabVisTable()[[dttype]]
      } else { #if (dttype == "mean_with_MOE")
        dt <- xtabVisTable.meanMOE()
      }
      
      if (dttype %in% c("sample_count", "mean", "N_HH")) {
        p <- xtab.plot.bar.fact(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption)
        return(p)
      } else { # mean_with_MOE
        p <- xtab.plot.bar.fact.moe(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption)
        return(p)
      }
    } # end of if/else dim or fact  
  })
  
# Crosstab Generator Table Rendering --------------------------------------------
  xtabDtypeBtns <- eventReactive(input$xtab_go, {
    # This reactive will change the display of 'Summary Types' radio buttons
    # depending on whether it is a dimension or fact related table
    
    if (xtabTableType()$Type == 'dimension') {
     btns <-  wellPanel(
        radioButtons("xtab_dtype_rbtns",
                     label = strong("Summary Types"),
                     choices = dtype.choice.xtab
                     ),
        div(p("Shares are based on rowwise totals."), style = 'font-size: 85%')
      ) # end wellPanel
    } else if (xtabTableType()$Type == 'fact') {
      btns <- wellPanel(
        radioButtons("xtab_dtype_rbtns_fact",
                     label = strong("Summary Types"),
                     choices = dtype.choice.xtab.facts
        )
      ) # end wellPanel
    }
    
    return(btns)
  })
  
  output$ui_xtab_dtype_rbtns <- renderUI(
    xtabDtypeBtns()
  )
  
  output$xtab_tbl <- DT::renderDataTable({
    if ((xtabTableType()$Type == 'dimension')) {
      if (is.null(input$xtab_dtype_rbtns)) return(NULL)
      dttype <- input$xtab_dtype_rbtns
      
      if (dttype %in% c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")) {
        # This if/else chunk joins sample count to the table of choice with the purpose
        # of greying out values where sample counts are low.
        dt <- xtab.join.samplecnt(xtabTableClean(), dttype, varsXAlias()) 
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      } else {
        if (dttype %in% c("share_with_MOE")) {
          dt <- xtab.tblMOE.join.samplecnt(xtabTableClean.DT.ShareMOE(), xtabTableClean(), dttype, varsXAlias())
        } else if (dttype %in% c("estimate_with_MOE")) {
          dt <- xtab.tblMOE.join.samplecnt(xtabTableClean.DT.EstMOE(), xtabTableClean(), dttype, varsXAlias())
        }

        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      }
      
      sketch.dtstyle <- dt.container.dtstyle(dt, varsXAlias(), varsYAlias())
      
      if (dttype == 'share') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatPercentage(colnames(dt)[2:disp.col.max], 1)
      } else if (dttype == 'estimate') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == 'sample_count') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == 'share_with_MOE') {
        sketch.dtstyle.exp <- dt.container.tblMOE.dtstyle(dt, varsXAlias(), varsYAlias(), "share")
        xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
          formatPercentage(cols.fmt, 1)
      } else if (dttype == 'estimate_with_MOE') {
        sketch.dtstyle.exp <- dt.container.tblMOE.dtstyle(dt, varsXAlias(), varsYAlias(), "estimate")
        xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(cols.fmt, 0)
      }
    } else if ((xtabTableType()$Type == 'fact')) {
      if (is.null(input$xtab_dtype_rbtns_fact)) return(NULL)
      dttype <- input$xtab_dtype_rbtns_fact
      
      if (dttype %in% c("mean", "sample_count")) {
        # This if/else chunk joins sample count to the table of choice with the purpose
        # of greying out values where sample counts are low.
        
        dt <- xtab.join.samplecnt(xtabTableClean(), dttype, varsXAlias()) 
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      }  else {
        if (dttype %in% c("mean_with_MOE")) {
          
          dt <- xtab.tblMOE.join.samplecnt(xtabTableClean.DT.MeanMOE(), xtabTableClean(), dttype, varsXAlias())
        }
        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      } 
      
      sketch.dtstyle <- dt.container.dtstyle(dt, varsXAlias(), varsYAlias()) 
    
      if (dttype == 'mean') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 2)
      } else if (dttype == 'sample_count') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == 'mean_with_MOE') {
        sketch.dtstyle.exp <- dt.container.tblMOE.dtstyle(dt, varsXAlias(), varsYAlias(), "mean")
        xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(cols.fmt, 2)
      }
    }
    
  })
  
  output$ui_xtab_tbl <- renderUI({
    div(DT::dataTableOutput('xtab_tbl'), style = 'font-size: 95%; width: 85%', class = 'visual-display')
  })

  output$ui_xtab_vis <- renderUI({
    # if (xtabTableType()$Type == 'dimension') {
    #   plotlyOutput("xtab_vis", width = "85%")
    # } else {
    #   div(p('Results not available. This functionality is in progress.'),
    #       style = 'display: flex; justify-content: center; align-items: center; margin-top: 5em;')
    # }
    div(plotlyOutput("xtab_vis", width = "85%"), class = 'visual-display')
      
  })
  

# Crosstab Generator Download ---------------------------------------------

  # Enable/Disable download button
  v <- reactiveValues(xtabxcol = NULL,
                      xtabycol = NULL,
                      xtabgo = 0,
                      xtabfltrsea = F)
  
  observeEvent(input$xtab_go, {
    v$xtabxcol <- input$xtab_xcol
    v$xtabycol <- input$xtab_ycol
    v$xtabgo <- v$xtabgo + 1
    v$xtabfltrsea <- input$xtab_fltr_sea
  })
  
  observe({
    if (v$xtabgo == 0 || (v$xtabycol != input$xtab_ycol) || (v$xtabxcol != input$xtab_xcol) || (v$xtabfltrsea != input$xtab_fltr_sea)) {
      disable("xtab_download")
    } else if (v$xtabgo > 0) {
      enable("xtab_download")  
      }
  })
  
  xtabDownloadOutput <- reactive({
    dtlist <- copy(xtabTableClean())
    t <- dtlist[['sample_count']]
    data.type <- xtabTableType()$Type
    geog <- xtabCaption()

    if (data.type == 'dimension') {
      tsm <- copy(xtabTableClean.DT.ShareMOE())
      tem <- copy(xtabTableClean.DT.EstMOE())
      
      # Format tsm, every other column as string starting at index 2
      nums <- seq(1, length(colnames(tsm)))
      evens <- unlist(lapply(nums, function(x) x %%2 ==0))
      ind <- nums[evens]
      
      cols.to.str <- colnames(tsm)[ind]
      tsm[, (cols.to.str) := lapply(.SD, function(x) paste0(as.character(round(x*100, 1)), '%')), .SDcols = cols.to.str]
      
      # Format tem, every other column as string starting at index 2
      cols.to.prettynum <- colnames(tem)[ind]
      tem[, (cols.to.prettynum) := lapply(.SD, function(x) prettyNum(round(x), big.mark = ",")), .SDcols = cols.to.prettynum]
      
      for(j in seq_along(tsm)){
        set(tsm, i = which(tsm[[j]] == "NA%"), j=j, value="")
      }
      
      for(j in seq_along(tem)){
        set(tem, i = which(tem[[j]] == "NA"), j=j, value="")
      }
      
      tsm[, `Result Type` := geog]
      tem[, `Result Type` := geog]
      t[, `Result Type` := geog]
      
      tbllist <- list("About" = readme.dt,
                      "Share with Margin of Error" = tsm,
                      "Total with Margin of Error" = tem,
                      "Sample Count" = t)
    } else if (data.type == 'fact') {
      # join mean/MOE with sample count
      tmm <- copy(xtabTableClean.DT.MeanMOE())
      tjoin <- tmm[t, on = varsXAlias()]
      tj <- tjoin[, Mean := lapply(.SD, function(x) round(x, 2)), .SDcols = 'Mean'
                  ][, `Sample Count`:= lapply(.SD, function(x) prettyNum(x, big.mark = ",")), .SDcols = "Sample Count"
                    ][, `Result Type` := geog]
      setnames(tj, "MOE", "Margin of Error (Mean)")
      tbllist <- list("About" = readme.dt,
                      "Mean with Margin of Error" = tj)
    }
    return(tbllist)
  })
  
  output$xtab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_19_", varsXAlias(), "_by_", varsYAlias(), "_", xtabCaption(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(xtabDownloadOutput(), file)
    }
  )
  
# Simple Table ------------------------------------------------------------
  
  # show/hide vars definition
  observe({
    onclick("stabXtoggleAdvanced",
            toggle(id = "stabXAdvanced", anim = TRUE))  
  })
  
  output$stab_xcol_det <- renderText({
    xvar.det <- variables.lu[variable %in% input$stab_xcol, .(detail)]
    unique(xvar.det$detail)
  })
  
  # variable X alias
  stab.varsXAlias <- eventReactive(input$stab_go, {
    xvar.alias <- variables.lu[variable %in% input$stab_xcol, .(variable_name)]
    unique(xvar.alias$variable_name)
  })
  
  # variable X alias list
  stab.varsListX <- reactive({
    t <- variables.lu[category %in% input$stab_xcat, ]
    vars.raw <- as.list(unique(t$variable))
    vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))
  })
  
  output$ui_stab_xcol <- renderUI({
    selectInput('stab_xcol',
                'Variable', 
                stab.varsListX())
  })
  
  stabXValues <- eventReactive(input$stab_go, {
    dt <- values.lu[variable %in% input$stab_xcol, ][order(value_order)] # return dt
  })
  
  stabCaption <- eventReactive(input$stab_go, {
    if (input$stab_fltr_sea == T) {
      cap <- "Seattle results"
    } else {
      cap <- "Regional results"
    }
    return(cap)
  })
  
  output$ui_stab_res_type_title <- renderUI({
    h4(stabCaption())
  })

# Simple Table Data Wrangling ---------------------------------------------

  
  stabTableType <- eventReactive(input$stab_go, {
    select.vars <- variables.lu[variable %in% c(input$stab_xcol), ]
    tables <- unique(select.vars$table_name) 
    dtypes <- as.vector(unique(select.vars$dtype)) 
    
    if('Trip' %in% tables){
      res<-'Trip'
    } else if('Person' %in% tables){
      res<-'Person'
    }else{
      res<-'Household'
    }
    
    if('fact' %in% dtypes){
      type<- 'fact'
    }
    else{
      type<-'dimension'
    }
    
    return(list(Res=res, Type=type))
  } )

  # return list of tables subsetted by value types
  stabTable <- eventReactive(input$stab_go, {
    table.type <- stabTableType()$Res
    wt_field<- table_names[[table.type]]$weight_name

    if(input$stab_xcol=='weighted_trip_count' ){
      # use a special weight here because trip counts are a weird case
      wt_field <-hh_day_weight_name
    }
    
    sql.query <- paste("SELECT seattle_home, hhid,", input$stab_xcol,",", wt_field, "FROM" , table_names[[table.type]]$table_name)
    survey <- read.dt(sql.query, 'sqlquery')
    type <- stabTableType()$Type
   
    if (input$stab_fltr_sea == T) survey <- survey[seattle_home == 'Home in Seattle',]
    
    xa <- stab.varsXAlias()
    
    simtable <- simple_table(survey, input$stab_xcol, wt_field, type)
  
    xvals <- stabXValues()[, .(value_order, value_text)][]
    
    # check input type and xvals. sometimes xvals doesn't exist for some variables
    if((typeof(input$stab_xcol) == 'character') & (nrow(xvals) > 0)){ 
        simtable <- merge(simtable, xvals, by.x=input$stab_xcol, by.y='value_text')
        setorder(simtable, value_order)
    }
    
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
    col2 <- names(dtype.choice[dtype.choice %in% "estMOE"])
    dt[, (col) := lapply(.SD, function(x) round(x*100, 1)), .SDcols = col
       ][, (col2) := lapply(.SD, function(x) prettyNum(round(x, 0), big.mark = ",", preserve.width = "none")), .SDcols = col2]
    dt[, (col) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")), .SDcols = col
       ][, (col2) := lapply(.SD, function(x) paste0("+/-", as.character(x))), .SDcols = col2]
    new.colorder <- c(xa, 
                      names(dtype.choice[dtype.choice %in% c("share")]), 
                      col, 
                      names(dtype.choice[dtype.choice %in% c("estimate")]), 
                      col2, 
                      names(dtype.choice[dtype.choice %in% c("sample_count")]))
    setcolorder(dt,  new.colorder)
    return(dt)
  })
  
  output$stab_tbl <- DT::renderDataTable({
    colors <- list(ltgrey = '#bdbdc3', dkgrey = '#343439')
    dt <- stabTable.DT()

    fmt.per <- names(dtype.choice[dtype.choice %in% c('share')])
    fmt.num <- names(dtype.choice[dtype.choice %in% c('estimate', 'sample_count')])
    DT::datatable(dt,
                  options = list(bFilter=0, 
                                 # pageLength = 10,
                                 autoWidth = FALSE,
                                 columnDefs = list(list(className = "dt-center", width = '100px', targets = c(2:ncol(dt))))
                                 )
                  ) %>%
      formatPercentage(fmt.per, 1) %>%
      formatRound(fmt.num, 0) %>%
      formatStyle(columns = 2:ncol(dt),
                  valueColumns = ncol(dt), 
                  color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey)))
  })

# Simple Table Visuals -----------------------------------------------------

  
  stabVisTable <- reactive({
    dt <- stabTable()
    idvar <- stab.varsXAlias()
    # xvals <- stabXValues()[, .(ValueOrder, ValueText)]
    xvals <- stabXValues()[, .(value_order, value_text)]

    cols <- names(dtype.choice[dtype.choice %in% c("sample_count")])
    dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    msr.vars <- names(dtype.choice[names(dtype.choice) %in% colnames(dt)])
    t <- melt.data.table(dt, id.vars = idvar, measure.vars = msr.vars, variable.name = "type", value.name = "result")
    setnames(t, idvar, "value")

    if (nrow(xvals) != 0) {
      # t[, value := factor(value, levels = xvals$ValueText)][, value := fct_explicit_na(value, "No Response")]
      t[, value := factor(value, levels = xvals$value_text)][, value := fct_explicit_na(value, "No Response")]
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
  
  stabVisTable.estMOE <- reactive({
    types <- names(dtype.choice[dtype.choice %in% c('estimate', 'estMOE')])
    dt <- stabVisTable()[type %in% types]
    t <- dcast.data.table(dt, value ~ type, value.var = "result")
    setnames(t, str_subset(colnames(t), paste(types, collapse = "|")), c("result", "result_moe"))
    return(t)
  })
  
  output$stab_vis <- renderPlotly({
    xlabel <- stab.varsXAlias() # first dim
    dttype <- input$stab_dtype_rbtns
    selection <- names(dtype.choice[dtype.choice %in% dttype])
    geog.caption <- stabCaption()
    
    if (dttype %in% col.headers) {
      dt <- stabVisTable()[type %in% selection, ]
    } else {
      if (dttype == "share_with_MOE") dt <- stabVisTable.shareMOE()
      if (dttype == "estimate_with_MOE") dt <- stabVisTable.estMOE() 
    }

    l <- length(stabXValues()$ValueText) 
    if(l == 0) l <- length(unique(dt$value)) # evaluate if values are not in lookup (length 0)
    
    if (dttype == 'share') {
      ifelse(l < 10, p <- stab.plot.bar(dt, "percent", xlabel, geog.caption), p <- stab.plot.bar2(dt, "percent", xlabel, geog.caption))
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      ifelse(l < 10, p <- stab.plot.bar(dt, "nominal", xlabel, geog.caption), p <- stab.plot.bar2(dt, "nominal", xlabel, geog.caption))
      return(p)
    } else if (dttype == 'share_with_MOE') {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "percent", xlabel, geog.caption), p <- stab.plot.bar2.moe(dt, "percent", xlabel, geog.caption))
      return(p)
    } else if (dttype == 'estimate_with_MOE') {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "nominal", xlabel, geog.caption),  p <- stab.plot.bar2.moe(dt, "nominal", xlabel, geog.caption))
      return(p)
    } else {
      return(NULL)
    }
  })
  
  output$ui_stab_tbl <- renderUI({
   # if (stabTableType()$Type == 'dimension') {
      div(DT::dataTableOutput('stab_tbl'), style = 'font-size: 95%; width: 85%')
   # } #else {
   #   #div(p('Tabular results not available. This functionality is in progress.'),
      #    style = 'display: flex; justify-content: center; align-items: center; margin-top: 5em;')
  #  }
    
  })

# Simple Table Map --------------------------------------------------------

  
  # stabToMap <- eventReactive(input$stab_go, {
  #   # TEST!!!!!!!!!!!!!!!!!!!!!!
  #   ifelse(str_detect(input$stab_xcol, "puma10$"), TRUE, FALSE)
  # })
  
  # map.colorBins <- function(diffcolumn){
  #   rng <- range(diffcolumn)
  #   if (rng[1] < 0 & rng[2] > 0){
  #     diff.range <- "both"
  #     bins.from.positive <- abs(rng[2]) > abs(rng[1])
  #   } else if (rng[1] >=0 & rng[2] > 0){
  #     diff.range <- "pos"
  #   } else if (rng[1] < 0 & rng[2] < 0){
  #     diff.range <- "neg"
  #   } else {
  #     diff.range <- "none"
  #   }
  #   max.bin <- max(abs(rng))
  #   round.to <- 10^floor(log10(max.bin))
  #   # round maximum to the nearest 100 or 1000 or whatever is appropriate (determined by the log10)
  #   max.bin <- ceiling(max.bin/round.to)*round.to
  #   absbreaks <- (sqrt(max.bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2 # breaks on sqrt scale
  #   
  #   if (diff.range == "both"){
  #     color <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#ffffff", "#f7f7f7",
  #                "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
  #     bin <- c(-rev(absbreaks), absbreaks)
  #   } else if (diff.range == "pos"){
  #     color <- "Reds"
  #     bin <- c(0, absbreaks)
  #   } else if (diff.range == "neg"){
  #     color <- "Blues"
  #     bin <- c(-rev(absbreaks), 0)
  #   } else if (diff.range == "none"){
  #     color <- "transparent"
  #     bin <- c(0, 1)
  #   }
  #   return(list(color=color, bin=bin))
  # }
  
  # output$stab_map <- renderLeaflet({
  #   # TEST!!!!!!!!!!!!!!!!!!!!!!!!
  #   if (stabToMap()) {
  #     xlabel <- stab.varsXAlias() # first dim
  #     dttype <- input$stab_dtype_rbtns
  #     selection <- names(dtype.choice[dtype.choice %in% dttype])
  #     geog.caption <- stabCaption()
  # 
  #     if (dttype %in% col.headers) {
  #       dt <- stabVisTable()[type %in% selection, ]
  #     } else {
  #       if (dttype == "share_with_MOE") dt <- stabVisTable.shareMOE()
  #       if (dttype == "estimate_with_MOE") dt <- stabVisTable.estMOE() 
  #     }
  #     
  #     # join data to shapefile (remove extra cols)
  #     shp <- sp::merge(puma.shape, dt, by.x = "PUMACE10", by.y = "value")
  #     
  #     # put into leaflet
  #     colorBinResult <- map.colorBins(shp$result)
  #     pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin)
  #     
  #     m <- leaflet(data = shp) %>%
  #       addProviderTiles("CartoDB.Positron") %>%
  #       addPolygons(fillColor = ~pal(result),
  #                   fillOpacity = 0.7,
  #                   stroke = T,
  #                   color = "#8a8a95",
  #                   weight = 2) %>%
  #       addLegend("topright",
  #                 pal = pal,
  #                 values = ~result,
  #                 title = paste0(geog.caption, ": <br>", selection, " of ", xlabel),
  #                 opacity = 1)
  #   } else {
  #     m <- NULL
  #   }
  # 
  #   return(m)
  # })
  
  output$ui_stab_vis <- renderUI({
    # ORIGINAL!!!!!!!!!!!!!!!!!!!!!!
    plotlyOutput("stab_vis", width = "85%")
    
    # if (stabToMap()) {
    #   # TEST!!!!!!!!!!!!!!!!!!!!!!!!
    #   tabsetPanel(type = "tabs",
    #               tabPanel("Chart", plotlyOutput("stab_vis", width = "85%")), # end tabPanel
    #               tabPanel("Map", 
    #                        leafletOutput("stab_map")#,
    #                        # div(
    #                        # 
    #                        # style = 'margin-top: 2rem;'
    #                        # )
    #                        )
    #               ) # end tabsetPanel
    # } else {
    #   plotlyOutput("stab_vis", width = "85%")
    # }
  })


# Simple Table Download ---------------------------------------------------

  # Enable/Disable Download button
  vs <- reactiveValues(stabxcol = NULL,
                       stabgo = 0,
                       stabfltrsea = F)
  
  observeEvent(input$stab_go, {
    vs$stabxcol <- input$stab_xcol
    vs$stabgo <- vs$stabgo + 1
    vs$stabfltrsea <- input$stab_fltr_sea
  })
  
  observe({
    if (vs$stabgo == 0 || (vs$stabxcol != input$stab_xcol) || (vs$stabfltrsea != input$stab_fltr_sea)) {
      disable("stab_download")
    } else if (vs$stabgo > 0) {
      enable("stab_download")  
    }
  })
  
  stabDownloadOutput <- reactive({
    t <- copy(stabTable())
    geog <- stabCaption()
    
    cols.fmt.per <- str_subset(colnames(t), "Share")
    cols.fmt.nom <- str_subset(colnames(t), "Total")
    cols.fmt.moe <- str_subset(colnames(t), "Margin of Error")
    t[, (cols.fmt.per) := lapply(.SD, function(x) paste0(as.character(round(x*100, 1)), '%')), .SDcols = cols.fmt.per
      ][, (cols.fmt.nom) := lapply(.SD, function(x) prettyNum(round(x), big.mark = ",")), .SDcols = cols.fmt.nom
        ][, (cols.fmt.moe) := lapply(.SD, function(x) paste0('+/-', x)), .SDcols = cols.fmt.moe
          ][, `Result Type` := geog]
    tlist <- list("About" = readme.dt, "Simple Table" = t)
    return(tlist)
    })
  
  output$stab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_19_", stab.varsXAlias(),"_", stabCaption(), ".xlsx")
    },
    content = function(file) {
      # write.xlsx(stabTable(), file)
      write.xlsx(stabDownloadOutput(), file)
    }
  )
  
}



