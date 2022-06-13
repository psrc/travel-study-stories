
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
  
  dt.container.tblMOE.dtstyle <- function(atable, xvaralias, yvaralias, tbltype = c("share", "estimate", "median")) {
    # ifelse(tbltype == "share", tbltype <- "Share", tbltype <- "Total")
    if (tbltype == "share") {
      tbltype <- "Share"
    } else if (tbltype == "estimate") {
      tbltype <- "Total"
    } else {
      tbltype <- "median"
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
  
  xtab.variable.tbl <-eventReactive(input$xtab_dataset,{
    # filter variables table by survey year
    if(input$xtab_dataset=='2017/2019'){
      survey_year_name='2019'
    }
    else{
      survey_year_name=input$xtab_dataset
    }
    variables.lu[survey_year == survey_year_name, ]
  })

  
  # show/hide vars definition
  observe({
    onclick("xtabXtoggleAdvanced",
            toggle(id = "xtabXAdvanced", anim = TRUE))
    onclick("xtabYtoggleAdvanced",
            toggle(id = "xtabYAdvanced", anim = TRUE))

  })

  output$xtab_xcol_det <- renderText({
    xvar.det <- xtab.variable.tbl()[variable %in% input$xtab_xcol, .(detail)]
    unique(xvar.det$detail)
  })

  output$xtab_ycol_det <- renderText({
    yvar.det <- xtab.variable.tbl()[variable %in% input$xtab_ycol, .(detail)]
    unique(yvar.det$detail)
  })

# Update X and Y Categories -----------------------------------------------

  
  # update x and y cateogries
  observeEvent(input$xtab_xcat, {
    move.var <- 'Reason for leaving previous residence'
    hh.var <- 'Household'
    move.choices <- c(hh.var, move.var)
    

    x <- input$xtab_xcat
    y <- input$xtab_ycat
    
    if (x == move.var) {
      # filter to two options
      y.choices <- move.choices
    } else if ((!(x %in% move.choices) & (y == hh.var))) {
      # exclude 'Reason...'
      y.choices <- vars.cat[!(vars.cat %in% move.var)]
    } else if (x == hh.var) {
      # default options
      y.choices <- vars.cat[!(vars.cat %in% "None")]
    } else if (x != move.var & y != move.var) {
      # exclude 'Reason...'
      y.choices <- vars.cat[!(vars.cat %in% move.var)]
    } else {
      # default options
      y.choices <- vars.cat[!(vars.cat %in% "None")]
    }
    
    updateSelectInput(session, "xtab_ycat",
                      label = "Category",
                      selected = y,
                      choices = y.choices)
    
  })
  
  observeEvent(input$xtab_ycat, ignoreInit = TRUE, {
    move.var <- 'Reason for leaving previous residence'
    hh.var <- 'Household'
    move.choices <- c(hh.var, move.var)
    
    x <- input$xtab_xcat
    y <- input$xtab_ycat
    
    if (y == move.var) {
      # filter to two options
      x.choices <- move.choices
    }  else if (!(y %in% move.choices)) {
      # exclude only 'Reason...'
      x.choices <- vars.cat[!(vars.cat %in% move.var)]
    } else if (y == hh.var) {
      # default options
      x.choices <- vars.cat[!(vars.cat %in% "None")]
    } else {
      # default options
      x.choices <- vars.cat[!(vars.cat %in% "None")]
    }
    
    updateSelectInput(session, "xtab_xcat",
                      label = "Category",
                      selected = x,
                      choices = x.choices)
  })
  
  # variable X alias list
  varsListX <- reactive({
    v <- xtab.variable.tbl()
    t <- v[category %in% input$xtab_xcat & dtype != 'fact', ]
    vars.raw <- as.list(unique(t$variable))
    vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))
  })
  
  # variable Y alias list
  varsListY <- reactive({
    v <- xtab.variable.tbl()
    t <- v[category %in% input$xtab_ycat, ]
    vars.raw <- as.list(unique(t$variable))
    vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))
  })
  
  # variable X alias
  varsXAlias <- eventReactive(input$xtab_go, {
    v <- xtab.variable.tbl()
    xvar.alias <- v[variable %in% input$xtab_xcol, .(variable_name)]
    unique(xvar.alias$variable_name)
  })
  
  # variable Y alias
  varsYAlias <- eventReactive(input$xtab_go, {
    v <- xtab.variable.tbl()
    yvar.alias <- v[variable %in% input$xtab_ycol, .(variable_name)]
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
  
  xtab.values.tbl <-eventReactive(input$xtab_dataset,{
    if(input$xtab_dataset=='2017/2019'){
      survey_year_name='2019'
    }
    else{
      survey_year_name=input$xtab_dataset
    }
    values.lu[survey_year == survey_year_name,]
  })
  
  xtabXValues <- eventReactive(input$xtab_go, {
    # survey_year?
    dt <-  xtab.values.tbl()[variable %in% input$xtab_xcol, ][order(value_order)] # return dt
  })
  
  xtabYValues <- eventReactive(input$xtab_go, {
    dt <- xtab.values.tbl()[variable %in% input$xtab_ycol, ][order(value_order)]
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
    v <- xtab.variable.tbl()
    select.vars <- v[variable %in% c(input$xtab_xcol, input$xtab_ycol), ]
    select.tables<-select.vars$table_name
    select.wts<-select.vars$weight_name
    select.priority<-select.vars$weight_priority
    weight_name<- select.wts[which.min(select.priority)]
    dtypes <- as.vector(unique(select.vars$dtype))
    
    if('Trip' %in% select.tables){
      res<-table_names[['Trip']]
    } else if('Person' %in% select.tables){
      res<- table_names[['Person']]
    }else{
      res<-table_names[['Household']]
    }
    
    
    if('fact' %in% dtypes){
      type<- 'fact'
    }
    else{
      type<-'dimension'
    }
    
    return(list(WeightName=weight_name, Type=type, Table_Name=res))
})


  # return list of tables subsetted by value types
  xtabTable <- eventReactive(input$xtab_go, {

    tbl_name <- xtabTableType()$Table_Name
    type <- xtabTableType()$Type
    data
    if (input$xtab_dataset == '2017/2019')
    {
      survey_yr = "2017_2019"
    }
    else{
      survey_yr = input$xtab_dataset
    }

    data_for_xtab <-
      get_hhts(
        survey = survey_yr,
        level = tbl_name,
        vars = c("seattle_home", input$xtab_xcol, input$xtab_ycol)
      ) %>% setDT()
    
    if (input$xtab_fltr_sea == T) {
      data_for_xtab[seattle_home == "Home in Seattle"]
    }
    
    if (type=='fact') {

      crosstab <-
        hhts_median(
          data_for_xtab,
          input$xtab_ycol,
          group_vars = input$xtab_xcol,
          incl_na = FALSE
        ) %>%  rename('median' = ends_with('median'))%>%  rename('MOE'=ends_with('MOE')) %>%
               rename(sample_count= sample_size)

      crosstab <- crosstab%>% select(input$xtab_xcol, "median", "MOE", 'sample_count')
       
      
    }
    else{

      crosstab <-
        hhts_count(
          data_for_xtab,
          group_vars = c(input$xtab_xcol, input$xtab_ycol),
          incl_na = FALSE)
  
      setnames(crosstab, old=c('count', 'count_moe', 'share', 'share_moe', 'sample_size'), new=c("estimate", "estMOE", "share", "MOE", 'sample_count'))
        
   
      crosstab <- crosstab%>% select(input$xtab_xcol, input$xtab_ycol, "estimate", "estMOE", "share", "MOE", 'sample_count')%>%
        pivot_wider(names_from=input$xtab_ycol, values_from=c("estimate", "estMOE", "share", "MOE", 'sample_count'))%>% setDT()

    }
   
    xvals <- xtabXValues()[, .(value_order, value_text)]
    

    crosstab <-
      merge(crosstab, xvals, by.x = input$xtab_xcol, by.y = 'value_text')
    setorder(crosstab, value_order)

    setnames(crosstab, input$xtab_xcol, varsXAlias(), skip_absent = TRUE)

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
      new.colnames.fact <- c("Median" = "median", "Sample Count" = "sample_count") 
      for (i in 1:length(dt.list)) {
        # set colnames for median, sample count
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
  
  # create separate table of median (for fact related tables) alongside margin of errors
  xtabTableClean.medianMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]

    dt.s <- xtabTableClean()[['median']]
    dt.m <- xtabTableClean()[['MOE']]
    dt <- merge(dt.s, dt.m, by = xa)
    dt[, var1.sort := factor(get(eval(xa)), levels = xvals$value_text)]
    dt.sm <- dt[order(var1.sort)][, var1.sort := NULL]
  })
  
  xtabTableClean.DT.medianMOE <- reactive({
    t <- copy(xtabTableClean.medianMOE())

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
  
  xtabVisTable.medianMOE <- reactive({
    xa <- varsXAlias()
    xvals <- xtabXValues()[, .(value_order, value_text)]
    dt.s <- xtabTableClean()[['median']]
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
    
    survey_year_name <- input$xtab_dataset

    source.string <- paste(survey_year_name, "Household Travel Survey")

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
        ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "percent", xlabel, ylabel, dttype.label, geog.caption, source.string), p <- xtab.plot.bar(dt, "percent", xlabel, ylabel, dttype.label, geog.caption, source.string))
        return(p)
      } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
        ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption, source.string), p <- xtab.plot.bar(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption, source.string))
        return(p)
      } else if (dttype %in% c('share_with_MOE')) {
        ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "percent", xlabel, ylabel, geog.caption, source.string), p <- xtab.plot.bar.moe(dt, "percent", xlabel, ylabel, geog.caption, source.string))
        return(p)
      } else if (dttype %in% c('estimate_with_MOE')) {
        ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "nominal", xlabel, ylabel, geog.caption, source.string), p <- xtab.plot.bar.moe(dt, "nominal", xlabel, ylabel, geog.caption, source.string))
        return(p)
      } else {
        return(NULL)
      }
    } else { # if xtabTableType()$Type == 'fact'
      if (is.null(input$xtab_dtype_rbtns_fact)) return(NULL)
      dttype <- input$xtab_dtype_rbtns_fact
      dttype.label <- names(dtype.choice.xtab.facts[dtype.choice.xtab.facts == dttype])
      
      if (dttype %in% c("sample_count", "median", "MOE", "N_HH")) {
        dt <- xtabVisTable()[[dttype]]
      } else { #if (dttype == "median_with_MOE")
        dt <- xtabVisTable.medianMOE()
      }
      
      if (dttype %in% c("sample_count", "median", "N_HH")) {
        p <- xtab.plot.bar.fact(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption, input$xtab_dataset)
        return(p)
      } else { # median_with_MOE
        p <- xtab.plot.bar.fact.moe(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption, input$xtab_dataset)
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
      
      if (dttype %in% c("median", "sample_count")) {
        # This if/else chunk joins sample count to the table of choice with the purpose
        # of greying out values where sample counts are low.
        
        dt <- xtab.join.samplecnt(xtabTableClean(), dttype, varsXAlias()) 
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      }  else {
        if (dttype %in% c("median_with_MOE")) {
          
          dt <- xtab.tblMOE.join.samplecnt(xtabTableClean.DT.medianMOE(), xtabTableClean(), dttype, varsXAlias())
        }
        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      } 
      
      sketch.dtstyle <- dt.container.dtstyle(dt, varsXAlias(), varsYAlias()) 
    
      if (dttype == 'median') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 2)
      } else if (dttype == 'sample_count') {
        xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == 'median_with_MOE') {
        sketch.dtstyle.exp <- dt.container.tblMOE.dtstyle(dt, varsXAlias(), varsYAlias(), "median")
        xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(cols.fmt, 2)
      }
    }
    
  })
  
  
  
  output$ui_xtab_tbl <- renderUI({
    div(DT::dataTableOutput('xtab_tbl'), style = 'font-size: 95%; width: 85%', class = 'visual-display', )
    
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
      # join median/MOE with sample count
      tmm <- copy(xtabTableClean.DT.medianMOE())
      tjoin <- tmm[t, on = varsXAlias()]
      tj <- tjoin[, median := lapply(.SD, function(x) round(x, 2)), .SDcols = 'median'
                  ][, `Sample Count`:= lapply(.SD, function(x) prettyNum(x, big.mark = ",")), .SDcols = "Sample Count"
                    ][, `Result Type` := geog]
      setnames(tj, "MOE", "Margin of Error (median)")
      tbllist <- list("About" = readme.dt,
                      "median with Margin of Error" = tj)
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
  
  stab.variable.tbl<-eventReactive(input$stab_dataset,{
    # filter variables table by survey year
    if(input$stab_dataset=='2017/2019'){
      survey_year_name='2019'
    }
    else{
      survey_year_name=input$stab_dataset
    }

    variables.lu[survey_year == survey_year_name, ]
  })
  
  stab.values.tbl<-eventReactive(input$stab_dataset,{
    # filter variables table by survey year
    if(input$stab_dataset=='2017/2019'){
      survey_year_name='2019'
    }
    else{
      survey_year_name=input$stab_dataset
    }
    values.lu[survey_year==survey_year_name, ]
  })
  
  
  output$stab_xcol_det <- renderText({
    xvar.det <- stab.variable.tbl()[variable %in% input$stab_xcol, .(detail)]
    unique(xvar.det$detail)
  })
  
  # variable X alias
  stab.varsXAlias <- eventReactive(input$stab_go, {
    #add survey_year
    xvar.alias <- stab.variable.tbl()[variable %in% input$stab_xcol, .(variable_name)]
    unique(xvar.alias$variable_name)
  })
  
  # variable X alias list
  stab.varsListX <- reactive({
    t <- stab.variable.tbl()[category %in% input$stab_xcat, ]
    vars.raw <- as.list(unique(t$variable))
    vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))
  })
  
  output$ui_stab_xcol <- renderUI({
    selectInput('stab_xcol',
                'Variable', 
                stab.varsListX())
  })
  
  stabXValues <- eventReactive(input$stab_go, {
    dt <- stab.values.tbl()[variable %in% input$stab_xcol, ][order(value_order)] # return dt
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
    select.vars <-   stab.variable.tbl()[variable %in% c(input$stab_xcol), ]
    tables <- unique(select.vars$table_name)
    table_name<- table_names[[tables]]
    dtypes <- as.vector(unique(select.vars$dtype)) 
    weight_name<- select.vars$weight_name
    dtypes <- as.vector(unique(select.vars$dtype))

    if('fact' %in% dtypes){
      type<- 'fact'
    }
    else{
      type<-'dimension'
    }
    
    return(list(Weight_Name=weight_name, Type=type, Table_Name=table_name))
  } )

  # return list of tables subsetted by value types
  stabTable <- eventReactive(input$stab_go, {
    wt_field <- stabTableType()$Weight_Name
    table_name<-stabTableType()$Table_Name

    if (input$stab_dataset == '2017/2019')
    {
      survey_yr = "2017_2019"
    }
    else{
      survey_yr = input$stab_dataset
    }

    type <- stabTableType()$Type
    
    data_for_stab <-
      get_hhts(
        survey = survey_yr,
        level = table_name,
        vars = c("seattle_home", input$stab_xcol)
      ) %>% setDT()
    
   
    if (input$stab_fltr_sea == T) data_for_stab <- data_for_stab[seattle_home == 'Home in Seattle',]
    
    xa <- stab.varsXAlias()

    if (type=='fact') {
      data_for_stab<- data_for_stab[eval(parse(text=input$stab_xcol))>min_float]
      data_for_stab <- data_for_stab[eval(parse(text=input$stab_xcol))<max_float]
      breaks<- hist_breaks
      hist_labels<- hist_breaks_labels
      data_for_stab<-data_for_stab[, input$stab_xcol:= cut(eval(parse(text=input$stab_xcol)),breaks,labels=hist_labels, order_result=TRUE,)]

    }

     simpletab <-
        hhts_count(
          data_for_stab,
          group_vars = c(input$stab_xcol),
          incl_na = FALSE
        )
     
     setnames(simpletab, old=c('count', 'count_moe', 'share', 'share_moe', 'sample_size'), new=c("estimate", "estMOE", "share", "MOE", 'sample_count'))
     
     
    simpletab<- simpletab%>% select(input$stab_xcol,  "estimate", "estMOE", "share", "MOE", 'sample_count')%>% filter(share != 1) %>% setDT()
  
    #%>%pivot_wider(names_from=input$stab_xcol, values_from=c("estimate", "estMOE", "share", "MOE", 'sample_count'))%>% setDT()
      
    
  
    xvals <- stabXValues()[, .(value_order, value_text)][]
    
    # check input type and xvals. sometimes xvals doesn't exist for some variables
    if((typeof(input$stab_xcol) == 'character') & (nrow(xvals) > 0)){ 
        simpletab <- merge(simpletab, xvals, by.x=input$stab_xcol, by.y='value_text')
        setorder(simpletab, value_order)
    }
    
    dtypes <- dtype.choice.stab 
    selcols <- c(xa, names(dtypes))

    setnames(simpletab, c(input$stab_xcol, dtypes), selcols)
    setcolorder(simpletab, selcols)
    
    dt <- simpletab[!(get(eval(xa)) %in% "")][, ..selcols]
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
    source.string<- paste(input$stab_dataset, "Household Travel Survey")

    if (dttype %in% col.headers) {
      dt <- stabVisTable()[type %in% selection, ]
    } else {
      if (dttype == "share_with_MOE") dt <- stabVisTable.shareMOE()
      if (dttype == "estimate_with_MOE") dt <- stabVisTable.estMOE() 
    }

    l <- length(stabXValues()$ValueText) 
    if(l == 0) l <- length(unique(dt$value)) # evaluate if values are not in lookup (length 0)
    
    if (dttype == 'share') {
      ifelse(l < 10, p <- stab.plot.bar(dt, "percent", xlabel, geog.caption, source.string=source.string), p <- stab.plot.bar2(dt, "percent", xlabel, geog.caption, source.string=source.string))
      return(p)
    } else if (dttype %in% c('estimate', 'sample_count', 'N_HH')) {
      ifelse(l < 10, p <- stab.plot.bar(dt, "nominal", xlabel, geog.caption, source.string=source.string), p <- stab.plot.bar2(dt, "nominal", xlabel, geog.caption, source.string=source.string))
      return(p)
    } else if (dttype == 'share_with_MOE') {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "percent", xlabel, geog.caption, source.string=source.string), p <- stab.plot.bar2.moe(dt, "percent", xlabel, geog.caption, source.string=source.string))
      return(p)
    } else if (dttype == 'estimate_with_MOE') {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "nominal", xlabel, geog.caption, source.string=source.string),  p <- stab.plot.bar2.moe(dt, "nominal", xlabel, geog.caption, source.string=source.string))
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

  output$ui_stab_vis <- renderUI({
    plotlyOutput("stab_vis", width = "85%")
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
      if(input$stab_dataset=='2017/2019'){
        survey_year_name='2017_2019'
      }
      else{
        survey_year_name=input$stab_dataset
      }
      paste0("HHSurvey_",survey_year_name,"_", stab.varsXAlias(),"_", stabCaption(), ".xlsx")
    },
    content = function(file) {
      # write.xlsx(stabTable(), file)
      write.xlsx(stabDownloadOutput(), file)
    }
  )
  
}



