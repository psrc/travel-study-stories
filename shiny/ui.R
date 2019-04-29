fluidPage(title = "", windowTitle = "2017 Household Survey Results",
          # shinythemes::themeSelector(),
          theme = shinytheme("flatly"),
          
          navbarPage("2017 Household Survey Results",
                     tabPanel("Crosstab Generator",
                              # fluidRow(
                              # ),
                              br(),
                              fluidRow(
                                column(3,
                                       wellPanel(
                                         p(strong("First Dimension (Rows)")),
                                         selectInput('xtab_xcat',
                                                     'Category',
                                                     width = '75%',
                                                     vars.cat[!(vars.cat %in% "None")]),
                                         uiOutput("ui_xtab_xcol")
                                         ) # end wellPanel
                                       ), # end column
                                column(3,
                                       wellPanel(
                                         p(strong("Second Dimension (Columns)")),
                                         selectInput('xtab_ycat',
                                                     'Category',
                                                     width = '75%',
                                                     vars.cat[!(vars.cat %in% "None")]),
                                         uiOutput("ui_xtab_ycol")
                                         ) # end welPanel
                                ), # end column
                                column(3,
                                       p("Select from the following characteristics, organized by categories, to generate cross-tabulated tables."), 
                                       p("Click 'Download Data' to download tabular data for all summary types after the cross-tabulations have been generated."), 
                                       # style = "margin-top: 15px;",
                                       actionButton('xtab_go', 
                                                    'Create Crosstab'),
                                       br(),
                                       br(),
                                       downloadButton("xtab_download", "Download Data")
                                ) # end column
                              ), # end fluidRow
                              br(),
                              br(), 
                              conditionalPanel(
                                "input.xtab_go",
                                fluidRow(
                                  column(2,
                                    wellPanel(
                                             radioButtons("xtab_dtype_rbtns",
                                                          label = strong("Summary Types"),
                                                          choices = dtype.choice
                                                          )
                                    )
                                  ),
                                  column(10, 
                                         fluidRow(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Table",
                                                                br(),
                                                                div(DTOutput('xtab_tbl'), style = 'font-size: 95%; width: 85%')),
                                                       tabPanel("Visual",
                                                                br(),
                                                                plotlyOutput("xtab_vis", width = "85%"))
                                                       
                                           ) # end tabsetPanel
                                         ) # end fluidRow
                                  ) # end column
                                ) # end fluidRow
                              ) # end conditional Panel
                              ), # end tabPanel
                     tabPanel("Simple Table")
                     ) # end navbarPage
          ) # end fluidPage

