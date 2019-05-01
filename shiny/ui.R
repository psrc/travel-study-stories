fluidPage(title = "", windowTitle = "2017 Household Survey Results",
          # shinythemes::themeSelector(),
          theme = shinytheme("flatly"),
          
          navbarPage("2017 Household Survey Results",
                     tabPanel("Crosstab Generator",
                              br(),
                              fluidRow(
                                column(3,
                                       wellPanel(
                                         p(strong("First Dimension (Rows)")),
                                         selectInput('xtab_xcat',
                                                     'Category',
                                                     # width = '75%',
                                                     vars.cat[!(vars.cat %in% "None")]),
                                         uiOutput("ui_xtab_xcol")
                                         ) # end wellPanel
                                       ), # end column
                                column(3,
                                       wellPanel(
                                         p(strong("Second Dimension (Columns)")),
                                         selectInput('xtab_ycat',
                                                     'Category',
                                                     # width = '75%',
                                                     vars.cat[!(vars.cat %in% "None")]),
                                         uiOutput("ui_xtab_ycol")
                                         ) # end welPanel
                                ), # end column
                                column(3,
                                       p("Select from the following characteristics, organized by categories, to generate cross-tabulated tables."), 
                                       p("Click 'Download Data' to download tabular data for all summary types after the cross-tabulations have been generated."), 
                                       actionButton('xtab_go', 'Create Crosstab'),
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
                                                          choices = dtype.choice[!(dtype.choice %in% 'N_HH')]
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
                     tabPanel("Simple Table",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             p("Select from the following characteristics, organized by categories, to generate a simple summary table."),
                                             p("Click 'Download Data' to download tabular data after the table has been generated."),
                                             br(),
                                            selectInput('stab_xcat',
                                                        'Category',
                                                        # width = '75%',
                                                        vars.cat[!(vars.cat %in% "None")]),
                                            uiOutput("ui_stab_xcol"),
                                            br(),
                                            actionButton('stab_go', 
                                                         'Create Table'),
                                            br(),
                                            br(),
                                            downloadButton("stab_download", "Download Data"),
                                            br(),
                                            br(),
                                            br(),
                                            conditionalPanel(
                                              "input.stab_go",
                                              radioButtons("stab_dtype_rbtns",
                                                           label = strong("Visual Options"),
                                                           choices = dtype.choice[!(dtype.choice %in% c('MOE', 'N_HH'))]
                                              )
                                              
                                            ) # end conditionalPanel
                                ), # end sidbarPanel
                                mainPanel(width = 9,
                                          div(DTOutput('stab_tbl'), style = 'font-size: 95%; width: 85%'),
                                          br(),
                                          br(),
                                          br(),
                                          plotlyOutput('stab_vis', width = "85%")
                                ) # end mainPanel
                              ) # end sidebarLayout
                              ) # end tabPanel
                     ) # end navbarPage
          ) # end fluidPage

