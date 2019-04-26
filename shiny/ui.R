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
                                       p("Data can be downloaded once the cross-tabulations have been generated."), 
                                       # style = "margin-top: 15px;",
                                       actionButton('xtab_go', 
                                                    'Create Crosstab'),
                                       br(),
                                       br(),
                                       downloadButton("xtab_download", "Download")
                                ) # end column
                              ), # end fluidRow
                              br(),
                              br(), 
                              fluidRow(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Shares", 
                                                     br(),
                                                     div(DTOutput('xtab_table_share'), style = 'font-size: 95%; width: 75%')),
                                            tabPanel("Estimates", 
                                                     br(),
                                                     div(DTOutput('xtab_table_estimate'), style = 'font-size: 95%; width: 75%')),
                                            tabPanel("Number of Households", 
                                                     br(),
                                                     div(DTOutput('xtab_table_N_HH'), style = 'font-size: 95%; width: 75%')),
                                            tabPanel("Margin of Error", 
                                                     br(),
                                                     div(DTOutput('xtab_table_MOE'), style = 'font-size: 95%; width: 75%')),
                                            tabPanel("Sample Count", 
                                                     br(),
                                                     div(DTOutput('xtab_table_sample_count'), style = 'font-size: 95%; width: 75%')))
                              ) # end fluidRow
                              ), # end tabPanel
                     tabPanel("Simple Table")
                     ) # end navbarPage
          ) # end fluidPage

