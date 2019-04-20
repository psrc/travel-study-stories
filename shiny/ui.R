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
                                                     vars.cat),
                                         uiOutput("ui_xtab_xcol")
                                         ) # end wellPanel
                                       ), # end column
                                column(3,
                                       wellPanel(
                                         p(strong("Second Dimension (Columns)")),
                                         selectInput('xtab_ycat',
                                                     'Category',
                                                     width = '75%',
                                                     vars.cat),
                                         uiOutput("ui_xtab_ycol")
                                         ) # end welPanel
                                ), # end column
                                column(2,
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
                                                     DTOutput('xtab_table_share')),
                                            tabPanel("Estimates", 
                                                     br(),
                                                     DTOutput('xtab_table_estimate')),
                                            tabPanel("Number of Households", 
                                                     br(),
                                                     DTOutput('xtab_table_N_HH')),
                                            tabPanel("Margin of Error", 
                                                     br(),
                                                     DTOutput('xtab_table_MOE')),
                                            tabPanel("Sample Count", 
                                                     br(),
                                                     DTOutput('xtab_table_sample_count')))
                              ) # end fluidRow
                              ) # end tabPanel
                     ) # end navbarPage
          ) # end fluidPage

