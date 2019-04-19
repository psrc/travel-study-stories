fluidPage(title = "", windowTitle = "2017 Household Survey Results",
          # shinythemes::themeSelector(),
          theme = shinytheme("flatly"),
          
          navbarPage("Household Survey Results",
                     tabPanel("Crosstab Generator",
                              fluidRow(
                                h5(class="header", 
                                   "Select from the following characteristics, organized by categories, to generate cross-tabulated tables")
                              ),
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
                                       ),
                                column(3,
                                       wellPanel(
                                         p(strong("Second Dimension (Columns)")),
                                         selectInput('xtab_ycat',
                                                     'Category',
                                                     width = '75%',
                                                     vars.cat),
                                         uiOutput("ui_xtab_ycol")
                                         ) # end welPanel
                                ),
                                column(1,
                                       style = "margin-top: 15px;",
                                       actionButton('xtab_go', 
                                                    'Create Crosstab'))
                              ), # end fluidRow
                              br(),
                              br(), 
                              fluidRow(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Shares", DTOutput('xtab_table_share')),
                                            tabPanel("Estimates", DTOutput('xtab_table_estimate')),
                                            tabPanel("Number of Households", DTOutput('xtab_table_N_HH')),
                                            tabPanel("Margin of Error", DTOutput('xtab_table_MOE')),
                                            tabPanel("Sample Count", DTOutput('xtab_table_sample_count')))
                              ) # end fluidRow
                              ) # end tabPanel
                     ) # end navbarPage
          ) # end fluidPage

