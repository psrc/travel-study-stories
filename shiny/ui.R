fluidPage(title = "", windowTitle = "2017 Household Survey Results",
          # shinythemes::themeSelector(),
          theme = shinytheme("flatly"),
          
          navbarPage("HH Survey Results",
                     tabPanel("Crosstab Generator",
                              fluidRow(
                                h5(class="header", 
                                   "Select from the following persons characteristics to generate a cross-tabulated table ")
                              ),
                              br(),
                              fluidRow(
                                column(3,
                                       selectInput('xtab_xcol', 
                                                   'First Dimension', 
                                                   width = '100%', 
                                                   person_variables)),
                                column(3,
                                       selectInput('xtab_ycol', 
                                                   'Second Dimension', 
                                                   width = '100%', 
                                                   person_variables)),
                                column(1,
                                       style = "margin-top: 15px;",
                                       actionButton('xtab_go', 
                                                    'Create Crosstab'))
                              ), # end fluidRow
                              br(),
                              br(), 
                              mainPanel(DTOutput('xtab_table'))
                              ) # end tabPanel
                     ) # end navbarPage
          ) # end fluidPage

