fluidPage(title = "", windowTitle = "2017 Household Survey Results",
          # shinythemes::themeSelector(),
          theme = shinytheme("flatly"),
          useShinyjs(),
          navbarPage("2017 Household Survey Results",
                     tabPanel("Crosstab Generator",
                              br()#,
                              # fluidRow(
                              #   column(2,
                              #          p("Select from the following characteristics, organized by categories, to generate cross-tabulated tables."),
                              #          p("Tables may be summarized by share, margin of error based on the share, weighted estimates, or sample count."), 
                              #          p("Click 'Download Data' to download tabular data for all summary types after the cross-tabulations have been generated.")
                              #          ),
                              #   column(3,
                              #          wellPanel(
                              #            p(strong("First Dimension (Rows)")),
                              #            selectInput('xtab_xcat',
                              #                        'Category',
                              #                        # width = '75%',
                              #                        vars.cat[!(vars.cat %in% "None")]),
                              #            uiOutput("ui_xtab_xcol"),
                              #            div(a(id = "xtabXtoggleAdvanced", "Show/hide variable detail", href = "#"),
                              #                hidden(
                              #                  div(id = "xtabXAdvanced",
                              #                      textOutput("xtab_xcol_det")
                              #                  ) # end div
                              #                ), # end hidden
                              #                style = 'font-size: 90%'
                              #            )# end div
                              #            ) # end wellPanel
                              #          ), # end column
                              #   column(3,
                              #          wellPanel(
                              #            p(strong("Second Dimension (Columns)")),
                              #            selectInput('xtab_ycat',
                              #                        'Category',
                              #                        # width = '75%',
                              #                        vars.cat[!(vars.cat %in% "None")]),
                              #            uiOutput("ui_xtab_ycol"),
                              #            div(a(id = "xtabYtoggleAdvanced", "Show/hide variable detail", href = "#"),
                              #                hidden(
                              #                  div(id = "xtabYAdvanced",
                              #                      textOutput("xtab_ycol_det")
                              #                      ) # end div
                              #                 ), # end hidden
                              #                style = 'font-size: 90%'
                              #            )# end div
                              #          ) # end welPanel
                              #   ), # end column
                              # 
                              #   column(3,
                              #          actionButton('xtab_go', 'Create Crosstab', width = '150px'),
                              #          br(),
                              #          br(),
                              #          downloadButton("xtab_download", "Download Data")
                              #   ) # end column
                              # ), # end fluidRow
                              # br(),
                              # br(),
                              # conditionalPanel(
                              #   "input.xtab_go",
                              #   fluidRow(
                              #     column(2,
                              #       wellPanel(
                              #                radioButtons("xtab_dtype_rbtns",
                              #                             label = strong("Summary Types"),
                              #                             # choices = dtype.choice[!(dtype.choice %in% 'N_HH')]
                              #                             choices = dtype.choice.xtab
                              #                             )
                              #       )
                              #     ),
                              #     column(10,
                              #            fluidRow(
                              #              tabsetPanel(type = "tabs",
                              #                          tabPanel("Table",
                              #                                   br(),
                              #                                   div(DTOutput('xtab_tbl'), style = 'font-size: 95%; width: 85%')
                              #                                   ),
                              #                          tabPanel("Visual",
                              #                                   br(),
                              #                                   plotlyOutput("xtab_vis", width = "85%"))
                              # 
                              #              ) # end tabsetPanel
                              #            ) # end fluidRow
                              #     ) # end column
                              #   ) # end fluidRow
                              # ) # end conditional Panel
                              ), # end tabPanel
                     tabPanel("Simple Table"#,
                              # sidebarLayout(
                              #   sidebarPanel(width = 3,
                              #                p("Select from the following characteristics, organized by categories, to generate a simple summary table."),
                              #                p("Click 'Download Data' to download tabular data after the table has been generated."),
                              #                br(),
                              #               selectInput('stab_xcat',
                              #                           'Category',
                              #                           # width = '75%',
                              #                           vars.cat[!(vars.cat %in% "None")]),
                              #               uiOutput("ui_stab_xcol"),
                              #               div(a(id = "stabXtoggleAdvanced", "Show/hide variable detail", href = "#"),
                              #                   hidden(
                              #                     div(id = "stabXAdvanced", 
                              #                         textOutput("stab_xcol_det")
                              #                     ) # end div 
                              #                   ), # end hidden
                              #                   style = 'font-size: 90%'
                              #               ), # end div
                              #               br(),
                              #               actionButton('stab_go', 
                              #                            'Create Table'),
                              #               br(),
                              #               br(),
                              #               downloadButton("stab_download", "Download Data"),
                              #               br(),
                              #               br(),
                              #               br(),
                              #               conditionalPanel(
                              #                 "input.stab_go",
                              #                 radioButtons("stab_dtype_rbtns",
                              #                              label = strong("Visual Options"),
                              #                              # choices = dtype.choice[!(dtype.choice %in% c('MOE', 'N_HH', 'share_with_MOE'))]
                              #                              choices = dtype.choice.stab.vis
                              #                 )
                              #                 
                              #               ) # end conditionalPanel
                              #   ), # end sidbarPanel
                              #   mainPanel(width = 9,
                              #             div(DTOutput('stab_tbl'), style = 'font-size: 95%; width: 85%'),#
                              #             br(),
                              #             br(),
                              #             br(),
                              #             plotlyOutput('stab_vis', width = "85%")
                              #   ) # end mainPanel
                              # ) # end sidebarLayout
                              ) # end tabPanel
                     ) # end navbarPage
          ) # end fluidPage

