fluidPage(title = "", windowTitle = "Travel Survey Data Explorer",
          #shinythemes::themeSelector(),
          #theme = shinytheme("flatly"),
          #theme = shinytheme("united"),
          theme = "bootstrap_united.css",
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "additional-styles.css")
          ),
          useShinyjs(),
          navbarPage("2017/2019 Household Survey Results",

# Simple Table ------------------------------------------------------------

                                          tabPanel("One-way Table",
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
                                                                 div(a(id = "stabXtoggleAdvanced", "Show/hide variable detail", href = "#"),
                                                                     hidden(
                                                                       div(id = "stabXAdvanced",
                                                                           textOutput("stab_xcol_det")
                                                                       ) # end div
                                                                     ), # end hidden
                                                                     style = 'font-size: 90%'
                                                                 ), # end div
                                                                 div(checkboxInput('stab_fltr_sea',
                                                                                   label = "Select Seattle households only",
                                                                                   value = FALSE), style="font-size:95%;"),
                                                                 actionButton('stab_go',
                                                                              'Create Table'),
                                                                 br(),
                                                                 br(),
                                                                 downloadButton("stab_download", "Download Data"),
                                                                 br(),
                                                                 br(),
                                                                 div(a(href = "https://en.wikipedia.org/wiki/Margin_of_error", "About the Margin of Error", target = "_blank"), style = 'font-size: 85%'),
                                                                 div(p("The Margin of Error is calculated for a 90% confidence interval.
                                                                    As a rule of thumb, you should have a sample count of 30 or more for any given statistic to feel comfortable with it.
                                                                    Statistics with less than 30 will be greyed out in the one-way table."), style = 'font-size: 85%'),
                                                                 br(),
                                                                 conditionalPanel(
                                                                   "input.stab_go",
                                                                   radioButtons("stab_dtype_rbtns",
                                                                                label = strong("Visual Options"),
                                                                                # choices = dtype.choice[!(dtype.choice %in% c('MOE', 'N_HH', 'share_with_MOE'))]
                                                                                choices = dtype.choice.stab.vis
                                                                   )

                                                                 ) # end conditionalPanel
                                                     ), # end sidbarPanel
                                                     mainPanel(width = 9,
                                                               div(
                                                               uiOutput("ui_stab_res_type_title"),
                                                               br(),
                                                               uiOutput("ui_stab_tbl"),
                                                               # div(DT::dataTableOutput('stab_tbl'), style = 'font-size: 95%; width: 85%'),#
                                                               br(),
                                                               br(),
                                                               br(),
                                                               uiOutput("ui_stab_vis"), 
                                                               class='visual-display'
                                                               # plotlyOutput('stab_vis', width = "85%")
                                                               ) # end div
                                                               
                                                     ) # end mainPanel
                                                   ) # end sidebarLayout
                                                   ), # end tabPanel
                     
# Crosstab Generator ------------------------------------------------------

                     tabPanel("Two-way Table",
                              br(),
                              fluidRow(
                                column(2,
                                       p("Select from the following characteristics, organized by categories, to generate two-way tables."),
                                       p("Tables may be summarized by sample counts, or by share or weighted totals, with and without margins of error."),
                                       p("Click 'Download Data' to download tabular data for all summary types after the cross-tabulation has been generated.")
                                       ),
                                column(3,
                                       wellPanel(
                                         p(strong("First Variable (Crosstab rows)")),
                                         selectInput('xtab_xcat',
                                                     'Category',
                                                     # width = '75%',
                                                     vars.cat[!(vars.cat %in% "None")]),
                                         uiOutput("ui_xtab_xcol"),
                                         div(a(id = "xtabXtoggleAdvanced", "Show/hide variable detail", href = "#"),
                                             hidden(
                                               div(id = "xtabXAdvanced",
                                                   textOutput("xtab_xcol_det")
                                               ) # end div
                                             ), # end hidden
                                             style = 'font-size: 90%'
                                         )# end div
                                         ) # end wellPanel
                                       ), # end column
                                column(3,
                                       wellPanel(
                                         p(strong("Second Variable (Crosstab columns)")),
                                         selectInput('xtab_ycat',
                                                     'Category',
                                                     # width = '75%',
                                                     vars.cat[!(vars.cat %in% "None")]),
                                         uiOutput("ui_xtab_ycol"),
                                         div(a(id = "xtabYtoggleAdvanced", "Show/hide variable detail", href = "#"),
                                             hidden(
                                               div(id = "xtabYAdvanced",
                                                   textOutput("xtab_ycol_det")
                                                   ) # end div
                                              ), # end hidden
                                             style = 'font-size: 90%'
                                         )# end div
                                       ) # end welPanel
                                ), # end column
                                
                                column(3,
                                       fluidRow(
                                         column(7,
                                                wellPanel(
                                                          div(checkboxInput('xtab_fltr_sea', 
                                                                        label = "Select Seattle households only", 
                                                                        value = FALSE), style="font-size:95%;"),
                                                          style = "padding: 25px;"
                                                        )
                                                ), # end column
                                         column(1,
                                                actionButton('xtab_go', 'Create Crosstab', width = '150px'),
                                                br(),
                                                br(),
                                                downloadButton("xtab_download", "Download Data")
                                                ) # end column
                                       ), # end fluidRow
                                       fluidRow(
                                         column(12,
                                           br(),
                                           div(a(href = "https://en.wikipedia.org/wiki/Margin_of_error", "About the Margin of Error", target = "_blank"), style = 'font-size: 85%'),
                                           div(p("The Margin of Error is calculated for a 90% confidence interval.
                                                  As a rule of thumb, you should have a sample count of 30 or more for any given statistic to feel comfortable with it.
                                                  Statistics with less than 30 will be greyed out in the two-way tables."), style = 'font-size: 85%')
                                         ) # end column
                                       ) # end fluidRow
                                       ) # end column
                               
                              ), # end fluidRow
                              

# Crosstab Generator Render Table and Visual ------------------------------

                              # br(),
                              # br(),
                              conditionalPanel(
                                "input.xtab_go",
                                fluidRow(
                                  column(2,
                                    uiOutput("ui_xtab_res_type_title"),
                                    uiOutput("ui_xtab_dtype_rbtns")
                                    # wellPanel(
                                             # radioButtons("xtab_dtype_rbtns",
                                             #              label = strong("Summary Types"),
                                             #              # choices = dtype.choice[!(dtype.choice %in% 'N_HH')]
                                             #              choices = dtype.choice.xtab
                                             #              ),
                                             # div(p("Shares are based on rowwise totals."), style = 'font-size: 85%')
                                    # ) # end wellPanel
                                  ),
                                  column(10,
                                         fluidRow(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Table",
                                                                br(),
                                                                # div(DT::dataTableOutput('xtab_tbl'), style = 'font-size: 95%; width: 85%')
                                                                uiOutput("ui_xtab_tbl")
                                                                ),
                                                       tabPanel("Visual",
                                                                br(),
                                                                # plotlyOutput("xtab_vis", width = "85%"))
                                                                uiOutput("ui_xtab_vis"))

                                           ) # end tabsetPanel
                                         ) # end fluidRow
                                  ) # end column
                                ) # end fluidRow
                              ) # end conditional Panel
                              ), # end tabPanel

# About  ------------------------------------------------------------------


                      tabPanel("About Travel Survey Data Explorer",
                               column(2),
                               column(8,
                                      includeMarkdown(file.path(wrkdir, "about_page.md"))
                                      ),
                               column(2)
                               )

# # Deprecated Simple Table ------------------------------------------------------------
# 
#                      tabPanel("Simple Table",
#                               sidebarLayout(
#                                 sidebarPanel(width = 3,
#                                              p("Select from the following characteristics, organized by categories, to generate a simple summary table."),
#                                              p("Click 'Download Data' to download tabular data after the table has been generated."),
#                                              br(),
#                                             selectInput('stab_xcat',
#                                                         'Category',
#                                                         # width = '75%',
#                                                         vars.cat[!(vars.cat %in% "None")]),
#                                             uiOutput("ui_stab_xcol"),
#                                             div(a(id = "stabXtoggleAdvanced", "Show/hide variable detail", href = "#"),
#                                                 hidden(
#                                                   div(id = "stabXAdvanced",
#                                                       textOutput("stab_xcol_det")
#                                                   ) # end div
#                                                 ), # end hidden
#                                                 style = 'font-size: 90%'
#                                             ), # end div
#                                             div(checkboxInput('stab_fltr_sea', 
#                                                               label = "Select Seattle households only", 
#                                                               value = FALSE), style="font-size:95%;"),
#                                             actionButton('stab_go',
#                                                          'Create Table'),
#                                             br(),
#                                             br(),
#                                             downloadButton("stab_download", "Download Data"),
#                                             br(),
#                                             br(),
#                                             div(a(href = "https://en.wikipedia.org/wiki/Margin_of_error", "About the Margin of Error", target = "_blank"), style = 'font-size: 85%'),
#                                             div(p("The Margin of Error is calculated for a 90% confidence interval.
#                                                As a rule of thumb, you should have a sample count of 30 or more for any given statistic to feel comfortable with it.
#                                                Statistics with less than 30 will be greyed out in the cross-tabulated tables."), style = 'font-size: 85%'),
#                                             br(),
#                                             conditionalPanel(
#                                               "input.stab_go",
#                                               radioButtons("stab_dtype_rbtns",
#                                                            label = strong("Visual Options"),
#                                                            # choices = dtype.choice[!(dtype.choice %in% c('MOE', 'N_HH', 'share_with_MOE'))]
#                                                            choices = dtype.choice.stab.vis
#                                               )
# 
#                                             ) # end conditionalPanel
#                                 ), # end sidbarPanel
#                                 mainPanel(width = 9,
#                                           uiOutput("ui_stab_res_type_title"),
#                                           br(),
#                                           div(DT::dataTableOutput('stab_tbl'), style = 'font-size: 95%; width: 85%'),#
#                                           br(),
#                                           br(),
#                                           br(),
#                                           plotlyOutput('stab_vis', width = "85%")
#                                 ) # end mainPanel
#                               ) # end sidebarLayout
#                               ) # end tabPanel

                     ) # end navbarPage
          ) # end fluidPage

