# Chull ui
ui <- shinyUI(navbarPage(theme = shinythemes::shinytheme("united"),"multichull",
                         windowTitle = "multichull",

                         ################ load files ###########
                         tabPanel("", icon = icon("folder-open","fa-2x"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput(inputId = 'loadfile',label = "Select file:",
                                                multiple = FALSE)

                                    ),# end sidebarPanel

                                    mainPanel(
                                      fluidRow(
                                        column(2,

                                               tableOutput('fileviewer')
                                        ),# end column
                                        column(8,
                                               #tableOutput('ui')
                                               DT::dataTableOutput('ui')
                                        )# end column
                                      ) # end fluidRow

                                    )# end main panel
                                  )# end sidebar layout
                         ), # end loaddata panel

                         ############### chull options ##########
                         tabPanel(title = "", icon = icon("gears","fa-2x"),
                                  fluidPage(
                                    titlePanel("Analysis Options"),

                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("CHull",
                                                 radioButtons("bound", label = h4("What does the fit measure indicate?"),
                                                              choices = list("Badness-of-fit" = "lower", "Goodness-of-fit" = "upper"
                                                              ),selected = "lower"),
                                                 numericInput("PercentageFit",
                                                              label = h4("Required improvement in fit?"),
                                                              value = 1,min=0,max=1,step=1),
                                                 br(),
                                                 actionButton('stChull',label = "Start CHull",
                                                              icon = icon("flash",lib = "glyphicon"))

                                        ), #end tabPanel CHull

                                        tabPanel("MultiCHull",
                                                 radioButtons("boundM", label = h4("What does the fit measure indicate?"),
                                                              choices = list("Badness-of-fit" = "lower", "Goodness-of-fit" = "upper"
                                                              ),selected = "lower"),
                                                 radioButtons("typeMC", label = h4("Multiple fit or multiple complexities?"),
                                                              choices = list("Multiple fit" = "multifit", "Multiple complexities" = "multicom"
                                                              ),selected = "multifit"),
                                                 numericInput("PercentageFitM",
                                                              label = h4("Required improvement in fit?"),
                                                              value = 1,min=0,max=100,step=1),
                                                 br(),
                                                 actionButton('stMultiChull',label = "Start MultiCHull",
                                                              icon = icon("flash",lib = "glyphicon"))
                                        )# end tabPanel MultiChull
                                      )#end tabset
                                    )# end mainpanel



                                  )# end fluidpage

                         ), #end settings panel

                         ############# results panel #########
                         tabPanel(title = "", icon = icon("bar-chart","fa-2x"),
                                  fluidPage(
                                    titlePanel("Results"),

                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Summary",
                                                 #verbatimTextOutput("summary"),

                                                 verbatimTextOutput("summary")
                                        ), #end tabPanel summary

                                        tabPanel("Plot",
                                                 plotly::plotlyOutput("plotly")
                                        )#,# end tabPanel plotly
                                        # tabPanel("Plot MultiCHull multiple complexities",
                                        #          pageWithSidebar(
                                        #
                                        #            headerPanel("MultiChull different complexities"),
                                        #
                                        #            sidebarPanel(
                                        #
                                        #              selectInput('cominput', 'Complexity', choices = '')
                                        #
                                        #            ),
                                        #
                                        #            mainPanel(
                                        #              plotly::plotlyOutput("plotlycom")
                                        #            )
                                        #          )
                                        #
                                        # ) # end tabPanel plotly
                                      )#end tabset
                                    )# end mainpanel



                                  )# end fluidpage

                         ), # end results panel


                         ########### information
                         # tabPanel(title = "", icon = icon("info-sign","fa-2x", "glyphicon"),
                         #          navlistPanel("CHull",id='info',br(), well = F,selected = "Details",
                         #                       tabPanel("Details",
                         #                                h2("Details page"),
                         #                                h3('Software details'),
                         #                                'Software status: this is an alpha version.',
                         #                                br(),
                         #                                'Please report bugs and errors to: j.durieux@fsw.leidenuniv.nl or open an issue on', tags$a(href='https://github.com/jeffreydurieux/multichull_shiny','GitHub', target = '_blank'),
                         #                                br(),
                         #                                h3('youtube video tutorials here:')),
                         #
                         #                       tabPanel("References",
                         #                                h2("References page"),
                         #                                br(),
                         #                                h3('CHull papers:'),
                         #                                br(),
                         #                                tags$a(href='https://bpspsychub.onlinelibrary.wiley.com/doi/full/10.1348/000711005X64817?casa_token=vzZewzwUsXoAAAAA%3ALsuycpnna2_nuQFtSWHr-En8Pjew6TLEIZRM1Snyckw1nvC4tBNkR1skUJcR6V4n0TtJ0dA7n0XQYQ', 'Ceulemans & Kiers (2006)', target = '_blank'),
                         #                                br(),
                         #                                tags$a(href='https://link.springer.com/article/10.3758/s13428-012-0238-5', 'Wilderjans, Ceulemans & Meers (2013)', target = '_blank')
                         #                                ),
                         #                       tabPanel("Authors",
                         #                                h2("Authors page"),
                         #                                br(),
                         #                                tags$a(href='https://www.universiteitleiden.nl/en/staffmembers/jeffrey-durieux#tab-1', 'Jeffrey Durieux', target = '_blank')),
                         #                       tabPanel("License",
                         #                                h3("Software is currently licensed under a:"),
                         #                                br(),
                         #                                tags$a(href='https://www.gnu.org/licenses/gpl-3.0.txt', 'GNU GPL version 3', target = '_blank')
                         #                                )
                         #          )#end navlistpanel
                         #
                         # )# end tabPanel


))# end shinyUI navbarPage
