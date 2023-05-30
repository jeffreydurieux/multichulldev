#' Plot for MultiCHull multiple complexities
#'
#' @param x object of class MultiCHullcom produced by \code{\link{MultiCHull}}
#' @param browser If FALSE plots are viewed in viewer panel. If TRUE, plots are viewed in a browser
#' @rdname MultiCHull
#' @export
#'
plot.MultiCHullcom <- function(x, browser=FALSE, ...){

    if(class(x) != 'MultiCHullcom'){
      stop('Input object not of class MultiCHullcom')
    }

    datalist <- x$CHulls

    plots <- list()
    for(i in 1:length(datalist)){
      plots[[i]] <- plot(datalist[[i]], type = 'interactive')
    }

    names(plots) <- paste('Complexity ', 1:length(plots), sep = '')

    if(browser == F){
      #options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
      viewer = paneViewer()
    }else{
      #options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
      viewer = browserViewer()
    }

    #app <- list(
    ui = pageWithSidebar(

      headerPanel("MultiChull different complexities"),

      sidebarPanel(

        selectInput('com', 'Complexity', names(plots))

      ),

      mainPanel(
        plotlyOutput('plot')
      )
    )
    server = function(input, output, session) {


      output$plot <- renderPlotly({

        plots[[input$com]]
      })
      session$onSessionEnded(stopApp)

    }
    #)

    runGadget(shinyApp(ui,server), viewer = viewer)
  }

