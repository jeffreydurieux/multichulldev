library(plotly)
#library(multichull)
library(multichulldev)
library(shinythemes)
library(DT)
shinyServer(function(input , output, session){


  ############# Load data operations

  #load files
  # function to load data
  data <- reactive({
    file <- input$loadfile
    if(is.null(file)){
      return(NULL)
    }
    dat <- read.csv(file = file$datapath, header = T, sep = ",")
    if(ncol(dat) == 2){
      dat <- cbind("Complexity" = dat[,1], "(mis-) fit" = dat[,2])
      dat <- cbind(model = 1:nrow(dat),dat)
    }else{
      #name <- c('Complexity', rep('(mis-) fit', ncol(dat)-1))
      #colnames(dat) <- name

      dat <- cbind(model = 1:nrow(dat),dat)
    }

    return(dat)
  })

  output$ui <- DT::renderDataTable({
    DT::datatable(data(), rownames = TRUE, filter = 'top', editable = TRUE)
    #dat <- cbind(1:nrow(dat), dat)
    #dat
  })
  # output$ui <- renderTable({
  #   dat <- data()
  #   dat
  # })# end render table


  ############# Chull options

  # start chull
  but1 = reactiveValues(but1=FALSE)
  but2 = reactiveValues(but2=FALSE)

  observeEvent(input$stChull,
               isolate({but1$but1=TRUE
               but2$but2=FALSE
               }))

  observeEvent(input$stMultiChull,
               isolate({but1$but1=FALSE
               but2$but2=TRUE
               }))

  chullstart <- eventReactive(input$stChull, {
    multichulldev::CHull(data()[,-1],input$bound,input$PercentageFit)
  })

  # start multichull

  Multichullstart <- eventReactive(input$stMultiChull, {
    multichulldev::MultiCHull(data()[,-1],input$boundM,input$PercentageFitM, input$typeMC)
    #x <- data()
    #updateSelectInput(session, 'cominput',
    #                  label = paste("Select input label"),
    #                  choices = 1:ncol(x)-1
    #                   )

  })

  ############# Results

  ### summary tables
  output$print <- renderPrint({
    if(but1$but1)
      print(chullstart())
    else if(but2$but2)
      print(Multichullstart())
    else
      return()
  })
  output$summary <- renderPrint({
    if(but1$but1)
      summary(chullstart())
    else if(but2$but2)
      summary(Multichullstart())
    else
      return()

  })


  ### plotly
  output$plotly <- renderPlotly({

    if(but1$but1){
      ch <- chullstart()
      plot(ch, plottype = 'interactive')
    }else if(but2$but2){
     mch <- Multichullstart()
     if(class(mch) == 'MultiCHullcom'){
       # do nothing
     }else{
       plot(mch, plottype = 'interactive')
     }

    }else
      return()
  })

  ### plotly
  output$plotlycom <- renderPlotly({

    if(but1$but1){
     # ch <- chullstart()
     #  plot(ch, plottype = 'interactive')
    }else if(but2$but2){
      mchcom <- Multichullstart()
      if(class(mchcom) == 'MultiCHullcom'){
        # do nothing
      }else{
        plot(mchcom, plottype = 'interactive')
      }

    }else
      return()
  })



})# shinyServer
