#' @export
#' @rdname MultiCHull
#' @param x An object of the type produced by \code{\link{MultiCHull}}
#' @param col Vector of \code{\link{colors}} used for plots
#' @param pch Vector of \code{\link{pch}} symbols
#' @param whichticks Model names of ticks that should be displayed
#' @param las Orientation of tick mark labels
#' @param plottype Type of plot. Either 'interactive' or 'static'
#' @param ... Additional arguments
#' @importFrom graphics legend
#' @importFrom graphics axis
plot.MultiCHull <-
  function(x, col = NULL, pch = NULL, whichticks = NULL, las = 2, plottype = 'interactive', ...){

    n_model <- nrow(x$OrigData)
    n_Multi <- ncol(x$OrigData)-1

    data <- x$OrigData
    order <- sort(data[,1],index.return=T)
    sorted <- cbind(data,1:n_model)
    sorted <- sorted[order$ix,]

    frq <- as.matrix(x$frq[order$ix,])
    mat <- x$st[order$ix,]
    tab <- x$tab[order$ix,]
    lim <- ceiling(max(mat,na.rm=T))

    if(plottype == 'interactive'){
      suppressPlotlyMessage <- function(p){
        suppressMessages(plotly_build(p))
      }

      xval1 <- which(tab == 1, arr.ind = T)[,1]
      xval2 <- which(tab == 2, arr.ind = T)[,1]
      xval3 <- which(tab == 3, arr.ind = T, useNames = T)[,1]

      xval1 <- xval2 <- xval3 <- yval1 <- yval2 <- yval3 <- numeric()
      for(j in 1:ncol(mat)){
        x1 <- which(tab[,j] ==1)
        x2 <- which(tab[,j] ==2)
        x3 <- which(tab[,j] ==3)
        xval1[j] <- ifelse(test = identical(x1, integer(0)), yes = NA, no = x1)
        xval2[j] <- ifelse(test = identical(x2, integer(0)), yes = NA, no = x2)
        xval3[j] <- ifelse(test = identical(x3, integer(0)), yes = NA, no = x3)
        yval1[j] <- mat[xval1[j],j]
        yval2[j] <- mat[xval2[j],j]
        yval3[j] <- mat[xval3[j],j]
      }

      titex <- paste('Model' , order$ix, sep = '')
      if(length(titex) <= 20){
        ax <- list( range = c(0, n_model),
                    dtick = 1,
                    title = 'Model',
                    ticktext = titex,
                    tickvals = c(1:n_model),

                    tickmode = 'array')
      }else{
        ax <- list(title = 'Model', showticklabels = F)
      }



      df <- data.frame(xval1, xval2, xval3, yval1, yval2, yval3)
      opti <- which(frq == max(frq, na.rm = TRUE))
      p <- plot_ly(data = df, x = ~xval1, y = ~yval1, type = 'scatter', name = 'best model', mode = 'markers', text = titex[xval1],hoverinfo = 'text') %>%
        add_trace(x = ~xval2, y = ~yval2, type = 'scatter',
                  name = '2nd best model', mode = 'markers',
                  text = titex[xval2],hoverinfo = 'text') %>%
        add_trace(x = ~xval3, y = ~yval3, type = 'scatter',
                  name = '3rd best model', mode = 'markers',
                  text = titex[xval3],hoverinfo = 'text') %>%
        add_segments(x = which.max(frq), xend = which.max(frq),
                     y = 0, yend = lim, line = list(color = 'lightgrey'), name = 'Most frequently selected model') %>%

        layout( xaxis = ax,
                yaxis = list( range = c(0, lim),
                              title = 'Scree test values'))

      suppressPlotlyMessage(p)

    }else if(plottype == 'static'){

      if (is.null(col)){
        col <- c(8,1,3,4,2)
      }
      if (is.null(pch)){
        pch <- c(15,17,19)
      }


      if (is.null(whichticks)){
        ticks <- 1:n_model
        ticknames <- rownames(data)[order$ix]
      } else {
        ticks <- sorted[whichticks,(n_Multi+2)]
        ticknames <- rownames(data)[ticks]
      }

      plot(1:n_model,seq(0,lim,length.out=n_model), ylab="scree test values", xlab="",type = "n",xaxt = "n")
      axis(1,ticks,ticknames,las=las)

      for (j in 1:n_Multi){
        if (n_Multi<21){
          points(which(!is.na(mat[,j])),mat[!is.na(mat[,j]),j],type="l",col=col[1])
        }
        points(which(tab[,j]==1),mat[which(tab[,j]==1),j],type="p",col=col[3],pch=pch[1])
        points(which(tab[,j]==2),mat[which(tab[,j]==2),j],type="p",col=col[4],pch=pch[2])
        points(which(tab[,j]==3),mat[which(tab[,j]==3),j],type="p",col=col[5],pch=pch[3])
      }
      opti <- which(frq == max(frq, na.rm = TRUE))
      for (i in 1:length(opti)){
        points(c(opti[i],opti[i]),c(-5,lim+5),type="l",col=col[2])
      }
      legend("topright", c("Most frequently selected model","best model","2nd best model","3rd best model"), col = col[2:5],lty=c(1,0,0,0),pch=c(NA,pch[1],pch[2],pch[3]))

    }else{
      stop("Plot type not correctly specified. Choose either 'interactive' or 'static' ")
    }



  }
