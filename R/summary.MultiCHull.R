#' @export
#' @rdname MultiCHull
#' @param object An object of the type produced by \code{\link{MultiCHull}}
summary.MultiCHull <-
  function(object, ...){
    frq <- object$frq
    n_Multi <- ncol(object$OrigData)-1
    cat("SETTINGS BY USER:\n")
    cat(paste(paste("Optimalization: ",object$Bound,sep="")," bound\n",sep=""))
    cat(paste(paste("Required improvement in fit:",as.character(object$PercentageFit)),"%\n",sep=""))
    cat(paste(paste("Number of considered models:",as.character(nrow(object$OrigData))),"\n"))
    cat(paste(paste("Number of samples:",as.character(n_Multi)),"\n\n"))
    cat("SELECTED MODELS:\n")
    order <- sort(frq[,1],index.return=T,decreasing=T)
    sorted <- frq[order$ix,]
    index <- which(sorted>0)
    index2 <- order$ix[index]
    selname <- rownames(frq)[index2]
    sel <- frq[index2,]
    for (i in 1:length(sel)){
      cat(paste(selname[i]," is selected in ",as.character(sel[i])," (",as.character(round(sel[i]*100/n_Multi)),"%) of the samples\n",sep=""))
    }
    cat("\nSCREE TEST VALUES\n")
    if (n_Multi<12){
      print(round(object$st),2)
    } else {
      index <- which(apply(object$st,1,na.rm=T,sum)>0)
      print(round(object$st[index,]),2)
    }


  }
