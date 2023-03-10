#' @export
#' @rdname CHull
#' @param object An object of the type produced by \code{\link{CHull}}
summary.CHull <-
  function(object, ...){
    cat("SETTINGS BY USER:\n")
    cat(paste(paste("Optimalization: ",object$Bound,sep="")," bound\n",sep=""))
    cat(paste(paste("Required improvement in fit:",as.character(object$PercentageFit)),"%\n",sep=""))
    cat(paste(paste("Number of considered models:",as.character(nrow(object$OrigData))),"\n\n"))
    cat("RESULTS:\n")
    nSol <- nrow(object$Solution)
    cat(paste("Number of selected models:",as.character(nSol)))
    if (nSol>1){
      cat("\n\nSELECTED MODELS:\n")
    } else {
      cat("\n\nSELECTED MODEL:\n")
    }
    print(object$Solution)
    cat(paste(paste("\nALL MODELS ON ",object$Bound,sep="")," BOUND:\n",sep=""))
    print(object$Hull)
    cat("\n\nORIGINAL MODELS\n")
    print(object$OrigData)
  }
