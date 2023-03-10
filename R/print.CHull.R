#' @export
#' @rdname CHull
#' @param x An object of the type produced by \code{\link{CHull}}
print.CHull <-
  function(x, ...){
    cat(paste(paste("Convex hull (",x$Bound,sep="")," bound)\n",sep=""))
    cat("\nSelected model(s):\n")
    print(x$Solution)
    cat(paste(paste("\n\nAll models on ",x$Bound,sep="")," bound:\n",sep=""))
    print(x$Hull)
  }