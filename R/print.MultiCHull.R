#' @export
#' @rdname MultiCHull
print.MultiCHull <-
  function(x, ...){
    cat("Frequency of selection by CHull\n")
    tabl <- x$frq
    print(tabl)
    cat("\nScree test values\n")
    if (ncol(x$st)<12){
      print(x$st)
    } else {
      index <- which(apply(x$st,1,na.rm=T,sum)>0)
      print(round(x$st[index,],2))
    }
  }
