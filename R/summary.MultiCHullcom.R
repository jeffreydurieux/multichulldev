#' @export
#' @rdname MultiCHull
#' @param object An object of the type produced by \code{\link{MultiCHull}}
summary.MultiCHullcom <- function(x, ...){
  mods <- lapply(seq_along(x$CHulls),
                 function(anom)x$CHulls[[anom]]$Solution )
  names(mods) <- paste('Complexity', 1:length(mods),sep = '')
  tab <- table(sapply(mods, rownames) )

  cat('Selected model per complexity: \n')
  print(mods)

  cat('Frequency table of selected models: \n' )
  print(tab)
}
