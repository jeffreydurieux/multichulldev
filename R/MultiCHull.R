#' Convex-Hull-Based Model Selection for multiple Samples
#'
#' Applying the \code{\link{CHull}} function on multiple samples of fit values at once, such as bootstrap samples.
#' @aliases MultiCHull.default
#' @param data Dataframe with complexity in 1st column and fit measures in next columns
#' @param bound Boundary of convex hull to inspect: \code{upper} or \code{lower}
#' @param PercentageFit Required proportion of increase in fit of a more complex model
#' @param type Either 'multifit' or 'multicom'
#' @return \item{st}{Dataframe with scree test values}
#' \item{tab}{Table which indicates the selected model in each sample}
#' \item{frq}{Table which indicates how often each model is selected}
#' \item{Origdata}{Original dataframe}
#' \item{Bound}{Boundary of convex hull that was requested}
#' \item{PercentageFit}{Requested proportion of increase in fit of a more complex model}
#' @section Details:
#' \subsection{\code{MultiCHull} function}{
#' \code{MultiCHull} applies the \code{\link{CHull}} code on multiple samples of fit values. To this end, the input parameter \code{data} consists of a dataframe with complexity values in the first column and fit values in the next columns. The different samples can for example be bootstrap samples, or fit values obtained with different random starts, or from different fit measures, etc. It is possible that in some samples no optimal solution can be found. This will generate a warning, which will include the sample number.
#'
#' Data frame \code{st} contains per sample the scree test values of the solutions that were found on the \code{upper} or \code{lower} bound of the hull (see also \code{\link{CHull}}). In each sample, the least and most complex model receive a 0 value. The other models have an NA value.
#' \code{tab} is also a dataframe, which indicates per sample the top three of optimal models (indicated by a 1, 2 and 3). The other models have an NA value. Finally, in \code{frq} the frequencies are shown for each model of being selected as the optimal model.
#' }
#' \subsection{Plot function}{
#' Applying the method \code{plot()} on output of \code{\link{MultiCHull}} yields a plot with the models on the x-axis, ordered by increasing complexity. By default, all model names are shown as perpendicular labels on the x-axis, but one can choose to display specific model names only (e.g., \code{whichticks=c("model13","model20")}). The tick mark labels can be made horizontal, by putting parameter \code{las} to 0.
#'
#' Solid lines (only shown in case of 20 or less samples) indicate the scree test values per sample, and symbols indicate the top three of the models per sample. The symbols can be adjusted with the parameter \code{pch} and the colors with \code{col}. The model (or multiple models) that is selected most often across samples, is indicated with a horizontal line.
#' }
#' @export
#' @seealso \code{\link{CHull}}
#' @keywords models
#' @examples
#' data <- cbind(c(305,456,460,607,612,615,758,764,768,770,909,916,921,924),
#' c(152,89,79,71,57,57,64,49,47,47,60,41,39,39))
#' test <- array(rnorm(14*20,sd=2.5),c(14,20))
#' for (i in 1:20){
#'   data <- cbind(data,data[,2]+test[,i])
#' }
#'
#' output <- MultiCHull(data)
#' summary(output)
#' plot(output)
#'
MultiCHull <-
  function(data, bound = "lower", PercentageFit = 1, type = 'multifit'){
    UseMethod("MultiCHull")
  }
