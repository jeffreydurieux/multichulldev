#' A Generic Convex-Hull-Based Model Selection Method
#'
#' Given a set of models for which a measure of model (mis)fit and model complexity is provided, CHull determines the models that are located on the boundary of the convex hull and selects an optimal model by means of the scree test values.
#' @aliases CHull.default
#' @param data Dataframe with complexity in 1st column and fit measures in 2nd column
#' @param bound Boundary of convex hull to inspect: \code{upper} or \code{lower}
#' @param PercentageFit Required proportion of increase in fit of a more complex model
#'
#' @return \item{Solution}{Dataframe with selected models}
#' \item{Hull}{Dataframe with all models on hull boundary and their \emph{st} value}
#' \item{Origdata}{Original dataframe}
#' \item{Bound}{Boundary of convex hull that was requested}
#' \item{PercentageFit}{Requested proportion of increase in fit of a more complex model}
#' @section Details:
#' The CHull method (Wilderjans, Ceulemans, & Meers, 2013) can be used for selecting a model by comparing model complexities (1st column of the input parameter \code{data}) and fit values (2nd column).
#'
#' In a first step, only the best model (or one of the best, if some models have an equal fit) is retained per complexity. This should be a model with a high fit value if the fit measure indicates goodness-of-fit  (\code{bound="upper"}) and a low fit value if it indicates badness-of-fit (\code{bound="upper"}). A warning will be generated if the sign of the correlation between complexity and fit is counterintuitive in this regard.
#'
#' In a second step, the remaining models are ordered (increasingly) on the basis of their complexity value.
#'
#' In Step 3, models are excluded that have a higher complexity but a worse (or equal) fit when compared to the other models. This procedure is repeated until a monotonical increase (\code{bound="upper"}) or decrease (\code{bound="lower"})  is reached. If less than 3 models remain, the method generates the error warning that not enough data points are available for computing the convex hull and the procedure stops.
#'
#' In Step 4, it is determined which models lie on the \code{upper} or \code{lower} boundary of the convex hull. Models are discarded if the improvement in fit, compared to a less complex model, is less than \code{PercentageFit} (default: \code{PercentageFit=.01} ). The remaining models are returned in \code{Hull}.
#'
#' Step 5 consists of computing the scree test values (\code{st}) for each remaining model. This is, however, not possible for the most simple and most complex model, and those models will therefore never be selected as the optimal solution, except when these are the only models that remain after the previous step.
#'
#' In Step 6, the model with highest scree test value is selected, and finally, in Step 7, also models are selected that were excluded in the first step, but that have the same complexity and fit value as the selected model. All selected models end up in \code{Solution}.
#' @export
#' @importFrom igraph convex_hull
#' @importFrom graphics plot points text
#' @importFrom stats cor
#' @references \cite{Wilderjans, T. F., Ceulemans, E., & Meers, K. (2013). CHull: A generic convex hull based model selection method. Behavior Research Methods, 45, 1-15.}
#'
#' \cite{Ceulemans, E., & Kiers, H. A. L. (2006). Selecting among three-mode principal component models of different types and complexities: A numerical convex hull based method. British Journal of Mathematical & Statistical Psychology, 59, 133-150.}
#' @keywords models
#' @examples
#' complexity.fit <- cbind(c(305,456,460,607,612,615,758,764,768,770,909,916,921,924),
#' c(152,89,79,71,57,57,64,49,47,47,60,41,39,39))
#'
#' output <- CHull(complexity.fit)
#' plot(output)
#' print(output)
#' summary(output)
#'
CHull <-
  function(data, bound = "lower", PercentageFit = 1){
    UseMethod("CHull")
  }
