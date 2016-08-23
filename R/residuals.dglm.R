#' @title residuals.dglm function
#' @name residuals.dglm
#' @author Robert W. Corty
#'
#' @param object The fitted dglm object whose residuals we want
#' @param ... additional parameters to residuals.glm
#'
#' @return
#' @export
#'
#' @examples
residuals.dglm <- function(object, ...) {
  return(stats::residuals.glm(object = object, ...))
}
