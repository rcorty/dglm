#' @title New implementation of dglm
#'
#' @param mean_formula
#' @param disp_formula
#' @param mean_family
#' @param disp_family
#' @param data 
#' @param weights 
#' @param subset 
#' @param na.action 
#' @param start 
#' @param etastart 
#' @param mustart 
#' @param offset 
#' @param control 
#' @param model 
#' @param method 
#' @param x 
#' @param y 
#' @param contrasts 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
new_dglm <- function(mean_formula, 
                     disp_formula,
                     mean_family = stats::gaussian(link = identity),
                     disp_family = stats::Gamma(link = log),
                     data = sys.parent(),
                     method = c('ml', 'reml'),
                     # weights,
                     # subset, 
                     # na.action, 
                     # start = NULL,
                     # etastart, 
                     # mustart, 
                     # offset,
                     # control = list(...),
                     # model = TRUE, 
                     # method = "glm.fit",
                     # x = FALSE, 
                     # y = TRUE,
                     # contrasts = NULL, 
                     ...)
{
  call <- match.call()
  
  ## family checking as per stats::glm
  if(is.character(mean_family))
    mean_family <- get(mean_family, mode = "function", envir = parent.frame())
  if(is.function(mean_family)) mean_family <- mean_family()
  if(is.null(mean_family$family)) {
    print(mean_family)
    stop("'mean_family' not recognized")
  }
  if(is.character(disp_family))
    disp_family <- get(disp_family, mode = "function", envir = parent.frame())
  if(is.function(disp_family)) disp_family <- mean_family()
  if(is.null(disp_family$family)) {
    print(disp_family)
    stop("'disp_family' not recognized")
  }
  
  # how I would do it - RWC
  # stopifnot(class(mean_family) == 'family')
  # stopifnot(class(disp_family) == 'family')
  
  # ## extract x, y, etc from the model formula and frame as per stats::glm
  # ## I don't understand the logic here, so commenting out and doing a way I understand - RWC
  # mf <- match.call(expand.dots = FALSE)
  # m <- match(c("formula", "data", "subset", "weights", "na.action",
  #              "etastart", "mustart", "offset"), names(mf), 0L)
  # mf <- mf[c(1L, m)]
  # mf$drop.unused.levels <- TRUE
  # ## need stats:: for non-standard evaluation
  # mf[[1L]] <- quote(stats::model.frame)
  # mf <- eval(mf, parent.frame())
  # if(identical(method, "model.frame")) return(mf)
  
  # extract mean_X, mean_y, and disp_X
  mean_X <- model.matrix(object = mean_formula, data = data)
  mean_y <- model.response(data = model.frame(formula = mean_formula, data = data))
  disp_X <- model.matrix(object = disp_formula, data = data)
  
  
  # # validate 'method' per stats::glm
  # if (!is.character(method) && !is.function(method))
  #   stop("invalid 'method' argument")
  
  # validate 'method' per RWC
  method <- tolower(x = method)
  method <- match.arg(arg = method)
  
  # don't know what this is for -RWC
  mt <- attr(mf, "terms") # allow model.frame to have updated it
  
  # above comment said that block was extracting y...
  # doing it again here? -RWC
  Y <- model.response(mf, "any") # e.g. factors are allowed
  ## avoid problems with 1D arrays, but keep names
  if(length(dim(Y)) == 1L) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if(!is.null(nm)) names(Y) <- nm
  }
  
  # now dealing with X "again"? -RWC
  ## null model support
  X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts) else matrix(,NROW(Y), 0L)
  ## avoid any problems with 1D or nx1 arrays by as.vector.
  
  # had to stop here for the day -RWC
  weights <- as.vector(model.weights(mf))
  if(!is.null(weights) && !is.numeric(weights))
    stop("'weights' must be a numeric vector")
  ## check weights and offset
  if( !is.null(weights) && any(weights < 0) )
    stop("negative weights not allowed")
  
  offset <- as.vector(model.offset(mf))
  if(!is.null(offset)) {
    if(length(offset) != NROW(Y))
      stop(gettextf("number of offsets is %d should equal %d (number of observations)", length(offset), NROW(Y)), domain = NA)
  }
  ## these allow starting values to be expressed in terms of other vars.
  mustart <- model.extract(mf, "mustart")
  etastart <- model.extract(mf, "etastart")
  
  ## We want to set the name on this call and the one below for the
  ## sake of messages from the fitter function
  fit <- eval(call(if(is.function(method)) "method" else method,
                   x = X, y = Y, weights = weights, start = start,
                   etastart = etastart, mustart = mustart,
                   offset = offset, family = family, control = control,
                   intercept = attr(mt, "intercept") > 0L))
  
  ## This calculated the null deviance from the intercept-only model
  ## if there is one, otherwise from the offset-only model.
  ## We need to recalculate by a proper fit if there is intercept and
  ## offset.
  ##
  ## The glm.fit calculation could be wrong if the link depends on the
  ## observations, so we allow the null deviance to be forced to be
  ## re-calculated by setting an offset (provided there is an intercept).
  ## Prior to 2.4.0 this was only done for non-zero offsets.
  if(length(offset) && attr(mt, "intercept") > 0L) {
    fit2 <-
      eval(call(if(is.function(method)) "method" else method,
                x = X[, "(Intercept)", drop=FALSE], y = Y,
                weights = weights, offset = offset, family = family,
                control = control, intercept = TRUE))
    ## That fit might not have converged ....
    if(!fit2$converged)
      warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
    fit$null.deviance <- fit2$deviance
  }
  if(model) fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  if(x) fit$x <- X
  if(!y) fit$y <- NULL
  fit <- c(fit, list(call = call, formula = formula,
                     terms = mt, data = data,
                     offset = offset, control = control, method = method,
                     contrasts = attr(X, "contrasts"),
                     xlevels = .getXlevels(mt, mf)))
  class(fit) <- c(fit$class, c("glm", "lm"))
  fit
}
