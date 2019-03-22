context(desc = 'Testing whether dglm matches glm when dformula is missing')

set.seed(27599)

effect <- function(obj, name) {
  unname(coef(object = obj)[name])
}

n <- 1e4
x1 <- runif(n = n)
x2 <- runif(n = n)

beta0 <- 10
beta1 <- 1
beta2 <- 1

mean_linpred <- beta0 + beta1*x1
disp_linpred <- beta2*x2

test_that(
  desc = 'Gaussian model with identity link',
  code = {
    
    y <- rnorm(n = n, mean = mean_linpred, sd = 1)
    d <- data.frame(y, x1, x2)
    
    fitted_glm <- glm(formula = y ~ x1,
                      data = d)
    
    fitted_dglm <- dglm(formula = y ~ x1,
                        data = d)
    
    expect_equal(object = effect(obj = fitted_dglm, name = '(Intercept)'),
                 expected = effect(obj = fitted_glm, name = '(Intercept)'))
    
    expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                 expected = effect(obj = fitted_glm, name = 'x1'))
    
    expect_equal(object = sigma(fitted_dglm),
                 expected = sigma(fitted_glm))
    
  }
)

test_that(
  desc = 'Gaussian model with log link',
  code = {
    
    y <- rnorm(n = n, mean = exp(mean_linpred), sd = 1)
    d <- data.frame(y, x1, x2)
    
    fitted_glm <- glm(formula = y ~ x1,
                      data = d,
                      family = stats::gaussian(link = 'log'))
    
    fitted_dglm <- dglm(formula = y ~ x1,
                        data = d,
                        family = stats::gaussian(link = 'log'))
    
    expect_equal(object = effect(obj = fitted_dglm, name = '(Intercept)'),
                 expected = effect(obj = fitted_glm, name = '(Intercept)'))
    
    expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                 expected = effect(obj = fitted_glm, name = 'x1'))
    
    expect_equal(object = sigma(fitted_dglm),
                 expected = sigma(fitted_glm))
    
  }
)

test_that(
  desc = 'Poisson model with log link',
  code = {
    y <- rpois(n = n, lambda = exp(mean_linpred))
    d <- data.frame(y, x1, x2)
    
    fitted_glm <- glm(formula = y ~ x1,
                      data = d,
                      family = stats::poisson(link = 'log'))
    
    fitted_dglm <- dglm(formula = y ~ x1,
                        data = d,
                        family = stats::poisson(link = 'log'))
    
    expect_equal(object = effect(obj = fitted_dglm, name = '(Intercept)'),
                 expected = effect(obj = fitted_glm, name = '(Intercept)'))
    
    expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                 expected = effect(obj = fitted_glm, name = 'x1'))
    
    expect_equal(object = sigma(fitted_dglm),
                 expected = sigma(fitted_glm))
  }
)
