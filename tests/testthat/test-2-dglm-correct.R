context(desc = 'Testing whether estimates from dglm are correct')

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
  desc = 'mean: normal w identity link -- disp: gamma w log link',
  code = {
    
    mu <- mean_linpred
    y <- rnorm(n = n, 
               mean = mu, 
               sd = sqrt(exp(disp_linpred)))

    fitted_dglm <- dglm(formula = y ~ x1,
                        dformula = ~ x2,
                        data = data.frame(y, x1, x2))
    
    expect_equal(object = effect(obj = fitted_dglm, name = '(Intercept)'),
                 expected = beta0,
                 tol = 0.1)
    
    expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                 expected = beta1,
                 tol = 0.1)
    
    expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x2'),
                 expected = beta2,
                 tol = 0.1)

  }
)


test_that(desc = 'mean: normal w log link -- disp: gamma w log link',
          code = {
            
            y <- rnorm(n = n,
                       mean = exp(mean_linpred), 
                       sd = sqrt(exp(disp_linpred)))
            
            fitted_dglm <- dglm(formula = y ~ x1 + x2,
                                dformula = ~ x1 + x2,
                                data = data.frame(x1, x2, y),
                                family = stats::gaussian(link = 'log'))
            
            expect_equal(object = effect(obj = fitted_dglm, name = '(Intercept)'),
                         expected = beta0,
                         tol = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                         expected = beta1,
                         tol = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x2'),
                         expected = beta2,
                         tol = 0.1)
            
          }
)


test_that(desc = 'mean: normal w inverse link -- disp: gamma w log link',
          code = {
            
            y <- rnorm(n = n, mean = 300/mean_linpred, sd = sqrt(exp(disp_linpred)))

            fitted_dglm <- dglm(formula = y ~ x1 + x2,
                                dformula = ~ x1 + x2,
                                data = data.frame(y, x1, x2),
                                family = stats::gaussian(link = 'inverse'))

            expect_equal(object = 300*effect(obj = fitted_dglm, name = '(Intercept)'),
                         expected = beta0,
                         tol = 0.1)

            expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                         expected = beta1,
                         tol = 0.1)

            expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x2'),
                         expected = beta2,
                         tol = 0.1)

          }
)
