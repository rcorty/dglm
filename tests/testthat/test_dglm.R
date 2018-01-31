context(desc = 'Testing dglm')

set.seed(27599)

n <- 1e4
x1 <- runif(n = n)
x2 <- runif(n = n)

mean_linpred <- 2 + x1 - 0.5*x2
disp_linpred <- -0.5*x1 + x2

effect <- function(obj, name) {
  unname(coef(object = obj)[name])
}


test_that(desc = 'mean: normal w identity link -- disp: gamma w log link',
          code = {
            
            
            mu <- mean_linpred
            y <- rnorm(n = n, mean = mu, sd = sqrt(exp(disp_linpred)))
            
            d <- data.frame(x1, x2, y)
            
            fitted_glm <- glm(formula = y ~ x1 + x2,
                              data = d,
                              family = stats::gaussian(link = 'identity'))

            fitted_dglm <- dglm(formula = y ~ x1 + x2,
                                dformula = ~ x1 + x2,
                                data = d,
                                family = stats::gaussian(link = 'identity'))
            
            expect_equal(object = effect(obj = fitted_glm, name = 'x1'),
                         expected = 1,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_glm, name = 'x2'),
                         expected = -0.5,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                         expected = 1,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm, name = 'x2'),
                         expected = -0.5,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x1'),
                         expected = -0.5,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x2'),
                         expected = 1,
                         tolerance = 0.1)
            
          }
)


test_that(desc = 'mean: normal w log link -- disp: gamma w log link',
          code = {
           
            mu <- exp(mean_linpred)
            y <- rnorm(n = n, mean = mu, sd = sqrt(exp(disp_linpred)))
            
            d <- data.frame(x1, x2, y)
            
            fitted_glm <- glm(formula = y ~ x1 + x2,
                              data = d,
                              family = stats::gaussian(link = 'log'),
                              mustart = rep(mean(y), n))
            
            fitted_dglm <- dglm(formula = y ~ x1 + x2,
                                dformula = ~ x1 + x2,
                                data = d,
                                family = stats::gaussian(link = 'log'),
                                mustart = rep(mean(y), n))
                                
            expect_equal(object = effect(obj = fitted_glm, name = 'x1'),
                         expected = 1,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_glm, name = 'x2'),
                         expected = -0.5,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
                         expected = 1,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm, name = 'x2'),
                         expected = -0.5,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x1'),
                         expected = -0.5,
                         tolerance = 0.1)
            
            expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x2'),
                         expected = 1,
                         tolerance = 0.1)
            
          }
)


# test_that(desc = 'mean: normal w inverse link -- disp: gamma w log link',
#           code = {
#             
#             mu <- 1/mean_linpred
#             y <- rnorm(n = n, mean = mu, sd = sqrt(exp(disp_linpred)))
#             
#             d <- data.frame(x1, x2, y)
#             
#             fitted_glm <- glm(formula = y ~ x1 + x2,
#                               data = d,
#                               family = stats::gaussian(link = 'inverse'),
#                               mustart = y)
#             
#             fitted_dglm <- dglm(formula = y ~ x1 + x2,
#                                 dformula = ~ x1 + x2,
#                                 data = d,
#                                 family = stats::gaussian(link = 'inverse'))
#             
#             expect_equal(object = effect(obj = fitted_glm, name = 'x1'),
#                          expected = 1,
#                          tolerance = 0.1)
#             
#             expect_equal(object = effect(obj = fitted_glm, name = 'x2'),
#                          expected = -0.5,
#                          tolerance = 0.1)
#             
#             expect_equal(object = effect(obj = fitted_dglm, name = 'x1'),
#                          expected = 1,
#                          tolerance = 0.1)
#             
#             expect_equal(object = effect(obj = fitted_dglm, name = 'x2'),
#                          expected = -0.5,
#                          tolerance = 0.1)
#             
#             expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x1'),
#                          expected = -0.5,
#                          tolerance = 0.1)
#             
#             expect_equal(object = effect(obj = fitted_dglm$dispersion.fit, name = 'x2'),
#                          expected = 1,
#                          tolerance = 0.1)
#             
#           }
# )
