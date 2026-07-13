test_that("parenthesis/bracket identical", {
   expect_equal(slca(l[3] ~ a + b + c), slca(l(3) ~ a + b + c))
})
test_that("stop without number works", {
   expect_error(slca(l ~ a + b + c))
})
test_that("stop if there are multiple parents", {
   expect_error(slca(l[3] ~ a + b + c,
                     l2[3] ~ a + b + c))
})

test_that("estimate returns a scalar convergence flag", {
   m <- slca(lc[2] ~ y1 + y2 + y3)
   par <- c(
      .55, .45,
      .85, .15, .80, .20, .75, .25,
      .20, .80, .25, .75, .30, .70
   )
   dat <- simulate(m, nsim = 80, seed = 1, parm = par)$response

   for (method in c("em", "nlm", "hybrid")) {
      fit <- estimate(
         m, dat, method = method,
         control = slcaControl(em.iterlim = 200, nlm.iterlim = 100)
      )
      expect_type(fit$convergence, "logical")
      expect_length(fit$convergence, 1)
   }
})
