# Public issue #1: constrained categorical indicators must share level vectors.
test_that("constrained indicators validate labels and order, not only counts", {
   m <- slca(L1[2] ~ x1 + x2, L2[2] ~ y1 + y2,
             L1 ~ L2, constraints = c("L1", "L2"))
   dat <- data.frame(x1 = c(1, 2, 3, 1, 2, 3),
                     x2 = c(1, 2, 3, 3, 2, 1),
                     y1 = c(1, 2, 1, 2, 1, 2),
                     y2 = c(1, 2, 3, 1, 2, 3))
   check <- function(d) {
      mf <- proc_data(d, m$model, na.rm = FALSE)
      arg_mf(m$model, m$arg, mf, fix2zero = NULL)
   }
   expect_error(check(dat), "levels for variables constrained")
   dat[] <- lapply(dat, factor, levels = 1:3)
   expect_type(check(dat), "list")
   labels <- dat
   levels(labels$y1) <- c("a", "b", "c")
   expect_error(check(labels), "levels for variables constrained")
   reversed <- dat
   reversed$y1 <- factor(reversed$y1, levels = 3:1)
   expect_error(check(reversed), "levels for variables constrained")
})

test_that("hybrid bootstrap requires convergence of the returned nlm fit", {
   est <- list(ll = -10, conv = c(EM = TRUE, nlm = FALSE))
   expect_false(bootstrap_converged(est, "hybrid"))
   expect_true(bootstrap_converged(est, "em"))
   est$conv <- c(EM = FALSE, nlm = TRUE)
   expect_true(bootstrap_converged(est, "hybrid"))
   expect_true(bootstrap_converged(est, "nlm"))
   est$ll <- Inf
   expect_false(bootstrap_converged(est, "hybrid"))
})

test_that("absolute bootstrap uses a larger default but respects explicit limits", {
   expect_equal(formals(gof.slcafit)$maxiter, 5000)
   expect_equal(formals(compare)$maxiter, 1000)
   model <- slca(l[2] ~ y1+y2+y3)
   fit <- estimate(model, simulate(model, nsim = 50, seed = 7)$response)
   seen <- list()
   local_mocked_bindings(estModel = function(method, control, par, mf, arg) {
      seen[[length(seen) + 1L]] <<- control
      list(par = par, ll = -1, conv = c(EM = TRUE, nlm = NA))
   }, .package = "slca")
   gof(fit, test = "boot", nboot = 1)
   gof(fit, test = "boot", nboot = 1, maxiter = 100)
   expect_equal(seen[[1]]$em.iterlim, 5000)
   expect_equal(seen[[1]]$nlm.iterlim, 5000)
   expect_equal(seen[[2]]$em.iterlim, 100)
   expect_equal(seen[[2]]$nlm.iterlim, 100)
   expect_equal(seen[[1]]$em.tol, 1e-6)
})
