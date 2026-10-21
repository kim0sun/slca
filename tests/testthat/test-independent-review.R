test_that("unfinished saturated EM cannot supply absolute fit statistics", {
   d <- as.data.frame(rbind(rep(1, 16), rep(2, 16),
      matrix(rep(c(rep(1, 4), rep(NA, 12)), 10000),
             ncol = 16, byrow = TRUE)))
   expect_warning(s <- count_row(d, rep(2, 16), FALSE), "did not converge")
   expect_false(s$converged)
   expect_equal(s$iterations, 20000L)
   expect_true(is.na(s$loglik))
   expect_true(all(is.na(s$freq)))
   expect_true(all(is.na(s$theta)))
})

test_that("saturated solver reports status and likelihood at returned probabilities", {
   failed <- calcfreq(0:1, 2L, 1L, 10L, c(1L, 1L, 1L), 3L, 13L,
                      1e-12, 1L)
   expect_false(failed$converged)
   expect_true(is.na(failed$loglik))
   fitted <- calcfreq(0:1, 2L, 1L, 10L, c(1L, 1L, 1L), 3L, 13L,
                      1e-12, 500L)
   expect_true(fitted$converged)
   expect_lt(fitted$iterations, 500L)
   expect_equal(fitted$loglik,
      sum(log(fitted$theta)) + 10 * log(sum(fitted$theta[1:2])),
      tolerance = 1e-12)
})

test_that("regression aligns equal-size and full covariate frames by row identity", {
   m <- slca(l[2] ~ y1 + y2 + y3 + y4)
   dat <- simulate(m, nsim = 100, seed = 710)$response
   dat[1, ] <- NA
   fit <- estimate(m, dat)
   x <- data.frame(x = seq_len(100))
   reference <- regress(fit, l ~ x, data = x)
   reversed_full <- regress(fit, l ~ x, data = x[100:1, , drop = FALSE])
   reversed_used <- regress(fit, l ~ x, data = x[100:2, , drop = FALSE])
   expect_equal(reversed_full$coefficients, reference$coefficients)
   expect_equal(reversed_used$coefficients, reference$coefficients)

   positional <- x[2:100, , drop = FALSE]
   rownames(positional) <- NULL
   expect_error(regress(fit, l ~ x, data = positional), "row names")
   expect_equal(regress(fit, l ~ x, data = positional, align = "position")$coefficients,
                reference$coefficients)
   expect_error(regress(fit, l ~ x, data = x, align = "position"), "same number")

   duplicate <- x[2:100, , drop = FALSE]
   attr(duplicate, "row.names") <- rep("duplicate", 99)
   expect_error(regress(fit, l ~ x, data = duplicate), "unique")
   unknown <- x[2:100, , drop = FALSE]
   rownames(unknown) <- paste0("id", 2:100)
   expect_error(regress(fit, l ~ x, data = unknown), "row names")
})

test_that("unavailable saturated fits propagate to diagnostics but allow estimation", {
   local_mocked_bindings(calcfreq = function(mis, nrep, nmis, freq, xobs,
                                           nc, N, tol, max_iter) {
      list(freq = rep(NA_real_, nc), theta = rep(NA_real_, nc),
           loglik = NA_real_, converged = FALSE, iterations = max_iter)
   }, .package = "slca")
   m <- slca(l[2] ~ y1 + y2 + y3)
   dat <- simulate(m, nsim = 100, seed = 42)$response
   dat$y1[1] <- NA
   expect_warning(fit <- estimate(m, dat), "did not converge")
   expect_false(attr(fit$mf, "saturated.converged"))
   expect_equal(attr(fit$mf, "saturated.iterations"), 500L)
   expect_true(is.finite(as.numeric(logLik(fit))))
   expect_true(is.na(deviance(fit)))
   expect_true(is.na(gof(fit)$Gsq))
   expect_true(is.na(gof(fit, test = "chisq")$`Pr(>Chi)`))
   expect_error(gof(fit, test = "boot"), "available saturated")
})
