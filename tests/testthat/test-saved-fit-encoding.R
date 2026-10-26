test_that("saved NLSY fits have current metadata without claiming a new fit", {
   for (fit in list(nlsy_jlcpa, nlsy_jlta)) {
      expect_setequal(names(fit$control), names(slcaControl()))
      expect_false(fit$control$hessian)
      expect_identical(dim(fit$hessian), rep(length(fit$par), 2L))
      expect_true(all(is.na(fit$hessian)))
      expect_identical(fit$convergence, c(EM = TRUE, nlm = NA))
      expect_true(isTRUE(attr(fit$mf, "saturated.converged")))
      expect_equal(fit$skeleton, get_frame(fit$model, fit$arg, fit$mf))
   }
})

test_that("saved NLSY fits can be reordered without changing their distributions", {
   for (fit in list(nlsy_jlcpa, nlsy_jlta)) {
      expect_identical(attr(fit$mf, "y"),
                       encode_response(fit$mf, fit$model$measure$indicator))
      expect_identical(attr(fit$mf, "yu"), encode_response(
         attr(fit$mf, "y_unique"), fit$model$measure$indicator))
      original <- fit
      changed <- reorder(fit, SMK_98 = c(1, 3, 2),
                         DRK_98 = c(3, 2, 1), MRJ_98 = c(3, 1, 2))
      raw <- fit$mf
      raw[] <- lapply(names(raw), function(nm)
         factor(raw[[nm]], levels = seq_along(attr(fit$mf, "levels")[[nm]]),
                labels = attr(fit$mf, "levels")[[nm]]))
      expect_equal(predict(changed, type = "posterior"),
                   predict(changed, raw, type = "posterior"), tolerance = 1e-10)
      expect_true(all(is.finite(unlist(changed$posterior))))
      expect_equal(logLik(changed), logLik(fit))
      expect_equal(sort(changed$par), sort(fit$par))
      restored <- reorder(changed, SMK_98 = c(1, 3, 2),
                          DRK_98 = c(3, 2, 1), MRJ_98 = c(2, 3, 1))
      expect_equal(restored$par, fit$par)
      expect_equal(restored$posterior, fit$posterior, tolerance = 1e-7)
      expect_identical(fit, original)
      expect_equal(range(attr(changed$mf, "y")), c(1, 2))
      zero_based <- fit
      attr(zero_based$mf, "y") <- attr(fit$mf, "y") - 1
      attr(zero_based$mf, "yu") <- attr(fit$mf, "yu") - 1
      expect_identical(capture.output(summary(fit)), capture.output(summary(zero_based)))
      expect_identical(reorder(fit, SMK_98 = c(1, 3, 2)),
                       reorder(zero_based, SMK_98 = c(1, 3, 2)))
   }
})

test_that("intervening zero-based caches do not change fitted-object methods", {
   model <- slca(l[2] ~ y1 + y2 + y3)
   dat <- simulate(model, nsim = 60, seed = 15)$response
   dat$y1[1:5] <- NA
   fit <- estimate(model, dat)
   zero_based <- fit
   attr(zero_based$mf, "y") <- attr(fit$mf, "y") - 1
   attr(zero_based$mf, "yu") <- attr(fit$mf, "yu") - 1
   expect_equal(reorder(zero_based, l = 2:1), reorder(fit, l = 2:1))
   expect_identical(capture.output(summary(zero_based)), capture.output(summary(fit)))
   expect_equal(estimate(zero_based), estimate(fit))
   expect_equal(predict(zero_based, dat, type = "posterior"),
                predict(fit, dat, type = "posterior"))
   expect_equal(unname(as.matrix(slca::compare(fit, zero_based))),
                unname(as.matrix(slca::compare(fit, fit))))
   expect_true(-1 %in% attr(zero_based$mf, "y"))
   expect_true(0 %in% attr(fit$mf, "y"))
})

test_that("zero labels are observed and only NA maps to the missing code", {
   model <- slca(l[2] ~ y1 + y2 + y3)
   dat <- data.frame(
      y1 = factor(c(0:3, NA, 0), levels = 0:3),
      y2 = factor(c(1, 2, NA, 1, 2, 1), levels = 1:2),
      y3 = factor(c(1, 2, 3, NA, 1, 3), levels = 1:3)
   )
   mf <- proc_data(dat, model$model, FALSE)
   expected <- as.matrix(mf)
   expected[is.na(expected)] <- 0
   expect_identical(attr(mf, "y"), as.vector(t(expected)))
   expect_identical(attr(mf, "levels")$y1, as.character(0:3))
   expect_equal(sum(attr(mf, "y") == 0), sum(is.na(dat)))
   expect_identical(attr(proc_data2(dat, model$model, FALSE), "y"),
                    attr(mf, "y"))
   expect_true(all(attr(mf, "yu") %in% 1:4))

   a <- arg_mf(model$model, model$arg, mf, NULL)
   a$par <- log(c(.6, .4,
                  .1, .2, .3, .4, .3, .7, .2, .3, .5,
                  .4, .3, .2, .1, .8, .2, .5, .3, .2))
   a$y <- attr(mf, "y")
   a$nlev <- unlist(a$nlev)
   a$ref <- a$ref - 1
   actual <- do.call(calcModel, a[names(formals(calcModel))])
   rho <- matrix(exp(a$par[-(1:2)]), ncol = 2)
   joint <- matrix(rep(c(.6, .4), each = nrow(dat)), ncol = 2)
   starts <- c(0, 4, 6)
   for (j in seq_along(dat)) {
      code <- as.integer(dat[[j]])
      obs <- !is.na(code)
      joint[obs, ] <- joint[obs, ] * rho[starts[j] + code[obs], , drop = FALSE]
   }
   expect_equal(as.numeric(actual$ll), log(rowSums(joint)), tolerance = 1e-12)
   expect_equal(t(matrix(exp(actual$post), nrow = 2)),
                joint / rowSums(joint), tolerance = 1e-12)
})

test_that("native likelihood rejects out-of-range response codes", {
   model <- slca(l[2] ~ y1 + y2 + y3)
   fit <- estimate(model, simulate(model, nsim = 20, seed = 15)$response)
   args <- fit$arg
   args$nlev <- unlist(args$nlev)
   args$ref <- args$ref - 1
   args$par <- fit$par
   args$y <- attr(fit$mf, "y")
   for (invalid in c(-1L, 3L, NA_integer_)) {
      args$y[1] <- invalid
      expect_error(do.call(calcModel, args[names(formals(calcModel))]),
                   "response code out of range")
   }
})
