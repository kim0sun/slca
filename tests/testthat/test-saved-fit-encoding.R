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
      expect_equal(range(attr(changed$mf, "y")), c(0, 1))
      legacy <- fit
      attr(legacy$mf, "y") <- attr(fit$mf, "y") + 1
      attr(legacy$mf, "yu") <- attr(fit$mf, "yu") + 1
      expect_identical(capture.output(summary(fit)), capture.output(summary(legacy)))
      expect_identical(reorder(fit, SMK_98 = c(1, 3, 2)),
                       reorder(legacy, SMK_98 = c(1, 3, 2)))
   }
})

test_that("legacy missing encodings and current fits give the same reorder output", {
   model <- slca(l[2] ~ y1 + y2 + y3)
   dat <- simulate(model, nsim = 60, seed = 15)$response
   dat$y1[1:5] <- NA
   fit <- estimate(model, dat)
   legacy <- fit
   attr(legacy$mf, "y") <- attr(fit$mf, "y") + 1
   attr(legacy$mf, "yu") <- attr(fit$mf, "yu") + 1
   expect_equal(reorder(legacy, l = 2:1), reorder(fit, l = 2:1))
   expect_identical(capture.output(summary(legacy)), capture.output(summary(fit)))
})

test_that("native likelihood rejects out-of-range response codes", {
   model <- slca(l[2] ~ y1 + y2 + y3)
   fit <- estimate(model, simulate(model, nsim = 20, seed = 15)$response)
   args <- fit$arg
   args$nlev <- unlist(args$nlev)
   args$ref <- args$ref - 1
   args$par <- fit$par
   args$y <- attr(fit$mf, "y")
   for (invalid in c(-2L, 2L, NA_integer_)) {
      args$y[1] <- invalid
      expect_error(do.call(calcModel, args[names(formals(calcModel))]),
                   "response code out of range")
   }
})
