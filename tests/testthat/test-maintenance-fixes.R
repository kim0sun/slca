maintenance_data <- function() {
   model <- slca(l[2] ~ y1 + y2 + y3 + y4)
   data <- simulate(model, nsim = 300, seed = 710,
      parm = c(.6,.4,.8,.2,.7,.3,.9,.1,.65,.35,
               .2,.8,.3,.7,.15,.85,.25,.75))$response
   list(model = model, data = data)
}

test_that("continued estimation retains omitted zero constraints", {
   input <- maintenance_data()
   fit <- estimate(input$model, input$data, fix2zero = 3)
   ctrl <- slcaControl(em.iterlim = 1)
   implicit <- estimate(fit, control = ctrl)
   explicit <- estimate(fit, fix2zero = fit$fix2zero, control = ctrl)
   expect_identical(implicit, explicit)
   expect_identical(implicit$fix2zero, fit$fix2zero)
   expect_equal(implicit$arg$df, fit$arg$df)
   expect_equal(exp(implicit$par[3]), 0)
   cleared <- estimate(fit, fix2zero = NULL, control = ctrl)
   expect_length(cleared$fix2zero, 0)
   expect_equal(cleared$arg$df, fit$arg$df + 1)
   changed <- estimate(fit, fix2zero = c(3, 5), control = ctrl)
   expect_equal(changed$fix2zero, c(3, 5))
   with_data <- estimate(fit, input$data, control = ctrl)
   expect_identical(with_data$fix2zero, fit$fix2zero)
})

test_that("control options are validated without shifting their values", {
   input <- maintenance_data()
   expect_error(estimate(input$model, input$data,
      control = list(typo = 2, em.iterlim = 5)), "Unknown control.*typo")
   for (bad in list(list(5), list(em.iterlim = 5, 1),
                    list(em.iterlim = 2, em.iterlim = 5), 5,
                    setNames(list(5), NA_character_)))
      expect_error(estimate(input$model, input$data, control = bad),
                   "unique, nonempty names")
   fit <- estimate(input$model, input$data,
                   control = list(em.iterlim = 5, em.tol = 1e-6))
   expect_equal(fit$control$em.iterlim, 5)
   expect_equal(fit$control$em.tol, 1e-6)
   expect_null(fit$control$init.param)
   expect_s3_class(estimate(input$model, input$data, control = list()), "slcafit")
})

test_that("rank diagnostics preserve generalized inverse covariance values", {
   set.seed(1)
   data <- data.frame(y1 = factor(rep(c(1,1,2,2), 25)),
                     y2 = factor(rep(c(1,2,1,2), 25)))
   fit <- estimate(slca(l[2] ~ y1 + y2), data)
   free <- which(!apply(fit$score, 2, anyNA))
   expect_warning(value <- vcov(fit, type = "logit"),
                  "Model information matrix.*rank deficient")
   expect_equal(unname(value[free, free]),
                MASS::ginv(crossprod(fit$score[, free, drop = FALSE])))
   fit$hessian <- matrix(NA_real_, length(fit$par), length(fit$par))
   fit$hessian[free, free] <- crossprod(fit$score[, free, drop = FALSE])
   expect_warning(from_hessian <- vcov(fit, type = "logit", hessian = TRUE),
                  "Model information matrix.*rank deficient")
   expect_equal(value, from_hessian)
})

test_that("fixed and reference parameters do not cause false rank warnings", {
   fit <- structure(list(
      arg = list(id = c(1,1,2,2), fix0 = c(TRUE,FALSE,FALSE,FALSE)),
      par = log(c(0,1,.4,.6)),
      score = cbind(c(0,0), NA_real_, c(-.4,.6), NA_real_),
      hessian = matrix(NA_real_, 4, 4)), class = "slcafit")
   expect_no_warning(vcov(fit, type = "logit"))
   fit$hessian[c(1,3), c(1,3)] <- diag(c(0, .52))
   expect_no_warning(vcov(fit, type = "logit", hessian = TRUE))
   expect_no_warning(warn_information_rank(matrix(numeric(), 0, 0), "Test"))
   expect_no_warning(warn_information_rank(diag(2), "Test"))
   expect_warning(warn_information_rank(matrix(0, 2, 2), "Test"), "rank 0 of 2")
})

test_that("zero regression columns are accompanied by a rank warning", {
   input <- maintenance_data()
   fit <- estimate(input$model, input$data)
   data <- data.frame(x = seq_len(nrow(input$data))/nrow(input$data), z = 0)
   set.seed(42)
   expect_warning(reg <- regress(fit, l ~ x + z, data = data),
                  "Regression information matrix.*rank deficient")
   expect_equal(unname(reg$std.err[, "z"]), 0)
   expect_true(all(is.finite(reg$coefficients)))
})

test_that("zero likelihood ratios do not attempt a bootstrap histogram", {
   input <- maintenance_data()
   fit <- estimate(input$model, input$data)
   local_mocked_bindings(hist = function(...) stop("unexpected histogram"),
                         .package = "graphics")
   expect_no_warning(result <- slca::compare(fit, fit, test = "boot", nboot = 2,
                                             plot = TRUE))
   expect_equal(result$`Pr(Boot)`[2], 1)
   expect_equal(attr(result, "bootFail"), 0)
})

test_that("historical controls inherit missing defaults without changing values", {
   for (fit in list(nlsy_jlcpa, nlsy_jlta)) {
      original <- fit
      old <- fit$control
      old$hessian <- NULL
      expect_null(old$hessian)
      complete <- slcaControl()
      complete[names(old)] <- old
      actual <- estimate(fit, control = old)
      expected <- estimate(fit, control = complete)
      expect_identical(actual, expected)
      expect_identical(unclass(actual$control[names(old)]), unclass(old))
      expect_false(actual$control$hessian)
      expect_identical(fit, original)
   }
})

test_that("comparison ignores saturated status but still validates actual data", {
   for (fit in list(nlsy_jlcpa, nlsy_jlta)) {
      original <- fit
      raw <- as.data.frame(fit$mf)
      lev <- attr(fit$mf, "levels")
      raw[] <- lapply(names(raw), function(v)
         factor(fit$mf[[v]], levels = seq_along(lev[[v]]), labels = lev[[v]]))
      updated <- fit
      updated$mf <- proc_data(raw, fit$model, FALSE)
      expect_equal(unname(as.matrix(slca::compare(fit, updated))),
                   unname(as.matrix(slca::compare(fit, fit))))
      expect_identical(fit, original)
      changed <- updated
      changed$mf[1, 1] <- 3 - changed$mf[1, 1]
      expect_error(slca::compare(fit, changed), "datasets used.*different")
      changed <- updated
      changed$mf[1, 1] <- NA
      expect_error(slca::compare(fit, changed), "datasets used.*different")
      changed <- updated
      attr(changed$mf, "levels")[[1]] <- rev(lev[[1]])
      expect_error(slca::compare(fit, changed), "datasets used.*different")
      changed <- updated
      row.names(changed$mf) <- rev(row.names(changed$mf))
      expect_error(slca::compare(fit, changed), "datasets used.*different")
   }
})
