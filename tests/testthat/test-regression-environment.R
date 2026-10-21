regression_environment_fixture <- local({
   cached <- NULL
   function() {
      if (!is.null(cached)) return(cached)
      model <- slca(l[2] ~ y1 + y2 + y3 + y4)
      start <- c(.6, .4, .8, .2, .7, .3, .9, .1, .65, .35,
                 .2, .8, .3, .7, .15, .85, .25, .75)
      dat <- simulate(model, nsim = 60, seed = 710, parm = start)$response
      rownames(dat) <- sprintf("person_%03d", seq_len(nrow(dat)))
      dat[7, ] <- NA
      fit <- estimate(model, dat, control = slcaControl(
         em.iterlim = 1000, init.param = start))
      covariates <- data.frame(
         x = sin(seq_len(nrow(dat)) / 3),
         z = cos(seq_len(nrow(dat)) / 5),
         group = factor(rep(c("low", "high", "middle"), length.out = nrow(dat)),
                        levels = c("middle", "low", "high")),
         row.names = rownames(dat))
      cached <<- list(fit = fit, covariates = covariates)
      cached
   }
})

# Observe the public regression path after model-frame preparation, before
# optimizers run. The scoped replacement restores stats::model.matrix on exit.
regression_environment_design <- function(object, formula, ...) {
   original <- stats::model.matrix
   captured <- NULL
   local_mocked_bindings(model.matrix = function(object, data, ...) {
      captured <<- list(frame = data, matrix = original(object, data, ...))
      stop(structure(list(message = "regression design captured", call = NULL),
                     class = c("slca_design_captured", "error", "condition")))
   }, .package = "stats")
   tryCatch(regress(object, formula, ...), slca_design_captured = function(e) NULL)
   if (is.null(captured)) stop("regression did not construct its design matrix")
   captured
}

test_that("environment vectors align full and equal-size inputs by names", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   ids <- rownames(fit$mf)
   expected <- regression_environment_design(fit, l ~ x, data = covariates)

   x <- setNames(covariates$x, rownames(covariates))
   x <- rev(x)
   actual <- regression_environment_design(fit, l ~ x)
   expect_equal(actual$matrix, expected$matrix)
   expect_identical(rownames(actual$frame), ids)

   x <- x[rev(ids)]
   actual <- regression_environment_design(fit, l ~ x)
   expect_equal(actual$matrix, expected$matrix)
})

test_that("environment factors retain levels and contrasts after alignment", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   contrasts(covariates$group) <- stats::contr.sum(3)
   expected <- regression_environment_design(fit, l ~ group, data = covariates)
   group <- covariates$group
   names(group) <- rownames(covariates)
   group <- rev(group)

   actual <- regression_environment_design(fit, l ~ group)
   expect_equal(actual$matrix, expected$matrix)
   expect_identical(levels(actual$frame$group), levels(covariates$group))
   expect_equal(contrasts(actual$frame$group), contrasts(covariates$group))
})

test_that("environment matrix covariates keep their shape and basis names", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   basis <- cbind(linear = covariates$x, quadratic = covariates$x^2)
   rownames(basis) <- rownames(covariates)
   reference <- data.frame(row.names = rownames(covariates))
   reference$basis <- I(basis)
   expected <- regression_environment_design(fit, l ~ basis, data = reference)
   basis <- basis[rev(seq_len(nrow(basis))), , drop = FALSE]

   actual <- regression_environment_design(fit, l ~ basis)
   expect_equal(actual$matrix, expected$matrix)
   expect_true(is.matrix(actual$frame$basis))
   expect_identical(colnames(actual$frame$basis), c("linear", "quadratic"))
   expect_identical(colnames(actual$matrix),
                    c("(Intercept)", "basislinear", "basisquadratic"))

   basis <- basis[, "linear", drop = FALSE]
   reference$basis <- I(basis[rownames(reference), , drop = FALSE])
   expected <- regression_environment_design(fit, l ~ basis, data = reference)
   actual <- regression_environment_design(fit, l ~ basis)
   expect_equal(actual$matrix, expected$matrix)
   expect_true(is.matrix(actual$frame$basis))
   expect_equal(ncol(actual$frame$basis), 1L)
})

test_that("each environment input aligns before transformations and interactions", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   ids <- rownames(fit$mf)
   expected <- regression_environment_design(fit, l ~ I(x^2) + z * group,
                                              data = covariates)
   x <- setNames(covariates$x, rownames(covariates))[rev(ids)]
   z <- setNames(covariates$z, rownames(covariates))[c(ids[-1], ids[1])]
   group <- covariates$group
   names(group) <- rownames(covariates)
   group <- group[c(ids[-c(1, 2)], ids[1:2])]
   actual <- regression_environment_design(fit, l ~ I(x^2) + z * group)
   expect_equal(actual$matrix, expected$matrix)

   center <- .25
   curve <- function(value, power = 2) value^power
   expected <- regression_environment_design(fit, l ~ I(x - center) + curve(z),
                                              data = covariates)
   actual <- regression_environment_design(fit, l ~ I(x - center) + curve(z))
   expect_equal(actual$matrix, expected$matrix)
})

test_that("environment alignment precedes covariate missing-value filtering", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   covariates$x[c(3, 11)] <- NA_real_
   expected <- regression_environment_design(fit, l ~ x, data = covariates)
   x <- rev(setNames(covariates$x, rownames(covariates)))
   actual <- regression_environment_design(fit, l ~ x)
   expect_equal(actual$matrix, expected$matrix)
   expect_identical(rownames(actual$frame), rownames(expected$frame))
})

test_that("environment row inputs require complete unique identities by default", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   ids <- rownames(fit$mf)
   x <- unname(fixture$covariates$x[match(ids, rownames(fixture$covariates))])
   expect_error(regression_environment_design(fit, l ~ x),
                "names|identif|align")
   x <- unname(fixture$covariates$x)
   expect_error(regression_environment_design(fit, l ~ x),
                "names|identif|align")

   x <- setNames(seq_along(ids), ids)
   bad <- list(duplicate = c(ids[-1], ids[2]),
               missing = c(ids[-1], "unrelated"),
               empty = replace(ids, 1, ""),
               absent = replace(ids, 1, NA_character_))
   for (bad_ids in bad) {
      names(x) <- bad_ids
      expect_error(regression_environment_design(fit, l ~ x),
                   "names|identif|observations|observation")
   }

   x <- setNames(seq_along(ids), ids)
   z <- unname(seq_along(ids))
   expect_error(regression_environment_design(fit, l ~ x + z),
                "names|identif|align")
   basis <- cbind(linear = z, quadratic = z^2)
   expect_null(rownames(basis))
   expect_error(regression_environment_design(fit, l ~ basis),
                "names|identif|align")
})

test_that("positional environment alignment is explicit and requires fitted row count", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   ordered <- fixture$covariates[rownames(fit$mf), , drop = FALSE]
   expected <- regression_environment_design(fit, l ~ x, data = ordered)
   x <- unname(ordered$x)
   actual <- regression_environment_design(fit, l ~ x, align = "position")
   expect_equal(actual$matrix, expected$matrix)
   names(x) <- rep("ignored duplicate ID", length(x))
   actual <- regression_environment_design(fit, l ~ x, align = "position")
   expect_equal(actual$matrix, expected$matrix)

   x <- fixture$covariates$x
   expect_error(regression_environment_design(fit, l ~ x, align = "position"),
                "same number|length|rows|observations")
})

test_that("explicit data takes precedence and external fallback also aligns safely", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   x <- setNames(c(999, 888), c("not", "observed"))
   expected <- regression_environment_design(fit, l ~ x + z, data = covariates)
   z <- rev(setNames(covariates$z, rownames(covariates)))
   supplied <- covariates[rev(seq_len(nrow(covariates))), "x", drop = FALSE]
   actual <- regression_environment_design(fit, l ~ x + z, data = supplied)
   expect_equal(actual$matrix, expected$matrix)

   z <- unname(z[rownames(fit$mf)])
   expect_error(regression_environment_design(fit, l ~ x + z, data = supplied),
                "names|identif|align")
})

test_that("intercept-only and fitted manifest columns need no external identities", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   actual <- regression_environment_design(fit, l ~ 1)
   expect_identical(rownames(actual$matrix), rownames(fit$mf))
   expect_equal(unname(actual$matrix[, 1]), rep(1, nrow(fit$mf)))
   y1 <- setNames(999, "not an observation")
   actual <- regression_environment_design(fit, l ~ y1)
   expected <- stats::model.matrix(~ y1, data = fit$mf)
   expect_equal(actual$matrix, expected)
})

test_that("saved formula environments are respected without mutation", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   saved <- new.env(parent = environment())
   saved$x <- rev(setNames(covariates$x, rownames(covariates)))
   saved$center <- .25
   formula <- stats::as.formula("l ~ I(x - center)", env = saved)
   original_x <- saved$x
   original_names <- ls(saved, all.names = TRUE)
   x <- 999
   center <- -100
   expected <- regression_environment_design(fit, formula, data = covariates)
   actual <- regression_environment_design(fit, formula)
   expect_equal(actual$matrix, expected$matrix)
   expect_identical(saved$x, original_x)
   expect_identical(ls(saved, all.names = TRUE), original_names)
   expect_identical(environment(formula), saved)
})

test_that("data-frame field access aligns its container rather than field labels", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   fields <- covariates[rev(seq_len(nrow(covariates))), , drop = FALSE]
   x <- 999
   z <- -999
   actual <- regression_environment_design(fit, l ~ fields$x + I(fields$z^2))
   expected <- covariates[rownames(fit$mf), , drop = FALSE]
   expect_equal(unname(actual$frame[["fields$x"]]), expected$x)
   expect_equal(unname(actual$frame[["I(fields$z^2)"]]), expected$z^2,
                ignore_attr = TRUE)
   expect_identical(colnames(actual$matrix),
                    c("(Intercept)", "fields$x", "I(fields$z^2)"))
})

test_that("aligned environment inputs reproduce a real fitted regression", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   set.seed(722)
   expected <- regress(fit, l ~ x + group, data = covariates,
                        imputation = "prop")
   x <- rev(setNames(covariates$x, rownames(covariates)))
   group <- covariates$group
   names(group) <- rownames(covariates)
   group <- group[c(2:length(group), 1)]
   set.seed(722)
   actual <- regress(fit, l ~ x + group, imputation = "prop")
   expect_equal(actual$coefficients, expected$coefficients, tolerance = 1e-8)
   expect_equal(actual$vcov, expected$vcov, tolerance = 1e-8)
   expect_equal(actual$ll, expected$ll, tolerance = 1e-8)
})

test_that("matrix and data-frame subscripts align before selecting columns", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   fields <- covariates[rev(seq_len(nrow(covariates))), , drop = FALSE]
   basis <- as.matrix(covariates[c("x", "z")])
   basis <- basis[c(2:nrow(basis), 1), , drop = FALSE]
   actual <- regression_environment_design(fit, l ~ basis[, 1] + fields[, "z"])
   expected <- covariates[rownames(fit$mf), , drop = FALSE]
   expect_equal(unname(actual$frame[["basis[, 1]"]]), expected$x)
   expect_equal(unname(actual$frame[["fields[, \"z\"]"]]), expected$z)
   expect_identical(rownames(actual$matrix), rownames(fit$mf))
})

test_that("dynamic formula lookups cannot bypass row-identity alignment", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   x <- unname(fixture$covariates[rownames(fit$mf), "x"])
   formulas <- list(l ~ get("x"), l ~ base::get("x"),
                    l ~ eval(quote(x)), l ~ unlist(mget("x")))
   for (formula in formulas)
      expect_error(regression_environment_design(fit, formula),
                   "dynamic formula lookups")
   actual <- regression_environment_design(fit, l ~ get("x"), align = "position")
   expect_equal(unname(actual$frame[["get(\"x\")"]]), x)
   expect_identical(rownames(actual$matrix), rownames(fit$mf))
})

test_that("non-scalar transform settings require inline values or explicit identities", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   x <- rev(setNames(covariates$x, rownames(covariates)))
   knots <- c(-.3, .3)
   expect_error(regression_environment_design(fit, l ~ splines::ns(x, knots = knots)),
                "knots.*supply observation IDs")
   expected <- regression_environment_design(
      fit, l ~ splines::ns(x, knots = c(-.3, .3)), data = covariates)
   actual <- regression_environment_design(fit, l ~ splines::ns(x, knots = c(-.3, .3)))
   expect_equal(actual$matrix, expected$matrix)
})

test_that("named function arguments can still contain independent row inputs", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   ids <- rownames(fit$mf)
   x <- setNames(covariates$x, rownames(covariates))[rev(ids)]
   z <- setNames(covariates$z, rownames(covariates))[c(ids[-1], ids[1])]
   expected <- regression_environment_design(fit, l ~ pmax(x, y = z),
                                              data = covariates)
   actual <- regression_environment_design(fit, l ~ pmax(x, y = z))
   expect_equal(actual$matrix, expected$matrix)
})

test_that("hidden environment lookups are rejected before evaluating covariates", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   # No global binding is created: this expression must be rejected before
   # globalenv() or its field access is evaluated.
   expect_error(regression_environment_design(fit, l ~ globalenv()$x),
                "argument.free|row identity|cannot.*align|dynamic")
   x <- setNames(fixture$covariates$x, rownames(fixture$covariates))
   provider <- function() x
   expect_error(regression_environment_design(fit, l ~ provider()),
                "argument.free|row identity|cannot.*align|dynamic")
})

test_that("explicit matrices cannot acquire implicit row identities by coercion", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   ordered <- fixture$covariates[rownames(fit$mf), c("x", "z"), drop = FALSE]
   # Use sequential fitted IDs so converting an unnamed matrix to a data.frame
   # would otherwise manufacture superficially matching observation IDs.
   ids <- as.character(seq_len(nrow(fit$mf)))
   rownames(fit$mf) <- ids
   fit$posterior$marginal <- lapply(fit$posterior$marginal, function(p) {
      rownames(p) <- ids
      p
   })
   rownames(ordered) <- ids
   supplied <- as.matrix(ordered)
   rownames(supplied) <- NULL
   expect_null(rownames(supplied))
   expect_error(regression_environment_design(fit, l ~ x + z, data = supplied),
                "row names|observation IDs|identif|align")
   expected <- regression_environment_design(fit, l ~ x + z, data = ordered)
   actual <- regression_environment_design(fit, l ~ x + z, data = supplied,
                                            align = "position")
   expect_equal(actual$matrix, expected$matrix)
})

test_that("qualified constants work but namespace row inputs require explicit data", {
   fixture <- regression_environment_fixture()
   fit <- fixture$fit
   covariates <- fixture$covariates
   x <- rev(setNames(covariates$x, rownames(covariates)))
   expected <- regression_environment_design(fit, l ~ I(x / base::pi),
                                              data = covariates)
   actual <- regression_environment_design(fit, l ~ I(x / base::pi))
   expect_equal(actual$matrix, expected$matrix)
   expect_error(regression_environment_design(fit, l ~ datasets::state.abb),
                "explicit.*data")
})
