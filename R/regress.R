#' Regress Exogenous Variables on Latent Variables
#'
#' Performs regression analysis to examine the influence of exogenous (external) variables on latent class variables in an estimated \code{slca} model. The function uses logistic regression with a three-step approach to account for measurement error.
#'
#' @usage
#' regress(object, ...)
#'
#' \method{regress}{slcafit}(
#'    object, formula, data = parent.frame(),
#'    imputation = c("modal", "prop"),
#'    method = c("naive", "BCH", "ML"), ...,
#'    align = c("rownames", "position")
#' )
#' @param object an object of class `slcafit`.
#' @param formula a formula specifying the regression model. The left-hand side must be a single latent class variable from the estimated model.
#' @param data an optional `data.frame` containing the exogenous variables of
#' interest. Variables absent from the fitted data and `data` are taken from
#' the formula's environment and aligned before formula transformations are
#' evaluated. Such vectors and factors need observation IDs in `names()`;
#' matrices and data frames need observation IDs in `rownames()`.
#' @param imputation a character string specifying the imputation method for latent class assignment. Options include:
#'    \itemize{
#'       \item `"modal"`: Assigns each individual to the latent class with the highest posterior probability.
#'       \item `"prop"`: Uses posterior probabilities as proportional assignment weights.
#'    }
#' @param method a character string specifying the method to adjust for bias in the three-step approach. Options include:
#'   \itemize{
#'     \item `"naive"`: A simple approach without correction for classification error.
#'     \item `"BCH"`: The bias-adjusted Bolck, Croon, and Hagenaars method.
#'     \item `"ML"`: A maximum likelihood approach that accounts for classification error.
#'   }
#' @param align how supplied covariate rows are aligned with the fitted data.
#' The default, `"rownames"`, requires unique, nonempty row names and matches
#' them to the fitted observations, regardless of row count or order. Extra rows
#' are allowed, but every fitted observation must be present. Automatic row
#' numbers work only if they still identify the original observations; do not
#' reset them after subsetting or reordering. Use `"position"` explicitly for
#' data already in fitted-observation order; it requires exactly the same number
#' of rows and ignores row names. There is no automatic positional fallback.
#' These rules also apply separately to each environment covariate, even when
#' its length already equals the fitted sample size. Unnamed environment
#' covariates require explicit `align = "position"` and fitted-row order.
#' After observations have been excluded, an unnamed original-length vector
#' cannot be aligned safely: supply `data` with preserved row IDs, or name the
#' vector with its original observation IDs. Matrix covariates remain matrices
#' and factor levels are retained. Unnamed scalar formula parameters are left
#' unchanged, not recycled as covariates. Non-scalar transformation settings
#' (such as spline knots or factor levels) must be written inline or captured
#' inside a transformation function; otherwise they are treated as row inputs.
#' Transformation functions must use their explicit covariate arguments rather
#' than retrieve hidden observation vectors. Use explicit covariate columns
#' instead of dynamic `get()` or `eval()` lookups with row-ID alignment.
#' @param ... additional arguments.
#'
#' @returns
#' A `list` of class `reg.slca` with the following components:
#' \item{coefficients}{A matrix of regression coefficients representing the odds ratios for each latent class against the baseline class (the last class).}
#' \item{std.err}{A matrix of standard errors corresponding to the regression coefficients.}
#' \item{vcov}{The variance-covariance matrix of the regression coefficients.}
#' \item{dim}{The dimensions of the coefficients matrix.}
#' \item{ll}{The log-likelihood of the regression model.}
#'
#' The `summary` function can be used to display the regression coefficients, standard errors, Wald statistics, and p-values. The standard errors are derived by numerically calculated Hessian matrix from `nlm` function.
#' A warning is issued if the regression information matrix is numerically
#' rank deficient. Existing generalized-inverse estimates are retained, but
#' their standard errors and Wald inference may be unreliable.
#'
#' @references Vermunt, J. K. (2010). Latent Class Modeling with Covariates: Two Improved Three-Step Approaches. Political Analysis, 18(4), 450–469. http://www.jstor.org/stable/25792024
#'
#' @example man/examples/regress.R
#'
#' @export
regress <- function(object, ...) UseMethod("regress")

classification_error <- function(w, p) {
   sweep(w %*% p, 2, colSums(p), "/")
}

bch_weights <- function(w, d) {
   t(w) %*% t(ginv(d))
}

regression_formula_inputs <- function(expr, align) {
   if (missing(expr)) return(character())
   if (is.symbol(expr)) return(setdiff(as.character(expr), ""))
   if (!is.call(expr)) return(character())
   head <- expr[[1]]
   fun <- if (is.symbol(head)) as.character(head) else ""
   if (fun %in% c("::", ":::")) {
      value <- eval(expr, envir = baseenv())
      if (align == "rownames" && !is.function(value) &&
          !(is.atomic(value) && is.null(dim(value)) &&
            length(value) == 1L && is.null(names(value))))
         stop("namespace-qualified row inputs cannot be aligned safely; use explicit covariate columns in `data`.",
              call. = FALSE)
      return(character())
   }
   if (is.call(head) && is.symbol(head[[1]]) &&
       as.character(head[[1]]) %in% c("::", ":::"))
      fun <- as.character(head[[3]])
   if (align == "rownames" &&
       (fun %in% c("get", "get0", "mget", "eval", "evalq") ||
        length(expr) == 1L ||
        (fun %in% c("$", "@", "[[") && is.call(expr[[2]]))))
      stop("dynamic formula lookups cannot be aligned by row identity; use explicit covariate columns in `data`.",
           call. = FALSE)
   args <- if (fun %in% c("$", "@")) list(expr[[2]]) else as.list(expr)[-1]
   unique(unlist(lapply(args, regression_formula_inputs, align = align),
                 use.names = FALSE))
}

align_regression_input <- function(x, fitted_ids, align, label) {
   has_rows <- is.matrix(x) || is.data.frame(x)
   ids <- if (has_rows) rownames(x) else names(x)
   if (align == "position") {
      if (NROW(x) != length(fitted_ids))
         stop("positional ", label,
              " must have the same number of rows as the fitted data.",
              call. = FALSE)
      keep <- seq_along(fitted_ids)
   } else {
      valid_ids <- function(ids) !is.null(ids) && !anyNA(ids) &&
         all(nzchar(ids)) && !anyDuplicated(ids)
      id_type <- if (has_rows) "row names" else "names"
      if (!valid_ids(ids) || !valid_ids(fitted_ids))
         stop(label, " ", id_type,
              " must be unique, nonmissing, and nonempty; supply observation IDs or use align = \"position\" for data already in fitted row order.",
              call. = FALSE)
      keep <- match(fitted_ids, ids)
      if (anyNA(keep))
         stop(label, " ", id_type,
              " must identify every fitted observation; use align = \"position\" only for data already in fitted row order.",
              call. = FALSE)
   }
   if (has_rows) {
      x <- x[keep, , drop = FALSE]
      rownames(x) <- fitted_ids
   } else {
      x <- x[keep]
      names(x) <- fitted_ids
   }
   x
}

align_regression_formula <- function(formula, data, align) {
   inputs <- setdiff(regression_formula_inputs(formula[[3]], align),
                     c(names(data), "."))
   if (!length(inputs)) return(formula)
   env <- new.env(parent = environment(formula))
   for (nm in inputs) {
      value <- get(nm, envir = environment(formula), inherits = TRUE)
      # Scalar transform parameters and functions are not observation vectors.
      if (is.function(value) || is.null(value) ||
          (is.atomic(value) && is.null(dim(value)) &&
           length(value) == 1L && is.null(names(value)))) next
      if ((!is.atomic(value) && !is.data.frame(value)) ||
          (!is.null(dim(value)) && length(dim(value)) != 2L))
         stop("environment input `", nm,
              "` cannot be aligned safely; use explicit covariate columns in `data`.",
              call. = FALSE)
      env[[nm]] <- align_regression_input(value, rownames(data), align,
         paste0("environment covariate `", nm, "`"))
   }
   environment(formula) <- env
   formula
}

#' @rdname regress
#' @exportS3Method slca::regress slcafit
regress.slcafit <- function(
      object, formula, data = parent.frame(),
      imputation = c("modal", "prop"),
      method = c("naive", "BCH", "ML"), ...,
      align = c("rownames", "position")
) {
   # Import
   model <- object$model
   if (missing(formula) || length(formula) < 3 || !is.symbol(formula[[2]]))
      stop("`formula` must have a single latent class variable on the left-hand side.",
           call. = FALSE)
   labels <- all.vars(formula)
   latent <- as.character(formula[[2]])
   if (!latent %in% row.names(model$latent))
      stop("`formula` must have a single latent class variable on the left-hand side.",
           call. = FALSE)
   imputation <- match.arg(imputation)
   method <- match.arg(method)
   align <- match.arg(align)

   model$reg$x <- setdiff(labels, latent)
   model$reg$y <- latent

   # Imputation
   impute <- function(x, imputation) {
      cls <- seq_len(ncol(x))
      y <- max.col(x, ties.method = "first")
      factor(y, levels = cls)
   }

   fitted_data <- object$mf
   if (!missing(data)) {
      if (align == "rownames" && is.null(rownames(data)))
         stop("`data` must have observation row names before conversion; supply a row-identified data frame or use align = \"position\" for fitted row order.",
              call. = FALSE)
      supplied <- align_regression_input(as.data.frame(data),
         rownames(fitted_data), align, "`data`")
      for (nm in setdiff(names(supplied), names(fitted_data)))
         fitted_data[[nm]] <- supplied[[nm]]
   }
   imputed <- lapply(object$posterior$marginal[latent],
                     impute, imputation)
   data <- fitted_data
   for (nm in names(imputed)) data[[nm]] <- imputed[[nm]]
   formula <- align_regression_formula(formula, data, align)
   mf <- stats::model.frame(formula, data)

   # Functions
   cprobs <- function(X, b, ref) {
      beta <- matrix(nrow = nrow(b), ncol = ncol(b) + 1)
      beta[, ref] = 0
      beta[, -ref] = b
      xb <- X %*% beta
      exb <- exp(xb)
      denom <- rowSums(exb)
      xb - log(denom)
   }
   p <- object$posterior$marginal[[latent]][rownames(mf), , drop = FALSE]
   w <- switch(
      imputation,
      modal = t(diag(ncol(p))[max.col(p, ties.method = "first"), , drop = FALSE]),
      prop  = t(p)
   )
   y <- stats::model.response(mf)
   X <- stats::model.matrix(formula, mf, ...)
   nr <- nlevels(y) - 1
   nc <- ncol(X)
   init <- numeric(nc * nr)

   if (method == "naive") {
      # naive (biased)
      naive_ll <- function(par, X, w, ref) {
         b <- matrix(par, nrow = ncol(X))
         prob <- cprobs(X, b, ref)
         - sum(prob * t(w))
      }
      fit1 <- try(suppressWarnings(stats::nlm(
         naive_ll, init, X = X, w = w, ref = nlevels(y), hessian = TRUE
         )), TRUE)
      if (!inherits(fit1, "try-error")) {
         ll <- fit1$minimum
         par <- list(fit1$estimate)
         hess <- list(fit1$hessian)
      } else {
         ll <- c()
         par <- list()
         hess <- list()
      }
      fit2 <- lapply(c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), function(x)
         try(suppressWarnings(stats::optim(
            init, naive_ll, X = X, w = w, method = x, ref = nlevels(y), hessian = TRUE
            )), TRUE))
      fit2 <- fit2[sapply(fit2, class) != "try-error"]
      ll <- c(ll, sapply(fit2, "[[", "value"))
      par <- c(par, lapply(fit2, "[[", "par"))
      hess <- c(hess, lapply(fit2, "[[", "hessian"))
   } else {
      # bias_adjusted
      d <- classification_error(w, p)

      if (method == "BCH") {
         # BCH
         w_ <- bch_weights(w, d)

         bch_ll <- function(par, X, w_, ref) {
            b <- matrix(par, ncol(X))
            prob <- cprobs(X, b, ref)
            - sum(w_ * prob)
         }
         fit1 <- try(suppressWarnings(stats::nlm(
            bch_ll, init, X = X, w_ = w_, ref = nlevels(y), hessian = TRUE
            )), silent =  TRUE)
         if (!inherits(fit1, "try-error")) {
            ll <- fit1$minimum
            par <- list(fit1$estimate)
            hess <- list(fit1$hessian)
         } else {
            ll <- c()
            par <- list()
            hess <- list()
         }
         fit2 <- lapply(c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), function(x)
            try(suppressWarnings(stats::optim(
               init, bch_ll, X = X, w_ = w_, method = x, ref = nlevels(y), hessian = TRUE
               )),silent = TRUE))
         fit2 <- fit2[sapply(fit2, class) != "try-error"]
         ll <- c(ll, sapply(fit2, "[[", "value"))
         par <- c(par, lapply(fit2, "[[", "par"))
         hess <- c(hess, lapply(fit2, "[[", "hessian"))
      } else if (method == "ML") {
         # ML
         ml_ll <- function(par, X, ref) {
            b <- matrix(par, ncol(X))
            prob <- t(cprobs(X, b, ref))
            - sum(w * log(d %*% exp(prob)))
         }

         fit1 <- try(suppressWarnings(stats::nlm(
            ml_ll, init, X = X, ref = nlevels(y), hessian = TRUE
            )), silent = TRUE)
         if (!inherits(fit1, "try-error")) {
            ll <- fit1$minimum
            par <- list(fit1$estimate)
            hess <- list(fit1$hessian)
         } else {
            ll <- c()
            par <- list()
            hess <- list()
         }
         fit2 <- lapply(c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"), function(x)
            try(suppressWarnings(stats::optim(
               init, ml_ll, X = X, method = x, ref = nlevels(y), hessian = TRUE
               )), TRUE))
         fit2 <- fit2[sapply(fit2, class) != "try-error"]
         ll <- c(ll, sapply(fit2, "[[", "value"))
         par <- c(par, lapply(fit2, "[[", "par"))
         hess <- c(hess, lapply(fit2, "[[", "hessian"))
      }
   }

   par <- par[[which.min(ll)]]
   hess <- hess[[which.min(ll)]]
   warn_information_rank(hess, "Regression information matrix")

   rn <- paste0(seq_len(nr), "/", nr + 1)
   cn <- colnames(X)
   coef <- matrix(
      par, nr, nc, byrow = TRUE,
      dimnames = list(class = rn, cn)
   )

   dn <- paste0(rep(cn, nr), "|", rep(rn, each = nc))
   vcov <- matrix(
      ginv(hess), nc * nr, nc * nr,
      dimnames = list(dn, dn)
   )

   se <- matrix(
      sqrt(pmax(diag(vcov), 0)), nr, nc, byrow = TRUE,
      dimnames = list(class = rn, cn)
   )

   res <- list()
   res$model <- model
   res$coefficients <- coef
   res$std.err <- se
   res$vcov <- vcov
   res$dim <- c(nr, nc)
   res$ll <- - min(ll)
   class(res) <- "reg.slca"

   return(res)
}


#' @exportS3Method base::print reg.slca
print.reg.slca <- function(
   x, digits = 3, wald = TRUE, pval = TRUE, ...
) {
   cat("Coefficients:")
   print.default(format(x$coefficients, digits = digits),
                 print.gap = 2L, quote = FALSE)
   invisible(x)
}

#' @exportS3Method base::summary reg.slca
summary.reg.slca <- function(
   object, digits = 3, odds.ratio = FALSE, wald = TRUE, ...
) {
   cat("Coefficients:")
   print.default(format(object$coefficients, digits = digits), print.gap = 2L,
                 quote = FALSE)
   cat("\nStd. Errors:")
   print.default(format(object$std.err, digits = digits), print.gap = 2L,
                 quote = FALSE)
   if (odds.ratio) {
      cat("Odds Ratio:")
      print.default(format(exp(object$coefficients), digits = digits),
                    print.gap = 2L, quote = FALSE)
   }
   if (wald) {
      wald <- object$coefficients / object$std.err
      pval <- 2 * stats::pnorm(abs(wald), lower.tail = FALSE)
      cat("\nValue/SE (Wald statistics):")
      print.default(format(wald, digits = digits),
                    print.gap = 2L, quote = FALSE)
      cat("\nPr(>|W|):")
      print.default(format(pval, digits = digits),
                    print.gap = 2L, quote = FALSE)
   }
   invisible(object)
}


#' @exportS3Method stats::confint reg.slca
confint.reg.slca <- function(
   object, parm, level = 0.95, odds.ratio = FALSE, ...
) {
   fci <- function(cf, se) {
      a <- (1 - level)/2
      a <- c(a, 1 - a)
      pct <- format_pc(a, 3)
      fac <- stats::qnorm(a)
      ci <- array(NA, dim = c(length(parm), 2L),
                  dimnames = list(parm, pct))
      ci[] <- cf[parm] + se[parm] %o% fac
      if (odds.ratio) ci <- exp(ci)
      ci
   }
   cf <- object$coefficients
   se <- object$std.err
   cn <- colnames(cf)
   rn <- rownames(cf)
   if (missing(parm)) parm <- cn
   else if (is.numeric(parm)) parm <- cn[parm]
   if (anyNA(parm) || any(!parm %in% cn))
      stop("unknown regression parameter(s).", call. = FALSE)
   ci <- lapply(seq_len(nrow(cf)), function(i)
      fci(cf[i, ], se[i,]))
   names(ci) <- rn

   for (i in seq_len(nrow(cf))) {
      cat(rn[i], ":\n")
      print.default(ci[[i]])
   }
   invisible(ci)
}

logit2ll <- function(x) {
   cf <- x$coefficient
   bb <- -colSums(cf) / (nrow(cf) + 1)
   ll <- rbind(sweep(cf, 2, -bb), bb)
   dimnames(ll) <- list(
      class = seq_len(nrow(ll)),
      colnames(cf)
   )
   ll
}
