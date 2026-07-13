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
#'    method = c("naive", "BCH", "ML"), ...
#' )
#' @param object an object of class `slcafit`.
#' @param formula a formula specifying the regression model, including both latent class variables (from the estimated model) and exogenous variables.
#' @param data an optional `data.frame` containing the exogenous variables of interest. If omitted, the variables are taken from the parent environment.
#' @param imputation a character string specifying the imputation method for latent class assignment. Options include:
#'    \itemize{
#'       \item `"modal"`: Assigns each individual to the latent class with the highest posterior probability.
#'       \item `"prop"`: Assigns classes probabilistically based on the posterior probability distribution.
#'    }
#' @param method a character string specifying the method to adjust for bias in the three-step approach. Options include:
#'   \itemize{
#'     \item `"naive"`: A simple approach without correction for classification error.
#'     \item `"BCH"`: The bias-adjusted Bolck, Croon, and Hagenaars method.
#'     \item `"ML"`: A maximum likelihood approach that accounts for classification error.
#'   }
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
#'
#' @references Vermunt, J. K. (2010). Latent Class Modeling with Covariates: Two Improved Three-Step Approaches. Political Analysis, 18(4), 450–469. http://www.jstor.org/stable/25792024
#'
#' @example man/examples/regress.R
#'
#' @export
regress <- function(object, ...) UseMethod("regress")

#' @rdname regress
#' @exportS3Method slca::regress slcafit
regress.slcafit <- function(
      object, formula, data = parent.frame(),
      imputation = c("modal", "prop"),
      method = c("naive", "BCH", "ML"), ...
) {
   # Import
   model <- object$model
   labels <- all.vars(formula)
   latent <- labels[labels %in% row.names(model$latent)]
   imputation <- match.arg(imputation)
   method <- match.arg(method)

   model$reg$x <- setdiff(labels, latent)
   model$reg$y <- latent

   # Imputation
   impute <- function(x, imputation) {
      as.factor(apply(x, 1, which.max))
   }

   if (missing(data)) data <- object$mf
   else data <- data.frame(object$mf, data)
   imputed <- lapply(object$posterior$marginal[latent],
                     impute, imputation)
   data <- data.frame(data, imputed)
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
   p <- object$posterior$marginal[[latent]][rownames(mf),]
   w <- switch(
      imputation,
      modal = apply(p, 1, function(x) as.numeric(x == max(x))),
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
      d <- (w %*% p) / colSums(p)

      if (method == "BCH") {
         # BCH
         w_ <- t(w) %*% ginv(d)

         bch_ll <- function(par, X, w_, ref) {
            b <- matrix(par, ncol(X))
            prob <- cprobs(X, b, ref)
            - sum(w_ * prob)
         }
         fit1 <- try(suppressWarnings(stats::nlm(
            bch_ll, init, X = X, w_ = w_, ref = nlevels(y), hessian = TRUE, iterlim = 2
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
         ml_ll <- function(par, X, w_, ref) {
            b <- matrix(par, ncol(X))
            prob <- t(cprobs(X, b, ref))
            - sum(w * log(d %*% exp(prob)))
         }

         fit1 <- try(suppressWarnings(stats::nlm(
            ml_ll, init, X = X, w_ = w_, ref = nlevels(y), hessian = TRUE
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
               init, ml_ll, X = X, w_ = w_, method = x, ref = nlevels(y), hessian = TRUE
               )), TRUE))
         fit2 <- fit2[sapply(fit2, class) != "try-error"]
         ll <- c(ll, sapply(fit2, "[[", "value"))
         par <- c(par, lapply(fit2, "[[", "par"))
         hess <- c(hess, lapply(fit2, "[[", "hessian"))
      }
   }

   par <- par[[which.min(ll)]]
   hess <- hess[[which.min(ll)]]

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
      diag(vcov), nr, nc, byrow = TRUE,
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
      pval <- stats::pnorm(abs(wald), 1, lower.tail = FALSE)
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
      ci[] <- cf[parm] + se %o% fac
      ci
   }
   cf <- object$coefficients
   se <- object$std.err
   cn <- colnames(cf)
   rn <- rownames(cf)
   if (missing(parm)) parm <- cn
   else parm <- cn[parm]
   ci <- lapply(seq_len(nrow(cf)), function(i)
      fci(cf[i, ], se[i,]))
   names(ci) <- rn

   for (i in seq_len(nrow(cf))) {
      cat(rn[i], ":\n")
      print.default(ci[[i]])
   }
   invisible(ci[, parm])
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
