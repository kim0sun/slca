#' Regress Exogenous Variables on Latent Variables
#'
#' This function performs regression analysis to explore the influence of exogenous (external) variables on the latent class variables within an estimated \code{slca} model. It utilizes logistic regression and employs a three-step approach.
#'
#' @aliases regress regress.slca
#' @usage
#' regress(object, ...)
#'
#' \method{regress}{slca}(
#'    object, formula, data = parent.frame(),
#'    imputation = c("modal", "prob"),
#'    method = c("naive", "BCH", "ML"), ...
#' )
#' @param object an object of class `slca` and `estimated`
#' @param formula a formula defining the regression model, including both latent class variables from the estimated model and any exogenous (external) variables.
#' @param data an optional data frame containing the exogenous variables of interest.
#' @param imputation the imputation method for imputing (assigning) latent class variables. Possible values are:
#' - "modal": Assigns each individual to the latent class with the highest posterior probability.
#' - "prob": Assigns classes to individuals randomly according to the distribution of posterior probabilities.
#' @param method the method used to adjust bias in the three-step approach, with options including "naive", "BCH", and "ML".
#' @param ... additional arguments.
#'
#' @returns
#' A `list` with following components:
#'
#' \item{coefficients}{a matrix of regression coefficients representing the odds ratios of each class against the baseline class (the last class).}
#' \item{std.err}{a matrix of standard errors corresponding to the regression coefficients.}
#' \item{vcov}{the calculated variance-covariance matrix for the regression coefficients.}
#' \item{dim}{the dimensions of the coefficients matrix.}
#' \item{ll}{the log likelihood of the regression model.}
#'
#' Using the `summary` function, you can print coefficients, standard errors, corresponding Wald statistics, and p-values for these statistics.
#'
#' @references Vermunt, J. K. (2010). Latent Class Modeling with Covariates: Two Improved Three-Step Approaches. Political Analysis, 18(4), 450–469. http://www.jstor.org/stable/25792024
#'
#' @example man/examples/reg.R
#'
#' @export
regress <- function(object, ...) UseMethod("regress")

#' @exportS3Method slca::regress slca
regress.slca <- function(
      object, formula, data = parent.frame(),
      imputation = c("modal", "prob"),
      method = c("naive", "BCH", "ML"), ...
) {
   if (!inherits(object, "estimated"))
      stop("Latent variable model should be estimated.")

   # Import
   labels <- all.vars(formula)
   latent <- labels[labels %in% row.names(object$model$latent)]
   imputation <- match.arg(imputation)
   method <- match.arg(method)

   # Imputation
   impute <- function(x, imputation) {
      if (imputation == "modal")
         imputed <- as.factor(apply(x, 1, which.max))
      else
         imputed <- as.factor(apply(x, 1, function(y)
            sample(seq_len(ncol(x)), 1, prob = y)))
      return(imputed)
   }

   if (missing(data)) data <- object$mf
   else data <- data.frame(object$mf, data)
   imputed <- lapply(object$posterior$marginal[latent],
                     impute, imputation)
   data <- data.frame(data, imputed)
   mf <- stats::model.frame(formula, data)

   # Functions
   dn <- function(X, b, y, k)
      -sum(stats::dnorm(X, b[y], b[k], log = TRUE))

   d <- function(X, b, y, k)
      -sum(stats::dnorm(X, b[y], b[k], log = TRUE))

   cprobs <- function(X, b, ref) {
      beta <- matrix(nrow = nrow(b), ncol = ncol(b) + 1)
      beta[, ref] = 0
      beta[, -ref] = b
      xb <- X %*% beta
      exb <- exp(xb)
      denom <- rowSums(exb)
      xb - log(denom)
   }

   y <- stats::model.response(mf)
   X <- stats::model.matrix(formula, mf)
   nr <- nlevels(y) - 1
   nc <- ncol(X)
   init <- numeric(nc * nr)

   if (method == "naive") {
      # naive (biased)
      naive_ll <- function(par, X, y, ref) {
         b <- matrix(par, nrow = ncol(X))
         prob <- cprobs(X, b, ref)
         - sum(prob[cbind(1:nrow(prob), y)])
      }
      fit1 <- try(suppressWarnings(stats::nlm(
         naive_ll, init, X = X, y = y, ref = nlevels(y), hessian = TRUE
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
            init, naive_ll, X = X, y = y, method = x, ref = nlevels(y), hessian = TRUE
            )), TRUE))
      fit2 <- fit2[sapply(fit2, class) != "try-error"]
      ll <- c(ll, sapply(fit2, "[[", "value"))
      par <- c(par, lapply(fit2, "[[", "par"))
      hess <- c(hess, lapply(fit2, "[[", "hessian"))
   } else {
      # bias_adjusted
      p <- object$posterior$marginal[[latent]][rownames(mf),]
      w <- switch(
         imputation,
         modal = apply(p, 1, function(x) x == max(x)),
         prob  = p
      )
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
         w_ <- log(sapply(y, function(x) d[, x]))

         ml_ll <- function(par, X, w_, ref) {
            b <- matrix(par, ncol(X))
            prob <- t(cprobs(X, b, ref))
            ll <- colSums(exp(prob + w_))
            -sum(log(ll))
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
