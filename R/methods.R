#' @exportS3Method base::print slca
print.slca <- function(x, ...) {
   lt <- x$model$latent
   mr <- x$model$measure
   st <- x$model$struct
   cat("Structural Latent Variable Model\n")
   cat("\nLatent variables (Root*):")
   label <- row.names(lt)
   label[lt$root] <- paste0(label[lt$root], "*")
   mat <- rbind(label, lt$nclass)
   dimnames(mat) <- list(c(" Label:", "nclass:"), rep("", ncol(mat)))
   print(mat, quote = FALSE)

   cat("\nMeasurement model:")
   mat <- mr[c("indicator", "constraint")]
   mapping1 <- function(x) paste("-> {", sapply(x, paste, collapse = ", "), "}")
   mat$indicator <- mapping1(lapply(mat$indicator, function(x)
      x[!(x %in% label)]))
   mat$constraint <- paste("", mat$constraint)
   mat <- as.matrix(mat)
   dimnames(mat) = list(
      paste0(" ", row.names(mat)), rep("", ncol(mat))
   )
   print(mat, quote = FALSE)

   if (nrow(st) > 0) {
      cat("\nStructural model:")
      vars <- tapply(st$child, st$parent, paste, collapse = ", ")
      vars <- vars[!is.na(vars)]
      mat <- cbind("->", paste("{", vars, "}"))
      dimnames(mat) = list(paste0(" ", names(vars)),  rep("", ncol(mat)))
      print(mat, quote = FALSE)

      cat("\nDependency constraints:\n")
      const <- st$constraint
      parent <- split(st$parent, const)
      child <- split(st$child, const)
      mapping2 <- function(x, y) paste(x, "->", y)
      maps <- mapply(mapping2, parent, child, SIMPLIFY = FALSE)
      mat <- matrix("", nrow = max(lengths(maps)), ncol = length(maps))
      for (i in seq_len(length(maps))) {
         mat[seq_len(length(maps[[i]])), i] = maps[[i]]
      }
      dimnames(mat) = list(rep("", nrow(mat)), names(maps))
      print(mat, quote = FALSE)
   }
}

#' @exportS3Method base::print slcafit
print.slcafit <- function(x, ...) {
   NextMethod()
   cat("\nLogLik:", logLik(x))
}

#' @exportS3Method base::summary slca
summary.slca = function(object, ...) {
   cat("Structural Latent Class Model\n")
   lt <- object$model$latent
   mr <- object$model$measure
   st <- object$model$struct
   tr <- object$model$tree
   nvar <- length(setdiff(tr$child, tr$parent))
   nlv <- nrow(lt)

   cat("\nSummary of model structure\n")
   mat <- rbind(nvar, nlv)
   dimnames(mat) <- list(
      c(" Number of manifest variables",
        " Number of latent class variables"), ""
   )
   print(mat)
   cat("\n Latent variables (Root*):")
   label <- row.names(lt)
   label[lt$root] <- paste0(label[lt$root], "*")
   mat <- rbind(label, lt$nclass)
   dimnames(mat) <- list(c("  Label:", " nclass:"), rep("", ncol(mat)))
   print(mat, quote = FALSE)

   cat("\n Measurement model:")
   mat <- mr[c("indicator", "constraint")]
   mapping1 <- function(x) paste("-> {", sapply(x, paste, collapse = ", "), "}")
   mat$indicator <- mapping1(lapply(mat$indicator, function(x)
      x[!(x %in% label)]))
   mat$constraint <- paste("", mat$constraint)
   mat <- as.matrix(mat)
   colnames(mat) = rep("", ncol(mat))
   rownames(mat) = paste0("  ", rownames(mat))
   print(mat, quote = FALSE)

   if (nrow(st) > 0) {
      cat("\n Structural model:")
      vars <- tapply(st$child, st$parent, paste, collapse = ", ")
      vars <- vars[!is.na(vars)]
      mat <- cbind("->", paste("{", vars, "}"))
      dimnames(mat) = list(paste0("  ", names(vars)),  rep("", ncol(mat)))
      print(mat, quote = FALSE)

      cat("\n Dependency constraints:\n")
      const <- st$constraint
      parent <- split(st$parent, const)
      child <- split(st$child, const)
      mapping2 <- function(x, y) paste(x, "->", y)
      maps <- mapply(mapping2, parent, child, SIMPLIFY = FALSE)
      mat <- matrix("", nrow = max(lengths(maps)), ncol = length(maps))
      for (i in seq_len(length(maps))) {
         mat[seq_len(length(maps[[i]])), i] = maps[[i]]
      }
      dimnames(mat) = list(rep(" ", nrow(mat)), names(

         maps))
      print(mat, quote = FALSE)
   }

   if (nrow(st) > 0) {
      cat("\n Tree of structural model:")
      root <- st[st$rank == 1, c(1, 3)]
      names(root) <- c("root", "child1")
      node <- st[, c(1, 3)]
      for (i in seq_len(max(lt$rank) - 1)) {
         names(node) <- paste0("child", c(i, i + 1))
         root <- merge(root, node, by = paste0("child", i),
                       no.dups = TRUE, all.x = TRUE)
      }
      nlev <- row.names(lt)[order(lt$rank, -lt$leaf, lt$nvar)]
      inv_tree <- root[-ncol(root)]
      tree <- inv_tree[rev(seq_len(ncol(inv_tree)))]
      tree[] <- lapply(tree, factor, levels = nlev)
      tree <- tree[do.call(order, tree),]
      tree[] <- lapply(tree, function(x) {
         x[duplicated(x)] <- NA
         x
      })
      mat <- as.matrix(tree)
      dimnames(mat) <- list(rep("", nrow(tree)), rep("", ncol(tree)))
      mat[, -1] <- ifelse(is.na(mat[, -1]), NA, paste0(" -> ", mat[, -1]))
      mat[, 1] <- ifelse(is.na(mat[, 1]), NA, paste0(" ", mat[, 1]))
      print(mat, quote = FALSE, na.print = "")
   }
   invisible(object)
}

#' @exportS3Method base::summary slcafit
summary.slcafit <- function(object, ...) {
   NextMethod()

   lt <- object$model$latent
   mr <- object$model$measure
   st <- object$model$struct
   tr <- object$model$tree
   nvar <- length(setdiff(tr$child, tr$parent))
   nlv <- nrow(lt)

   cat("\n\nSummary of manifest variables\n")
   cat("\n Categories for each variable:\n")
   lev <- attr(object$mf, "levels")
   mat <- matrix(nrow = nvar, ncol = max(sapply(lev, length)))
   for (i in seq_len(nvar)) {
      mat[i, seq_along(lev[[i]])] <- lev[[i]]
   }
   dimnames(mat) <- list(
      paste0(" ", names(lev)),
      response = seq_len(ncol(mat))
   )
   print(mat, quote = FALSE, print.gap = 2,
         na.print = "")

   cat("\n Frequencies for each categories:\n")
   freq <- lapply(object$mf, function(x) {
      tab <- table(x, useNA = "always")
      names(tab)[length(tab)] <- "<NA>"
      tab
   })
   mat <- matrix(nrow = nvar, ncol = max(sapply(lev, length)) + 1)
   dimnames(mat) <- list(
      paste0(" ", names(lev)),
      response = c(seq_len(ncol(mat) - 1), "<NA>")
   )
   for (i in seq_len(nvar)) {
      mat[i, names(freq[[i]])] <- freq[[i]]
   }
   print(mat, quote = FALSE, print.gap = 2,
         na.print = "")

   cat("\n\nSummary of model fit\n")
   nobs <- object$arg$nobs
   npar <- object$arg$df
   llik <- stats::logLik(object)
   aic <- stats::AIC(object)
   bic <- stats::BIC(object)

   arg <- object$arg
   mf <- object$mf
   ll <- calcModel(
      attr(mf, "yu"), nrow(attr(mf, "y_unique")),
      arg$nvar, unlist(arg$nlev), object$par, arg$fix0,
      arg$ref - 1, arg$nlv, arg$nrl, arg$nlf,
      arg$npi, arg$ntau, arg$nrho, arg$ul, arg$vl,
      arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
      arg$nc, arg$nk, arg$nl, arg$ncl,
      arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
   )$ll
   mfreq <- exp(rowSums(ll) + log(arg$nobs))
   dfreq <- attr(object$mf, "freq")
   chisq <- sum(((dfreq - mfreq)^2 / mfreq)[mfreq > 0]) +
      (arg$nobs - sum(mfreq))

   gsq <- 2 * (attr(object$mf, "loglik") - stats::logLik(object))
   resdf <- attr(object$mf, "df") - npar
   sprintf("%.0f", npar)
   mat <- rbind(
      sprintf("%.0f", nobs), sprintf("%.0f", npar),
      sprintf("%.3f", llik), NA,
      sprintf("%.3f", aic), sprintf("%.3f", bic), NA,
      sprintf("%.0f", resdf), sprintf("%.3f", chisq),
      sprintf("%.3f", stats::pchisq(chisq, resdf, lower.tail = FALSE)),
      sprintf("%.3f", gsq),
      sprintf("%.3f", stats::pchisq(gsq, resdf, lower.tail = FALSE)))
   format(stats::pchisq(gsq, resdf, lower.tail = FALSE), digits = 3)
   dimnames(mat) <- list(
      c(" Number of observations",
        " Number of free parameters",
        " Log-likelihood",
        " Information criteria",
        "   Akaike (AIC)",
        "   Bayesian (BIC)",
        " Chi-squared Tests",
        "   Residual degree of freedom (df)",
        "   Pearson Chi-squared (X-squared)",
        "     P(>Chi)",
        "   Likelihood Ratio (G-squared)",
        "     P(>Chi)"
      ), ""
   )
   print(mat, na.print = "", quote = FALSE, right = TRUE)
   invisible(object)
}

#' Print Estimated Parameters of `slcafit` Object
#'
#' This function prints the estimated parameters of the `slca` model by accepting an `slcafit` object.
#'
#' @aliases param param.slcafit
#' @usage
#' param(object, ...)
#'
#' \method{param}{slcafit}(
#'    object, type = c("probs", "logit"),
#'    se = FALSE, index = FALSE, ...
#' )
#'
#' @param object an object of class `slcafit`.
#' @param type a character string specifying the format in which the estimated parameters should be displayed. The options are `"probs"` for probability format or `"logit"` for log-odds (logit) format. The default setting is `"probs"`.
#' @param se a logical indicating whether standard errors (TRUE) or parameter estimates (FALSE) should be displayed.
#' @param index a logical indicating whether to include (`TRUE`) or exclude (`FALSE`) the indices of the estimated parameters in the output.
#' @param ... additional arguments.
#'
#' @returns
#' A `list` containing the specified estimated parameters or their standard errors if `se` is set to `TRUE`. The components of the list include:
#' \item{pi}{Membership probabilities of the root variable.}
#' \item{tau}{Conditional probabilities between latent class variables, represented with uppercase alphabets for considering measurement invariance.}
#' \item{rho}{Item response probabilities for each measurement model, represented with lowercase alphabets for considering measurement invariance.}
#'
#' @export
param <- function(object, ...) UseMethod("param")
#' @exportS3Method slca::param slcafit
param.slcafit <- function(
   object, type = c("probs", "logit"),
   se = FALSE, index = FALSE, ...
) {
   type <- match.arg(type)

   if (se) {
      vcov <- vcov.slcafit(object, type)
      var <- diag(vcov)
      est <- numeric(length(var))
      est[var >= 0] <- sqrt(var[var >= 0])
   } else {
      est <- switch(
         type,
         probs = exp(object$par),
         logit = object$logit,
      )
   }

   skeleton <- object$skeleton$par

   res <- utils::relist(est, skeleton)
   attr(res, "index") <- index
   if (index) {
      attr(res, "idx") <- seq_along(est)
   }
   class(res) <- c("slcapar", "list")
   res
}

#' @exportS3Method base::print slcapar
print.slcapar <- function(
   x, digits = max(3L, getOption("digits") - 3L), ...
) {
   class(x) <- "list"
   index <- attr(x, "index")
   val <- unlist(x)
   val <- sprintf(paste0(" %.", digits, "f"), val)
   if (index) {
      idx <- attr(x, "idx")
      ans <- utils::relist(paste0(val, " (", idx, ")"), x)
   } else ans <- utils::relist(val, x)

   cat("PI :\n")
   for (i in names(ans[["pi"]])) {
      cat(paste0("(", i, ")\n"))
      print(ans[["pi"]][[i]], right = TRUE, quote = FALSE)
   }
   if (length(ans[["tau"]])) {
      cat("\nTAU :\n")
      for (i in names(ans[["tau"]])) {
         cat(paste0("(", i, ")\n"))
         print(ans[["tau"]][[i]], right = TRUE, quote = FALSE)
         print(attr(x[["tau"]][[i]], "vars"), quote = FALSE)
      }
   }
   cat("\nRHO :\n")
   for (i in names(ans[["rho"]])) {
      cat(paste0("(", i, ")\n"))
      print(ans[["rho"]][[i]], right = TRUE, quote = FALSE)
      cat("\n")
      print(attr(x[["rho"]][[i]], "vars"), quote = FALSE)
   }
}


#' @exportS3Method stats::vcov slcafit
vcov.slcafit <- function(object, type = c("probs", "logit"), ...) {
   type <- match.arg(type)
   id <- object$arg$id
   score <- object$score
   fi <- crossprod(score)
   vcov <- matrix(0, nrow(fi), ncol(fi), dimnames = dimnames(fi))
   nan <- apply(score, 2, anyNA)
   vcov[!nan, !nan] <- MASS::ginv(fi[!nan, !nan])

   if (type == "probs") {
      jac <- bdiag(tapply(object$par, id, jmat))
      vcov <- jac %*% vcov %*% t(jac)
   }

   vcov
}

#' @exportS3Method stats::predict slcafit
predict.slcafit <- function(
   object, newdata, type = c("class", "posterior"), ...
) {
   type <- match.arg(type)
   if (missing(newdata)) post <- object$posterior
   else {
      mf <- proc_data(newdata, object$model, FALSE)
      arg <- arg_mf(object$model, object$arg, mf, object$fix2zero)

      post <- calcModel(
         attr(mf, "y"), arg$nobs, arg$nvar, unlist(arg$nlev),
         object$par, arg$fix0, arg$ref - 1, arg$nlv, arg$nrl, arg$nlf,
         arg$npi, arg$ntau, arg$nrho, arg$ul, arg$vl,
         arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
         arg$nc, arg$nk, arg$nl, arg$ncl,
         arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
      )$post
      skeleton <- get_frame(object$model, arg, mf)
      post <- utils::relist(post, skeleton$post)
   }
   impute <- function(x) apply(x, 2, which.max)

   switch(
      type,
      class = sapply(post, impute),
      posterior = lapply(post, t)
   )
}


#' Confidence Intervals for Model Parameters
#'
#' Computes confidence intervals for one or more parameters of fitted model. Package \pkg{slca} adds methods for \code{slca} fits.
#'
#' @param object an object of class `slcafit`.
#' @param parm an integer string specifying parameters to be given confidence intervals.
#' @param level numeric value representing the desired confidence level for the intervals, with a default of 0.95.
#' @param type a character string specifying the format in which the results should be returned. Options include `"probs"` for probability format and `"logit"` for log-odds (logit) format, with the default being `"probs"`.
#' @param ... additional arguments.
#'
#' @returns
#' A `matrix` with two columns representing the confidence intervals for the selected parameters. The columns are named based on the specified confidence level (`level`):
#'
#' - `100 * (level / 2) %`: This column shows the lower bound of the confidence interval.
#' - `100 * (1 - level / 2) %`: This column shows the upper bound of the confidence interval.
#'
#' The `level` parameter specifies the confidence level, with common values being 0.05 for a 95% confidence interval and 0.01 for a 99% confidence interval.
#'
#' @example man/examples/confint.R
#'
#' @exportS3Method stats::confint slcafit
confint.slcafit <- function(
   object, parm, level = 0.95, type = c("param", "logit"), ...
) {
   if (missing(parm)) parm <- seq_along(object$par)
   type <- match.arg(type)

   logit <- object$logit
   vcov <- stats::vcov(object, "logit")
   vars <- diag(vcov)
   se <- vars
   se[] <- 0
   se[vars >= 0] <- sqrt(vars[vars >= 0])

   lower <- (1 - level) / 2
   upper <- 1 - lower
   cn <- format_pc(c(lower, upper), 3)

   ci_logit <- logit + se %o% stats::qnorm(c(lower, upper))
   ci_par <- ci_logit
   ci_par[] <- exp(unlist(apply(ci_logit, 2, tapply, object$arg$id, norm2)))

   ci <- switch(type, param = ci_par[parm,], logit = ci_logit[parm,])
   colnames(ci) <- cn
   rownames(ci) <- paste0("(", parm, ")")
   ci
}

format_pc <- function(perc, digits)
   paste(format(100 * perc, trim = TRUE, scientific = FALSE, digits = digits), "%")

#' Reorder Latent Class Membership of Class Variables
#'
#' This function reorders the latent class membership for specified latent class variables.

#' @param x an object of class `slcafit`.
#' @param ... additional arguments specifying the new order for the latent class variables.
#'
#' @returns Returns the modified `slcafit` object with the reordered latent classes.
#'
#' @example man/examples/reorder.R
#'
#' @exportS3Method stats::reorder slcafit
reorder.slcafit <- function(x, ...) {
   m <- match.call(expand.dots = FALSE)
   orders <- lapply(list(...), rank, ties.method = "first")
   name <- intersect(names(orders), row.names(x$model$latent))

   id <- utils::relist(seq_along(x$par), x$skeleton$par)
   pi_name <- names(id$pi)
   tau_name <- lapply(id$tau, attr, "vars")
   parent <- lapply(tau_name, "[", 1, )
   child <- lapply(tau_name, "[", 2, )
   rho_name <- lapply(id$rho, function(x)
      row.names(attr(x, "vars")))

   for (nm in name) {
      for (i in names(parent)) {
         if (nm %in% child[[i]]) {
            name <- union(name, child[[i]])
            for (j in child[[i]])
               orders[[j]] <- orders[[nm]]
         }
         if (nm %in% parent[[i]]) {
            name <- union(name, parent[[i]])
            for (j in parent[[i]])
               orders[[j]] <- orders[[nm]]
         }
      }
      for (i in names(rho_name)) {
         if (nm %in% rho_name[[i]]) {
            name <- union(name, rho_name[[i]])
            for (j in rho_name[[i]])
               orders[[j]] <- orders[[nm]]
         }
      }
   }
   idx <- list()
   for (nm in name) {
      orig <- seq_len(x$model$latent[nm, "nclass"])
      idx[[nm]] <- c(orders[[nm]], setdiff(orig, orders[[nm]]))
   }

   for (i in pi_name) {
      if (i %in% name)
         id$pi[[i]] <- id$pi[[i]][, idx[[i]]]
   }
   for (i in names(parent)) {
      if (any(parent[[i]] %in% name)) {
         j <- min(match(parent[[i]], name))
         id$tau[[i]] <- id$tau[[i]][, idx[[j]]]
      }
      if (any(child[[i]] %in% name)) {
         j <- min(match(child[[i]], name))
         id$tau[[i]] <- id$tau[[i]][idx[[j]], ]
      }
   }
   for (i in names(rho_name)) {
      if (any(rho_name[[i]] %in% name)) {
         j <- min(match(rho_name[[i]], name))
         id$rho[[i]] <- id$rho[[i]][, idx[[j]]]
      }
   }

   rid <- unlist(id, use.names = FALSE)
   par <- x$par
   par[] <- x$par[rid]
   logit <- x$logit
   logit[] <- x$logit[rid]
   arg <- x$arg
   arg$fix0 <- arg$fix0[rid]
   fix2zero <- which(arg$fix0)
   ref <- arg$ref
   ref_idx <- cumsum(ref)
   while (any(cond <- ref_idx %in% arg$fix2zero)) {
      ref[cond] <- ref[cond] - 1
      ref_idx[cond] <- ref_idx[cond] - 1
   }
   arg$ref <- ref
   arg$ref_idx <- ref_idx

   etc <- calcModel(
      attr(x$mf, "y"), arg$nobs, arg$nvar, unlist(arg$nlev),
      par, arg$fix0, arg$ref - 1, arg$nlv, arg$nrl, arg$nlf,
      arg$npi, arg$ntau, arg$nrho, arg$ul, arg$vl,
      arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
      arg$nc, arg$nk, arg$nl, arg$ncl,
      arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
   )

   skeleton <- x$skeleton
   par_index <- utils::relist(paste0("(", seq_along(arg$id), ")"),
                       skeleton$par)

   score <- utils::relist(etc$score, skeleton$score)
   score <- t(do.call(rbind, score))
   dimnames(score) <- dimnames(x$score)

   post <- utils::relist(exp(etc$post), skeleton$post)
   joint <- utils::relist(exp(etc$joint), skeleton$joint)

   x$arg <- arg
   x$par <- par
   x$logit <- logit
   x$fix2zero <- fix2zero
   x$score <- score
   x$posterior <- list(
      marginal = lapply(post, t), joint = joint
   )

   x
}
