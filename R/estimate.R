#' Estimate Parameters of `slca` Object
#'
#' Estimate the parameters of model constructed using the `slca` function.
#'
#' @aliases estimate estimate.slca
#' @usage
#' estimate(x, ...)
#'
#' \method{estimate}{slca}(x, data,
#'     method = c("em", "hybrid", "nlm"),
#'     fix2zero = NULL,
#'     control = slcaControl(), ...)
#'
#' @param x an `slca` object defining SLCM model to be estimated.
#' @param data a `data.frame` object containing observed categorical variables incorporated in the model.
#' @param method estimation method for SLCM parameters. The default is `"em"`, which employs expecation-maximization (EM) algorithm for estimation; the alternative `"nlm"`, utilizes `nlm` function for Newton-Raphson algorithm. The `"hybrid"` method begins with the EM algorithm and concludes with the `nlm` function for refined estimation.
#' @param fix2zero a `vector` of parameters to be restricted to zero. The details of restriction is given under 'Details'
#' @param control a `list` of control for the estimation procedure. Used to modify default values in [slcaControl].
#' @param ... additional arguments.
#'
#' @details
#' To constrain certain parameters to zero, use the `fix2zero` argument. Each parameter is associated with a unique index. You can identify the index of a specific parameter by invoking the \link[slca]{param} function with the `index = TRUE` argument. To apply these constraints, include the relevant parameter indices in the `fix2zero` argument.
#'
#' @returns
#' An object of class `slca` and `estimated` with an following elements:
#' \item{model}{a `list` describing of the model.}
#' \item{method}{the method used for estimation}
#' \item{arg}{the brief model description used during the estimation.}
#' \item{mf}{the data.frame used for estimation.}
#' \item{par}{the log of the estimated paramters.}
#' \item{logit}{the log-odds of the estimated parameters.}
#' \item{score}{the score function for the estimated parameters.}
#' \item{posterior}{the `list` of posterior probablities for each latent class variable.}
#' \item{convergence}{a logical indicator of whether convergence was achieved.}
#' \item{loglikelihood}{the loglikelihood of the estimated model.}
#' \item{control}{the control values used during the estimation process.}
#'
#' This returned object can be further processed using the \link[slca]{param} functions to extract the estimated parameters or their respective standard errors. Additionally, the \link[slca]{regress} function enables logistic regression analysis using three-step approach to evaluate the effect of external variables on latent class variables.
#'
#' @seealso \link[slca]{slca} \link[slca]{param} \link[slca]{regress} \link[slca]{slcaControl} \link[slca]{gss7677}, \link[slca]{nlsy97}
#'
#'
#'
#' @export
estimate <- function(x, ...) UseMethod("estimate")

#' @exportS3Method slca::estimate slca
estimate.slca <- function(
   x, data, method = c("em", "hybrid", "nlm"),
   fix2zero = NULL, control = slcaControl(), ...
) {
   method <- match.arg(method)
   if (!inherits(control, "slcaControl")) {
      ctrl <- slcaControl()
      index <- match(names(control), names(ctrl), 0L)
      ctrl[index] <- control[!is.na(index)]
      control <- ctrl
   }
   na.rm <- control$na.rm
   if (!missing(data))
      mf <- proc_data(data, x$model, na.rm)
   else if (inherits(x, "estimated"))
      mf <- x$mf
   else data = parent.frame()

   arg <- arg_mf(x$model, x$arg, mf, fix2zero)

   if (inherits(x, "estimated")) par <- x$par
   if (!is.null(control$init.param)) {
      init.param <- unlist(control$init.param)
      if (all(init.param >= 0))
         par <- unlist(tapply(init.param, arg$id, norm1), use.names = FALSE)
      else
         par <- unlist(tapply(init.param, arg$id, norm2), use.names = FALSE)
   }
   if (!inherits(x, "estimated") &&
       is.null(control$init.param)) {
      if (control$nrep > 1) {
         if (control$verbose)
            cat("Inital parameter test: \n")
         testll <- -Inf
         for (i in 1:control$nrep) {
            if (control$verbose) {
               cat(i, "/", control$nrep, " ")
            }
            init.param <- stats::runif(length(arg$id), 1, 1.1)
            init.param[arg$fix0] <- -Inf
            tpar <- unlist(tapply(init.param, arg$id, norm1),
                           use.names = FALSE)
            em <- em_est(
               attr(mf, "y"), arg$nobs, arg$nvar, unlist(arg$nlev),
               tpar, arg$fix0, arg$nlv, arg$nrl, arg$nlf,
               arg$npi, arg$ntau, arg$nrho,
               arg$ul, arg$vl, arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
               arg$nc, arg$nk, arg$nl, arg$ncl,
               arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho,
               control$test.iter, 0, FALSE
            )
            if (em$ll > testll) {
               par <- em$param
               testll <- em$ll
               best <- i
            }
            if (control$verbose)
               cat("logLik:", em$ll, "\n")
         }
         if (control$verbose) {
            cat("\n", best,
                "th parameter set has been selected.\n\n",
                sep = "")
         }
      } else {
         init.param <- stats::runif(length(arg$id), 1, 1.1)
         init.param[arg$fix0] <- -Inf
         par <- unlist(tapply(init.param, arg$id, norm2), use.names = FALSE)
      }
   }
   par[arg$fix0] <- -Inf
   est <- estModel(method, control, par, mf, arg)
   par <- est$par
   conv <- est$conv
   logit <- par - par[arg$ref_idx[arg$id]]

   etc <- calcModel(
      attr(mf, "y"), arg$nobs, arg$nvar, unlist(arg$nlev),
      par, arg$fix0, arg$ref - 1, arg$nlv, arg$nrl, arg$nlf,
      arg$npi, arg$ntau, arg$nrho, arg$ul, arg$vl,
      arg$lf, arg$tr, arg$rt, arg$eqrl, arg$eqlf,
      arg$nc, arg$nk, arg$nl, arg$ncl,
      arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
   )

   skeleton <- get_frame(x$model, arg, mf)
   par_index <- utils::relist(paste0("(", seq_along(arg$id), ")"),
                       skeleton$par)

   score <- utils::relist(etc$score, skeleton$score)
   score <- t(do.call(rbind, score))
   dimnames(score) <- list(dimnames(mf)[[1]], unlist(par_index))

   post <- utils::relist(exp(etc$post), skeleton$post)
   joint <- utils::relist(exp(etc$joint), skeleton$joint)

   x$method = method
   x$arg <- arg
   x$mf <- mf
   x$par <- par
   x$logit <- logit
   x$fix2zero <- which(arg$fix0)
   x$score <- score
   x$posterior <- list(
      marginal = lapply(post, t), joint = joint
   )
   x$skeleton <- skeleton
   x$convergence <- conv
   x$loglikelihood <- etc$ll
   x$control <- control

   class(x) <- c("slca", "estimated")
   x
}


