#' Estimate Parameters of an `slca` Object
#'
#' Estimates the parameters of a model created using the `slca` function.
#'
#' @aliases estimate estimate.slca
#' @usage
#' estimate(x, ...)
#'
#' \method{estimate}{slca}(x,
#'    data,
#'    method = c("em", "hybrid", "nlm"),
#'    fix2zero = NULL,
#'    control = slcaControl(), ...)
#'
#' @param x an `slca` object defining the `slca` model to be estimated.
#' @param data a `data.frame` containing the observed categorical variables included in the model.
#' @param method a character string specifying the estimation method for SLCM parameters. The default is `"em"`, which uses the expectation-maximization (EM) algorithm. The alternative `"nlm"` employs the Newton-Raphson algorithm via the `nlm` function, while `"hybrid"` combines both approaches, starting with EM and finishing with `nlm` for refined estimates.
#' @param fix2zero a `vector` specifying parameters to be constrained to zero. See the 'Details' section for further information.
#' @param control a `list` of control parameters for the estimation procedure. Modify default values using [slcaControl()].
#' @param ... additional arguments passed to the estimation process.
#'
#' @details
#' The `fix2zero` argument allows you to constrain specific parameters to zero. Each parameter is associated with a unique index, which can be identified using the \link[slca]{param} function with the argument `index = TRUE`. To apply constraints, provide the relevant parameter indices in the `fix2zero` arguments with vector.
#'
#' @returns
#' An object of class `slcafit` containing the following components:
#' \item{model}{a `list` describing of the model structure.}
#' \item{method}{the estimation method used.}
#' \item{arg}{a brief description of the model used during estimation.}
#' \item{mf}{the `data.frame` used for estimation.}
#' \item{par}{the log of the estimated paramters.}
#' \item{logit}{the log-odds of the estimated parameters.}
#' \item{score}{the score function for the estimated parameters.}
#' \item{posterior}{a `list` of posterior probablities for each latent class variable.}
#' \item{convergence}{a logical indicator of whether convergence was achieved.}
#' \item{loglikelihood}{the loglikelihood value of the estimated model.}
#' \item{control}{the control settings used during the estimation process.}
#'
#' The returned object can be further processed using the \link[slca]{param} function to extract the estimated parameters or their standard errors. The \link[slca]{regress} function allows for logistic regression analysis using a three-step approach to evaluate the effects of external variables on latent class variables. Additionally, several other methods are available, including \link[slca]{predict.slcafit}, \link[slca]{reorder.slcafit}, \link[slca]{gof}, and others.
#'
#' @example man/examples/estimate.R
#'
#' @seealso [slca()] [param()] [slcaControl()]
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
   else if (inherits(x, "slcafit"))
      mf <- x$mf
   else {
      data = parent.frame()
      mf <- proc_data(data, x$model, na.rm)
   }

   arg <- arg_mf(x$model, x$arg, mf, fix2zero)

   if (inherits(x, "slcafit")) par <- x$par
   if (!is.null(control$init.param)) {
      init.param <- unlist(control$init.param)
      if (all(init.param >= 0))
         par <- unlist(tapply(init.param, arg$id, norm1), use.names = FALSE)
      else
         par <- unlist(tapply(init.param, arg$id, norm2), use.names = FALSE)
   }
   if (!inherits(x, "slcafit") &&
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

   class(x) <- c("slcafit", "slca")
   x
}


