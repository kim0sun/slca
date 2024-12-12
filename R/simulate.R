#' Simulate Data from an \code{slca} Model
#'
#' Simulates data based on a specified \code{slca} model. If the model parameters are not already estimated, they can either be provided by the user or generated randomly.
##'
#' @param object an \code{slca} object representing the model from which data will be simulated.
#' @param nsim an integer specifying the number of response observations to simulate. The default is 500.
#' @param seed an integer specifying the random seed for reproducibility. If not provided, results will vary across runs.
#' @param parm a user-specified set of parameters to guide the simulation. This is required if the model has not been previously estimated.
#' @param nlevel an integer or integer vector specifying the number of levels for each manifest item in the model. If a single integer is provided, all manifest items will have the same number of levels. The default is 2.
#' @param ... Additional arguments passed to other methods.
#'
#' @returns
#' A `list` with the following components:
#' \item{class}{A `data.frame` containing the assigned latent class for each individual across all latent class variables.}
#' \item{response}{A `data.frame` containing the simulated manifest item responses.}
#'
#' @example man/examples/simulate.R
#'
#' @exportS3Method stats::simulate slca
simulate.slca <- function(
   object, nsim = 500, seed = NULL, parm, nlevel, ...
) {
   if (!is.null(seed)) set.seed(seed)
   model <- object$model
   arg <- object$arg
   arg$nobs <- nsim

   vars <- unlist(arg$vars)
   nvar <- length(vars)
   if (missing(nlevel)) {
      nlevel <- rep(2, nvar)
   } else if (length(nlevel) != nvar) {
      nlev <- rep(2, nvar)
      j <- 1
      for (i in match(names(nlevel), vars)) {
         nlev[i] <- nlevel[j]
         j = j + 1
      }
      nlevel <- nlev
   }
   names(nlevel) <- vars

   if (inherits(object, "slcafit")) {
      level <- levels(object$mf)
      par <- object$par
   } else if (missing(parm)) {
      level <- sapply(unlist(arg$vars, use.names = FALSE), function(x)
         seq_len(nlevel[x]), simplify = FALSE)
      arg <- arg_sim(arg, level, object$fix2zero, nsim)
      par <- stats::runif(length(arg$id))
      par <- unlist(tapply(par, arg$id, norm1), use.names = FALSE)
   } else {
      level <- sapply(unlist(arg$vars, use.names = FALSE), function(x)
         seq_len(nlevel[x]), simplify = FALSE)
      arg <- arg_sim(arg, level, object$fix2zero, nsim)
      if (length(unlist(parm)) == length(arg$id)) {
         par <- unlist(tapply(parm, arg$id, norm1), use.names = FALSE)
      } else {
         par <- stats::runif(length(arg$id))
         par <- unlist(tapply(par, arg$id, norm1), use.names = FALSE)
      }
   }
   sim <- simModel(
      nsim, arg$nvar, arg$nlev, par,
      arg$nlv, arg$nrl, arg$nlf, arg$npi, arg$ntau, arg$nrho,
      arg$ul, arg$vl, arg$lf, arg$rt, arg$eqrl, arg$eqlf,
      arg$nc, arg$nk, arg$nl, arg$ncl,
      arg$nc_pi, arg$nk_tau, arg$nl_tau, arg$nc_rho, arg$nr_rho
   )
   class <- data.frame(sim$class)
   names(class) <- model$latent$label

   # data.name
   y <- data.frame(do.call(cbind, sim$y))
   child <- model$latent[model$latent$leaf, "children"]
   items <- setdiff(unlist(child), names(child))
   colnames(y) <- items
   y[] <- lapply(items, function(x)
      factor(y[[x]], labels = level[[x]]))
   mf <- proc_data(y, model, FALSE)

   class <- data.frame(sim$class + 1)
   colnames(class) <- row.names(model$latent)
   rownames(class) <- row.names(mf)

   skeleton <- get_frame(model, arg, mf)
   param <- utils::relist(exp(par), skeleton$par)

   list(class = class, response = mf, parm = param)
}
