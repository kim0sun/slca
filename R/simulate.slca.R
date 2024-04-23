#' Simulate data from \code{slca} model.
#'
#' This function simulates data from a constructed \code{slca} model. If the model is not already estimated, parameters can be specified by the user or generated randomly.
#'
#' @param object a \code{slca} object representing the model from which data will be simulated.
#' @param nsim the number of response observations to simulate. Defaults to 500.
#' @param seed a random seed for reproducibility of the
#' @param parm a set of parameters provided by the user to guide the simulation, if the model has not been estimated.
#' @param nlevel the number of levels for each manifest item declared in the model. If not provided, the default is 2.
#' @param ... additional arguments.
#'
#' @returns
#' A `list` of two components:
#' \item{class}{A `data.frame` providing the assigned latent class for each individual across different latent class variables.}
#' \item{response}{A `data.frame` containing the manifest items that were simulated.}
#'
#' @example man/examples/sim.R
#'
#' @exportS3Method stats::simulate slca
simulate.slca <- function(
   object, nsim = 500, seed = NULL, parm, nlevel, ...
) {
   model <- object$model
   arg <- object$arg

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

   if (inherits(object, "estimated")) {
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
      if (length(parm) == length(arg$id)) {
         par <- unlist(tapply(par, arg$id, norm1), use.names = FALSE)
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
   items <- unlist(model$latent[model$latent$leaf, "children"],
                   use.names = FALSE)
   colnames(y) <- items
   y[] <- lapply(items, function(x)
      factor(y[[x]], labels = level[[x]]))
   mf <- proc_data(y, model, FALSE)

   class <- data.frame(sim$class + 1)
   colnames(class) <- row.names(model$latent)
   rownames(class) <- row.names(mf)

   list(class = class, response = mf)
}
