#' Simulate data from \code{slca} model.
#' This function simulate data from constructed \code{slca} model.
#' If the object is not estimated, the parameters can be given by user or can be generated at random.
#'
#' @param object a \code{slca} object
#' @param nsim number of response observation to be simulated. Defaults to 500.
#' @param seed random seed.
#' @param parm parameter set by users.
#' @param nlevel number of levels for each manifest items declared in the model.
#' @param ... additional arguments.
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
