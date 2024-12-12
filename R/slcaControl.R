#' Control Parameters for `slca` Estimation
#'
#' Specifies control parameters for estimating `slca` model.
#'
#' @param em.iterlim an integer specifying the maximum number of iterations allowed for the EM algorithm. The default is `5000`.
#' @param em.tol a numeric value setting the tolerance for convergence of the EM algorithm. The default is `1e-8`.
#' @param nlm.iterlim an integer specifying the maximum number of iterations allowed when using the \code{nlm} function for estimation. The default is `1000`.
#' @param nlm.tol a numeric value setting the tolerance for convergence of the \code{nlm} function. The default is `1e-10`.
#' @param init.param a numeric vector specifying the initial parameter values for estimation.
#' @param nrep an integer specifying the number of estimation trials. The default is `1`.
#' @param test.iter an integer specifying the maximum number of iterations allowed for parameter testing. The default is `500`.
#' @param na.rm a logical value indicating whether to remove observations containing missing values (`NA`). The default is `FALSE`.
#' @param verbose a logical value indicating whether to display progress updates during the estimation process. The default is `FALSE`.
#'
#' A \code{list} containing control parameters for `slca` estimation, including the specified iteration limits, tolerances, and additional options.
#'
#' @seealso [slca]
#'
#' @export
slcaControl <- function(
   em.iterlim = 5000, em.tol = 1e-8,
   nlm.iterlim = 1000, nlm.tol = 1e-10,
   init.param = NULL, nrep = 1, test.iter = 500,
   na.rm = FALSE, verbose = FALSE
) {
   ctrl <- list(
      em.iterlim = em.iterlim, em.tol = em.tol,
      nlm.iterlim = nlm.iterlim, nlm.tol = nlm.tol,
      init.param = init.param,
      nrep = nrep, test.iter = test.iter,
      na.rm = na.rm, verbose = verbose
   )

   class(ctrl) <- "slcaControl"
   ctrl
}
