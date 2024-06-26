# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

log_add_exp <- function(x, y) {
    .Call(`_slca_log_add_exp`, x, y)
}

log_sum_exp <- function(x) {
    .Call(`_slca_log_sum_exp`, x)
}

calcModel <- function(y, nobs, nvar, nlev, par, fix0, ref, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, tr, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho) {
    .Call(`_slca_calcModel`, y, nobs, nvar, nlev, par, fix0, ref, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, tr, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho)
}

em_est <- function(y, nobs, nvar, nlev, par_origin, fix0, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, tr, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho, max_iter, tol, verbose) {
    .Call(`_slca_em_est`, y, nobs, nvar, nlev, par_origin, fix0, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, tr, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho, max_iter, tol, verbose)
}

fll <- function(y, par, nobs, nvar, nlev, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, tr, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho) {
    .Call(`_slca_fll`, y, par, nobs, nvar, nlev, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, tr, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho)
}

calcfreq <- function(mis, nrep, nmis, freq, xobs, nc, N, tol, max_iter) {
    .Call(`_slca_calcfreq`, mis, nrep, nmis, freq, xobs, nc, N, tol, max_iter)
}

simModel <- function(nobs, nvar, nlev, par, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho) {
    .Call(`_slca_simModel`, nobs, nvar, nlev, par, nlv, nrl, nlf, npi, ntau, nrho, ul, vl, lf, rt, eqrl, eqlf, nc, nk, nl, ncl, nc_pi, nk_tau, nl_tau, nc_rho, nr_rho)
}

