#' Add Health data about adolescents delinquency.
#'
#' This dataset contains responses from the National Longitudinal Study of Adolescent Health (Add Health) study.
#' The subjects were in Grades 10 and 11 during the 1994-1995 academic year and provided data on at least one variable measuring adolescent delinquency at Wave 1.
#' The latent class models can be fitted using this dataset to replicate the analysis conducted by Collins and Lanza (2008).\cr
#' The data includes two covariates: grade and sex of respondents.
#' The variables measuring adolescent delinquency include: lying to parents about where or with whom they were, acting loud, rowdy, or unruly in public, damaging property, stealing something from a store, stealing something worth less than $50, and taking part in a group fight.
#' These variables were originally coded as "Never", “1-2 times,” “3-4 times,” or “5 or more times.”
#' In this dataset, those responses are recoded to "No" for "Never" and "Yes" for all other responses from both Waves I and II, providing a longitudinal pattern of adolescent delinquency.
#' Variables ending in "w1" were obtained from Wave I, while variables ending in "w2" were obtained from Wave II.
#'
#' @name addhealth
#' @format A data frame with 2087 rows and 14 variables:
#' \describe{
#'   \item{\code{GRADE}}{Respondent's grade level at Wave I}
#'   \item{\code{SEX}}{Respondent's sex \cr levels: (1)\code{Male}, (2)\code{Female}}
#'   \item{\code{LIEw1, LIEw2}}{Lying to parents about where or with whom they were}
#'   \item{\code{LOUDw1, LOUDw2}}{Acting loud, rowdy, or unruly in public}
#'   \item{\code{DAMAGEw1, DAMAGEw2}}{Damaging property}
#'   \item{\code{SHOPLIFTw1, SHOPLIFTw2}}{Stealing something from a store}
#'   \item{\code{STEALw1, STEALw2}}{Stealing something worth less than $50}
#'   \item{\code{FIGHTw1, FIGHTw2}}{Taking part in a group fight}
#' }
#'
#' @source \link{https://addhealth.cpc.unc.edu/data/#public-use}
#'
#' @references
#' Collins, L.M., & Lanza, S.T. (2009). Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences.
#'
#' J.R. Udry. The National Longitudinal Study of Adolescent Health (Add Health), Waves I & II, 1994-1996 [machine-readable data file and documentation]. Carolina Population Center, University of North Carolina at Chapel Hill, Chapel Hill, NC, 2003.
#'
#' @example man/examples/addhealth.R
#'
NULL
