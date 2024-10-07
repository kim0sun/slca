#' Adolescent Depression Data from the Add Health Study
#'
#' This dataset includes responses from the National Longitudinal Study of Adolescent Health (Add Health),
#' focusing on adolescents' experiences with depression. The subjects, who were in Grades 10 and 11 during
#' the 1994-1995 academic year, provided data on at least one measure of adolescent delinquency in Wave 1.
#' These data can be used to replicate the latent class analysis conducted by Collins and Lanza (2008).\cr
#' The dataset includes five covariates, notably grade level and sex of respondents, along with variables
#' capturing depressive emotions: sadness (\code{S1-S4}), feeling disliked (\code{D1-D2}), and feelings of failure (\code{F1-F2}).
#'
#' Responses for these variables were initially categorized as "Never," "Sometimes," "Often," or "Most or All of the Time."
#' In this dataset, responses are recoded as "No" for "Never" and "Yes" for all other responses, providing
#' a longitudinal perspective on adolescent depression from Waves I and II. Variables with suffix \code{"w1"} are from Wave I,
#' and those with suffix \code{"w2"} are from Wave II.
#'
#' @name addhealth
#' @format A data frame with 2061 rows and 18 variables:
#' \describe{
#'   \item{\code{GRADE}}{Respondent's grade level at Wave I}
#'   \item{\code{SEX}}{Respondent's sex \cr levels: (1)\code{Male}, (2)\code{Female}}
#'   \item{\code{S1w1, S1w2}}{I felt that I could not shake off the blues even with help from my family and friends.}
#'   \item{\code{S2w1, S2w2}}{I felt depressed.}
#'   \item{\code{S3w1, S3w2}}{I felt lonely.}
#'   \item{\code{S4w1, S4w2}}{I felt sad.}
#'   \item{\code{D1w1, D1w2}}{People were unfriendly to me.}
#'   \item{\code{D2w1, D2w2}}{I felt that people disliked me}
#'   \item{\code{F1w1, F1w2}}{I thought my life had been a failure.}
#'   \item{\code{F2w1, F2w2}}{I felt life was not worth living}
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
