#' GSS 1976-1977 Data on Social Status and Tolerance towards Minorities
#'
#' This dataset contains responses from the General Social Survey (GSS) for the years 1976 and 1977, focusing on social status and tolerance towards minorities
#' The latent class models can be fitted using this dataset replicate the analysis carried on McCutcheon (1985) and Bakk et al. (2014). \cr
#' The data contains some covariates including year of the interview, age, sex, race, degree, and income of respondents.
#' The variables associating social status include father's occupation and education level, and mother's education level, while the variables associating tolerance towards minorities are created by agreeing three related questions: (1) allowing public speaking, (2) allowing teaching, and (3) allowing literatures.
#'
#' @name gss7677
#' @format A data frame with 2942 rows and 14 variables:
#' \describe{
#'   \item{\code{YEAR}}{Interview year (1976, 1977)}
#'   \item{\code{COHORT}}{Respondent's age \cr levels: (1)\code{YOUNG}, (2)\code{YOUNG-MIDDLE}, (4)\code{MIDDLE}, (5)\code{OLD}}
#'   \item{\code{SEX}}{Respondent's sex \cr levels: (1)\code{MALE}, (2)\code{FEMALE}}
#'   \item{\code{RACE}}{Respondent's race \cr levels: (1)\code{WHITE} (2)\code{BLACK}, (3)\code{OTHER}}
#'   \item{\code{DEGREE}}{Respondent's degree \cr levels: (1)\code{LT HS}, (2)\code{HIGH-SCH}, (3)\code{HIGHER}}
#'   \item{\code{REALRINC}}{Income of respondents}
#'   \item{\code{PAPRES}}{Father's prestige (occupation) \cr levels: (1)\code{LOW}, (2)\code{MIDIUM}, (2)\code{HIGH}}
#'   \item{\code{PADEG}}{Father's degree \cr levels: (1)\code{LT HS}, (2)\code{HIGH-SCH}, (3)\code{COLLEGE}, (4) \code{BACHELOR}, (5)\code{GRADUATE}}
#'   \item{\code{MADEG}}{Mother's degree \cr levels: (1)\code{LT HS}, (2)\code{HIGH-SCH}, (3)\code{COLLEGE}, (4) \code{BACHELOR}, (5)\code{GRADUATE}}
#'   \item{\code{TOLRAC}}{Tolerance towards racists}
#'   \item{\code{TOLCOM}}{Tolerance towards communists}
#'   \item{\code{TOLHOMO}}{Tolerance towards homosexuals}
#'   \item{\code{TOLATH}}{Tolerance towards atheists}
#'   \item{\code{TOLMIL}}{Tolerance towards militarists}
#' }
#'
#' @source General Social Survey (GSS) 1976, 1977
#'
#' @references Bakk Z, Kuha J. (2021) Relating latent class membership to external variables: An overview. Br J Math Stat Psychol. 74(2):340-362.
#'
#' McCutcheon, A. L. (1985). A latent class analysis of tolerance for nonconformity in the American public. Public Opinion Quarterly, 49, 474–488.
#'
#' @example man/examples/gss7677.R
#'
NULL
