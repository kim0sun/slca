#' GSS 1976-1977 Data on Social Status and Tolerance towards Minorities
#'
#' This dataset contains responses from the General Social Survey (GSS) for the years 1976 and 1977, focusing on social status and tolerance towards minorities
#' The latent class models can be fitted using this dataset replicate the analysis carried on McCutcheon (1985) and Bakk et al. (2014). \cr
#' The data contains some covariates including year of the interview, age, sex, race, degree, and income of respondents.
#' The variables associating social status include father's occupation and education level, and mother's education level, while the variables associating tolerance towards minorities are created by agreeing three related questions: (1) allowing public speaking, (2) allowing teaching, and (3) allowing literatures.
#'
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
"gss7677"


#' NLSY97 Substance Use Data
#'
#' A dataset containing substance use behavior from the National Longitudinal
#' Survey of Youth 1997 (NLSY97) for three years: 1998, 2003, and 2008.
#' The dataset focuses on the youth born in 1984 and tracks
#' three substance use behaviors: tobacco/cigarette smoking, alcohol drinking,
#' and marijuana use.
#'
#' @format A data frame with 1004 rows and 38 columns:
#' \describe{
#'   \item{SEX}{Respondent's sex}
#'   \item{RACE}{Respondent's race}
#'   \item{ESMK_98, ESMK_03, ESMK_08}{(Ever smoked) Ever smoked in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{FSMK_98, FSMK_03, FSMK_08}{(Frequent smoke) Monthly smokes in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{DSMK_98, DSMK_03, DSMK_08}{(Daily smoke) Daily smokes in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{HSMK_98, HSMK_03, HSMK_08}{(Heavy smoke) 10+ cigarettes per day in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{EDRK_98, EDRK_03, EDRK_08}{(Ever drunk) Have you ever drunk in 1998, 2003, and 2008? (0: No, 1: Yes)}
#'   \item{CDRK_98, CDRK_03, CDRK_08}{(Current drinker) Monthly drinking in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{WDRK_98, WDRK_03, WDRK_08}{(Weakly drinker) 5+ days drinking in a month in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{BDRK_98, BDRK_03, BDRK_08}{(Binge drinker) 5+ drinks on the same day at least one time in the last 30 day (0: No, 1: Yes)}
#'   \item{EMRJ_98, EMRJ_03, EMRJ_08}{(Ever marijuana used) Have you ever used marijuana in 1998, 2003, and 2008? (0: No, 1: Yes)}
#'   \item{CMRJ_98, CMRJ_03, CMRJ_08}{(Corrent marijuana user) Monthly marijuana use in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{OMRJ_98, OMRJ_03, OMRJ_08}{(Occasional marijuana user) 10+ days marijuana use in a month in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{SMRJ_98, SMRJ_03, SMRJ_08}{(School/work marijuana user) Marijuana use before/during school or work in 1998, 2003, and 2008 (0: No, 1: Yes)}
#' }
#' Similar naming conventions apply for the years 2003 and 2008, replacing '98' with '03' and '08', respectively.
#'
#' @source National Longitudinal Survey of Youth 1997 (NLSY97)
#'
#' @references Bureau of Labor Statistics, U.S. Department of Labor. National Longitudinal Survey of Youth 1997 cohort, 1997-2017 (rounds 1-18). Produced and distributed by the Center for Human Resource Research (CHRR), The Ohio State University. Columbus, OH: 2019.
#'
#' @example man/examples/nlsy97.R
"nlsy97"

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
#' @format A data frame with 2061 rows and 18 variables:
#' \describe{
#'   \item{\code{GRADE}}{Respondent's grade level at Wave I}
#'   \item{\code{SEX}}{Respondent's sex \cr levels: (1)\code{Male}, (2)\code{Female}}
#'   \item{\code{S1w1}, \code{S1w2}}{I felt that I could not shake off the blues even with help from my family and friends.}
#'   \item{\code{S2w1}, \code{S2w2}}{I felt depressed.}
#'   \item{\code{S3w1}, \code{S3w2}}{I felt lonely.}
#'   \item{\code{S4w1}, \code{S4w2}}{I felt sad.}
#'   \item{\code{D1w1}, \code{D1w2}}{People were unfriendly to me.}
#'   \item{\code{D2w1}, \code{D2w2}}{I felt that people disliked me}
#'   \item{\code{F1w1}, \code{F1w2}}{I thought my life had been a failure.}
#'   \item{\code{F2w1}, \code{F2w2}}{I felt life was not worth living}
#' }
#'
#' @source \url{https://addhealth.cpc.unc.edu/data/#public-use}
#'
#' @references
#' Collins, L.M., & Lanza, S.T. (2009). Latent Class and Latent Transition Analysis: With Applications in the Social, Behavioral, and Health Sciences.
#'
#' J.R. Udry. The National Longitudinal Study of Adolescent Health (Add Health), Waves I & II, 1994-1996. Carolina Population Center, University of North Carolina at Chapel Hill, Chapel Hill, NC, 2003.
#'
#' @example man/examples/addhealth.R
#'
"addhealth"
