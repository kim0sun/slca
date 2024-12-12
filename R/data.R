#' GSS 1976-1977 Data on Social Status and Tolerance towards Minorities
#'
#' This dataset contains responses from the General Social Survey (GSS) for the years 1976 and 1977, focusing on social status and tolerance towards minorities.
#' The dataset can be used to replicate the analyses conducted in McCutcheon (1985) and Bakk et al. (2014). \cr
#' It includes covariates such as interview year, age, sex, race, education level, and income. Social status-related variables include father's occupation and education level, as well as mother's education level. Tolerance towards minorities is measured by agreement with three questions: (1) allowing public speaking, (2) allowing teaching, and (3) allowing literature publication.
#'
#' @format A data frame with 2942 rows and 14 variables:
#' \describe{
#'   \item{\code{YEAR}}{Interview year (1976, 1977).}
#'   \item{\code{COHORT}}{Respondent's age cohort.\cr Levels: (1) \code{YOUNG}, (2) \code{YOUNG-MIDDLE}, (4) \code{MIDDLE}, (5) \code{OLD}.}
#'   \item{\code{SEX}}{Respondent's sex.\cr Levels: (1) \code{MALE}, (2) \code{FEMALE}.}
#'   \item{\code{RACE}}{Respondent's race.\cr Levels: (1) \code{WHITE}, (2) \code{BLACK}, (3) \code{OTHER}.}
#'   \item{\code{DEGREE}}{Respondent's education level.\cr Levels: (1) \code{LT HS}, (2) \code{HIGH-SCH}, (3) \code{HIGHER}.}
#'   \item{\code{REALRINC}}{Respondent's income.}
#'   \item{\code{PAPRES}}{Father's occupational prestige.\cr Levels: (1) \code{LOW}, (2) \code{MEDIUM}, (3) \code{HIGH}.}
#'   \item{\code{PADEG}}{Father's education level.\cr Levels: (1) \code{LT HS}, (2) \code{HIGH-SCH}, (3) \code{COLLEGE}, (4) \code{BACHELOR}, (5) \code{GRADUATE}.}
#'   \item{\code{MADEG}}{Mother's education level.\cr Levels: (1) \code{LT HS}, (2) \code{HIGH-SCH}, (3) \code{COLLEGE}, (4) \code{BACHELOR}, (5) \code{GRADUATE}.}
#'   \item{\code{TOLRAC}}{Tolerance towards racists.}
#'   \item{\code{TOLCOM}}{Tolerance towards communists.}
#'   \item{\code{TOLHOMO}}{Tolerance towards homosexuals.}
#'   \item{\code{TOLATH}}{Tolerance towards atheists.}
#'   \item{\code{TOLMIL}}{Tolerance towards militarists.}
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
#' This dataset contains substance use behavior data from the National Longitudinal Survey of Youth 1997 (NLSY97) for three years: 1998, 2003, and 2008.
#' The dataset focuses on youth born in 1984 and tracks three types of substance use behaviors: tobacco/cigarette smoking, alcohol drinking, and marijuana use.
#'
#' @format A data frame with 1004 rows and 38 columns:
#' \describe{
#'   \item{\code{SEX}}{Respondent's sex}
#'   \item{\code{RACE}}{Respondent's race}
#'   \item{\code{ESMK_98}, \code{ESMK_03}, \code{ESMK_08}}{(Ever smoked) Ever smoked in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{FSMK_98}, \code{FSMK_03}, \code{FSMK_08}}{(Frequent smoke) Monthly smoking in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{DSMK_98}, \code{DSMK_03}, \code{DSMK_08}}{(Daily smoke) Daily smoking in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{HSMK_98}, \code{HSMK_03}, \code{HSMK_08}}{(Heavy smoke) 10+ cigarettes per day in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{EDRK_98}, \code{EDRK_03}, \code{EDRK_08}}{(Ever drunk) Ever drunk in 1998, 2003, and 2008? (0: No, 1: Yes)}
#'   \item{\code{CDRK_98}, \code{CDRK_03}, \code{CDRK_08}}{(Current drinker) Monthly drinking in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{WDRK_98}, \code{WDRK_03}, \code{WDRK_08}}{(Weakly drinker) 5+ days drinking in a month in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{BDRK_98}, \code{BDRK_03}, \code{BDRK_08}}{(Binge drinker) 5+ drinks on the same day at least one time in the last 30 day (0: No, 1: Yes)}
#'   \item{\code{EMRJ_98}, \code{EMRJ_03}, \code{EMRJ_08}}{(Ever marijuana used) Have you ever used marijuana in 1998, 2003, and 2008? (0: No, 1: Yes)}
#'   \item{\code{CMRJ_98}, \code{CMRJ_03}, \code{CMRJ_08}}{(Current marijuana user) Monthly marijuana use in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{OMRJ_98}, \code{OMRJ_03}, \code{OMRJ_08}}{(Occasional marijuana user) 10+ days marijuana use in a month in 1998, 2003, and 2008 (0: No, 1: Yes)}
#'   \item{\code{SMRJ_98}, \code{SMRJ_03}, \code{SMRJ_08}}{(School/work marijuana user) Marijuana use before/during school or work in 1998, 2003, and 2008 (0: No, 1: Yes)}
#' }
#'
#' @source National Longitudinal Survey of Youth 1997 (NLSY97)
#'
#' @references Bureau of Labor Statistics, U.S. Department of Labor. National Longitudinal Survey of Youth 1997 cohort, 1997-2017 (rounds 1-18). Produced and distributed by the Center for Human Resource Research (CHRR), The Ohio State University. Columbus, OH: 2019.
#'
#' @example man/examples/nlsy97.R
"nlsy97"

#' Adolescent Depression Data from the Add Health Study
#'
#' This dataset contains responses from the National Longitudinal Study of Adolescent Health (Add Health), focusing on adolescents' experiences with depression. The subjects, who were in Grades 10 and 11 during the 1994–1995 academic year, provided data on at least one measure of adolescent delinquency in Wave I. \cr
#' These data can be used to replicate the latent class analysis conducted by Collins and Lanza (2009).\cr
#' The dataset includes five covariates, notably grade level and sex of respondents, along with variables capturing depressive emotions: sadness (\code{S1-S4}), feeling disliked (\code{D1-D2}), and feelings of failure (\code{F1-F2}). \cr
#' Responses for these variables were initially categorized as "Never," "Sometimes," "Often," or "Most or All of the Time." In this dataset, responses have been recoded as "No" for "Never" and "Yes" for all other responses, providing  a longitudinal perspective on adolescent depression across Waves I and II. Variables with the suffix \code{"w1"} are from Wave I, while those with the suffix \code{"w2"} are from Wave II.
#'
#' @format A data frame with 2061 rows and 18 variables:
#' \describe{
#'   \item{\code{GRADE}}{Respondent's grade level at Wave I.}
#'   \item{\code{SEX}}{Respondent's sex \cr levels: (1)\code{Male}, (2)\code{Female}.}
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
