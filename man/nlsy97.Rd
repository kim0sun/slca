% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{nlsy97}
\alias{nlsy97}
\title{NLSY97 Substance Use Data}
\format{
A data frame with 1004 rows and 38 columns:
\describe{
\item{SEX}{Respondent's sex}
\item{RACE}{Respondent's race}
\item{ESMK_98, ESMK_03, ESMK_08}{(Ever smoked) Ever smoked in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{FSMK_98, FSMK_03, FSMK_08}{(Frequent smoke) Monthly smokes in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{DSMK_98, DSMK_03, DSMK_08}{(Daily smoke) Daily smokes in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{HSMK_98, HSMK_03, HSMK_08}{(Heavy smoke) 10+ cigarettes per day in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{EDRK_98, EDRK_03, EDRK_08}{(Ever drunk) Have you ever drunk in 1998, 2003, and 2008? (0: No, 1: Yes)}
\item{CDRK_98, CDRK_03, CDRK_08}{(Current drinker) Monthly drinking in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{WDRK_98, WDRK_03, WDRK_08}{(Weakly drinker) 5+ days drinking in a month in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{BDRK_98, BDRK_03, BDRK_08}{(Binge drinker) 5+ drinks on the same day at least one time in the last 30 day (0: No, 1: Yes)}
\item{EMRJ_98, EMRJ_03, EMRJ_08}{(Ever marijuana used) Have you ever used marijuana in 1998, 2003, and 2008? (0: No, 1: Yes)}
\item{CMRJ_98, CMRJ_03, CMRJ_08}{(Corrent marijuana user) Monthly marijuana use in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{OMRJ_98, OMRJ_03, OMRJ_08}{(Occasional marijuana user) 10+ days marijuana use in a month in 1998, 2003, and 2008 (0: No, 1: Yes)}
\item{SMRJ_98, SMRJ_03, SMRJ_08}{(School/work marijuana user) Marijuana use before/during school or work in 1998, 2003, and 2008 (0: No, 1: Yes)}
}
Similar naming conventions apply for the years 2003 and 2008, replacing '98' with '03' and '08', respectively.
}
\source{
National Longitudinal Survey of Youth 1997 (NLSY97)
}
\usage{
nlsy97
}
\description{
A dataset containing substance use behavior from the National Longitudinal
Survey of Youth 1997 (NLSY97) for three years: 1998, 2003, and 2008.
The dataset focuses on the youth born in 1984 and tracks
three substance use behaviors: tobacco/cigarette smoking, alcohol drinking,
and marijuana use.
}
\examples{
library(magrittr)
nlsy_smoke <- slca(SMK_98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) \%>\%
   estimate(data = nlsy97, control = list(verbose = FALSE))
summary(nlsy_smoke)

\donttest{
# JLCA
model_jlca <- slca(
   SMK_98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   DRK_98(3) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   MRJ_98(3) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   SUB_98(4) ~ SMK_98 + DRK_98 + MRJ_98
) \%>\% estimate(data = nlsy97, control = list(verbose = FALSE))
summary(model_jlca)
param(model_jlca)

# JLCPA
nlsy_jlcpa <- slca(
   SMK_98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   DRK_98(3) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   MRJ_98(3) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   SUB_98(5) ~ SMK_98 + DRK_98 + MRJ_98,
   SMK_03(3) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
   DRK_03(3) ~ EDRK_03 + CDRK_03 + WDRK_03 + BDRK_03,
   MRJ_03(3) ~ EMRJ_03 + CMRJ_03 + OMRJ_03 + SMRJ_03,
   SUB_03(5) ~ SMK_03 + DRK_03 + MRJ_03,
   SMK_08(3) ~ ESMK_08 + FSMK_08 + DSMK_08 + HSMK_08,
   DRK_08(3) ~ EDRK_08 + CDRK_08 + WDRK_08 + BDRK_08,
   MRJ_08(3) ~ EMRJ_08 + CMRJ_08 + OMRJ_08 + SMRJ_08,
   SUB_08(5) ~ SMK_08 + DRK_08 + MRJ_08,
   PROF(4) ~ SUB_98 + SUB_03 + SUB_08,
   constraints = list(
      c("SMK_98", "SMK_03", "SMK_08"),
      c("DRK_98", "DRK_03", "DRK_08"),
      c("MRJ_98", "MRJ_03", "MRJ_08"),
      c("SUB_98 ~ SMK_98", "SUB_03 ~ SMK_03", "SUB_08 ~ SMK_08"),
      c("SUB_98 ~ DRK_98", "SUB_03 ~ DRK_03", "SUB_08 ~ DRK_08"),
      c("SUB_98 ~ MRJ_98", "SUB_03 ~ MRJ_03", "SUB_08 ~ MRJ_08")
   )
) \%>\% estimate(nlsy97, control = list(verbose = FALSE))
}
}
\references{
Bureau of Labor Statistics, U.S. Department of Labor. National Longitudinal Survey of Youth 1997 cohort, 1997-2017 (rounds 1-18). Produced and distributed by the Center for Human Resource Research (CHRR), The Ohio State University. Columbus, OH: 2019.
}
\keyword{datasets}
