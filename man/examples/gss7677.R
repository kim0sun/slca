library(magrittr)
data <- gss7677[gss7677$RACE == "BLACK",]
model_stat <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data)
summary(model_stat)
param(model_stat)

model_tol <- slca(tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL) %>%
   estimate(data = data)
summary(model_tol)
param(model_tol)

model_lta <- slca(
   status(3) ~ PAPRES + PADEG + MADEG,
   tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL,
   status ~ tol
) %>% estimate(data = data)
summary(model_lta)
param(model_lta)

regress(model_lta, status ~ SEX, data)
\dontrun{
regress(model_lta, status ~ SEX, data, method = "BCH")
regress(model_lta, status ~ SEX, data, method = "ML")
}
