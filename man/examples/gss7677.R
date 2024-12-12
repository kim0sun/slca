library(magrittr)
gss500 <- gss7677[1:500,] %>% na.omit
model_stat <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = gss500, control = list(em.tol = 1e-6))
summary(model_stat)
param(model_stat)

model_tol <- slca(tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL) %>%
   estimate(data = gss500, control = list(em.tol = 1e-6))
summary(model_tol)
param(model_tol)

model_lta <- slca(
   status(3) ~ PAPRES + PADEG + MADEG,
   tol(4) ~ TOLRAC + TOLCOM + TOLHOMO + TOLATH + TOLMIL,
   status ~ tol
) %>% estimate(data = gss500, control = list(em.tol = 1e-6))
summary(model_lta)
param(model_lta)

\donttest{
regress(model_lta, status ~ SEX, gss500)
regress(model_lta, status ~ SEX, gss500, method = "BCH")
regress(model_lta, status ~ SEX, gss500, method = "ML")
}
