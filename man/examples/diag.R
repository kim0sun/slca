library(magrittr)
data <- gss7677[gss7677$COHORT == "YOUNG", ]
stat2 <- slca(status(2) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))
stat3 <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))
stat4 <- slca(status(4) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data, control = list(verbose = FALSE))

gof(stat2, stat3, stat4)
gof(stat2, stat3, stat4, test = "chisq")
\donttest{
gof(stat2, stat3, stat4, test = "boot")
}

compare(stat3, stat4)
compare(stat3, stat4, test = "chisq")
\donttest{
compare(stat3, stat4, test = "boot")
}
