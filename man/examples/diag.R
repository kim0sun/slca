library(magrittr)
data <- gss7677[gss7677$COHORT == "YOUNG", ]
stat2 <- slca(status(2) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data)
stat3 <- slca(status(3) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data)
stat4 <- slca(status(4) ~ PAPRES + PADEG + MADEG) %>%
   estimate(data = data)

gof(stat2, stat3, stat4)
gof(stat2, stat3, stat4, test = "chisq")
\dontrun{
gof(stat2, stat3, stat4, test = "boot")
}

compare(stat3, stat4)
compare(stat3, stat4, test = "chisq")
\dontrun{
compare(stat3, stat4, test = "boot")
}
