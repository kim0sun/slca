library(magrittr)
nlsy_smoke <- slca(smk98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
   estimate(data = nlsy97)
summary(nlsy_smoke)

\dontrun{
# JLCA
model_jlca <- slca(
   smk98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   drk98(3) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   mrj98(3) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   substance(4) ~ smk98 + drk98 + mrj98
) %>% estimate(data = nlsy97)
summary(model_jlca)
param(model_jlca)

# JLCPA
nlsy_jlcpa <- slca(
   smk98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   drk98(3) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   mrj98(3) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   use98(5) ~ smk98 + drk98 + mrj98,
   smk03(3) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
   drk03(3) ~ EDRK_03 + CDRK_03 + WDRK_03 + BDRK_03,
   mrj03(3) ~ EMRJ_03 + CMRJ_03 + OMRJ_03 + SMRJ_03,
   use03(5) ~ smk03 + drk03 + mrj03,
   smk08(3) ~ ESMK_08 + FSMK_08 + DSMK_08 + HSMK_08,
   drk08(3) ~ EDRK_08 + CDRK_08 + WDRK_08 + BDRK_08,
   mrj08(3) ~ EMRJ_08 + CMRJ_08 + OMRJ_08 + SMRJ_08,
   use08(5) ~ smk08 + drk08 + mrj08,
   prof(4) ~ use98 + use03 + use08,
   constraints = list(
      c("smk98", "smk03", "smk08"),
      c("drk98", "drk03", "drk08"),
      c("mrj98", "mrj03", "mrj08"),
      c("use98 ~ smk98", "use03 ~ smk03", "use08 ~ smk08"),
      c("use98 ~ drk98", "use03 ~ drk03", "use08 ~ drk08"),
      c("use98 ~ mrj98", "use03 ~ mrj03", "use08 ~ mrj08")
   )
) %>% estimate(nlsy97, control = list(nrep = 30, test.iter = 1000))
}
nlsy_jlcpa %>% param
