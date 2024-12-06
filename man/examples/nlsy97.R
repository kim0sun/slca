library(magrittr)
nlsy_smoke <- slca(SMK_98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
   estimate(data = nlsy97, control = list(verbose = FALSE))
summary(nlsy_smoke)

\donttest{
# JLCA
model_jlca <- slca(
   SMK_98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   DRK_98(3) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   MRJ_98(3) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   SUB_98(4) ~ SMK_98 + DRK_98 + MRJ_98
) %>% estimate(data = nlsy97, control = list(verbose = FALSE))
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
) %>% estimate(nlsy97, control = list(verbose = FALSE))
}
