# NLSY97 Substance Use Data

This dataset contains substance use behavior data from the National
Longitudinal Survey of Youth 1997 (NLSY97) for three years: 1998, 2003,
and 2008. The dataset focuses on youth born in 1984 and tracks three
types of substance use behaviors: tobacco/cigarette smoking, alcohol
drinking, and marijuana use.

## Usage

``` r
nlsy97
```

## Format

A data frame with 1004 rows and 38 columns:

- `SEX`:

  Respondent's sex

- `RACE`:

  Respondent's race

- `ESMK_98`, `ESMK_03`, `ESMK_08`:

  (Ever smoked) Ever smoked in 1998, 2003, and 2008 (0: No, 1: Yes)

- `FSMK_98`, `FSMK_03`, `FSMK_08`:

  (Frequent smoke) Monthly smoking in 1998, 2003, and 2008 (0: No, 1:
  Yes)

- `DSMK_98`, `DSMK_03`, `DSMK_08`:

  (Daily smoke) Daily smoking in 1998, 2003, and 2008 (0: No, 1: Yes)

- `HSMK_98`, `HSMK_03`, `HSMK_08`:

  (Heavy smoke) 10+ cigarettes per day in 1998, 2003, and 2008 (0: No,
  1: Yes)

- `EDRK_98`, `EDRK_03`, `EDRK_08`:

  (Ever drunk) Ever drunk in 1998, 2003, and 2008? (0: No, 1: Yes)

- `CDRK_98`, `CDRK_03`, `CDRK_08`:

  (Current drinker) Monthly drinking in 1998, 2003, and 2008 (0: No, 1:
  Yes)

- `WDRK_98`, `WDRK_03`, `WDRK_08`:

  (Weakly drinker) 5+ days drinking in a month in 1998, 2003, and 2008
  (0: No, 1: Yes)

- `BDRK_98`, `BDRK_03`, `BDRK_08`:

  (Binge drinker) 5+ drinks on the same day at least one time in the
  last 30 day (0: No, 1: Yes)

- `EMRJ_98`, `EMRJ_03`, `EMRJ_08`:

  (Ever marijuana used) Have you ever used marijuana in 1998, 2003, and
  2008? (0: No, 1: Yes)

- `CMRJ_98`, `CMRJ_03`, `CMRJ_08`:

  (Current marijuana user) Monthly marijuana use in 1998, 2003, and 2008
  (0: No, 1: Yes)

- `OMRJ_98`, `OMRJ_03`, `OMRJ_08`:

  (Occasional marijuana user) 10+ days marijuana use in a month in 1998,
  2003, and 2008 (0: No, 1: Yes)

- `SMRJ_98`, `SMRJ_03`, `SMRJ_08`:

  (School/work marijuana user) Marijuana use before/during school or
  work in 1998, 2003, and 2008 (0: No, 1: Yes)

## Source

National Longitudinal Survey of Youth 1997 (NLSY97)

## References

Bureau of Labor Statistics, U.S. Department of Labor. National
Longitudinal Survey of Youth 1997 cohort, 1997-2017 (rounds 1-18).
Produced and distributed by the Center for Human Resource Research
(CHRR), The Ohio State University. Columbus, OH: 2019.

## Examples

``` r
library(magrittr)
nlsy_smoke <- slca(SMK_98(3) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98) %>%
   estimate(data = nlsy97, control = list(verbose = FALSE))
summary(nlsy_smoke)
#> Structural Latent Class Model
#> 
#> Summary of model structure
#>                                    
#>  Number of manifest variables     4
#>  Number of latent class variables 1
#> 
#>  Latent variables (Root*):                
#>   Label: SMK_98*
#>  nclass: 3      
#> 
#>  Measurement model:                                                     
#>   SMK_98 -> { ESMK_98, FSMK_98, DSMK_98, HSMK_98 }  a
#> 
#> 
#> Summary of manifest variables
#> 
#>  Categories for each variable:
#>           response
#>             1    2 
#>    ESMK_98  Yes  No
#>    FSMK_98  Yes  No
#>    DSMK_98  Yes  No
#>    HSMK_98  Yes  No
#> 
#>  Frequencies for each categories:
#>           response
#>               1    2  <NA>
#>    ESMK_98  558  446     0
#>    FSMK_98  413  591     0
#>    DSMK_98  179  825     0
#>    HSMK_98  115  889     0
#> 
#> 
#> Summary of model fit
#>                                             
#>  Number of observations                 1004
#>  Number of free parameters                14
#>  Log-likelihood                    -1473.365
#>  Information criteria                       
#>    Akaike (AIC)                     2974.731
#>    Bayesian (BIC)                   3043.495
#>  Chi-squared Tests                          
#>    Residual degree of freedom (df)         1
#>    Pearson Chi-squared (X-squared)     0.000
#>      P(>Chi)                           1.000
#>    Likelihood Ratio (G-squared)        0.000
#>      P(>Chi)                           1.000


# A reproducible subset and two-class models keep these syntax examples small.
# Choose class counts and fit the full data separately for substantive inference.
set.seed(20261017)
example_data <- nlsy97[sample.int(nrow(nlsy97), 500), ]
# JLCA
model_jlca <- slca(
   SMK_98(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   DRK_98(2) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   MRJ_98(2) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   SUB_98(2) ~ SMK_98 + DRK_98 + MRJ_98
) %>% estimate(data = example_data, control = slcaControl(em.tol = 1e-6))
summary(model_jlca)
#> Structural Latent Class Model
#> 
#> Summary of model structure
#>                                     
#>  Number of manifest variables     12
#>  Number of latent class variables  4
#> 
#>  Latent variables (Root*):                                     
#>   Label: SMK_98 DRK_98 MRJ_98 SUB_98*
#>  nclass: 2      2      2      2      
#> 
#>  Measurement model:                                                     
#>   SMK_98 -> { ESMK_98, FSMK_98, DSMK_98, HSMK_98 }  a
#>   DRK_98 -> { EDRK_98, CDRK_98, WDRK_98, BDRK_98 }  b
#>   MRJ_98 -> { EMRJ_98, CMRJ_98, OMRJ_98, SMRJ_98 }  c
#> 
#>  Structural model:                                      
#>   SUB_98 -> { SMK_98, DRK_98, MRJ_98 }
#> 
#>  Dependency constraints:
#>   A                B                C               
#>   SUB_98 -> SMK_98 SUB_98 -> DRK_98 SUB_98 -> MRJ_98
#> 
#>  Tree of structural model:                   
#>   SUB_98  -> SMK_98
#>           -> DRK_98
#>           -> MRJ_98
#> 
#> 
#> Summary of manifest variables
#> 
#>  Categories for each variable:
#>           response
#>             1    2 
#>    ESMK_98  Yes  No
#>    FSMK_98  Yes  No
#>    DSMK_98  Yes  No
#>    HSMK_98  Yes  No
#>    EDRK_98  Yes  No
#>    CDRK_98  Yes  No
#>    WDRK_98  Yes  No
#>    BDRK_98  Yes  No
#>    EMRJ_98  Yes  No
#>    CMRJ_98  Yes  No
#>    OMRJ_98  Yes  No
#>    SMRJ_98  Yes  No
#> 
#>  Frequencies for each categories:
#>           response
#>               1    2  <NA>
#>    ESMK_98  280  220     0
#>    FSMK_98  204  296     0
#>    DSMK_98   93  407     0
#>    HSMK_98   52  448     0
#>    EDRK_98  374  126     0
#>    CDRK_98  265  235     0
#>    WDRK_98  110  390     0
#>    BDRK_98  137  363     0
#>    EMRJ_98  191  309     0
#>    CMRJ_98  110  390     0
#>    OMRJ_98   42  458     0
#>    SMRJ_98   44  456     0
#> 
#> 
#> Summary of model fit
#>                                             
#>  Number of observations                  500
#>  Number of free parameters                31
#>  Log-likelihood                    -2156.738
#>  Information criteria                       
#>    Akaike (AIC)                     4375.477
#>    Bayesian (BIC)                   4506.130
#>  Chi-squared Tests                          
#>    Residual degree of freedom (df)      4064
#>    Pearson Chi-squared (X-squared)   492.146
#>      P(>Chi)                           1.000
#>    Likelihood Ratio (G-squared)      419.510
#>      P(>Chi)                           1.000
param(model_jlca)
#> PI :
#> (SUB_98)
#>   class
#>          1       2
#>     0.5853  0.4147
#> 
#> TAU :
#> (A)
#>      parent
#> child       1       2
#>     1  0.1386  0.7883
#>     2  0.8614  0.2117
#>              
#> parent SUB_98
#> child  SMK_98
#> (B)
#>      parent
#> child       1       2
#>     1  0.2782  0.8855
#>     2  0.7218  0.1145
#>              
#> parent SUB_98
#> child  DRK_98
#> (C)
#>      parent
#> child       1       2
#>     1  0.0248  0.4956
#>     2  0.9752  0.5044
#>              
#> parent SUB_98
#> child  MRJ_98
#> 
#> RHO :
#> (a)
#>         class
#> response       1       2
#>    1(V1)  1.0000  0.2568
#>    2      0.0000  0.7432
#>    1(V2)  1.0000  0.0000
#>    2      0.0000  1.0000
#>    1(V3)  0.4559  0.0000
#>    2      0.5441  1.0000
#>    1(V4)  0.2549  0.0000
#>    2      0.7451  1.0000
#> 
#>        V1      V2      V3      V4     
#> SMK_98 ESMK_98 FSMK_98 DSMK_98 HSMK_98
#> (b)
#>         class
#> response       1       2
#>    1(V1)  1.0000  0.4638
#>    2      0.0000  0.5362
#>    1(V2)  1.0000  0.0000
#>    2      0.0000  1.0000
#>    1(V3)  0.4151  0.0000
#>    2      0.5849  1.0000
#>    1(V4)  0.5170  0.0000
#>    2      0.4830  1.0000
#> 
#>        V1      V2      V3      V4     
#> DRK_98 EDRK_98 CDRK_98 WDRK_98 BDRK_98
#> (c)
#>         class
#> response       1       2
#>    1(V1)  1.0000  0.2077
#>    2      0.0000  0.7923
#>    1(V2)  1.0000  0.0000
#>    2      0.0000  1.0000
#>    1(V3)  0.3818  0.0000
#>    2      0.6182  1.0000
#>    1(V4)  0.4000  0.0000
#>    2      0.6000  1.0000
#> 
#>        V1      V2      V3      V4     
#> MRJ_98 EMRJ_98 CMRJ_98 OMRJ_98 SMRJ_98

# JLCPA
nlsy_jlcpa <- slca(
   SMK_98(2) ~ ESMK_98 + FSMK_98 + DSMK_98 + HSMK_98,
   DRK_98(2) ~ EDRK_98 + CDRK_98 + WDRK_98 + BDRK_98,
   MRJ_98(2) ~ EMRJ_98 + CMRJ_98 + OMRJ_98 + SMRJ_98,
   SUB_98(2) ~ SMK_98 + DRK_98 + MRJ_98,
   SMK_03(2) ~ ESMK_03 + FSMK_03 + DSMK_03 + HSMK_03,
   DRK_03(2) ~ EDRK_03 + CDRK_03 + WDRK_03 + BDRK_03,
   MRJ_03(2) ~ EMRJ_03 + CMRJ_03 + OMRJ_03 + SMRJ_03,
   SUB_03(2) ~ SMK_03 + DRK_03 + MRJ_03,
   SMK_08(2) ~ ESMK_08 + FSMK_08 + DSMK_08 + HSMK_08,
   DRK_08(2) ~ EDRK_08 + CDRK_08 + WDRK_08 + BDRK_08,
   MRJ_08(2) ~ EMRJ_08 + CMRJ_08 + OMRJ_08 + SMRJ_08,
   SUB_08(2) ~ SMK_08 + DRK_08 + MRJ_08,
   PROF(2) ~ SUB_98 + SUB_03 + SUB_08,
   constraints = list(
      c("SMK_98", "SMK_03", "SMK_08"),
      c("DRK_98", "DRK_03", "DRK_08"),
      c("MRJ_98", "MRJ_03", "MRJ_08"),
      c("SUB_98 ~ SMK_98", "SUB_03 ~ SMK_03", "SUB_08 ~ SMK_08"),
      c("SUB_98 ~ DRK_98", "SUB_03 ~ DRK_03", "SUB_08 ~ DRK_08"),
      c("SUB_98 ~ MRJ_98", "SUB_03 ~ MRJ_03", "SUB_08 ~ MRJ_08")
   )
) %>% estimate(example_data, control = slcaControl(em.tol = 1e-6))

nlsy_jlcpa$convergence
#>   EM  nlm 
#> TRUE   NA 
```
