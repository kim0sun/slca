# Adolescent Depression Data from the Add Health Study

This dataset contains responses from the National Longitudinal Study of
Adolescent Health (Add Health), focusing on adolescents' experiences
with depression. The subjects, who were in Grades 10 and 11 during the
1994–1995 academic year, provided data on at least one measure of
adolescent delinquency in Wave I.\
These data can be used to replicate the latent class analysis conducted
by Collins and Lanza (2009).\
The dataset includes five covariates, notably grade level and sex of
respondents, along with variables capturing depressive emotions: sadness
(`S1-S4`), feeling disliked (`D1-D2`), and feelings of failure
(`F1-F2`).\
Responses for these variables were initially categorized as "Never,"
"Sometimes," "Often," or "Most or All of the Time." In this dataset,
responses have been recoded as "No" for "Never" and "Yes" for all other
responses, providing a longitudinal perspective on adolescent depression
across Waves I and II. Variables with the suffix `"w1"` are from Wave I,
while those with the suffix `"w2"` are from Wave II.

## Usage

``` r
addhealth
```

## Format

A data frame with 2061 rows and 18 variables:

- `GRADE`:

  Respondent's grade level at Wave I.

- `SEX`:

  Respondent's sex\
  levels: (1)`Male`, (2)`Female`.

- `S1w1`, `S1w2`:

  I felt that I could not shake off the blues even with help from my
  family and friends.

- `S2w1`, `S2w2`:

  I felt depressed.

- `S3w1`, `S3w2`:

  I felt lonely.

- `S4w1`, `S4w2`:

  I felt sad.

- `D1w1`, `D1w2`:

  People were unfriendly to me.

- `D2w1`, `D2w2`:

  I felt that people disliked me

- `F1w1`, `F1w2`:

  I thought my life had been a failure.

- `F2w1`, `F2w2`:

  I felt life was not worth living

## Source

<https://addhealth.cpc.unc.edu/data/#public-use>

## References

Collins, L.M., & Lanza, S.T. (2009). Latent Class and Latent Transition
Analysis: With Applications in the Social, Behavioral, and Health
Sciences.

J.R. Udry. The National Longitudinal Study of Adolescent Health (Add
Health), Waves I & II, 1994-1996. Carolina Population Center, University
of North Carolina at Chapel Hill, Chapel Hill, NC, 2003.

## Examples

``` r
library(magrittr)
#> Warning: package ‘magrittr’ was built under R version 4.5.2
data <- addhealth[1:200,]
lta5 <- slca(
   DEP1(5) ~ S1w1 + S2w1 + S3w1 + S4w1 + D1w1 + D2w1 + F1w1 + F2w1,
   DEP2(5) ~ S1w2 + S2w2 + S3w2 + S4w2 + D1w2 + D2w2 + F1w2 + F2w2,
   DEP1 ~ DEP2
) %>% estimate(data, control = list(em.tol = 1e-6))
lta5inv <- slca(
   DEP1(5) ~ S1w1 + S2w1 + S3w1 + S4w1 + D1w1 + D2w1 + F1w1 + F2w1,
   DEP2(5) ~ S1w2 + S2w2 + S3w2 + S4w2 + D1w2 + D2w2 + F1w2 + F2w2,
   DEP1 ~ DEP2,
   constraints = c("DEP1", "DEP2")
) %>% estimate(data, control = list(em.tol = 1e-6))

compare(lta5inv, lta5, test = "chisq")
#> Analysis of Relative Model Fit
#> 
#> Model H0: lta5inv
#> Model H1: lta5
#>          Df  logLik    AIC    BIC    Gsq Res. Df Pr(>Chi)
#> lta5inv  64 -1401.5 2930.9 3142.0                        
#> lta5    104 -1381.0 2969.9 3312.9 41.037      40   0.4249
lta5inv %>% param()
#> PI :
#> (DEP1)
#>   class
#>          1       2       3       4       5
#>     0.2020  0.0854  0.1715  0.3575  0.1836
#> 
#> TAU :
#> (A)
#>      parent
#> child       1       2       3       4       5
#>     1  0.3655  0.2843  0.0190  0.0633  0.0000
#>     2  0.0111  0.5074  0.0000  0.0908  0.0004
#>     3  0.1000  0.2083  0.2564  0.1426  0.1819
#>     4  0.1893  0.0000  0.7246  0.7033  0.0528
#>     5  0.3341  0.0000  0.0000  0.0000  0.7649
#>            
#> parent DEP1
#> child  DEP2
#> 
#> RHO :
#> (a)
#>         class
#> response       1       2       3       4       5
#>    1(V1)  0.1337  1.0000  0.2831  0.0526  0.7210
#>    2      0.8663  0.0000  0.7169  0.9474  0.2790
#>    1(V2)  0.2130  0.9334  0.4670  0.0606  0.8572
#>    2      0.7870  0.0666  0.5330  0.9394  0.1428
#>    1(V3)  0.2700  0.5473  0.5630  0.0489  0.8124
#>    2      0.7300  0.4527  0.4370  0.9511  0.1876
#>    1(V4)  0.3824  1.0000  0.6800  0.1069  0.9277
#>    2      0.6176  0.0000  0.3200  0.8931  0.0723
#>    1(V5)  0.6583  0.1145  0.0734  0.0977  0.7571
#>    2      0.3417  0.8855  0.9266  0.9023  0.2429
#>    1(V6)  0.6629  0.3061  0.0000  0.0181  0.9439
#>    2      0.3371  0.6939  1.0000  0.9819  0.0561
#>    1(V7)  0.1471  0.4382  0.0601  0.0000  0.5709
#>    2      0.8529  0.5618  0.9399  1.0000  0.4291
#>    1(V8)  0.1548  0.1406  0.0459  0.0000  0.3758
#>    2      0.8452  0.8594  0.9541  1.0000  0.6242
#> 
#>      V1   V2   V3   V4   V5   V6   V7   V8  
#> DEP1 S1w1 S2w1 S3w1 S4w1 D1w1 D2w1 F1w1 F2w1
#> DEP2 S1w2 S2w2 S3w2 S4w2 D1w2 D2w2 F1w2 F2w2
```
