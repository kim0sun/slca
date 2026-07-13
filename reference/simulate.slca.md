# Simulate Data from an `slca` Model

Simulates data based on a specified `slca` model. If the model
parameters are not already estimated, they can either be provided by the
user or generated randomly.

## Usage

``` r
# S3 method for class 'slca'
simulate(object, nsim = 500, seed = NULL, parm, nlevel, ...)
```

## Arguments

- object:

  an `slca` object representing the model from which data will be
  simulated.

- nsim:

  an integer specifying the number of response observations to simulate.
  The default is 500.

- seed:

  an integer specifying the random seed for reproducibility. If not
  provided, results will vary across runs.

- parm:

  a user-specified set of parameters to guide the simulation. This is
  required if the model has not been previously estimated. If the
  supplied length does not match the model, random parameters are used
  with a warning.

- nlevel:

  an integer or integer vector specifying the number of levels for each
  manifest item in the model. If a single integer is provided, all
  manifest items use that number of levels. Named vectors may specify a
  subset of manifest items, with unspecified items using the default of
  2.

- ...:

  Additional arguments passed to other methods.

## Value

A `list` with the following components:

- class:

  A `data.frame` containing the assigned latent class for each
  individual across all latent class variables.

- response:

  A `data.frame` containing the simulated manifest item responses as
  factors. Internal response encodings used for model calculation are
  not stored on this object.

## Examples

``` r
m1 <- slca(lc1[3] ~ x1 + x2 + x3 + x4 + x5,
              lc2[4] ~ y1 + y2 + y3 + y4 + y5)
sim <- simulate(m1, 1000)
sapply(sim$class, table)
#> $lc1
#> 
#>   1   2   3 
#> 155 347 498 
#> 
#> $lc2
#> 
#>   1   2   3   4 
#> 473  26 257 244 
#> 

# simulate data with defined number of levels of manifest items
m2 <- slca(lc1[3] ~ x1 + x2 + x3 + x4)
sim <- simulate(m2, nlevel = c(3, 3, 3, 3))
d <- sim$response
sapply(d, table)
#>    x1  x2  x3  x4
#> 1 134 148 108  83
#> 2 169 107  86 210
#> 3 197 245 306 207

sim <- simulate(m2, nlevel = c(x1 = 2, x3 = 3, x4 = 4))
d <- sim$response
sapply(d, table)
#> $x1
#> 
#>   1   2 
#> 180 320 
#> 
#> $x2
#> 
#>   1   2 
#> 237 263 
#> 
#> $x3
#> 
#>   1   2   3 
#> 226 182  92 
#> 
#> $x4
#> 
#>   1   2   3   4 
#> 101 196  91 112 
#> 

# simulate data with user-defined parameters
pi <- rep(1 / 3, 3)
rho <- c(.9, .1, .9, .1, .9, .1, .9, .1,
         .9, .1, .9, .1, .1, .9, .1, .9,
         .1, .9, .1, .9, .1, .9, .1, .9)
par <- c(pi, rho)
m3 <- slca(lc[3] ~ y1 + y2 + y3 + y4)
sim <- simulate(m3, parm = par)
mf <- estimate(m3, sim$response)
param(mf)
#> PI :
#> (lc)
#>   class
#>          1       2       3
#>     0.2774  0.3115  0.4110
#> 
#> RHO :
#> (a)
#>         class
#> response       1       2       3
#>    1(V1)  0.8900  0.0000  0.8980
#>    2      0.1100  1.0000  0.1020
#>    1(V2)  0.7723  0.1051  0.9075
#>    2      0.2277  0.8949  0.0925
#>    1(V3)  0.0606  0.0992  0.8035
#>    2      0.9394  0.9008  0.1965
#>    1(V4)  0.0000  0.1169  0.8310
#>    2      1.0000  0.8831  0.1690
#> 
#>    V1 V2 V3 V4
#> lc y1 y2 y3 y4
```
