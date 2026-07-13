# Model Predictions for Estimated `slca` Object

Provides predicted class memberships or posterior probabilities for new
data based on a fitted `slca` model.

## Usage

``` r
# S3 method for class 'slcafit'
predict(object, newdata, type = c("class", "posterior"), ...)
```

## Arguments

- object:

  An object of class `slcafit`, representing a fitted `slca` model.

- newdata:

  A `data.frame` containing the same variables as those used to estimate
  the `object`.

- type:

  A character string indicating the type of prediction. Use `"class"` to
  obtain the predicted class membership for each observation and latent
  class variable, or `"posterior"` to retrieve posterior probabilities
  for each class. The default is `"class"`.

- ...:

  Additional arguments passed to other methods.

## Value

A `data.frame` or `list` depending on the `type`:

- For `type = "class"`, a `data.frame` is returned where rows represent
  observations and columns correspond to latent class variables.

- For `type = "posterior"`, a `list` is returned containing
  `data.frame`s with posterior probabilities for each latent class
  variable.
