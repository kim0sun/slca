test_that("parenthesis/bracket identical", {
   expect_equal(slca(l[3] ~ a + b + c), slca(l(3) ~ a + b + c))
})
test_that("stop without number works", {
   expect_error(slca(l ~ a + b + c))
})
