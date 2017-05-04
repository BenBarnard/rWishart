context("rNonsingularWishart")

test_that("rNonsingularWishart produces Wishart Matrix", {
  expect_equal(
    round(
      mean(
        replicate(1000, 
                  wishartTest(
                    rNonsingularWishart(1, 50, 
                                     diag(1, 20))[,,1], 
                    diag(1, 20)
                  )
        )
      )
    ), 
    50)
})

test_that("rNonsingularWishart produces Wishart Matrix", {
  expect_equal(
    round(
      mean(
        replicate(1000, 
                  wishartTest(
                    NonsingularWishart(50, 
                                    diag(1, 20)), 
                    diag(1, 20)
                  )
        )
      )
    ), 
    50)
})

test_that("rNonsingularWishart produces Wishart Matrix", {
  expect_true(class(NonsingularWishart(25, diag(1, 20), 
                                    covariance = TRUE))[[1]] == "covariance")
})
