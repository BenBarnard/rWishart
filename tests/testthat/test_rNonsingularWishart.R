context("rNonsingularWishart")

test_that("rNonsingularWishart produces Wishart Matrix", {
  expect_equal(
    round(
      mean(
        replicate(1000, 
                  wishartTest(
                    rSingularWishart(1, 50, 
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
                    SingularWishart(50, 
                                    diag(1, 20)), 
                    diag(1, 20)
                  )
        )
      )
    ), 
    50)
})
