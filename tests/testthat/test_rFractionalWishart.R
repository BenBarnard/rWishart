context("rFractionalWishart")

test_that("rFractionalWishart produces Wishart Matrix", {
  expect_equal(
    round(
      mean(
        replicate(1000, 
                  wishartTest(
                    rFractionalWishart(1, 4.99, 
                                       diag(1, 3))[,,1], 
                    diag(1, 3)
                  )
        )
      )
    ), 
    5)
})

test_that("rFractionalWishart produces Wishart Matrix", {
  expect_equal(
    round(
      mean(
        replicate(1000, 
                  wishartTest(
                    FractionalWishart(4.99, 
                                    diag(1, 3)), 
                    diag(1, 3)
                  )
        )
      )
    ), 
    5)
})
