context("fx-fun-ggplot-fxGeomOrdinalCI")

test_that("nominations work", {
  expect_equal(
    fxe_layer_complete_nominate(fxGeom("OrdinalCI"),
                                AesName("y"),
                                data.frame()) %>%
      length(),
    1
  )
  expect_equal(
    suppressWarnings(
      fxe_layer_complete_nominate(
        fxGeom("OrdinalCI"),
        AesName("y"),
        data.frame(upper = LETTERS[1:10], lower = LETTERS[1:10]),
        fxGeom_assoc_vars = ggplot2::aes(upper = upper, lower = lower)) %>%
        length()
    ),
    4
  )
})

test_that("votes work", {
  expect_equal(
    fxe_layer_complete_vote(nomination(ggplot2::geom_linerange()),
                            fxGeom("OrdinalCI"),
                            AesName("y"),
                            data.frame()),
    1
  )
})
