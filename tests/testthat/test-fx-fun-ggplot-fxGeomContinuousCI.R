context("fx-fun-ggplot-fxGeomContinuousCI")

test_that("nomination works", {
  expect_equal(
    fxe_layer_complete_nominate(fxGeom("ContinuousCI"),
                                AesName("y"),
                                data.frame()) %>%
      length(),
    3
  )
  expect_equal(
    suppressWarnings(
      fxe_layer_complete_nominate(
        fxGeom("ContinuousCI"),
        AesName("y"),
        data.frame(upper = 1:10, lower = 1:10),
        fxGeom_assoc_vars = ggplot2::aes(upper = upper, lower = lower)) %>%
        length()
    ),
    5
  )
})

test_that("votes work", {
  expect_equal(
    fxe_layer_complete_vote(nomination(ggplot2::geom_linerange()),
                            fxGeom("ContinuousCI"),
                            AesName("y"),
                            data.frame()),
    1
  )
})
