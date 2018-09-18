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

test_that("nomination works with groups", {
  expect_equal(
    suppressWarnings(
      fxe_layer_complete_nominate(
        fxGeom("OrdinalCI"),
        AesName("y"),
        data.frame(upper = 1:10, lower = 1:10, group = c(rep(1, 5), rep(2, 5))),
        fxGeom_assoc_vars = ggplot2::aes(upper = upper, lower = lower,
                                         group = group)) %>%
        length()
    ),
    4
  )
  expect_equal(
    suppressWarnings(
      fxe_layer_complete_nominate(
        fxGeom("OrdinalCI"),
        AesName("y"),
        data.frame(upper = 1:10, lower = 1:10, group = c(rep(1, 5), rep(2, 5))),
        fxGeom_assoc_vars = ggplot2::aes(upper = upper, lower = lower,
                                         group = notagroup)) %>%
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
    3
  )
  expect_equal(
    fxe_layer_complete_vote(nomination(ggplot2::geom_area()),
                            fxGeom("OrdinalCI"),
                            AesName("y"),
                            data.frame()),
    2
  )
  expect_equal(
    fxe_layer_complete_vote(nomination(ggplot2::geom_point()),
                            fxGeom("OrdinalCI"),
                            AesName("y"),
                            data.frame()),
    0
  )
})
