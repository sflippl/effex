context("fx-fun-ggplot-fxGeomTime")

test_that("nominations works", {
  ex1 <- fxe_layer_complete_nominate(
    fxGeom("Time"), AesName("x"), data.frame()
  )
  expect_equal(
    sum(
      purrr::map_lgl(
        ex1,
        ~ any(
          purrr::map_lgl(
            nom_layers(.),
            ~ inherits(.$geom, "GeomPath")
          )
        )
      )
    ), 2
  )
  ex2 <- fxe_layer_complete_nominate(
    fxGeom("Time"), AesName("x"), data.frame(group = 1:5),
    fxGeom_assoc_vars = ggplot2::aes(group = group)
  )
  expect_equal(
    sum(
      purrr::map_lgl(
        ex2,
        ~ any(
          purrr::map_lgl(
            nom_layers(.),
            ~ inherits(.$geom, "GeomPath")
          )
        )
      )
    ), 2
  )
  ex3 <- fxe_layer_complete_nominate(
    fxGeom("Time"), AesName("x"), data.frame(group = 1:15),
    fxGeom_assoc_vars = ggplot2::aes(group = group)
  )
  expect_equal(
    sum(
      purrr::map_lgl(
        ex3,
        ~ any(
          purrr::map_lgl(
            nom_layers(.),
            ~ inherits(.$geom, "GeomSmooth")
          )
        )
      )
    ), 2
  )
  ex4 <- fxe_layer_complete_nominate(
    fxGeom("Time"), AesName("x"), data.frame(other_group = 1:5),
    fxGeom_assoc_vars = ggplot2::aes(group = group)
  )
  expect_equal(
    sum(
      purrr::map_lgl(
        ex4,
        ~ any(
          purrr::map_lgl(
            nom_layers(.),
            ~ inherits(.$geom, "GeomPath")
          )
        )
      )
    ), 2
  )
})

test_that("votes work", {
  expect_equal(
    fxe_layer_complete_vote(nomination(ggplot2::geom_step()),
                            fxGeom("Time"),
                            AesName("x"),
                            data.frame()),
    3
  )
})
