context("is_pbd_params_selector")

test_that("use", {
  expect_true(is_pbd_params_selector(x = create_pbd_params_selector()))
  expect_false(is_pbd_params_selector(x = "nonsense"))
  expect_false(is_pbd_params_selector(NULL))
  expect_false(is_pbd_params_selector(NA))
  expect_false(
    becosys:::is_pbd_params_selector(
      list(
        erg = "nonsense",
        eri = TRUE,
        scr = TRUE,
        sirg = TRUE,
        siri = TRUE
      )
    )
  )
  expect_false(
    becosys:::is_pbd_params_selector(
      list(
        erg = TRUE,
        eri = "nonsense",
        scr = TRUE,
        sirg = TRUE,
        siri = TRUE
      )
    )
  )
  expect_false(
    becosys:::is_pbd_params_selector(
      list(
        erg = TRUE,
        eri = TRUE,
        scr = "nonsense",
        sirg = TRUE,
        siri = TRUE
      )
    )
  )
  expect_false(
    becosys:::is_pbd_params_selector(
      list(
        erg = TRUE,
        eri = TRUE,
        scr = TRUE,
        sirg = "nonsense",
        siri = TRUE
      )
    )
  )
  expect_false(
    becosys:::is_pbd_params_selector(
      list(
        erg = TRUE,
        eri = TRUE,
        scr = TRUE,
        sirg = TRUE,
        siri = "nonsense"
      )
    )
  )
})
