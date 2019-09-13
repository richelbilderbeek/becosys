test_that("use", {
  expect_silent(check_pbd_params_selector(create_pbd_params_selector()))
  expect_error(check_pbd_params_selector("nonsense"))
  expect_error(check_pbd_params_selector(NULL))
  expect_error(check_pbd_params_selector(NA))
  expect_error(
    check_pbd_params_selector(
      list(
        erg = "nonsense",
        eri = TRUE,
        scr = TRUE,
        sirg = TRUE,
        siri = TRUE
      )
    )
  )
  expect_error(
    check_pbd_params_selector(
      list(
        erg = TRUE,
        eri = "nonsense",
        scr = TRUE,
        sirg = TRUE,
        siri = TRUE
      )
    )
  )
  expect_error(
    check_pbd_params_selector(
      list(
        erg = TRUE,
        eri = TRUE,
        scr = "nonsense",
        sirg = TRUE,
        siri = TRUE
      )
    )
  )
  expect_error(
    check_pbd_params_selector(
      list(
        erg = TRUE,
        eri = TRUE,
        scr = TRUE,
        sirg = "nonsense",
        siri = TRUE
      )
    )
  )
  expect_error(
    check_pbd_params_selector(
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
