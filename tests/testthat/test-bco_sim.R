context("bco_sim")

test_that("use", {

  skip("WIP")
  expect_silent(
    bco_sim(
      spec_params = create_pbd_params(
        erg = 0.1,
        eri = 0.2,
        scr = 0.3,
        siri = 0.4,
        sirg = 0.5
      ),
      crown_age = 3
    )
  )

})
