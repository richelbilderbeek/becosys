test_that("use", {
  pbd_sim_out <- pbd_sim_checked(
    erg = 0.0,
    eri = 0.0,
    scr = 1.0,
    sirg = 1.0,
    siri = 1.0,
    crown_age = 1.0
  )
  expect_silent(check_pbd_sim_out(pbd_sim_out))
})

test_that("use, add_shortest_and_longest", {
  pbd_sim_out <- pbd_sim_checked(
    erg = 0.0,
    eri = 0.0,
    scr = 1.0,
    sirg = 1.0,
    siri = 1.0,
    crown_age = 1.0,
    add_shortest_and_longest = TRUE
  )
  expect_silent(check_pbd_sim_out(pbd_sim_out))
})

test_that("abuse", {

  skip("WIP")
  good_pbd_sim_out <- pbd_sim_checked(
    erg = 0.0,
    eri = 0.0,
    scr = 1.0,
    sirg = 1.0,
    siri = 1.0,
    crown_age = 1.0
  )
  expect_silent(check_pbd_sim_out(good_pbd_sim_out))

  pbd_sim_out <- good_pbd_sim_out
  expect_silent(check_pbd_sim_out(pbd_sim_out))

  pbd_sim_out <- good_pbd_sim_out
  pbd_sim_out$erg <- -12.34
  expect_error(check_pbd_sim_out(pbd_sim_out),
    "'erg' must be positive"
  )

  pbd_sim_out <- good_pbd_sim_out
  pbd_sim_out$eri <- -12.34
  expect_error(check_pbd_sim_out(pbd_sim_out),
    "'eri' must be positive"
  )

  pbd_sim_out <- good_pbd_sim_out
  pbd_sim_out$scr <- -12.34
  expect_error(check_pbd_sim_out(pbd_sim_out),
    "'scr' must be positive"
  )

  pbd_sim_out <- good_pbd_sim_out
  pbd_sim_out$sirg <- -12.34
  expect_error(check_pbd_sim_out(pbd_sim_out),
    "'sirg' must be positive"
  )

  pbd_sim_out <- good_pbd_sim_out
  pbd_sim_out$siri <- -12.34
  expect_error(check_pbd_sim_out(pbd_sim_out),
    "'siri' must be positive"
  )
})
