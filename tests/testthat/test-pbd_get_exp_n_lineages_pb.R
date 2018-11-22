context("pbd_get_exp_n_lineages_pb")

test_that("use", {

  testthat::expect_silent(
    pbd_get_exp_n_lineages_pb(
      sirg = 1.0,
      scr = 1.0,
      siri = 1.0,
      age = 1.0,
      ng = 1.0,
      ni = 0.0
    )
  )

  expect_silent(
    pbd_get_exp_n_lineages_pb(
      sirg = 1.0,
      scr = 1.0,
      siri = 1.0,
      age = 1.0,
      ng = 0.0,
      ni = 1.0
    )
  )
})

test_that("abuse", {

  expect_error(
    pbd_get_exp_n_lineages_pb(
      sirg = -1.0,
      scr = 1.0,
      siri = 1.0,
      age = 1.0,
      ng = 0.0,
      ni = 1.0
    ),
    "'sirg' must be non-zero and positive"
  )

  expect_error(
    pbd_get_exp_n_lineages_pb(
      sirg = 1.0,
      scr = -1.0,
      siri = 1.0,
      age = 1.0,
      ng = 0.0,
      ni = 1.0
    ),
    "'scr' must be non-zero and positive"
  )

  expect_error(
    pbd_get_exp_n_lineages_pb(
      sirg = 1.0,
      scr = 1.0,
      siri = -1.0,
      age = 1.0,
      ng = 0.0,
      ni = 1.0
    ),
    "'siri' must be non-zero and positive"
  )

  expect_error(
    pbd_get_exp_n_lineages_pb(
      sirg = 1.0,
      scr = 1.0,
      siri = 1.0,
      age = -1.0,
      ng = 0.0,
      ni = 1.0
    ),
    "'age' must be non-zero and positive"
  )

   expect_error(
    pbd_get_exp_n_lineages_pb(
      sirg = 1.0,
      scr = 1.0,
      siri = 1.0,
      age = 1.0,
      ng = -1.0,
      ni = 0.0
    ),
    "'ng' must be zero or positive"
  )

   expect_error(
     pbd_get_exp_n_lineages_pb(
       sirg = 1.0,
       scr = 1.0,
       siri = 1.0,
       age = 1.0,
       ng = 0.0,
       ni = -1.0
     ),
     "'ni' must be zero or positive"
   )

   expect_error(
     pbd_get_exp_n_lineages_pb(
       sirg = 1.0,
       scr = 1.0,
       siri = 1.0,
       age = 1.0,
       ng = 1.0,
       ni = 1.0
     ),
     "Only one of 'ng' and 'ni' can be non-zero"
   )

   expect_error(
     pbd_get_exp_n_lineages_pb(
       sirg = 1.0,
       scr = 1.0,
       siri = 1.0,
       age = 1.0,
       ng = 0.0,
       ni = 0.0
     ),
     "One of 'ng' and 'ni' must be non-zero"
   )

   expect_error(
     pbd_get_exp_n_lineages_pb(
       sirg = 1.0,
       scr = 1.0,
       siri = 1.0,
       age = 1.0,
       ng = -1.0,
       ni = -1.0
     ),
     "'ng' must be zero or positive"
   )
})

test_that("pbd_get_exp_n_lineages gives right number of lineages", {

  # Excel calculation for sirg=2,scr=0.5,siri=2,age=2,ng=1,ni=0 yields:
  # Expected number of good lineages: 11.21393
  # Expected number of inicipien lineages: 43.38422
  exp_out <- list(exp_ng = 11.21393, exp_ni = 43.38422)

  # Now using pbd_get_exp_n_lineages_pb yields:
  out <- pbd_get_exp_n_lineages_pb(
    sirg = 2,
    scr = 0.5,
    siri = 2,
    age = 2,
    ng = 1,
    ni = 0
  )

  testthat::expect_equal(exp_out, out, tolerance = 0.00001)

  # Excel calculation for sirg=3,scr=0.4,siri=4,age=3,ng=0,ni=1 yields:
  # Expected number of good lineages: 953.2686
  # Expected number of inicipien lineages: 7149,815
  exp_out <- list(exp_ng = 953.2686, exp_ni = 7149.815)

  # Now using pbd_get_exp_n_lineages_pb yields:
 out <- pbd_get_exp_n_lineages_pb(
    sirg = 3,
    scr = 0.4,
    siri = 4,
    age = 3,
    ng = 0,
    ni = 1
  )

 testthat::expect_equal(exp_out, out, tolerance = 0.00001)
})
