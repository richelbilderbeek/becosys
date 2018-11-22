## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load_library--------------------------------------------------------
library(raket)

## ----define_plot---------------------------------------------------------
# Plot the results of PBD::pbd_sim
# '@param out the results of PBD::pbd_sim
plot <- function(out) {
  # Check input
  testit::assert("igtree.extant" %in% names(out))
  testit::assert("stree_youngest" %in% names(out))
  testit::assert("stree_oldest" %in% names(out))
  testit::assert("stree_random" %in% names(out))
  testit::assert(class(out$igtree.extant) == c("simmap", "phylo"))
  testit::assert(class(out$stree_youngest) == "phylo")
  testit::assert(class(out$stree_oldest) == "phylo")
  testit::assert(class(out$stree_random) == "phylo")

  graphics::par(mfrow = c(1, 4), mar = c(5, 4, 6, 2) + 0.3)
  cols <- stats::setNames(c("gray", "black"), c("i", "g"))
  phytools::plotSimmap(out$igtree.extant, colors = cols, fsize = 2)

  sum_youngest <- sum(out$stree_youngest$edge.length)
  sum_oldest <- sum(out$stree_oldest$edge.length)
  sum_random <- sum(out$stree_random$edge.length)

  ape::plot.phylo(out$stree_youngest, edge.width = 2, font = 1,
    label.offset = 0.1, cex = 2, cex.main = 0.75,
    main = paste("\n\n", "youngest", format(round(sum_youngest, 2), nsmall = 2), "\n\n"))
  ape::add.scale.bar()
  ape::plot.phylo(out$stree_random, edge.width = 2, font = 1,
    label.offset = 0.1, cex = 2, cex.main = 0.75, 
    main = paste("\n\n", "random", format(round(sum_random, 2), nsmall = 2), "\n\n"))
  ape::add.scale.bar()
  ape::plot.phylo(out$stree_oldest, edge.width = 2, font = 1,
    label.offset = 0.1, cex = 2, cex.main = 0.75,
    main = paste("\n\n", "oldest", format(round(sum_oldest, 2), nsmall = 2), "\n\n"))
  ape::add.scale.bar()
  graphics::par(mfrow = c(1, 1))
}

## ----find_expected_simplest, fig.width=7, fig.height=5-------------------
out <- raket::pbd_find_scenario(
  scr = 0.1, sirg = 2, siri = 2, scenario = "expected", 
  rng_seed = 42, max_n_subspecies = 3
)
plot(out)

## ----find_expected_simpler, fig.width=7, fig.height=5--------------------
out <- raket::pbd_find_scenario(
  scr = 0.1, sirg = 2, siri = 2, scenario = "expected", 
  rng_seed = 44, 
  min_n_subspecies = 4,
  max_n_subspecies = 4
)
plot(out)

## ----find_expected_simple, fig.width=7, fig.height=5---------------------
out <- raket::pbd_find_scenario(scr = 0.5, sirg = 1, siri = 2, scenario = "expected",
  rng_seed = 51, 
  min_n_subspecies = 5, 
  max_n_subspecies = 10,
  min_n_species = 3
)
plot(out)

## ----fig.width=7, fig.height=5-------------------------------------------
out <- raket::pbd_find_scenario(scr = 0.5, sirg = 1, siri = 2, scenario = "ylto",
  rng_seed = 54, 
  min_n_subspecies = 4, 
  max_n_subspecies = 4,
  min_n_species = 3
)
plot(out)

## ----fig.width=7, fig.height=5-------------------------------------------
out <- raket::pbd_find_scenario(scr = 0.5, sirg = 1, siri = 2, scenario = "rsty",
  rng_seed = 55, 
  min_n_subspecies = 4, 
  max_n_subspecies = 7,
  min_n_species = 3
)
plot(out)

## ----fig.width=7, fig.height=5-------------------------------------------
out <- raket::pbd_find_scenario(scr = 0.5, sirg = 1, siri = 2, scenario = "rlto",
  rng_seed = 57, 
  min_n_subspecies = 4, 
  max_n_subspecies = 7,
  min_n_species = 3
)
plot(out)

