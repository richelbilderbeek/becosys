#' Create an example scenario
#' @param scr Speciation Completion Rate
#' @param sirg Speciation Initiation Rate of Good Species
#' @param siri Speciation Initiation Rate of Incipient Species
#' @param scenario scenario to look for, can be
#'  'equal' (branch length of youngest equals oldest equals random)
#'  'expected',
#'  'ylto' (youngest longer than oldest)
#'  'rsty' (random shorter than youngest)
#'  'rlto' (random longer than oldest)
#'  'expsl' (expected, but also includes shortest and longest)
#'  'yltosl' (ylto, but also includes shortest and longest)
#'  'rstysl' (rsty, but also includes shortest and longest)
#'  'rltosl' (rlto, but also includes shortest and longest)
#'  'rsts' (random shorter than shortest)
#'  'rltl' (random longer than longest)
#'  'sltl' (shortest longer than longest)
#' @param erg Extinction Rate of Good Species
#' @param eri Extinction Rate of Incipient Species
#' @param crown_age crown age of the phylogeny
#' @param min_n_species minimal number of good species
#' @param max_n_species maximal number of good species,
#' @param min_n_subspecies minimal number of subspecies
#' @param max_n_subspecies maximal number of subspecies
#' @param rng_seed Random Number Generator seed
#' @export
pbd_find_scenario <- function(
  scr,
  sirg,
  siri,
  scenario,
  erg = 0.0,
  eri = 0.0,
  crown_age = 1,
  min_n_species = 1,
  max_n_species = 10000,
  min_n_subspecies = 2,
  max_n_subspecies = 10000,
  rng_seed = 42
) {
  # Check input
  testit::assert(scr >= 0.0)
  testit::assert(sirg >= 0.0)
  testit::assert(siri >= 0.0)
  sl <- c("equal", "expected", "ylto", "rsty", "rlto", "expsl", "yltosl",
          "rstysl", "rltosl", "rsts", "rltl", "sltl")
  testit::assert(scenario %in% sl)
  testit::assert(erg >= 0.0)
  testit::assert(eri >= 0.0)
  testit::assert(crown_age >= 0.0)
  testit::assert(min_n_species >= 1)
  testit::assert(max_n_species >= 1)
  testit::assert(min_n_species <= max_n_species)
  testit::assert(min_n_subspecies >= 1)
  testit::assert(max_n_subspecies >= 1)
  testit::assert(min_n_subspecies <= max_n_subspecies)

  is_scenario <- becosys::get_pbd_scenario_function(scenario)

  if (scenario %in% c("expsl", "yltosl", "rstysl", "rltosl",
                      "rsts", "rltl", "sltl")) {
    add_shortest_and_longest <- TRUE
  } else {
    add_shortest_and_longest <- FALSE
  }

  set.seed(rng_seed)
  while (TRUE) {
    out <- pbd_sim_checked(
      erg = erg,
      eri = eri,
      scr = scr,
      sirg = sirg,
      siri = siri,
      crown_age = crown_age,
      add_shortest_and_longest = add_shortest_and_longest
    )

    # Count subspecies
    n_subspecies <- length(out$igtree.extant$tip.label)
    if (n_subspecies < min_n_subspecies) next
    if (n_subspecies > max_n_subspecies) next

    # Count species
    n_species <- length(out$stree_youngest$tip.label)
    if (n_species < min_n_species) next
    if (n_species > max_n_species) next

    # Sum the branch lengths
    sum_youngest <- sum(out$stree_youngest$edge.length)
    sum_oldest <- sum(out$stree_oldest$edge.length)
    sum_random <- sum(out$stree_random$edge.length)
    sum_shortest <- sum(out$stree_shortest$edge.length)
    sum_longest <- sum(out$stree_longest$edge.length)

    if (
      !is_scenario(
        sum_youngest = sum_youngest,
        sum_oldest = sum_oldest,
        sum_random = sum_random,
        sum_shortest = sum_shortest,
        sum_longest = sum_longest
      )
    ) next

    # Found an example!
    return(out)
  }
}

#' Gets a function to determine if a scenario is true
#' @param scenario_description the scenario description
#' @export
get_pbd_scenario_function <- function(scenario_description) {

  dictionary <- list(
    "equal" = is_pbd_scenario_equal,
    "expected" = is_pbd_scenario_expected,
    "ylto" = is_pbd_scenario_ylto,
    "rsty" = is_pbd_scenario_rsty,
    "rlto" = is_pbd_scenario_rlto,
    "expsl" = is_pbd_scenario_expsl,
    "yltosl" = is_pbd_scenario_yltosl,
    "rstysl" = is_pbd_scenario_rstysl,
    "rltosl" = is_pbd_scenario_rltosl,
    "rsts" = is_pbd_scenario_rsts,
    "rltl" = is_pbd_scenario_rltl,
    "sltl" = is_pbd_scenario_sltl
  )
  testit::assert(scenario_description %in% names(dictionary))
  dictionary[scenario_description]$equal
}

#' Scenario equal
#'  'equal' (branch length of youngest equals oldest equals random)
#' @noRd
is_pbd_scenario_equal <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  if (sum_youngest != sum_oldest) return(FALSE)
  if (sum_youngest != sum_random) return(FALSE)
  TRUE
}

#' Scenario expected
#'  'expected',
#' @noRd
is_pbd_scenario_expected <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  # No unexpected things
  if (sum_random < sum_youngest) return(FALSE)
  if (sum_random > sum_oldest) return(FALSE)
  if (sum_youngest > sum_oldest) return(FALSE)
  testit::assert(sum_youngest < sum_oldest)
  testit::assert(sum_youngest <= sum_random)
  testit::assert(sum_random <= sum_oldest)
  TRUE
}

#' Scenario ylto
#'  'ylto' (youngest longer than oldest)
#' @noRd
is_pbd_scenario_ylto <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  # Younger Less Than Oldest
  if (sum_youngest < sum_oldest) return(FALSE)
  testit::assert(sum_youngest > sum_oldest)
  TRUE
}

#' Scenario rsty
#'  'rsty' (random shorter than youngest)
#' @noRd
is_pbd_scenario_rsty <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  # Random Shorter Than Youngest
  if (sum_youngest > sum_oldest) return(FALSE)
  if (sum_random >= sum_youngest) return(FALSE)
  if (sum_random >= sum_oldest) return(FALSE)
  testit::assert(sum_youngest < sum_oldest)
  testit::assert(sum_random < sum_youngest)
  testit::assert(sum_random < sum_oldest)
  TRUE
}

#' Scenario rlto
#'  'rlto' (random longer than oldest)
#' @noRd
is_pbd_scenario_rlto <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  if (sum_youngest > sum_oldest) return(FALSE)
  if (sum_random <= sum_youngest) return(FALSE)
  if (sum_random <= sum_oldest) return(FALSE)
  testit::assert(sum_youngest < sum_oldest)
  testit::assert(sum_random > sum_youngest)
  testit::assert(sum_random > sum_oldest)
  TRUE
}

#' Scenario expsl
#'  'expsl' (expected, but also includes shortest and longest)
#' @noRd
is_pbd_scenario_expsl <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  if (sum_random < sum_youngest) return(FALSE)
  if (sum_random > sum_oldest) return(FALSE)
  if (sum_youngest >= sum_oldest) return(FALSE)
  if (sum_random < sum_shortest) return(FALSE)
  if (sum_random > sum_longest) return(FALSE)
  if (sum_shortest >= sum_longest) return(FALSE)

  # Only measure when sampling does give different branch lengths
  testit::assert(sum_youngest != sum_oldest)
  testit::assert(sum_shortest != sum_longest)

  testit::assert(sum_youngest < sum_oldest)
  testit::assert(sum_youngest <= sum_random)
  testit::assert(sum_random <= sum_oldest)
  testit::assert(sum_shortest < sum_longest)
  testit::assert(sum_shortest <= sum_random)
  testit::assert(sum_random <= sum_longest)
  TRUE
}

#' Scenario yltosl
#'  'yltosl' (ylto, but also includes shortest and longest)
#' @noRd
is_pbd_scenario_yltosl <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  # Younger Less Than Oldest
  if (sum_youngest < sum_oldest) return(FALSE)
  testit::assert(sum_youngest > sum_oldest)
  TRUE
}

#' Scenario rstysl
#'  'rstysl' (rsty, but also includes shortest and longest)
#' @noRd
is_pbd_scenario_rstysl <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  # Random Shorter Than Youngest
  if (sum_youngest > sum_oldest) return(FALSE)
  if (sum_random >= sum_youngest) return(FALSE)
  if (sum_random >= sum_oldest) return(FALSE)
  testit::assert(sum_youngest < sum_oldest)
  testit::assert(sum_random < sum_youngest)
  testit::assert(sum_random < sum_oldest)
  TRUE
}


#' Scenario rltosl
#'  'rltosl' (rlto, but also includes shortest and longest)
#' @noRd
is_pbd_scenario_rltosl <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  if (sum_youngest > sum_oldest) return(FALSE)
  if (sum_random <= sum_youngest) return(FALSE)
  if (sum_random <= sum_oldest) return(FALSE)
  testit::assert(sum_youngest < sum_oldest)
  testit::assert(sum_random > sum_youngest)
  testit::assert(sum_random > sum_oldest)
  TRUE
}

#' Scenario rsts
#'  'rsts' (random shorter than shortest)
#' @noRd
is_pbd_scenario_rsts <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  if (sum_shortest > sum_longest) return(FALSE)
  if (sum_random  >= sum_shortest) return(FALSE)
  if (sum_random >= sum_longest) return(FALSE)
  testit::assert(sum_shortest < sum_longest)
  testit::assert(sum_random < sum_shortest)
  testit::assert(sum_random < sum_longest)
  TRUE
}

#' Scenario rltl
#'  'rltl' (random longer than longest)
#' @noRd
is_pbd_scenario_rltl <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  if (sum_shortest > sum_longest) return(FALSE)
  if (sum_random <= sum_shortest) return(FALSE)
  if (sum_random <= sum_longest) return(FALSE)
  testit::assert(sum_shortest < sum_longest)
  testit::assert(sum_random > sum_shortest)
  testit::assert(sum_random > sum_longest)
  TRUE
}

#' Scenario sltl
#'  'sltl' (shortest longer than longest)
#' @noRd
is_pbd_scenario_sltl <- function(
  sum_youngest,
  sum_oldest,
  sum_random,
  sum_shortest,
  sum_longest
) {
  # Only measure when sampling does give different branch lengths
  if (sum_youngest == sum_oldest) return(FALSE)
  if (sum_shortest == sum_longest) return(FALSE)

  if (sum_shortest < sum_longest) return(FALSE)
  testit::assert(sum_shortest > sum_longest)
  TRUE
}
