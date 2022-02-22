# sample size calculation for a prevalence survey with different options for
# tests; to account for sensitivity of positivity over time when converting
# to daily attack rates

# mean duration for each test is the average number of days an individual would
# test positive, if tested once every day. This accounts for lower detection
# probabilities at the beginning and end of the infection.

# # simulate a viral load for a typically detected person
# days <- 0:30
# load <- dlnorm(days, 0, 1)
# load <- 100 * load / max(load)
#
# # for those who ever test positive, this is the probability of testing positive
# # on each day, based on viral load (random encounter model)
# prob_if_any <- 1 - exp(-load)
#
# # if they were to be tested on each day, the expected number of positive days is
# # the sum of this. This is the expected duration of test positivity for
# # converting prevalence to incidence, assuming all cases test positive at some point.
#
# # this is the fraction of infections that are detectable at all
# prob_any <- 0.9
#
# # so the average number of detectable days per infected person is the product of
# # these (1-prob_any are detectable for 0 days)
# sum(prob_if_any) * prob_any
#
# # when sensitivity over time is measured for a cohort, the two things get
# # conflated. This results in curves like the below that get the fraction of
# # infections on each day/test that are positive.
# prob <- prob_any * prob_if_any
# plot(prob ~ days, type = "l", xlim = c(0, 14))
#
# # we can get the average number of detectable days per infected person as the
# # sum of this curve (same as above)
# sum(prob)
#
# # so we can get these sensitivity curves, sum across them, and use this as mean_duration * sensitivity


# visually approximate PCR sensitivity over time from this (appendix):
# https://doi.org/10.1016/S1473-3099(20)30630-7
pcr_sensitivity <- tibble(
  day = 0:35
) %>%
  mutate(
    sens = case_when(
      # day <= 8 ~ 0.95 * (day / 8),
      day < 8 ~ 0.95 * plogis((day - 5) / 1),
      day == 8 ~ 0.95,
      TRUE ~ plogis(-(day - 20) / 4)
    )
  )

# lamp vs PCR for symptomatic/asymptomatic from this:
# https://doi.org/10.1001/jama.2021.13967
lamp_sensitivity <- pcr_sensitivity %>%
  mutate(
    # switch to days sunce symptom onset
    day = day - 5,
    # scale down to symptomatic LAMP
    symptomatic = case_when(
      day < 7 ~ sens * 0.89,
      day >= 7 & day < 14 ~ sens * 0.82,
      day >= 14 & day < 21 ~ sens * 0.61,
      day >= 21 & day < 28 ~ sens * 0.24,
      TRUE ~ 0
    ),
    asymptomatic = case_when(
      day < 7 ~ sens * 0.58,
      day >= 7 & day < 14 ~ sens * 0.52,
      day >= 14 & day < 21 ~ sens * 0.45,
      day >= 21 & day < 28 ~ sens * 0.27,
      day >= 28 & day < 35 ~ sens * 0.13,
      TRUE ~ 0
    )
  )

# what about RAT sensitivity over time?

# assume RATs average 72% sens for symptomatic, 56% for asymptomatic
# https://doi.org/10.1136/bmjebm-2021-111828

# for those with symptoms, assume 87% sens in first week, then 54% for following week.
# https://dx.doi.org/10.1016%2Fj.jcv.2020.104659

# assume asymptomatics the same but downweighted as per averages (truncate second week)
rat_symptomatic_mean_duration <- 0.87 * 7 + 0.54 * 7 * 0.5
rat_asymptomatic_mean_duration <- rat_symptomatic_mean_duration * 0.56 / 0.72

# ONS has symptomatic fraction against Omicron at 50%
symptomatic_fraction <- 0.5

mean_durations <- list(

  pcr = sum(pcr_sensitivity$sens),

  lamp = sum(lamp_sensitivity$symptomatic) * symptomatic_fraction +
    sum(lamp_sensitivity$asymptomatic) * (1 - symptomatic_fraction),

  rat = rat_symptomatic_mean_duration * symptomatic_fraction +
    rat_asymptomatic_mean_duration * (1 - symptomatic_fraction)

)

# see also:
# https://doi.org/10.1016/j.ijid.2021.04.018
# https://doi.org/10.7883/yoken.jjid.2021.476
# https://doi.org/10.7326/M20-1495

# # raw sensitivities for PCR and LAMP over time in hospitalised patients from Table 3 in Inaba et al.
# tibble::tribble(
#   ~days_start, ~days_end, ~pcr, ~lamp,
#   0, 9, 13/14, 13/14,
#   10, 19, 30/53, 13/53,
#   20, 29, 15/40, 8/40,
#   30, 39, 4/17, 2/17
# ) %>%
#   # convert from days since symptom onset to days since infection
#   mutate(
#     # extend the first bin back to include test positivity 3 days prior to
#     # symptom onset
#     days_start = case_when(
#       days_start == 0 ~ -3,
#       TRUE ~ days_start
#     ),
#     # add on 5 day incubation period, so they are days from infection
#     across(
#       starts_with("days"),
#       ~.x + 5
#     )
#   ) %>%
#   rowwise() %>%
#   summarise(
#     days = days_start:days_end,
#     pcr = pcr,
#     lamp = lamp
#   ) %>%
#   ungroup() %>%
#   summarise(
#     across(
#       c(pcr, lamp),
#       sum
#     )
#   )

# household_size <- 2.6 3.5 is about the average for Gold Coast survey, biased
# up because either larger households (e.g. with kids) are more keen to be
# swabbed, or because larger households are more likely to have someone home
household_size <- 3.5

# assumption about total cost of a lamp sample (including consumables)
lamp_sample_cost <- 20

# assume casual RA rates, plus $10 per hour for travel etc.
hourly_rate <- 43 + 10

# options for either effective sample sizes, or target numbers of households
effective_options <- c(500, 1000, 2000, 3000)
household_options <- c(250, 500, 1000, 2000)
household_correction <- household_size / deff(household_size)
sample_size_options <- c(
  household_options * household_correction,
  effective_options
)

personnel_per_household <- 1

# set this up for different prevalences
# set up different levels of PCR prevalence (for comparison with UK)
sample_size_est <- expand_grid(
  test = c("pcr", "lamp", "rat"),
  pcr_prev = c(0.005, 0.01, 0.05, 0.1),
  moe_ratio = c(0.1, 0.25, 0.5, 1)
) %>%
  mutate(
    mean_duration = unlist(mean_durations)[test],
    .after = "test"
  ) %>%
  # convert to incidence and then to prevalence with different tests
  mutate(
    incidence = incidence(
      prevalence = pcr_prev,
      mean_duration = mean_durations$pcr
    ),
    prevalence = prevalence(
      incidence = incidence,
      mean_duration = mean_duration
    )
  ) %>%
  # compute margins of error, relative to 30% for PCR (similar to NI)
  mutate(
    moe_incidence = moe_prev_to_inc(
      moe_prevalence = pcr_prev * moe_ratio,
      mean_duration = mean_durations$pcr
    ),
    .after = incidence
  ) %>%
  mutate(
    moe_prevalence = moe_inc_to_prev(
      moe_incidence,
      mean_duration = mean_duration
    )
  ) %>%
  # compute sample size required for each
  mutate(
    sample_size = sample_size_from_moe(
      moe = moe_prevalence,
      proportion = prevalence
    )
  )

sample_size_est_formatted <- sample_size_est %>%
  mutate(
    pcr_moe = paste0(signif(100 * moe_ratio), "%"),
    pcr_prevalence = paste0(signif(100 * pcr_prev), "%"),
    pcr_range = prevalence_range(pcr_prev, pcr_prev * moe_ratio),
    # sample_size = prettyNum(sample_size, big.mark = ",")
  ) %>%
  select(
    pcr_prevalence,
    pcr_moe,
    pcr_range,
    test,
    sample_size
  ) %>%
  pivot_wider(
    names_from = test,
    values_from = sample_size
  )

# now do the same but start from sample size, and compute PCR-equivalent
# prevalence, and MOE as a percentage of true prevalence

moe_est <- expand_grid(
  test = c("pcr", "lamp", "rat"),
  pcr_prev = c(0.005, 0.01, 0.05, 0.1),
  sample_size = sample_size_options
) %>%
  mutate(
    mean_duration = unlist(mean_durations)[test],
    .after = "test"
  ) %>%
  # convert to incidence and then to prevalence with different tests
  mutate(
    incidence = incidence(
      prevalence = pcr_prev,
      mean_duration = mean_durations$pcr
    ),
    prevalence = prevalence(
      incidence = incidence,
      mean_duration = mean_duration
    )
  ) %>%
  # compute margins of error, relative to 30% for PCR (similar to NI)
  mutate(
    # get the margin of error, given this sample size
    moe = moe_from_sample_size(
      sample_size = sample_size,
      proportion = prevalence
    ),
    # get the equivalent moe for incidence
    moe_incidence = moe_prev_to_inc(
      moe_prevalence = moe,
      mean_duration = mean_duration
    ),
    # and convert to moe for PCR,
    moe_pcr = moe_inc_to_prev(
      moe_incidence = moe_incidence,
      mean_duration = mean_durations$pcr
    ),
  )

moe_est_formatted <- moe_est %>%
  filter(
    sample_size %in% effective_options
  ) %>%
  mutate(
    pcr_equivalent_moe = paste0(round(100 * moe_pcr / pcr_prev), "%"),
    pcr_prevalence = paste0(signif(100 * pcr_prev), "%"),
    pcr_equivalent_range = prevalence_range(pcr_prev, moe_pcr, pretext = "prev: "),
    result = paste(pcr_equivalent_moe, pcr_equivalent_range)
  ) %>%
  select(
    sample_size,
    pcr_prevalence,
    result,
    test
  ) %>%
  pivot_wider(
    names_from = test,
    names_prefix = "moe_",
    values_from = result
  ) %>%
  arrange(sample_size)

sample_size_est_formatted %>%
  print(n = Inf)

moe_est_formatted %>%
  print(n = Inf)

# consider different costings (for equivalent power) under testing of entire households versus one random member
sampling_efficiency <- expand_grid(
  # different speeds of teams
  households_per_hour = c(1, 2, 3),
  # assume a  team of x casual RAs per household, plus travel expenses ($10 per hour)
  cost_per_personnel_hour = personnel_per_household * hourly_rate,
  # whether to pick a random person, or do everyone
  test_household = c(FALSE, TRUE),
  # upper bound on LAMP cost per test (includes consumables)
  cost_per_test = lamp_sample_cost,
  # mean number of people per household
  household_size = household_size
) %>%
  mutate(
    tests_per_household = ifelse(test_household, household_size, 1),
    tests_per_hour = tests_per_household * households_per_hour,
    cost_per_hour = cost_per_personnel_hour + cost_per_test * tests_per_hour,
    design_effect = deff(tests_per_household),
    effective_samples_per_hour = tests_per_hour / design_effect,
    effective_samples_per_dollar = effective_samples_per_hour / cost_per_hour
  ) %>%
  arrange(
    desc(effective_samples_per_dollar)
  )

costing <- sampling_efficiency %>%
  expand_grid(
    effective_samples = sample_size_options
  ) %>%
  mutate(
    design = ifelse(test_household, "household", "individual"),
    households = effective_samples * design_effect / tests_per_household,
    tests = households * tests_per_household,
    testing_costs = tests * cost_per_test,
    personnel_costs = cost_per_personnel_hour * households / households_per_hour,
    total_costs = personnel_costs + testing_costs,
    household_hours = effective_samples / effective_samples_per_hour,
    person_days =  personnel_per_household * household_hours / 7.5,
  ) %>%
  select(
    effective_samples,
    households_per_hour,
    total_costs,
    design,
    households,
    tests,
    testing_costs,
    personnel_costs,
    person_days,
  ) %>%
  arrange(
    effective_samples,
    desc(households_per_hour),
    total_costs
  ) %>%
  print(
    n = Inf
  )

costing %>%
  filter(
    effective_samples %in% effective_options
  ) %>%
  print(n = Inf)





# household surveys are the best option, so summarise some simple
# household-level targets in terms of costs and precision

# compute CIs for these under different prevalences
moe_est %>%
  filter(
    test == "lamp",
    !sample_size %in% effective_options
  ) %>%
  mutate(
    households = sample_size / household_correction,
    pcr_prevalence = paste0(signif(100 * pcr_prev), "%"),
    moe_95 = moe_pcr,
    moe_50 = moe_pcr * Z(0.5) / Z(0.05),
    prevalence_ci_50 = prevalence_range(pcr_prev, moe_50),
    prevalence_ci_95 = prevalence_range(pcr_prev, moe_95),
  ) %>%
  select(
    pcr_prevalence,
    households,
    prevalence_ci_50,
    prevalence_ci_95
  ) %>%
  print(
    n = Inf
  )

# show costs for a proposed set of options: household surveys with 200, 500, 1000 households
person_day_cost <- sampling_efficiency$cost_per_personnel_hour[1] * 7.5

costing %>%
  mutate(
    minutes_per_household = round(60 / households_per_hour),
    # make sure the staff numbers are multiples of the number of peronnel needed
    # per household; 2 days per weekend
    staff_per_weekend = personnel_per_household *
      ceiling(person_days / (2 * personnel_per_household)),
    field_costs = staff_per_weekend * 2 * person_day_cost, # 2 days per wekend
    total_costs = personnel_costs + testing_costs
  ) %>%
  filter(
    !effective_samples %in% effective_options,
    design == "household",
    minutes_per_household %in% c(20, 30, 60),
  ) %>%
  select(
    minutes_per_household,
    households,
    tests,
    staff_per_weekend,
    field_costs,
    testing_costs,
    total_costs
  ) %>%
  arrange(
    minutes_per_household,
    households
  )


# all test types in this one
moe_est %>%
  filter(
    !sample_size %in% effective_options
  ) %>%
  mutate(
    households = sample_size / household_correction,
    pcr_prevalence = paste0(signif(100 * pcr_prev), "%"),
    moe_95 = moe_pcr,
    moe_50 = moe_pcr * Z(0.5) / Z(0.05),
    prevalence_ci_50 = prevalence_range(pcr_prev, moe_50),
    prevalence_ci_95 = prevalence_range(pcr_prev, moe_95),
  ) %>%
  select(
    test,
    pcr_prevalence,
    households,
    prevalence_ci_50,
    prevalence_ci_95
  ) %>%
  print(
    n = Inf
  )

