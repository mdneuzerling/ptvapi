# Testing for disruptions is hard because there may be none! We rely on skipping
# tests here if possible, but it should be rare to have no disruptions across
# the network, even if we require a variety of disruption modes.

# ---------------------------------------------------------------------------- #
# ---- Define values if they haven't already been defined by another test ---- #
# ---------------------------------------------------------------------------- #
if (!exists("stops_near_flinders_street")) {
  stops_near_flinders_street <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671
  )
}

if (!exists("flinders_street_stop_id")) {
  flinders_street_stop_id <- stops_near_flinders_street %>%
    filter(stop_name == "Flinders Street Railway Station") %>%
    pull(stop_id)
}
# ---------------------------------------------------------------------------- #

all_disruptions <- disruptions()
disruptions_exist <- (nrow(all_disruptions) > 0)


test_that("Can filter disruptions to current or planned", {
  skip_if(nrow(all_disruptions) == 0)
  skip_if_not("Current" %in% all_disruptions$disruption_status)
  skip_if_not("Planned" %in% all_disruptions$disruption_status)
  current_disruptions <- disruptions(disruption_status = "current")
  planned_disruptions <- disruptions(disruption_status = "planned")
  expect_gt(nrow(current_disruptions), 0)
  expect_equal(unique(current_disruptions$disruption_status), "Current")
  expect_gt(nrow(planned_disruptions), 0)
  expect_equal(unique(planned_disruptions$disruption_status), "Planned")
})

test_that("Can filter by disruption modes", {
  skip_if(nrow(all_disruptions) == 0)
  dis_modes <- sort(unique(all_disruptions$disruption_mode))

  skip_if(length(dis_modes) < 2)
  a_dis_mode <- sample(dis_modes, 1)
  disruptions_with_a_dis_mode <- disruptions(disruption_modes = a_dis_mode)
  expect_equal(
    unique(disruptions_with_a_dis_mode$disruption_mode),
    a_dis_mode
  )

  skip_if(length(dis_modes) < 3)
  two_dis_modes <- sort(sample(dis_modes, 2))
  disruptions_with_two_dis_modes <- disruptions(
    disruption_modes = two_dis_modes
  )
  expect_equal(
    sort(unique(disruptions_with_two_dis_modes$disruption_mode)),
    two_dis_modes
  )
})

test_that("Can filter by route_types", {
  # Route types seem to correspond to disruption modes, not the contents of the
  # routes tibbles in the disruptions. If at least 3 disruption modes are
  # present, we test for the ability to filter to 2, using route_type.
  skip_if(nrow(all_disruptions) == 0)
  skip_if_not("metro_train" %in% all_disruptions$disruption_mode_description)
  skip_if_not("metro_tram" %in% all_disruptions$disruption_mode_description)
  skip_if_not("metro_bus" %in% all_disruptions$disruption_mode_description)

  train_route_type <- translate_route_type("Train")
  tram_route_type <- translate_route_type("Tram")
  two_route_types <- c(train_route_type, tram_route_type)
  disruptions_with_two_route_types <- disruptions(
    route_types = two_route_types
  )
  expect_equal(
    sort(unique(disruptions_with_two_route_types$disruption_mode_description)),
    c("metro_train", "metro_tram")
  )
})
