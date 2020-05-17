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
  skip_if(nrow(all_disruptions) == 0)
  affected_routes <- rbind(all_disruptions$routes)
  affected_route_types <- sort(unique(affected_routes$route_type))

  skip_if(length(affected_route_types) < 2)
  a_route_type <- sample(affected_route_types, 1)
  disruptions_with_a_route_type <- disruptions(
    route_types = a_route_type
  )
  expect_equal(
    unique(rbind(disruptions_with_a_route_type$routes)$route_type),
    a_route_type
  )

  skip_if(length(affected_route_types) < 3)
  two_route_types <- sort(sample(affected_route_types, 2))
  disruptions_with_two_route_types <- disruptions(
    route_types = two_route_types
  )
  expect_equal(
    sort(unique(rbind(disruptions_with_two_route_types$routes)$route_type)),
    two_route_types
  )
})
