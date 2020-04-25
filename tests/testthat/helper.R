library(dplyr, quietly = TRUE)
frankston_route_id <- dplyr::filter(
  routes(), route_name == "Frankston"
)$route_id
train_route_type <- translate_route_type("Train")
