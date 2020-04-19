route_types <- function(user_id = determine_user_id(),
                        api_key = determine_api_key()) {
  PTVGET(request = "route_types", user_id = user_id, api_key = api_key)
}
