determine_user_id <- function() {
  user_id <- Sys.getenv("PTV_USER_ID")
  if (identical(user_id, "")) {
    stop("Could not find PTV_USER_ID environment variable. See ?ptvapi for ",
         "details on obtaining and providing this information.")
  }
  user_id
}

determine_api_key <- function() {
  api_key <- Sys.getenv("PTV_API_KEY")
  if (identical(api_key, "")) {
    stop("Could not find PTV_API_KEY environment variable. See ?ptvapi for ",
         "details on obtaining and providing this information.")
  }
  api_key
}
