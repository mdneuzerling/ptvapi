# I've avoided this dependency for the package functions, because I truly
# believe it's unnecessary. But we can be more liberal with dependencies for
# the tests.

library(dplyr, quietly = TRUE)
library(lubridate, quietly = TRUE)

# The API doesn't seem to keep a full history, so dates too far in the past
# may return empty results. Our test times will be two days from today: one in
# the morning, and one in the afternoon.

morning_test_time <- paste(
  format(Sys.Date() + 2, format = "%Y-%m-%d", tz = "Australia/Melbourne"),
  "T07:48:08"
)

afternoon_test_time <- paste(
  format(Sys.Date() + 2, format = "%Y-%m-%d", tz = "Australia/Melbourne"),
  "16:48:08"
)
