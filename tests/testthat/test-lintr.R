# nolint start
# This only works if I run `rcmdcheck::rcmdcheck` from the console.
# If I run this in the Build panel in RStudio, or through Github Actions, I get:
#
# invalid 'path' argument
# Backtrace:
#   1. lintr::expect_lint_free()
#   2. lintr::lint_package(...)
#   3. base::normalizePath(path, mustWork = FALSE)
#   4. base::path.expand(path)
#
# This seems to be related to the exclusions: If I remove the # nolint tags in
# ptv-verbs.R, everything is okay.
#
# test_that("Package linting", {
#   lintr::expect_lint_free()
# })
# nolint end
