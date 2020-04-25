# NSE can trip up the lintr, especially with glue.
# We add those variables here so that lintr is happy.
utils::globalVariables(c(
  "version", "conjunction", "base_url", "version"
))
