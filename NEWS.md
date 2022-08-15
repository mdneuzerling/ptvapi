# ptvapi 2.0.3

* Fixed 301 link in README

# ptvapi 2.0.2

* Remove HTML tags in logo unsupported in HTML5 to meet new CRAN policy.

# ptvapi 2.0.1

* Remove `lazyload` in DESCRIPTION to meet new CRAN policy (this package has no data)

# ptvapi 2.0.0

* All API request now use SSL by default. To force connections without SSL, set the option `use_insecure_ptv_connection` to `TRUE`.
* In response to changes to the PTV API, `run_information` now requires a character `run_ref` instead of an integer `run_id`. Functions will attempt to convert the argument to a character, and so integer arguments will still work for runs that are not bus or nightrider runs.
* Functions that return `route_type` will now also return `route_type_description`, which is a human-readable interpretation of the route type. For example, when `route_type` is 0, `route_type_description` is "Train".
* In response to changes to the PTV API, run functions now support a `date_utc` argument for returning results as of a given date. This is experimental, and possibly non-functional.
* Functions that internally convert between a route type and a human-readable description of a route type will use a cached version of `route_types`, in order to reduce the number of queries of the API.
* Fixed failing assertion that was causing `run_information` to fail when specifying a route type.
* The column `service_status_timestampe` in data returned by route functions has been changed to `service_status_timestamp` .

# ptvapi 1.1.3

* Changed unit tests to use lubridate instead of base for time zone declarations. The lubridate package better supports adding time zones to NA datetimes, as this was causing issues with R-devel (4.0.3).

# ptvapi 1.1.2

This release was the first to be accepted to CRAN.

* Fixed misquoted metadata in package DESCRIPTION

# ptvapi 1.1.1

* Fixed invalid URL in package DESCRIPTION

# ptvapi 1.1.0

* Adjustments to package DESCRIPTION to meet CRAN standards
* Removed internal function need_api_details, as it depended on changing the user's options. This function has been incorporated into the error messages in `determine_user_id` and `determine_api_key`.
* Removed documentation for the following internal functions, which should resolve the issue of missing argument values in the respective .Rd files: `determine_user_id`, `determine_api_key`, `prefix_base_url`, `prefix_version`, `prefix_base_url_and_version`
* Created `value` Rd-tag for `departures` function
* Removed examples for the following internal functions: `convert_to_melbourne_time`, `PTVGET` 
* Added `ptvapi:::` to examples in the following internal functions: `add_parameter`, `add_parameters`
* Removed internal function `route_types_cached`, and replaced all uses with `route_types`. With these changes, there are no references to user options remaining in the package.
