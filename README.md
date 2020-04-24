
# ptvapi

<!-- badges: start -->
[![R build status](https://github.com/mdneuzerling/ptvapi/workflows/R-CMD-check/badge.svg)](https://github.com/mdneuzerling/ptvapi/actions)
<!-- badges: end -->

This package provides a friendly interface to the Public Transport Victoria (PTV) API. Results are returned as tibbles where possible, and authentication is handled under the hood.

## Installing

You can install this package with `remotes::install_github(mdneuzerling/ptvapi)`

## Authentication

Using the API requires a user ID (also called a `devid`) and an API key from Public Transport Victoria. These can be requested in an email. Refer to the [PTV website](https://www.ptv.vic.gov.au/footer/data-and-reporting/datasets/ptv-timetable-api/) for instructions.

The user ID and API key be provided directly to the functions, for example:
```
routes(
  user_id = 1234567,
  api_key = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
)
```

Alternatively, this information can be set as environment variables. These can be configured directly within R:
```
Sys.setenv("PTV_USER_ID" = 1234567)
Sys.setenv("PTV_API_KEY" = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
```

If a `user_id` or `api_key` value is not provided to the functions within this package, then they will be retrieved from the "PTV_USER_ID" and "PTV_API_KEY" environment variables, if possible.

## Example usage

The code examples below assume that you've set environment variables for authentication.

```
# list of all routes
routes()
# A tibble: 828 x 7
#   route_id route_gtfs_id route_name route_type route_number service_status
#      <int> <chr>         <chr>           <int> <chr>        <chr>    
```

```
# Train stops near Flinders Street Station
stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671,
    route_types = "Train"
  )
# # A tibble: 1 x 8
#   stop_id stop_name stop_suburb route_type stop_sequence stop_latitude stop_longitude
#     <int> <chr>     <chr>            <int>         <int>         <dbl>          <dbl>
```

## A note about route types

The API recognises five route types: "Train", "Tram", "Bus", "Vline", and "Night Bus". Many functions have arguments such as `route_type` and `route_types` that expect a non-negative integer code representing these route types. To simplify calling the API, these functions will also accept a character description like those above. Under the hood, the functions will translate these descriptions to the non-negative integer codes that the API expects.

## Implementation progress

The below information can be obtained with the API. Some are split into multiple functions, eg. `stops` is represented by functions `stops_on_route()`, `stops_nearby()`, and `stop_information()`

1. `departures`
1. `directions`
1. `route_type`
1. `routes`
1. `stops`

Yet to be implemented:

1. `disruptions`
1. `outlets`
1. `patterns`
1. `runs`
1. `search`

In addition, many of the implemented functions make API calls that have optional parameters, and these optional parameters have not all been implemented.
