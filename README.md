
# rally

R wrapper for Danfoss Ally API

## Installation

Install the development version from github:

``` r
## install.packages("devtools")
devtools::install_github("itkonen/rally")
```

## Getting started

Once installed, follow the setup guide at
<https://developer.danfoss.com/> to get your API key and secret. The API
uses an OAuth 2.0 client credentials grant to give access to your data.

Authorize your app with your API key and secret:

    library(rally)
    authorize("my_key", "my_secret")

To get one or all devices and their status, use `get_data`

    get_data()
    get_data(id = "abcd1234")

For convenience, you can use `devices()` to get a single tibble
containing both device and status information.

Set device status with `set_device`, e.g.:

    set_device(id = "abcd1234", code = "temp_set", "200")

or use the shorthand wrapper:

    temp_set(id = "abcd1234", 20.0)
