# Test for getting combi station_info and data from luchtmeetnet ----
httptest2::with_mock_dir("luchtmeetnet_api_combi",{
  test_that("No data available for station",{
    # Get data
    data_station <- GetLMLAPI2("NL01908", "20260505", "20260510")

    # Test
    expect_named(data_station, c("info", "data"))
    expect_named(data_station$info, c("station_number", "naam", "lat", "lon",
                                 "stattype", "organisatie"))
    expect_length(data_station$data, 0)
  })
  })

# Test for getting the station information from Luchtmeetnet ----
httptest2::with_mock_dir("luchtmeetnet_api_statinfo", {

  test_that("Test name and organisation of specific station",{
    station_info <- GetLMLstatinfoAPI2("NL01908")
    expect_identical(station_info$organisatie, "DCMR (Rijnmond)")
    expect_identical(station_info$naam, "Alblasserdam-Ruigenhil")
    expect_identical(round(station_info$lon,2), 4.66)
    expect_identical(round(station_info$lat,2), 51.86)
  })

  test_that("Geen geldig station meegegeven",{
    # Hier komt geen error uit maar een NULL
    station_error <-(GetLMLstatinfoAPI2("NL0190"))
    expect_equal(station_error, NULL)
  })

  test_that("All stations are obtained",{
    #Expected amount stations
    exp_stations <- 102

    # Get the data
    all_stations <- GetLMLallstatinfoAPI2()

    # Test
    expect_equal(nrow(all_stations), exp_stations)
    expect_named(all_stations, c("station_number", "naam", "lat",
                                 "lon", "organisatie", "stattype"),
                 ignore.order = TRUE,
                 ignore.case = TRUE)
  })
})

# Test for getting the data from Luchtmeetnet ----
httptest2::with_mock_dir("luchtmeetnet_api_data", {
  test_that("All data is there, grootheden PM available",{
    data_station <- GetLMLstatdataAPI2("NL49012", "20260205", "20260215")
    expect_length(data_station$value, 1934)
    expect_equal("PM25" %in% data_station$formula, TRUE)
    expect_equal("PM10" %in% data_station$formula, TRUE)
    expect_named(data_station, c("station_number", "value", "timestamp_measured",
                                 "formula"),
                 ignore.order = TRUE,
                 ignore.case = TRUE)
  })

  test_that("No data available for station",{
    # Get the data
    data_station <- GetLMLstatdataAPI2("NL01908", "20190505", "20190510")
    # Test that empty dataframe
    expect_length(data_station, 0)
  })
})

# test API call returns NULL
local_mocked_bindings(GetAPIDataframe2 = function(req_url_api) {
  return(NULL)
})

test_that("For all LML-functions NULL is returned",{
  # Expect NULL : GetLMLstatinfoAPI2
  data_station <- GetLMLstatinfoAPI2("NL49012")
  expect_true(is.null(data_station))

  # Expect NULL : GetLMLallstatinfoAPI2
  data_all_stations <- GetLMLallstatinfoAPI2()
  expect_true(is.null(data_all_stations))

  # Expect NULL : GetLMLstatdataAPI2
  data_lml <- GetLMLstatdataAPI2("NL01495", "20260505", "20260510")
  expect_true(is.null(data_lml))

  # Expect NULL : GetLMLAPI2
  all_lml <- GetLMLAPI2("NL01908", "20260505", "20260510")
  expect_true(is.null(all_lml$info))
  expect_true(is.null(all_lml$data))
  expect_true(is.list(all_lml))
  expect_named(all_lml, c("info", "data"))
})

# test API call returns empty list
local_mocked_bindings(GetAPIDataframe2 = function(req_url_api) {
  return(list())
})

test_that("For all LML-functions empty dataframe is returned",{
    # Expect empty dataframe : GetLMLstatinfoAPI2
    data_station <- GetLMLstatinfoAPI2("NL49012")
    expect_equal(class(data_station), "data.frame")
    expect_equal(nrow(data_station), 0)

    # Expect empty dataframe :GetLMLallstatinfoAPI2
    data_all_stations <- GetLMLallstatinfoAPI2()
    expect_equal(class(data_all_stations), "data.frame")
    expect_equal(nrow(data_all_stations), 0)

    # Expect empty dataframe : GetLMLstatdataAPI2
    data_lml <- GetLMLstatdataAPI2("NL01495", "20260505", "20260510")
    expect_equal(class(data_lml), "data.frame")
    expect_equal(nrow(data_lml), 0)

    # Expect empty dataframe : GetLMLAPI2
    all_lml <- GetLMLAPI2("NL01908", "20260505", "20260510")
    expect_equal(class(all_lml$info), "data.frame")
    expect_equal(nrow(all_lml$info), 0)
    expect_equal(class(all_lml$data), "data.frame")
    expect_equal(nrow(all_lml$data), 0)
    expect_true(is.list(all_lml))
    expect_named(all_lml, c("info", "data"))
})
