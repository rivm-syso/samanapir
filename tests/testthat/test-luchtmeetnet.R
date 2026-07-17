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
