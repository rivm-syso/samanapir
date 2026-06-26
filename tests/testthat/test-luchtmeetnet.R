with_mock_api({
  test_that("test name and organisation of specific station",{
    station_info <- GetLMLstatinfoAPI("NL01908")
    expect_identical(station_info$organisatie, "DCMR (Rijnmond)")
    expect_identical(station_info$naam, "Alblasserdam-Ruigenhil")
    expect_identical(round(station_info$lon,2), 4.66)
    expect_identical(round(station_info$lat,2), 51.86)
  })

  test_that("Geen geldig station meegegeven",{
    station_error <- tryCatch(GetLMLstatinfoAPI("NL0190"),
                              error = function(e) {
                                "Error van api luchtmeetnet."
                              })
    expect_identical(station_error, "Error van api luchtmeetnet.")
  })

  test_that("Alle data is opgehaald, grootheden PM aanwezig",{
    data_station <- GetLMLstatdataAPI("NL49012", "20260205", "20260215")
    expect_equal(length(data_station$value), 1584)
    expect_equal("PM25" %in% data_station$formula, TRUE)
    expect_equal("PM10" %in% data_station$formula, TRUE)

  })

})

# TIP: for creating datasets
# start_capturing()
# GetLMLstatinfoAPI("NL01908")
# stop_capturing()
