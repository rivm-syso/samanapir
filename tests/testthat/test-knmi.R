httptest2::with_mock_dir("knmi", {

  test_that("Data knmi has data and expected columns",{
  # initialisatie
  stations <- c('260', '248')
  ymd_vanaf = 20191214
  ymd_tot =20191215

  # Get the data
  knmi_data <- GetKNMIAPI2(stations, ymd_vanaf, ymd_tot )

  # Tests
  # For part info
  expect_equal(nrow(knmi_data$info), 2)
  expect_equal(knmi_data$info$NAME[1], "De Bilt")
  expect_named(knmi_data$info, c("STNS", "LON", "LAT",
                               "ALT", "NAME"),
               ignore.order = TRUE,
               ignore.case = TRUE)

  # For part data
  expect_equal(nrow(knmi_data$data), 144)
  expect_named(knmi_data$data, c("STNS", "YYYYMMDD", "HH", "DD", "FF", "TEMP",
                                 "U", "tijd"),
               ignore.order = TRUE,
               ignore.case = TRUE)
  })
})
