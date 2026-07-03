with_mock_api({
  ### TESTS GetSamenMetenAPI() ----
  test_that("Check input error - wrong project",{
    data_project_error <- try(GetSamenMetenAPI("gemeente eq'Ameland'",
                                               "20250909", "20250912"))

    expect_equal(class(data_project_error), "try-error")

  })

  test_that("Check columns GetSamenMetenAPI - data",{
    # Create the expected names of the columns
    exp_names_dept1 <- c("sensordata", "metingen", "dataopslag")
    exp_names_sensordata <- c("kit_id", "sensor_id", "project", "url_loc",
                              "url_datastream", "knmicode", "pm10closecode",
                              "pm10regiocode", "pm10stadcode", "pm25closecode",
                              "pm25regiocode", "pm25stadcode","id", "lat", "lon")
    exp_names_metingen <- c("waarde", "tijd", "grootheid", "kit_id", "error")
    exp_names_dataopslag <- c("sensor_data", "urls_meet")

    # Get the data
    data_project <- GetSamenMetenAPI("project eq'Deventer Datastad'",
                                     "20250909", "20250911")

    # Test if the data has the expected columns
    expect_equal(names(data_project), exp_names_dept1)
    expect_equal(names(data_project$sensordata), exp_names_sensordata)
    expect_equal(names(data_project$metingen), exp_names_metingen)
    expect_equal(names(data_project$dataopslag), exp_names_dataopslag)
  })

  ### TESTS GetSamenMetenAPIinfoProject() ----
  test_that("check columnnames sensordata and datastream for project data",{
  expect_names_sensordata <-
    c("things_id", "kit_id", "project", "lat", "lon", "knmicode",
      "pm10closecode", "pm10regiocode", "pm10stadcode", "pm25closecode",
      "pm25regiocode", "pm25stadcode")
  expect_names_datastream <-
    c("kit_id", "kit_id_ext", "unit", "datastream_id")

  data_project <- GetSamenMetenAPIinfoProject("HEI")

  expect_equal(expect_names_sensordata, names(data_project$sensor_data))
  expect_equal(expect_names_datastream, names(data_project$datastream_data))

  })

  test_that("Check project data - wrong project",{

    data_project_error <- try(GetSamenMetenAPIinfoProject("HEI_bestaatniet"))

    expect_equal(class(data_project_error), "try-error")

  })

  ### TESTS GetSamenMetenAPIinfo ----
  test_that("check columnnames sensordata and datastream for project data",{
    # Create expected
    expect_names_sensordata <-
      c("things_id", "kit_id", "project", "lat", "lon", "knmicode",
        "pm10closecode", "pm10regiocode", "pm10stadcode", "pm25closecode",
        "pm25regiocode", "pm25stadcode")
    expect_names_datastream <-
      c("kit_id", "kit_id_ext", "unit", "datastream_id")

    # Get the data
    data_project <- GetSamenMetenAPIinfo("project eq'HEI'")

    # test expected
    expect_equal(expect_names_sensordata, names(data_project$sensor_data))
    expect_equal(expect_names_datastream, names(data_project$datastream_data))

  })


  ### TESTS GetSamenMetenAPIinfoMuni() ----
  # Todo, deze nog aanvullen


  ### TESTS GetSamenMetenAPIobs() ----
  test_that("Check input error - wrong project",{
    # create expected
    exp_names_obs <- c("kit_id", "parameter", "timestamp", "value")

    # Get data
    data_obs <- GetSamenMetenAPIobs("31508","LTD_55101","20220101","20220103")

    # test expected with data
    expect_equal(class(data_obs), "data.frame")
    expect_equal(exp_names_obs, names(data_obs))

  })

  })
