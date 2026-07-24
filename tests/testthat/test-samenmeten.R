# Test voor getting de sensor info specific - for a municipality
httptest2::with_mock_dir("SamenMeten_info_municipality_60",{
  test_that("GetSamenMetenAPIinfoMuni2",{
    # Expected
    names_list <- c("sensor_data", "datastream_data")
    names_sensordata <-
       c("things_id", "kit_id", "project", "lat", "lon","knmicode","pm10closecode",
        "pm10regiocode", "pm10stadcode","pm25closecode","pm25regiocode",
        "pm25stadcode")
    names_datastream <-
       c("kit_id_ext", "unit", "datastream_id", "kit_id", "url_prop", "url_obs")
    dim_sensordata <- c(7L, 12L)
    dim_datastream <- c(37L, 6L)

    # Get the data
    muni_info <- GetSamenMetenAPIinfoMuni2("60")

    # Check with expected
    expect_true(is.list(muni_info))
    expect_true(is.data.frame(muni_info$sensor_data))
    expect_true(is.data.frame(muni_info$datastream_data))

    expect_named(muni_info, names_list)
    expect_named(muni_info$sensor_data, names_sensordata, ignore.order = TRUE)
    expect_named(muni_info$datastream_data, names_datastream, ignore.order = TRUE)

    expect_equal(dim(muni_info$sensor_data), dim_sensordata)
    expect_equal(dim(muni_info$datastream_data), dim_datastream)
  })
})

# Test voor getting de sensor info specific- for a project
httptest2::with_mock_dir("SamenMeten_info_project_amersfoort",{
  test_that("GetSamenMetenAPIinfoProject2",{
    # Expected
    names_list <- c("sensor_data", "datastream_data")
    names_sensordata <-
      c("things_id", "kit_id", "project", "lat", "lon","knmicode","pm10closecode",
        "pm10regiocode", "pm10stadcode","pm25closecode","pm25regiocode",
        "pm25stadcode")
    names_datastream <-
      c("kit_id_ext", "unit", "datastream_id", "kit_id", "url_prop", "url_obs")
    dim_sensordata <- c(18L, 12L)
    dim_datastream <- c(99L, 6L)

    # Get the data
    project_info <- GetSamenMetenAPIinfoProject2("Amersfoort")

    # Check with expected
    expect_true(is.list(project_info))
    expect_true(is.data.frame(project_info$sensor_data))
    expect_true(is.data.frame(project_info$datastream_data))

    expect_named(project_info, names_list)
    expect_named(project_info$sensor_data, names_sensordata, ignore.order = TRUE)
    expect_named(project_info$datastream_data, names_datastream, ignore.order = TRUE)

    expect_equal(dim(project_info$sensor_data), dim_sensordata)
    expect_equal(dim(project_info$datastream_data), dim_datastream)
  })
})

# Test voor getting de sensor info generic - for a municipality
httptest2::with_mock_dir("SamenMeten_info_municipality_60",{
  test_that("GetSamenMetenAPIinfo2 - municipality",{
    # Expected
    names_list <- c("sensor_data", "datastream_data")
    names_sensordata <-
      c("things_id", "kit_id", "project", "lat", "lon","knmicode","pm10closecode",
        "pm10regiocode", "pm10stadcode","pm25closecode","pm25regiocode",
        "pm25stadcode")
    names_datastream <-
      c("kit_id_ext", "unit", "datastream_id", "kit_id", "url_prop", "url_obs")
    dim_sensordata <- c(7L, 12L)
    dim_datastream <- c(37L, 6L)

    # Get the data
    muni_info <- GetSamenMetenAPIinfo2("codegemeente eq'60'")

    # Check with expected
    expect_true(is.list(muni_info))
    expect_true(is.data.frame(muni_info$sensor_data))
    expect_true(is.data.frame(muni_info$datastream_data))

    expect_named(muni_info, names_list)
    expect_named(muni_info$sensor_data, names_sensordata, ignore.order = TRUE)
    expect_named(muni_info$datastream_data, names_datastream, ignore.order = TRUE)

    expect_equal(dim(muni_info$sensor_data), dim_sensordata)
    expect_equal(dim(muni_info$datastream_data), dim_datastream)

  })
})

# Test voor getting de sensor info generic - for a project
httptest2::with_mock_dir("SamenMeten_info_project_amersfoort",{
  test_that("GetSamenMetenAPIinfo2 - project",{
    # Expected
    names_list <- c("sensor_data", "datastream_data")
    names_sensordata <-
      c("things_id", "kit_id", "project", "lat", "lon","knmicode","pm10closecode",
        "pm10regiocode", "pm10stadcode","pm25closecode","pm25regiocode",
        "pm25stadcode")
    names_datastream <-
      c("kit_id_ext", "unit", "datastream_id", "kit_id", "url_prop", "url_obs")
    dim_sensordata <- c(18L, 12L)
    dim_datastream <- c(99L, 6L)

    # Get the data
    project_info <- GetSamenMetenAPIinfo2("project eq'Amersfoort'")

    # Check with expected
    expect_true(is.list(project_info))
    expect_true(is.data.frame(project_info$sensor_data))
    expect_true(is.data.frame(project_info$datastream_data))

    expect_named(project_info, names_list)
    expect_named(project_info$sensor_data, names_sensordata, ignore.order = TRUE)
    expect_named(project_info$datastream_data, names_datastream, ignore.order = TRUE)

    expect_equal(dim(project_info$sensor_data), dim_sensordata)
    expect_equal(dim(project_info$datastream_data), dim_datastream)

  })
})

# Test voor getting observations
httptest2::with_mock_dir("SamenMeten_observations",{
  test_that("GetSamenMetenAPIobs2",{
    # Expected
    exp_dim <- c(47L, 4L)
    exp_names <- c("timestamp", "kit_id", "value", "parameter")

    # Get the data
    obs_data <- GetSamenMetenAPIobs2("31508","LTD_55101","20220101","20220103")

    # Check with expected
    expect_true(is.data.frame(obs_data))
    expect_named(obs_data, exp_names, ignore.order = TRUE)
    expect_equal(dim(obs_data), exp_dim)
  })
})

# Test voor getting all data, info and observations
httptest2::with_mock_dir("SamenMeten_all_data",{
  test_that("GetSamenMetenAPI2",{
    # Expected
    names_list <- c("sensordata", "metingen")
    names_sensordata <-
      c("things_id", "kit_id", "project", "lat", "lon", "knmicode",
        "pm10closecode", "pm10regiocode", "pm10stadcode", "pm25closecode",
        "pm25regiocode", "pm25stadcode")
    names_metingen <-
      c("waarde", "tijd", "name", "kit_id")
    dim_sensordata <- c(18L, 12L)
    dim_metingen <- c(4135L, 4L)

    # Get the data
    all_data <- GetSamenMetenAPI2("project eq'Amersfoort'","20190909", "20190912")

    # Check with expected
    expect_true(is.list(all_data))
    expect_true(is.data.frame(all_data$sensordata))
    expect_true(is.data.frame(all_data$metingen))

    expect_named(all_data, names_list, ignore.order = TRUE)
    expect_named(all_data$sensordata, names_sensordata, ignore.order = TRUE)
    expect_named(all_data$metingen, names_metingen, ignore.order = TRUE)

    expect_equal(dim(all_data$sensordata), dim_sensordata)
    expect_equal(dim(all_data$metingen), dim_metingen)
  })
})
