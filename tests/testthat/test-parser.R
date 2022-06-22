# Time ####
test_that("parse_ISO8601", {
  expect_equal(
    parse_ISO8601("T20"),
    list(time=TRUE, hour="20")
  )
  
  expect_equal(
    parse_ISO8601("T20Z"),
    list(time=TRUE, hour="20", tz="Z")
  )
  expect_equal(
    parse_ISO8601("T20+00"),
    list(time=TRUE, hour="20", tz_plus_minus="+", tz_hour="00")
  )
  expect_equal(
    parse_ISO8601("T20+00:00"),
    list(time=TRUE, hour="20", iso_8601_format="extended", tz_plus_minus="+", tz_hour="00", tz_minute="00"),
    list_as_map=TRUE
  )
  expect_equal(
    parse_ISO8601("T20+00:15"),
    list(time=TRUE, hour="20", iso_8601_format="extended", tz_plus_minus="+", tz_hour="00", tz_minute="15"),
    list_as_map=TRUE
  )
  expect_equal(
    parse_ISO8601("T20-00:15"),
    list(time=TRUE, hour="20", iso_8601_format="extended", tz_plus_minus="-", tz_hour="00", tz_minute="15"),
    list_as_map=TRUE
  )
  
  expect_error(
    parse_ISO8601("T20-00")
  )
  expect_error(
    parse_ISO8601("T20-00:00")
  )
  expect_equal(
    parse_ISO8601("20201101"),
    list(year = "2020", iso_8601_format = "basic", month = "11", mday = "01", date=TRUE),
    list_as_map=TRUE
  )
  expect_equal(
    parse_ISO8601("2020.123"),
    list(year = "2020", fraction = "123", date=TRUE),
    list_as_map=TRUE
  )
  expect_equal(
    parse_ISO8601("202011.123"),
    list(year = "2020", iso_8601_format = "basic", month = "11", fraction = "123", date=TRUE),
    list_as_map=TRUE
  )
  expect_equal(
    parse_ISO8601("20201101.123"),
    list(date = TRUE, year = "2020", iso_8601_format = "basic", month = "11", 
         mday = "01", fraction = "123")
  )
  expect_equal(
    parse_ISO8601("20201101"),
    list(date = TRUE, year = "2020", iso_8601_format = "basic", month = "11", 
         mday = "01")
  )
  expect_equal(
    parse_ISO8601("2020110"),
    list(date = TRUE, year = "2020", iso_8601_format = "basic", oday = "110")
  )
  expect_equal(
    parse_ISO8601("2020-11-01"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01")
  )
  expect_equal(
    parse_ISO8601("2020-110"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         oday = "110")
  )
  expect_equal(
    parse_ISO8601("2020110T12"),
    list(date = TRUE, year = "2020", iso_8601_format = "basic", oday = "110", 
         time = TRUE, hour = "12")
  )
  expect_equal(
    parse_ISO8601("2020110T1234"),
    list(date = TRUE, year = "2020", iso_8601_format = "basic", oday = "110", 
         time = TRUE, hour = "12", minute = "34")
  )
  expect_error(
    parse_ISO8601("2020110T12:34"),
    regexp="mismatch with iso_8601_format: basic vs extended"
  )
  expect_equal(
    parse_ISO8601("2020110T1234.5678"),
    list(date = TRUE, year = "2020", iso_8601_format = "basic", oday = "110", 
         time = TRUE, hour = "12", minute = "34", fraction = "5678")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.56789"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "56789")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.567890"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "567890")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.5678901"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "5678901")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.56789012"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "56789012")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.567890123"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "567890123")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.5678901234"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "5678901234")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.56789012345"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "56789012345")
  )
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.567890123456"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "567890123456")
  )
  
  # Timezone ####
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.567890Z"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "567890", tz = "Z")
  )
  
  expect_equal(
    parse_ISO8601("2020-11-01T12:34:56.567890+01:15"),
    list(date = TRUE, year = "2020", iso_8601_format = "extended", 
         month = "11", mday = "01", time = TRUE, hour = "12", minute = "34", 
         second = "56", fraction = "567890", tz_plus_minus = "+", 
         tz_hour = "01", tz_minute = "15")
  )
})

# Duration #####

test_that("duration", {
  expect_equal(
    parse_ISO8601("P1Y1M1DT1H1M1.1S"),
    list(duration = TRUE, duration_yearnum = "1", duration_monthnum = "1", 
         duration_daynum = "1", duration_hournum = "1", duration_minutenum = "1", 
         duration_secondnum = "1", fraction = "1")
  )
})

# Repeating interval ####

test_that("repeating interval", {
  expect_equal(
    parse_ISO8601("R/P1Y2M3DT4H5M6.7S"),
    list(repeating_interval = TRUE, interval = TRUE, duration = list(
      duration = TRUE, duration_yearnum = "1", duration_monthnum = "2", 
      duration_daynum = "3", duration_hournum = "4", duration_minutenum = "5", 
      duration_secondnum = "6", fraction = "7"))
  )
  expect_equal(
    parse_ISO8601("R8/P1Y2M3DT4H5M6.7S"),
    list(
      repeating_interval = TRUE, repeating_interval_n = "8", interval = TRUE,
      duration =
        list(
          duration = TRUE, duration_yearnum = "1", 
          duration_monthnum = "2", duration_daynum = "3", duration_hournum = "4", 
          duration_minutenum = "5", duration_secondnum = "6", fraction = "7"
        )
    )
  )
})

# Interval ####

test_that("interval", {
  expect_equal(
    parse_ISO8601("2020-01-12/2020-02-12"),
    list(interval = TRUE,
         start = list(
           date = TRUE, year = "2020", 
           iso_8601_format = "extended", month = "01", mday = "12"
         ), 
         end = list(
           date = TRUE, year = "2020", iso_8601_format = "extended", 
           month = "02", mday = "12"
         )
    )
  )
  expect_equal(
    parse_ISO8601("2020-01-12/02-12"),
    list(interval = TRUE,
         start = list(
           date = TRUE, year = "2020", 
           iso_8601_format = "extended", month = "01", mday = "12"), 
         end = list(
           month = "02", iso_8601_format = "extended", mday = "12"
         )
    )
  )
})

