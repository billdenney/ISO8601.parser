# General parser support functions ####

l_parser_general <-
  list(
    tokens=TOKENS,
    literals=LITERALS,
    verbose=FALSE,
    allow_leap_second=FALSE,
    allow_before_1583=TRUE,

    p_fraction=function(doc="fraction : DECIMALPOINT multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(fraction=p$get(3)))
    },
    p_multi_digit=function(doc="multi_digit : DIGIT
                                            | digit2
                                            | digit3
                                            | digit4
                                            | digit4 DIGIT
                                            | digit4 digit2
                                            | digit4 digit3
                                            | digit4 digit4
                                            | DIGIT9PLUS", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      if (p$length() > 2) {
        p$set(1, p_collapse(2:3, p))
      } else {
        p$set(1, p$get(2))
      }
    },
    p_digit4=function(doc="digit4 : digit3 DIGIT", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, p_collapse(2:3, p))
    },
    p_digit3=function(doc="digit3 : digit2 DIGIT", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, p_collapse(2:3, p))
    },
    p_digit2=function(doc="digit2 : DIGIT DIGIT", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, p_collapse(2:3, p))
    },
    p_basic=function(doc="basic : ", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(iso_8601_format="basic"))
    },
    p_error = function(p) {
      if(is.null(p)) {
        rlang::abort("Syntax error at EOF", class="ISO8601_parser_parse_error")
      } else {
        rlang::abort(
          sprintf(
            "Syntax error at '%s'\n%s\n%s^",
            p$value, p$lexer$lexdata, strrep(' ', p$lexpos - 1)
          ),
          class="ISO8601_parser_parse_error"
        )
      }
    }
  )

# Specific numbers ####

l_specific_numbers <-
  list(
    p_yearnum=function(doc="yearnum : digit4", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      if (!self$allow_before_1583) {
        value_between(p$get(2), 1583, 9999)
      }
      p$set(1, list(year=p$get(2)))
    },
    p_monthnum=function(doc="monthnum : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 1, 12)
      p$set(1, list(month=p$get(2)))
    },
    p_month_mdaynum=function(doc="month_mdaynum : digit4", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      month <- substr(p$get(2), 1, 2)
      mday <- substr(p$get(2), 3, 4)
      value_between(month, 1, 12)
      value_between(mday, 1, 31)
      p$set(
        1,
        list(
          month=month,
          mday=mday,
          iso_8601_format="basic"
        )
      )
    },
    p_mdaynum=function(doc="mdaynum : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 1, 31)
      p$set(1, list(mday=p$get(2)))
    },
    p_weeknum=function(doc="weeknum : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 1, 53)
      p$set(1, list(week=p$get(2)))
    },
    p_weekdaynum=function(doc="weekdaynum : DIGIT", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 1, 7)
      p$set(1, list(weekday=p$get(2)))
    },
    p_odaynum=function(doc="odaynum : digit3", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 1, 366)
      p$set(1, list(oday=p$get(2)))
    },
    p_hournum=function(doc="hournum : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 0, 23)
      p$set(1, list(hour=p$get(2)))
    },
    p_minutenum=function(doc="minutenum : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), 0, 59)
      p$set(1, list(minute=p$get(2)))
    },
    p_secondnum=function(doc="secondnum : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      if (self$allow_leap_second) {
        value_between(p$get(2), 0, 60)
      } else {
        value_between(p$get(2), 0, 59)
      }
      p$set(1, list(second=p$get(2)))
    }
  )

# Date/Time Parser ####

l_iso8601_datetime <-
  list(
    p_date_or_time=function(doc="date_or_time : date
                                              | time", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_date=function(doc="date : year", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, c(list(date=TRUE), set_value(p)))
    },
    p_year=function(doc="year : yearnum
                              | yearnum fraction
                              | yearnum basic subyear
                              | yearnum dash subyear", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_subyear=function(doc="subyear : month_mday
                                    | month
                                    | week
                                    | oday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_month_mday=function(doc="month_mday : month_mdaynum
                                          | month_mdaynum fraction
                                          | month_mdaynum subday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_month=function(doc="month : monthnum
                                | monthnum fraction
                                | monthnum basic mday
                                | monthnum dash mday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_mday=function(doc="mday : mdaynum
                              | mdaynum fraction
                              | mdaynum subday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_week=function(doc="week : week_w weeknum
                              | week_w weeknum fraction
                              | week_w weeknum basic weekday
                              | week_w weeknum dash weekday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_week_w=function(doc="week_w : 'W'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list())
    },
    p_weekday=function(doc="weekday : weekdaynum
                                    | weekdaynum fraction
                                    | weekdaynum subday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_oday=function(doc="oday : odaynum
                              | odaynum fraction
                              | odaynum subday", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_subday=function(doc="subday : time_with_t", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_time=function(doc="time : time_with_t
                              | time_without_t", p) {
      # if just hour is given, it must be preceded by 'T'
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, c(list(time=TRUE), set_value(p)))
    },
    p_time_with_t=function(doc="time_with_t : time_with_t_notz
                                            | time_with_t_notz tz", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, c(list(time=TRUE), set_value(p)))
    },
    p_time_with_t_notz=function(doc="time_with_t_notz : time_t hournum
                                                      | time_t hournum fraction
                                                      | time_t time_without_t", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, c(list(time=TRUE), set_value(p)))
    },
    p_time_t=function(doc="time_t : 'T'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list())
    },
    p_time_without_t=function(doc="time_without_t : time_without_t_notz
                                                  | time_without_t_notz tz", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_time_without_t_notz=function(doc="time_without_t_notz : hournum basic minute
                                                            | hournum colon minute", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_minute=function(doc="minute : minutenum
                                  | minutenum fraction
                                  | minutenum basic second
                                  | minutenum colon second", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_second=function(doc="second : secondnum
                                  | secondnum fraction", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_dash=function(doc="dash : '-'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(iso_8601_format="extended"))
    },
    p_colon=function(doc="colon : ':'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(iso_8601_format="extended"))
    }
  )

# Timezone parser ####

l_timezone <-
  list(
    p_tz=function(doc="tz : tz_zulu
                          | tz_plus_minus tz_hour
                          | tz_plus_minus tz_hour basic tz_minute
                          | tz_plus_minus tz_hour colon tz_minute", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      tz_value <- set_value(p)
      if ("00" %in% tz_value$tz_hour &
          ("00" %in% tz_value$tz_minute | is.null(tz_value$tz_minute)) &
          "-" %in% tz_value$tz_plus_minus) {
        stop("timezone zero (00 or 00:00) must have a plus not a minus")
      }
      p$set(1, tz_value)
    },
    p_tz_plus_minus=function(doc="tz_plus_minus : '+'
                                                | '-'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(tz_plus_minus=p$get(2)))
    },
    p_tz_minute=function(doc="tz_minute : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      stopifnot("tz_minute must be 00, 15, 30, or 45"=as.numeric(p$get(2)) %in% c(0, 15, 30, 45))
      p$set(1, list(tz_minute=p$get(2)))
    },
    p_tz_hour=function(doc="tz_hour : digit2", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      value_between(p$get(2), -1, 15)
      p$set(1, list(tz_hour=p$get(2)))
    },
    p_tz_zulu=function(doc="tz_zulu : 'Z'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(tz="Z"))
    }
  )

# Duration parser ####

l_duration <-
  list(
    p_duration=function(doc="duration : duration_p duration_parts", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    # Reading wikipedia suggests that week and year cannot be combined, so it
    # should not be part of duration_subyear
    p_duration_parts=function(doc="duration_parts : duration_year
                                                  | duration_week
                                                  | duration_subyear
                                                  | date_or_time", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_year=function(doc="duration_year : duration_yearnum duration_y
                                                | duration_yearnum duration_y duration_subyear", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_week=function(doc="duration_week : duration_weeknum duration_w
                                                | duration_weeknum fraction duration_w
                                                | duration_weeknum duration_w duration_subweek", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_subyear=function(doc="duration_subyear : duration_subweek
                                                      | duration_monthnum duration_m
                                                      | duration_monthnum fraction duration_m
                                                      | duration_monthnum duration_m duration_subweek", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_subweek=function(doc="duration_subweek : duration_daynum duration_d
                                                      | duration_daynum fraction duration_d
                                                      | duration_daynum duration_d duration_time
                                                      | duration_time", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_time=function(doc="duration_time : duration_t duration_hournum duration_h
                                                | duration_t duration_hournum fraction duration_h
                                                | duration_t duration_hournum duration_h duration_subhour
                                                | duration_t duration_subhour", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_subhour=function(doc="duration_subhour : duration_minutenum duration_m
                                                      | duration_minutenum fraction duration_m
                                                      | duration_minutenum duration_m duration_subminute
                                                      | duration_subminute", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_duration_subminute=function(doc="duration_subminute : duration_secondnum duration_s
                                                          | duration_secondnum fraction duration_s", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },

    ## Duration numbers ####
    p_duration_yearnum=function(doc="duration_yearnum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_yearnum=p$get(2)))
    },
    p_duration_monthnum=function(doc="duration_monthnum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_monthnum=p$get(2)))
    },
    p_duration_weeknum=function(doc="duration_weeknum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_yearnum=p$get(2)))
    },
    p_duration_daynum=function(doc="duration_daynum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_daynum=p$get(2)))
    },
    p_duration_hournum=function(doc="duration_hournum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_hournum=p$get(2)))
    },
    p_duration_minutenum=function(doc="duration_minutenum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_minutenum=p$get(2)))
    },
    p_duration_secondnum=function(doc="duration_secondnum : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration_secondnum=p$get(2)))
    },
    ## Duration single-character matches ####
    p_duration_p=function(doc="duration_p : 'P'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_y=function(doc="duration_y : 'Y'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_m=function(doc="duration_m : 'M'", p) {
      # this is used for both month and minute
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_w=function(doc="duration_w : 'W'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_d=function(doc="duration_d : 'D'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_t=function(doc="duration_t : 'T'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_h=function(doc="duration_h : 'H'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    },
    p_duration_s=function(doc="duration_s : 'S'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(duration=TRUE))
    }
  )

# Intervals ####

l_interval <-
  list(
    p_repeating_interval=function(doc="repeating_interval : repeating_interval_r repeating_interval_n solidus interval
                                                          | repeating_interval_r solidus interval", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_interval=function(doc="interval : date solidus interval_end_date
                                      | date solidus duration
                                      | duration solidus date
                                      | duration", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      if (p$length() == 4) {
        p$set(1, list(interval=TRUE, start=p$get(2), end=p$get(4)))
      } else {
        p$set(1, list(interval=TRUE, duration=p$get(2)))
      }
    },
    p_solidus=function(doc="solidus : '/'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(interval=TRUE))
    },
    p_interval_end_date=function(doc="interval_end_date : date
                                                        | subyear
                                                        | month
                                                        | mday
                                                        | week
                                                        | weekday
                                                        | oday
                                                        | time
                                                        | minute
                                                        | second", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, set_value(p))
    },
    p_repeating_interval_r=function(doc="repeating_interval_r : 'R'", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(repeating_interval=TRUE))
    },
    p_repeating_interval_n=function(doc="repeating_interval_n : multi_digit", p) {
      if (self$verbose) message(gsub(x=doc, pattern=" .*$", replacement=""))
      p$set(1, list(repeating_interval_n=p$get(2)))
    }
  )

# All interfaces combined ####

l_combined <-
  list(
    p_iso8601_any=function(doc="any : interval
                                    | repeating_interval
                                    | duration
                                    | date_or_time", p) {
      p$set(1, set_value(p))
    }
  )

# User interface ####

#' Create an ISO 8601 parser with options controlling which optional parts are included in the parser.
#' 
#' @param allow_leap_second Should leap seconds be allowed (61 seconds in a minute)?
#' @param allow_before_1583 Should years before 1583 be allowed?
#' @param verbose Show verbose messages while parsing?
#' @export
build_parser <- function(allow_leap_second=FALSE, allow_before_1583=TRUE, verbose=FALSE) {
  public_values <-
    c(
      l_combined,
      l_interval, l_duration,
      l_iso8601_datetime, l_specific_numbers,
      l_timezone, l_parser_general
    )
  public_values$allow_leap_second <- allow_leap_second
  public_values$verbose <- verbose
  Parser <-
    R6::R6Class(
      "ISO8601 Parser",
      public=public_values
    )
  rly::yacc(Parser)
}

default_parser <- build_parser()

#' Parse a single ISO8601 string
#' 
#' @param x The string to parse
#' @param parser The parser to use for parsing (as created by build_parser()).
#'   If not given, the default parser will be used.
#' @return A named list with the parsed parts 
#' @export
parse_ISO8601 <- function(x, parser) {
  if (missing(parser)) {
    parser <- default_parser
  }
  parser$parse(x, lexer)
}
