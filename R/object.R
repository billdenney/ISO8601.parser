#' Create an ISO8601_parsed object
#' 
#' @param x a list from parse_ISO8601
#' @export
as_ISO8601_parsed <- function(x) {
  UseMethod("as_ISO8601_parsed")
}

as_ISO8601_parsed.list <- function(x) {
  class(x) <- "ISO8601_parsed"
  x
}

as_ISO8601_parsed.character <- function(x, parser, ...) {
  as_ISO8601_parsed(
    parse_ISO8601(x=x, parser=parser, ...)
  )
}
