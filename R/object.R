#' Create an ISO8660_parsed object
#' 
#' @param x a list from parse_ISO8660
#' @export
as_ISO8660_parsed <- function(x) {
  UseMethod("as_ISO8660_parsed")
}

as_ISO8660_parsed.list <- function(x) {
  class(x) <- "ISO8660_parsed"
  x
}

as_ISO8660_parsed.character <- function(x, parser, ...) {
  as_ISO8660_parsed(
    parse_ISO8660(x=x, parser=parser, ...)
  )
}
