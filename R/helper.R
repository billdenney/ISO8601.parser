p_collapse <- function(x, p) {
  paste0(sapply(X=x, FUN=p$get), collapse="")
}

set_value <- function(p) {
  ret <- list()
  for (idx in (1 + seq_len(p$length() - 1))) {
    current <- p$get(idx)
    for (nm in names(current)) {
      if (nm %in% names(ret)) {
        if (ret[[nm]] != current[[nm]]) {
          print(ret)
          print(current)
          stop(sprintf("mismatch with %s: %s vs %s", nm, ret[[nm]], current[[nm]]))
        }
      } else {
        ret[[nm]] <- current[[nm]]
      }
    }
  }
  ret
}

value_between <- function(x, low, high) {
  n_x <- as.numeric(x)
  if (n_x < low) {
    stop(x, " is not above or equal to ", low)
  } else if (n_x > high) {
    stop(x, " is not below or equal to ", high)
  }
}
