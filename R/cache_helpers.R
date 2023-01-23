isUpdated <- function(x) {
  isTRUE(attr(x, ".Cache")[["newCache"]]) ||
    any(vapply(attr(x, ".Cache")[["changed"]], function(m) length(m[[".objects"]]) > 0, logical(1)))
}
