normalize_minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
