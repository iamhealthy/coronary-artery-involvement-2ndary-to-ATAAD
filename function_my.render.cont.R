my.render.cont <- function(x) {
  median_val <- median(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  sprintf("%.1f [%.1f, %.1f]", median_val, min_val, max_val)
}