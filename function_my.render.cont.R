my.render.cont_quantile <- function(x) {
  val_median <- quantile(x, na.rm = TRUE)[3]
  val_0.25 <- quantile(x, na.rm = TRUE)[2]
  val_0.75 <- quantile(x, na.rm = TRUE)[4]
  sprintf("%.2f [%.2f, %.2f]", val_median, val_0.25, val_0.75)
}