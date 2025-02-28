hos_to_dis <- function(t1_hos, t2_dis) { # t1_hos: the date of hospitalizaiton; t2_dis: the date of discharge.
  if (!is.na(t1_hos) && !is.na(t2_dis)) {
    days_off <- as.numeric(difftime(t1_hos, t2_dis, units = "days"))
    return(days_off)
  } else {
    return(NA)
  }
}