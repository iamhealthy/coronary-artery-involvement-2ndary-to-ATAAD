for_FO_FP <- function(num1, num2) { # num1 and num2 must be 0, 1, or NA.
  if (all(is.na(c(num1, num2)))) { # num1 = NA, num2 = NA.
    return(NA)
  } else if (is.na(num1) && num2 == 1) { # num1 = NA, num2 = 1.
    return(NA)
  } else if (num1 == 1 && is.na(num2)) { # num1 = 1, num2 = NA.
    return(NA)
  } else if (num1 == 1 && num2 == 1) { # num1 = 1, num2 = 1.
    return(1)
  } else {
    return(0)
  }
}