for_TND_PND <- function(num1, num2) { # num1 and num2 must be 0, 1, or NA.
  if (all(is.na(c(num1, num2)))) { # Both are NAs.
    return(NA)
  } else if (is.na(num1)) { # num1 is NA, num2 is numeric.
    
    if (num2 == 1) {
      return(1)
    } else if (num2 == 0) {
      return(NA)
    }

  } else if (is.na(num2)) { # num1 is numeric, num2 is NA.
    
    if (num1 == 1) {
      return(1)
    } else if (num1 == 0) {
      return(NA)
    }

  } else if (num1 == 1 || num2 == 1) { # num1 and num2 are numeric and either one == 1.
    return(1)
  } else {
    return(0)
  }
}