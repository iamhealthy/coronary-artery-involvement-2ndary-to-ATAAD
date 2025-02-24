# Split the string with "+" and sum the numbers.
splitAndSum <- function(x) {
  if (grepl("\\+", x)) {
    x <- as.numeric(strsplit(x, "\\+")[[1]])
    sum(x)
  } else {
    as.numeric(x)
  }  
}