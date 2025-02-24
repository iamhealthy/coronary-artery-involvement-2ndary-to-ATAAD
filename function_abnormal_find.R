# Set a function to find those abnormal items (i.e. "NA", "N/A", "na", "n/a", or " ").
abnormal_find <- function(df, abnormal_item) {
  print("-----")
  if (is.data.frame(df) == FALSE) {
    msg <- c("A data frame should be provided! Exiting function~")
    return(msg)
  }

  if (is.character(abnormal_item) == FALSE & length(abnormal_item) != 1) {
    msg <- c("A character with length 1 should be provided! Exiting function~")
    return(msg)
  }

  flag1 <- 0 # for flagging if data frame contains abnormal items.

  for (column in names(df)) { # Check all columns one by one.
    flag <- 0    
    
    for (row in 1:nrow(df)) { # Check all rows of a column on by one.
      value <- df[row, column] # Extract the value from the data frame.
      
      if (abnormal_item == c("NA")) {
        if (is.na(value) == TRUE) {
          flag <- 1          
        }
      } else {
        if (grepl(abnormal_item, value) == TRUE) {
          flag <- 1          
        }
      }
            
    }

    if (flag == 1) {
      flag1 <- 1
      msg <- paste0("The column of ", "'", column, "'", " contains ", "'", abnormal_item, "'.")
      print(msg)
    }

  }

  if (flag1 == 0) {
    msg <- paste0("This data frame doesn't contain ", "'", abnormal_item, "'.")
    print(msg)
    return("-----")
  } else {
    msg <- paste0("As shown above, this data frame contains ", "'", abnormal_item, "'.")    
    print(msg)
    return("-----")
  }

}