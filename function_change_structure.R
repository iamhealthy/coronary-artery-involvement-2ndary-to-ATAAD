change_structure <- function(df, df_num_factor) {
  for (flag in 1:nrow(df_num_factor)) {
    df_colname <- df_num_factor$old[flag]
    
    if (df_num_factor$num_or_fac[flag] == c("num")) {
      df[[df_colname]] <- as.numeric(df[[df_colname]])
    } else if (df_num_factor$num_or_fac[flag] == c("fac")) {
      df[[df_colname]] <- as.factor(df[[df_colname]])
    }
  }
  return(df)
}