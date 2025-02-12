setwd("D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122")

# ======================================================

# 1. Load the package of 'readxl'.

library("readxl")

# Get the sheet names.

sheet_names <- excel_sheets("原始数据库.xlsx")
print(sheet_names)

# Read the sheets of "Injury group", "Intact group", "Control group".

injury_group <- read_excel("原始数据库.xlsx", sheet = "Injury group") # For duplicated column names, they will be suffixed with "...column number".
intact_group <- read_excel("原始数据库.xlsx", sheet = "Intact group") # For duplicated column names, they will be suffixed with "...column number".
control_group <- read_excel("原始数据库.xlsx", sheet = "Control group") # For duplicated column names, they will be suffixed with "...column number".

# ======================================================

# 2. Tag the values of column Echo_AI >= 2 being "1", and the ones < 2 being "0".

# ------------------------------------------------------

injury_group$Echo_AI # Display the column numbers.

injury_group$Echo_AI_tag <- injury_group$Echo_AI
injury_group$Echo_AI_tag[injury_group$Echo_AI_tag < 2] <- "0" # Tag "0" for values < 2.
injury_group$Echo_AI_tag[injury_group$Echo_AI_tag >= 2] <- "1" # Tag "1" for values >= 2.
injury_group$Echo_AI_tag

# ------------------------------------------------------

intact_group$Echo_AI # Display the column numbers.

intact_group$Echo_AI_tag <- intact_group$Echo_AI
intact_group$Echo_AI_tag[intact_group$Echo_AI_tag < 2] <- "0" # Tag "0" for values < 2.
intact_group$Echo_AI_tag[intact_group$Echo_AI_tag >= 2] <- "1" # Tag "1" for values >= 2.
intact_group$Echo_AI_tag

# ------------------------------------------------------

control_group$Echo_AI # Display the column numbers. And find there are "NA", and "N/A".

control_group$Echo_AI[control_group$Echo_AI == "N/A"] <- NA # Change "N/A" to "NA".

control_group$Echo_AI # Display once again.

mode(control_group$Echo_AI) # Display the mode (e.g. character, numeric).
control_group$Echo_AI <- as.numeric(control_group$Echo_AI) # Change from character to numeric.
mode(control_group$Echo_AI) # Display the mode once again. Should be numeric this time.

control_group$Echo_AI_tag <- control_group$Echo_AI
control_group$Echo_AI_tag[control_group$Echo_AI_tag < 2] <- "0" # Tag "0" for values < 2.
control_group$Echo_AI_tag[control_group$Echo_AI_tag >= 2] <- "1" # Tag "1" for values >= 2.
control_group$Echo_AI_tag

# ======================================================

# 3. Merging the columns of "循环衰竭", "MH_AHF", and "MH_心包填塞",
# since they are going to be displayed/analyzed together (choose the max number of the rows).
# The new column name is "hst" (aka Hypotension/shock/tamponade).

# ------------------------------------------------------

injury_group$循环衰竭 # Check if there are "NAs".
injury_group$MH_AHF # Check if there are "NAs".
injury_group$MH_心包填塞 # Check there are "NAs".
injury_group$hst <- pmax(injury_group$循环衰竭, injury_group$MH_AHF, injury_group$MH_心包填塞)
injury_group$hst

# ------------------------------------------------------

intact_group$循环衰竭 # Check if there are "NAs".
intact_group$MH_AHF # Check if there are "NAs".
intact_group$MH_心包填塞 # Check if there are "NAs".
intact_group$hst <- pmax(intact_group$循环衰竭, intact_group$MH_AHF, intact_group$MH_心包填塞)
intact_group$hst

# ------------------------------------------------------

control_group$循环衰竭 # Check if there are "NAs".
unique(control_group$循环衰竭) # Display the unique values.

control_group$MH_AHF # Check if there are "NAs".
unique(control_group$MH_AHF) # Display the unique values.

control_group$MH_心包填塞 # Check if there are "NAs".
unique(control_group$MH_心包填塞) # Display the unique values.

control_group$hst <- pmax(control_group$循环衰竭, control_group$MH_AHF, control_group$MH_心包填塞)
control_group$hst

View(control_group[,c("循环衰竭", "MH_AHF", "MH_心包填塞", "hst")]) # View and compare.

# ======================================================

# 4. Merging the columns of "AD_Coronary_LCA Neri" and "AD_Coronary_RCA Neri",
# and those cells being not equal to "0" will be set to "1".
# The new column name is "AD_Coronary_LRCA Neri".

# ------------------------------------------------------

injury_group$`AD_Coronary_LCA Neri` # Check if there are "NAs".
injury_group$`AD_Coronary_RCA Neri` # Check if there are "NAs".

injury_group$`AD_Coronary_LRCA Neri` <- pmax(injury_group$`AD_Coronary_LCA Neri`, injury_group$`AD_Coronary_RCA Neri`)

View(injury_group[,c("AD_Coronary_LCA Neri", "AD_Coronary_RCA Neri", "AD_Coronary_LRCA Neri")])

# ------------------------------------------------------

intact_group$`AD_Coronary_LCA Neri` # Check if there are "NAs". And find there are both "A" and "a", so change "a" to "A".
unique(intact_group$`AD_Coronary_LCA Neri`) # Display unique values.
intact_group$`AD_Coronary_LCA Neri`[intact_group$`AD_Coronary_LCA Neri` == "a"] <- "A" # Change "a" to "A".
unique(intact_group$`AD_Coronary_LCA Neri`) # Double check.

intact_group$`AD_Coronary_RCA Neri` # Check if there are "NAs". And find there are "N/A", "a", and "b".
unique(intact_group$`AD_Coronary_RCA Neri`) # Display unique values.
intact_group$`AD_Coronary_RCA Neri`[intact_group$`AD_Coronary_RCA Neri` == "a"] <- "A" # Change "a" to "A".
intact_group$`AD_Coronary_RCA Neri`[intact_group$`AD_Coronary_RCA Neri` == "b"] <- "B" # Change "b" to "B".
intact_group$`AD_Coronary_RCA Neri`[intact_group$`AD_Coronary_RCA Neri` == "N/A"] <- NA # Change "N/A" to "NA".
unique(intact_group$`AD_Coronary_RCA Neri`)

intact_group$`AD_Coronary_LRCA Neri` <- pmax(intact_group$`AD_Coronary_LCA Neri`, intact_group$`AD_Coronary_RCA Neri`)

# ------------------------------------------------------

control_group$`AD_Coronary_LCA Neri` # Check if there are "NAs". And find there are "NAs".
unique(control_group$`AD_Coronary_LCA Neri`) # Display unique values.

control_group$`AD_Coronary_RCA Neri` # Check if there are "NAs". And find there are "NAs".
unique(control_group$`AD_Coronary_RCA Neri`) # Display unique values.

control_group$`AD_Coronary_LRCA Neri` <- pmax(control_group$`AD_Coronary_LCA Neri`, control_group$`AD_Coronary_RCA Neri`)

View(control_group[,c("AD_Coronary_LCA Neri", "AD_Coronary_RCA Neri", "AD_Coronary_LRCA Neri")])

# ======================================================

# 5. Extract the columns relevant to Table 1 (5 items per row for clarity).

table1_colnames <- c(
  "Age", "Male", "BMI", "起病至手术", "Op_Emergency",
  "PMH_HBP", "PMH_DM", "PMH_CAD", "PMH_COPD", "PMH_CKD",
  "PMH_Stroke", "Dx_BAV", "Echo_LVEF", "Echo_AI_tag", "Echo_AS",
  "MH_Ventilation", "hst", "MH_Ischemia_Coronary", "MH_Ischemia_Cerebral", "MH_Ischemia_Spinal",
  "MH_Ischemia_Mesenteric", "MH_Ischemia_Lower Extremity", "AD_Coronary Involvement", "AD_Coronary_LCA Neri", "AD_Coronary_RCA Neri",
  "AD_Coronary_LRCA Neri"
)

table1_colnames_new <- c(
  "Age, y", "Male sex", "BMI, kg/m^2", "Time to surgery, h", "Emergency surgery",
  "Hypertension", "Diabetes mellitus", "Coronary artery disease", "Chronic lung disease", "Chronic kidney disease",
  "Previous stroke", "Bicuspid aortic valve", "LVEF, %", "Moderate-to-severe AR", "Aortic stenosis",
  "Preoperative ventilation", "Hypotension/shock/tamponade", "Malperfusion_Coronary", "Malperfusion_Cerebral", "Malperfusion_Spinal",
  "Malperfusion_Visceral", "Malperfusion_Lower limbs", "CA involvement", "CA involvement_Left CA only", "CA involvement_Right CA only",
  "CA involvement_Left+right CA"
)

table1_names <- data.frame(
  old = table1_colnames,
  new = table1_colnames_new
)

table1_names # Do check the correctness of the matched rows.

# ======================================================

# 6. Add one more column to table1_names marking if they are numeric or factor.

num_or_factor <- c(
  "num", "fac", "num", "num", "fac",
  "fac", "fac", "fac", "fac", "fac",
  "fac", "fac", "num", "fac", "fac",
  "fac", "fac", "fac", "fac", "fac",
  "fac", "fac", "fac", "fac", "fac",
  "fac"
)

table1_names$num_or_fac <- num_or_factor
View(table1_names)

# ======================================================

# 7. Extract the columns based on table1_names$old

injury_group_table1 <- injury_group[, table1_names$old]
intact_group_table1 <- intact_group[, table1_names$old]
control_group_table1 <- control_group[, table1_names$old]

dim(injury_group_table1) # Show the dimention, and should be 26 columns.
dim(intact_group_table1) # Show the dimention, and should be 26 columns.
dim(control_group_table1) # Show the dimention, and should be 26 columns.

names(injury_group_table1) # Show the column names.
names(injury_group_table1) == names(intact_group_table1) # Compare column names between "injury_group_tables" and "intact_group_table1".  Should be all TRUE. 
names(injury_group_table1) == names(control_group_table1) # Compare column names between "injury_group_tables" and "control_group_table1". Should be all TRUE.

names(injury_group_table1) == table1_names$old # Compare column names between "injury_group_tables" and "table1_names$old". Should be all TRUE.

# ======================================================

# 8. See if there are any "NA", "N/A", "na", "n/a" or " " within table.

# ------------------------------------------------------

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

# ------------------------------------------------------

abnormal_find(injury_group_table1, "NA") # No matter contains or not, leave it alone.

abnormal_find(injury_group_table1, "N/A") # Once contains, change to NA.
injury_group_table1$Echo_LVEF[injury_group_table1$Echo_LVEF == c("N/A")] <- NA
injury_group_table1$Echo_AS[injury_group_table1$Echo_AS == c("N/A")] <- NA
abnormal_find(injury_group_table1, "N/A")

abnormal_find(injury_group_table1, "na")
abnormal_find(injury_group_table1, "n/a")
abnormal_find(injury_group_table1, " ")

View(injury_group_table1)

# ------------------------------------------------------

abnormal_find(intact_group_table1, "NA") # No matter contains or not, leave it alone.

abnormal_find(intact_group_table1, "N/A") # Once contains, change to NA.
intact_group_table1$Echo_LVEF[intact_group_table1$Echo_LVEF == c("N/A")] <- NA
intact_group_table1$Echo_AS[intact_group_table1$Echo_AS == c("N/A")] <- NA
abnormal_find(intact_group_table1, "N/A")

abnormal_find(intact_group_table1, "na")
abnormal_find(intact_group_table1, "n/a")
abnormal_find(intact_group_table1, " ")

View(intact_group_table1)
# Notice "发病时合并脑梗" in the column of "PMH_Stroke".
intact_group_table1$PMH_Stroke[intact_group_table1$PMH_Stroke == c("发病时合并脑梗")] <- c(1)
View(intact_group_table1)

# ------------------------------------------------------

abnormal_find(control_group_table1, "NA") # No matter contains or not, leave it alone.

abnormal_find(control_group_table1, "N/A") # Once contains, change to NA.
control_group_table1$Echo_LVEF[control_group_table1$Echo_LVEF == c("N/A")] <- NA
control_group_table1$Echo_AS[control_group_table1$Echo_AS == c("N/A")] <- NA
abnormal_find(control_group_table1, "N/A")

abnormal_find(control_group_table1, "na")
abnormal_find(control_group_table1, "n/a")
abnormal_find(control_group_table1, " ")

View(control_group_table1)
# Notice "1，术前长期透析" in the column of "PMH_CKD",
# and "合并急性脑梗死" in the column of "PMH_Stroke",
# and "-" in the column of "起病至手术",
# and "3" and "21" in the column of "Echo_AS".
control_group_table1$PMH_CKD[control_group_table1$PMH_CKD == c("1，术前长期透析")] <- c(1)
control_group_table1$PMH_Stroke[control_group_table1$PMH_Stroke == c("合并急性脑梗死")] <- c(1)
control_group_table1$起病至手术[control_group_table1$起病至手术 == c("-")] <- c(NA)
control_group_table1$Echo_AS[control_group_table1$Echo_AS == c(3)] <- 1
control_group_table1$Echo_AS[control_group_table1$Echo_AS == c(21)] <- 1
View(control_group_table1)

# ======================================================

# 9. Check the data structure.

# Set a function to change data structure (i.e. numeric or factor).
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

str(injury_group_table1) # Check the internal structure of objects.
injury_group_table1 <- change_structure(injury_group_table1, table1_names)
str(injury_group_table1) # Check the internal structure once again.

str(intact_group_table1) # Check the internal structure of objects.
intact_group_table1 <- change_structure(intact_group_table1, table1_names)
str(intact_group_table1) # Check the internal structure once again.

str(control_group_table1) # Check the internal structure of objects.
control_group_table1 <- change_structure(control_group_table1, table1_names)
str(control_group_table1) # Check the internal structure once again.

# ======================================================

# 10. Generate table 1.

# ------------------------------------------------------

# Add a column for group identification.
injury_group_table1$group <- c("Injury_group")
intact_group_table1$group <- c("Intact_group")
control_group_table1$group <- c("Control_group")

# ------------------------------------------------------

# Combine the three tables.
combined_data <- rbind(injury_group_table1, intact_group_table1, control_group_table1)

# Compare the colnames between combined_data and table1_names$old
names(combined_data) == table1_names$old # The 1st 26 items are the same (one is 27 in length, the other one is 26 in length).
length(names(combined_data)) # 27
names(combined_data)
names(combined_data)[1:26] <- table1_names$new # Rename the column names.
names(combined_data)

# ------------------------------------------------------

# Change the sequence of levels of some columns.
combined_data$`CA involvement_Left CA only` <- factor(combined_data$`CA involvement_Left CA only`, levels = c("0", "A", "B", "C"))
combined_data$`CA involvement_Right CA only` <- factor(combined_data$`CA involvement_Right CA only`, levels = c("0", "A", "B", "C"))
combined_data$`CA involvement_Left+right CA` <- factor(combined_data$`CA involvement_Left+right CA`, levels = c("0", "A", "B", "C"))

# Generate the comparative table using the package of table1.

library(table1)

# Show table with just Median [Min, Max]
my.render.cont <- function(x) {
  median_val <- median(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  sprintf("%.1f [%.1f, %.1f]", median_val, min_val, max_val)
}

# Generate table.
combined_data_table <- table1::table1(~ . - group | group, data = combined_data, render.continuous = my.render.cont)

# Save the table as csv file.
write.csv(combined_data_table, "1_table1.csv", row.names = F)

# Save the table as html file.
library(htmlTable)
htmlTable(combined_data_table) |> writeLines("1_table1.html")

# ------------------------------------------------------

# Calculate p-value.
col_name <- names(combined_data)[1:(length(names(combined_data)) - 1)] # Store the column names without "group" name (last one).
col_name_p <- rep(1, length(col_name)) # For storing the calculated p-value.

for (i1 in 1:(length(col_name) - 1)) { # Loop to calculate p-value.  
  
  if (is.numeric(combined_data[[col_name[i1]]])) {    
    p_value <- kruskal.test(combined_data[[col_name[i1]]] ~ combined_data$group)$p.value # Kruskal-Wallis test for numeric.
    col_name_p[i1] <- sprintf("%.3f", p_value) # Display only two decimal places.
  } else if (is.factor(combined_data[[col_name[i1]]])) {    
    p_value <- chisq.test(table(combined_data[[col_name[i1]]], combined_data$group), simulate.p.value = TRUE)$p.value # chi-square test for categorical vector.
    col_name_p[i1] <- sprintf("%.3f", p_value) # Display only two decimal places.
  }

}

combined_data_p_value <- data.frame(
  columns = col_name,
  'p-value' = col_name_p
)

# Save the p-value as csv file.
write.csv(combined_data_p_value, "1_table1_p_value.csv", row.names =F)

# ======================================================

# 11. Save the current R session.

save.image("1_read_the_three_sheets_containing_clinical_info.Rdata")

# ======================================================