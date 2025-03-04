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

# 2. Prepare the column for "Rescue CABG during surgery".

# Control组的戴荣兰和Intact 组的陆卫兴。

injury_group$`Rescue CABG during surgery` <- 0

grep("陆卫兴", intact_group$Name) # Appears at row 126.
intact_group$`Rescue CABG during surgery` <- 0
intact_group$`Rescue CABG during surgery`[grep("陆卫兴", intact_group$Name)] <- 1 # Set the row of "陆卫兴" to "1".

grep("戴荣兰", control_group$Name) # Appears at row 324.
control_group$`Rescue CABG during surgery` <- 0
control_group$`Rescue CABG during surgery`[grep("戴荣兰", control_group$Name)] <- 1 # Set the row of "戴荣兰" to "1".

# ======================================================

# 3. Prepare the column for "Surgical death".

# GZ=1的人数，不包括自动出院。

mode(injury_group$`死亡1，自动出院2`) # It is a "numeric" type.
unique(injury_group$`死亡1，自动出院2`) # There are not NAs.
injury_group$surgical_death <- ifelse(injury_group$`死亡1，自动出院2` == 1, 1, 0)
View(injury_group[,c("死亡1，自动出院2", "surgical_death")])

mode(intact_group$`死亡1，自动出院2`) # It is a "character" type.
intact_group$`死亡1，自动出院2` <- as.numeric(intact_group$`死亡1，自动出院2`) # Change from character to numeric.
unique(intact_group$`死亡1，自动出院2`) # There are NAs.
intact_group$surgical_death <- ifelse(intact_group$`死亡1，自动出院2` == 1, 1, 0)
View(intact_group[,c("死亡1，自动出院2", "surgical_death")])

mode(control_group$`死亡1，自动出院2`) # It is a "character" type.
control_group$`死亡1，自动出院2` <- as.numeric(control_group$`死亡1，自动出院2`) # Change from character to numeric. 
unique(control_group$`死亡1，自动出院2`) # There are NAs.
control_group$surgical_death <- ifelse(control_group$`死亡1，自动出院2` == 1, 1, ifelse(is.na(control_group$`死亡1，自动出院2`), NA, 0))
View(control_group[,c("死亡1，自动出院2", "surgical_death")])

# ======================================================

# 4. Prepare the column for "New-onset neurological deficit".

# Create a function (i.e. for_TND_PND) to achieve the task below.
# See "D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_for_TND_PND.R" for detail.
source("function_for_TND_PND.R")

# TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）或
# PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）任意一个为1，两者相加。
# 即FT或FU任意一个为1

mode(injury_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`) # Is is a "numeric" type.
unique(injury_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`) # Must be 0, 1, or NA.
mode(injury_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`) # It is a "numeric" type.
unique(injury_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`) # Must be 0, 1, or NA.
injury_group$new_onset_neuro_defi <- mapply(for_TND_PND,
  injury_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`,
  injury_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`)
View(injury_group[, c("PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）", "PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）", "new_onset_neuro_defi")])

mode(intact_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`) # Is is a "numeric" type.
unique(intact_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`) # Must be 0, 1, or NA.
mode(intact_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`) # It is a "numeric" type.
unique(intact_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`) # Must be 0, 1, or NA.
intact_group$new_onset_neuro_defi <- mapply(for_TND_PND,
  intact_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`,
  intact_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`)
View(intact_group[, c("PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）", "PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）", "new_onset_neuro_defi")])

mode(control_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`) # Is is a "numeric" type.
unique(control_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`) # Must be 0, 1, or NA.
mode(control_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`) # It is a "numeric" type.
unique(control_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`) # Must be 0, 1, or NA.
control_group$new_onset_neuro_defi <- mapply(for_TND_PND,
  control_group$`PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）`,
  control_group$`PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）`)
View(control_group[, c("PtO_TND（存在谵妄、激越、迟钝、术后意识模糊或没有任何神经系统体征的短暂性帕金森病）", "PtO_CNS_PND（存在局灶性或全身性的永久性神经功能缺损，并在出院时持续存在）", "new_onset_neuro_defi")])

# ======================================================

# 5. Prepare the column for "AKI requiring hemodialysis".

# Create a function (i.e. for_FO_FP) to achieve the task below.
# See "D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_for_FO_FP_table3.R" for detail.
source("function_for_FO_FP_table3.R")

# （FO=1且FP=1）。

mode(injury_group$PtO_AKI) # It is numeric.
unique(injury_group$PtO_AKI) # Must be 0, 1, or NA.
mode(injury_group$PtO_hemodialysis) # It is numeric.
unique(injury_group$PtO_hemodialysis) # Must be 0, 1, or NA.
injury_group$aki_req_hemo <- mapply(for_FO_FP, injury_group$PtO_AKI, injury_group$PtO_hemodialysis)
View(injury_group[, c("PtO_AKI", "PtO_hemodialysis", "aki_req_hemo")])

mode(intact_group$PtO_AKI) # It is numeric.
unique(intact_group$PtO_AKI) # Must be 0, 1, or NA.
mode(intact_group$PtO_hemodialysis) # It is numeric.
unique(intact_group$PtO_hemodialysis) # Must be 0, 1, or NA.
intact_group$aki_req_hemo <- mapply(for_FO_FP, intact_group$PtO_AKI, intact_group$PtO_hemodialysis)
View(intact_group[, c("PtO_AKI", "PtO_hemodialysis", "aki_req_hemo")])

mode(control_group$PtO_AKI) # It is numeric.
unique(control_group$PtO_AKI) # Must be 0, 1, or NA.
mode(control_group$PtO_hemodialysis) # It is numeric.
unique(control_group$PtO_hemodialysis) # Must be 0, 1, or NA.
control_group$aki_req_hemo <- mapply(for_FO_FP, control_group$PtO_AKI, control_group$PtO_hemodialysis)
View(control_group[, c("PtO_AKI", "PtO_hemodialysis", "aki_req_hemo")])

# ======================================================

# 6. Prepare the column for "Hospital stay, days".

# Create a function (i.e. hos_to_dis) to calculate the days from hospitalization to discharge.
# See "D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_hospitalization_to_discharge_table3.R" for detail.
source("function_hospitalization_to_discharge_table3.R")

# 入院至出院时间。

names(injury_group)[grep("Date_入院", names(injury_group))] # There are two columns.
names(injury_group)[grep("Date_出院", names(injury_group))] # There are two columns.
View(injury_group[, c("Date_入院...37", "Date_入院...204", "Date_出院...38", "Date_出院...205")]) # They are all "POSIXct" type.
# After viewing, choose the columns, i.e. "Date_入院...37" and "Date_出院...38", arbitrarily.
injury_group$stay_days <- mapply(hos_to_dis, injury_group$Date_出院...38, injury_group$Date_入院...37)
View(injury_group[, c("Date_入院...37", "Date_出院...38", "stay_days")])

names(intact_group)[grep("Date_入院", names(intact_group))] # There are two columns.
names(intact_group)[grep("Date_出院", names(intact_group))] # There are two columns.
View(intact_group[, c("Date_入院...37", "Date_入院...204", "Date_出院...38", "Date_出院...205")]) # They are all "POSIXct" type.
# After viewing, choose the columns, i.e. "Date_入院...37" and "Date_出院...38", arbitrarily.
intact_group$stay_days <- mapply(hos_to_dis, intact_group$Date_出院...38, intact_group$Date_入院...37)
View(intact_group[, c("Date_入院...37", "Date_出院...38", "stay_days")])

names(control_group)[grep("Date_入院", names(control_group))] # There are two columns.
names(control_group)[grep("Date_出院", names(control_group))] # There are two columns.
View(control_group[, c("Date_入院...37", "Date_入院...204", "Date_出院...38", "Date_出院...205")]) # They are all "POSIXct" type.
# After viewing, choose the right columns, i.e. "Date_入院...37" and "Date_出院...38".
control_group$stay_days <- mapply(hos_to_dis, control_group$Date_出院...38, control_group$Date_入院...37)
View(control_group[, c("Date_入院...37", "Date_出院...38", "stay_days")])

# ======================================================

# 7. Retrieve the columns relevant to Table 3 (5 items per row for clarity).

table3_colnames <- c(
  "Rescue CABG during surgery", "surgical_death", "PtO_LCOS", "PtO_ECMO", "new_onset_neuro_defi",
  "PtO_Prolonged Ventilation", "aki_req_hemo", "PtO_Paraplegia", "PtO_ReOp for Bleeding", "PtO_deep sternal wound infection",
  "ICU 时间", "stay_days"
)

table3_colnames_new <- c(
  "Rescue CABG during surgery", "Surgical death", "Low cardiac output syndrome", "ECMO support", "New-onset neurological deficit",
  "Prolonged ventilation", "AKI requiring hemodialysis", "Paraplegia", "Reoperation for bleeding", "Deep sternal wound infection",
  "ICU stay, days", "Hospital stay, days"
)

table3_names <- data.frame(
  old = table3_colnames,
  new = table3_colnames_new
)
  
View(table3_names) # Do check the correctness of the matched rows.

# ======================================================

# 8. Add one more column to table3_names marking if they are numeric or factor.

num_or_factor <- c(
  "fac", "fac", "fac", "fac", "fac",
  "fac", "fac", "fac", "fac", "fac",
  "num", "num"
)

table3_names$num_or_fac <- num_or_factor
View(table3_names)

# ======================================================

# 9. Extract the columns based on table3_names$old.

injury_group_table3 <- injury_group[, table3_names$old]
intact_group_table3 <- intact_group[, table3_names$old]
control_group_table3 <- control_group[, table3_names$old]

dim(injury_group_table3) # Should be 12 columns.
dim(intact_group_table3) # Should be 12 columns.
dim(control_group_table3) # Should be 12 columns.

names(injury_group_table3) # Show the column names.
names(injury_group_table3) == names(intact_group_table3) # Compare the column names between the two dataframes. Should be all TRUE.
all(names(injury_group_table3) == names(control_group_table3)) # Compare the column names between the two dataframes. Should be TRUE.

# ======================================================

# 10. See if there are any "NA", "N/A", "na", "n/a" or " " within table.

# ------------------------------------------------------

source("function_abnormal_find.R") # Load the function which could find abnormal items.

abnormal_find(injury_group_table3, "NA") # No matter contains or not, leave it alone.
abnormal_find(injury_group_table3, "N/A") # Once contains, change to NA.
abnormal_find(injury_group_table3, "na") # Once contains, change to NA.
abnormal_find(injury_group_table3, "n/a") # Once contains, change to NA.
abnormal_find(injury_group_table3, " ") # Once contains, change to NA.

abnormal_find(intact_group_table3, "NA") # No matter contains or not, leave it alone.
abnormal_find(intact_group_table3, "N/A") # Once contains, change to NA.
abnormal_find(intact_group_table3, "na") # Once contains, change to NA.
abnormal_find(intact_group_table3, "n/a") # Once contains, change to NA.
abnormal_find(intact_group_table3, " ") # Once contains, change to NA.

abnormal_find(control_group_table3, "NA") # No matter contains or not, leave it alone.
abnormal_find(control_group_table3, "N/A") # Once contains, change to NA.
abnormal_find(control_group_table3, "na") # Once contains, change to NA.
abnormal_find(control_group_table3, "n/a") # Once contains, change to NA.
abnormal_find(control_group_table3, " ") # Once contains, change to NA.

# ======================================================

# 11. Check the data structure.

# ------------------------------------------------------

# Create a function to change data structure (i.e. numeric or factor).
# See "D:\R&S\1_2_项目或任务\1_邵医生数据\2_20250122\function_change_structure.R" for details.
source("function_change_structure.R")

# ------------------------------------------------------

str(injury_group_table3) # Check the internal structure of objects.
injury_group_table3 <- change_structure(injury_group_table3, table3_names)
str(injury_group_table3) # Check the internal structure once again.

str(intact_group_table3) # Check the internal structure of objects. 
intact_group_table3 <- change_structure(intact_group_table3, table3_names)
str(intact_group_table3) # Check the internal structure once again. Noticed sth abnormal at the column "PtO_Paraplegia".
View(intact_group_table3)
intact_group_table3$PtO_Paraplegia[124]
intact_group_table3$PtO_Paraplegia[124] <- c(1) # Change to 1.
intact_group_table3$PtO_Paraplegia[124]
intact_group_table3$PtO_Paraplegia[142]
intact_group_table3$PtO_Paraplegia[142] <- c(0) # Change to 0.
intact_group_table3$PtO_Paraplegia[142]
str(intact_group_table3)
intact_group_table3$PtO_Paraplegia <- factor(intact_group_table3$PtO_Paraplegia, levels = c(0, 1)) # Change levels to 0, 1.
str(intact_group_table3)

str(control_group_table3) # Check the internal structure of objects.
control_group_table3 <- change_structure(control_group_table3, table3_names)
str(control_group_table3) # Check the internal structure once again.

# ======================================================

# 12. Generate table 3.

# ------------------------------------------------------

# Add a column for group identification.
injury_group_table3$group <- rep("Injury_group", nrow(injury_group_table3))
intact_group_table3$group <- rep("Intact_group", nrow(intact_group_table3))
control_group_table3$group <- rep("Control_group", nrow(control_group_table3))

# ------------------------------------------------------

# Combine the three tables.
combined_data_table3 <- rbind(injury_group_table3, intact_group_table3, control_group_table3)

# Compare the colnames between combined_data_table2 and table2_names$old
names(combined_data_table3) == table3_names$old # The 1st 12 items are the same (one is 13 in length, the other one is 12 in length).
length(names(combined_data_table3)) # 13
names(combined_data_table3)
names(combined_data_table3)[1:12] <- table3_names$new # Rename the column names.
names(combined_data_table3)

# ------------------------------------------------------

# Test if numerical variable is normally-distributed or not using shapiro.test().

str(combined_data_table3) # Check the internal structure of objects.
shapiro.test(combined_data_table3$`ICU stay, days`) # p < 0.05, not normally-distributed.
shapiro.test(combined_data_table3$`Hospital stay, days`) # p < 0.05, not normally-distributed.

# ------------------------------------------------------

# Needn't change the sequence of levels of some columns.

# Generate the comparative table using the package of table1.

library(table1)

# Create a function to show table with just 'quantile_0.5 [quantile_0.25, quantile_0.75]'.
# See "D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_my.render.cont_quantile.R" for details.
source("D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_my.render.cont_quantile.R")

# Generate table.
combined_data_table3_TABLE <- table1::table1(~ . - group | group, data = combined_data_table3, render.continuous = my.render.cont_quantile)
combined_data_table3_TABLE # Noticed some bizarra values at the column "ICU stay, days" of control_group_table3 and intact_group_table3.

combined_data_table3$`ICU stay, days`[combined_data_table3$`ICU stay, days` < 0] <- NA # Those values less than 0 are changed to NAs.

combined_data_table3_TABLE <- table1::table1(~ . - group | group, data = combined_data_table3, render.continuous = my.render.cont_quantile)
combined_data_table3_TABLE # Normal this time.

# Save the table as csv file.
write.csv(combined_data_table3_TABLE, "3_table3.csv", row.names = F)

# Save the table as html file.
library(htmlTable)
htmlTable(combined_data_table3_TABLE) |> writeLines("3_table3.html")

# ------------------------------------------------------

# Calculate p-value.
col_name <- names(combined_data_table3)[1:(length(names(combined_data_table3)) - 1)] # Store the column names without "group" name (last one).
col_name_p <- rep(1, length(col_name)) # For storing the calculated p-value.

for (i1 in 1:(length(col_name))) { # Loop to calculate p-value.  
  
  if (is.numeric(combined_data_table3[[col_name[i1]]])) {    
    p_value <- kruskal.test(combined_data_table3[[col_name[i1]]] ~ combined_data_table3$group)$p.value # Kruskal-Wallis test for numeric.
    col_name_p[i1] <- sprintf("%.3f", p_value) # Display only two decimal places.
  } else if (is.factor(combined_data_table3[[col_name[i1]]])) {    
    p_value <- chisq.test(table(combined_data_table3[[col_name[i1]]], combined_data_table3$group), simulate.p.value = TRUE)$p.value # chi-square test for categorical vector.
    col_name_p[i1] <- sprintf("%.3f", p_value) # Display only two decimal places.
  }
  
}

combined_data_table3_p_value <- data.frame(
  columns = col_name,
  'p-value' = col_name_p
)

View(combined_data_table3_p_value)


# Save the table as csv file.
write.csv(combined_data_table3_p_value, "3_table3_p_value.csv", row.names = F)

# ======================================================

# 13. Save the table 3.

injury_group_table3_patient <- cbind(injury_group[, c("编号", "His_ID", "Name")], injury_group_table3) # Add three more columns to identify the patients.
names(injury_group_table3_patient) <- c("编号", "His_ID", "Name", table3_names$new) # Rename the column names.

intact_group_table3_patient <- cbind(intact_group[, c("编号", "His_ID", "Name")], intact_group_table3) # Add three more columns to identify the patients.
names(intact_group_table3_patient) <- c("编号", "His_ID", "Name", table3_names$new) # Rename the column names.

control_group_table3_patient <- cbind(control_group[, c("编号", "His_ID", "Name")], control_group_table3) # Add three more columns to identify the patients.
names(control_group_table3_patient) <- c("编号", "His_ID", "Name", table3_names$new) # Rename the column names.

combined_data_table3_patient <- rbind(injury_group_table3_patient, intact_group_table3_patient, control_group_table3_patient) # Combine the three tables.

# Save the table as csv file.
write.csv(combined_data_table3_patient, "3_table3_patient.csv", row.names = F)

# ======================================================

# 14. Save the current R session.

save.image("3_read_the_three_sheets_containing_clinical_info_for_table3.Rdata")

# ======================================================