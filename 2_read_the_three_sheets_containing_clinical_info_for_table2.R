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

# 2. Prepare the column for "Supracoronary AAR". 

# 看Op-手术名称，从中筛选。

# Supracoronary AAR

any(is.na(injury_group$Op_手术名称)) # No NAs.
injury_group$supracoronory_aar <- 0 # Injury group的所有病人都不是，0。

any(is.na(intact_group$Op_手术名称)) # No NAs.
intact_group$supracoronory_aar <- c(rep(1, 146), 0, rep(1, (nrow(intact_group) - 147))) # Intact group中除了147号戴克贤，其余都是。
View(intact_group[,c("编号", "Name", "Op_手术名称", "supracoronory_aar")]) # To view the result.

any(is.na(control_group$Op_手术名称)) # There are NAs.
control_group$supracoronory_aar <- c(rep(1, 226), 0, rep(1, (nrow(control_group) - 227))) # Control group中除了227号 朱钰婷外，其余都是。
# If control_group$Op_手术名称 is NA, then control_group$supracoronory_aar is NA as well.
control_group$supracoronory_aar[is.na(control_group$Op_手术名称)] <- NA
View(control_group[,c("编号", "Name", "Op_手术名称", "supracoronory_aar")]) # To view the result.

# ======================================================

# 3. Prepare the column for "Isolated AVR".

# 看Op-手术名称，从中筛选。

# Isolated AVR。

# 仅Control group中的227号 朱钰婷 1例。

any(is.na(injury_group$Op_手术名称)) # No NAs.
injury_group$isolated_avr <- 0

any(is.na(intact_group$Op_手术名称)) # No NAs.
intact_group$isolated_avr <- 0

any(is.na(control_group$Op_手术名称)) # There are NAs.
control_group$isolated_avr <- c(rep(0, 226), 1, rep(0, (nrow(control_group) - 227))) # Control group中除了227号 朱钰婷外，其余都是。
# If control_group$Op_手术名称 is NA, then control_group$isolated_avr is NA as well.
control_group$isolated_avr[is.na(control_group$Op_手术名称)] <- NA
View(control_group[,c("编号", "Name", "Op_手术名称", "isolated_avr")]) # To view the result.

# ======================================================

# 4. Prepare the column for "Single sinus replacement".

# 看Op-手术名称，从中筛选。

# Single sinus replacement.

# 仅有Intact group中147号戴克贤 1例。

any(is.na(injury_group$Op_手术名称)) # No NAs.
injury_group$single_sinus_replacement <- 0

any(is.na(intact_group$Op_手术名称)) # There are NAs.
intact_group$single_sinus_replacement <- c(rep(0, 146), 1, rep(0, (nrow(intact_group) - 147)))

any(is.na(control_group$Op_手术名称)) # There are NAs.
control_group$single_sinus_replacement <- 0
# If control_group$Op_手术名称 is NA, then control_group$single_sinus_replacement is NA as well.
control_group$single_sinus_replacement[is.na(control_group$Op_手术名称)] <- NA
View(control_group[,c("编号", "Name", "Op_手术名称", "single_sinus_replacement")]) # To view the result.


# ======================================================

# 5. merging the columns of CL and CM for "TAR_with_FET".

# ------------------------------------------------------

mode(injury_group$Op_TAR) # Check the mode of CL column. The result is "numeric".
mode(intact_group$Op_TAR) # Check the mode of CL column. The result is "numeric".
mode(control_group$Op_TAR) # Check the mode of CL column. The result is "numeric".

mode(injury_group$Op_FET) # Check the mode of CM column. The result is "numeric".
mode(intact_group$Op_FET) # Check the mode of CM column. The result is "numeric".
mode(control_group$Op_FET) # Check the mode of CM column. The result is "numeric".

# ------------------------------------------------------

# Create a function to merge the columns of CL and CM with the criteira:
# if the cells of CL is 1, and the cells of CM is 1, then the cells of "TAR-with-FET" is 1.
# if the cells of CL is 1, and the cells of CM is 0, then the cells of "TAR-with-FET" is 0.
# other conditions, the cells of "TAR-with-FET" is "NA".

unique(injury_group$Op_TAR) # Display unique values.
unique(injury_group$Op_FET) # Display unique values.

unique(intact_group$Op_TAR) # Display unique values.
unique(intact_group$Op_FET) # Display unique values.

unique(control_group$Op_TAR) # Display unique values. There are NAs.
unique(control_group$Op_FET) # Display unique values. There are NAs.

tar_with_fet <- function(cl, cm) {
    if (!is.na(cl) && !is.na(cm) && cl == 1 && cm == 1) {
        return(1)
    } else if (!is.na(cl) && !is.na(cm) && cl == 1 && cm == 0) {
        return(0)
    } else if (!is.na(cl) && !is.na(cm) && cl == 0) {
        return(2) # indicates not TAR
    }else {
        return(NA)
    }
}

injury_group$TAR_with_FET <- mapply(tar_with_fet, injury_group$Op_TAR, injury_group$Op_FET)
intact_group$TAR_with_FET <- mapply(tar_with_fet, intact_group$Op_TAR, intact_group$Op_FET)
control_group$TAR_with_FET <- mapply(tar_with_fet, control_group$Op_TAR, control_group$Op_FET)

View(injury_group[,c("Op_TAR", "Op_FET", "TAR_with_FET")])
View(intact_group[,c("Op_TAR", "Op_FET", "TAR_with_FET")])
View(control_group[,c("Op_TAR", "Op_FET", "TAR_with_FET")])

# ------------------------------------------------------

# ======================================================

# 6. Merge the columns of CL and CL for "TAR without FET".

# Create a function to merge the columns of CL and CL with the criteira:
# if the cells of CL is 1, and the cells of CL is 0, then the cells of "TAR-without-FET" is 1.
# if the cells of CL is 1, and the cells of CL is 1, then the cells of "TAR-without-FET" is 0.
# other conditions, the cells of "TAR-without-FET" is "NA".

tar_without_fet <- function(cl, cm) {
    if (!is.na(cl) && !is.na(cm) && cl == 1 && cm == 0) {
        return(1)
    } else if (!is.na(cl) && !is.na(cm) && cl == 1 && cm == 1) {
        return(0)
    } else if (!is.na(cl) && !is.na(cm) && cl == 0) {
        return(2) # indicates not TAR
    } else {
        return(NA)
    }
}

injury_group$TAR_without_FET <- mapply(tar_without_fet, injury_group$Op_TAR, injury_group$Op_FET)
intact_group$TAR_without_FET <- mapply(tar_without_fet, intact_group$Op_TAR, intact_group$Op_FET)
control_group$TAR_without_FET <- mapply(tar_without_fet, control_group$Op_TAR, control_group$Op_FET)

View(injury_group[,c("Op_TAR", "Op_FET", "TAR_without_FET")])
View(intact_group[,c("Op_TAR", "Op_FET", "TAR_without_FET")])
View(control_group[,c("Op_TAR", "Op_FET", "TAR_without_FET")])

# ======================================================

# 7. Prepare the column of "Associated MV/TC procedures".

# 看Op-手术名称，从中筛选。

# Associated MV/TC procedures.

# 仅Control group中的38号 朱尔成 1例。

any(is.na(injury_group$Op_手术名称)) # No NAs.
injury_group$associated_mv_tv_procedures <- 0

any(is.na(intact_group$Op_手术名称)) # No NAs.
intact_group$associated_mv_tv_procedures <- 0

any(is.na(control_group$Op_手术名称)) # There are NAs.
control_group$associated_mv_tv_procedures <- c(rep(0, 37), 1, rep(0, (nrow(control_group) - 38)))
# If control_group$Op_手术名称 is NA, then control_group$associated_mv_tv_procedures is NA as well.
control_group$associated_mv_tv_procedures[is.na(control_group$Op_手术名称)] <- NA
View(control_group[,c("编号", "Name", "Op_手术名称", "associated_mv_tv_procedures")]) # To view the result.

# ======================================================

# 8. Prepare the column of "Antegrade cerebral perfusion".

# 脑灌方式中不为0的人数（无论是单侧顺灌还是双侧顺灌）（EK）。

unique(injury_group$CPB_脑灌方式) # No NAs.
injury_group$antegrate_cerebral_perfusion <- injury_group$CPB_脑灌方式
injury_group$antegrate_cerebral_perfusion[injury_group$CPB_脑灌方式 != c("0")] <- c("1")
View(injury_group[, c("CPB_脑灌方式", "antegrate_cerebral_perfusion")])

unique(intact_group$CPB_脑灌方式) # No NAs.
intact_group$antegrate_cerebral_perfusion <- intact_group$CPB_脑灌方式
intact_group$antegrate_cerebral_perfusion[intact_group$CPB_脑灌方式 != c(0)] <- c("1")
View(intact_group[, c("CPB_脑灌方式", "antegrate_cerebral_perfusion")])

unique(control_group$CPB_脑灌方式) # There are NAs.
sum(is.na(control_group$CPB_脑灌方式)) # There are 27 NAs.
control_group$antegrate_cerebral_perfusion <- control_group$CPB_脑灌方式
control_group$antegrate_cerebral_perfusion[control_group$CPB_脑灌方式 %in% c("单侧顺灌", "双侧顺灌", "两次顺灌", "单纯顺灌")] <- c("1")
View(control_group[, c("CPB_脑灌方式", "antegrate_cerebral_perfusion")])

# ======================================================

# 9. Prepare the column of "No. of patient".

# 任意有术中（红细胞、血浆、血小板）输血的人数总和（EY，EZ，FA）。

unique(injury_group$`术中（红细胞）`) # There are NAs.
unique(injury_group$`术中（血浆）`) # There are NAs.
unique(injury_group$`术中（血小板）`) # There are NAs.

unique(intact_group$`术中（红细胞）`) # There are NAs.
unique(intact_group$`术中（血浆）`) # There are NAs.
unique(intact_group$`术中（血小板）`) # There are NAs.

unique(control_group$`术中（红细胞）`) # There are NAs, "——", and "？". And is character.
control_group$`术中（红细胞）`[control_group$`术中（红细胞）` %in% c("——", "？")] <- NA
control_group$`术中（红细胞）` <- as.numeric(control_group$`术中（红细胞）`) # Change from character to numeric.
unique(control_group$`术中（红细胞）`)

unique(control_group$`术中（血浆）`) # There are NAs, "——", and "？". And is character.
control_group$`术中（血浆）`[control_group$`术中（血浆）` %in% c("——", "？")] <- NA
control_group$`术中（血浆）` <- as.numeric(control_group$`术中（血浆）`) # Change from character to numeric.
unique(control_group$`术中（血浆）`)

unique(control_group$`术中（血小板）`) # There are NAs, "——", and "术前贫血，AKI". And is character.
control_group$`术中（血小板）`[control_group$`术中（血小板）` %in% c("——", "术前贫血，AKI")] <- NA
control_group$`术中（血小板）` <- as.numeric(control_group$`术中（血小板）`) # Change from character to numeric.
unique(control_group$`术中（血小板）`)

# Create a function to count the tranfused patients, i.e. transfused patients were tagged with 1,
# untransfused 0.

transfused <- function(ey, ez, fa) {
    if (any(!is.na(c(ey, ez, fa))) && sum(c(ey, ez, fa), na.rm = TRUE) > 0) {
        return(1)
    } else if (all(is.na(c(ey, ez, fa)))) {
        return(NA)
    } else {
        return(0)
    }
}

injury_group$transfused_patients <- mapply(transfused, injury_group$`术中（红细胞）`, injury_group$`术中（血浆）`, injury_group$`术中（血小板）`)
View(injury_group[, c("术中（红细胞）", "术中（血浆）", "术中（血小板）", "transfused_patients")])

intact_group$transfused_patients <- mapply(transfused, intact_group$`术中（红细胞）`, intact_group$`术中（血浆）`, intact_group$`术中（血小板）`)
View(intact_group[, c("术中（红细胞）", "术中（血浆）", "术中（血小板）", "transfused_patients")])

control_group$transfused_patients <- mapply(transfused, control_group$`术中（红细胞）`, control_group$`术中（血浆）`, control_group$`术中（血小板）`)
View(control_group[, c("术中（红细胞）", "术中（血浆）", "术中（血小板）", "transfused_patients")])

# ======================================================

# 10. Retrieve the columns relevant to Table 2 (5 items per row for clarity).

table2_colnames <- c(
    "supracoronory_aar", "isolated_avr", "single_sinus_replacement", "Op_HAR", "TAR_with_FET",
    "TAR_without_FET", "associated_mv_tv_procedures", "CPB_CPB", "CPB_Xclamp", "CPB_DHCA time",
    "antegrate_cerebral_perfusion", "CPB_最低鼻咽温度", "transfused_patients", "术中（红细胞）", "术中（血浆）"
)

table2_colnames_new <- c(
    "Supracoronary AAR", "Isolated AVR", "Single sinus replacement", "HAR", "TAR with FET",
    "TAR without FET", "Associated MV/TV procedures", "CPB time (min)", "ACC time (min)", "Duration of HCA, min",
    "Antegrade cerebral perfusion", "Lowest temperature, °C", "No. of patient", "Red blood cells, U", "Plasma, ml"
)

table2_names <- data.frame(
    old = table2_colnames,
    new = table2_colnames_new
)

View(table2_names) # Do check the correctness of the matched rows.

# ======================================================

# 11. Add one more column to table2_names marking if they are numeric or factor.

num_or_factor <- c(
    "fac", "fac", "fac", "fac", "fac",
    "fac", "fac", "num", "num", "num",
    "fac", "num", "fac", "num", "num"
)

table2_names$num_or_fac <- num_or_factor
View(table2_names)

# ======================================================

# 12. Extract the columns based on table2_names$old

injury_group_table2 <- injury_group[, table2_names$old]
intact_group_table2 <- intact_group[, table2_names$old]
control_group_table2 <- control_group[, table2_names$old]

dim(injury_group_table2) # Should be 15 columns.
dim(intact_group_table2) # Should be 15 columns.
dim(control_group_table2) # Should be 15 columns.

names(injury_group_table2) # Show the column names.
names(injury_group_table2) == names(intact_group_table2) # Compare the column names between the two dataframes. Should be all TRUE.
all(names(injury_group_table2) == names(control_group_table2)) # Compare the column names between the two dataframes. Should be TRUE.

all(names(injury_group_table2) == table2_names$old) # Compare between the column names of "injury_group_table2" and table2_names$old. Should be TRUE.

# ======================================================

# 13. See if there are any "NA", "N/A", "na", "n/a" or " " within table.

# ------------------------------------------------------

source("function_abnormal_find.R") # Load the function which could find abnormal items.

abnormal_find(injury_group_table2, "NA") # No matter contains or not, leave it alone.
abnormal_find(injury_group_table2, "N/A") # Once contains, change to NA.
abnormal_find(injury_group_table2, "na") # Once contains, change to NA.
abnormal_find(injury_group_table2, "n/a") # Once contains, change to NA.
abnormal_find(injury_group_table2, " ") # Once contains, change to NA.

abnormal_find(intact_group_table2, "NA") # No matter contains or not, leave it alone.
abnormal_find(intact_group_table2, "N/A") # Once contains, change to NA.
abnormal_find(intact_group_table2, "na") # Once contains, change to NA.
abnormal_find(intact_group_table2, "n/a") # Once contains, change to NA.
abnormal_find(intact_group_table2, " ") # Once contains, change to NA.

abnormal_find(control_group_table2, "NA") # No matter contains or not, leave it alone.
abnormal_find(control_group_table2, "N/A") # Once contains, change to NA.
abnormal_find(control_group_table2, "na") # Once contains, change to NA.
abnormal_find(control_group_table2, "n/a") # Once contains, change to NA.
abnormal_find(control_group_table2, " ") # Once contains, change to NA.

# ------------------------------------------------------

# Create a function to split the string with "+" and sum the numbers.
# See "D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_splitAndSum.R" for details.
source("D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_splitAndSum.R")

# ------------------------------------------------------

View(injury_group_table2)
injury_group_table2$CPB_CPB <- sapply(injury_group_table2$CPB_CPB, splitAndSum, USE.NAMES = FALSE)
injury_group_table2$CPB_Xclamp <- sapply(injury_group_table2$CPB_Xclamp, splitAndSum, USE.NAMES = FALSE)
injury_group_table2$`CPB_DHCA time` <- sapply(injury_group_table2$`CPB_DHCA time`, splitAndSum, USE.NAMES = FALSE)
View(injury_group_table2)

View(intact_group_table2)
intact_group_table2$CPB_CPB <- sapply(intact_group_table2$CPB_CPB, splitAndSum, USE.NAMES = FALSE)
intact_group_table2$CPB_Xclamp <- sapply(intact_group_table2$CPB_Xclamp, splitAndSum, USE.NAMES = FALSE)
intact_group_table2$`CPB_DHCA time` <- sapply(intact_group_table2$`CPB_DHCA time`, splitAndSum, USE.NAMES = FALSE)
View(intact_group_table2)

View(control_group_table2) # There is "47？" in the column of "CPB_DHCA time" (i.e. control_group_table2$`CPB_DHCA time`[73]), and change it to "47".
control_group_table2$`CPB_DHCA time`[73]
control_group_table2$`CPB_DHCA time`[73] <- "47" # Change to "47".
control_group_table2$`CPB_DHCA time`[73]
control_group_table2$CPB_CPB <- sapply(control_group_table2$CPB_CPB, splitAndSum, USE.NAMES = FALSE)
control_group_table2$CPB_Xclamp <- sapply(control_group_table2$CPB_Xclamp, splitAndSum, USE.NAMES = FALSE)
control_group_table2$`CPB_DHCA time` <- sapply(control_group_table2$`CPB_DHCA time`, splitAndSum, USE.NAMES = FALSE)
View(control_group_table2)

# ======================================================

# 14. Check the data structure.

# ------------------------------------------------------

# Create a function to change data structure (i.e. numeric or factor).
# See "D:\R&S\1_2_项目或任务\1_邵医生数据\2_20250122\function_change_structure.R" for details.
source("D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_change_structure.R")

# ------------------------------------------------------

str(injury_group_table2) # Check the internal structure of objects.
injury_group_table2 <- change_structure(injury_group_table2, table2_names)
str(injury_group_table2) # Check the internal structure once again.

str(intact_group_table2) # Check the internal structure of objects. 
intact_group_table2 <- change_structure(intact_group_table2, table2_names)
str(intact_group_table2) # Check the internal structure once again.

str(control_group_table2) # Check the internal structure of objects.
control_group_table2 <- change_structure(control_group_table2, table2_names)
str(control_group_table2) # Check the internal structure once again.

# ======================================================

# 15. Generate table 2.

# ------------------------------------------------------

# Add a column for group identification.
injury_group_table2$group <- rep("Injury_group", nrow(injury_group_table2))
intact_group_table2$group <- rep("Intact_group", nrow(intact_group_table2))
control_group_table2$group <- rep("Control_group", nrow(control_group_table2))

# ------------------------------------------------------

# Combine the three tables.
combined_data_table2 <- rbind(injury_group_table2, intact_group_table2, control_group_table2)

# Compare the colnames between combined_data_table2 and table2_names$old
names(combined_data_table2) == table2_names$old # The 1st 26 items are the same (one is 16 in length, the other one is 15 in length).
length(names(combined_data_table2)) # 16
names(combined_data_table2)
names(combined_data_table2)[1:15] <- table2_names$new # Rename the column names.
names(combined_data_table2)

# ------------------------------------------------------

# Needn't change the sequence of levels of some columns.

# Generate the comparative table using the package of table1.

library(table1)

# Create a function to show table with just 'Median [Min, Max]'.
# See "D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_my.render.cont.R" for details.
source("D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/function_my.render.cont.R")

# Generate table.
combined_data_table2_TABLE <- table1::table1(~ . - group | group, data = combined_data_table2, render.continuous = my.render.cont)

# Save the table as csv file.
write.csv(combined_data_table2_TABLE, "2_table2.csv", row.names = F)

# Save the table as html file.
library(htmlTable)
htmlTable(combined_data_table2_TABLE) |> writeLines("2_table2.html")

# ------------------------------------------------------

# Calculate p-value.
col_name <- names(combined_data_table2)[1:(length(names(combined_data_table2)) - 1)] # Store the column names without "group" name (last one).
col_name_p <- rep(1, length(col_name)) # For storing the calculated p-value.

for (i1 in 1:(length(col_name))) { # Loop to calculate p-value.  
  
  if (is.numeric(combined_data_table2[[col_name[i1]]])) {    
    p_value <- kruskal.test(combined_data_table2[[col_name[i1]]] ~ combined_data_table2$group)$p.value # Kruskal-Wallis test for numeric.
    col_name_p[i1] <- sprintf("%.3f", p_value) # Display only two decimal places.
  } else if (is.factor(combined_data_table2[[col_name[i1]]])) {    
    p_value <- chisq.test(table(combined_data_table2[[col_name[i1]]], combined_data_table2$group), simulate.p.value = TRUE)$p.value # chi-square test for categorical vector.
    col_name_p[i1] <- sprintf("%.3f", p_value) # Display only two decimal places.
  }
  
}

combined_data_table2_p_value <- data.frame(
  columns = col_name,
  'p-value' = col_name_p
)

# Save the table as csv file.
write.csv(combined_data_table2_p_value, "2_table2_p_value.csv", row.names = F)

# ======================================================

# 16. Save the current R session.

save.image("1_read_the_three_sheets_containing_clinical_info_for_table2.Rdata")

# ======================================================