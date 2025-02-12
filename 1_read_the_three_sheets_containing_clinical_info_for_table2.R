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