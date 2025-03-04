setwd("D:/R&S/1_2_项目或任务/1_邵医生数据/2_20250122/")

# ======================================================

# 1. Read the three tables.

table_one <- read.csv("1_table1_patient.csv") # Read table 1.
table_two <- read.csv("2_table2_patient.csv") # Read table 2.
table_three <- read.csv("3_table3_patient.csv") # Read table 3.

dim(table_one) # 508 30
dim(table_two) # 508 19
dim(table_three) # 508 16

View(table_one)
View(table_two)
View(table_three)

# ======================================================

# 2. Check the consistency of the three tables.

unique(table_one$编号 == table_two$编号) # Should be TRUE.
unique(table_one$编号 == table_three$编号) # Should be TRUE.

unique(table_one$His_ID == table_two$His_ID) # Should be TRUE.
unique(table_one$His_ID == table_three$His_ID) # Should be TRUE.

unique(table_one$Name == table_two$Name) # Should be TRUE.
unique(table_one$Name == table_three$Name) # Should be TRUE.

unique(table_one$group == table_two$group) # Should be TRUE.
unique(table_one$group == table_three$group) # Should be TRUE.

# ======================================================

# 3. Merge the three tables.

combined_123 <- cbind(table_one[, -c(30)], table_two[, -c(1:3, 19)], table_three[, -c(1:3)]) # Merge the three tables.

# ======================================================

# 4. Save the table as csv file.

write.csv(combined_123, "4_table-one-two-three.csv", row.names = F)

# ======================================================

# 4. Save the image.

save.image("4_read_the_three_tables.Rdata")

# ======================================================