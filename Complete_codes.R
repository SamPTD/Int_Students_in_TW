#Load Necessary Packages
install.packages("dplyr")
install.packages("showtext")
install.packages("cowplot")
install.packages("ggplot2")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("ggrepel")

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(showtext)
library(cowplot)
library(tidyr)
library(forcats)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

#Set Up
showtext_auto(enable = TRUE)

# Set working directory
new_path <- "C:/Dataset" #change path accordingly
setwd(new_path)
print(getwd())
list.files()

#Step 1: Load and Process International Student Data
# Load International Student Data
int_folder_path <- "C:/Dataset/2015_2023 按科別分類外國學生及其畢業生人數"
int_file_names <- sprintf("%s/1%02d_regular.csv", int_folder_path, 4:12)
int_data_list <- lapply(int_file_names, read.csv)

# Check for Required Columns
required_columns <- c("學年度", "學校名稱", "國別名稱", "科系代碼", "科系名稱", "學生總計", "男學生", "女學生", "上學年畢業生總計", "上學年度男畢業生", "上學年度女畢業生")
check_missing_columns <- function(dataset, required_cols) {
  missing_cols <- required_cols[!required_cols %in% names(dataset)]
  return(missing_cols)
}
int_missing_columns_list <- lapply(int_data_list, check_missing_columns, required_columns)

# Identify Datasets with Missing Columns
int_datasets_with_missing_columns <- which(sapply(int_missing_columns_list, length) > 0)
if (length(int_datasets_with_missing_columns) > 0) {
  cat("The following datasets are missing columns:\n")
  for (i in int_datasets_with_missing_columns) {
    cat("Dataset", i, "is missing columns:", paste(int_missing_columns_list[[i]], collapse = ", "), "\n")
  }
} else {
  cat("All datasets have the required columns.\n")
}

# Replace NAs and Dashes
replace_na_and_dash <- function(column) {
  column <- gsub(" -", "0", column)
  column <- ifelse(is.na(column) | column == "", 0, column)
  as.numeric(column)
}
correct_and_check_values <- function(dataset) {
  numeric_columns <- c("一年級男", "二年級男", "三年級男", "四年級男", "五年級男", "六年級男", "七年級男", "延修生男",
                       "一年級女", "二年級女", "三年級女", "四年級女", "五年級女", "六年級女", "七年級女", "延修生女",
                       "上學年度男畢業生", "上學年度女畢業生")
  dataset[numeric_columns] <- lapply(dataset[numeric_columns], replace_na_and_dash)
  dataset$男學生 <- rowSums(dataset[, c("一年級男", "二年級男", "三年級男", "四年級男", "五年級男", "六年級男", "七年級男", "延修生男")], na.rm = TRUE)
  dataset$女學生 <- rowSums(dataset[, c("一年級女", "二年級女", "三年級女", "四年級女", "五年級女", "六年級女", "七年級女", "延修生女")], na.rm = TRUE)
  dataset$學生總計 <- dataset$男學生 + dataset$女學生
  dataset$上學年畢業生總計 <- rowSums(dataset[, c("上學年度男畢業生", "上學年度女畢業生")], na.rm = TRUE)
  return(dataset)
}
int_data_list <- lapply(int_data_list, correct_and_check_values)

# Select Required Columns and Merge
select_required_columns <- function(dataset, required_cols) {
  dataset %>%
    select(all_of(required_cols)) %>%
    mutate(across(everything(), as.character))
}
int_data_list_selected <- lapply(int_data_list, select_required_columns, required_columns)
int_data <- Reduce(function(x, y) full_join(x, y, by = required_columns), int_data_list_selected)

#Step 2: Load and Process Overseas Chinese Data
# Load Overseas Chinese Data
oc_folder_path <- "C:/Dataset/2015_2023 大專校院僑生、港澳生及其畢業生人數"
oc_file_names <- sprintf("%s/1%02d_over_higher.csv", oc_folder_path, 4:12)
oc_data_list <- lapply(oc_file_names, read.csv)

# Check for Required Columns
oc_missing_columns_list <- lapply(oc_data_list, check_missing_columns, required_columns)
oc_datasets_with_missing_columns <- which(sapply(oc_missing_columns_list, length) > 0)
if (length(oc_datasets_with_missing_columns) > 0) {
  cat("The following datasets are missing columns:\n")
  for (i in oc_datasets_with_missing_columns) {
    cat("Dataset", i, "is missing columns:", paste(oc_missing_columns_list[[i]], collapse = ", "), "\n")
  }
} else {
  cat("All datasets have the required columns.\n")
}

# Correct Values and Ensure Numeric Conversions
oc_check_and_correct_dataset <- function(dataset) {
  required_columns <- c("學生總計", "上學年畢業生總計")
  missing_columns <- required_columns[!required_columns %in% names(dataset)]
  if (length(missing_columns) > 0) {
    dataset <- correct_and_check_values(dataset)
  }
  return(dataset)
}
oc_data_list <- lapply(oc_data_list, oc_check_and_correct_dataset)
cols_to_convert <- c("學生總計", "男學生", "女學生", "上學年畢業生總計", "上學年度男畢業生", "上學年度女畢業生")
oc_data_list <- lapply(oc_data_list, function(dataset) {
  dataset[cols_to_convert] <- lapply(dataset[cols_to_convert], replace_na_and_dash)
  return(dataset)
})

# Select Required Columns and Merge
oc_data_list_selected <- lapply(oc_data_list, select_required_columns, required_columns)
oc_data <- Reduce(function(x, y) full_join(x, y, by = required_columns), oc_data_list_selected)

#Step 3: Merge International and Overseas Chinese Data
# Clean and Convert Data Types
clean_and_convert_to_numeric <- function(column) {
  column <- gsub("[^0-9.-]", "", column)
  as.numeric(column)
}
int_data$學年度 <- clean_and_convert_to_numeric(int_data$學年度)
int_data$科系代碼 <- clean_and_convert_to_numeric(int_data$科系代碼)
oc_data$學年度 <- clean_and_convert_to_numeric(oc_data$學年度)
oc_data$科系代碼 <- clean_and_convert_to_numeric(oc_data$科系代碼)
int_data$學校名稱 <- as.character(int_data$學校名稱)
int_data$國別名稱 <- as.character(int_data$國別名稱)
int_data$科系名稱 <- as.character(int_data$科系名稱)
oc_data$學校名稱 <- as.character(oc_data$學校名稱)
oc_data$國別名稱 <- as.character(oc_data$國別名稱)
oc_data$科系名稱 <- as.character(oc_data$科系名稱)
numeric_cols <- c("學生總計", "男學生", "女學生", "上學年畢業生總計", "上學年度男畢業生", "上學年度女畢業生")
int_data[numeric_cols] <- lapply(int_data[numeric_cols], clean_and_convert_to_numeric)
oc_data[numeric_cols] <- lapply(oc_data[numeric_cols], clean_and_convert_to_numeric)

# Summarize Data
int_data_summary <- int_data %>%
  group_by(學年度, 學校名稱, 國別名稱, 科系代碼, 科系名稱) %>%
  summarise(
    學生總計 = sum(學生總計, na.rm = TRUE),
    男學生 = sum(男學生, na.rm = TRUE),
    女學生 = sum(女學生, na.rm = TRUE),
    上學年畢業生總計 = sum(上學年畢業生總計, na.rm = TRUE),
    上學年度男畢業生 = sum(上學年度男畢業生, na.rm = TRUE),
    上學年度女畢業生 = sum(上學年度女畢業生, na.rm = TRUE),
    .groups = 'drop'
  )
oc_data_summary <- oc_data %>%
  group_by(學年度, 學校名稱, 國別名稱, 科系代碼, 科系名稱) %>%
  summarise(
    學生總計 = sum(學生總計, na.rm = TRUE),
    男學生 = sum(男學生, na.rm = TRUE),
    女學生 = sum(女學生, na.rm = TRUE),
    上學年畢業生總計 = sum(上學年畢業生總計, na.rm = TRUE),
    上學年度男畢業生 = sum(上學年度男畢業生, na.rm = TRUE),
    上學年度女畢業生 = sum(上學年度女畢業生, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge Data
merged_data <- full_join(int_data_summary, oc_data_summary, by = c("學年度", "學校名稱", "國別名稱", "科系代碼", "科系名稱"))
int_oc_data <- merged_data %>%
  mutate(
    學生總計 = rowSums(cbind(學生總計.x, 學生總計.y), na.rm = TRUE),
    男學生 = rowSums(cbind(男學生.x, 男學生.y), na.rm = TRUE),
    女學生 = rowSums(cbind(女學生.x, 女學生.y), na.rm = TRUE),
    上學年畢業生總計 = rowSums(cbind(上學年畢業生總計.x, 上學年畢業生總計.y), na.rm = TRUE),
    上學年度男畢業生 = rowSums(cbind(上學年度男畢業生.x, 上學年度男畢業生.y), na.rm = TRUE),
    上學年度女畢業生 = rowSums(cbind(上學年度女畢業生.x, 上學年度女畢業生.y), na.rm = TRUE)
  ) %>%
  select(學年度, 學校名稱, 國別名稱, 科系代碼, 科系名稱, 學生總計, 男學生, 女學生, 上學年畢業生總計, 上學年度男畢業生, 上學年度女畢業生)
int_oc_data[is.na(int_oc_data)] <- 0

#Step 4: Load and Process Chinese Current Students
# Load Chinese Current Students Data
cc_folder_path <- "C:/Dataset/2015_2023 大專校院科系別正式修讀學位之大陸學生數（學生＋畢業生）"
cc_file_names <- sprintf("%s/%03d_chinas1a.csv", cc_folder_path, 104:112)

# Check for Required Columns
cc_required_columns <- c("學年度", "學校名稱", "科系代碼", "科系名稱", "學生總計", "男學生", "女學生")
cc_numeric_columns <- c("一年級男生", "二年級男生", "三年級男生", "四年級男生", "五年級男生", "六年級男生", "七年級男生", "延修生男生",
                        "一年級女生", "二年級女生", "三年級女生", "四年級女生", "五年級女生", "六年級女生", "七年級女生", "延修生女生")
cc_data_list <- lapply(cc_file_names, function(file_name) {
  year <- as.numeric(sub(".*/(\\d{3})_.*\\.csv", "\\1", file_name))
  print(paste("Year extracted:", year))
  df <- read.csv(file_name)
  missing_cols <- cc_required_columns[!cc_required_columns %in% names(df)]
  if (length(missing_cols) > 0) {
    cat("File:", file_name, "\nMissing columns:", paste(missing_cols, collapse = ", "), "\n\n")
  }
  return(list(data = df, missing_cols = missing_cols, year = year))
})

# Add Missing Columns and `學年度` Column
cc_data_list <- lapply(cc_data_list, function(df_info) {
  df <- df_info$data
  missing_cols <- df_info$missing_cols
  year <- df_info$year
  for (col in missing_cols) {
    df[[col]] <- 0
  }
  df$學年度 <- year
  return(df)
})

# Ensure Numeric Conversions and Calculate Values
cc_data_list <- lapply(cc_data_list, function(df) {
  replace_na_and_dash <- function(column) {
    column <- gsub(" -", "0", column)
    column <- ifelse(is.na(column) | column == "", 0, column)
    as.numeric(column)
  }
  df[cc_numeric_columns] <- lapply(df[cc_numeric_columns], replace_na_and_dash)
  df$男學生 <- rowSums(df[, cc_numeric_columns[1:8]], na.rm = TRUE)
  df$女學生 <- rowSums(df[, cc_numeric_columns[9:16]], na.rm = TRUE)
  df$學生總計 <- df$男學生 + df$女學生
  return(df)
})

# Select Required Columns and Merge
cc_data_list_selected <- lapply(cc_data_list, function(df) {
  df %>%
    mutate(across(all_of(cc_required_columns), as.character)) %>%
    select(all_of(cc_required_columns))
})
cc_data_combined <- bind_rows(cc_data_list_selected)
cc_data <- cc_data_combined %>%
  group_by(學年度, 學校名稱, 科系代碼, 科系名稱) %>%
  summarise(
    學生總計 = sum(as.numeric(學生總計), na.rm = TRUE),
    男學生 = sum(as.numeric(男學生), na.rm = TRUE),
    女學生 = sum(as.numeric(女學生), na.rm = TRUE),
    .groups = 'drop'
  )

#Step 5: Load and Process Chinese Graduates
# Load Chinese Graduates Data
cg_folder_path <- "C:/Dataset/2015_2023 大專校院科系別正式修讀學位之大陸學生數（學生＋畢業生）"
cg_file_names <- sprintf("%s/%03d_chinas2a.csv", cg_folder_path, 104:112)

# Check for Required Columns
cg_required_columns <- c("學年度", "學校名稱", "科系代碼", "科系名稱", "上學年度男畢業生", "上學年度女畢業生", "上學年畢業生總計")
cg_numeric_columns <- c("男生", "女生", "學年度", "科系代碼")
cg_data_list <- lapply(cg_file_names, function(file_name) {
  year <- as.numeric(sub(".*/(\\d{3})_.*\\.csv", "\\1", file_name))
  print(paste("Year extracted:", year))
  df <- read.csv(file_name)
  missing_cols <- cg_required_columns[!cg_required_columns %in% names(df)]
  if (length(missing_cols) > 0) {
    cat("File:", file_name, "\nMissing columns:", paste(missing_cols, collapse = ", "), "\n\n")
  }
  return(list(data = df, missing_cols = missing_cols, year = year))
})

# Add Missing Columns and `學年度` Column
cg_data_list <- lapply(cg_data_list, function(df_info) {
  df <- df_info$data
  missing_cols <- df_info$missing_cols
  year <- df_info$year
  for (col in missing_cols) {
    df[[col]] <- 0
  }
  df$學年度 <- year
  return(df)
})

# Ensure Numeric Conversions and Calculate Values
cg_data_list <- lapply(cg_data_list, function(df) {
  replace_na_and_dash <- function(column) {
    column <- gsub(" -", "0", column)
    column <- ifelse(is.na(column) | column == "", 0, column)
    as.numeric(column)
  }
  df[c("男生", "女生")] <- lapply(df[c("男生", "女生")], replace_na_and_dash)
  df$上學年度男畢業生 <- df$男生
  df$上學年度女畢業生 <- df$女生
  df$上學年畢業生總計 <- df$上學年度男畢業生 + df$上學年度女畢業生
  return(df)
})

# Select Required Columns and Merge
cg_data_list_selected <- lapply(cg_data_list, function(df) {
  df %>%
    mutate(across(all_of(c("學校名稱", "科系名稱")), as.character),
           across(all_of(c("學年度", "科系代碼", "上學年度男畢業生", "上學年度女畢業生", "上學年畢業生總計")), as.numeric)) %>%
    select(all_of(cg_required_columns))
})
cg_data_combinded <- bind_rows(cg_data_list_selected)
cg_data <- cg_data_combinded %>%
  group_by(學年度, 學校名稱, 科系代碼, 科系名稱) %>%
  summarise(
    上學年度男畢業生 = sum(上學年度男畢業生, na.rm = TRUE),
    上學年度女畢業生 = sum(上學年度女畢業生, na.rm = TRUE),
    上學年畢業生總計 = sum(上學年畢業生總計, na.rm = TRUE),
    .groups = 'drop'
  )

#Step 6: Merge Chinese Current Students and Graduates
# Clean and Convert Data Types for Merging
cc_data <- cc_data %>%
  mutate(across(all_of(c("學年度", "科系代碼")), as.numeric),
         across(all_of(c("學校名稱", "科系名稱")), as.character))
cg_data <- cg_data %>%
  mutate(across(all_of(c("學年度", "科系代碼")), as.numeric),
         across(all_of(c("學校名稱", "科系名稱")), as.character))

# Merge Current Students and Graduates Data
chin_data <- full_join(cc_data, cg_data, by = c("學年度", "學校名稱", "科系代碼", "科系名稱"))
chin_data <- chin_data %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(國別名稱 = "陸生")

#Step 7: Merge All Data
# Clean and Convert Data Types for Merging
int_oc_data <- int_oc_data %>%
  mutate(across(all_of(c("學年度", "科系代碼")), as.numeric),
         across(all_of(c("學校名稱", "國別名稱", "科系名稱")), as.character))
chin_data <- chin_data %>%
  mutate(across(all_of(c("學年度", "科系代碼")), as.numeric),
         across(all_of(c("學校名稱", "國別名稱", "科系名稱")), as.character))

# Merge All Data
all_data <- full_join(int_oc_data, chin_data, by = c("學年度", "學校名稱", "科系代碼", "科系名稱","國別名稱", "學生總計", "男學生", "女學生", "上學年畢業生總計", "上學年度男畢業生", "上學年度女畢業生"))

# Add `學門` Column
digit_counts <- nchar(as.character(all_data$科系代碼))
table(digit_counts)
na_count <- sum(is.na(all_data$科系代碼))
print(paste("Number of NA values in '科系代碼':", na_count))

# Correct `科系代碼` for Entries with 1 and 5 Digits
correct_codes <- data.frame(
  科系名稱 = c(
    "商品創意經營系",
    "工業管理與資訊系工業管理組",
    "工業管理與資訊系電子商務組",
    "巨量資料管理學院學士學位學程",
    "數位設計與資訊管理系",
    "時尚創意管理學士學位學程",
    "海洋觀光管理學士學位學程"
  ),
  正確科系代碼 = c(
    "3499A2",
    "3499A9",
    "3499A9",
    "3499A4",
    "3499A7",
    "3499B7",
    "3499A1"
  ),
  stringsAsFactors = FALSE
)
all_data <- all_data %>%
  left_join(correct_codes, by = "科系名稱") %>%
  mutate(科系代碼 = ifelse(!is.na(正確科系代碼), 正確科系代碼, 科系代碼)) %>%
  select(-正確科系代碼)

# Add `學門` Column Based on `科系代碼`
mapping_104_105 <- data.frame(
  prefix = c("14", "21", "22", "23", "31", "32", "34", "38", "42", "44", "46", "48", "52", "58", "62", "64", "72", "76", "81", "84", "85", "86", "99"),
  學門 = c("教育學門", "藝術學門", "人文學門", "設計學門", "社會及行為科學學門", "傳播學門", "商業及管理學門", "法律學門", "生命科學學門", "自然科學學門", "數學及統計學門", "電算機學門", "工程學門", "建築及都市規劃學門", "農業科學學門", "獸醫學門", "醫藥衛生學門", "社會服務學門", "民生學門", "運輸服務學門", "環境保護學門", "軍警國防安全學門", "其他學門"),
  stringsAsFactors = FALSE
)
mapping_106_112 <- data.frame(
  prefix = c("011", "021", "022", "023", "031", "032", "041", "042", "051", "052", "053", "054", "061", "071", "072", "073", "081", "082", "083", "084", "091", "092", "101", "102", "103", "104", "999"),
  學門 = c("教育學門", "藝術學門", "人文學門", "語文學門", "社會及行為科學學門", "新聞學及圖書資訊學門", "商業及管理學門", "法律學門", "生命科學學門", "環境學門", "物理、化學及地球科學學門", "數學及統計學門", "資訊通訊科技學門", "工程及工程業學門", "製造及加工學門", "建築及營建工程學門", "農業學門", "林業學門", "漁業學門", "獸醫學門", "醫藥衛生學門", "社會福利學門", "餐旅及民生服務學門", "衛生及職業衛生服務學門", "安全服務學門", "運輸服務學門", "其他學門"),
  stringsAsFactors = FALSE
)
all_data <- all_data %>%
  mutate(科系代碼 = ifelse((nchar(科系代碼) %in% c(6, 7)) & 學年度 >= 106, paste0("0", 科系代碼), 科系代碼))
all_data <- all_data %>%
  mutate(學門 = case_when(
    學年度 %in% 104:105 ~ mapping_104_105$學門[match(substr(科系代碼, 1, 2), mapping_104_105$prefix)],
    學年度 %in% 106:112 & nchar(科系代碼) == 8 ~ mapping_106_112$學門[match(substr(科系代碼, 1, 3), mapping_106_112$prefix)],
    學年度 %in% 106:112 & nchar(科系代碼) == 6 ~ mapping_104_105$學門[match(substr(科系代碼, 1, 2), mapping_104_105$prefix)],
    TRUE ~ NA_character_
  ))
na_count <- sum(is.na(all_data$學門))
print(paste("Number of NA values in '學門':", na_count))

# Manually Map Specified 科系名稱 to Their Respective 學門
all_data <- all_data %>%
  mutate(學門 = case_when(
    科系名稱 == "數位設計與資訊管理系" ~ "商業及管理學門",
    科系名稱 == "海洋觀光管理學士學位學程" ~ "商業及管理學門",
    科系名稱 == "工學院製造與管理外國學生專班" ~ "工程及工程業學門",
    科系名稱 == "時尚創意管理學士學位學程" ~ "商業及管理學門",
    科系名稱 == "電資學院_新南向車用電子國際學生學合作專班" ~ "工程及工程業學門",
    科系名稱 == "巨量資料管理學院學士學位學程" ~ "商業及管理學門",
    科系名稱 == "科技學院_新南向車用電子國際學生學合作專班" ~ "工程及工程業學門",
    科系名稱 == "人類發展與健康學院_國際運動科學外國學生專班" ~ "餐旅及民生服務學門",
    科系名稱 == "學士後跨藝合創音樂學士學位學程" ~ "藝術學門",
    科系名稱 == "美國聖荷西州立大學商學大數據分析雙學士學位學程" ~ "商業及管理學門",
    科系名稱 == "商品創意經營系" ~ "商業及管理學門",
    TRUE ~ 學門
  ))
all_data <- all_data %>%
  mutate(學門 = ifelse(is.na(學門), "先修部", 學門))

na_count <- sum(is.na(all_data$學門))
print(paste("Number of NA values in '學門':", na_count))

# Add Origin_EN Column
country_translation <- c(
  "馬來西亞" = "Malaysia", "新加坡" = "Singapore", "泰國" = "Thailand",
  "印尼" = "Indonesia", "加拿大" = "Canada", "越南" = "Vietnam",
  "美國" = "United States of America", "南韓" = "South Korea", "日本" = "Japan",
  "澳門" = "Macau", "緬甸" = "Myanmar", "香港" = "Hong Kong",
  "菲律賓" = "Philippines", "瑞士" = "Switzerland", "俄羅斯" = "Russia",
  "巴拉圭" = "Paraguay", "南非" = "South Africa", "智利" = "Chile",
  "紐西蘭" = "New Zealand", "法國" = "France", "土耳其" = "Turkey",
  "巴西" = "Brazil", "奈及利亞" = "Nigeria", "烏克蘭" = "Ukraine",
  "瓜地馬拉" = "Guatemala", "蒙古" = "Mongolia", "波蘭" = "Poland",
  "葡萄牙" = "Portugal", "義大利" = "Italy", "英國" = "United Kingdom",
  "巴拿馬" = "Panama", "斯洛伐克" = "Slovakia", "阿根廷" = "Argentina",
  "甘比亞" = "The Gambia", "印度" = "India", "格瑞那達" = "Grenada",
  "塞內加爾" = "Senegal", "瑞典" = "Sweden", "柬埔寨" = "Cambodia",
  "厄瓜多" = "Ecuador", "沙烏地阿拉伯" = "Saudi Arabia", "哈薩克" = "Kazakhstan",
  "巴基斯坦" = "Pakistan", "辛巴威" = "Zimbabwe", "海地" = "Haiti",
  "吉爾吉斯" = "Kyrgyzstan", "寮國" = "Laos", "澳大利亞" = "Australia",
  "以色列" = "Israel", "汶萊" = "Brunei", "約旦" = "Jordan",
  "聖克里斯多福" = "Saint Kitts and Nevis", "聖文森" = "Saint Vincent and the Grenadines",
  "薩爾瓦多" = "El Salvador", "史瓦帝尼王國" = "Eswatini", "剛果" = "Republic of the Congo",
  "巴哈馬" = "The Bahamas", "匈牙利" = "Hungary", "秘魯" = "Peru",
  "衣索比亞" = "Ethiopia", "尼泊爾" = "Nepal", "芬蘭" = "Finland",
  "多明尼加" = "Dominican Republic", "史瓦濟蘭" = "Eswatini", "伊朗" = "Iran",
  "突尼西亞" = "Tunisia", "委內瑞拉" = "Venezuela", "西班牙" = "Spain",
  "布吉納法索" = "Burkina Faso", "土庫曼" = "Turkmenistan", "烏茲別克" = "Uzbekistan",
  "德國" = "Germany", "尼加拉瓜" = "Nicaragua", "查德" = "Chad",
  "拉脫維亞" = "Latvia", "諾魯" = "Nauru", "吐瓦魯" = "Tuvalu",
  "塞爾維亞共和國" = "Serbia", "馬紹爾群島共和國" = "Marshall Islands", "哥倫比亞" = "Colombia",
  "烏干達" = "Uganda", "聖多美普林西比" = "São Tomé and Príncipe", "宏都拉斯" = "Honduras",
  "玻利維亞" = "Bolivia", "馬拉威" = "Malawi", "黎巴嫩" = "Lebanon",
  "白俄羅斯" = "Belarus", "喬治亞" = "Georgia", "荷蘭" = "Netherlands",
  "帛琉" = "Palau", "墨西哥" = "Mexico", "貝里斯" = "Belize",
  "聖露西亞" = "Saint Lucia", "葉門" = "Yemen", "亞美尼亞" = "Armenia",
  "索馬利蘭共和國" = "Somaliland", "利比亞" = "Libya", "奧地利" = "Austria",
  "尚比亞" = "Zambia", "斯里蘭卡" = "Sri Lanka", "盧安達" = "Rwanda",
  "剛果民主共和國" = "Democratic Republic of the Congo", "埃及" = "Egypt", "喀麥隆" = "Cameroon",
  "孟加拉" = "Bangladesh", "丹麥" = "Denmark", "坦尚尼亞" = "Tanzania",
  "摩洛哥" = "Morocco", "迦納" = "Ghana", "賴索托" = "Lesotho",
  "莫三比克" = "Mozambique", "象牙海岸" = "Ivory Coast", "索羅門群島" = "Solomon Islands",
  "索馬利亞民主共和國" = "Somalia", "肯亞" = "Kenya", "賴比瑞亞" = "Liberia",
  "納米比亞" = "Namibia", "羅馬尼亞" = "Romania", "立陶宛" = "Lithuania",
  "南蘇丹共和國" = "South Sudan", "蘇丹" = "Sudan", "密克羅尼西亞" = "Federated States of Micronesia",
  "塔吉克" = "Tajikistan", "斐濟" = "Fiji", "捷克" = "Czech Republic",
  "蓋亞那" = "Guyana", "烏拉圭" = "Uruguay", "吉布地" = "Djibouti",
  "馬爾地夫" = "Maldives", "克羅埃西亞" = "Croatia", "阿爾巴尼亞" = "Albania",
  "巴布亞紐幾內亞" = "Papua New Guinea", "阿富汗" = "Afghanistan", "波札那" = "Botswana",
  "斯洛維尼亞" = "Slovenia", "保加利亞" = "Bulgaria", "愛爾蘭" = "Ireland",
  "牙買加" = "Jamaica", "敘利亞" = "Syria", "愛沙尼亞" = "Estonia",
  "哥斯大黎加" = "Costa Rica", "阿曼" = "Oman", "貝南" = "Benin",
  "獅子山共和國" = "Sierra Leone", "巴勒斯坦" = "Palestine", "阿爾及利亞" = "Algeria",
  "幾內亞" = "Guinea", "愛沙尼亞" = "Estonia", "挪威" = "Norway",
  "模里西斯" = "Mauritius", "摩納哥" = "Monaco", "蒲隆地" = "Burundi",
  "盧森堡" = "Luxembourg", "科索沃共和國" = "Kosovo", "冰島" = "Iceland",
  "馬爾他" = "Malta", "不丹" = "Bhutan", "阿拉伯聯合大公國" = "United Arab Emirates",
  "波士尼亞與赫塞哥維納" = "Bosnia and Herzegovina", "多米尼克" = "Dominica",
  "加彭" = "Gabon", "馬其頓" = "Republic of Macedonia", "巴貝多" = "Barbados",
  "賽普勒斯" = "Cyprus", "北馬其頓共和國" = "Republic of Macedonia", "希臘" = "Greece",
  "多哥" = "Togo", "古巴" = "Cuba", "巴林" = "Bahrain",
  "科威特" = "Kuwait", "幾內亞比索" = "Guinea-Bissau", "亞塞拜然" = "Azerbaijan",
  "馬達加斯加" = "Madagascar", "萬那杜" = "Vanuatu", "摩爾多瓦" = "Moldova",
  "陸生" = "China", "密克羅尼西亞" = "Federated States of Micronesia", "安道爾" = "Andorra", 
  "伊拉克" = "Iraq", "比利時" = "Belgium", "吉里巴斯" = "Kiribati"
)
all_data <- all_data %>%
  mutate(Origin_EN = country_translation[國別名稱])
all_data$Origin_EN[is.na(all_data$Origin_EN)] <- "Unknown"

# Add Discipline Column
discipline_translation <- c(
  "設計學門" = "Design",
  "傳播學門" = "Journalism and Library Information",
  "社會及行為科學學門" = "Social and Behavioral Sciences",
  "電算機學門" = "Information and Communication Technologies",
  "民生學門" = "Hospitality and Domestic Services",
  "人文學門" = "Humanities",
  "商業及管理學門" = "Business and Administration",
  "工程學門" = "Engineering and Engineering Trades",
  "生命科學學門" = "Life Sciences",
  "自然科學學門" = "Physical Sciences, Chemistry and Earth Sciences",
  "數學及統計學門" = "Mathematics and Statistics",
  "建築及都市規劃學門" = "Architecture and Construction Engineering",
  "教育學門" = "Education",
  "藝術學門" = "Arts",
  "法律學門" = "Law",
  "社會服務學門" = "Social Welfare",
  "醫藥衛生學門" = "Medicine, Health and Welfare",
  "農業科學學門" = "Agriculture",
  "運輸服務學門" = "Transportation Services",
  "獸醫學門" = "Veterinary",
  "環境保護學門" = "Environment",
  "其他學門" = "Other Disciplines",
  "語文學門" = "Languages and Literatures",
  "新聞學及圖書資訊學門" = "Journalism and Library Information",
  "餐旅及民生服務學門" = "Hospitality and Domestic Services",
  "資訊通訊科技學門" = "Information and Communication Technologies",
  "工程及工程業學門" = "Engineering and Engineering Trades",
  "物理、化學及地球科學學門" = "Physical Sciences, Chemistry and Earth Sciences",
  "建築及營建工程學門" = "Architecture and Construction Engineering",
  "農業學門" = "Agriculture",
  "製造及加工學門" = "Manufacturing and Processing",
  "社會福利學門" = "Social Welfare",
  "環境學門" = "Environment",
  "林業學門" = "Forestry",
  "漁業學門" = "Fisheries",
  "安全服務學門" = "Security Services",
  "衛生及職業衛生服務學門" = "Health and Occupational Health Services",
  "軍警國防安全學門" = "Security Services",
  "先修部" = "Preparatory Department"
)
all_data <- all_data %>%
  mutate(Discipline = discipline_translation[學門])
all_data$Discipline[is.na(all_data$Discipline)] <- "Unknown"

# Rename Columns and Adjust `學年度`
all_data <- all_data %>%
  mutate(`學年度` = as.numeric(`學年度`) + 1911)
all_data <- all_data %>%
  rename(
    `學年度 Year` = `學年度`,
    `學校名稱 School` = `學校名稱`,
    `國別名稱 Origin` = `國別名稱`,
    `科系代碼 Code` = `科系代碼`,
    `科系名稱 Major` = `科系名稱`,
    `學生總計 Current Students` = `學生總計`,
    `男學生 Male Students` = `男學生`,
    `女學生 Female Students` = `女學生`,
    `上學年畢業生總計 Last Yr Graduate` = `上學年畢業生總計`,
    `上學年度男畢業生 Last Yr Male Graduate` = `上學年度男畢業生`,
    `上學年度女畢業生 Last Yr Female Graduate` = `上學年度女畢業生`
  )
all_data <- all_data %>%
  select(
    `學年度 Year`, `學校名稱 School`, `國別名稱 Origin`, `Origin_EN`, 
    `科系代碼 Code`, `科系名稱 Major`, `學門`, `Discipline`,
    `學生總計 Current Students`, `男學生 Male Students`, `女學生 Female Students`, 
    `上學年畢業生總計 Last Yr Graduate`, `上學年度男畢業生 Last Yr Male Graduate`, 
    `上學年度女畢業生 Last Yr Female Graduate`
  )

#all_data is now set

#Process with Charts

#Q1.1: Origins of International Students
# Find Top 10 Origins by Year
top_origins_current_students_per_year <- all_data %>%
  group_by(`學年度 Year`, Origin_EN) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  arrange(`學年度 Year`, desc(total_students_current)) %>%
  group_by(`學年度 Year`) %>%
  slice_head(n = 10) %>%
  ungroup()

# Create a Bar Chart for Current Students
all_data_with_top_origins_current <- all_data %>%
  left_join(top_origins_current_students_per_year, by = c("學年度 Year", "Origin_EN")) %>%
  mutate(Top_Origin_Current = ifelse(!is.na(total_students_current), Origin_EN, "Other")) %>%
  select(-total_students_current)
summarized_data_current_students <- all_data_with_top_origins_current %>%
  group_by(`學年度 Year`, Top_Origin_Current) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()
current_students_plot <- ggplot(summarized_data_current_students, aes(x = as.factor(`學年度 Year`), y = total_students_current, fill = Top_Origin_Current)) +
  geom_bar(stat = "identity") +
  labs(title = "Origins of Current Students from 2015-2023", x = "Year", y = "Total Current Students") +
  theme_minimal()
current_students_plot 


# Create World Map for 2015
data_2015 <- all_data %>%
  filter(`學年度 Year` == 2015) %>%
  group_by(Origin_EN) %>%
  summarise(Current_Students_Sum = sum(`學生總計 Current Students`, na.rm = TRUE))
world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- world %>%
  left_join(data_2015, by = c("name" = "Origin_EN"))
current_students_2015 <- ggplot(data = world_data) +
  geom_sf(aes(fill = Current_Students_Sum), color = "white") +
  scale_fill_continuous(name = "Current Students", na.value = "grey", low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Current Students by Origin in 2015") +
  theme(legend.position = "bottom")
current_students_2015 


# Create World Map for 2023
data_2023 <- all_data %>%
  filter(`學年度 Year` == 2023) %>%
  group_by(Origin_EN) %>%
  summarise(Current_Students_Sum = sum(`學生總計 Current Students`, na.rm = TRUE))
world <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- world %>%
  left_join(data_2023, by = c("name" = "Origin_EN"))
current_students_2023 <- ggplot(data = world_data) +
  geom_sf(aes(fill = Current_Students_Sum), color = "white") +
  scale_fill_continuous(name = "Current Students", na.value = "grey", low = "lightblue", high = "darkblue") +
  theme_minimal() +
  labs(title = "Current Students by Origin in 2023") +
  theme(legend.position = "bottom")
current_students_2023 


#Q1.2: Industrial-Academic Cooperation (IAC) Program Students
# Filter Data for ISP
ISP <- all_data %>%
  filter(grepl("產學", `科系名稱 Major`)) %>%
  unique(ISP$`國別名稱 Origin`)

# Compare Origins of ISP Students
summarized_ISP <- ISP %>%
  group_by(`學年度 Year`, Origin_EN) %>%
  summarise(Total_Current_Students = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()
top_origins_ISP <- summarized_ISP %>%
  group_by(Origin_EN) %>%
  summarise(Total_Students = sum(Total_Current_Students)) %>%
  ungroup() %>%
  arrange(desc(Total_Students)) %>%
  slice_head(n = 3) %>%
  pull(Origin_EN)
summarized_ISP <- summarized_ISP %>%
  mutate(Origin_Grouped = ifelse(Origin_EN %in% top_origins_ISP, Origin_EN, "Other"))
summarized_ISP_grouped <- summarized_ISP %>%
  group_by(`學年度 Year`, Origin_Grouped) %>%
  summarise(Total_Current_Students = sum(Total_Current_Students)) %>%
  ungroup()
bar_chart_ISP <- ggplot(summarized_ISP_grouped, aes(x = `學年度 Year`, y = Total_Current_Students, fill = Origin_Grouped)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Total_Current_Students), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "white", 
            fontface = "bold") +
  labs(title = "Top 3 Origins of Current Students in ISP",
       x = "Year", y = "Total Current Students") +
  theme_minimal() 
bar_chart_ISP

# Compare ISP and Non-ISP Students from Vietnam
vietnam_ISP_data <- all_data %>%
  filter(`Origin_EN` == "Vietnam") %>%
  group_by(`學年度 Year`, In_ISP = grepl("產學", `科系名稱 Major`)) %>%
  summarise(Total_Current_Students = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  mutate(In_ISP = ifelse(In_ISP, "Vietnam ISP", "Vietnam Not ISP")) %>%
  ungroup()
vietnam_ISP_comparison_chart <- ggplot(vietnam_ISP_data, aes(x = `學年度 Year`, y = Total_Current_Students, fill = In_ISP)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Total_Current_Students),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold", show.legend = FALSE) + 
  labs(title = "Comparison of Current Students from Vietnam in ISP vs. Not ISP",
       x = "Year", y = "Total Current Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Vietnam ISP" = "blue", "Vietnam Not ISP" = "red"))
vietnam_ISP_comparison_chart


#Q2: Disciplines of International Students
# Identify Top Disciplines by Year
summarized_disciplines <- all_data %>%
  group_by(`學年度 Year`, Discipline) %>%
  summarise(Total_Current_Students = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()
top_disciplines_per_year <- summarized_disciplines %>%
  group_by(`學年度 Year`) %>%
  arrange(desc(Total_Current_Students)) %>%
  slice_head(n = 5) %>%
  ungroup()
all_data_with_top_disciplines <- all_data %>%
  left_join(top_disciplines_per_year, by = c("學年度 Year", "Discipline")) %>%
  mutate(Top_Discipline_Current = ifelse(!is.na(Total_Current_Students), as.character(Discipline), "Other")) %>%
  select(-Total_Current_Students)
summarized_data_top_disciplines <- all_data_with_top_disciplines %>%
  group_by(`學年度 Year`, Top_Discipline_Current) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()

# Create a Line Chart for Top Disciplines
top_disciplines_line_chart <- ggplot(summarized_data_top_disciplines, aes(x = `學年度 Year`, y = total_students_current, color = Top_Discipline_Current)) +
  geom_line() +  
  geom_point(size = 3, shape = 21, fill = "white", color = "black") +  
  geom_text_repel(aes(label = total_students_current), 
                  box.padding = 0.5, 
                  point.padding = 0.3, 
                  segment.color = "grey", 
                  size = 3, 
                  color = "black") +  
  labs(title = "Total Current Students by Top Disciplines + Other (2015-2023)", x = "Year", y = "Total Current Students") +
  theme_minimal() +  
  theme(legend.position = "right") +  
  ylim(0, max(summarized_data_top_disciplines$total_students_current, na.rm = TRUE) * 1.1)  
top_disciplines_line_chart

# Create Pie Chart for 2015
data_2015_top <- all_data_with_top_disciplines %>%
  filter(`學年度 Year` == 2015)
summarized_data_2015 <- data_2015_top %>%
  group_by(Top_Discipline_Current) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()
summarized_data_2015 <- summarized_data_2015 %>%
  arrange(desc(total_students_current))
summarized_data_2015$percentage <- summarized_data_2015$total_students_current / sum(summarized_data_2015$total_students_current) * 100
pie_chart_2015_top <- ggplot(data = summarized_data_2015, aes(x = "", y = percentage, fill = Top_Discipline_Current)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Disciplines of Current Students (2015)", fill = "Discipline") +
  theme_void() +
  geom_text(
    aes(label = ifelse(percentage >= 5, paste0(round(percentage, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  guides(fill = guide_legend(
    title = "Discipline",
    label.position = "right",
    label.hjust = 0,
    keywidth = unit(2, "cm"),
    keyheight = unit(0.8, "cm"), 
    override.aes = list(
      label = paste(summarized_data_2015$Top_Discipline_Current, " (", summarized_data_2015$total_students_current, ")", sep = "")
    )
  ))
pie_chart_2015_top

# Create Pie Chart for 2023
data_2023_top <- all_data_with_top_disciplines %>%
  filter(`學年度 Year` == 2023)
summarized_data_2023 <- data_2023_top %>%
  group_by(Top_Discipline_Current) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()
summarized_data_2023 <- summarized_data_2023 %>%
  arrange(desc(total_students_current))
summarized_data_2023$percentage <- summarized_data_2023$total_students_current / sum(summarized_data_2023$total_students_current) * 100
pie_chart_2023_top <- ggplot(data = summarized_data_2023, aes(x = "", y = percentage, fill = Top_Discipline_Current)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Disciplines of Current Students (2023)", fill = "Discipline") +
  theme_void() +
  geom_text(
    aes(label = ifelse(percentage >= 5, paste0(round(percentage, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white"
  ) +
  guides(fill = guide_legend(
    title = "Discipline",
    label.position = "right",
    label.hjust = 0,
    keywidth = unit(2, "cm"),
    keyheight = unit(0.8, "cm"), 
    override.aes = list(
      label = paste(summarized_data_2023$Top_Discipline_Current, " (", summarized_data_2023$total_students_current, ")", sep = "")
    )
  ))
pie_chart_2023_top

#Q3: Information and Communication Technologies (ICT) and Engineering Students
# Summarize Data for ICT Origins
summarized_origins_ict <- all_data %>%
  filter(Discipline == "Information and Communication Technologies") %>%
  group_by(`學年度 Year`, Origin_EN) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()

# Identify Top 5 Origins for Each Year in ICT
top_origins_per_year_ict <- summarized_origins_ict %>%
  group_by(`學年度 Year`) %>%
  arrange(`學年度 Year`, desc(total_students_current)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Create Line Chart for Top ICT Origins
line_chart_top_origins_ict <- ggplot(top_origins_per_year_ict, aes(x = `學年度 Year`, y = total_students_current, color = Origin_EN)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text_repel(aes(label = total_students_current), 
                  box.padding = 0.5, 
                  point.padding = 0.3, 
                  segment.color = "grey", 
                  size = 3, 
                  color = "black") +
  labs(title = "Top 5 Origins of Current ICT Students (2015-2023)",
       x = "Year",
       y = "Total Current ICT Students",
       color = "Origin") +
  theme_minimal() +
  theme(legend.position = "right")
line_chart_top_origins_ict

# Summarize Data for Engineering Origins
summarized_origins_eng <- all_data %>%
  filter(Discipline == "Engineering and Engineering Trades") %>%
  group_by(`學年度 Year`, Origin_EN) %>%
  summarise(total_students_current = sum(`學生總計 Current Students`, na.rm = TRUE)) %>%
  ungroup()

# Identify Top 5 Origins for Each Year in Engineering
top_origins_per_year_eng <- summarized_origins_eng %>%
  group_by(`學年度 Year`) %>%
  arrange(`學年度 Year`, desc(total_students_current)) %>%
  slice_head(n = 5) %>%
  ungroup()

# Create Line Chart for Top Engineering Origins
line_chart_top_origins_eng <- ggplot(top_origins_per_year_eng, aes(x = `學年度 Year`, y = total_students_current, color = Origin_EN)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text_repel(aes(label = total_students_current), 
                  box.padding = 0.5, 
                  point.padding = 0.3, 
                  segment.color = "grey", 
                  size = 3, 
                  color = "black") +
  labs(title = "Top 5 Origins of Current Engineering Students (2015-2023)",
       x = "Year",
       y = "Total Current Engineering Students",
       color = "Origin") +
  theme_minimal() +
  theme(legend.position = "right")
line_chart_top_origins_eng

# Create Gender Ratio Charts for ICT
summarized_data_ict_gender <- all_data %>%
  filter(Discipline == "Information and Communication Technologies") %>%
  group_by(`學年度 Year`) %>%
  summarise(Male_Students = sum(`男學生 Male Students`, na.rm = TRUE),
            Female_Students = sum(`女學生 Female Students`, na.rm = TRUE)) %>%
  ungroup()
summarized_data_ict_gender_long <- summarized_data_ict_gender %>%
  pivot_longer(cols = c(Male_Students, Female_Students),
               names_to = "Gender",
               values_to = "Number of Students")
ict_gender_ratio_chart <- ggplot(summarized_data_ict_gender_long, aes(x = `學年度 Year`, y = `Number of Students`, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Ratio in Information and Communication Technologies (ICT)",
       x = "Year", y = "Number of Students") +
  scale_fill_manual(values = c("Male_Students" = "blue", "Female_Students" = "red"),
                    labels = c("Female Students", "Male Students")) +
  theme_minimal()
ict_gender_ratio_chart

# Create Gender Ratio Charts for Engineering
summarized_data_engineering_gender <- all_data %>%
  filter(Discipline == "Engineering and Engineering Trades") %>%
  group_by(`學年度 Year`) %>%
  summarise(Male_Students = sum(`男學生 Male Students`, na.rm = TRUE),
            Female_Students = sum(`女學生 Female Students`, na.rm = TRUE)) %>%
  ungroup()
summarized_data_engineering_gender_long <- summarized_data_engineering_gender %>%
  pivot_longer(cols = c(Male_Students, Female_Students),
               names_to = "Gender",
               values_to = "Number of Students")
engineering_gender_ratio_chart <- ggplot(summarized_data_engineering_gender_long, aes(x = `學年度 Year`, y = `Number of Students`, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender Ratio in Engineering and Engineering Trades",
       x = "Year", y = "Number of Students") +
  scale_fill_manual(values = c("Male_Students" = "blue", "Female_Students" = "red"),
                    labels = c("Female Students", "Male Students")) +
  theme_minimal()
engineering_gender_ratio_chart


