library(dplyr)
library(tidyr)
library(stringr)

df<-read.csv("comp.csv")
selected_countries <- c("MDV", "MUS", "LKA")


df_filtered <- comp %>%
  filter(`Country.Code` %in% selected_countries) %>%
  select(`Country.Name`, `Country.Code`, `Series.Name`, `Series.Code`,
         `X2022..YR2022.`, `X2023..YR2023.`)
df_filtered <- df_filtered %>%
  mutate(across(starts_with("X202"), as.numeric))

# 将数据从宽格式转为长格式，并提取年份信息
df_long <- df_filtered %>%
  pivot_longer(
    cols = starts_with("X202"),
    names_to = "year",
    values_to = "value"
  ) %>%
  # 提取year列中的4位年份
  mutate(year = as.numeric(str_extract(year, "\\d{4}")))

write_csv(df_long,"comp2.csv")
