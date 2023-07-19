library(readxl)
library(dplyr)

IR_calvings <- read_excel("IR_calvings.xlsx", 
                       col_types = c("text", "text", "date", "text"))
View(IR_calvings)

colnames(IR_calvings) <- c("CowId", "CalfId", "calv_date", "Sex_calf")

IR_data <- read_excel("./IR_data.xlsx", 
                      col_types = c("numeric", "text", "numeric", 
                            "numeric", "date", "date", "text", 
                            "text", "text", "text", "date"))
colnames(IR_data) <- c("UBN", "CowId", "StartCode", "EndCode",
                       "Start_date", "End_date", "Q_start", "Start_year",
                       "Q_end", "Sex", "Birth_date")

IR_data$age_days <- difftime("2021-12-31", IR_data$Birth_date, unit = "days")
IR_data$age <- IR_data$age_days/30.5


IR_herd_calves <- IR_data %>%
  group_by(UBN) %>%
  filter(is.na(EndCode) == T) %>%
  filter(age <= 12 & Sex == "F" & StartCode == 0) %>%
  summarise(female_calves = n_distinct(CowId))

IR_heifers <- IR_data %>% 
  select(UBN:EndCode, Sex:age) %>%
  left_join(., IR_calvings, by = "CowId") %>%
  filter(is.na(calv_date) == T) %>%
  filter(Sex == "F") %>%
  filter(is.na(EndCode) == T) %>%
  filter(age < 24 & age > 12)

IR_herd_heifers_tot <- IR_heifers %>%
  group_by(UBN) %>%
  summarise(heif_tot = n_distinct(CowId))

IR_herd_heifers_purchased <- IR_heifers %>%
  group_by(UBN) %>%
  filter(StartCode != 0) %>%
  summarise (heif_purchased = n_distinct(CowId))

IR_herd_heifers <- IR_herd_heifers_tot %>% 
  left_join(., IR_herd_heifers_purchased, by = "UBN")

IR_herd_heifers$heif_purchased[is.na(IR_herd_heifers$heif_purchased) == T] <- 0

IR_herd_data <- IR_herd_calves %>% left_join(., IR_herd_heifers, by = "UBN")

rm(IR_heifers, IR_herd_calves, IR_herd_heifers, IR_herd_heifers_purchased, IR_herd_heifers_tot)

IR_cows <- IR_data %>% 
  select(UBN:EndCode, Sex:age) %>%
  left_join(., IR_calvings, by = "CowId") %>%
  filter(is.na(calv_date) == F) %>%
  filter(is.na(EndCode) == T)

IR_herd_cows <- IR_cows %>% group_by(UBN) %>%
  summarise(cows_tot = n_distinct(CowId))

IR_herd_data <- full_join(IR_herd_data, IR_herd_cows,by = "UBN")


Anouk_IR_hsize <- read_excel("./Anouk_IR_hsize.xlsx")

IR_ref <- Anouk_IR_hsize %>%
  filter(year_quarter == 2104)

IR_herd <- read.csv("./IR_herd_summary.csv", header = T)

IR_check <-IR_ref %>% select(UBN, akalf, ajongvee, avolw, totaal, ndieraanv) %>%
  left_join(., IR_herd, by = "UBN")
