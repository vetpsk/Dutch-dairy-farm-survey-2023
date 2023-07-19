library(dplyr)
library(skimr)

merged <- read.csv("2_merged_data_survey_PK.csv", header = T)

attach(merged)
table(age_group_farmer)
table(type_farm)
summary(hectares)
table(ams_cms)
table(as.factor(replacement_stock_type))
table(alva)
summary(milk305)
detach(merged)

herdage = merged$herd_age_years*12 + merged$herd_age_months

summary(herdage)
attach(merged)
table(as.factor(ir_data))
table(ir_data == 1 & is.na(UBN) == F)
summary(herdsize)
summary(heifers)
summary(calves_1year)
summary(purchase_heifers)


detach(merged)
intervals <- cut(merged$purchase_heifers, breaks = 5)
table(intervals)

attach(merged)
table(decision_responsible)
table(recent_primi_remember)
table(recent_primi_decision_time)
table(recent_primi_unforseen)

table(recent_multi_remember)
table(recent_multi_decision_time)
table(recent_multi_unforseen)
summary(n_culled)


detach(merged)
cull_rate  = merged$n_culled/ (merged$herdsize + merged$n_culled)

summary(cull_rate)
intervals <- cut(cull_rate, breaks = c(0.10517, 0.13961, 0.17348, 0.5))
table(intervals)

merged <- read.csv("3_merged_data_survey_PK.csv", header = T, stringsAsFactors = T)

merged <- merged[merged$n_culled <= 1000,]
merged <- merged[merged$milk305 <= 50000,]

data <- merged %>% 
  mutate(replacement_stock_type = as.factor(replacement_stock_type),
         herd_age_months = (herd_age_years*12) + herd_age_months,
         cull_rate = n_culled/(n_culled + herdsize)
         ) %>%
  select(ResponseId, age_group_farmer, type_farm,
         hectares, ams_cms, replacement_stock_type,
         alva, milk305, herd_age_months,
         herdsize, calves_1year, purchase_heifers,
         decision_responsible, recent_primi_remember,
         recent_primi_decision_time, recent_primi_unforseen,
         recent_multi_remember,
         recent_multi_parity,
         recent_multi_decision_time,
         recent_multi_unforseen,
         statement_1:statement_4,
         intention_1:intention_3,
         intention_4,n_culled, cull_rate
  ) %>%
  mutate(recent_primi_remember = as.factor(recent_primi_remember),
         recent_primi_unforseen = as.factor(recent_primi_unforseen),
         recent_multi_remember = as.factor(recent_multi_remember),
         recent_multi_unforseen = as.factor(recent_multi_unforseen),
         intention_1 = as.factor(intention_1),
         intention_2 = as.factor(intention_2),
         intention_3 = as.factor(intention_3),
         intention_4 = as.factor(intention_4)
  )
  
a <- skim_without_charts(data)
a <- partition(a)


a[[2]][,3:10] <- apply(a[[2]][,3:10], 2, round,2)

View(a[[2]])
