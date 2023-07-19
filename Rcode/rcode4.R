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


df <- read.csv("3_merged_data_survey_PK.csv")

freqdf <- read.csv("frequent reasons.csv")

df <- df %>% left_join(., freqdf, by = "ResponseId")

df2 <- df %>% filter(hectares > 10 | is.na(hectares) == T) %>% 
  filter(milk305 %in% 4000:15000| is.na(milk305) == T)

df2 %>% mutate_at(vars(most_Reproduction:least_reduce_herdsize), as.factor) -> df2

df2 %>% select(-c(least_Reproduction:least_reduce_herdsize)) %>%
  tidyr::pivot_longer(., cols = most_Reproduction:most_reduce_herdsize, names_to = "reasons",
                      values_to = "rank") %>% 
  filter(rank == 1 & n_culled <= 536) %>%
  mutate (reasons = as.character(reasons)) %>% 
  mutate(reasons = gsub("most_", "", reasons)) %>% 
  mutate(reasons = as.factor(reasons),
         intensity = ifelse(is.na(hectares) == F, herdsize/hectares, NA),
         cull_rate = n_culled/herdsize) -> df3

df3 %>% group_by(reasons) %>%
  summarise(n_farms = n_distinct(ResponseId),
            mean_herdsize = mean(herdsize, na.rm = T),
            sd_herdsize = sd(herdsize, na.rm = T),
            mean_heifers = mean(heifers, na.rm = T),
            sd_heifers = sd(heifers, na.rm = T),
            mean_cull_rate = mean(cull_rate, na.rm = T),
            sd_cull_rate = sd(cull_rate, na.rm = T),
            mean_intensity = mean(intensity, na.rm = T),
            sd_intensity = sd(intensity, na.rm = T),
            mean_female_calves = mean(calves_1year, na.rm = T),
            sd_female_calves = sd(calves_1year, na.rm = T),
            mean_milk305 = mean(milk305, na.rm = T),
            sd_milk305 = sd(milk305, na.rm = T)) -> num_summary

df3 %>% select(ResponseId, reasons, statement_1:statement_4) %>%
  tidyr::drop_na(reasons, statement_1:statement_4) %>% 
  ggplot(., aes(x = statement_1, fill = reasons)) + 
  stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), col = "black", show.legend = F) + 
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") +
  coord_flip() +  theme_bw()-> s1
df3 %>% select(ResponseId, reasons, statement_1:statement_4) %>%
  tidyr::drop_na(reasons, statement_1:statement_4) %>% 
  ggplot(., aes(x = statement_2, fill = reasons)) + 
  stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), col = "black", show.legend = F) + 
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") +
  coord_flip() +  theme_bw()-> s2
df3 %>% select(ResponseId, reasons, statement_1:statement_4) %>%
  tidyr::drop_na(reasons, statement_1:statement_4) %>% 
  ggplot(., aes(x = statement_3, fill = reasons)) + 
  stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), col = "black", show.legend = F) + 
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") +
  coord_flip() +  theme_bw()-> s3
df3 %>% select(ResponseId, reasons, statement_1:statement_4) %>%
  tidyr::drop_na(reasons, statement_1:statement_4) %>% 
  ggplot(., aes(x = statement_4, fill = reasons)) + 
  stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), col = "black", show.legend = F) + 
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") +
  coord_flip() +  theme_bw()-> s4

ggpubr::ggarrange(s1, s2, s3, s4,
                  labels = c(1:4),
                  ncol= 1, nrow = 4, common.legend = T)
num_summary %>% mutate_if(is.numeric, round, 2) -> num_summary
ggplot(df3, aes(x = reasons, y = cull_rate, fill = reasons)) + 
  geom_boxplot(outlier.shape = NA, outlier.color = "white", col = "black", na.rm = T) + 
  geom_jitter(size = 0.5,na.rm = T) + 
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") + 
  coord_flip() + theme_bw() + labs(fill = "most frequent culling reason")



df3 %>% select(ResponseId, reasons, intention_1:intention_3, intention_4) %>%
  mutate(intention_1 = factor(intention_1, levels = 1:3, labels = c("cull primiparous cows quicker",
                                                                    "cull primiparous cows less quick",
                                                                    "no change to culling strategy")),
         intention_2 = factor(intention_2, levels = 1:3, labels = c("cull the multiparous cows quicker",
                                                                    "cull the multiparous cows less quick",
                                                                    "no change to culling strategy")),
         intention_3 = factor(intention_3, levels = 1:3, labels = c("increase the % culled cows",
                                                                    "decrease the % culled cows",
                                                                    "don't alter %")),
         intention_4 = factor(intention_4, levels = 1:3, 
                              labels = c("increase the replacement stock",
                                         "decrease the replacement stock",
                                         "don't alter this amount"))) %>% 
  tidyr::drop_na(reasons, intention_1:intention_4) -> freqdf
  

ggplot(freqdf, aes(x = intention_1, fill = reasons)) + stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), show.legend = F) +
  scale_x_discrete(drop=F, labels = c("cull primiparous \n cows quicker",
                                      "cull primiparous cows \n less quick",
                                      "no change to \n culling strategy")) +
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") + labs(fill = "most frequent \n culling reason") +
  coord_flip() +
  theme_bw() -> s1
ggplot(freqdf, aes(x = intention_2, fill = reasons)) + stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), show.legend = F) +
  scale_x_discrete(drop=F, labels = c("cull the multiparous \n cows quicker",
                                      "cull the multiparous \n cows less quick",
                                      "no change to \n culling strategy")) +
  scale_fill_brewer(type = "qual", palette = 3) + xlab("")  + labs(fill = "most frequent \n culling reason") +
  coord_flip() +
  theme_bw() -> s2
ggplot(freqdf, aes(x = intention_3, fill = reasons)) + stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), show.legend = F) +
  scale_x_discrete(drop=F, labels = c("increase the \n % culled cows",
                                      "decrease the \n % culled cows",
                                      "don't alter %")) +
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") + labs(fill = "most frequent \n culling reason") +
  coord_flip() +
  theme_bw() -> s3
ggplot(freqdf, aes(x = intention_4, fill = reasons)) + stat_count(geom = "bar", col = "black") +
  stat_count(geom = "label", aes(label = ..count..), show.legend = F) +
  scale_x_discrete(drop=F,labels = c("increase the \n replacement stock",
                                     "decrease the \n replacement stock",
                                     "don't alter \n this amount") ) +
  scale_fill_brewer(type = "qual", palette = 3) + xlab("") + labs(fill = "most frequent \n culling reason") +
  coord_flip() +
  theme_bw() -> s4


ggpubr::ggarrange(s1, s2, s3, s4,
                  labels = c(1:4),
                  ncol= 1, nrow = 4, common.legend = T)
