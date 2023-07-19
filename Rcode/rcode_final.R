library(tidyverse)
library(likert)

df <- read.csv("4_merged_data_survey_PK.csv", header = T)
df %>% mutate(intensity = herdsize/hectares,
              herd_age_months = (herd_age_years*12) + herd_age_months) %>%
  select(-herd_age_years) -> df
df$n_culled[df$n_culled > 500] <- NA
df$cull_rate <- df$n_culled/df$herdsize
df <- df %>% filter(intensity < 7 | is.na(intensity) == T)

h1 <- ggplot(df) + 
  geom_histogram(mapping = aes(x = hectares), fill = "brown", col = "black",
                 bins = 10, na.rm = T, show.legend = T) +
  labs(x = "Agricultural land in hectares", y = "number of respondents",
       caption = "(n = 110)") +
  scale_fill_manual(labels = "area", values = "brown") + theme_classic() +
  theme(text = element_text(size = 15))

h2 <- ggplot(df) + 
  geom_histogram(mapping = aes(x = milk305), fill = "purple1", col = "black",
                 bins = 12) + 
  labs(x = "mean 305-day milk (kg)", y = "number of respondents",
       caption = "(n = 112)") + 
  scale_fill_manual(labels = "milk_305", values = "purple1") + theme_classic()+
  theme(text = element_text(size = 15))
h2

h3 <- ggplot(df[df$intensity < 10,]) + 
  geom_histogram(mapping = aes(x = intensity), fill = "darkgreen", col = "black",
                 binwidth = 0.5) + 
  labs(x = "number of animals per hectare (intensity)", y = "number of respondents",
       caption = "(n = 103)") + 
  scale_fill_manual(labels = "intensity", values = "darkgreen") + 
  scale_x_continuous(breaks = seq(0, 6.0, 0.5), labels = waiver()) +
  theme_classic()+
  theme(text = element_text(size = 15))
h3

h4 <- ggplot(df) + 
  geom_histogram(mapping = aes(x = herd_age_months), fill = "orchid", col = "black",
                 binwidth = 10) + 
  labs(x = "mean herd age in months", y = "number of respondents",
       caption = "(n = 112)") + 
  scale_fill_manual(labels = "herd_age_months", values = "orchid") + 
  scale_x_continuous(breaks = seq(30, 100, 10), labels = seq(30, 100, 10)) +
  theme_classic() +
  theme(text = element_text(size = 15))
h4
ggpubr::ggarrange(h1,h2,h3,h4, ncol = 2, nrow = 2, 
                  labels = c("A. Land Area", "B. 305-Milk", "C. Intensity", "D. Mean herd age"),
                  font.label = list(size = 10, face = "bold"), hjust = -0.7, common.legend = T)

rm(h1, h2, h3, h4)

df_primi <- df %>% select(ResponseId, recent_primi_remember:primi_reason_3, 
                          recent_primi_decision_time:recent_primi_unforseen) %>%
  pivot_longer(., cols = primi_reason_1:primi_reason_3, names_to = "rank", values_to = "reason")

table(df$recent_primi_unforseen)
table(df_primi$reason)

df_multi <- df %>% select(ResponseId, recent_multi_remember:multi_reason_4, 
                          recent_multi_decision_time, recent_multi_unforseen) %>%
  pivot_longer(., cols = multi_reason_1:multi_reason_4, names_to = "rank", values_to = "reason")


df_freq1 <- df %>% select(ResponseId, most_Reproduction:most_reduce_herdsize) %>%
  pivot_longer(., cols = most_Reproduction:most_reduce_herdsize, names_to = "most_name", values_to = "most_rank")
df_freq2 <- df %>% select(ResponseId, least_Reproduction:least_reduce_herdsize) %>%
  pivot_longer(., cols = least_Reproduction:least_reduce_herdsize, names_to = "least_name", values_to = "least_rank")

df_freq <- left_join(df_freq1, df_freq2, by = c("ResponseId"))
rm(df_freq2, df_freq1)

df_freq <- df_freq %>% drop_na(most_rank, least_rank)
df_freq$most_name <- gsub("most_", "", df_freq$most_name)
df_freq$least_name <- gsub("least_", "", df_freq$least_name)

df_freq <- df_freq %>% mutate_all(as.factor)
df_freq %>% group_by(most_name, most_rank) %>% summarise(n = n_distinct(ResponseId)) %>%
  ggplot(aes(x = most_name, y =n, fill = most_rank)) +
    geom_col(col = "black", position = "stack", width = 0.5) +
    labs(x = "most frequent culling reasons", y = "number of respondents",
         fill = "Rank of given reason") +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 20,vjust = 0.5, size = 10),
          legend.position = "bottom", axis.title = element_text(size = 15)) -> f1

df_freq %>% group_by(least_name, least_rank) %>% summarise(n = n_distinct(ResponseId)) %>%
  ggplot(aes(x = least_name, y = n, fill = least_rank)) +
    geom_col(col = "black", position = "stack", width = 0.5) +
    labs(x = "least frequent culling reasons", y = "number of respondents",
         fill = "Rank of given reason") +
    scale_fill_manual(values = c("beige", "darkturquoise", "grey54")) +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 20,vjust = 0.5, size = 10),
          legend.position = "bottom",axis.title = element_text(size = 15)) -> f2


ggpubr::ggarrange(f1, f2, ncol = 2, nrow = 1, 
                  labels = c("1. 3-most frequent culling reasons", 
                             "2. 3-least frequent culling reasons"),
                  font.label = list(size = 11),
                  hjust = -0.5) -> f
ggpubr::annotate_figure(f, bottom = "(N = 130)")

rm(f, f1, f2, df_freq)


df_stat <- df %>% select(statement_1:statement_4)

statfunc <- function(x) factor(x, levels = c("totally disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "totally agree"), ordered = T)
df_stat <- df_stat %>% mutate_all(., statfunc)

mycor <- Hmisc::rcorr(as.matrix(df_stat), type = "spearman")
View(mycor$r)

df_stat$statement_1 <- factor(df_stat$statement_1, levels = c("totally disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "totally agree"),
                              ordered = T)            
df_stat$statement_2 <- factor(df_stat$statement_2, levels = c("totally disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "totally agree"),
                              ordered = T)  
df_stat$statement_3 <- factor(df_stat$statement_3, levels = c("totally disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "totally agree"),
                              ordered = T)  
df_stat$statement_4 <- factor(df_stat$statement_4, levels = c("totally disagree", "somewhat disagree", "neither agree nor disagree", "somewhat agree", "totally agree"),
                              ordered = T)  

stat <- likert(df_stat)

stat[["results"]][1]

a <- plot(stat, type = "bar", group.order = c("statement_1", "statement_2", "statement_3", "statement_4")) +
  scale_y_discrete(labels = c("I have a clear long-term \n culling plan on my farm",
                               "I consider the culling \n strategy on my farm to \n be optimal",
                              "The culling decisions \n I make are unavoidable",
                              "When deciding to cull a cow, \n I follow specific \n rules of thumb/ guidelines")) +
  theme(text = element_text(size = 5), legend.text = element_text(size = 5),
        axis.text = element_text(size = 7.5), axis.title = element_text(size = 7.5),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))
a$layers[[4]]$aes_params$size = 2
a$layers[[5]]$aes_params$size = 2
a$layers[[6]]$aes_params$size = 2
a
a + guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(axis.line = element_line(colour = "black", size = 1))-> a
rm(a)
df_stat <- df %>% select(ResponseId, statement_1:statement_4) %>%
  pivot_longer(., -ResponseId, names_to = "statements", values_to = "likert") %>%
  drop_na(likert) %>%
  group_by(statements, likert) %>% summarise(n = n_distinct(ResponseId)) %>%
  ungroup() %>% group_by(statements) %>% mutate(n_tot = sum(n)) %>%
  mutate(perc = n*100/n_tot)

df_stat1 <- df %>% 
  select(ResponseId, primi_plan1:primi_plan3, multi_plan1:multi_plan4) %>%
  pivot_longer(.,-ResponseId, names_to = "plan", values_to = "criteria") %>%
  drop_na(criteria)

df_stat1$criteria <- as.character(df_stat1$criteria)
df_stat1$criteria[df_stat1$criteria == "Dezelfde criteria als voor eerste kalfskoeien"] <- "Same as primiparous"
df_stat1$criteria[df_stat1$criteria == ""] <- NA
df_stat1 <- drop_na(df_stat1, criteria)
df_stat1$criteria <- as.factor(df_stat1$criteria) #n = 85

df_stat1$plan <- substr(df_stat1$plan,start = 1, stop = 10)
df_stat1$plan <- factor(df_stat1$plan, levels = c("primi_plan", "multi_plan"))

df_stat1 %>% group_by(plan, criteria) %>% mutate(n = n_distinct(ResponseId))

ggplot(df_stat1, aes(y = criteria, fill = plan)) +
  geom_bar(col = "black", position = position_dodge2(preserve = "single", 
                                                     reverse = T)) +
  scale_fill_manual(values = c("beige", "darkturquoise")) +
  scale_x_continuous(breaks = seq(0, 50, 10), labels = seq(0, 50, 10)) +
  scale_y_discrete(limits = rev(levels(df_stat1$criteria))) +
  labs(x = "number of responses", y = "Criteria for specific rule of thumb",
       fill = "Sub-group of cows \n under the rule",
       caption = "(N = 85)") + 
  theme_classic() + theme(text = element_text(size = 15))

s2
rm(df_stat, df_stat1, s1, s2)

df_int <- df %>% select(ResponseId, motivation_1:motivation_4) %>%
  pivot_longer(., cols = -ResponseId, names_to = "Rank", values_to = "Motivations") %>%
  drop_na(Motivations) %>%
  group_by(Motivations) %>% summarise(n = n_distinct(ResponseId)) %>%
  ungroup() %>% mutate(n_tot = sum(n),perc = round(n*100/n_tot,2)) %>%
  mutate(Motivations = factor(Motivations, 
                              levels = c("improve economic result",
                                         "improve longevity",
                                         "improve environmental sustainability",
                                         "management that matched my vision",
                                         "other")))

View(df_int)
rm(df_int)

df %>% select(ResponseId, age_group_farmer, type_farm,
              intensity, ams_cms, replacement_stock_type,
              alva, milk305, herd_age_months, herdsize,
              heifers,calves_1year, purchase_heifers, decision_responsible, n_culled, cull_rate) %>%
  skimr::skim_without_charts()

df_ams <- df %>% drop_na(ams_cms)
ggplot(df_ams, aes(y = ams_cms, x = herdsize, fill = ams_cms)) +
  geom_boxplot(col = "black", outlier.shape = NA, ) +
  geom_jitter(size = 0.5, shape = 2) +
  labs(x = "number of producing cows (herdsize)", y = "Type of milking system",
       fill = "Type of milking system",
       caption = "(N = 108)") +
  scale_fill_manual(values = c("khaki", "turquoise1")) +
  theme_classic() -> a1

ggplot(df_ams, aes(y = ams_cms, x = cull_rate, fill = ams_cms)) +
  geom_boxplot(col = "black", outlier.shape = NA, ) +
  geom_jitter(width = 0.2,size = 0.5, shape = 2) +
  labs(x = "culling rate (%/100)", y = "Type of milking system",
       fill = "Type of milking system",
       caption = "(N = 108)") +
  scale_fill_manual(values = c("khaki", "turquoise1")) +
  theme_classic() -> a2

ggplot(df_ams, aes(y = ams_cms, x = hectares, fill = ams_cms)) +
  geom_boxplot(col = "black", outlier.shape = NA, ) +
  geom_jitter(width = 0.2,size = 0.5, shape = 2) +
  labs(x = "Area of farm (hectares)", y = "Type of milking system",
       fill = "Type of milking system",
       caption = "(N = 110)") +
  scale_fill_manual(values = c("khaki", "turquoise1")) +
  theme_classic() -> a3

ggplot(df_ams, aes(y = ams_cms, x = herd_age_months, fill = ams_cms)) +
  geom_boxplot(col = "black", outlier.shape = NA, ) +
  geom_jitter(width = 0.2,size = 0.5, shape = 2) +
  labs(x = "Avg. age of herd (in months)", y = "Type of milking system",
       fill = "Type of milking system",
       caption = "(N = 112)") +
  scale_fill_manual(values = c("khaki", "turquoise1")) +
  theme_classic() -> a4

df_ams %>% drop_na(alva) %>% ggplot(., aes(x = alva, fill = ams_cms)) +
  geom_bar(col = "black", position = position_dodge(preserve = "single"))+
  labs(x = "Avg. age at 1st calving", y = "Number of responses",
       fill = "Type of milking system",
       caption = "(N = 115)") +
  scale_fill_manual(values = c("khaki", "turquoise1")) +
  theme_classic() -> a5

ggplot(df_ams, aes(x = milk305, y = ams_cms, fill = ams_cms)) +
  geom_boxplot(col = "black", outlier.shape = NA, ) +
  geom_jitter(width = 0.2,size = 0.5, shape = 2) +
  labs(x = "Avg. 305-day milk (kg)", y = "Type of milking system",
       fill = "Type of milking system",
       caption = "(N = 112)") +
  scale_fill_manual(values = c("khaki", "turquoise1")) +
  scale_x_continuous(breaks = seq(5000, 15000, 1000), labels = waiver()) +
  theme_classic() -> a6

ggpubr::ggarrange(a1,a2,a3,a4,a5,a6, ncol = 3, nrow = 2, labels = paste0(seq(1,6,1), "."),
                  common.legend = T, hjust = -1, vjust = -0.5)

rm(a1, a2, a3, a4, a5, a6)
df %>% drop_na(herdsize) %>% filter(cull_rate <= 0.75 & intensity <= 7) %>%
  ggplot(., aes(x = intensity, y = cull_rate, shape = cut_number(herdsize, 5))) + 
  geom_point(size = 2, col = "darkolivegreen") +
  geom_label(aes(x = 5, y = 0.6, label = "corr = -0.18 \n cov = -0.02"), size = 3,
            label.padding = unit(0.25, "lines")) +
  labs(x = "Intensity (herd size / area in num/hectares)",
       y = "Culling rate (%/100)",
       caption = "(N = 102)",
       shape = "Groups of herd sizes \n(number of producing cows)") + 
  scale_shape_manual(values = 1:5, labels = paste(c("31-80", "81-110", "111-140","141-210", "210-370"), "cows", sep = " ")) +
  theme_classic() -> a1

df %>% filter(cull_rate <= 0.75) %>%
  select(ResponseId, replacement_stock_type, cull_rate, intensity) %>%
  drop_na(replacement_stock_type, cull_rate) %>%
  mutate(replacement_stock_type = factor(replacement_stock_type, levels = 1:5)) %>%
  ggplot(., aes(fill = replacement_stock_type)) + 
  geom_boxplot(aes(y = cull_rate, x= replacement_stock_type), col = "black", outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0.0, 0.5, 0.05), labels = waiver(), limits = c(0,0.5)) +
  scale_fill_manual(values = c("orangered","steelblue", "darkgreen",  "lightblue", "lightyellow1"),
                    labels = c("1=Yes, I breed and rear \n my own replacement stock",
                               "2=Yes, I breed my own \n replacement stock, \n however I also purchase \n replacement animals",
                               "3=No, my own bred \n replacement stock is \n reared on another location \n by someone else and \n will be back on my \n own farm as heifers",
                               "4=No, my own bred \n replacement stock is \n reared by someone else, \n and I purchase replacement \n animals",
                               "5=No, I purchase all \n my replacement stock")) +
  labs(x = "Type of replacement stock", y = "Culling rate (%/100)",
       fill = "Type of replacement stock",
       caption = "(N = 104) \n *outliers not plotted") + theme_classic() -> a2

ggpubr::ggarrange(a1, a2, labels = paste0(1:2, "."),
                  nrow = 2, ncol = 1)

rm(df_ams, a1, a2)

df %>% drop_na(age_group_farmer, decision_responsible) %>%
  mutate(age_group_farmer = factor(age_group_farmer, 
                                   levels = c("18_35", "36_55", ">55")),
                                   decision_responsible = factor(decision_responsible, 
                                                                 levels = c("Yes", "Partly", "No"))) %>%
  ggplot(., aes(x = age_group_farmer, fill = decision_responsible)) + 
  geom_bar(col = 'black', width = 0.5, position = "dodge") +
  scale_fill_manual(values = c("red", "purple", "white"),
                    labels = c("Yes", "Partly", "No"),
                    drop = F) +
  scale_x_discrete(labels = c("18-35 years", "36-55 years", ">55 years")) +
  scale_y_continuous(breaks = seq(0, 50, 5), labels = waiver()) +
  labs(x = "Age group of responding farmers",
       y = "Number of responses",
       fill = "Responsible for decision \n (Yes/ No)?") +
  theme_classic()


rm(list = setdiff(ls(), "df"))


a1 <- df %>% select(ResponseId, cull_rate, statement_1:statement_4) %>%
  pivot_longer(cols = statement_1:statement_4, names_to = "stat", values_to = "resp") %>%
  mutate(stat = as.factor(stat)) %>% drop_na(resp) %>%
  ggplot(., aes(fill = stat,x = resp,  y = cull_rate)) + 
  geom_boxplot(col = "black", outlier.shape = NA) +
  labs(x = "Response to statements", y = "Reported culling rate (%/100)",
       fill = "Statements", caption = "(N = 122, n = 479)") +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_fill_discrete(labels = c("When deciding to cull a \n cow, I follow specific rules \n of thumb/guidelines",
                                 "The culling decisions I \n make, are unavoidable",
                                 "I consider the culling \n strategy on my farm to be \n optimal",
                                 "I have a clear long-term \n culling plan on my farm")) +
  theme_classic()
a1
a1 + theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))
a2 <- df %>% select(ResponseId, cull_rate, intention_1:intention_3, intention_4) %>%
  pivot_longer(cols = intention_1:intention_4, names_to = "stat", values_to = "resp") %>%
  mutate(stat = factor(stat, levels = c("intention_1", "intention_2", "intention_3", "intention_4")),
         resp = factor(resp, levels = 1:3)) %>% 
  drop_na(resp) %>%
  ggplot(., aes(fill = stat,x = resp,  y = cull_rate)) + 
  geom_boxplot(col = "black", outlier.shape = NA, position = position_dodge2(preserve = "single")) +
  labs(x = "Response to Intention statements", y = "Reported culling rate (%/100)",
       fill = "Future intention \n Statements", caption = "(N = 116, n = 461)") +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_fill_discrete(labels = c("I intend to alter my culling \n strategy for primiparous cows",
                                 "I intend to alter my culling \n strategy for multiparous cows",
                                 "I intend to alter the \n percentage of culled cows \n in next year",
                                 "I intend to alter the \n amount of replacement stock \n in next year")) +
  scale_x_discrete(labels = c("less", "more", "no change")) +
  theme_classic()
a2
rm(a1,a2)

df_chi <- df %>% select(statement_1:statement_4, intention_1:intention_3, intention_4) %>% drop_na(.) %>%
  mutate(across(statement_1:statement_4, statfunc)) %>% mutate(across(intention_1:intention_4, as.factor)) 

chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num,ncol=num,dimnames=list(names,names))
  for (i in 1:(num-1)) {
    for (j in (i+1):num) {
      #browser()
      m[j,i] = paste(round(chisq.test(x[, i, drop = TRUE],x[, j, drop = TRUE])$statistic,2), "(",
                     round(chisq.test(x[, i, drop = TRUE],x[, j, drop = TRUE])$p.value,4), ")")
    }
  }
  return (m)
}

mat <- chisqmatrix(df_chi)
mat[-1, -ncol(mat)]


df_num <- df %>% 
  select(hectares, milk305, herd_age_months, 
         herdsize, heifers, calves_1year, n_culled, 
         intensity, cull_rate) %>% drop_na()

shapiro.test(df_num$hectares)
shapiro.test(df_num$milk305)
shapiro.test(df_num$herd_age_months)
shapiro.test(df_num$herdsize)
shapiro.test(df_num$heifers)
shapiro.test(df_num$calves_1year)
shapiro.test(df_num$n_culled)
shapiro.test(df_num$intensity)

rm(df_num)

wilcox.test(herdsize ~ ams_cms, data = df, na.rm = T)
wilcox.test(herd_age_months ~ ams_cms, data = df, na.rm = T)
wilcox.test(heifers ~ ams_cms, data = df, na.rm = T)
wilcox.test(calves_1year ~ ams_cms, data = df, na.rm = T)
wilcox.test(cull_rate ~ ams_cms, data = df, na.rm = T)
wilcox.test(intensity ~ ams_cms, data = df, na.rm = T)
wilcox.test(hectares ~ ams_cms, data = df, na.rm = T)
wilcox.test(milk305 ~ ams_cms, data = df, na.rm = T)

kruskal.test(herdsize ~ ams_cms, data = df)

rankt <- function(x) {
  stat1 <- kruskal.test(x ~ statement_1, data = df)
  stat2 <- kruskal.test(x ~ statement_2, data = df)
  stat3 <- kruskal.test(x ~ statement_3, data = df)
  stat4 <- kruskal.test(x ~ statement_4, data = df)
  int1 <- kruskal.test(x ~ intention_1, data = df)
  int2 <- kruskal.test(x ~ intention_2, data = df)
  int3 <- kruskal.test(x ~ intention_3, data = df)
  int4 <- kruskal.test(x ~ intention_4, data = df)
  ranktest <- list(stat1, stat2, stat3, stat4, int1, int2, int3, int4)
  return(ranktest)
}

sink(file = "./../kruskaltest.txt")
rankt(df$herdsize)
rankt(df$hectares)
rankt(df$milk305)
rankt(df$heifers)
rankt(df$herd_age_months)
rankt(df$intensity)
rankt(df$cull_rate)
rankt(df$calves_1year)
sink()


df_freq1 <- df %>% select(ResponseId, most_Reproduction:most_reduce_herdsize) %>%
  pivot_longer(., cols = most_Reproduction:most_reduce_herdsize, 
               names_to = "most_name", values_to = "most_rank") %>% drop_na(most_rank) %>%
  mutate(most_name = gsub("most_", "", most_name)) %>%
  left_join(select(df, ResponseId, hectares, milk305, herd_age_months, 
                   herdsize, heifers, calves_1year, n_culled, 
                   intensity, cull_rate), by = "ResponseId")

df_freq1$most_name <- as.factor(df_freq1$most_name)
sink(file = "./../Kruskaltest_freq.txt")

kruskal.test(hectares~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(herdsize~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(intensity~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(cull_rate~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(heifers~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(milk305~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(calves_1year~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)
kruskal.test(herd_age_months~most_name, data = df_freq1, subset = df_freq1$most_rank == 1)

sink()

df_freq1 <- df_freq1[df_freq1$most_rank == 1,c(1:3)]

df_freq1 <- df_freq1 %>% 
  left_join(select(df, ResponseId, statement_1:statement_4, 
                   intention_1:intention_3, intention_4), 
            by = "ResponseId")

df_freq1$intention_1 <- factor(df_freq1$intention_1, levels = 1:3, labels = c("increase", "no change", "decrease"))
df_freq1$intention_2 <- factor(df_freq1$intention_2, levels = 1:3, labels = c("increase", "no change", "decrease"))
df_freq1$intention_3 <- factor(df_freq1$intention_3, levels = 1:3, labels = c("increase", "no change", "decrease"))
df_freq1$intention_4 <- factor(df_freq1$intention_4, levels = 1:3, labels = c("increase", "no change", "decrease"))

chisq.test(df_freq1$most_name, y = df_freq1$statement_1)
chisq.test(df_freq1$most_name, y = df_freq1$statement_2)
chisq.test(df_freq1$most_name, y = df_freq1$statement_3)
chisq.test(df_freq1$most_name, y = df_freq1$statement_4)
chisq.test(df_freq1$most_name, y = df_freq1$intention_1)
chisq.test(df_freq1$most_name, y = df_freq1$intention_2)
chisq.test(df_freq1$most_name, y = df_freq1$intention_3)
chisq.test(df_freq1$most_name, y = df_freq1$intention_4)

rm(df_freq1)

df$replacement_stock_type[df$replacement_stock_type > 1] <- 2
df$replacement_stock_type <- factor(df$replacement_stock_type, levels = c("1", "2"), labels = c("closed", "open"))
chisq.test(df$ams_cms, df$statement_1)
chisq.test(df$ams_cms, df$statement_2)
chisq.test(df$ams_cms, df$statement_3)
chisq.test(df$ams_cms, df$statement_4)
chisq.test(df$replacement_stock_type, df$statement_1)
chisq.test(df$replacement_stock_type, df$statement_2)
chisq.test(df$replacement_stock_type, df$statement_3)
chisq.test(df$replacement_stock_type, df$statement_4)
chisq.test(df$alva, df$statement_1)
chisq.test(df$alva, df$statement_2)
chisq.test(df$alva, df$statement_3)
chisq.test(df$alva, df$statement_4)


df$intention_1 <- factor(df$intention_1, levels = 1:3, labels = c("increase", "no change", "decrease"))
df$intention_2 <- factor(df$intention_2, levels = 1:3, labels = c("increase", "no change", "decrease"))
df$intention_3 <- factor(df$intention_3, levels = 1:3, labels = c("increase", "no change", "decrease"))
df$intention_4 <- factor(df$intention_4, levels = 1:3, labels = c("increase", "no change", "decrease"))


chisq.test(df$ams_cms, df$intention_1)
chisq.test(df$ams_cms, df$intention_2)
chisq.test(df$ams_cms, df$intention_3)
chisq.test(df$ams_cms, df$intention_4)
chisq.test(df$replacement_stock_type, df$intention_1)
chisq.test(df$replacement_stock_type, df$intention_2)
chisq.test(df$replacement_stock_type, df$intention_3)
chisq.test(df$replacement_stock_type, df$intention_4)
chisq.test(df$alva, df$intention_1)
chisq.test(df$alva, df$intention_2)
chisq.test(df$alva, df$intention_3)
chisq.test(df$alva, df$intention_4)
