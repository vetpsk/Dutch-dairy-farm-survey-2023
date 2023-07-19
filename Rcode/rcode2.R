library(dplyr)
library(ggplot2)
search()


recent <- read.csv("2_recent_culling_data.csv", header = T)

recent1 <- recent[rowSums(!is.na(recent[,5:7]))>0,]

primi_reason_1 <- recent1 %>% group_by(primi_reason_1) %>% 
  summarise(counts_1 = n()) %>% rename(reason = primi_reason_1)
  
primi_reason_2 <- recent1 %>% group_by(primi_reason_2) %>% 
  summarise(counts_2 = n()) %>% rename(reason = primi_reason_2)
primi_reason_3 <- recent1 %>% group_by(primi_reason_3) %>% 
  summarise(counts_3 = n()) %>% rename(reason = primi_reason_3)

primi_reason <- primi_reason_1 %>% 
  full_join(primi_reason_2) %>% 
  full_join(primi_reason_3)
primi_reason <- primi_reason[-9,] 
primi_reason[is.na(primi_reason) == T] <- 0
primi_reason$total_count <- primi_reason$counts_1 + 
  primi_reason$counts_2 + 
  primi_reason$counts_3
rm(primi_reason_1, primi_reason_2, primi_reason_3)

primi_reason <- primi_reason %>% 
  mutate(percent = round(total_count/sum(total_count), digits = 2))

# primi_reason <- tidyr::pivot_longer(primi_reason, c(counts_1, counts_2, counts_3), 
#                                     names_to = "reasons", values_to = "counts")

recent2 <- recent[rowSums(!is.na(recent[,14:17]))>0,]
multi_reason_1 <- recent1 %>% group_by(multi_reason_1) %>% 
  summarise(counts_1 = n()) %>% rename(reason = multi_reason_1)
multi_reason_2 <- recent1 %>% group_by(multi_reason_2) %>% 
  summarise(counts_2 = n()) %>% rename(reason = multi_reason_2)
multi_reason_3 <- recent1 %>% group_by(multi_reason_3) %>% 
  summarise(counts_3 = n()) %>% rename(reason = multi_reason_3)
multi_reason_4 <- recent1 %>% group_by(multi_reason_4) %>% 
  summarise(counts_4 = n()) %>% rename(reason = multi_reason_4)

multi_reason <- multi_reason_1 %>% full_join(multi_reason_2) %>%
  full_join(multi_reason_3) %>% full_join(multi_reason_4)

multi_reason <- multi_reason[-8,]
multi_reason[is.na(multi_reason) == T] <- 0
multi_reason$total_count <- multi_reason$counts_1 + multi_reason$counts_2 + 
  multi_reason$counts_3 + multi_reason$counts_4
rm(multi_reason_1, multi_reason_2, multi_reason_3, multi_reason_4)

multi_reason <- multi_reason %>% mutate(percent = round(total_count/sum(total_count), digits = 2))

multi_par <- recent2 %>% select(ResponseId,recent_multi_parity:multi_reason_4) %>% 
  tidyr::pivot_longer(., c(multi_reason_1:multi_reason_4), 
                      names_to  = "reason_number", values_to = "reason") %>%
  filter(!is.na(reason))

multi_par_reasons <- as.data.frame(table(multi_par$reason,multi_par$recent_multi_parity))
colnames(multi_par_reasons) <- c("reasons", "parity", "count")
multi_par_reasons <- multi_par_reasons %>% group_by(parity) %>% 
  mutate(percent_par = round(count/sum(count), 2))


a <- ggplot(multi_par_reasons, aes(x = parity, fill = reasons, y = count)) + 
          geom_bar(stat = "identity", position = "dodge", col = "black") +
          geom_text(aes(label = percent_par), size = 3, 
            position = position_dodge(width = .9), 
            check_overlap = T, vjust = -1) + 
          scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", 
                               "#009E73", "#F0E442", "#0072B2", "#D55E00")) + 
          ggtitle("Paritywise reasons for recently culled multiparous cows", 
          subtitle = "(labels are %/100)")
a + theme(plot.title = element_text(size = rel(1), hjust = 0.5),
          plot.subtitle = element_text(size = rel(0.8), hjust = 0.75)) 

primi_reason <- primi_reason %>% 
  mutate(reason = factor(reason, 
                         levels = c("reproduction",
                                    "SCC", "low milk",
                                    "claw health",
                                    "Undesirable behaviour",
                                    "udder conformity",
                                    "reduce herdsize",
                                    "other")))
a <- ggplot(primi_reason, aes(x = total_count, y = reason)) + 
          geom_col(fill = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), col = "black") + 
          geom_text(aes(label = percent), size = 3, hjust = 1) +
          ylab("Reasons") + xlab("Count (total)") + 
          ggtitle("Primiparous culling reasons", subtitle = "(text values are %/100)")

a + theme(plot.title = element_text(hjust = 0.5, size = rel(1.2)), 
          plot.subtitle = element_text(hjust = 0.5, size = rel(0.6)))

a <- as.character(unique(recent1$recent_primi_reason_appendix))          

b <- c(NA,
  "Blood Effusion in Udder",
  "Metabolism",
  "don't miss out on milk.",
  "Caved too much",
  "gust during dry period",
  "probably flesh sperm",
  "Broken leg",
  "Distortion of the abomasum",
  " trembling with the hind legs when standing . something with muscles .",
  "PERIODIC INflammation",
  "This heifer did not shed milk after 2 weeks of milking",
  "Udder wrinkled",
  "dismissed after injury in the buttock, hind leg",
  "does not fit into frieslancampina policy",
  "Veterinarian had ejected the calf and couldn't get her pregnant after the time",
  "We Needed Money",
  "milk don't shoot",
  "Serious injuries",
  "had a fracture in knee and therefore splinters in knee",
  "Disaster slaughter.",
  "calved with mastitis",
  "Broken Leg",
  "Extend leg",
  "Accident Disaster",
  "Could not enter the robot because of too restless and beat a fine heifer just too restless for the ams milking",
  "Too Tough",
  "convulsion",
  "Pumped, draughty",
  "phased stop operation")
primi_appendix <- data.frame(a,b)
multi_reason <- multi_reason %>% full_join(multi_par_reasons, by = c("reason" = "reasons"))
