library(dplyr)
library(ggplot2)

farm <- read.csv("2_farm characteristic data.csv", header = T)

culldf <- read.csv("2_cull_rate.csv", header = T)
colnames(culldf) <- c("ResponseId", "n_culled")

farm <- farm %>% left_join(culldf, by = "ResponseId")
rm(culldf)

summary(farm$n_culled)
farm <- farm %>% filter(!n_culled == 50000)
farm <- farm %>% mutate(cull_rate = n_culled/(herdsize + n_culled))

a <- ggplot(farm, aes(cull_rate)) + 
  geom_histogram(bins = 15, col = "black", fill = "brown", na.rm = T) +
  ylab("number of responses") + xlab("reported cull_rate")
a + theme(panel.background = element_blank(), panel.border = element_rect(fill = NA, color = "black", size = 2))

farm %>% 
  filter(!is.na(herdsize)) %>%
  mutate(size = as.factor(ifelse(herdsize < 80, "<80",
                                 ifelse(herdsize %in% 80:109, "80-109",
                                        ifelse(herdsize %in% 110:170, "110-170", ">170")))),
         herdage = (herd_age_years*12) + herd_age_months) %>%
  mutate(size = factor(size, levels = c("<80", "80-109", "110-170", ">170")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5)) %>%
  ggplot(., aes(y = herdage, x = size, fill = size)) + 
  geom_boxplot(col = "black", na.rm = T, outlier.size = 1)  +
  # ylab("Herd average 305 day milk yield") +
  scale_fill_manual(values = c("yellow", "orangered", "brown", "blue"),
                     name = "Herd size group") + 
                    # labels = c("1.Ja, ik fok mijn eigen jongvee op",
                    #            "2.Ja, ik fok mijn eigen jongvee op, \n echter koop ik ook rundvee aan",
                    #            "3.Nee, mijn eigen jongvee wordt op een \n andere locatie, door iemand anders, \n opgefokt en komt als drachtig pink \n terug naar mijn bedrijf",
                    #            "4.Nee, mijn jongvee wordt door iemand \n anders opgefokt en ik koop een deel aan",
                    #            "5.Nee, ik koop al het vervangende rundvee aan")) +
  stat_summary(fun=mean, colour="black", geom="text", show.legend=F, na.rm = T,
               aes( label=round(..y.., digits=1)), size = 4,
               vjust = -1, angle = 45, hjust = 0.2) +
  stat_summary(fun=mean, colour="black", geom="point",
               shape=18, size=3, show.legend=F, na.rm = T) +
  # scale_x_continuous(breaks = seq(50, 100, 10), labels = seq(50, 100, 10)) +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "right") +
  facet_grid(rows = vars(alva), scales = "free")
  # guides(fill=guide_legend(nrow=3,byrow=TRUE, title.position = "top"))
  
farm %>% select(ResponseId, heifers, herdsize, calves_1year, n_culled, purchase_heifers, ams_cms) %>%
  tidyr::pivot_longer(data = ., cols = c(heifers, herdsize, calves_1year, n_culled,
                               purchase_heifers), names_to = "number_of_animals", 
                      values_to = "count") %>%
  filter(!is.na(ams_cms)) %>%
  ggplot(., aes(x = number_of_animals, y = count, fill = number_of_animals)) + 
  geom_boxplot(col = "black", outlier.shape = NA, na.rm = T) + 
  stat_summary(fun=mean, colour="black", geom="text", show.legend=F, na.rm = T,
               aes( label=round(..y.., digits=1)), size = 4,
               vjust = -1, angle = 45, hjust = 0.2) +
  stat_summary(fun=mean, colour="black", geom="point",
               shape=18, size=3, show.legend=F, na.rm = T) +
  scale_fill_manual(values = c("yellow", "orangered", "brown", "blue", "magenta"),
                    name = "Type of animal count") + 
  coord_flip() + 
  facet_grid(rows = vars(ams_cms), scales = "free") +
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom")



rm(culldf, a)
culldf <- read.csv("frequent reasons.csv", header = T, stringsAsFactors = F)
culldf <- culldf[3:209,]
rownames(culldf) <- 1:207

cull_most <- culldf %>% tidyr::pivot_longer(., cols = most_Reproduction:most_reduce_herdsize,
                                            names_to = "criteria", values_to = "rank") %>%
  select(ResponseId, criteria, rank) %>% filter(!is.na(rank))
cull_least <- culldf %>% tidyr::pivot_longer(., cols = least_Reproduction:least_reduce_herdsize,
                                            names_to = "criteria", values_to = "rank") %>%
  select(ResponseId, criteria, rank) %>% filter(!is.na(rank))

cull_most <- cull_most %>% select(ResponseId, most_criteria = criteria, most_rank = rank) %>%
  mutate(most_criteria = as.factor(gsub(x = most_criteria, pattern = "most_", "")))
cull_least <- cull_least %>% select(ResponseId, least_criteria = criteria, least_rank = rank) %>%
  mutate(least_criteria = as.factor(gsub(x = least_criteria, pattern = "least_", "")))

culldf <- cull_most %>% full_join(cull_least, by = "ResponseId")
farm$herd_age_months <- (farm$herd_age_years*12) + farm$herd_age_months
culldf <- culldf %>% left_join(farm[,c(1,2,5:9,11,14:18)], by = "ResponseId")

write.csv(culldf, "3_freqculling_farmchars.csv", row.names = F)
culldf <- read.csv("3_freqculling_farmchars.csv", header = T)
culldf2 <- culldf %>% 
  select(-c(least_criteria, least_rank)) %>%
  distinct(.)

culldf3 <- culldf %>% 
  select(-c(most_criteria, most_rank)) %>%
  distinct(.) %>%
  filter(!is.na(least_criteria))
culldf2 %>% ggplot(., aes(x = most_criteria, fill = most_criteria)) + 
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                    name = "Culling criteria") + 
  stat_count(geom = "label", colour = "white", size = 3.5,
             aes(label = ..count..)) + 
  coord_flip() + 
  facet_grid(rows = vars(most_rank)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "none")

culldf3 %>% ggplot(., aes(x = least_criteria, fill = least_criteria)) + 
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                               "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
                    name = "Culling criteria") + 
  stat_count(geom = "label", colour = "white", size = 3.5,
             aes(label = ..count..)) + 
  coord_flip() + 
  facet_grid(rows = vars(least_rank)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "none")

culldf2 %>% filter(!is.na(herdsize)) %>%
  mutate(size = as.factor(ifelse(herdsize < 80, "<80",
                                 ifelse(herdsize %in% 80:109, "80-109",
                                        ifelse(herdsize %in% 110:170, "110-170", ">170"))))) %>%
  mutate(size = factor(size, levels = c("<80", "80-109", "110-170", ">170")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5),
         most_rank = as.factor(most_rank)) %>%
  ggplot(., aes(x = most_criteria, fill = most_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9"),
                    name = "Rank-most frequent") + 
  stat_count(geom = "label", colour = "white", size = 2.5,
             aes(label = ..count..)) +
  coord_flip() + 
  facet_wrap(facets = vars(hectares)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))

culldf3 %>% filter(!is.na(herdsize)) %>%
  mutate(size = as.factor(ifelse(herdsize < 80, "<80",
                                 ifelse(herdsize %in% 80:109, "80-109",
                                        ifelse(herdsize %in% 110:170, "110-170", ">170"))))) %>%
  mutate(size = factor(size, levels = c("<80", "80-109", "110-170", ">170")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5),
         least_rank = as.factor(least_rank)) %>%
  ggplot(., aes(x = least_criteria, fill = least_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9"),
                    name = "Rank-least frequent") + 
  stat_count(geom = "label", colour = "white", size = 2.5,
             aes(label = ..count..)) +
  coord_flip() + 
  facet_wrap(facets = vars(size)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))
  
culldf2 %>% filter(!is.na(hectares)) %>%
  mutate(size = as.factor(ifelse(hectares <= 39, "<40ha",
                                 ifelse(hectares %in% 40:59, "40-59ha",
                                        ifelse(hectares %in% 60:74, "60-74ha", "75-150ha"))))) %>%
  mutate(size = factor(size, levels = c("<40ha", "40-59ha", "60-74ha", "75-150ha")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5),
         most_rank = as.factor(most_rank)) %>%
  ggplot(., aes(x = most_criteria, fill = most_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#CC79A7"),
                    name = "Rank-most frequent") + 
  stat_count(geom = "label", colour = "white", size = 2.5 , show.legend = F,
             aes(label = ..count..)) +
  coord_flip() + 
  facet_wrap(facets = vars(size)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))



culldf3 %>% filter(!is.na(hectares)) %>%
  mutate(size = as.factor(ifelse(hectares <= 39, "<40ha",
                                 ifelse(hectares %in% 40:59, "40-59ha",
                                        ifelse(hectares %in% 60:74, "60-74ha", "75-150ha"))))) %>%
  mutate(size = factor(size, levels = c("<40ha", "40-59ha", "60-74ha", "75-150ha")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5),
         least_rank = as.factor(least_rank)) %>%
  ggplot(., aes(x = least_criteria, fill = least_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#CC79A7"),
                    name = "Rank-least frequent") + 
  stat_count(geom = "label", colour = "white", size = 2.5 , show.legend = F,
             aes(label = ..count..)) +
  coord_flip() + 
  facet_wrap(facets = vars(size)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))




culldf2 %>% filter(!is.na(ams_cms)) %>%
  mutate(size = as.factor(ifelse(hectares <= 39, "<40ha",
                                 ifelse(hectares %in% 40:59, "40-59ha",
                                        ifelse(hectares %in% 60:74, "60-74ha", "75-150ha"))))) %>%
  mutate(size = factor(size, levels = c("<40ha", "40-59ha", "60-74ha", "75-150ha")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5),
         most_rank = as.factor(most_rank)) %>%
  ggplot(., aes(x = most_criteria, fill = most_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#56B4E9", "#009E73", 
                               "#F0E442"),
                    name = "Rank-most frequent") + 
  stat_count(geom = "label", colour = "black", size = 2 , show.legend = F,
             aes(label = ..count..),
             position = position_stack()) +
  coord_flip() + 
  facet_grid(rows = vars(ams_cms), scales = "free") + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))


culldf3 %>% filter(!is.na(ams_cms)) %>%
  mutate(size = as.factor(ifelse(hectares <= 39, "<40ha",
                                 ifelse(hectares %in% 40:59, "40-59ha",
                                        ifelse(hectares %in% 60:74, "60-74ha", "75-150ha"))))) %>%
  mutate(size = factor(size, levels = c("<40ha", "40-59ha", "60-74ha", "75-150ha")),
         replacement_stock_type = factor(replacement_stock_type,
                                         levels = 1:5),
         least_rank = as.factor(least_rank)) %>%
  ggplot(., aes(x = least_criteria, fill = least_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#56B4E9", "#009E73", 
                               "#F0E442"),
                    name = "Rank-least frequent") + 
  stat_count(geom = "label", colour = "black", size = 2 , show.legend = F,
             aes(label = ..count..),
             position = position_stack()) +
  coord_flip() + 
  facet_grid(rows = vars(ams_cms)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))



culldf2 <- culldf2 %>% filter(!is.na(n_culled)) %>% filter(!is.na(herdsize)) %>%
  mutate(cull_rate = (n_culled)/ (n_culled + herdsize))

culldf2 %>% 
  mutate(cull_rate_factor = as.factor(ifelse(cull_rate <= 0.14, "<=14%", ">14%"))) %>%
  mutate(most_rank = as.factor(most_rank)) %>%
  ggplot(., aes(x = most_criteria, fill = most_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#D55E00", "#009E73", 
                               "#F0E442"),
                    name = "Rank-most frequent") + 
  stat_count(geom = "label", colour = "black", size = 2 , show.legend = F,
             aes(label = ..count..),
             position = position_stack()) +
  coord_flip() + 
  facet_grid(cols = vars(cull_rate_factor), scales = "free") + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))


culldf3 <- culldf3 %>% filter(!is.na(n_culled)) %>% filter(!is.na(herdsize)) %>%
  mutate(cull_rate = (n_culled)/ (n_culled + herdsize))

culldf3 %>% 
  mutate(cull_rate_factor = as.factor(ifelse(cull_rate <= 0.14, "<=14%", ">14%"))) %>%
  mutate(least_rank = as.factor(least_rank)) %>%
  ggplot(., aes(x = least_criteria, fill = least_rank)) +
  geom_bar(stat = "count", position = position_stack(), col = "black") +
  ylab("Count of responses per rank") +
  scale_fill_manual(values = c("#D55E00", "#009E73", 
                               "#F0E442"),
                    name = "Rank-least frequent") + 
  stat_count(geom = "label", colour = "black", size = 2 , show.legend = F,
             aes(label = ..count..),
             position = position_stack()) +
  coord_flip() + 
  facet_grid(cols = vars(cull_rate_factor)) + 
  theme(panel.background = element_blank(),
        axis.line = element_line(color = "black", size = 1),
        panel.border = element_rect(fill = NA, size = 0.5),
        panel.grid.major = element_line(color = "lightgray", size = 0.3),
        legend.position = "bottom") +
  guides(fill = guide_legend(title.position = "top"))
