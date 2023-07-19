library(dplyr)
library(ggplot2)

culstrat <- read.csv("2_culling strategies data.csv", header = T)

culstrat1 <- culstrat[complete.cases(culstrat[,1:5]),1:5]



culstrat_sum <- culstrat1 %>% 
  tidyr::pivot_longer(., statement_1:statement_4, 
                      names_to = "statement", values_to = "scale") %>%
  group_by(statement, scale) %>% summarise(count = n()) %>%
  ungroup() %>% group_by(statement) %>% mutate(percent = count*100/sum(count)) %>%
  mutate(scale = factor(scale, levels = c("totally disagree",
                                          "somewhat disagree",
                                          "neither agree nor disagree",
                                          "somewhat agree",
                                          "totally agree")),
         statement = factor(statement, levels = c("statement_4",
                                                  "statement_3",
                                                  "statement_2",
                                                  "statement_1"))) %>%
  mutate(labels = paste0("(", count, ")", percent, "%", sep  =""))
culstrat_sum$labels[culstrat_sum$statement == "statement_3" & culstrat_sum$scale == "totally disagree"] <- ""


a <- ggplot(culstrat_sum, aes(x = percent, y = statement, fill = scale)) + 
  geom_bar(stat = "identity", col  ="black", position = position_stack(reverse = T)) +
  scale_fill_manual(values = c("red", "indianred1", "white", "lightblue", "blue")) +
  geom_text(aes(label = labels), angle = 90, size = 2.5, 
            position = position_stack(reverse = T), vjust = -1) +
  labs(title = "Response to statements regarding \n culling strategies followed by Dutch dairy farmers",
       subtitles = "* (numbers in the bracket indicate counts of responses in particular scale)",
       fill = "Scale of responses")
a + theme(plot.title = element_text(hjust = 0.5, size = rel(1.5)),
          plot.subtitle = element_text(hjust = 0.5, size = rel(0.75)),
          panel.background = element_rect(fill = "white", color = "black"))
rm(culstrat_sum)
rm(a)

culstrat2 <- culstrat[,c(1, 7:10,12:15)]
culstrat2 <- culstrat2 %>% select(!primi_criteria_appendix) %>%
  tidyr::pivot_longer(., c(primi_plan1:primi_plan3, multi_plan1:multi_plan4),
                      names_to = "plan",
                      values_to = "criteria")
culstrat2$type <- ifelse(substr(culstrat2$plan, 1, 3) == "pri", "primiparous", "multiparous")
culstrat2$type[culstrat2$criteria == "Dezelfde criteria als voor eerste kalfskoeien"] <- "both"
culstrat2$type <- as.factor(culstrat2$type)
culstrat2$criteria <- as.character(culstrat2$criteria)
culstrat2$criteria[culstrat2$criteria == "Dezelfde criteria als voor eerste kalfskoeien"] <- "same criteria for all cows"
culstrat2$criteria[culstrat2$criteria == ""] <- NA
culstrat2_sum <- culstrat2 %>% group_by(type, criteria) %>% summarise(counts = n()) %>%
  filter(!is.na(criteria))

a <- ggplot(culstrat2_sum, aes(x = counts, y = criteria, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#F0E442"), 
                    breaks = c("multiparous", "primiparous", "both"))
a + labs(title = "Specific plan for group of cows followed by farmers")

