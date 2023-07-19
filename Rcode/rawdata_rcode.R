library(dplyr)
farm_char <- read.csv("./farm characteristic data.csv", header = T, stringsAsFactors = F)

farm_char <- farm_char[-c(1, 2),]
farm_char$age_group_farmer <- ifelse(farm_char$age_group_farmer == "18 tot en met 35 jaar", 
                                     "18_35",
                                     ifelse(farm_char$age_group_farmer == "36 tot en met 55 jaar", 
                                            "36_55",
                                            ifelse(farm_char$age_group_farmer == "56 jaar of ouder", ">55", NA)))
# farm_char$age_group_farmer <- as.factor(farm_char$age_group_farmer)
unique(farm_char$alva)
farm_char <- farm_char %>%
  mutate(type_farm = ifelse(type_farm == "Gangbaar", "conventional",
                            ifelse(type_farm == "Biologisch", "organic",
                                   ifelse(type_farm == "Anders, namelijk: (Bijvoorbeeld een onderzoek of onderwijs bedrijf)", "other", NA))),
         ams_cms = ifelse(ams_cms == "Nee, ik gebruik een melkmachine", "cms",
                          ifelse(ams_cms == "Ja, ik gebruik een melkrobot", "ams", NA)))

farm_char <- farm_char %>%
  mutate(replacement_stock_type = ifelse(replacement_stock_type == "Ja, ik fok mijn eigen jongvee op",
                                         "1",
                                         ifelse(replacement_stock_type == 
                                                  "Nee, mijn eigen jongvee wordt op een andere locatie, door iemand anders, opgefokt en komt als drachtig pink terug naar mijn bedrijf",
                                                "3",
                                                ifelse(replacement_stock_type == "Ja, ik fok mijn eigen jongvee op, echter koop ik ook rundvee aan",
                                                       "2",
                                                       ifelse(replacement_stock_type == "Nee, ik koop al het vervangende rundvee aan",
                                                              "5", 
                                                              ifelse(replacement_stock_type == "Nee, mijn jongvee wordt door iemand anders opgefokt en ik koop een deel aan",
                                                                     "4", NA))))))

farm_char <- farm_char %>%
  mutate(alva = ifelse(alva == "20 tot en met 24 maanden", "20_24",
                       ifelse(alva == "25 tot en met 27 maanden", "25_27",
                              ifelse(alva == "28 tot en met 32 maanden", "28_32",NA))),
         ir_data = ifelse(ir_data == "Ja", "1",
                          ifelse(ir_data == "Nee", "0", NA)))

write.csv(farm_char, "./2_farm characteristic data.csv", row.names = F)

rm(farm_char)

cull <- read.csv("./recent_culling_data.csv", header = T, stringsAsFactors = F)

cull <- cull[-c(1,2),]
rownames(cull) <- 1:nrow(cull)

unique(cull$recent_primi_remember)
cull <- cull %>%
  mutate(decision_responsible =
           ifelse(decision_responsible == "Deels, ik ben samen met iemand anders verantwoordelijk voor die beslissing",
                  "Partly",
                  ifelse(decision_responsible == "Ja", "Yes", NA)),
         recent_primi_remember = ifelse(recent_primi_remember == "Ja", 1,
                                        ifelse(recent_primi_remember == "Nee", 0, NA)),
         recent_multi_remember = ifelse(recent_multi_remember == "Ja", 1, 
                                        ifelse(recent_multi_remember == "Nee", 0, NA)))


cull$recent_primi_reason <- gsub("Uiervorm, ", "Uiervorm/", cull$recent_primi_reason)
cull$recent_primi_reason <- gsub("gedrag, zoals", "gedrag-zoals", cull$recent_primi_reason)
cull <- cull %>% tidyr::separate(., recent_primi_reason, remove = F, into = c("primi_reason_1", "primi_reason_2", "primi_reason_3"), sep = ",")

cull$primi_reason_2[cull$primi_reason_2 == " namelijk:"] <- NA
cull$primi_reason_3[cull$primi_reason_3 == " namelijk:"] <- NA


cull <- cull %>%
  mutate(primi_reason_1 = ifelse(primi_reason_1 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                           ifelse(primi_reason_1 == "Kreupelheid en/of klauwaandoening(en)",
                                  "claw health",
                                  ifelse(primi_reason_1 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                         ifelse(primi_reason_1 == "Lage melkproductie", "low milk",
                                                ifelse(primi_reason_1 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                       ifelse(primi_reason_1 == "Vruchtbaarheidsproblemen", "reproduction",
                                                              ifelse(primi_reason_1 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                     ifelse(primi_reason_1 == "Anders", "other", NA)))))))))

cull <- cull %>%
  mutate(primi_reason_2 = ifelse(primi_reason_2 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                                 ifelse(primi_reason_2 == "Kreupelheid en/of klauwaandoening(en)",
                                        "claw health",
                                        ifelse(primi_reason_2 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                               ifelse(primi_reason_2 == "Lage melkproductie", "low milk",
                                                      ifelse(primi_reason_2 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                             ifelse(primi_reason_2 == "Vruchtbaarheidsproblemen", "reproduction",
                                                                    ifelse(primi_reason_2 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                           ifelse(primi_reason_2 == "Anders", "other", NA)))))))))

cull <- cull %>%
  mutate(primi_reason_3 = ifelse(primi_reason_3 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                                 ifelse(primi_reason_3 == "Kreupelheid en/of klauwaandoening(en)",
                                        "claw health",
                                        ifelse(primi_reason_3 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                               ifelse(primi_reason_3 == "Lage melkproductie", "low milk",
                                                      ifelse(primi_reason_3 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                             ifelse(primi_reason_3 == "Vruchtbaarheidsproblemen", "reproduction",
                                                                    ifelse(primi_reason_3 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                           ifelse(primi_reason_3 == "Anders", "other", NA)))))))))


unique(cull$recent_primi_decision_time)

cull <- cull %>%
  mutate(recent_primi_decision_time = ifelse(recent_primi_decision_time == "Na een behandeling (waarna het dier niet herstelde)", "after treatment",
                                             ifelse(recent_primi_decision_time == "Na inseminatie", "after insemination",
                                                    ifelse(recent_primi_decision_time == "Na het kalven", "after calving",
                                                           ifelse(recent_primi_decision_time == "Anders, namelijk:", "other",
                                                                  ifelse("Einde van de lactatie", "end of lactation", NA))))))


cull <- cull[,-10]

cull <- cull %>% mutate(recent_primi_unforseen = ifelse(recent_primi_unforseen == "Ja", 1,
                                                        ifelse(recent_primi_unforseen == "Nee", 0, NA)))

unique(cull$recent_multi_parity)
cull <- cull %>% mutate(recent_multi_parity = ifelse(recent_multi_parity == "4 of meer keer", "4+",
                                                     ifelse(recent_multi_parity == "3 keer", "3",
                                                            ifelse(recent_multi_parity == "2 keer", "2", NA))))

cull$recent_multi_reasons <- gsub("Uiervorm, grootte", "Uiervorm-grootte", cull$recent_multi_reasons)
cull$recent_multi_reasons <- gsub("gedrag, zoals", "gedrag-zoals", cull$recent_multi_reasons)

cull <- cull %>% tidyr::separate(., recent_multi_reasons, remove = F, into = c("multi_reason_1", "multi_reason_2", "multi_reason_3", "multi_reason_4"), sep = ",")
#row65 1 reasons removed + row124 1 reason removed

cull$multi_reason_1[cull$multi_reason_1 == " namelijk:"] <- NA
cull$multi_reason_2[cull$multi_reason_2 == " namelijk:"] <- NA
cull$multi_reason_3[cull$multi_reason_3 == " namelijk:"] <- NA
cull$multi_reason_4[cull$multi_reason_4 == " namelijk:"] <- NA

unique(cull$multi_reason_4)

cull <- cull %>%
  mutate(multi_reason_1 = ifelse(multi_reason_1 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                                 ifelse(multi_reason_1 == "Kreupelheid en/of klauwaandoening(en)",
                                        "claw health",
                                        ifelse(multi_reason_1 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                               ifelse(multi_reason_1 == "Lage melkproductie", "low milk",
                                                      ifelse(multi_reason_1 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                             ifelse(multi_reason_1 == "Vruchtbaarheidsproblemen", "reproduction",
                                                                    ifelse(multi_reason_1 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                           ifelse(multi_reason_1 == "Anders", "other", NA)))))))),
         multi_reason_2 = ifelse(multi_reason_2 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                                 ifelse(multi_reason_2 == "Kreupelheid en/of klauwaandoening(en)",
                                        "claw health",
                                        ifelse(multi_reason_2 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                               ifelse(multi_reason_2 == "Lage melkproductie", "low milk",
                                                      ifelse(multi_reason_2 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                             ifelse(multi_reason_2 == "Vruchtbaarheidsproblemen", "reproduction",
                                                                    ifelse(multi_reason_2 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                           ifelse(multi_reason_2 == "Anders", "other", NA)))))))),
         multi_reason_3 = ifelse(multi_reason_3 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                                 ifelse(multi_reason_3 == "Kreupelheid en/of klauwaandoening(en)",
                                        "claw health",
                                        ifelse(multi_reason_3 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                               ifelse(multi_reason_3 == "Lage melkproductie", "low milk",
                                                      ifelse(multi_reason_3 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                             ifelse(multi_reason_3 == "Vruchtbaarheidsproblemen", "reproduction",
                                                                    ifelse(multi_reason_3 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                           ifelse(multi_reason_3 == "Anders", "other", NA)))))))),
         multi_reason_4 = ifelse(multi_reason_4 == "Ongewenst gedrag-zoals agressiviteit", "Undesirable behaviour",
                                 ifelse(multi_reason_4 == "Kreupelheid en/of klauwaandoening(en)",
                                        "claw health",
                                        ifelse(multi_reason_4 == "Verhoogd celgetal en/of uierontsteking", "SCC",
                                               ifelse(multi_reason_4 == "Lage melkproductie", "low milk",
                                                      ifelse(multi_reason_4 == "Uiervorm/grootte en/of symmetrie is niet zoals gewenst", "udder conformity",
                                                             ifelse(multi_reason_4 == "Vruchtbaarheidsproblemen", "reproduction",
                                                                    ifelse(multi_reason_4 == "Het verkleinen van de melkveestapel bijvoorbeeld vanwege fosfaatrechten", "reduce herdsize", 
                                                                           ifelse(multi_reason_4 == "Anders", "other", NA)))))))))

cull <- cull %>%
  mutate(recent_multi_decision_time = ifelse(recent_multi_decision_time == "Na een behandeling (waarna het dier niet herstelde)", "after treatment",
                                             ifelse(recent_multi_decision_time == "Na inseminatie", "after insemination",
                                                    ifelse(recent_multi_decision_time == "Na het kalven", "after calving",
                                                           ifelse(recent_multi_decision_time == "Anders, namelijk:", "other",
                                                                  ifelse("Einde van de lactatie", "end of lactation", NA))))))
cull <- cull %>% mutate(recent_multi_unforseen = ifelse(recent_multi_unforseen == "Ja", 1,
                                                        ifelse(recent_multi_unforseen == "Nee", 0, NA)))

cull <- cull[,c(22, 2:21)]
write.csv(cull, "./2_recent_culling_data.csv", row.names = F)

rm(cull)

strat <- read.csv("./culling strategies data.csv", header = T, stringsAsFactors = F)
strat <- strat[-c(1:2),]
rownames(strat) <- 1:nrow(strat)

strat[,2:5][strat[,2:5] == "Deels mee eens"] <- "somewhat agree"
strat[,2:5][strat[,2:5] == "Helemaal mee eens"] <- "totally agree"
strat[,2:5][strat[,2:5] == "Neutraal"] <- "neither agree nor disagree"
strat[,2:5][strat[,2:5] == "Helemaal mee oneens"] <- "totally disagree"
strat[,2:5][strat[,2:5] == "Deels mee oneens"] <- "somewhat disagree"
strat[,2:5][strat[,2:5] == ""] <- NA


strat <- strat %>%
  tidyr::separate(., primi_criteria_statement_4, remove = F, 
                  into = c('primi_plan1', "primi_plan2", "primi_plan3"), sep = ",")
strat[,7:9][strat[,7:9] == " namelijk:"] <- NA
strat[,7:9][strat[,7:9] == "Vruchtbaarheid"] <- "reproduction"
strat[,7:9][strat[,7:9] == "Anders"] <- "other"
strat[,7:9][strat[,7:9] == "Fokwaarde"] <- "breeding value"
strat[,7:9][strat[,7:9] == "Melkproductie"] <- "milk production"
strat[,7:9][strat[,7:9] == "Uiergezondheid"] <- "udder health"
strat[,7:9][strat[,7:9] == "Klauwgezondheid"] <- "claw health"
strat[,7:9][strat[,7:9] == "Exterieur"] <- "body conformation"

strat <- strat %>%
  tidyr::separate(., multi_criteria_statement_4, remove = F, 
                  into = c('multi_plan1', "multi_plan2", "multi_plan3", "multi_plan4"), sep = ",")

strat[,14:17][strat[,14:17] == " namelijk:"] <- NA
strat[,14:17][strat[,14:17] == "Vruchtbaarheid"] <- "reproduction"
strat[,14:17][strat[,14:17] == "Anders"] <- "other"
strat[,14:17][strat[,14:17] == "Fokwaarde"] <- "breeding value"
strat[,14:17][strat[,14:17] == "Melkproductie"] <- "milk production"
strat[,14:17][strat[,14:17] == "Uiergezondheid"] <- "udder health"
strat[,14:17][strat[,14:17] == "Klauwgezondheid"] <- "claw health"
strat[,14:17][strat[,14:17] == "Exterieur"] <- "body conformation"

strat <- strat[,c(1:9, 11, 13:18)]

write.csv(strat, "./2_culling strategies data.csv", row.names = F)
rm(strat)

int <- read.csv("./intention statement data.csv", header = T, stringsAsFactors = F, na.strings = "")
int <- int[-c(1:2),]
rownames(int) <- 1:nrow(int)

unique(int$intention_1)
unique(int$intention_2)
unique(int$intention_3)
unique(int$intention_4)

int$intention_1[int$intention_1 == "Ja, ik wil eerste kalfskoeien minder snel afvoeren"] <- 2
int$intention_1[int$intention_1 == "Nee, ik wil mijn afvoerstrategie niet veranderen"] <- 3

int$intention_2[int$intention_2 == "Ja, ik wil oudere kalfskoeien sneller afvoeren"] <- 1
int$intention_2[int$intention_2 == "Ja, ik wil oudere kalfskoeien minder snel afvoeren"] <- 2
int$intention_2[int$intention_2 == "Nee, ik wil mijn afvoerstrategie niet veranderen"] <- 3

int$intention_3[int$intention_3 == "Ja, ik wil meer melkkoeien afvoeren"] <- 1
int$intention_3[int$intention_3 == "Ja, ik wil minder melkkoeien afvoeren"] <- 2
int$intention_3[int$intention_3 == "Nee, ik wil dit aantal niet veranderen"] <- 3

int$intention_4[int$intention_4 == "Ja, ik wil meer vrouwelijke runderen aanhouden of aankopen"] <- 1
int$intention_4[int$intention_4 == "Ja, ik wil minder vrouwelijke runderen aanhouden of aankopen"] <- 2
int$intention_4[int$intention_4 == "Nee, ik wil dit aantal niet veranderen"] <- 3

unique(int$motivation_intention_3)

int <- int %>% tidyr::separate(., motivation_intention_3, remove = F, into = c("motivation_1", "motivation_2", "motivation_3", "motivation_4"), sep  =",")

int[,6:9][int[,6:9] == " namelijk:"] <- NA
int[,6:9][int[,6:9] == "bedrijfsvoering die beter aansluit bij mijn huidige visie"] <- "management that matched my vision"
int[,6:9][int[,6:9] == "verhoging van de gemiddelde leeftijd van de melkkoeien"] <- "improve longevity"
int[,6:9][int[,6:9] == "verbetering van de ecologische duurzaamheid van mijn bedrijf"] <- "improve environmental sustainability"
int[,6:9][int[,6:9] == "Anders"] <- "other"
int[,6:9][int[,6:9] == "verhoogd economisch resultaat (uitbreiding valt hieronder)"] <- "improve economic result"

write.csv(int, "./2_intention statement data.csv", row.names = F)
rm(int)
