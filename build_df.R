rm(list=ls())
graphics.off()

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(tidyr)
library(stringr)

df <- read_excel("~/better_structure/SUDI_interactive/raw_data/ANZCDR & PG SUDI Data (Responses) (17).xlsx") %>%
  slice(-c(1:31, 44:47, 260:261)) %>% #remove practice rows
  mutate(id_time = paste(`Case number used internally by local group`, "_", Timestamp)) %>%
  filter(Jurisdiction == "SA") %>% #keep to SA only
  mutate("Year of death" = year(`Date of death`)) %>%
  filter(`Year of death` > 2004) %>% #keep to our years
  filter(`Year of death` < 2017) %>% 
  mutate(days_old = as.duration(interval(`Date of birth`, `Date of death`))/ddays(1)) %>% 
  filter(days_old < 367) %>% #make sure they are all < 1 year old
  filter(`Had the child been placed to sleep before the fatal episode?` == "Yes") %>% 
  mutate(risk_bed = fct_collapse(`Child placed to sleep in an infant specific bed?`, evidence = "No", 
                                     no_evidence = c("Unknown", "Yes"))) %>% 
  mutate(risk_objects = fct_collapse(`Bedding, pillows, toys or other objects posed a suffocation risk?`, evidence = "Yes", 
                                     no_evidence = c("Unknown", "No"))) %>% 
  mutate(risk_postsmoking = fct_collapse(`Child exposed to smoking after birth?`, evidence = "Yes", 
                                         no_evidence = c("Unknown", "No"))) %>% 
  mutate(risk_presmoking = fct_collapse(`Prenatal smoking by mother?`, evidence = "Yes", 
                                        no_evidence = c("Unknown", "No"))) %>% 
  mutate(risk_fed = fct_collapse(`Was the child breastfed fully or partially at any stage?`, evidence = "No", 
                                 no_evidence = c("Unknown", "Yes"))) %>% 
  mutate(risk_back = fct_collapse(`Child placed to sleep on back (supine)?`, evidence = "No", 
                                     no_evidence = c("Unknown", "Yes"))) %>% 
  mutate(risk_sharing = fct_collapse(`Bed sharing at time of death?`, evidence = "Yes", 
                                     no_evidence = c("Unknown", "No"))) %>% 
  mutate(risk_safe = as.factor(if_else(risk_bed == "evidence" | risk_objects == "evidence", "evidence", "no_evidence"))) %>%
  mutate(risk_smoking = as.factor(if_else(risk_presmoking == "evidence" | risk_postsmoking == "evidence", "evidence", "no_evidence")))

risks_base <- df[, c(44:48)]

saveRDS(risks_base, "~/better_structure/SUDI_interactive/app/SUDI_app/risks_base.rds")

########################
gathered_all_risks <- risks_base %>%
  gather(risk, present) %>%
  filter(present == "evidence") %>%
  select(-one_of("present")) %>%
  group_by(risk) %>%
  summarise(n = n()) %>%
  mutate(total = nrow(risks_base)) %>%
  mutate(proportion = n/total) %>%
  mutate(selected = "All") %>%
  mutate(highlight = "yes")

gathered_back <- risks_base %>%
  filter(risk_back == "evidence") %>%
  gather(risk, present) %>%
  filter(present == "evidence") %>%
  select(-one_of("present")) %>%
  group_by(risk) %>%
  summarise(n = n()) %>%
  mutate(total = nrow(filter(risks_base, risk_back == "evidence"))) %>%
  mutate(proportion = n/total) %>%
  mutate(selected = "risk_back") %>%
  mutate(highlight = if_else(risk == selected, "yes", "no"))

gathered_fed <- risks_base %>%
  filter(risk_fed == "evidence") %>%
  gather(risk, present) %>%
  filter(present == "evidence") %>%
  select(-one_of("present")) %>%
  group_by(risk) %>%
  summarise(n = n()) %>%
  mutate(total = nrow(filter(risks_base, risk_fed == "evidence"))) %>%
  mutate(proportion = n/total) %>%
  mutate(selected = "risk_fed") %>%
  mutate(highlight = if_else(risk == selected, "yes", "no"))

gathered_share <- risks_base %>%
  filter(risk_sharing == "evidence") %>%
  gather(risk, present) %>%
  filter(present == "evidence") %>%
  select(-one_of("present")) %>%
  group_by(risk) %>%
  summarise(n = n()) %>%
  mutate(total = nrow(filter(risks_base, risk_sharing == "evidence"))) %>%
  mutate(proportion = n/total) %>%
  mutate(selected = "risk_sharing") %>%
  mutate(highlight = if_else(risk == selected, "yes", "no"))

gathered_safe <- risks_base %>%
  filter(risk_safe == "evidence") %>%
  gather(risk, present) %>%
  filter(present == "evidence") %>%
  select(-one_of("present")) %>%
  group_by(risk) %>%
  summarise(n = n()) %>%
  mutate(total = nrow(filter(risks_base, risk_safe == "evidence"))) %>%
  mutate(proportion = n/total) %>%
  mutate(selected = "risk_safe") %>%
  mutate(highlight = if_else(risk == selected, "yes", "no"))

gathered_smoking <- risks_base %>%
  filter(risk_smoking == "evidence") %>%
  gather(risk, present) %>%
  filter(present == "evidence") %>%
  select(-one_of("present")) %>%
  group_by(risk) %>%
  summarise(n = n()) %>%
  mutate(total = nrow(filter(risks_base, risk_smoking == "evidence"))) %>%
  mutate(proportion = n/total) %>%
  mutate(selected = "risk_smoking") %>%
  mutate(highlight = if_else(risk == selected, "yes", "no"))

risks_bound <- rbind(gathered_all_risks, gathered_back, gathered_fed, gathered_safe, gathered_share, gathered_smoking) %>%
  mutate(risk = as.factor(risk)) %>%
  mutate(risk = fct_recode(risk, "Not in an\napproved bed" = "risk_safe", 
                           "Parental smoking" = "risk_smoking", 
                           "Bed sharing" = "risk_sharing", 
                           "Not breast fed" = "risk_fed", 
                           "Not placed\non back" = "risk_back")) %>% 
  mutate(risk = fct_relevel(risk, "Not in an\napproved bed",
                                "Parental smoking",
                                "Bed sharing", 
                                "Not breast fed", 
                                "Not placed\non back")) %>%
  mutate(selected = as.factor(selected)) %>%
  mutate(selected = fct_recode(selected, "Not in an\napproved bed" = "risk_safe", 
                               "Parental smoking" = "risk_smoking", 
                               "Bed sharing" = "risk_sharing", 
                               "Not breast fed" = "risk_fed", 
                               "Not placed\non back" = "risk_back")) %>% 
  mutate(selected = fct_relevel(selected, "All", "Not in an\napproved bed",
                                "Parental smoking",
                                "Bed sharing", 
                                "Not breast fed", 
                                "Not placed\non back")) %>% 
  mutate(highlight = fct_relevel(highlight, "yes", "no")) %>%
  select(-c(n, total))
  

saveRDS(risks_bound, "~/better_structure/SUDI_interactive/app/SUDI_app/risks_bound.rds")

###############
selected_risk <- levels(as.factor(risks_bound$selected))[2] #change this with the GUI

risk_plot <- risks_bound %>%
  filter(selected == selected_risk)

ggplot(data = risk_plot, aes(x = risk, y = proportion, fill = highlight)) + 
  geom_bar(stat = "identity")