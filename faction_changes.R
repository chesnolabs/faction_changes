#  rm(list=ls())
library(tidyverse)
library(stringr)
library(xlsx)
library(jsonlite)
setwd("C:/Users/user/Desktop/scripts")

fchanges <- read.csv("faction_changes/data/faction_changes_full.csv",
                     stringsAsFactors = F) %>% mutate(fullname = str_trim(fullname))

mps <- get_all_mps() %>% 
  mutate(mazhor = ifelse(is.na(district_num), "l", "m"))

fchanges <- fchanges %>% 
  left_join(mps[, c("fullname", "mazhor")])

transitions <- changes_pairs(fchanges, count = F) %>% 
  mutate(from_power = ifelse(origins%in%c("БПП", "Народний фронт")|
                      origins%in%c("Батьківщина", "Самопоміч") & date >= as.Date("2016-02-19")|
                      origins == "РПЛ" & date >= "2016-03-29", 1, 0),
         to_power = ifelse(destinations%in%c("БПП", "Народний фронт")|
                    destinations%in%c("Батьківщина", "Самопоміч") & date < as.Date("2016-02-19")|
                    destinations == "РПЛ" & date < "2016-03-29", 1, 0))

transitions_count <- transitions %>% 
  group_by(origins, destinations, mazhor) %>% 
  count() %>% 
  spread(mazhor, n, fill = 0) %>% 
  mutate(total = l + m,
         m_percent = round(m/total*100),
         l_percent = round(l/total*100)) %>% 
  arrange(desc(total))

transitions_summary <- transitions %>% 
  group_by(mazhor) %>% 
  summarize(n = n(),
            n_from_power = sum(from_power),
            n_to_power = sum(to_power),
            percent_from_power = mean(from_power),
            percent_to_power = mean(to_power)) 

transitions_summary$propensity_change <- c(transitions_summary$n[transitions_summary$mazhor == "m"]/sum(mps$mazhor == "m"),
             transitions_summary$n[transitions_summary$mazhor == "l"]/sum(mps$mazhor == "l"))

summary_mazhor_factions <- fchanges %>% 
  group_by(fullname) %>% 
  mutate(faction_order = row_number()) %>% 
  ungroup() %>% 
  group_by(faction, faction_order) %>% 
  summarize(m_n = sum(mazhor == "m"),
            l_n = sum(mazhor == "l"),
            m_percent = round(mean(mazhor == "m")*100, 1),
            l_percent = round(mean(mazhor == "l")*100, 1))

summary_by_name <- fchanges %>% 
  group_by(fullname, mazhor) %>% 
  summarize(n = n()-1) %>% 
  arrange(desc(n)) %>% 
  filter(n > 0)

transitions_count_total <- transitions_count %>% 
  group_by(origins) %>% 
  summarize(sum_from = sum(total)) %>% 
  left_join(summarize(group_by(transitions_count, destinations),
                      sum_to = sum(total)), 
            by = c("origins" = "destinations")) %>% 
  replace_na(list(sum_to = 0)) %>% 
  rename(faction = origins)

write.xlsx(as.data.frame(transitions), file="faction_changes/output/faction_changes.xlsx",
           sheetName="Всі переходи ВР 8", row.names=FALSE)
write.xlsx(as.data.frame(transitions_count), file="faction_changes/output/faction_changes.xlsx",
           sheetName="Переходи ВР 8 пораховані", row.names=FALSE, append = T)
write.xlsx(as.data.frame(transitions_count_total), file="faction_changes/output/faction_changes.xlsx",
           sheetName="Переходи ВР 8 - сума по фракціях", row.names=FALSE, append = T)
write.xlsx(as.data.frame(transitions_summary), file="faction_changes/output/faction_changes.xlsx",
           sheetName="Переходи ВР 8 влада резюме", row.names=FALSE, append = T)
write.xlsx(as.data.frame(summary_mazhor_factions), file="faction_changes/output/faction_changes.xlsx",
           sheetName="% мажоритарників за фракцією", row.names=FALSE, append = T)
write.xlsx(as.data.frame(summary_by_name), file="faction_changes/output/faction_changes.xlsx",
           sheetName="Кількість переходів за депутатом", row.names=FALSE, append = T)

# number of coalition members at signature date

fchanges %>% 
  filter(faction_start == "2014-11-27") %>% 
  group_by(fullname) %>% 
  slice(1) %>% 
  mutate(coalition = ifelse(faction%in%c("Батьківщина", "БПП", "Народний фронт", "РПЛ", "Самопоміч"), 1, 0)) %>% 
  ungroup() %>% 
  summarize(coal = sum(coalition == 1))
