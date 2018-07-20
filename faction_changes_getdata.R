library(tidyverse)
library(stringr)
library(rvest)
library(jsonlite)
library(TraMineR)
setwd("C:/Users/user/Desktop/scripts")

mps <- get_all_mps()

links <- paste0("http://w1.c1.rada.gov.ua/pls/site2/p_deputat_fr_changes?d_id=", mps$id)

html <- read_html(links[1], encoding = "Windows-1251")

get_names <- function(x){
  html <- read_html(x, encoding = "Windows-1251")
  fullname <- html %>% 
    html_nodes("b") %>% 
    html_text() %>% 
    str_replace_all(" - Переходи по фракціях", "")
  print(fullname)
  print(fullname)
  Sys.sleep(0.05)
  return(fullname)
}  

get_changes <- function(x){  
  html <- read_html(x, encoding = "Windows-1251")  
  changes <- html %>% 
    html_nodes("td") %>% 
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_replace_all("\t", "") %>% 
    str_trim()
  print(changes)
  Sys.sleep(0.1)
  return(changes)
}

get_fchanges <- function(){
  mp_names <- map(links, get_names)
  changes <- map(links, get_changes)
  
  for(i in seq_along(changes)){
    if(length(changes[[i]])==0){
      changes[[i]] <- as.character(c("Позафракційний", "-", "-"))
    }}
  
  id_nrows <- changes %>% 
    map(~length(.)/3) %>% unlist()
  changes_full <- changes %>% 
    map(matrix, ncol = 3, byrow = T) %>% 
    map(as_data_frame) %>% 
    bind_rows() %>% 
    mutate(id = rep(mps$id, times = id_nrows), fullname = rep(unlist(mp_names), times = id_nrows)) %>% 
    select(id:fullname, V1:V3) %>% 
    rename(faction = V1, 
           faction_start = V2, 
           faction_end = V3)
  return(changes_full)
}

fchanges <- get_fchanges()

fchanges_tidy <- fchanges %>% 
  mutate(faction = recode(faction, 
                          `Група "Воля народу"` = "Воля народу",
                          `Група "Партія "Відродження"` = "Відродження",
                          `Група "Відродження"` = "Відродження",
                          `Група "Економічний розвиток"` = "Відродження",
                          `Народні депутати, які не входять до складу жодної фракції чи групи` = "Позафракційні",
                          `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "БПП",
                          `Фракція політичної партії "Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України` = "Батьківщина",
                          `Фракція Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                          `Фракція Політичної партії "Опозиційний блок" у Верховній Раді України восьмого скликання` = "Опозиційний блок",
                          `Фракція Радикальної партії Олега Ляшка` = "РПЛ",
                          `Фракція Політичної партії "Об'єднання "САМОПОМІЧ"` = "Самопоміч"))

write.csv(fchanges_tidy, "faction_changes/data/faction_changes_raw.csv", row.names = F)


fchanges <- read.csv("faction_changes/data/faction_changes_raw.csv",
                      stringsAsFactors = F)

fchanges <- fchanges %>% 
  left_join(mps[, c("fullname", "date_oath", "resignation_date")], by = "fullname") %>% 
  mutate_at(vars(faction_start:faction_end), as.Date, format = "%d.%m.%Y") %>% 
  mutate_at(vars(date_oath:resignation_date), as.Date)
    
fchanges$faction_end[is.na(fchanges$faction_end)] <- Sys.Date()
fchanges$faction_start[is.na(fchanges$faction_start)] <- fchanges$date_oath[is.na(fchanges$faction_start)]
fchanges$faction_start[fchanges$faction_start == "2014-10-27"] <- "2014-11-27"

fchanges_sts <- seqformat(fchanges[, 2:5], id = "fullname", begin = "faction_start", end = "faction_end", 
                           status = "faction", from = "SPELL", to = "STS", process = FALSE) 
fchanges_spell <- seqformat(fchanges_sts, id = "fullname", begin = "faction_start", end = "faction_end", 
                             status = "faction", from = "STS", to = "SPELL",
                             right = NA, process = FALSE)
fchanges_spell$states <- as.character(fchanges_spell$states)

fchanges_spell$states[fchanges_spell$states == "*" & fchanges_spell$end-fchanges_spell$begin >= 31] <- "Позафракційний"
fchanges_spell <- fchanges_spell %>% 
  filter(states != "*") %>% 
  mutate(begin = as.Date(begin-1, origin = "2014-11-27"),
         end = as.Date(end-1, origin = "2014-11-27")) 

colnames(fchanges_spell) <- c("fullname", "faction_start", "faction_end", "faction")
fchanges_spell <- fchanges_spell %>% left_join(mps[, c("fullname", "date_oath", "resignation_date")]) %>% 
  mutate(date_oath = as.Date(date_oath),
         resignation_date = as.Date(resignation_date))

fchanges_spell$resignation_date[is.na(fchanges_spell$resignation_date)] <- Sys.Date()

# redundant "without faction" status created for new mps, deal with it

redundant1 <- fchanges_spell %>% 
  group_by(fullname) %>% 
  filter(row_number() == 1 & 
           faction == "Позафракційний" & (faction_end - date_oath < 31))

redundant2 <- fchanges_spell %>% 
  group_by(fullname) %>% 
  filter(row_number() == n() & 
           faction == "Позафракційний" & (resignation_date - faction_start < 31))

fchanges_spell <- fchanges_spell %>% 
  anti_join(redundant1) %>% 
  anti_join(redundant2) %>%
  arrange(fullname, faction_start) %>% 
  select(fullname, faction, faction_start:resignation_date)

rm(redundant1, redundant2)
write.csv(fchanges_spell, "faction_changes/data/faction_changes_full.csv", row.names = F)
