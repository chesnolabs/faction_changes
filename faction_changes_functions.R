# rm(list=ls())
library(tidyverse)
library(stringr)
library(rvest)
library(jsonlite)
setwd("C:/Users/user/Desktop/scripts")

get_all_mps <- function(){
  mps <- fromJSON(readLines(file("http://data.rada.gov.ua/ogd/mps/skl8/mps-data.json",
                                 encoding = "UTF-16")))
  mps_list <- mps
  mps <- mps_list[[1]]
  mps$fullname <- with(mps, paste(surname, firstname, patronymic))
  mps$shortname <- paste0(mps$surname, " ", 
                          str_sub(mps$firstname, 1, 1), ".", 
                          str_sub(mps$patronymic, 1, 1), ".")
  mps$shortname <- gsub("'", "’", mps$shortname)
  mps$shortname[mps$fullname == "Тимошенко Юлія Володимирівна"] <- "Тимошенко Юлія В."
  mps$shortname[mps$fullname == "Тимошенко Юрій Володимирович"] <- "Тимошенко Юрій В."
  mps$fullname[mps$surname == "Найєм"] <- "Найєм Мустафа-Масі"
  mps$shortname[mps$surname == "Найєм"] <- "Найєм М. ."
  mps$fullname[mps$surname == "Джемілєв"] <- "Джемілєв Мустафа"
  mps$shortname[mps$surname == "Джемілєв"] <- "Джемілєв М. ."
  mps$absence_s <- with(mps,
                        round(presentAuto_absent/(presentAuto_absent+presentAuto_present)*100, 1))
  return(mps)
}

get_factions <- function(){
  factions <- read.csv("tour/data/mps_facts.csv")
  levels(factions$faction) <- c("Воля народу", "Відродження",
                                "Позафракційні", "Блок Петра Порошенка", "Батьківщина", "Народний фронт",
                                "Самопоміч", "Опозиційний блок", "Радикальна партія Ляшка")
  colnames(factions)[1] <- "fullname"
  return(factions)
}

changes_pairs <- function(df, count = T){
  
  more_than_one <- df %>% group_by(fullname) %>%
    count() %>% filter(n > 1) %>% 
    select(fullname) %>% unlist()
  
  fchanges_yes <- df[df$fullname %in% more_than_one,]
  
  destinations <- fchanges_yes %>% 
    group_by(fullname) %>% 
    slice(-1) %>% 
    ungroup() %>% 
    select(faction) %>% 
    unlist()
  
  origins <- fchanges_yes %>% 
    group_by(fullname) %>% 
    slice(-n()) %>%
    ungroup() %>% 
    select(faction) %>% 
    unlist()
  
  dates <- fchanges_yes %>% 
    group_by(fullname) %>% 
    slice(-1) %>% 
    ungroup() %>% 
    select(faction_start) %>% 
    unlist()
  
  mazhor <- fchanges_yes %>% 
    group_by(fullname) %>% 
    slice(-1) %>% 
    ungroup() %>% 
    select(mazhor) %>% 
    unlist()
  
  dest_or <- data.frame(origins = origins,
                        destinations = destinations,
                        date = dates,
                        mazhor = mazhor,
                        stringsAsFactors = F)
  
  if(count == F){
    return(dest_or)
  } else {
    dest_or_count <- dest_or %>%
      group_by(origins, destinations) %>%
      summarize(counts = n()) %>%
      ungroup() %>%
      arrange(desc(counts))
    return(dest_or_count)
  }}

get_mps4 <- function(){
  mps4 <- fromJSON(readLines(file("http://data.rada.gov.ua/ogd/mps/skl4/mps-data.json",
                                  encoding = "UTF-16")))
  str(mps4)
  mps_list <- mps4
  mps4 <- mps_list[[1]]
  mps4$fullname <- with(mps4, paste(surname, firstname, patronymic))
  mps4$shortname <- paste0(mps4$surname, " ", 
                           str_sub(mps4$firstname, 1, 1), ".", 
                           str_sub(mps4$patronymic, 1, 1), ".")
  mps4$shortname <- gsub("'", "’", mps4$shortname)
  mps4$fullname[mps4$surname == "Джемілєв"] <- "Джемілєв Мустафа"
  mps4$shortname[mps4$surname == "Джемілєв"] <- "Джемілєв М."
  return(mps4)
}

get_mps7 <- function(){
  mps7 <- fromJSON(readLines(file("http://data.rada.gov.ua/ogd/mps/skl7/mps-data.json",
                                  encoding = "UTF-16")))
  str(mps7)
  mps_list <- mps7
  mps7 <- mps_list[[1]]
  mps7$fullname <- with(mps7, paste(surname, firstname, patronymic))
  mps7$shortname <- paste0(mps7$surname, " ", 
                           str_sub(mps7$firstname, 1, 1), ".", 
                           str_sub(mps7$patronymic, 1, 1), ".")
  mps7$shortname <- gsub("'", "’", mps7$shortname)
  mps7$fullname[mps7$surname == "Джемілєв"] <- "Джемілєв Мустафа"
  mps7$shortname[mps7$surname == "Джемілєв"] <- "Джемілєв М."
  return(mps7)
}