## Data wrangling
# Author: TIM
# Date: December 2021


## Load packages ----

pkg_list <- c("dplyr", "tidyr", "igraph", "sf", "ggplot2", "gridExtra", "assertthat")

lapply(pkg_list, FUN = function(pkg_list) {
  do.call("require", list(pkg_list)) 
})


## Load data ----

# 'cluster' is synonym with 'site=node'
data_raw <- read.csv("../data/datos_nov2020_colonia_lobos.csv", sep = ",", header = TRUE)

## Curate data to generate site (node) & movement (link) lists

## Site lists ----

nodes_all <- data_raw %>%
  rename(site = cluster) %>%
  select(id, hour, lon, lat, distppa, colonia, site, distlobos, lobos) %>%
  add_count(site) %>%
  rename(frequency = n) %>%
  distinct(site, hour, id, .keep_all = TRUE) %>%
  mutate(site.dup = ifelse(duplicated(site) | duplicated(site, fromLast = TRUE),
                           "TRUE", "FALSE")) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(no.ind = nrow(data.frame(site))) %>%
  mutate(time = if_else(site.dup == "TRUE", "both", hour)) %>%
  mutate(time = recode(time, "16:00:00" = "day", "04:00:00" = "night")) %>%
  rename(dist.col = distppa, dist.lob = distlobos) %>%
  distinct(site, .keep_all = TRUE) %>%
  group_by(site, lon, lat, time, colonia, dist.col, dist.lob, lobos, frequency, no.ind) %>%
  summarise()


## Site list filtered by 'colonia' site
# I chose the most visited (highest 'frequency') site as colony site
# Then I incorporated 'frequency' and 'no.ind' data into the colony site
nodes_colonia <- nodes_all %>%
  group_by(colonia) %>%
  filter(colonia == 0 | frequency == max(frequency))
nodes_colonia$frequency[nodes_colonia$colonia == 1] <- sum(nodes_all[nodes_all$colonia == 1,]$frequency)
nodes_colonia$no.ind[nodes_colonia$colonia == 1] <- 5

# Day and night lists
nodes_day <- data_raw %>%
  filter(hour == "16:00:00") %>%
  rename(site = cluster) %>%
  select(id, hour, lon, lat, distppa, colonia, site, distlobos, lobos) %>%
  add_count(site) %>%
  rename(frequency = n) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  mutate(site.dup = ifelse(duplicated(site) | duplicated(site, fromLast = TRUE),
                           "TRUE", "FALSE")) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(no.ind = nrow(data.frame(site))) %>%
  rename(dist.col = distppa, dist.lob = distlobos) %>%
  distinct(site, .keep_all = TRUE) %>%
  rename(time = hour) %>%
  group_by(site, lon, lat, time, colonia, dist.col, dist.lob, lobos, frequency, no.ind) %>%
  summarise()
nodes_col_day <- nodes_day %>%
  group_by(colonia) %>%
  filter(colonia == 0 | frequency == max(frequency))
nodes_col_day$frequency[nodes_col_day$colonia == 1] <- sum(nodes_day[nodes_day$colonia == 1,]$frequency)
nodes_col_day$no.ind[nodes_col_day$colonia == 1] <- 5

nodes_night <- data_raw %>%
  filter(hour == "04:00:00") %>%
  rename(site = cluster) %>%
  select(id, hour, lon, lat, distppa, colonia, site, distlobos, lobos) %>%
  add_count(site) %>%
  rename(frequency = n) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  mutate(site.dup = ifelse(duplicated(site) | duplicated(site, fromLast = TRUE),
                           "TRUE", "FALSE")) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(no.ind = nrow(data.frame(site))) %>%
  rename(dist.col = distppa, dist.lob = distlobos) %>%
  distinct(site, .keep_all = TRUE) %>%
  rename(time = hour) %>%
  group_by(site, lon, lat, time, colonia, dist.col, dist.lob, lobos, frequency, no.ind) %>%
  summarise()
nodes_col_night <- nodes_night %>%
  group_by(colonia) %>%
  filter(colonia == 0 | frequency == max(frequency))
nodes_col_night$frequency[nodes_col_night$colonia == 1] <- sum(nodes_night[nodes_night$colonia == 1,]$frequency)
nodes_col_night$no.ind[nodes_col_night$colonia == 1] <- 5

# Pre and pos-penguin lists (2020-03-10)
nodes_prepen <- data_raw %>%
  filter(date <= "2020-03-10") %>%
  rename(site = cluster) %>%
  select(id, hour, lon, lat, distppa, colonia, distlobos, lobos, site) %>%
  add_count(site) %>%
  rename(frequency = n) %>%
  distinct(site, hour, id, .keep_all = TRUE) %>%
  mutate(site.dup = ifelse(duplicated(site) | duplicated(site, fromLast = TRUE),
                           "TRUE", "FALSE")) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(no.ind = nrow(data.frame(site))) %>%
  mutate(time = if_else(site.dup == "TRUE", "both", hour)) %>%
  mutate(time = recode(time, "16:00:00" = "day", "04:00:00" = "night")) %>%
  rename(dist.col = distppa, dist.lob = distlobos) %>%
  distinct(site, .keep_all = TRUE) %>%
  group_by(site, lon, lat, time, colonia, dist.col, dist.lob, lobos, frequency, no.ind) %>%
  summarise()
nodes_col_prepen <- nodes_prepen %>%
  group_by(colonia) %>%
  filter(colonia == 0 | frequency == max(frequency))
nodes_col_prepen$site[nodes_col_prepen$colonia == 1] <- 10
nodes_col_prepen$frequency[nodes_col_prepen$colonia == 1] <- sum(nodes_prepen[nodes_prepen$colonia == 1,]$frequency)

nodes_pospen <- data_raw %>%
  filter(date > "2020-03-10") %>%
  rename(site = cluster) %>%
  select(id, hour, lon, lat, distppa, colonia, distlobos, lobos, site) %>%
  add_count(site) %>%
  rename(frequency = n) %>%
  distinct(site, hour, id, .keep_all = TRUE) %>%
  mutate(site.dup = ifelse(duplicated(site) | duplicated(site, fromLast = TRUE),
                           "TRUE", "FALSE")) %>%
  distinct(site, id, .keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(no.ind = nrow(data.frame(site))) %>%
  mutate(time = if_else(site.dup == "TRUE", "both", hour)) %>%
  mutate(time = recode(time, "16:00:00" = "day", "04:00:00" = "night")) %>%
  rename(dist.col = distppa, dist.lob = distlobos) %>%
  distinct(site, .keep_all = TRUE) %>%
  group_by(site, lon, lat, time, colonia, dist.col, dist.lob, lobos, frequency, no.ind) %>%
  summarise()
nodes_col_pospen <- nodes_pospen %>%
  group_by(colonia) %>%
  filter(colonia == 0 | frequency == max(frequency))
nodes_col_pospen$frequency[nodes_col_pospen$colonia == 1] <- sum(nodes_pospen[nodes_pospen$colonia == 1,]$frequency)

# Site lists by Individual
# Ind A == "49127", Ind B == "49138", Ind C == "49152", Ind D == "49178", Ind E == "49180"
nodes_indA <- data_raw %>%
  mutate(ind = factor(id, levels = c("49127", "49138", "49152", "49178", "49180"),
                      labels = c("A", "B", "C", "D", "E"))) %>%
  filter(ind == "A") %>%  # replace letter to refer to another ind
  rename(site = cluster) %>%
  mutate(time = recode(hour, "16:00:00" = "day", "04:00:00" = "night")) %>%
  select(ind, time, lon, lat, distppa, colonia, distlobos, lobos, site) %>%
  add_count(site) %>%
  rename(frequency = n) %>%
  distinct(site, time, ind, .keep_all = TRUE) %>%
  mutate(site.dup = ifelse(duplicated(site) | duplicated(site, fromLast = TRUE),
                           "TRUE", "FALSE")) %>%
  distinct(site, ind, .keep_all = TRUE) %>%
  group_by(site) %>%
  mutate(time = if_else(site.dup == "TRUE", "both", time)) %>%
  rename(dist.col = distppa, dist.lob = distlobos) %>%
  distinct(site, .keep_all = TRUE) %>%
  group_by(site, lon, lat, time, colonia, dist.col, dist.lob, lobos, frequency, ind) %>%
  summarise()
nodes_indA$site[nodes_indA$colonia == 1] <- 10  # replace letter to refer to another ind
nodes_indA$frequency[nodes_indA$colonia == 1] <- sum(nodes_indA[nodes_indA$colonia == 1,]$frequency)
nodes_indA <- nodes_indA[!duplicated(nodes_indA$site),]


## Different site=node lists
# All sites
nodes_all
# Day
nodes_all_day <- nodes_all[nodes_all$time == 'day', ]
# Night
nodes_all_night <- nodes_all[nodes_all$time == 'night', ]
# Both (sites visited during day & night)
nodes_all_both <- nodes_all[nodes_all$time == 'both', ]
# Colony
nodes_colonia
nodes_col_day
nodes_col_night
nodes_col_prepen
nodes_col_pospen

# Individual
nodes_indA
nodes_indB
nodes_indC
nodes_indD
nodes_indE

## Export site lists as .csv files
# write.csv(list_name, file = "../data/list_name.csv")


## Movement (link) lists ----

links_all <- data_raw %>%
  rename(site = cluster) %>%
  select(id, date, lon, lat, colonia, site) %>%
  mutate(ind = factor(id, levels = c("49127", "49138", "49152", "49178", "49180"),
                      labels = c("A", "B", "C", "D", "E"))) %>%
  select(ind, date, lon, lat, colonia, site) %>%
  mutate(from = paste(site), to = NA) %>%
  arrange(ind, date) %>%
  mutate(to = ifelse(is.na(to), lead(from), to)) %>%
  group_by(ind) %>%
  mutate(to = replace(to, n(), NA)) %>%
  distinct(from, to, .keep_all = TRUE) %>%
  select(from, to, ind, date) %>%
  drop_na()

# Link list filtered by 'colonia' site
# Site #10 was considered as colony site, so I replaced all [colonia == 1] sites
# from 'links_all' with #10 site
links_col <- links_all
from <-  c(1:9, 11:34, 36:38, 40:41, 43:46, 48:49, 55, 58:59,
           63:65, 67, 69, 78:80, 82:84, 92, 96, 123)
to <- 10
for (i in seq_along(from))links_col$from[links_col$from==from[i]] <- to
for (i in seq_along(from))links_col$to[links_col$to==from[i]] <- to
links_colonia <- links_col %>% distinct(from, to, .keep_all = T)

# Pre and pos-penguin lists (2020-03-10)
links_col_prepen <- links_colonia %>%
  filter(date <= "2020-03-10")
links_col_pospen <- links_colonia %>%
  filter(date > "2020-03-10")

# Link lists by Individual
links_indA <- links_colonia %>%
  filter(ind == "A")
links_indB <- links_colonia %>%
  filter(ind == "B")
links_indC <- links_colonia %>%
  filter(ind == "C")
links_indD <- links_colonia %>%
  filter(ind == "D")
links_indE <- links_colonia %>%
  filter(ind == "E")


## Different link lists
# All links
links_all
# Links filtered by 'colonia'
links_colonia
links_col_prepen
links_col_pospen
links_indA
links_indB
links_indC
links_indD
links_indE

## Export movement list as .csv file
# write.csv(list_name, file = "../data/list_name.csv")


## Save curated data ----

save(data_raw, nodes_all, nodes_colonia, nodes_col_day, nodes_col_night, nodes_col_prepen,
     nodes_col_pospen, nodes_indA, nodes_indB, nodes_indC, nodes_indD, nodes_indE,
     links_all, links_colonia, links_col_prepen, links_col_pospen, links_indA, links_indB,
     links_indC, links_indD, links_indE,
     file = "../data/data_curated.rda")
