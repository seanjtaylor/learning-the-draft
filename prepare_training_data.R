library(readr)
library(dplyr)
library(tidyr)
library(feather)
library(stringr)
library(mice) ## multiple data imputation
source('utils.R')

combine.table <- read_feather('data/combines.feather')
draft.table <- read_feather('data/drafts.feather')
college.stats <- read_feather('data/college_stats.feather')

## join on url first, then name
left <- draft.table %>%
  select(year, round, pick, team,
         player,
         college,
         pos,
         age,
         carav,
         drav,
         url) %>%
  mutate(key = ifelse(is.na(url), paste(player, year, sep = '-'), url))

right <- combine.table %>%
  select(year_combine = year,
         player_combine = player,
         pos_combine = pos,
         college_combine = college,
         height,
         weight,
         forty,
         vertical,
         broad,
         bench,
         threecone,
         shuttle,
         url_combine = url) %>%
  mutate(key = ifelse(is.na(url_combine),
           paste(player_combine, year_combine, sep = '-'),
           url_combine)) %>%
  ## This next block filters out multiple rows with the same player
  group_by(key) %>%
  mutate(appearance = row_number()) %>%
  filter(appearance == 1) %>%
  select(-appearance) %>%
  ungroup

combined <- full_join(left, right, by = 'key') %>%
  mutate(player = coalesce2(player, player_combine),
         pos = coalesce2(pos, pos_combine),
         college = coalesce2(college, college_combine),
         year = coalesce2(year, year_combine),
         url = coalesce2(url, url_combine))

## Convert into long format so we can merge with college stats
training1 <- combined %>%
  select(key, carav,
         height, weight,
         forty, vertical,
         bench, age,
         threecone, shuttle,
         broad) %>%
  mutate(height = ifelse(is.na(height), 'NA-NA', height)) %>%
  separate(height, c('feet', 'inches'), sep = '-', convert = TRUE) %>%
  mutate(height = feet * 12 + inches) %>%
  select(-feet, -inches) %>%
  gather(metric, value, carav,
         height, weight,
         forty, vertical,
         bench, age,
         threecone, shuttle,
         broad) %>%
  filter(!is.na(value), value != '') %>%
  mutate(value = as.numeric(value))

## Impute the missing combine data
## A. Convert to wide
training1a <- training1 %>%
  spread(metric, value, fill = NA)

## B. do the imputation and add back the non-imputed columns
training1b <- complete(mice(training1a %>% select(-key, -carav)))
training1b$key <- training1a$key
training1b$carav <- training1a$carav

## C. Convert back to long format
training1c <- training1b %>%
  gather(metric, value, -key)

## Rename some of the collge stats
## make sure we only have one stat per person
training2 <- college.stats %>%
  group_by(url, stat) %>%
  mutate(row = row_number()) %>%
  filter(row == 1) %>%
  select(-row) %>%
  ungroup %>%
  rename(key = url, metric = stat) %>%
  select(-section) %>%
  mutate(metric = str_replace_all(metric, '[.]', '_'))

## Convert back into wide form
training3 <- bind_rows(training1c, training2) %>%
  spread(metric, value, fill = 0) ## note we fill zeros, not NAs

## Join the pick/position/college/year/team back on
## Aggregate smaller schools into representative small school
training <- combined %>%
  select(key, player, pick, pos, college, year, team) %>%
  group_by(college) %>%
  mutate(n_college_picks = n()) %>%
  ungroup %>%
  mutate(short_college = ifelse(n_college_picks < 50, 'SMALL SCHOOL', college),
         pick = ifelse(is.na(pick), 257, as.numeric(pick))) %>%
  inner_join(training3)
  
N <- nrow(training)
train.set <- (rbinom(N, 1, prob = 0.9) == 1 & training$year < 2016)
test.set <- (!train.set & training$year < 2016)
holdout.set <- !(test.set | train.set)

# Outcome variables
pick <- training$pick
carav <- training$carav
first.round <- as.numeric(training$pick <= 32)
