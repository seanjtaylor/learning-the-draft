library(rvest)
library(readr)
library(dplyr)
library(RCurl)
library(tidyr)
library(stringr)
library(feather)

read_html_cache <- function(url, cache.dir = 'cache') {
  fn <- tail(strsplit(url, '/')[[1]], 1)
  fn.path <- paste(cache.dir, fn, sep = '/')
  if (!file.exists(fn.path)) {
    text <- getURL(url)
    write(text, fn.path)
  }
  read_html(fn.path)
}

draft.header <- c('round', 'pick', 'team', 'player', 'pos', 'age', 'to', 'ap1', 'pb', 'st', 'carav', 'drav', 'games', 'pass.cmp', 'pass.att', 'pass.yds', 'pass.tds', 'pass.ints', 'rush.att', 'rush.yds', 'rush.tds', 'receptions', 'rec.yds', 'rec.tds', 'tackles', 'ints', 'sacks', 'college', 'stats')

combine.header <- c('player', 'pos', 'college', 'stats', 'height', 'weight', 'forty', 'vertical', 'bench', 'broad', 'threecone', 'shuttle', 'drafted')

url.extract <- function(tds) {
  results <- c()
  for(td in tds) {
    children <- html_children(td)
    if (length(children) == 0) {
      results <- c(results, NA)
    } else{
      results <- c(results, (html_attr(html_children(td), 'href')))
    }
  }
  results
}

headers <- list()
headers[['defense']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'solo.tackes', 'ast.tackles', 'tackles', 'loss.tackles', 'sacks', 'int', 'int.yards', 'int.yards.avg', 'int.td', 'pd', 'fum.rec', 'fum.yds', 'fum.tds', 'fum.forced')
headers[['scoring']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'td.rush', 'td.rec', 'td.int', 'td.fr', 'td.pr', 'td.kr', 'td.oth', 'td.tot', 'kick.xpm', 'kick.fgm', 'twopm', 'safety', 'total.pts')
headers[['punt_ret']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'punt.returns', 'punt.return.yards', 'punt.return.avg', 'punt.return.td', 'kick.returns', 'kick.return.yards', 'kick.return.avg', 'kick.return.td')
headers[['receiving']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'receptions', 'rec.yards', 'rec.avg', 'rec.td', 'rush.att', 'rush.yds', 'rush.avg', 'rush.td', 'scrim.plays', 'scrim.yds', 'scrim.avg', 'scrim.tds')
headers[['rushing']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'receptions', 'rec.yards', 'rec.avg', 'rec.td', 'rush.att', 'rush.yds', 'rush.avg', 'rush.td', 'scrim.plays', 'scrim.yds', 'scrim.avg', 'scrim.tds')
headers[['passing']] <- c('year', 'school', 'conf', 'class', 'pos', 'games', 'completions', 'attempts', 'comp.pct', 'pass.yards', 'yards.per.attempt', 'adj.yards.per.attempt', 'pass.tds', 'pass.ints', 'int.rate')

parse_pfr_tables <- function(tables) {
  results = list()
  for (tbl in tables) {
    id <- html_attr(tbl, 'id')
    if (id %in% names(headers)) {
      
      df <- html_table(tbl) %>%
        head(-1) %>% tail(-1)

      if(ncol(df) == length(headers[[id]])) {
        colnames(df) <- headers[[id]]
      } else {
        next;
      }
      
      melted <- df %>%
        select(-year, -school, -conf, -class, -pos) %>%
        mutate(seasons = 1) %>%
        gather(stat, value) %>%
        mutate(stat = as.character(stat)) %>%
        filter(value != '') %>%
        mutate(value = as.numeric(value),
               section = id)

      
      results[[id]] <- melted
    }
  }
  bind_rows(results)
}

if (!file.exists('data/drafts.feather')) {
  
draft.table <- data_frame(year = 2000:2015) %>%
  group_by(year) %>% do({
    url <- paste('http://www.pro-football-reference.com/years/', .$year, '/draft.htm', sep ='')
    doc <- read_html(url)
    html.table <- doc %>%
      html_nodes('table') %>%
      first
    urls <- html.table %>%
      html_nodes('tr td:nth-child(29)') %>%
      url.extract
    my.table <- html_table(html.table)
    colnames(my.table) <- draft.header
    my.table <- my.table %>%
      filter(pos != 'Pos') %>%
      mutate(url = urls)
    my.table
  }) %>%
  ungroup
write_feather(draft.table, 'data/drafts.feather')

}

if (!file.exists('data/combines.feather')) {
  
combine.table <- data_frame(year = 2000:2016) %>%
  group_by(year) %>% do({
    url <- paste('http://www.pro-football-reference.com/draft/', .$year, '-combine.htm', sep ='')
    html.table <- read_html(url) %>%
      html_nodes('table') %>%
      first
    urls <- html.table %>%
      html_nodes('tr td:nth-child(4)') %>%
      url.extract
    my.table <- html_table(html.table)
    colnames(my.table) <- combine.header
    my.table <- my.table %>%
      filter(pos != 'Pos') %>%
      mutate(url = urls)
    my.table
  }) %>%
  ungroup

write_feather(combine.table, 'data/combines.feather')
}

all.urls <- combine.table %>%
  select(url) %>%
  full_join(draft.table %>% select(url)) %>%
  filter(!is.na(url))

college.stats <- all.urls %>%
  group_by(url) %>% do({
    #cat('URL = ', .$url, '\n')
    doc <- read_html_cache(.$url)
    stats <- doc %>%
      html_nodes('table') %>%
      parse_pfr_tables
    if (nrow(stats) > 0) {
      stats <- stats %>%
        group_by(section, stat) %>%
        summarise(value = sum(value))
    }
    stats
  })
  
write_feather(college.stats, 'data/college_stats.feather')
