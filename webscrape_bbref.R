library(pacman)
p_load(rvest, tidyverse, janitor)

basic_offense_url = "https://www.sports-reference.com/cbb/seasons/2022-school-stats.html"

basic_offense = read_html(basic_offense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(basic_offense)= c(colnames(basic_offense[,1:7]), paste("offense_", colnames(basic_offense[,8:24]), sep = ""))


basic_defense_url = "https://www.sports-reference.com/cbb/seasons/2022-opponent-stats.html"

basic_defense = read_html(basic_defense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(basic_defense)= c(colnames(basic_defense[,1:7]), paste("defense_", colnames(basic_defense[,8:24]), sep = ""))


advanced_offense_url = "https://www.sports-reference.com/cbb/seasons/2022-advanced-school-stats.html"

advanced_offense = read_html(advanced_offense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(advanced_offense)= c(colnames(advanced_offense[,1:7]), paste("offense_", colnames(advanced_offense[,8:20]), sep = ""))


advanced_defense_url = "https://www.sports-reference.com/cbb/seasons/2022-advanced-opponent-stats.html"

advanced_defense = read_html(advanced_defense_url) %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>% as.data.frame() %>%
  .[,-c(9,12,15,18,21)] %>%
  row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
  filter(!g=='G') %>% filter(!g=="Overall") %>%
  mutate(school = ifelse(endsWith(school, "NCAA"), substr(school, 1, nchar(school)-5), school)) %>%
  .[,-c(1, 9:16)]
colnames(advanced_defense)= c(colnames(advanced_defense[,1:7]), paste("defense_", colnames(advanced_defense[,8:20]), sep = ""))


team_stats = left_join(basic_offense, basic_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
team_stats = left_join(team_stats, advanced_offense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
team_stats = left_join(team_stats, advanced_defense, by=c("school", "g", "w", "l", "w_l_percent", "srs", "sos"))
