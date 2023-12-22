viewable_opps = read.csv(here::here("data/viewable_opps.csv"))
opponentSRurl_db = read.csv(here::here("data/opp_url.csv"), fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))

season_dates = seq(as.Date("2023-11-06"), as.Date("2024-04-08"), by="days") %>% format(format = "%Y/%m/%d")

#find all games our viewable opps have played this season
games = data.frame()
for(d in season_dates){
  Sys.sleep(20)
  exist = tryCatch({paste0("https://data.ncaa.com/casablanca/scoreboard/basketball-men/d1/", d, "/scoreboard.json") %>% jsonlite::fromJSON(flatten = T)}, 
                   error = function(e) {TRUE})
  if(exist[[1]] == TRUE){next} #go to the next day if there are no games that day
  json <- paste0("https://data.ncaa.com/casablanca/scoreboard/basketball-men/d1/", d, "/scoreboard.json") %>%
    jsonlite::fromJSON(flatten = T)
  if(length(json$games) == 0){next} #go to the next day if there are no games that day
  json = json$games %>% filter(game.gameState=="final")
  games = rbind(games, json)
}

#make names from games df match team names for our app
games = games %>% mutate(away = gsub(" St.", " State", game.away.names.short),
                         home = gsub(" St.", " State", game.home.names.short)) %>% 
  mutate(away = case_when(away=="UConn" ~ "UCONN",
                          .default = away),
         home = case_when(home=="UConn" ~ "UCONN",
                          .default = home)) %>%
  select(gameID = game.gameID, home, away, url = game.url, start_date = game.startDate)

write.csv(games, file = here::here("data/ncaa_games.csv"), row.names = F)
pb_upload(here::here("data/ncaa_games.csv"))

rm(json, opponentSRurl_db, viewable_opps, d, exist, season_dates)
