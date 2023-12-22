viewable_opps = read.csv(here::here("data/viewable_opps.csv"))
opponentSRurl_db = read.csv(here::here("data/opp_url.csv"), fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))

games = read.csv(here::here("data/ncaa_games.csv"))

opp_games = games %>% filter(away %in% viewable_opps$opp | home %in% viewable_opps$opp)

rotation_times = data.frame()
for(t in viewable_opps$opp){
  t_games = opp_games %>% filter(away==t | home==t)
  if(nrow(t_games)==0){next}
  for(g in 1:nrow(t_games)){
    g_id = t_games[g, 'url'] %>% gsub("/game/", "", .) %>% as.numeric()
    h_v = ifelse(t_games[g, 'home'] == t, "home", "visitor")
    h_a = ifelse(h_v == "home", "home", "away")
    h_a_opp = ifelse(h_v == "home", "away", "home")
    pbp = parse_pbp(scrape_pbp(g_id))
    if(nrow(pbp) == 0){Sys.sleep(20)}
    if(nrow(pbp) == 0){next}
    pbp = pbp %>% filter(periodNumber == 1 | periodNumber == 2) %>%
      #convert character stint times to numeric
      separate_wider_delim(delim = " ", names = c("mins_stint_start", "secs_stint_start"), cols = stint_start, too_few = "align_end") %>%
      mutate(mins_stint_start = gsub("M", "", mins_stint_start) %>% as.numeric(),
             secs_stint_start = gsub("S", "", secs_stint_start) %>% as.numeric(),
             mins_stint_start = ifelse(is.na(mins_stint_start), 0, mins_stint_start)) %>%
      separate_wider_delim(delim = " ", names = c("mins_stint_end", "secs_stint_end"), cols = stint_end, too_few = "align_end") %>%
      mutate(mins_stint_end = gsub("M", "", mins_stint_end) %>% as.numeric(),
             secs_stint_end = gsub("S", "", secs_stint_end) %>% as.numeric(),
             mins_stint_end = ifelse(is.na(mins_stint_end), 0, mins_stint_end)) %>%
      #create columns for stint start and end only using seconds as a unit
      mutate(periodSeconds = ifelse(periodNumber == 1, 1200, 0)) %>%
      mutate(stint_start_s = periodSeconds + (mins_stint_start*60) + secs_stint_start,
             stint_end_s = periodSeconds + (mins_stint_end*60) + secs_stint_end)
    #fix error in pbp_parse where stint end times are not correct
    for(r in 1:nrow(pbp)){
      next_row_start = pbp[r+1, "stint_start_s"] %>% as.numeric()
      pbp[r, "stint_end_s"] = case_when(is.na(next_row_start) ~ 0,
                                        .default = next_row_start)
    }
    pbp = pbp %>% select(on_court = paste0("on_court_", h_v), stint_start_s, stint_end_s) %>%
      separate_longer_delim(cols = on_court, delim = ";") %>%
      mutate(on_court = gsub("x_", "", on_court) %>% standardize_name()) %>%
      mutate(g_date = t_games[g, "start_date"],
             opp = t_games[g, h_a_opp],
             team = t_games[g, h_a])
    rotation_times = rbind(rotation_times, pbp)
    Sys.sleep(20)
  }
}

write.csv(rotation_times, file = here::here("data/rotation_times.csv"), row.names = F)
pb_upload(here::here("data/rotation_times.csv"))


rm(opp_games, opponentSRurl_db, pbp, t_games, viewable_opps, g, g_id, h_a, h_a_opp, h_v, next_row_start, r, t)
