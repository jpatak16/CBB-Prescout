opponentSRurl_db = read.csv("data/opp_url.csv", fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))
opponentList_ESPN = opponentSRurl_db$ESPN_name

headshots = hoopR::load_mbb_player_box(seasons = this_year) %>% 
  select(team_location, athlete_display_name, athlete_jersey, athlete_headshot_href) %>%
  unique() %>%
  filter(team_location %in% opponentList_ESPN) %>%
  mutate(athlete_headshot_href = ifelse(is.na(athlete_headshot_href), "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png", athlete_headshot_href))

write.csv(headshots, file = "data/headshot_url.csv", row.names = FALSE)
pb_upload(file = "data/headshot_url.csv")


#viewable opps
viewable_opps = data.frame()
kp_ot = opponentSRurl_db %>% filter(opponent %in% our_teams)

for(t in kp_ot$KP_name){
  n3g = kp_team_schedule(t, this_year) %>% filter(!is.na(pre_wp))
  n3g = n3g[1:3, 'opponent']
  n3g_fix = opponentSRurl_db %>% filter(KP_name %in% n3g)
  n3g_fix = n3g_fix$opponent
  n3g_this_opp = data.frame(our_team = opponentSRurl_db %>% filter(KP_name == t) %>% pull(opponent), 
                            opp = n3g_fix)
  viewable_opps = rbind(viewable_opps, n3g_this_opp)
  Sys.sleep(15)
}

write.csv(viewable_opps, file = "data/viewable_opps.csv", row.names = FALSE)
pb_upload(file = "data/viewable_opps.csv")

rm(kp_ot, n3g_this_opp, opponentSRurl_db, viewable_opps, n3g, n3g_fix, opponentList_ESPN, t)
