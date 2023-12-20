viewable_opps = read.csv("data/viewable_opps.csv")
opponentSRurl_db = read.csv("data/opp_url.csv", fileEncoding = "ISO-8859-1") %>% 
  mutate(ESPN_name = ifelse(ESPN_name == "", opponent, ESPN_name),
         KP_name = ifelse(KP_name == "", gsub(" State", " St.", opponent), KP_name))

headshot_urls_db = read.csv("data/headshot_url.csv") %>%
  mutate(athlete_headshot_href = ifelse(athlete_headshot_href == "", "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png", athlete_headshot_href))

PPT_data = read.csv("data/PPT_data.csv", check.names = FALSE)

player_totals_url = 'https://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2024&year_max=2024&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_f=Y&pos_is_c=Y&games_type=A&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=pts&order_by_asc=&offset='
offset = seq(0, 6000, by = 100)

player_totals = data.frame()
for(num in offset){
  pt_url = paste0(player_totals_url, num)
  raw_pt = read_html(pt_url) %>% html_table()
  if(length(raw_pt) == 0){break}
  player_totals_partial = raw_pt %>% .[[1]] %>% as.data.frame() %>%
    row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
    filter(rk != "") %>% filter(rk != "Rk")
  player_totals = rbind(player_totals, player_totals_partial)
  Sys.sleep(30)
}
player_totals = player_totals %>% unique() %>% select(-rk)

#scrape ncaa box score advanced
player_advanced_url = 'https://www.sports-reference.com/cbb/play-index/psl_finder.cgi?request=1&match=single&year_min=2024&year_max=2024&conf_id=&school_id=&class_is_fr=Y&class_is_so=Y&class_is_jr=Y&class_is_sr=Y&pos_is_g=Y&pos_is_f=Y&pos_is_c=Y&games_type=A&qual=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2val=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&order_by=per&order_by_asc=&offset='

player_advanced = data.frame()
for(num in offset){
  pa_url = paste0(player_advanced_url, num)
  raw_pa = read_html(pa_url) %>% html_table()
  if(length(raw_pa) == 0){break}
  player_advanced_partial = raw_pa %>% .[[1]] %>% as.data.frame() %>%
    row_to_names(row_number = 1, remove_row = T) %>% clean_names() %>%
    filter(rk != "") %>% filter(rk != "Rk")
  player_advanced = rbind(player_advanced, player_advanced_partial)
  Sys.sleep(30)
}
player_advanced = player_advanced %>% unique() %>% select(-rk)

ncaa_player_stats = player_totals %>% full_join(player_advanced) %>%
  mutate_at(vars('g':'bpm'), as.numeric) %>%
  select(-c('season', 'conf', 'p_prod')) %>%
  rename(team = school)

#load in a saved dataset of nba total and advanced stats
nba_player_stats = read.csv("data/nba_stats.csv") %>%
  clean_names() %>% 
  mutate(class = 'NBA') %>%
  #match names to ncaa table
  select(player, class, pos, team, g, mp, fg, fga, fg_percent=fg_2, x2p, x2pa, x3p, x3pa, x3p_percent=x3p_2, ft, fta, ft_percent=ft_2,
         orb, drb, trb, ast, stl, blk, tov, pf, pts, per, ts_percent=ts, e_fg_percent=e_fg, orb_percent=orb_2, drb_percent=drb_2, 
         trb_percent=trb_2, ast_percent=ast_2, stl_percent=stl_2, blk_percent=blk_2, tov_percent=tov_2, usg_percent=usg, o_rtg,
         d_rtg, ows, dws, ws, obpm, dbpm, bpm)

#min and gp qualifyer threshold
gp_q = ncaa_player_stats %>%
  pull(g) %>% max()*.4
min_q = ncaa_player_stats %>%
  pull(mp) %>% max()*.2

#join nba and ncaa tables together
all_player_stats = rbind(ncaa_player_stats, nba_player_stats) %>%
  filter((class=="NBA" & g>=10 & mp>=400) | (class!="NBA" & g>=gp_q & mp>=min_q)) %>%
  mutate(x3p_percent = ifelse(is.na(x3p_percent), 0, x3p_percent)) %>%
  arrange(desc(mp)) %>%
  mutate(player_join = standardize_name(player))

#in the future, this is where I will add a part to change some listed player positions

#separate positions into three groups
guard_stats = all_player_stats %>% filter(pos=='G' | pos=='G-F')
wing_stats = all_player_stats %>% filter(pos=='F' | pos=='F-G')
big_stats = all_player_stats %>% filter(pos=='C' | pos=='C-F' | pos=='F-C')

#calculate play styles and percentiles for each league and each position
nba_guard_styles = guard_stats %>% 
  filter(class=="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url=NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, perim_defender, oreb, dreb, 
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank)

ncaa_guard_styles = guard_stats %>% 
  filter(class!="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, perim_defender, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank)

nba_wing_styles = wing_stats %>% 
  filter(class=="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = (x2pa/fga) + (fta/fga),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, perim_defender, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank)

ncaa_wing_styles = wing_stats %>% 
  filter(class!="NBA") %>%
  mutate(x3_p_s = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         slasher = ifelse(fga==0, 0, (x2pa/fga) + (fta/fga)),
         ball_dominant = usg_percent,
         playmaker = ast_percent - .5*(usg_percent),
         dbpm_pct = percent_rank(dbpm),
         perim_defender = dbpm_pct/2 + stl_percent/10,
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, x3_p_s, slasher, ball_dominant, playmaker, perim_defender, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank)

nba_big_styles = big_stats %>% 
  filter(class=="NBA") %>%
  mutate(stretch_big = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         draws_fouls = (fta/fga),
         passing_big = ast_percent,
         dbpm_pct = percent_rank(dbpm),
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, stretch_big, draws_fouls, passing_big, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank)

ncaa_big_styles = big_stats %>% 
  filter(class!="NBA") %>%
  mutate(stretch_big = (x3p * 3)/pts + x3p_percent + (x3pa/mp),
         draws_fouls = (fta/fga),
         passing_big = ast_percent,
         dbpm_pct = percent_rank(dbpm),
         rim_protector = dbpm_pct/2 + blk_percent/10,
         oreb = orb_percent,
         dreb = drb_percent,
         sim_1=NA, sim_1_url = NA, sim_2=NA, sim_3=NA, sim_4=NA, sim_5=NA) %>%
  select(player, player_join, class, pos, team, stretch_big, draws_fouls, passing_big, rim_protector, oreb, dreb,
         sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5) %>%
  mutate_if(is.numeric, percent_rank)

styles_list = list(list(ncaa_guard_styles, nba_guard_styles), 
                   list(ncaa_wing_styles, nba_wing_styles), 
                   list(ncaa_big_styles, nba_big_styles))


# #PCA
# ncaa_guard_pca = styles_list[[1]][[1]] %>% select(x3_p_s:dreb) %>% PCA() %>% .$ind %>% .$coord %>% as.data.frame() %>% .[,1:4]
# nba_guard_pca = styles_list[[1]][[2]] %>% select(x3_p_s:dreb) %>% PCA() %>% .$ind %>% .$coord %>% as.data.frame() %>% .[,1:4]
# ncaa_wing_pca = styles_list[[2]][[1]] %>% select(x3_p_s:dreb) %>% PCA() %>% .$ind %>% .$coord %>% as.data.frame() %>% .[,1:4]
# nba_wing_pca = styles_list[[2]][[2]] %>% select(x3_p_s:dreb) %>% PCA() %>% .$ind %>% .$coord %>% as.data.frame() %>% .[,1:4]
# ncaa_big_pca = styles_list[[3]][[1]] %>% select(stretch_big:dreb) %>% PCA() %>% .$ind %>% .$coord %>% as.data.frame() %>% .[,1:4]
# nba_big_pca = styles_list[[3]][[2]] %>% select(stretch_big:dreb) %>% PCA() %>% .$ind %>% .$coord %>% as.data.frame() %>% .[,1:4]
# 
# #add pca vars back to styles list
# styles_list = list(list(cbind(ncaa_guard_styles, ncaa_guard_pca),
#                         cbind(nba_guard_styles, nba_guard_pca)),
#                    list(cbind(ncaa_wing_styles, ncaa_wing_pca),
#                         cbind(nba_wing_styles, nba_wing_pca)),
#                    list(cbind(ncaa_big_styles, ncaa_big_pca),
#                         cbind(nba_big_styles, nba_big_pca)))

#only find similar players for players on viewable opp teams
styles_list[[1]][[1]] = styles_list[[1]][[1]] %>% filter(team %in% viewable_opps$opp)
styles_list[[2]][[1]] = styles_list[[2]][[1]] %>% filter(team %in% viewable_opps$opp)
styles_list[[3]][[1]] = styles_list[[3]][[1]] %>% filter(team %in% viewable_opps$opp)

#for the desired ncaa athletes, find their most alike nba comparison
sim_list = list(); z=1
for(a in styles_list){
  n_ncaa_players = nrow(a[[1]])
  for(player in 1:n_ncaa_players){
    similarity_vec = dist(rbind(a[[1]][player,] %>% select(-player:-team) %>% select(-sim_1:-sim_5), 
                                a[[2]] %>% select(-player:-team) %>% select(-sim_1:-sim_5)))[1:nrow(a[[2]])]
    top_5 = head(order(similarity_vec), 5)
    top_5 = a[[2]][top_5, 'player']
    a[[1]][player, 'sim_1'] = top_5[1]
    sim_1_url = hoopR::nba_playerheadshot(player_id = 
                                            (hoopR::nba_commonallplayers()$CommonAllPlayers %>% as.data.frame() %>% 
                                               filter(DISPLAY_FIRST_LAST == iconv(top_5[1], to="ASCII//TRANSLIT") %>% unlist()) %>% 
                                               select(PERSON_ID) %>% unlist() %>% as.character()))
    Sys.sleep(10)
    a[[1]][player, 'sim_1_url'] = sim_1_url
    a[[1]][player, 'sim_2'] = top_5[2]
    a[[1]][player, 'sim_3'] = top_5[3]
    a[[1]][player, 'sim_4'] = top_5[4]
    a[[1]][player, 'sim_5'] = top_5[5]
  }
  sim_list[[z]] = a
  z = z+1
}

ncaa_guard_sim = sim_list[[1]][[1]] %>% left_join(PPT_data %>% select(player_join, URL, '#'), by = "player_join") %>% select(-player_join)
ncaa_wing_sim = sim_list[[2]][[1]] %>% left_join(PPT_data %>% select(player_join, URL, '#'), by = "player_join") %>% select(-player_join)
ncaa_big_sim = sim_list[[3]][[1]] %>% left_join(PPT_data %>% select(player_join, URL, '#'), by = "player_join") %>% select(-player_join)


write.csv(ncaa_guard_sim, file = "data/ncaa_guard_sim.csv", row.names = FALSE)
pb_upload("data/ncaa_guard_sim.csv")

write.csv(ncaa_wing_sim, file = "data/ncaa_wing_sim.csv", row.names = FALSE)
pb_upload("data/ncaa_wing_sim.csv")

write.csv(ncaa_big_sim, file = "data/ncaa_big_sim.csv", row.names = FALSE)
pb_upload("data/ncaa_big_sim.csv")


# write.csv(styles_list[[1]][[2]] %>% select(-c(player_join, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)), file = "data/nba_guard_style.csv", row.names = FALSE)
# pb_upload("data/nba_guard_style.csv")
# 
# write.csv(styles_list[[2]][[2]] %>% select(-c(player_join, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)), file = "data/nba_wing_style.csv", row.names = FALSE)
# pb_upload("data/nba_wing_style.csv")
# 
# write.csv(styles_list[[3]][[2]] %>% select(-c(player_join, sim_1, sim_1_url, sim_2, sim_3, sim_4, sim_5)), file = "data/nba_big_style.csv", row.names = FALSE)
# pb_upload("data/nba_big_style.csv")

rm(a, all_player_stats, big_stats, guard_stats, headshot_urls_db, nba_big_pca, nba_big_styles, nba_guard_pca, nba_guard_styles,
   nba_player_stats, nba_wing_pca, nba_wing_styles, ncaa_big_pca, ncaa_big_sim, ncaa_big_styles, ncaa_guard_pca, ncaa_guard_sim,
   ncaa_guard_styles, ncaa_wing_pca, ncaa_wing_sim, ncaa_wing_styles, player_advanced, player_advanced_partial, player_totals,
   player_totals_partial, raw_pa, raw_pt, sim_list, styles_list, viewable_opps, wing_stats, gp_q, min_q, n_ncaa_players, num,
   offset, pa_url, player, player_advanced_url, player_totals_url, pt_url, sim_1_url, similarity_vec, top_5, z, opponentSRurl_db)
