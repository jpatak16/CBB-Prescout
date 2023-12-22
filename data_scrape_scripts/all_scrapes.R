library(pacman)
p_load(magrittr, dplyr, jsonlite, purrr, tidyr, stringr, lubridate, cli,
       janitor, rvest, cfbplotR, hoopR, httr, rlang, piggyback, here)
#p_load(FactoMineR)

here::i_am("data_scrape_scripts/all_scrapes.R")

this_year=2024
our_teams = c("Oregon", "Clemson", "Mississippi State", "New Mexico")

resume_database <- function(n = 500, min_year = 2008, max_year = 2024,
                            min_net = NULL, max_net = NULL, conf_group = NULL) {
  
  # conference group must be valid
  if(!is.null(conf_group) && (!conf_group %in% c('All', 'NCAA', 'Himajor', 'mid'))) {
    cli_abort(c('Unsupported `conf_group`.',
                'i' = 'conf_group must be one of: All, NCAA, Himajor, or mid.',
                'x' = 'You called `conf_group = {conf_group}`'))
  }
  
  url <- httr::modify_url(
    url = 'https://barttorvik.com/resume-compare-all.php?',
    query = list(
      start = min_year,
      end = max_year,
      netlow = min_net,
      nethigh = max_net,
      maxct = n,
      conlimit = conf_group
    )
  )
  
  # scrape table
  rlang::local_options(HTTPUserAgent='CBB-DATA') # in case user on windows
  data <- suppressWarnings({
    read_html(url) %>%
      html_table() %>%
      purrr::pluck(2) %>%
      clean_names() %>%
      rename('games_above_500' = x500,
             'q2_w' = q2w)
  })
  
  return(data)
  
}

standardize_name = function(player_name){
  #remove periods
  name = gsub("\\.", "", player_name)
  #remove apostrophe
  name = gsub("'", "", name)
  #remove dashes
  name = gsub("-", "", name)
  #remove spaces and any third or more name element
  name = paste0(strsplit(name, split = " ")[[1]][1], strsplit(name, split = " ")[[1]][2])
  #upcase all letters
  name = toupper(name)
  return(name)
}
standardize_name = Vectorize(standardize_name)

###### ncaa participatR functions
scrape_pbp <- function(game_id = 6048478) {
  json <-
    paste0("https://data.ncaa.com/casablanca/game/",
           game_id,
           "/pbp.json") |>
    jsonlite::fromJSON(flatten = T)
  teams <- json |>
    purrr::pluck("meta") |>
    purrr::pluck("teams") |>
    dplyr::select(-c(color)) |>
    tidyr::pivot_wider(
      names_from = homeTeam,
      values_from = c(id, shortName, seoName, sixCharAbbr, shortName, nickName)
    ) |>
    dplyr::rename_with(\(x) gsub("_true", "_home", x), dplyr::ends_with("_true")) |>
    dplyr::rename_with(\(x) gsub("_false", "_visitor", x), dplyr::ends_with("_false"))
  pbp <- json |>
    purrr::pluck("periods") |>
    tidyr::unnest(playStats) |>
    dplyr::mutate(game_id = game_id) |>
    dplyr::bind_cols(teams)
  return(pbp)
}
extract_all_players <- function(text) {
  # extract pos player (first player on play)
  pos_1 <-
    stringr::str_extract(text, "^(.*) (misses|defensive|offensive|makes|turnover)")
  # extract def player (first player on play)
  def_1 <- stringr::str_extract(text, "^(.*) blocks")
  # extract pos player (second player on play)
  pos_2 <- stringr::str_extract(text, "\\(.* assists|blocks .*'s")
  # extract def player (second player on team)
  def_2 <- stringr::str_extract(text, "\\(.* draws|\\(.* steals")
  pos_1 <-
    trimws(gsub('(misses|defensive|offensive|makes|turnover)', '', pos_1))
  pos_2 <- trimws(gsub("\\(| assists|blocks|'s", '', pos_2))
  def_1 <- trimws(gsub("blocks", '', def_1))
  def_2 <-
    trimws(gsub("\\(|draws|steals|bad pass\\)|lost ball\\)", '', def_2))
  return(data.frame(pos_1,
                    pos_2,
                    def_1,
                    def_2))
}
parse_pbp <- function(pbp_text, type = "mens") {
  stopifnot(type %in% c("mens","womens"))
  pbp <- pbp_text |>
    # fill in score column (is left blank when score is unchanged)
    dplyr::mutate(score = dplyr::case_when(score == "" ~ NA_character_,
                                           T ~ score)) |>
    tidyr::fill(score, .direction = "down") |>
    dplyr::mutate(score = dplyr::case_when(is.na(score) ~ "0-0",
                                           T ~ score)) |>
    # generate time columns
    tidyr::separate(time,
                    c("minute", "second"),
                    fill = "right") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      dplyr::across(c(minute, second),
                    \(x) as.numeric(x)),
      time = lubridate::period(minute = minute, second = second),
      play_id = paste0(game_id, "_", dplyr::row_number()),
      events = dplyr::case_when(homeText == '' ~ visitorText,
                                visitorText == '' ~ homeText,
                                T ~ '')
    )
  if((type == "mens" & max(pbp_text$periodNumber) < 2) | (type == "womens" & max(pbp_text$periodNumber) < 4)) {
    cli::cli_alert_warning("Incomplete game provided! Unable to provide lineups for {pbp$game_id[1]}, returning empty df...")
    pbp_parsed <- data.frame()
  } else
    # identify if this is v1 or v2 or v3
    if (any(grepl('Subbing in for ', pbp$events)) & any(grepl(",", pbp$events))) {
      # v3
      pbp_parsed <- pbp |>
        # check which plays get subbed in, which get subbed out
        dplyr::mutate(
          subbed_in_home = grepl('Subbing in for ', homeText),
          subbed_out_home = grepl('Subbing out for ', homeText),
          player_on_play_home = gsub(
            "((.*)'s |(.*)- |(.*)by )",
            "",
            homeText),
          player_on_play_home = gsub("Subbing in for ", "", player_on_play_home),
          player_on_play_home = gsub("Subbing out for ", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(grepl("\\[", player_on_play_home) ~ NA_character_,
                                                 grepl("Foul on ", player_on_play_home) ~ NA_character_,
                                                 grepl("End of ", player_on_play_home) ~ NA_character_,
                                                 T ~ player_on_play_home),
          player_on_play_home = gsub("\\(.*\\)", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            player_on_play_home == "second time out." ~ "",
            T ~ player_on_play_home
          ),
          player_on_play_home = trimws(player_on_play_home),
          player_on_play_home = gsub(dplyr::first(shortName_home), "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            !grepl(",", player_on_play_home) ~ NA_character_,
            player_on_play_home == '' ~ NA_character_,
            player_on_play_home == "'s" ~ NA_character_,
            player_on_play_home == ",," ~ NA_character_,
            player_on_play_home == "Turnover" ~ NA_character_,
            T ~ player_on_play_home
          ),
          on_court_flag_home = dplyr::case_when(subbed_in_home ~ 1,
                                                subbed_out_home ~ 0,
                                                T ~ NA_real_),
          subbed_in_visitor = grepl('Subbing in for ', visitorText),
          subbed_out_visitor = grepl('Subbing out for ', visitorText),
          player_on_play_visitor = gsub(
            "((.*)'s |(.*)- |(.*)by )",
            "",
            visitorText),
          player_on_play_visitor = gsub("Subbing in for ", "", player_on_play_visitor),
          player_on_play_visitor = gsub("Subbing out for ", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(grepl("\\[", player_on_play_visitor) ~ NA_character_,
                                                    grepl("Foul on ", player_on_play_visitor) ~ NA_character_,
                                                    grepl("End of ", player_on_play_visitor) ~ NA_character_,
                                                    T ~ player_on_play_visitor),
          player_on_play_visitor = gsub("\\(.*\\)", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(
            player_on_play_visitor == "second time out." ~ "",
            T ~ player_on_play_visitor
          ),
          player_on_play_visitor = trimws(player_on_play_visitor),
          player_on_play_visitor = gsub(
            dplyr::first(shortName_visitor),
            "",
            player_on_play_visitor
          ),
          player_on_play_visitor = dplyr::case_when(
            !grepl(",", player_on_play_visitor) ~ NA_character_,
            player_on_play_visitor == '' ~ NA_character_,
            player_on_play_visitor == "'s" ~ NA_character_,
            player_on_play_visitor == ",," ~ NA_character_,
            player_on_play_visitor == "Turnover" ~ NA_character_,
            T ~ player_on_play_visitor
          ),
          on_court_flag_visitor = dplyr::case_when(subbed_in_visitor ~ 1,
                                                   subbed_out_visitor ~ 0,
                                                   T ~ NA_real_),
        ) |>
        tidyr::separate_wider_delim(cols = player_on_play_home, delim = ", ", names = c("home_last", "home_first")) |>
        tidyr::separate_wider_delim(cols = player_on_play_visitor, delim = ", ", names = c("vis_last", "vis_first")) |>
        dplyr::mutate(player_on_play_home = paste0(home_first, " ", home_last),
                      player_on_play_home = ifelse(player_on_play_home == "NA NA", NA, player_on_play_home),
                      player_on_play_visitor = paste0(vis_first, " ", vis_last),
                      player_on_play_visitor = ifelse(player_on_play_visitor == "NA NA", NA, player_on_play_visitor)) |>
        dplyr::select(-home_first, -home_last, -vis_first, -vis_last) |>
        dplyr::filter(!is.na(player_on_play_visitor) |
                        !is.na(player_on_play_home)) |>
        dplyr::group_by(time,player_on_play_home) |>
        dplyr::filter(!(sum(subbed_in_home) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(time,player_on_play_visitor) |>
        dplyr::filter(!(sum(subbed_in_visitor) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(periodNumber) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_home,
                values_from = on_court_flag_home,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row) |>
                  dplyr::summarise(on_court_home = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_visitor,
                values_from = on_court_flag_visitor,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row_1 = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row_1) |>
                  dplyr::summarise(on_court_visitor = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::ungroup() |>
        # find out when lineups change
        dplyr::mutate(stint = cumsum(
          subbed_in_home + subbed_in_visitor
        )) |>
        dplyr::group_by(stint, on_court_home, on_court_visitor, periodNumber) |>
        dplyr::summarise(
          stint_start = dplyr::first(time),
          stint_end = dplyr::last(time),
          .groups = "drop"
        ) |>
        dplyr::filter(stint_start != stint_end) |>
        dplyr::ungroup() |>
        dplyr::arrange(periodNumber,-stint_start) |>
        dplyr::group_by(periodNumber) |>
        dplyr::mutate(
          stint_start = dplyr::case_when(
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2) &
              type == "mens" ~ lubridate::as.period("20M 0S"),
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2, 3, 4) &
              type == "womens" ~ lubridate::as.period("10M 0S"),
            (
              dplyr::row_number() == 1 &
                periodNumber >= 3 &
                type == "mens"
            ) |
              (
                dplyr::row_number() == 1 &
                  periodNumber >= 5 &
                  type == "womens"
              ) ~ lubridate::as.period("5M 0S"),
            T ~ stint_start
          ),
          stint_end = dplyr::case_when(
            dplyr::row_number() == dplyr::n() ~ lubridate::as.period("0M 0S"),
            T ~ stint_end
          )
        ) |>
        dplyr::select(-c(stint)) |>
        #remove x_team from listed players in on_court
        dplyr::mutate(
          on_court_home = gsub(";x_team|x_team;", "", on_court_home),
          on_court_visitor = gsub(";x_team|x_team;", "", on_court_visitor),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
        ) |>
        # omit bad sub data
        dplyr::mutate(dplyr::across(
          dplyr::starts_with("on_court"),
          \(x) dplyr::case_when(stringr::str_count(x, ';') == 4 ~ x,
                                T ~ NA_character_)
        )) |>
        data.frame()
    } else if (any(grepl('Subbing in for ', pbp$events))) {
      # v2
      pbp_parsed <- pbp |>
        # check which plays get subbed in, which get subbed out
        dplyr::mutate(
          subbed_in_home = grepl('Subbing in for ', homeText),
          subbed_out_home = grepl('Subbing out for ', homeText),
          player_on_play_home = gsub(
            "((.*)'s |-| by )",
            "",
            homeText),
          player_on_play_home = gsub("Subbing in for ", "", player_on_play_home),
          player_on_play_home = gsub("Subbing out for ", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(grepl("\\[", player_on_play_home) ~ NA_character_,
                                                 grepl("Foul on ", player_on_play_home) ~ NA_character_,
                                                 grepl("End of ", player_on_play_home) ~ NA_character_,
                                                 T ~ player_on_play_home),
          player_on_play_home = gsub("\\(.*\\)", "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            player_on_play_home == "second time out." ~ "",
            T ~ player_on_play_home
          ),
          player_on_play_home = trimws(player_on_play_home),
          player_on_play_home = gsub(dplyr::first(shortName_home), "", player_on_play_home),
          player_on_play_home = dplyr::case_when(
            player_on_play_home == '' ~ NA_character_,
            player_on_play_home == "'s" ~ NA_character_,
            player_on_play_home == "Turnover" ~ NA_character_,
            T ~ player_on_play_home
          ),
          on_court_flag_home = dplyr::case_when(subbed_in_home ~ 1,
                                                subbed_out_home ~ 0,
                                                T ~ NA_real_),
          subbed_in_visitor = grepl('Subbing in for ', visitorText),
          subbed_out_visitor = grepl('Subbing out for ', visitorText),
          player_on_play_visitor = gsub(
            "((.*)'s |-| by )",
            "",
            visitorText),
          player_on_play_visitor = gsub("Subbing in for ", "", player_on_play_visitor),
          player_on_play_visitor = gsub("Subbing out for ", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(grepl("\\[", player_on_play_visitor) ~ NA_character_,
                                                    grepl("Foul on ", player_on_play_visitor) ~ NA_character_,
                                                    grepl("End of ", player_on_play_visitor) ~ NA_character_,
                                                    T ~ player_on_play_visitor),
          player_on_play_visitor = gsub("\\(.*\\)", "", player_on_play_visitor),
          player_on_play_visitor = dplyr::case_when(
            player_on_play_visitor == "second time out." ~ "",
            T ~ player_on_play_visitor
          ),
          player_on_play_visitor = trimws(player_on_play_visitor),
          player_on_play_visitor = gsub(
            dplyr::first(shortName_visitor),
            "",
            player_on_play_visitor
          ),
          player_on_play_visitor = dplyr::case_when(
            player_on_play_visitor == '' ~ NA_character_,
            player_on_play_visitor == "'s" ~ NA_character_,
            player_on_play_visitor == "Turnover" ~ NA_character_,
            T ~ player_on_play_visitor
          ),
          on_court_flag_visitor = dplyr::case_when(subbed_in_visitor ~ 1,
                                                   subbed_out_visitor ~ 0,
                                                   T ~ NA_real_),
        ) |>
        dplyr::filter(!is.na(player_on_play_visitor) |
                        !is.na(player_on_play_home)) |>
        dplyr::group_by(time,player_on_play_home) |>
        dplyr::filter(!(sum(subbed_in_home) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(time,player_on_play_visitor) |>
        dplyr::filter(!(sum(subbed_in_visitor) == sum(subbed_out_home) & sum(subbed_in_home) > 0)) |>
        dplyr::ungroup() |>
        dplyr::group_by(periodNumber) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_home,
                values_from = on_court_flag_home,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row) |>
                  dplyr::summarise(on_court_home = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::group_modify( ~ (\(a) {
          cbind(
            a,
            a |>
              # create a matrix of all players mentioned in the pbp at any point
              tidyr::pivot_wider(
                names_from = player_on_play_visitor,
                values_from = on_court_flag_visitor,
                names_prefix = 'x_',
                names_repair = "check_unique"
              ) |>
              #' whenever a player is subbed in, they have a 1 in that row and a 0
              #' in the row before. whenever a player is subbed out, they have a 0
              #' in that row and a 1 in the row before
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(dplyr::lead(.) == 0 ~ 1,
                                     dplyr::lead(.) == 1 ~ 0,
                                     T ~
                                       .)
                )
              ) |>
              #' we then cascade the 1s and 0s up and down to create map of who is
              #' in the game at any given time
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "down") |>
              tidyr::fill(c(dplyr::starts_with('x_')), .direction = "up") |>
              dplyr::mutate(
                dplyr::across(
                  dplyr::starts_with('x_'),
                  ~ dplyr::case_when(is.na(.x) ~ 1,
                                     T ~ .x)
                )
              ) |>
              # finally we collapse the values down into a string of who is on court
              (\(d) {
                d |>
                  dplyr::select(dplyr::starts_with('x_')) |>
                  dplyr::select(-dplyr::any_of('x_NA')) |>
                  dplyr::mutate(row_1 = dplyr::row_number()) |>
                  tidyr::pivot_longer(dplyr::starts_with("x_")) |>
                  dplyr::group_by(row_1) |>
                  dplyr::summarise(on_court_visitor = paste0(name[value == 1],collapse=';'))
              })()
          )
        })(.x)) |>
        dplyr::ungroup() |>
        # find out when lineups change
        dplyr::mutate(stint = cumsum(
          subbed_in_home + subbed_in_visitor
        )) |>
        dplyr::group_by(stint, on_court_home, on_court_visitor, periodNumber) |>
        dplyr::summarise(
          stint_start = dplyr::first(time),
          stint_end = dplyr::last(time),
          .groups = "drop"
        ) |>
        dplyr::filter(stint_start != stint_end) |>
        dplyr::ungroup() |>
        dplyr::arrange(periodNumber,-stint_start) |>
        dplyr::group_by(periodNumber) |>
        dplyr::mutate(
          stint_start = dplyr::case_when(
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2) &
              type == "mens" ~ lubridate::as.period("20M 0S"),
            dplyr::row_number() == 1 &
              periodNumber %in% c(1, 2, 3, 4) &
              type == "womens" ~ lubridate::as.period("10M 0S"),
            (
              dplyr::row_number() == 1 &
                periodNumber >= 3 &
                type == "mens"
            ) |
              (
                dplyr::row_number() == 1 &
                  periodNumber >= 5 &
                  type == "womens"
              ) ~ lubridate::as.period("5M 0S"),
            T ~ stint_start
          ),
          stint_end = dplyr::case_when(
            dplyr::row_number() == dplyr::n() ~ lubridate::as.period("0M 0S"),
            T ~ stint_end
          )
        ) |>
        dplyr::select(-c(stint)) |>
        #remove x_team from listed players in on_court
        dplyr::mutate(
          on_court_home = gsub(";x_team|x_team;", "", on_court_home),
          on_court_visitor = gsub(";x_team|x_team;", "", on_court_visitor),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_home = gsub(';x_ \\[.*\\]', "", on_court_home),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
          on_court_visitor = gsub(';x_ \\[.*\\]', "", on_court_visitor),
        ) |>
        # omit bad sub data
        dplyr::mutate(dplyr::across(
          dplyr::starts_with("on_court"),
          \(x) dplyr::case_when(stringr::str_count(x, ';') == 4 ~ x,
                                T ~ NA_character_)
        )) |>
        data.frame()
    } 
  else if (any(grepl(' lineup change ', pbp$events))) {
    # v1
    pbp_parsed <- pbp |>
      # v1 pbp just tells us full new lineup whenever there's a change, except to start the game
      dplyr::mutate(
        on_court = dplyr::case_when(
          grepl(' lineup change ', events) ~ stringr::str_extract(events, '\\((.*)\\)'),
          T ~ NA_character_
        ),
        on_court = gsub('\\(', 'x_', on_court),
        on_court = gsub(', ', ';x_', on_court),
        on_court = gsub(')', '', on_court),
        home_lineup_change = grepl(' lineup change ', homeText),
        away_lineup_change = grepl(' lineup change ', visitorText),
        home_on_court = dplyr::case_when(home_lineup_change ~ on_court,
                                         T ~ NA_character_),
        away_on_court = dplyr::case_when(away_lineup_change ~ on_court,
                                         T ~ NA_character_),
      ) |>
      tidyr::fill(c(home_on_court, away_on_court), .direction = "down") |>
      (\(x) {
        # for the start of the game, we're missing lineups, so we have to estimate these
        missing_both <- x |>
          dplyr::filter(is.na(home_on_court) &
                          is.na(away_on_court))
        # sometimes only one team makes a change initially, this handles that
        missing_home <- x |>
          dplyr::filter(is.na(home_on_court) &
                          !is.na(away_on_court))
        missing_away <- x |>
          dplyr::filter(is.na(away_on_court) &
                          !is.na(home_on_court))
        complete <- x |>
          dplyr::filter(!is.na(home_on_court) &
                          !is.na(away_on_court))
        # extract players/teams from missing data
        extracted_both_home <-
          extract_all_players(missing_both$homeText)
        extracted_both_away <-
          extract_all_players(missing_both$visitorText)
        # if the away team made a substitution first and the home team did not make a sub at the same time
        if (nrow(missing_home)) {
          # grab the players from the text
          extracted_one_home <-
            extract_all_players(missing_home$homeText)
          extracted_one_away <-
            extract_all_players(missing_home$visitorText)
          # pull in all the players who are recorded as being on court for the home team while this is missing
          on_court_home <- c(
            extracted_both_home$pos_1,
            extracted_both_home$pos_2,
            extracted_both_away$def_1,
            extracted_both_away$def_2,
            extracted_one_home$pos_1,
            extracted_one_home$pos_2,
            extracted_one_away$def_1,
            extracted_one_away$def_2
          ) |>
            unique() |>
            (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
          #  check that there's five players on the court for the home team
          if (length(on_court_home) == 5) {
            on_court_home <- on_court_home |>
              (\(y) paste0('x_', y))() |>
              paste0(collapse = ';')
          } else {
            # if not, NA the lineup
            on_court_home <- NA_character_
          }
          # pull in the away participation data, combine with the known lineups
          on_court_away <- c(
            extracted_both_away$pos_1,
            extracted_both_away$pos_2,
            extracted_both_home$def_1,
            extracted_both_home$def_2
          ) |>
            unique() |>
            (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
          if (length(on_court_away) == 5) {
            on_court_away <- on_court_away |>
              (\(y) paste0('x_', y))() |>
              paste0(collapse = ';')
          } else {
            on_court_away <- NA_character_
          }
          # finally set the lineups for the data
          missing_both$away_on_court <- on_court_away
          missing_both$home_on_court <- on_court_home
          missing_home$home_on_court <- on_court_home
          # do the same process for home sub first/away no sub
        } else if (nrow(missing_away)) {
          extracted_one_away <- extract_all_players(missing_away$awayText)
          extracted_one_home <-
            extract_all_players(missing_away$visitorText)
          on_court_away <- c(
            extracted_both_away$pos_1,
            extracted_both_away$pos_2,
            extracted_both_home$def_1,
            extracted_both_home$def_2,
            extracted_one_away$pos_1,
            extracted_one_away$pos_2,
            extracted_one_home$def_1,
            extracted_one_home$def_2
          ) |>
            unique() |>
            (\(y) y[!(y %in% c(x$nickName_away[1], x$nickName_visitor[1], NA_character_))])()
          if (length(on_court_away) == 5) {
            on_court_away <- on_court_away |>
              (\(y) paste0('x_', y))() |>
              paste0(collapse = ';')
          } else {
            on_court_away <- NA_character_
          }
          on_court_home <- c(
            extracted_both_home$pos_1,
            extracted_both_home$pos_2,
            extracted_both_away$def_1,
            extracted_both_away$def_2
          ) |>
            unique() |>
            (\(y) y[!(y %in% c(x$nickName_away[1], x$nickName_visitor[1], NA_character_))])()
          if (length(on_court_home) == 5) {
            on_court_home <- on_court_home |>
              (\(y) paste0('x_', y))() |>
              paste0(collapse = ';')
          } else {
            on_court_home <- NA_character_
          }
          missing_both$home_on_court <- on_court_home
          missing_both$away_on_court <- on_court_away
          missing_away$away_on_court <- on_court_away
          # now if both teams sub at the same time, rely on the usual lineup logic
        } else {
          on_court_home <- c(
            extracted_both_home$pos_1,
            extracted_both_home$pos_2,
            extracted_both_away$def_1,
            extracted_both_away$def_2
          ) |>
            unique() |>
            (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
          if (length(on_court_home) == 5) {
            on_court_home <- on_court_home |>
              (\(y) paste0('x_', y))() |>
              paste0(collapse = ';')
          } else {
            on_court_home <- NA_character_
          }
          on_court_away <- c(
            extracted_both_away$pos_1,
            extracted_both_away$pos_2,
            extracted_both_home$def_1,
            extracted_both_home$def_2
          ) |>
            unique() |>
            (\(y) y[!(y %in% c(x$nickName_home[1], x$nickName_visitor[1], NA_character_))])()
          if (length(on_court_away) == 5) {
            on_court_away <- on_court_away |>
              (\(y) paste0('x_', y))() |>
              paste0(collapse = ';')
          } else {
            on_court_away <- NA_character_
          }
          missing_both$away_on_court <- on_court_away
          missing_both$home_on_court <- on_court_home
        }
        # combine everything back together
        return(dplyr::bind_rows(missing_both,
                                missing_away,
                                missing_home,
                                complete))
      })() |>
      # clean the data and calculate stints
      dplyr::mutate(
        on_court = paste0(home_on_court, ";", away_on_court),
        substitution = grepl(' lineup change ', events)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(stint = cumsum(substitution)) |>
      dplyr::group_by(stint, home_on_court, away_on_court, periodNumber) |>
      dplyr::summarise(
        stint_start = dplyr::first(time),
        stint_end = dplyr::last(time),
        .groups = "drop"
      ) |>
      dplyr::filter(stint_start != stint_end) |>
      dplyr::ungroup() |>
      dplyr::arrange(periodNumber,-stint_start) |>
      dplyr::group_by(periodNumber) |>
      dplyr::mutate(
        stint_start = dplyr::case_when(
          dplyr::row_number() == 1 &
            periodNumber %in% c(1, 2) &
            type == "mens" ~ lubridate::as.period("20M 0S"),
          dplyr::row_number() == 1 &
            periodNumber %in% c(1, 2, 3, 4) &
            type == "womens" ~ lubridate::as.period("10M 0S"),
          (
            dplyr::row_number() == 1 &
              periodNumber >= 3 &
              type == "mens"
          ) |
            (
              dplyr::row_number() == 1 &
                periodNumber >= 5 &
                type == "womens"
            ) ~ lubridate::as.period("5M 0S"),
          T ~ stint_start
        ),
        stint_end = dplyr::case_when(
          dplyr::row_number() == dplyr::n() ~ lubridate::as.period("0M 0S"),
          T ~ stint_end
        )
      ) |>
      dplyr::select(-c(stint)) |>
      # omit bad sub data
      dplyr::mutate(
        home_on_court = dplyr::case_when(
          stringr::str_count(home_on_court, ';') == 4 ~ home_on_court,
          T ~ NA_character_
        )
      ) |>
      dplyr::mutate(
        away_on_court = dplyr::case_when(
          stringr::str_count(away_on_court, ';') == 4 ~ away_on_court,
          T ~ NA_character_
        )
      ) |>
      #consistent column naming with v2
      dplyr::rename(on_court_home = home_on_court,
                    on_court_visitor = away_on_court) |>
      data.frame()
  } else {
    cli::cli_alert_warning("No substitutions found for {pbp$game_id[1]}! Returning empty DF...")
    pbp_parsed <- data.frame()
  }
  return(pbp_parsed)
}

pb_upload(here::here("data/opp_url.csv"))
pb_upload(here::here("data/nba_stats.csv"))

source(here::here("data_scrape_scripts/data_scrape_headshots.R"))

Sys.sleep(300)

source(here::here("data_scrape_scripts/data_scrape_GMC.R"))

Sys.sleep(300)

source(here::here("data_scrape_scripts/data_scrape_PPT.R"))

Sys.sleep(300)

source(here::here("data_scrape_scripts/data_scrape_OO.R"))

Sys.sleep(300)

source(here::here("data_scrape_scripts/data_scrape_PC.R"))

Sys.sleep(300)

source(here::here("data_scrape_scripts/data_scrape_games_list.R"))

Sys.sleep(300)

source(here::here("data_scrape_scripts/data_scrape_lineup_viz_scratch.R"))