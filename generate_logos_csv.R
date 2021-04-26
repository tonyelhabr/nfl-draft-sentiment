
library(tidyverse)
library(httr)
resp <- 
  'https://www.pff.com/api/mock_draft_simulator/draft_picks' %>%
  httr::GET() %>%
  httr::content('parsed') 
# resp
teams_pff_init <- resp %>% pluck('teams') %>% enframe() %>% unnest_wider(value)
teams_pff <- 
  teams_pff_init %>% 
  select(draft_franchise_id = franchise_id, abbrv_pff = abbreviation, nickname) %>% 
  arrange(abbrv_pff)
teams_pff

teams_nflfastr <- 
  nflfastR::teams_colors_logos %>% 
  filter(!(team_abbr %in% c('LAR', 'STL', 'OAK', 'SD'))) %>% 
  select(abbrv_nflfastr = team_abbr, nickname = team_nick, team_logo = team_logo_espn)
teams_nflfastr

teams_reddit <-
  c(
    'Cardinals',
    'Falcons',
    'Ravens',
    'Bills',
    'Panthers',
    'Bears',
    'Bengals',
    'Browns',
    'Cowboys',
    'Broncos',
    'Lions',
    'Packers',
    'Texans',
    'Colts',
    'Jaguars',
    'Chiefs',
    'Rams',
    'Chargers',
    'Raiders',
    'Dolphins',
    'Vikings',
    'Patriots',
    'Saints',
    'Giants',
    'Jets',
    'Eagles',
    'Steelers',
    'Seahawks',
    '49ers',
    'Buccaneers',
    'Titans',
    'Redskins'
  )

logos <- 
  teams_nflfastr %>% 
  inner_join(teams_pff) %>% 
  bind_cols(tibble(team_reddit = teams_reddit)) %>% 
  rename(team_nflfastr = nickname) %>% 
  relocate(abbrv_nflfastr, abbrv_pff, team_nflfastr, team_reddit, team_logo)
logos
# Should just be WFT that's a misamtch.
# logos %>% filter(team_nflfastr != team_reddit)

write_csv(teams, file.path('data', 'logos.csv'))
