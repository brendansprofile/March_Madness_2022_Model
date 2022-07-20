# March Madness 2022 Prediction Competition Kaggle
# Brendan Bell


# OUTCOME -----------------------------------------------------------------

# RESULT: TOP ~ 25%  /1,0000 teams
# 2022 LOGLOSS SCORE: 0.62994


# REFERENCES & RESEARCH --------------------------------------------------------------

# https://kenpom.com/blog/four-factors/ 
# https://www.maizenbrew.com/2019/10/23/20928669/kenpom-explained-what-it-means-michigan-basketball-ranking 

# PRELIMINARY -------------------------------------------------------------

#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("zoo")
library(tidyverse)
library(lubridate)
library(zoo)


# DATA IMPORT ----------------------------------------------------------

# data location
filepaths <- list.files("MDataFiles_Stage2")
files_to_read <- tibble(paths = filepaths, df_name = str_remove(filepaths, ".csv"))

# reading in all data from path loop
for (i in seq_along(files_to_read$paths)) {
  assign(files_to_read$df_name[i],
         read_csv(str_c("MDataFiles_Stage2/", files_to_read$paths[i]),
                  show_col_types = FALSE))
} 


# DATA SELECT ----------------------------------------------------------

# view folder
filepaths

# 1. TOURNEY data to compute stats
TourneyStats <- MNCAATourneyDetailedResults

# 2. TOURNEY seeding 
Seeds <- MNCAATourneySeeds
# convert to just numeric seed 1-16
Seeds$Seed = as.numeric(substring(Seeds$Seed,2,4))

# 3. REGULAR -- not sure if to use...
RegularStats <- MRegularSeasonDetailedResults



# INITIAL DATA WRANGLING --------------------------------------------------------

# estimate possessions Tourney
TourneyStats <- TourneyStats %>% mutate(
  Wposs = WFGA + (0.475 * WFTA) - WOR + WTO, 
  Lposs = LFGA + (0.475 * LFTA) - LOR + LTO,
  )

# split into two dfs (winner, loser) to merge 
WTourneyStats <- TourneyStats %>% 
  select(Season, DayNum, WLoc, WTeamID, LTeamID, WScore, LScore, Lposs, LDR, WFGM:WPF, Wposs) %>%
  mutate(win = 1) %>% 
  rename_with(~ str_remove(., "W"), .cols = WScore:Wposs) %>% #remove the W 
  rename(TeamID = WTeamID, OppID = LTeamID, Loc = WLoc, OppDR = LDR, OppScore = LScore, OppPoss = Lposs)
glimpse(WTourneyStats)

# flip the WLoc variable for loser
LTourneyStats <- TourneyStats %>% 
  select(Season, DayNum, WLoc, WTeamID, LTeamID, LScore, WScore, Wposs, WDR, LFGM:LPF, Lposs) %>%
  mutate(win = 0,
         WLoc = case_when(
           WLoc == "H" ~ "A",
           WLoc == "A" ~ "H",
           WLoc == "N" ~ "N"
         )) %>% 
  rename_with(~ str_remove(., "L"), .cols = LScore:Lposs) %>%
  rename(TeamID = LTeamID,OppID = WTeamID, Loc = WLoc, OppDR = WDR, OppScore = WScore, OppPoss = Wposs)
glimpse(LTourneyStats)

# merge tables using bind rows -- same columns, puts one under the other
TourneyStats2 <- bind_rows(WTourneyStats,LTourneyStats) %>%
  select(Season:Score,poss,FGA,FGM,FGA3,FGM3,FTA,OR,TO,OppScore,OppDR,OppPoss,win)


# -------------------------------------------------------------------------
## REGULAR SEASON

# estimate possessions REGULAR
RegularStats <- RegularStats %>% mutate(
  Wposs = WFGA + (0.475 * WFTA) - WOR + WTO, 
  Lposs = LFGA + (0.475 * LFTA) - LOR + LTO,
)

# split into two dfs (winner, loser) to merge 
WRegularStats <- RegularStats %>% 
  select(Season, DayNum, WLoc, WTeamID, LTeamID, WScore, LScore, Lposs, LDR, WFGM:WPF, Wposs) %>%
  mutate(win = 1) %>% 
  rename_with(~ str_remove(., "W"), .cols = WScore:Wposs) %>% #remove the W 
  rename(TeamID = WTeamID, OppID = LTeamID, Loc = WLoc, OppDR = LDR, OppScore = LScore, OppPoss = Lposs)

# lip the WLoc variable for loser
LRegularStats <- RegularStats %>% 
  select(Season, DayNum, WLoc, WTeamID, LTeamID, LScore, WScore, Wposs, WDR, LFGM:LPF, Lposs) %>%
  mutate(win = 0,
         WLoc = case_when(
           WLoc == "H" ~ "A",
           WLoc == "A" ~ "H",
           WLoc == "N" ~ "N"
         )) %>% 
  rename_with(~ str_remove(., "L"), .cols = LScore:Lposs) %>%
  rename(TeamID = LTeamID,OppID = WTeamID, Loc = WLoc, OppDR = WDR, OppScore = WScore, OppPoss = Wposs)

# merge tables using bind rows -- same columns, puts one under the other
RegularStats2 <- bind_rows(WRegularStats,LRegularStats) %>%
  select(Season:Score,poss,FGA,FGM,FGA3,FGM3,FTA,OR,TO,OppScore,OppDR,OppPoss,win)



# FEATURE ENGINEERING -----------------------------------------------------

# REGULAR STATS
RegularFeatures <- RegularStats2 %>%
  mutate(
    eFGpct = (0.5*FGM3 + FGM) / (FGA),  # 1. Effective FG%
    TOpct = TO / poss,  # 2.Turnover %
    ORpct = OR / (OR + OppDR),  # 3. Offensive Rebounding %
    FTrate = FTA / FGA,  #4. Free Throw Rate
    AdjO = Score * 100 / poss, # Adjusted Off Eff (pts / 100 poss)
    AdjD = OppScore * 100 / poss # Adjusted Def Eff
  ) %>% 
  select(Season,DayNum,Loc,TeamID,OppID,Score,OppScore,poss,eFGpct,TOpct,ORpct,FTrate,AdjO,AdjD,win)

# REGULAR Summary
RegularSummary <- RegularFeatures %>% 
  group_by(Season, TeamID) %>% 
  summarize(
    PointsMean = mean(Score),
    PointsMedian = median(Score),
    PointDiffMean = mean(Score - OppScore),
    PossMean = mean(poss),
    eFGpctMean = mean(eFGpct),
    TOpctMean = mean(TOpct),
    ORpctMean = mean(ORpct),
    FTRate = mean(FTrate),
    AdjOMean = mean(AdjO),
    AdjDMean = mean(AdjD),
    Wins = sum(win)
  ) %>% 
  arrange(desc(Season), desc(Wins)) #arrange latest season and top teams for wins
# ----------------------------------

# Now, we combine the join the reg season summaries to the tourney data + SEEDING
FinalDF <- TourneyStats2 %>% 
  left_join(RegularSummary, by = c("Season", "TeamID")) %>% 
  left_join(select(Seeds,Season,Seed, TeamID), by = c("Season", "TeamID")) %>% 
  select(Season:OppID,Seed,PointsMean:AdjDMean,win, -Score, -OppScore, -poss) #OMIT REG SEASON WINS????????
  # filter(!is.na(Seed))  ## only tourney teams ????


# FINAL DATA FRAME FOR MODEL -- Self join to get the Opp data
FinalDFx <- FinalDF %>% 
  left_join(
    FinalDF %>% rename_with(~ str_c("Opp", .), .cols = Seed:AdjDMean) %>% select(-c(OppID, Loc, win)), 
    by = c("Season", "DayNum", "OppID" = "TeamID") #Left join on OppID (losing team !)
  ) %>% 
  mutate(SeedDiff = Seed - OppSeed) %>% 
  filter(
    !is.na(Seed) & !is.na(OppSeed), # ONLY TOURNEY TEAMS
    TeamID < OppID # Team1 vs 2 same as 2v1
  ) %>% 
  relocate(win, .after = everything()) %>% 
  select(-Loc) %>% 
  arrange(desc(Season), DayNum)

# ensure no na anywhere
sum(is.na(FinalDFx))
# 0

# look at the seeds
FinalDFx %>% select(TeamID,Seed,OppID,OppSeed,SeedDiff)

# last season 2021
FinalDFx %>% arrange(desc(Season))
