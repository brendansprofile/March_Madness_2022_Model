# PRELIMINARY -------------------------------------------------------------

# install.packages("mlr3verse")
# install.packages("mlr3")
# install.packages("xgboost")
library(tidyverse)
library(mlr3verse)
library(mlr3)
library(xgboost)

# CONNECT TO DATA FILE 
source('/Users/brendanbell/Documents_Disk/MarchMadness2022/MM22_Data_V3.R')


# DATA SETS SETUP ---------------------------------------------------------

# close to balanced
FinalDFx %>% count(win)


# SET TASK -> task = "DataSet"
DataSet <- FinalDFx %>%
  select(-c("Season","DayNum","TeamID","OppID")) %>% # dont need row info (team, season, etc)
  mutate(
    win = ifelse(win == 1, "W", "L"),
    win = as_factor(win) # make this a factor
  ) %>% as_task_classif(target = "win", positive = "W") # set TASK target

# set task = DataSet (not necessary just nomenclature)
task = DataSet
print(task)


# Define train and test sets - this will be base case to improve using CV
TrainSet = sample(task$nrow, 0.8 * task$nrow)
TestSet = setdiff(seq_len(task$nrow), TrainSet)
length(TrainSet)
length(TestSet)



# MODELLING --------------------------------------------------------------------

# ESTABLISHING BASELINE ---------------------------------------------------

# SET LEARNER, TRAIN MODEL --------------

# set learner (model)
xgb_initial = lrn("classif.xgboost", predict_type = "prob")

# ensure null
xgb_initial$model

# train
xgb_initial$train(task, row_ids = TrainSet)
trainxgb_initial = xgb_initial$train(task, row_ids = TrainSet)

# print
print(xgb_initial$model)
# logloss = 0.579

# get predictions
predictionxgb_initial = xgb_initial$predict(task, row_ids = TestSet)
predictionxgb_initial


# ASESS BASELINE -----------------------

# PREDICTIONS -----------------
predictionxgb_initial$confusion

measuresxgb <- msrs(c("classif.acc", "classif.logloss"))

predictionxgb_initial$score(measuresxgb)
  # classif.acc classif.logloss 
  # 0.6074766       0.6439106 

tableXGB_initial <- as.data.table(predictionxgb_initial)



# K FOLD CROSS VALIDATION -------------------------------------------------

xgb = lrn("classif.xgboost", predict_type = "prob")

# select sampling method
resampleCV = rsmp("cv", folds = 10) # 10 is standard practice
print(resampleCV)

# instantiate
resampleCV$instantiate(task)

# view sets
str(resampleCV$train_set(1))
str(resampleCV$test_set(1))

# execute
cvr = resample(task, xgb, resampleCV, store_models = TRUE)

# viewing

# measure -----------------------
cvr$aggregate(msr("classif.acc"))
  # classif.acc 
  # 0.6805325 
cvr$aggregate(msr("classif.logloss"))

# view iterations
cvr$score(msr("classif.acc"))
#  9:   0.7735849

# select the best performer --------------------

# viewing test and train set rows
TrainRows <- str(cvr$resampling$train_set(9))
TestRows <- str(cvr$resampling$test_set(9))

# investigate best run
xgb = cvr$learners[[9]]
xgb$model

# predictions of best run
cvr$predictions()[[9]]

# filter kep best
cvr$filter(c(9))
print(cvr)




# APPLY CV TO MODEL -------------------------------------------------------
xgbCV = lrn("classif.xgboost", predict_type = "prob")

# ensure null
xgbCV$model

# train w/ CV fold 9
xgbCV$train(task, row_ids = TrainRows) # APPLY CV TRAIN ROWS

# print
print(xgbCV$model)
# logloss = 0.579

# get predictions
predictionxgbCV = xgbCV$predict(task, row_ids = TestRows)
predictionxgbCV



# PREDICTIONS -----------------
predictionxgbCV$confusion

measuresxgb <- msrs(c("classif.acc", "classif.logloss"))

predictionxgbCV$score(measuresxgb)
  # classif.acc classif.logloss 
  # 0.8238051       0.5798495 

tablexgbCV <- as.data.table(predictionxgb_initial)




# HYPERPARAMTER TUNING OPTIMIZATION --------------------------------------------

# choose subset of parameters to tune
xgbCV$param_set

# opt to tune 2 hyperparamter
search_space = ps(
  eta = p_dbl(lower = 0.01, upper = 0.3), # must set upper and lower bound
  #lambda = p_dbl(lower = 0.01, upper = 0.7),
  #subsample = p_dbl(lower = 0.5, upper = 0.8),
  max_depth = p_int(lower = 3, upper = 10)
)
search_space

# specify how to evaluate -> need resampling strategy and performance measure 
cv = rsmp("cv") #default to 10 iterations
measure = msr("classif.logloss")  

# specify the budget for tuning using -> Terminators
# Terminate after a given time (TerminatorClockTime).
# Terminate after a given number of iterations (TerminatorEvals).
# Terminate after a specific performance has been reached (TerminatorPerfReached).
# Terminate when tuning does find a better configuration for a given number of iterations (TerminatorStagnation).
# A combination of the above in an ALL or ANY fashion (TerminatorCombo).

# we choose to specify a budget of 20 iterations and put it all together
# -> TuningInstanceSingleCrit:
library("mlr3tuning")

# set to term after 20
evals20 = trm("evals", n_evals = 20)

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = xgbCV,
  resampling = cv,
  measure = measure,
  search_space = search_space,
  terminator = evals20
)
instance


# NEXT, must choose hyperparamter optimization algorithm -> Tuner

# simple grid search w/ resolution 10
tuner = tnr("grid_search", resolution = 10)

# To start the tuning, we simply pass the TuningInstanceSingleCrit to the 
# $optimize() method of the initialized Tuner.

tuner$optimize(instance)

instance$result_learner_param_vals

instance$result_y


# can take the optimized hyperparameters, set them for the previously-created 
# Learner, and train it on the full dataset.
xgbCV$param_set$values = instance$result_learner_param_vals
xgbCV$train(task)

# now model can be used to predict new data ....
predictionxgbCV = xgbCV$predict(task, row_ids = TestRows)
predictionxgbCV

predictionxgbCV$confusion

measuresxgb <- msrs(c("classif.acc", "classif.logloss"))

predictionxgbCV$score(measuresxgb)
# classif.acc classif.logloss 
# 0.7291471       0.6126113 

tablexgbCV <- as.data.table(predictionxgb_initial)




# SUBMISSION ------------------------------------------------------------------

# transform submission
M2022TourneyStart <- str_split(MSampleSubmissionStage2$ID, "_", simplify = TRUE) %>%
  as_tibble(.name_repair = "unique") %>%
  mutate(across(.fn = as.numeric)) # convert to type dbl

names(M2022TourneyStart) <- c("Season", "TeamID", "OppID")

M2022TourneyStart

# NA Seeds fix play in games (allowed final subs after play ins)
SeedFix <- Seeds %>% filter(Season == 2022, is.na(Seed)) %>% 
  left_join(select(MTeams,TeamID,TeamName), by = "TeamID") %>% 
  mutate(Seed = c(12,12,11,11,16,16,16,16)) %>% 
  select(-TeamName)

# Seeds22 has filtered NA values out + FIXED 2022 seeding
Seed22 <- Seeds %>% 
  filter(!is.na(Seed)) %>% 
  bind_rows(SeedFix) %>% 
  filter(!is.na(Seed))

# 68 teams check
filter(Seed22, Season == 2022)


# join this years tourney with model data frame (2,278)
M2022TourneyData <- M2022TourneyStart %>%
  inner_join(RegularSummary, by = c("Season", "TeamID")) %>% 
  inner_join(select(Seed22,Season,Seed, TeamID), by = c("Season", "TeamID")) %>%
  select(Season:OppID,Seed,PointsMean:AdjDMean) %>% 
  distinct(TeamID, .keep_all = TRUE)


# self join to get opponent data -- ONLY 2,211 vs 22,78 (67 less)
M2022TourneyDataFinal <- M2022TourneyStart %>%
  inner_join(
    M2022TourneyData %>% select(-OppID), 
    by = c("Season","TeamID")
  ) %>%
  inner_join(
    M2022TourneyData %>% rename_with(~ str_c("Opp", .), .cols = Seed:AdjDMean) %>% select(-OppID), 
    by = c("Season", "OppID" = "TeamID") #join on OppID
  ) %>%
  mutate(SeedDiff = Seed - OppSeed)

# find rows missing
MissingData <- anti_join(M2022TourneyStart, M2022TourneyDataFinal, by = c("TeamID", "OppID")) %>% 
  inner_join(RegularSummary, by = c("Season","TeamID")) %>% 
  inner_join(select(Seed22,Season,Seed, TeamID), by = c("Season", "TeamID")) %>%
  select(Season:OppID,Seed,PointsMean:AdjDMean) %>% 
  inner_join(
    RegularSummary %>% rename_with(~ str_c("Opp", .), .cols = PointsMean:AdjDMean) %>% select(-Wins), 
    by = c("Season", "OppID" = "TeamID")
    ) %>% 
  inner_join(Seed22 %>% rename(OppSeed = Seed), by = c("Season", "OppID" = "TeamID")) %>% 
  mutate(SeedDiff = Seed - OppSeed)

M2022TourneyDataFinal <- 
  bind_rows(M2022TourneyDataFinal,MissingData)

# just the variables
PredSet <- M2022TourneyDataFinal %>%
  select(Seed:SeedDiff)

# XG booost
Pred2022xg <- xgbCV$predict_newdata(PredSet)$prob %>%
  as_tibble()

# final df
Pred2022dfXGB <- M2022TourneyDataFinal %>%
  select(Season, TeamID, OppID) %>%
  bind_cols(pred = Pred2022xg$W) %>%
  left_join(MTeams %>% select(TeamID, TeamName), by = c("TeamID")) %>%
  left_join(MTeams %>% select(TeamID, OppName = TeamName), by = c("OppID" = "TeamID"))

# Final submission format
SubmissionXGB <- Pred2022dfXGB %>%
  mutate(id = str_c(Season, "_", TeamID, "_", OppID)) %>% 
  select(id, pred)

# csv file
write_csv(SubmissionXGB, "MM022Predictions_V3_XGB.csv")
