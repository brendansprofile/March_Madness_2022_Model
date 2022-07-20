# March_Madness_2022_Model
2022 March Madness Prediction Kaggle Competition

Hello,

This model was built to predict NCAA March Madness tournament outcomes. It was entered into the Kaggle Competition where is placed in the top 25%.

The MM22_Data_V3 file contains the majority of the feature engineering and data wrangling required for the model. Features mostly focus on team performance metrics throughout the regular season and joined with tournament data for respective season. Seeding was also included in the final data frame.

The model of choice is an extreme gradient boosting tree model (XGBoost). This was chosen for its overall success in such competitions, as well as outperforming the previous models I tried (glm, mlr3:logregr). I used cross fold validation and hyperparameter tunig/optimization to reduce the chance of overfitting.


KEY LEARNINGS
- The importance of the Seed feature (a Team's seed in the torunament)
- Sometimes, it is ok to just start over (my initial model failed somewhat miserably...)
- Importance of regularization and avoiding overfit when working with xgboost
- Hyperparamter optimization

THOUGHTS FOR NEXT YEAR
- Work to improve feature engineering and eliminating any non-useful ones.
- Continue to explore various models 
- Further improve the hyperparamter optimization
- Investigate further the logloss tradeoff for certainty, my model produces more conservative probabilities which are punished less when incorrect, but also rewarded less when correct
- Explore deep learning application if this could be a good use case

Thank you for reading!!!
