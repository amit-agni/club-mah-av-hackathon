<a href="https://discuss.analyticsvidhya.com/t/3rd-place-solution-club-mahindra-dataolympics/80519/11" class="uri">https://discuss.analyticsvidhya.com/t/3rd-place-solution-club-mahindra-dataolympics/80519/11</a>

This is a quick summary for the [Club Mahindra Data
Competition](https://datahack.analyticsvidhya.com/contest/club-mahindra-dataolympics/)
that was hosted by Analytics Vidhya between 3-5May 2019

-   My best score (RMSE) was 96.68 with a private leaderboard rank of
    220 out of 983

### Journey

-   Mainly relied on the Xgboost algorithm
-   For Hyperparameter tuning I tried
    -   Bayesian optimisation
    -   Genetic algorithm
-   Tried below without much success :
    -   Polynomials
    -   glm on some categoricals
    -   Eliminating linear combinations using lincomb
    -   Feature selection (kept top 10 XGB identified features)

### Learnings

-   Scaling helps in faster convergence
-   Decision tree based algos tend to give preference to continuous
    variables \[Source\]
    (<a href="https://stackoverflow.com/questions/51601122/xgboost-minimize-influence-of-continuous-linear-features-as-opposed-to-categori" class="uri">https://stackoverflow.com/questions/51601122/xgboost-minimize-influence-of-continuous-linear-features-as-opposed-to-categori</a>)

-   Need to work on a my cross validation strategy
-   Algorithms to try :
    -   catboost
    -   lightgbm

### Other LB Solutions

-   [LB Rank 1
    solution](https://github.com/sahil711/AV/tree/master/Club%20Mahindra%20DataOlympics)

-   \[LB Rank 50 solution\]
    (<a href="https://github.com/sk48880/AV--Club-Mahindra-DataOlympics/blob/master/Club_Mahindra_DataOlympics_EDA%2C_Feature_Eng_%2C_Catboost_%26_LGBM.ipynb" class="uri">https://github.com/sk48880/AV--Club-Mahindra-DataOlympics/blob/master/Club_Mahindra_DataOlympics_EDA%2C_Feature_Eng_%2C_Catboost_%26_LGBM.ipynb</a>)
    -   group by mean,max,min features
-   \[LB Rank 12 solution\]
    (<a href="https://github.com/skg01/Competition/tree/master/AV_Club_Mahindra" class="uri">https://github.com/skg01/Competition/tree/master/AV_Club_Mahindra</a>)
    -   Did he use the target column to create features ?
-   \[LB Rank 2 solution\]
    (<a href="https://github.com/bishwarup307/AV_ClubMahindra" class="uri">https://github.com/bishwarup307/AV_ClubMahindra</a>)

### Misc notes

-   Try 1
    -   nrounds = 427.3809 eta = 0.3832 max\_depth = 3.0000 gamma =
        0.0000 Value = -0.9835
    -   Gave LB 96.7602365861488
-   Try 2
    -   nrounds = 746.5130 eta = 0.2058 max\_depth = 5.0000 gamma =
        3.6674 Value = -0.9625
    -   Stopping. Best iteration: \[418\] test-rmse:0.951616
        train-rmse:0.947857
    -   Gave LB Score of 96.6886094264532
-   Try 3
    -   nrounds = 300.0000 eta = 0.1889 max\_depth = 4.0000 gamma =
        4.8701 Value = -0.9729
    -   Stopping. Best iteration: \[193\] test-rmse:0.973284
        train-rmse:0.963898
    -   Gave LB Score of 97.02
-   eta = 0.2,max\_depth=5,gamma = 4
    -   Best iteration: \[238\] test-rmse:0.973781 train-rmse:0.948695
-   eta = 0.2,max\_depth=4,gamma = 1,min\_child\_weight = 1,subsample =
    0.9,colsample\_bytree = 0.9

-   5th May 19. The best we have got so far. LB : 96.6886094264532
    nrounds = 400 eta = 0.2058  
    max\_depth = 5.0000  
    gamma = 3.6674  
    Test RMSE = -0.9625

-   Without scaling
    -   Stopping. Best iteration: \[289\] test-rmse:0.972385
        train-rmse:0.944414
-   With scaling, converges faster with better test RMSE (but are also
    random as seen in second try)
    -   Stopping. Best iteration: \[215\] test-rmse:0.971473
        train-rmse:0.950535
    -   Stopping. Best iteration: \[219\] test-rmse:0.979283
        train-rmse:0.948156
-   After accounting for the Tot\_pax outliers (group by features)
    -   Stopping. Best iteration: \[198\] test-rmse:0.967705
        train-rmse:0.948594
    -   On full dataset gave :
        -   Stopping. Best iteration: \[181\] train-rmse:0.953192
    -   On LB worsened to 97.3939561450232 !!!
        -   Overfitting, may need to find different parameters
-   Added more member id - group by (\~ interactions ?)
    -   Not much improvement .. may need different parameters ?
    -   Stopping. Best iteration: \[274\] test-rmse:0.969651
        train-rmse:0.938974
    -   On full dataset gave : +Stopping. Best iteration: \[236\]
        train-rmse:0.945570
    -   On LB : Did not try
-   When LOS was factorised, it disappeared as top gain feature
    -   \[200\] test-rmse:0.978460 train-rmse:0.938315
-   LOS new is numeric
    -   \[200\] test-rmse:0.977013 train-rmse:0.938877
-   Feature /Gain /Cover no\_of\_rooms 0.16112941321 0.02965163869476  
    length\_of\_stay\_new 0.10876715108 0.01307624159602  
    total\_pax.2 0.06647513055 0.00899993784670  
    roomnights 0.04114446519 0.02850132133291  
    resort\_id\_checkin\_date\_month 0.03605975888 0.01641730914433  
    persontravellingid\_length\_of\_stay 0.02864570289
    0.01208988920054  
    resort\_id\_memberid 0.02838646881 0.01389018024741  
    resort\_id\_length\_of\_stay 0.01775251701 0.01333075930600  
    persontravellingid.47 0.01704931531 0.00496852309983  
    checkin\_date\_year\_memberid 0.01620617944 0.01399446282786
