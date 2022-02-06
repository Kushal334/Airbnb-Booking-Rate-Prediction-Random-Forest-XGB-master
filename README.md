## _Airbnb-Booking-Rate-Prediction_

Hi there!! In this repository, me and my team outperformed 19 other teams(85 other students) and won a Kaggle-style competition that our amazing professor,Dr. Jessica Clark, organised using using Airbnb's dataset for high booking rate.This dataset has a total of **70 feature attributes** and **100K rows**. Our goal as a team of 5 students and a part of the Data Mining and Predictive Analytics course, during the period of this 2 month competition, was to come up with a predictive model using any machine learning technique to score the highest accuracy. 

------------------------------------

For this dataset, we tried the following four models with their accuracies listed as below 

|   Model                |Test Accuracy|
|------------------------|-------------------|
|Logistic Regression     |   0.7929503       |
|Naive Bayes Model       |   0.7743653       |
|K-Nearest Neighbours    |   0.8295174       |
|Random Forest           |   0.8380622       |
|XG Boost                |   0.8451639       |

The best model, as we can see, is the XG Boost model with a validation accuracy of 84.51%.

Following is my contribution to the project :

1) Feature Engineering of 14 variables out of the 70 variables
2) Building K-Nearest Neighbours model.
3) Hyper parameter tuning to find the best "k" value
4) Building the Random Forest model (using traditional RandomForest library and the new Caret Library)
5) Hyper parameter tuning of the Random Forest model (mtry = 14) and 5-fold cross validation

## Overview

  1. The Data folder consists of the data that we used to train these models, this is the output data of our feature engineering process
  2. The Feature Engineering folder consists of the feature engineering R script that was used to clean the original messy data
  3. Final Predictions consists of an Excel file that stores our final predictions on the test data that scored us the highest accuracy in our class:)
  4.Machine_Learning_Models consists of all the four machine learning model scripts in R
  
## Results
![Winning Team](https://github.com/Aishwarya4823/Airbnb-Booking-Rate-Prediction/blob/master/Winner%20Snapshot/We%20won!!.PNG "We won!!")

Lastly, I would like to thank my professor and my four team members 
1) **Cindy Chang**
2) **Huyen Nguyen**
3) **Jiakun Luo**
4) **Wenjing Cui**

Without these beautiful people, this memory would not have been created! I hope you like this chapter:D 
