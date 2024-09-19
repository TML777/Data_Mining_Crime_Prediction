# Crime Prediction in California Cities
## Overview
  This project explores and predicts crime, specifically the number of murders, in various cities across California. The data used spans from 2005 to 2019 and was collected from the FBI website. We deployed a web scraper to download the data from the FBI website. We employed data mining techniques to clean, preprocess, and analyze the data, deploying both Linear Regression and Random Forest algorithms to predict the murder rates with high accuracy. 
  
   Some feature engineering processes used include the reduction of unnecessary/irrelevant columns and the addition of new attributes such as the density of the population. The most cited data mining prediction algorithms included Linear Regression Rate, Random Forest, Gradient Boosting, and Bayesian Network algorithms. After a literature review of related topics, we deployed Linear Regression and Random Forest algorithms.
  
  We used 5-fold cross-validation for training and testing the data. RMSE, MSE, MAE, MAPE, and Confusion Matrix were used to evaluate the accuracy of each model. Each algorithm predicted the murder number relatively accurately. Based on the result we figured out Linear Regression is better for predicting the number of murders in
California cities.

## Project Details
This project was completed as part of the Data Mining course (COMP 541) at California State University, Northridge.

## Data
The dataset consists of crime statistics from 2005 to 2019 for cities in California. It was gathered from the FBI's Crime in the U.S. reports. Data cleaning included dealing with missing values, feature engineering (e.g., calculating population density), and merging data tables.

## Technologies Used
* **Programming Language:** R
* **Libraries:** Tidyverse, Vroom, Readr, Caret, Corrplot, Metrics, Dplyr, Plyr
* **Algorithms:** Linear Regression, Random Forest
* **Data Source:** FBI Crime Data (2005-2019)

## Key Features
* **Web Scraping:** Collected crime data from the FBI website using a custom web scraper.
* **Data Preprocessing:** Cleaned and combined 15 datasets, handled missing values, and performed feature engineering.
* **Data Mining:** Used Linear Regression and Random Forest algorithms for prediction, evaluated using metrics such as RMSE, MAE, MAPE, and Confusion Matrix.
* **Cross-Validation:** Performed 5-fold cross-validation to ensure the models' accuracy.

## Results
* **Linear Regression:** Achieved an accuracy of 99.99% in predicting the number of murders.
* **Random Forest:** Achieved an accuracy of 80.87%.
* **Conclusion:** Linear Regression outperformed Random Forest in this dataset due to the high correlation between attributes.

## Future Work
* **Geographically Weighted Regression:** Explore crime patterns by considering the geographical locations of the cities.
* **Deep Learning:** Use Neural Networks for more complex and detailed crime prediction.

## Dataset
The datasets used in this project can be found [here](https://ucr.fbi.gov/crime-in-the-u.s).

## Notes
[Project Report](https://github.com/TML777/Crime_Rate_DM/blob/main/Project%20Report.pdf) is a full report on the whole project. [Project Proposal](https://github.com/TML777/Crime_Rate_DM/blob/main/Project%20Proposal.pdf) and [Final Presentations](https://github.com/TML777/Crime_Rate_DM/blob/main/Final%20Presentation.pdf) are presentations done in class.
