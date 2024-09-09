# PD-RegressionAnalysis

Reference Paper: Accurate Telemonitoring of Parkinson's Disease Progression by Noninvasive Speech Tests, DOI: 10.1109/TBME.2009.2036000

The project aims to predict the severity of vocal impairment in PD patients using statistical regression models, based on speech data collected through an at-home testing device. Key aspects include:

## Parkinson’s Disease and Vocal Impairment: 
Vocal anomalies, such as hypophonia and dysphonia, are significant but often overlooked symptoms of PD, affecting 70%-90% of patients. These impairments can serve as early indicators of the disease.

## Data Collection: 
The study used data from 42 PD patients collected over six months. Speech data, including variables like jitter, shimmer, and other vocal features, were analyzed alongside Unified Parkinson’s Disease Rating Scale (UPDRS) scores, which measure motor and non-motor symptoms.

## Exploratory Analysis: 
Initial statistical summaries highlighted relationships between speech features and PD severity. Outliers and non-normal data distributions led to transformations, such as logarithmic adjustments, to normalize the data.

## Regression Models: 
Several models were tested to predict UPDRS scores based on vocal data, including linear models, ridge, lasso, and elastic net regressions. However, these models suffered from low R-squared values and multicollinearity issues, indicating limited predictive power.

## Decision Tree Models: 
Due to the non-linear nature of the data, decision trees
were used, and they performed better in predicting PD severity based on vocal features. A logarithmic transformation of the data further improved the model’s accuracy. The decision tree model ultimately emerged as the best fit, yielding the highest predictive power with metrics like RMSE (4.41), MAE (3.14), and R-squared (0.797).

## Conclusion: 
The study concludes that vocal impairment data, when properly analyzed using non-linear models like decision trees, can effectively predict Parkinson's disease progression. The findings suggest that remote speech monitoring, combined with sophisticated modeling techniques, could be a valuable tool for early detection and intervention in PD.

