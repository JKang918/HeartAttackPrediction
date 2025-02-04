# Heart Attack Prediction

## Overview
### summary: logistic regression to measure the risk of possible heart attack

This project was a team project conducted as part of the [ISYE 6414 Regression Analysis course](https://oscar.gatech.edu/bprod/bwckctlg.p_disp_course_detail?cat_term_in=202408&subj_code_in=ISYE&crse_numb_in=6414) at Georgia Tech during Fall 2024. My contribution for the project is including but not limited to:

* constructing the overall structure of analysis
* writing the majority of R codes
* writing 'model evaluation' section in the presentation file and final report

Since I handled most of the coding tasks, I intend to share the work on GitHub.

The summary of analysis can be stated as below:

1. Load a Kaggle data set consisting of various predictors for heart attak
2. Train/Test split
3. Preprocess the imported data set
4. Perform exprolatory data analysis
5. Initialize the model (logistic regression) using all predictors in the data set
6. Conduct bidirectional stepwise regression
7. Model evaluation - overfitting check with K-fold cross evaluation, accuracy rate, ROC curve, etc.
8. Test the model on the test data set

Since the course was on regression techniques, we used logistic regression. In the future, I hope to gain further insigt by leveraging other machine learning classification method to increase the perfomance.  

## Folder Structure
- **data/**: Contains the datasets. It is a preprocessed data set so no further work was required.
- **scripts/**: Includes R scripts for analysis.
- **results/**: Contains statistical results and outputs.
- **figures/**: Contains plots and visualizations.