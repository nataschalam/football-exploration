# Exploratory Analysis of Statsbomb Football Match Data 
Football match data exploration of Men's World Cup 2022 and Women's World Cup 2023 to understand differences between men's and women's playing styles beyond physiological differences between gender. 

## Data
- Data exploration used freely accessible football match level data from Statsbomb (see below for how to install Statsbomb library)

## Methodology
- Data cleaning and basic calculations performed to derive specific metrics (e.g., passing accuracy)
- Performed logistic regression LASSO to determine most important features to focus on
- Performed multiple linear regression and t-tests to uncover trends and statistical significance between different football metrics 
- Visualised results in using ggplot2 library in the form of bar plots and scatter plots

## Results / Key Findings
- The LASSO model showed metrics related to passes and applied pressure were key predictors of a men's vs. women's match.

![Top Metrics Predicting Men's vs. Women's Match](plots/lasso_results.png)

- Passing accuracy was significantly higher in men's matches, however, confounding factors such as passing direction, length of pass and applied pressure play key roles in shaping the discrepancy in passing accuracy

![Passing Accuracy by Team](plots/pass_accuracy_team.png)

![Passing Accuracy Accounting for Applied Pressure and Direction](plots/final_plot.png)

- The influence of confounding variables emphasises the importance of looking beyond high level differences and ensuring context and nuance is captured when analysing football data

## How to Run Analysis
1. Open the R script called: `passing_analysis.R`
2. Follow the installation instructions to obtain access to the StatsbombR library using the repo: https://github.com/statsbomb/StatsBombR
3. Install other required packages:
   ```r
   install.packages(c("tidyverse", "ggplot2", "dplyr", "tidyr", "purrr", "glmnet", "interactions"))
