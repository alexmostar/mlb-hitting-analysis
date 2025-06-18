# MLB Hitting Analysis
This project is a regression based analysis of 2024 MLB hitting data to identify potential breakout candidates using advanced metrics like barrel percentage, hard-hit rate, and xwOBA.

## Files included 

-**savant_breakout.xlsx**
main dataset used for the regression model

-**breakout_age.xlsx**
supplemental dataset used to filter and categorize players by age then merged into main dataset.

-**mlb_regression_analysis.R**
R script used for data cleaning, modeling, and plot creation**.

-**MLB_Breakout_Analysis_Writeup.pdf**
Written report summarizing the analysis, player insights, and visualizations.

## Project Overview

The goal of this analysis is to: 
- Identify potential breakout candidates using several advanced Statcast metrics
- Build a multiple linear regression model to predict xwOBA.
- Compare predicted values to actual outcomes
- Highlight players who may be undervalued or underperforming due to luck or small sample sizes.

Players like **Jonathan Aranda**, **Jerar Encarnacion**, and **Dominic Canzone** were identified as potential breakout candidates based on their strong underlying metrics and model performance. 
Aranda was highlighted in part due to his success again in 2025, while the Encarnacion and Canzone were identified due to the potential based on the model, despite limited playing opportunities this season.

## Tools Used

- **R** for data cleaning, modeling, and visualization
- **ggplot2** for generating plots
- **Excel** for data prep 
- **Baseball Savant**, **Baseball Reference**, and **FanGraphs**  used for data sources
