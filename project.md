---
  layout: stat697
---
  
  Project
-------


Summary of Tasks:
  
- Submit a data set;
- Choose a data set from the approved datasets;
- Submit analysis as part of homework - to be assigned;
- Submit final paper on Friday, 5/01/20 by 11:59pm. See below for guidelines.

## Rubric for Final Paper
  
The final project will be:
- 5 pages long including figures and tables, excluding `R` code and references;
- Double spaced, with font size 12 and approximate 1 inch margins;
    - If you are using `R` Markdown and cannot figure out how to achieve double spacing, you can single-space but then make sure that your final draft is no more than 3 pages long;
- Written using the word processing software of your choosing;
- Be accompanied by `R` code that reproduces all results.
  
It will be graded out of 25 points as follows:

- (4 points) Introduction and exploratory analysis of proposed data
- (7.5 points) Analysis using ARIMA with last 10% of observations witheld
- (3 points) Conclusions of ARIMA analysis
    - Compare forecasts and 95% intervals for the remaining 10% of observations from the first time series, alongside the true values.  
- (7.5 points) Analysis using a state-space model with last 10% of observations witheld
- (3 points) Conclusions of state-space analysis
    - Compare forecasts and 95% intervals for the remaining 10% of observations from the first time series, alongside the true values. 

Some miscellaneous notes:

  - Introduction and exploratory analysis could include ACF/PACF/smoothed periodogram/subset mean/subset variance plots, and a discussion of any possibly unusual or complicated features of the data, etc.
  - Any time you select a model based on one of several possible tests or one of several possible criteria, justify the test or criteria you use! For instance, if you use augmented Dickey-Fuller comment on why you do that instead of using Dickey-Fuller or Phillips-Perron. Similarly, if you use AICc, explain why you do that instead of using AIC or SIC.  
  
  
## Submitting a Potential Dataset

It is ok (and encouraged) to submit as a group! For full consideration, the submission should include the data (in whatever format you have it in, e.g. Excel or .csv) and an at most one page description which includes:

- One to five sentences describing the data, specifically:
    - The names of the students in the group;
    - How the data was obtained;
    - What the data are measuring;
    - The number of observations (if the data you're interested in contains multiple time series, just describe the number of observations for a single one)
    - The amount of time between each observed value.
    
- A single time series plot (if the data you're interested in contains multiple time series, just pick one);
- A sample autocorrelation plot;
- A sample partial autocorrelation plot.

All students who submit at least one potential dataset will have a single bonus point added to their final project grade.

Here are some links to some places to look for data, which range from data repositories that contain cleaned datasets that are often used to demonstrate various concepts to places where raw (uncleaned) time series data is available for download:

- [Federal Reserve Bank of Saint Louis](https://www.stlouisfed.org)
- [`fivethirtyeight` Package for `R`](https://fivethirtyeight-r.netlify.com/articles/fivethirtyeight.html) 
- [Dryad Data Repository](https://datadryad.org)
- [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets.html?format=&task=&att=&area=&numAtt=&numIns=&type=ts&sort=nameUp&view=table)
- [US Department of the Treasury Finance Data Directory](https://www.treasury.gov/resource-center/financial-education/Pages/fdd.aspx)
- [Google Trends](https://trends.google.com/trends/?ctab=0&date=all&geo=all&q=google&sort=0)
- [Time Series Data Library](https://pkg.yangzhuoranyang.com/tsdl/)
- [City of Chicago Data Portal](https://data.cityofchicago.org)
- [US Government Open Data Portal](https://www.data.gov)
- [`babynames` Package for `R`](https://cran.r-project.org/web/packages/babynames/babynames.pdf)
- [ASA Statistics in Sports Section's Recommended Data Resources](https://community.amstat.org/sis/sportsdataresources)
- [`nbastatR` Package for `R`](http://asbcllc.com/nbastatR/)


Before spring break, I will collect potential datasets. Over spring break, I will explore the submitted data and decide which ones seem well suited for the project. 
   
  