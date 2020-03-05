---
  layout: stat697
---
  
Notes and Slides
-------

Please keep in mind that these will be updated periodically as I catch and am otherwise made aware of typos.

## Lecture Notes
* ARMA Estimation: [`notes_5.pdf`](https://maryclare.github.io/stat697/content/notes/notes_5.pdf)
* Forecasting: [`notes_4.pdf`](https://maryclare.github.io/stat697/content/notes/notes_4.pdf)
* AR, MA, and ARMA: [`notes_3.pdf`](https://maryclare.github.io/stat697/content/notes/notes_3.pdf)
* Basic Time Series Concepts: [`notes_2.pdf`](https://maryclare.github.io/stat697/content/notes/notes_2.pdf)
* Introduction and a review of linear regression: [`notes_1.pdf`](https://maryclare.github.io/stat697/content/notes/notes_1.pdf)

## Lecture Code
* Using `acf` and `pacf` to choose model order: [`ma_arima.R`](https://maryclare.github.io/stat697/content/code/acf_pacf.R)
* Using the Durbin-Levinson Algorithm: [`durbin_levinson.R`](https://maryclare.github.io/stat697/content/code/durbin_levinson.R)
* More on using `arima` for moving average models: [`ma_arima.R`](https://maryclare.github.io/stat697/content/code/ma_arima.R)
* More on using `arima` for autoregressive models: [`ar_arima.R`](https://maryclare.github.io/stat697/content/code/ar_arima.R)
* Using `arima`: [`arima_for_ar.R`](https://maryclare.github.io/stat697/content/code/arima_for_ar.R)
* The ACF of a Nonstationary Process: [`acf_nonstationary.R`](https://maryclare.github.io/stat697/content/code/acf_nonstationary.R)
* Motivating Time Series Models: [`motivate_tsmodels.R`](https://maryclare.github.io/stat697/content/code/motivate_tsmodels.R)
* Regression Continued: [`regression_review_continued.R`](https://maryclare.github.io/stat697/content/code/regression_review_continued.R)
* Regression: [`regression_review.R`](https://maryclare.github.io/stat697/content/code/regression_review.R)

Lecture Data: 
* Massachusetts Exports: [`expo`](https://maryclare.github.io/stat697/content/data/expo.RData)
* Strawberry Prices: [`straw`](https://maryclare.github.io/stat697/content/data/straw.RData)

Exam Solutions:
* [Exam 1](https://maryclare.github.io/stat697/content/homework/exam_1_sol.pdf)

Resources for `R`:
* We will use `R` extensively. Here are some recommended resources if are either entirely or somewhat new to to `R`:
    - [`swirl`](https://swirlstats.com/students.html) is a very unique resource that teaches you how to use `R` from within `R`. I recommend the ``R Programming'' course, it's surprisingly fun even if you know most or all of the concepts already!
    - The folks at RStudio have made a nice [cheatsheet](https://www.rstudio.com/wp-content/uploads/2016/10/r-cheat-sheet-3.pdf) that summarizes various essential features of `R`.
    - `CRAN` has a very comprehensive [reference card](https://cran.r-project.org/doc/contrib/Short-refcard.pdf).
    - The course page for [Introduction to R for Social Scientists](https://rebeccaferrell.github.io/CSSS508/), a course on `R` taught at the University of Washington a few years ago.
    - The book [Advanced `R`](http://adv-r.had.co.nz) by Hadley Wickham gives a very detailed look at `R`'s data structures, e.g. matrices, arrays, and data frames.
* You will also learn a lot by comparing your homework solutions to the ones I provide, although keep in mind that the way I code up solutions to the homework problems is rarely the only way someone could code them up!

Time Series Textbooks (All Online, Free via UMass Amherst Library):
* [Shumway and Stoffer (2006)](https://link.springer.com/book/10.1007\%2F0-387-36276-2) is our main reference. Based on my research, it's the best regarded textbook for a course of this type. That said, there are some parts where notation gets quite confusing and we'll stray from it a bit.
* [Chan (2010)](https://onlinelibrary-wiley-com.proxy.library.cornell.edu/doi/book/10.1002/9781118032466) is a really lovely book that has very clear explanations, but sometimes goes a bit above and beyond the technical level we want for this class.
* [Cowpertwait and Metcalfe (2009)](https://link-springer-com.proxy.library.cornell.edu/book/10.1007\%2F978-0-387-88698-5) is a simpler book that focuses more on `R`. It's not quite technical enough to be a primary reference for this course, but it is a very nice place to learn various concepts, e.g. how to simulate from different time series models.
* [Tsay (2010)](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470644560) is a on a similar but slightly more advanced level than our course, but it contains less `R` code and is more financially oriented.
* [Brockwell and Davis (1991)](https://www.springer.com/us/book/9780387974293) is too technical for our purposes and doesn't have much in the way of `R` examples. But, it is very detailed and comprehensive. When I want to make sure I understand how something I'll be teaching can be proven, I look here.
* [Homes, Scheuerell, and Ward (2018)](https://nwfsc-timeseries.github.io/stat697-labs/) is a very practically oriented online textbook motivated by time series problems in Fisheries and the Environmental Sciences.
