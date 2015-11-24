---
title: "Temporal Anomaly Work"
author: "Temporal Group!"
date: "November 8, 2015"
output: html_document
---

## Installation


This repository has collected different work from the Temporal Group. To run the Shiny app, you will need to install R, then from within R you can install the required R packages as 
follows: 

```r
install.packages(c("shiny", "ggplot2", "data.table", "lubridate", "magrittr", 
                 "gridExtra", "ggfortify", "forecast", "changepoint", "strucchange"), 
                 dependencies=TRUE)
```

Then you can run the app from within Rstudio by clicking Run App after opening either 
`ui.R` or `server.R`. 

## The app

For a selected condition (concept_id) and data source, the Shiny app presents results of different analyses in 4 tabs:

The 1st tab on the app overviews the data and calculates a "typical" annual pattern. It then highlights data points that statistically deviate from the "typical" pattern.

![tab 1](https://github.com/alexperrone/dqcdm-temporal/blob/master/img/screenshot-03.png)

```{r}
##the R code goes here
```

The 2nd tab returns a table of high prevalence .....

Picture goes below...
![tab 2](https://github.com/alexperrone/dqcdm-temporal/blob/master/img/screenshot-04.png)

```{r}
##the R code goes here
```

The 3rd tab produces plots from a set of time series analyses. 

Picture goes below...
![tab 3](https://github.com/alexperrone/dqcdm-temporal/blob/master/img/screenshot-07.png)

```{r}
##the R code goes here
```

The 4th tab plots prevelance overtime of the selected concept_id by database (for every database available).  

Picture goes below...
![tab 4](https://github.com/alexperrone/dqcdm-temporal/blob/master/img/screenshot-08.png)

```{r}
##the R code goes here
```
