
requiredPackages = c('shiny','highcharter','shinythemes',"actuar","DT","dplyr","rintrojs","shinycssloaders",
                     "htmltools","devtools")
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
}

requir = c('tychobratools')
for(p in requir){
  if(!require(p,character.only = TRUE)) devtools::install_github(p)
}



library(shiny)
library(highcharter)
library(shinythemes)
library(actuar)
library(DT)
library(dplyr)
library(rintrojs)
library(shinycssloaders)
library(tychobratools)
library(htmltools)
library(shinyWidgets)

# turn off scientific notation
options(scipen=999)

freq_choices <- c("Poisson" = "poisson", 
                  "Binomial" = "binomial", 
                  "Negative Binomial" = "nbinomial")

sev_choices <- c("Lognormal" = "lognormal", 
                 "Pareto" = "pareto", 
                 "Weibull" = "weibull", 
                 "Gamma" = "gamma", 
                 "Exponential" = "exponential")