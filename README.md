# OverlapAnalysis
This repository contains a sample data set and some R code for performing Overlap Analysis on it.

Overlap analysis is a method for quantifying how much overlap exists between two density curves. In an ecological context, we can use it to compare the daily patterns of activity in two groups of animals, such as comparing between different species or even between different populations of the same species. Our research data comes from camera trap studies performed on mammals in the lowland and cloud forests of Costa Rica.

-The Example folder contains a sample data set (sampledata.csv), some sample plots, some basic results from our data, and a simple script to show some of the basic functionalities of the overlap package in R.
-The R scripts folder mostly contains R programs that can perform overlap analysis on a data set and output bulk results rather than one at a time. It may be helpful for providing additional examples of how to use the overlap package in R.
-The Shiny2 folder contains the source code for a web app that is available at https://tjwieg.shinyapps.io/shiny2/. That app has most of the functionality of the scripts found here, with a more beginner-friendly graphical interface. However, it can only calculate a single overlap coefficient (and associated 95% confidence interval) at a time.
