# OverlapAnalysis
This repository contains a sample data set and some R code for performing Overlap Analysis on it, as well as the source code for three apps written with R Shiny. The two oldest, "Shiny2" and "ShinyLunar" are deprecated, replaced by "Starshiny".

Overlap analysis is a method for quantifying how much overlap exists between two density curves. In an ecological context, we can use it to compare the daily patterns of activity in two groups of animals, such as comparing between different species or even between different populations of the same species. Our research data comes from camera trap studies performed on mammals in the lowland and cloud forests of Costa Rica.

### Archive
This folder contains a few subfolders with old projects in it. These are kept around for historical reference, but they are not maintained.

* The R scripts folder mostly contains R programs that can perform overlap analysis on a data set and output bulk results rather than one at a time. It may be helpful for providing additional examples of how to use the overlap package in R.

* The Shiny2 folder contains the source code for a web app that is available at https://tjwieg.shinyapps.io/shiny2/. That app has most of the functionality of the scripts found here, with a more beginner-friendly graphical interface. However, it can only calculate a single overlap coefficient (and associated 95% confidence interval) at a time.

* The ShinyLunar folder contains the source code for a web app that is available at https://tjwieg.shinyapps.io/ShinyLunar/. That app is built to calculate many of the same overlap statistics that can be done by Shiny2 and the other scripts here, but it uses the phase of the moon as the independent time variable (x-axis).

### Examples
This folder contains a sample data set (sampledata.csv), some sample plots, some basic results from our data, and a simple script to show some of the basic functionalities of the overlap package in R.

### Starshiny
This folder contains the source code for a web app that will be available online when finished. The app should offer all the functionality of the scripts in the "Archive/R Scripts" folder, but with an intuitive graphical interface. Note that it can only calculate one set of values at a time, so writing a more complex script to run directly in R may be preferable for bulk data analysis.


This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version.
