# VancouverCrimeDataVisualization
Crime mapping for Vancouver between 2003-2019
### Developed by - Kushal Mahalingaiah and Harsh Sharma, University of Alberta

## Requirements

1) R Studio
2) leaflet
3) gfplot2
4) shiny
5) readr
6) dplyr
7) rgdal
8) rsconnect - optional, required only if you want to deploy

## File details

1) app.R - main code file
2) crime.csv - data file

## How to run

1) Make sure your dataset csv is in the same folder as app.R
2) Run the app file using rstudio
```
shiny::runApp()
```
The port number for localhost deployment will be displayed on screen and can be used to access the page in any browser.

3) To deploy, load rsconnect and after setting up your account key and private, use

```
library(rsconnect)
rsconnect::deployApp('/full/path/to/the/app/folder') 
```
