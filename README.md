# Standardized Pharmacometric Dashboard for Model Evaluations [Shiny]

### Started, but ongoing...

<img src="www/preview3.png">


__Sample version 1 deployed template__: https://pharmacometric.shinyapps.io/

Standardizing scripts for creating pharmacometric model evaluation outputs, such as summary tables and visual predictive check plots, is crucial for ensuring consistency, reproducibility, and clarity in data presentation. Standardized scripts facilitate collaboration among researchers by providing a common framework that reduces variability in analysis and interpretation. 

The current shiny template for models, plays a vital role in this standardization effort by automating the generation of R code for all scripts used in creating plots and tables. This automation not only streamlines the workflow but also minimizes the risk of errors, allowing researchers to focus on the scientific interpretation of their results rather than the intricacies of coding. By adopting such standardized tools, the pharmacometric community can enhance the quality and reliability of model evaluations.


### Usage 
```r

# Download and unzip the content of this repository
# Set working directory to the unzipped folder
setwd("pharmacometric-shiny-template-model")

# Load shiny and run app
library(shiny)
runApp(launch.browser = 1L)

```


```
# Required libraries and versions


```
### Features

The following features are available in the current template for you to get started.

 - __Pre-built modules__ Already configured set-up for modules for exploratory data analysis. User may tweak further to their specific routines
 - __Example data & uploads__ Availability of data to test out the app, but also a feature that allows upload of user dataset for analysis
 - __Plot/table output and code download__ Download all output plots and tables either as final outputs (png/docx) or objects (ggplot/flextable/table1) or code download to direct use in submissions
 - __User customizable interface__. This means the user can move around the panels to desired locations on the screen. They may also change the title and color of the panels to suite their needs. 
 - __Panel location and features saved across sessions__. This means that after re-arrangement of panels, changing panel colors or titles, one may refresh the page and the settings are retained.
 - __Resizable Panels__. Panels are resizable to allow easy focus on specific contents, like plots or tables of result.
 - __Aesthetics for completion__. App title are formatted to look professional. A sample icon is also provided, which may be replaced when updating the template to suit your need.
 
 
 
### Issues

If you have issues or questions, create an 'issue' or contribute to one within the issue tab of this github repository


### Contributors

Contributors interact with the project on GitHub by filing new issues, improving existing issues, or submitting pull requests. Anyone can become a contributor, which means there is no expectation of commitment to the project, no required set of skills, and no selection process.
