############################################################################
############################################################################
##  Document Path: ~/GitHub/pharmacometric-shiny-template-eda/includes/body/cov.relationship/ui.R
##
##  Description: User interface for main body section
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################

# plot panels
body.panel.right.etaplot = card.pro(

  title = "Eta covariate plots",
  header.bg = "blueDark",
  icon = icon("chart-simple"),
  editbtn = 1L,
  collapsed = TRUE,
  xtra.header.content = tags$span(id="reportetaplotstats",class="text-danger"),
  div(id = "repetabioplot"),
  tabs = list(
    tabEntry("Continuous",
             div(class ="d-md-flex gap-2",
                 selectInput("etaplotsfileuse", "Models", choices = c(), multiple = TRUE, width = "40%"),
                 selectInput("etaplotsparmcontcat","Variables to use", choices = c("WT","AGE","BSA","BMI","CLCR"), multiple = 1L, width = "40%"),
                 div(actionButton("makeetaplotscont","generate", icon = icon("person-running")),style="padding-top: 24px;")
             ),
             plotOutput("etaplotscontinuous", height = 600)
    ),
    tabEntry("Categorical",
             div(class ="d-md-flex gap-2",
                 selectInput("etaplotsfileuse2",  "Models", choices = c(), multiple = TRUE, width = "40%"),
                 selectInput("etaplotsparmcontcat2","Variables to use", choices = c("SEX","SMOKING","DIABETES"), multiple = 1L, width = "40%"),
             div(actionButton("makeetaplotscont2","generate", icon = icon("person-running")),style="padding-top: 24px;")
    ),
             plotOutput("etaplotscategorical", height = 600)
    )
  ),
  sidebar = div(
    tags$h2("ETA plot settings"),
    tags$i("If chanes are made in the settings, the user must click the generate button to re-plot", style="color:red"),
    numericInput("etaplotsetplotcol",  "Number of columns", value = 2, width = "90%"),
    textInput("etaplotysuffix",  "Y-axis suffix", value = "Value", width = "90%"),
    numericInput("etaplotsdpi",  "Plot DPI", value = 300, width = "90%")
  ),
  footer = list(
    downloadButton("vtabdownloadimg", "Download plot file (ggplot)", icon = icon("image")),
    downloadButton("vtabtimedownloadimg2", "Download plot object (table1)", icon = icon("image"), class="downloadbtns2"),
    downloadButton("vtabndloadconcvt2", "Download plot code (R)", icon = icon("code"), class="downloadbtns")
  )
)

