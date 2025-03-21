############################################################################
############################################################################
##  Document Path: /includes/body/modelruns/ui.R
##
##  Description: User interface for main body section
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################

# plot panels
body.panel.right.moderun = card.pro(

  title = "Model Runs & Estimates",
  header.bg = "blueLight",
  icon = icon("table-list"),
  editbtn = 1L,
  collapsed = FALSE,
  tabs = list(
    tabEntry(
      "Models",
      actionButton("comparemodelsest","Compare estimates",icon=icon("code-compare")),
      actionButton("refreshmodest","Refresh models",icon=icon("refresh")),
      downloadButton("parmmodelsest","Download estimate table and R code",icon=icon("download")),br(),br(),
      uiOutput("modeltabsrun")
    ),
    tabEntry(
      "Running tasks",
      actionButton("refreshruns","Refresh runs",icon=icon("refresh")),
      actionButton("clearcptruns0","Clear finished runs",icon=icon("trash-arrow-up")),
      actionButton("clearcptruns","Clear all runs",icon=icon("trash")),br(),br(),
      uiOutput("modelrunning")

    )
  ),
  sidebar = div(
    selectInput("modellistsort", "Sort model list", choices = c("Decreasing" = 1,"Increasing" = 0), width = "90%"),
    hr(),
    tags$h3("Columns to show"),
    selectInput("shwest", "Minimization", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "Covariance", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "ZeroGradient", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "OFV", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "NSIG", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "Method", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "Description", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "Dataset", choices = c("Yes" = 1,"No" = 0), width = "90%"),
    selectInput("shwest", "Notes", choices = c("Yes" = 1,"No" = 0), width = "90%")
  ),
  xtra.header.content = div(id="reportmodliststats",class="text-danger"),
  footer = list(tags$b("Legend: "),ticksuccess," = successful",tickfailed," = failed or uncompleted",tickplay," = run model")
)

