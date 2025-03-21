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
body.panel.right.individ = card.pro(
  title = "Individual plots: DV, IPRED, PRED",
  header.bg = "darken",
  icon = icon("chart-simple"),
  editbtn = 1L,
  colorbtn = 1L,
  collapsed = 1L,
  xtra.header.content = tags$span(id="repdvpredipredbioplot",class="text-danger"),

  div(class ="d-md-flex gap-2",
      selectInput("dvpredipredplotsfileuse", "Model to view", choices = c(), multiple = FALSE, width = "60%"),
      div(actionButton("makedvpredipredplotscont","generate", icon = icon("person-running")),style="padding-top: 24px;"),
      numericInput("dvpredipredplotspage", "Page to view", value="1", width = "90px"),
  ),
  plotOutput("dvpredipredplotscontinuous", height = 600)
  ,
  sidebar = div(
    tags$h3("dvpredipred plot settings"),
    tags$i("If chanes are made in the settings, the user must click the generate button to re-plot", style="color:red"),
    textInput("dvpredipredylab",  "Y-axis label", "Observations/Predictions (ng/mL)", width = "90%"),
    numericInput("dvpredipredplotperpage",  "Plots per page", value = 6, width = "90%"),
    textInput("dvpredipredtime",  "Time variable", "TIME", width = "90%"),
    textInput("dvpredipredlabels", "What to plot (separated by comma)","DV,IPRED,PRED", width = "90%")
  ),
  footer = list(
    downloadButton("dvpredipreddownloadimg", "Download plot file (ggplot)", icon = icon("image")),
    downloadButton("dvpredipredtimedownloadimg2", "Download plot object (table1)", icon = icon("image"), class="downloadbtns2"),
    downloadButton("dvpredipredndloadconcvt2", "Download plot code (R)", icon = icon("code"), class="downloadbtns")
  )
)

