############################################################################
############################################################################
##  Document Path: includes/body/hist.distr/ui.R
##
##  Description: User interface for histogram of WT and Age plots
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################
# plot panels


body.panel.right.etapair <- card.pro(
  title = "ETA pair plots",
  header.bg = "blueLight",
  icon = icon("chart-simple"),
  editbtn = 1L,
  colorbtn = 1L,
  collapsed = TRUE,
  xtra.header.content = tags$span(id="repetapairbioplot",class="text-danger"),

  div(class ="d-md-flex gap-2",
      selectInput("etapairplotsfileuse", "Model to view", choices = c(), multiple = FALSE, width = "60%"),
      div(actionButton("makeetapairplotscont","generate", icon = icon("person-running")),style="padding-top: 24px;")
  ),
  plotOutput("etapairplotscontinuous", height = 600)
  ,
  sidebar = div(
    tags$h3("etapair plot settings"),
    tags$i("If chanes are made in the settings, the user must click the generate button to re-plot", style="color:red"),
    selectInput("etapairplotstdplot",  "Standardize?", choices = c("Yes"=1,"No"=2), width = "90%"),
    selectInput("etapairdensity2d",  "ETA 2D density position", choices = c("lower","none", "upper"), width = "90%"),
    textInput("etapairlabels", "ETA labels (separated by comma)","")
  ),
  footer = list(
    downloadButton("etapairdownloadimg", "Download plot file (ggplot)", icon = icon("image")),
    downloadButton("etapairtimedownloadimg2", "Download plot object (table1)", icon = icon("image"), class="downloadbtns2"),
    downloadButton("etapairndloadconcvt2", "Download plot code (R)", icon = icon("code"), class="downloadbtns")
  )
)
