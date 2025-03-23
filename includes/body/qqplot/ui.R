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
body.panel.right.qqplot = card.pro(
  title = "QQ-norm plots",
  header.bg = "blueLight",
  icon = icon("chart-simple"),
  editbtn = 1L,
  colorbtn = 1L,
  collapsed = 1L,
  xtra.header.content = tags$span(id="repqqbioplot",class="text-danger"),

             div(class ="d-md-flex gap-2",
                 selectInput("qqplotsfileuse", "Model to view", choices = c(), multiple = FALSE, width = "60%"),
                 div(actionButton("makeqqplotscont","generate", icon = icon("person-running")),style="padding-top: 24px;")
             ),
             plotOutput("qqplotscontinuous", height = 600)
   ,
  sidebar = div(
    tags$h2("QQ plot settings"),
    tags$i("If chanes are made in the settings, the user must click the generate button to re-plot", style="color:red"),
    selectInput("qqplotseltype",  "Type of plot", choices = c("both", "qqnorm", "hist"), width = "90%"),
    selectInput("qqplotstdplot",  "Standized?", choices = c("Yes"=1,"No"=2), width = "90%")
  ),
  footer = list(
    downloadButton("qqdownloadimg", "Download plot file (ggplot)", icon = icon("image")),
    downloadButton("qqtimedownloadimg2", "Download plot object (table1)", icon = icon("image"), class="downloadbtns2"),
    downloadButton("qqndloadconcvt2", "Download plot code (R)", icon = icon("code"), class="downloadbtns")
  )
)

