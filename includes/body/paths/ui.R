############################################################################
############################################################################
##  Document Path: includes/body/ui.R
##
##  Description: User interface for main body section
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################

body.panel.paths <- card.pro(
  title = "Path Selector",
  removebtn = 0L,
  colorbtn = 0L,
  expandbtn = 0L,
  editbtn = 0L,
  collapsed = FALSE,
  icon = icon("folder-open"),
  header.bg = "white",
  radioButtons(
    "checkGroupDatasetT",
    "Type of directory",
    choices = list(
      "User uploaded directory" = 2,
      "Local file directory" = 1
    ),
    selected = 1
  ),
  conditionalPanel(
    condition = "input.checkGroupDatasetT == 2",
    fileInput(
      "ufileupd1a",
      "Upload zipped directory (.zip or .tgz)",
      accept = c(".zip", ".tgz", ".tar.gz"),
      width = "100%"
    )
  ),
  textInput(
    "dirfiletype1a",
    "Files directory path","",
    width = "100%"
  ),
  # shwhdbtn("dirfiletype1afiles"),
  # div(id = "dirfiletype1afiles",class = "hidethis",),
  shinyTree("s2tree", checkbox = TRUE),
  footer = textOutput("trackfileupdates")
)

