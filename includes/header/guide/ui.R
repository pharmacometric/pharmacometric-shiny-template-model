############################################################################
############################################################################
##  Document Path: includes/body/ui.R
##
##  Description: User interface for header guide panel
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################

body.top.guide = card.pro(
  title = "Model App Guide", width = 12,
  header.bg = "darken",
  removebtn = FALSE,
  colorbtn = FALSE,
  expandbtn = FALSE,
  editbtn = FALSE,
  collapsed = TRUE,
  shadow = FALSE,
  sortable = FALSE,
  shiny::includeMarkdown(file.path(this.path, "model_information.md"))
)
