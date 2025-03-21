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
body.panel.right.modelwrite <- card.pro(
  title = "Create & Edit Model",
  header.bg = "yellow",
  icon = icon("code"),
  editbtn = 1L,
  collapsed = 1L,

  # file chooser

    div(class = "d-flex gap-4",
      selectInput("editcreataction", "Edit or create files", choices = c(
        "Edit file " = 1,
        "Create New" = 2
      )),
      conditionalPanel(
        condition = "input.editcreataction == 1",
        selectInput("editcreataction1", "Select file", choices = c())
      ),
      conditionalPanel(
        condition = "input.editcreataction == 2",
        textInput("editcreataction2", "File name", "", placeholder = "enter file name..."),
      ),
      tags$div(
        style="padding-top:23px", #mt-2 pt-4
        actionButton("editcreateend", "Save file"),
        actionButton("clearcodefile", "Clear content"),
        actionButton("modtempcodefile", "Model library template (v2)", disabled = TRUE)
      )
    )
  ,



  # editor
  tags$div(id = "editmessagerep", class = "text-danger pb-3 font-weight-bold"),
  aceEditor(
    outputId = "aceeditor",
    selectionId = "selection1",
    value = "",
    placeholder = "File is empty. Start typing code..."
  ),

  # sidebar
  sidebar = div(
    tags$blockquote("Code Settings"),
    selectInput("codingmode", "Code Language: ", choices = modes, selected = "r", width = "90%"),
    selectInput("codingtheme", "Code Theme: ", choices = themes, selected = "solarized_light", width = "90%"),
    numericInput("codingsize", "Tab size:", 4, width = "90%"),
    numericInput("codingheight", "Code box height (px):", 500, width = "90%")
  )
)
