header.main = titlePanel(tags$div(
  tags$img(src="logo.png", height="30")," ",
  tags$img(src="modelsuite.png", height="25", style="display: inline-block;"),
  tags$div(class = "hidden-mobile hidden-tablet pull-right",
           #actionButton("moduleproj", "Module", icon = icon("cog")),
             actionButton("aboutproject", "", icon = icon("question")))
), windowTitle = "Exploratory Data Analysis")
