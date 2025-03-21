header.main = titlePanel(tags$div(
  tags$img(src="modr_urbanjungle.png", height="25")," ",
  tags$img(src="ms_suite.png", height="30", style="display: inline-block; margin-top: 10px;"),
  tags$div(class = "hidden-mobile hidden-tablet pull-right",
           #actionButton("moduleproj", "Module", icon = icon("cog")),
             actionButton("aboutproject", "", icon = icon("question")))
), windowTitle = "Exploratory Data Analysis")
