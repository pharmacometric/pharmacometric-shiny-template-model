############################################################################
############################################################################
##  Document Path: ~/GitHub/pharmacometric-shiny-template-eda/includes/body/conc.vs.time/ui.R
##
##  Description: User interface for concentration vs time plots
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################

#for(u in indexed(libs))message(u$key,"-",u$val)
# for exporting code
GLOBAL$code.convtm.tpl = paste0(this.path,"/code.tpl")
GLOBAL$code.convtm.libs.glue = paste0('c("',paste(libs[c(10,11,12,20,19)], collapse = '","'),'")')


# plot panels

body.panel.right.gof = card.pro(
  title = "Goodness of fit plots",
  icon = icon("chart-area"),
  collapsed = TRUE,
  header.bg = "blueLight",
  xtra.header.content = tags$span(id="reportgofplotstats",class="text-danger"),
  div(id = "reportgraphstatus2"),
  sidebar = list(
    tags$h3("Variable setting"),
    textInput("gofdvvar","DV variable","DV", width = "90%"),
    textInput("goftimevar","Time variable","TIME", width = "90%"),
    textInput("gofPREDvar","PRED variable","PRED", width = "90%"),
    textInput("gofIPREDvar","IPRED variable","IPRED", width = "90%"),
    textInput("gofcwresvar","CWRES variable","CWRES", width = "90%"),
    textInput("gofconcuni","Concentration unit","ug/mL", width = "90%"),
    textInput("goftimeuni","Time unit","hrs", width = "90%"),

    tags$h3("Plot setting"),
    textInput("filterpre","Subset output before plot","", width = "90%"),
    numericInput("setcols","column number",3),
    numericInput("setrows","row number",2),
      checkboxInput("shouldLog","Log scale?",value = FALSE)
  ),
  div(id = "deliveralert", class = "text-danger"),
  div(class ="d-md-flex gap-4",
      selectInput("modelsgofshow","Models to plot",choices = c(), multiple = TRUE, width = "55%"),
      selectInput("modgofftype","Plot type",choices = c(
        "Choose plot" = "",
        "PRED vs DV" = "dvpred",
        "IPRED vs DV" = "dvipred",
        "PRED vs CWRES" = "cwrespred",
        "Time vs CWRES" = "cwrestime"
      ), width = "30%"),
      div(style="padding-top:23px",actionButton("refreshgofplots","reset plots",icon = icon("refresh")))

  ),
  div(
    plotOutput("plotgofsui", height = 750)
  )
  ,
  verbatimTextOutput("sel_names"),
  footer = list(
    downloadButton("gofdownloadimg", "Download plot file (png)", icon = icon("image")),
    downloadButton("gofdownloadimgobj", "Download plot object (ggplot)", icon = icon("image")),
    downloadButton("gofdownloadcode", "Download plot code (R)", icon = icon("code"), class="downloadbtns")
  )


)
