#########################################################################################
#########################################################################################
##
##  Document Path: includes/body/qqplot/server.R
##
##  R version 4.4.1 (2022-04-22)
##
#########################################################################################
##
##  Program purpose:
##
##
#########################################################################################
#########################################################################################
GLOBAL$startqqplotting <- FALSE
plot.this.model <- plot.this.model2 <- c()
observeEvent(input$qqplotsfileuse, {
  updateSelectInput(session, "qqplotsparmcontcat", selected = "")
  updateQQheadreport()
})
observeEvent(input$qqplotsfileuse2, {
  updateSelectInput(session, "qqplotsparmcontcat2", selected = "")
  updateQQheadreport()
})
GLOBAL$startqqBoxplotting <- FALSE
.proceed$qqCont <- FALSE
observeEvent(input$makeqqplotscont, {
  if (all(not.empty(input$qqplotsfileuse))) {
    updateQQheadreport("Preparing qq scatter plots")
    plot.this.model <<- file.path(dirname(GLOBAL$dir.files)[1], input$qqplotsfileuse)
    combineStoreTabs(plot.this.model)
    .proceed$qqCont <- TRUE
    GLOBAL$startqqplotting <- !GLOBAL$startqqplotting # trigger plotting
  }
})
output$qqplotscontinuous <- renderPlot({
  GLOBAL$startqqplotting
  if (!.proceed$qqCont) exit() # only run when generate is clicked
  finalplot <- list()
  ploton <- intersect(names(modelnum.dt.tabs.), plot.this.model)
  if (length(ploton)) {
    updateQQheadreport("Created qq plots...")
    dttab <- modelnum.dt.tabs.[[ploton]]
    updateQQheadreport("Displaying qq plots...")
    basic.eta.GOF(dttab, global.ggplot.options=plot.theme10, type = input$qqplotseltype, standardize = as.boolean(input$qqplotstdplot))
  }
})
