#########################################################################################
#########################################################################################
##
##  Document Path: includes/body/pairplot/server.R
##
##
##  R version 4.4.1 (2022-04-22)
##
#########################################################################################
##
##  Program purpose:
##
##
##
#########################################################################################
#########################################################################################

GLOBAL$startetapairplotting <- FALSE
plot.this.model <- plot.this.model2 <- c()
observeEvent(input$etapairplotsfileuse, {
  updateSelectInput(session, "etapairplotsparmcontcat", selected = "")
  updatePAIRheadreport()
})
observeEvent(input$etapairplotsfileuse2, {
  updateSelectInput(session, "etapairplotsparmcontcat2", selected = "")
  updatePAIRheadreport()
})
GLOBAL$startetapairBoxplotting <- FALSE
.proceed$etapairCont <- FALSE
observeEvent(input$makeetapairplotscont, {
  if (all(not.empty(input$etapairplotsfileuse))) {
    updatePAIRheadreport("Preparing etapair scatter plots")
    plot.this.model <<- file.path(dirname(GLOBAL$dir.files)[1], input$etapairplotsfileuse)
    combineStoreTabs(plot.this.model)
    .proceed$etapairCont <- TRUE
    GLOBAL$startetapairplotting <- !GLOBAL$startetapairplotting # trigger plotting
  }
})


output$etapairplotscontinuous <- renderPlot({
  GLOBAL$startetapairplotting
  if (!.proceed$etapairCont) exit() # only run when generate is clicked
  finalplot <- list()
  ploton <- intersect(names(modelnum.dt.tabs.), plot.this.model)
  if (length(ploton)) {
    showloader("etapairplotscontinuous")
    updatePAIRheadreport("Created etapair plots...")
    dttab <- modelnum.dt.tabs.[[ploton]]
    updatePAIRheadreport("Displaying etapair plots...")
    etavars <- grep("^ETA", names(dttab), value = TRUE)
    etalbs = strsplit(input$etapairlabels,",")[[1]]
    if(length(etalbs) == length(etavars))
    set.GOF.params(eta.labels=etalbs)
    eta.pairs.GOF(dttab,density2D=input$etapairdensity2d,standardize = as.boolean(input$etapairplotstdplot,2))
  }
})
