############################################################################
############################################################################
##  Document Path: includes/body/etaplot/server.R
##
##
##  Date: 2025-03-05
##
##  Description: Server for plotting etaplots
##
##  R Version: 4.2.0 (2022-04-22)
##
#############################################################################
#############################################################################
GLOBAL$startETAplotting <- FALSE
plot.this.model <- plot.this.model2 <- c()
observeEvent(input$etaplotsfileuse, {
  updateSelectInput(session, "etaplotsparmcontcat", selected = "")
  updateETAheadreport()
})
observeEvent(input$etaplotsfileuse2, {
  updateSelectInput(session, "etaplotsparmcontcat2", selected = "")
  updateETAheadreport()
})

GLOBAL$startETABoxplotting <- FALSE
.proceed$EtaCont <- FALSE
observeEvent(input$makeetaplotscont, {
  if (all(not.empty(input$etaplotsfileuse)) & all(not.empty(input$etaplotsparmcontcat))) {
    updateETAheadreport("Preparing ETA scatter plots")
    plot.this.model <<- file.path(dirname(GLOBAL$dir.files)[1], input$etaplotsfileuse)
    combineStoreTabs(plot.this.model)
    .proceed$EtaCont <- TRUE
    GLOBAL$startETAplotting <- !GLOBAL$startETAplotting # trigger plotting
  }
})
output$etaplotscontinuous <- renderPlot({
  GLOBAL$startETAplotting
  if (!.proceed$EtaCont) exit() # only run when generate is clicked
  finalplot <- list()
  for (i in intersect(names(modelnum.dt.tabs.), plot.this.model)) {
    showloader("etaplotscontinuous")
    dttab <- modelnum.dt.tabs.[[i]]
    etavars <- grep("^ETA", names(dttab), value = TRUE)
    if (length(etavars)) {
      for (etai in etavars) {
        updateETAheadreport(paste0("Preparing ", etai, " plots..."))
        covariate.def <- covariates_titles[input$etaplotsparmcontcat]
        finalplot[[paste0(i, etai)]] <- createETAcontcov(dttab, eta = etai, ylab = paste0(etai, " ", input$etaplotysuffix), covs = covariate.def, model = basename(i))
      }
    }
  }
  updateETAheadreport("Created ETA plots...")
  if (length(finalplot)) {
    .proceed$EtaCont <- FALSE
    updateETAheadreport("Displaying ETA plots...")
    patchwork::wrap_plots(unlist(finalplot, recursive = FALSE), ncol = input$etaplotsetplotcol)
  }
})

observeEvent(input$makeetaplotscont2, {
  if (all(not.empty(input$etaplotsfileuse2)) & all(not.empty(input$etaplotsparmcontcat2))) {
    updateETAheadreport("Preparing ETA box plots")
    plot.this.model2 <<- file.path(dirname(GLOBAL$dir.files)[1], input$etaplotsfileuse2)
    combineStoreTabs(plot.this.model2)
    .proceed$EtaCont <- TRUE
    GLOBAL$startETABoxplotting <- !GLOBAL$startETABoxplotting # trigger plotting
  }
})
output$etaplotscategorical <- renderPlot({
  GLOBAL$startETABoxplotting
  if (!.proceed$EtaCont) exit()
  finalplot <- list()
  for (i in intersect(names(modelnum.dt.tabs.), plot.this.model2)) {
    showloader("etaplotscategorical")
    dttab <- modelnum.dt.tabs.[[i]]
    etavars <- grep("^ETA", names(dttab), value = TRUE)
    if (length(etavars)) {
      for (etai in etavars) {
        updateETAheadreport(paste0("Preparing ", etai, " box plots..."))
        covariate.def <- covariates_titles[input$etaplotsparmcontcat2]
        finalplot[[paste0(i, etai)]] <- createETAcatcov(dttab, eta = etai, ylab = paste0(etai, " ", input$etaplotysuffix), covs = covariate.def, model = basename(i))
      }
    }
  }
  if (length(finalplot)) {
    .proceed$EtaCont <- FALSE
    updateETAheadreport("Displaying ETA box plots...")
    patchwork::wrap_plots(unlist(finalplot, recursive = FALSE), ncol = input$etaplotsetplotcol)
  }
})
