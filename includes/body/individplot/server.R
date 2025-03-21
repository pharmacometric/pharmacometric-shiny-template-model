#########################################################################################
#########################################################################################
##
##  Document Path: includes/body/individplot/server.R
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

GLOBAL$startdvpredipredplotting <- FALSE
plot.this.model <- plot.this.model2 <- c()
observeEvent(input$dvpredipredplotsfileuse, {
  updateSelectInput(session, "dvpredipredplotsparmcontcat", selected = "")
  updateDVPIPheadreport()
})
observeEvent(input$dvpredipredplotsfileuse2, {
  updateSelectInput(session, "dvpredipredplotsparmcontcat2", selected = "")
  updateDVPIPheadreport()
})
GLOBAL$startdvpredipredBoxplotting <- FALSE
.proceed$dvpredipredCont <- FALSE
observeEvent(input$makedvpredipredplotscont, {
  if (all(not.empty(input$dvpredipredplotsfileuse))) {
    updateDVPIPheadreport("Preparing dvpredipred scatter plots")
    plot.this.model <<- file.path(dirname(GLOBAL$dir.files)[1], input$dvpredipredplotsfileuse)
    combineStoreTabs(plot.this.model)
    .proceed$dvpredipredCont <- TRUE
    GLOBAL$startdvpredipredplotting <- !GLOBAL$startdvpredipredplotting # trigger plotting
  }
})


output$dvpredipredplotscontinuous <- renderPlot({
  GLOBAL$startdvpredipredplotting
  if(as.numeric(input$dvpredipredplotperpage) < 1) exit()
  if (!.proceed$dvpredipredCont) exit() # only run when generate is clicked
  finalplot <- list()
  ploton <- intersect(names(modelnum.dt.tabs.), plot.this.model)
  if (length(ploton)) {
    showloader("dvpredipredplotscontinuous")
    updateDVPIPheadreport("Created dvpredipred plots...")
    dttab <- modelnum.dt.tabs.[[ploton]]
    updateDVPIPheadreport("Displaying dvpredipred plots...")
    etalbs = strsplit(input$dvpredipredlabels,",")[[1]]
    idvar = grep("^ID$",names(dttab), value = T, ignore.case = T)
    if(length(idvar) & as.numeric(input$dvpredipredplotspage)>0){
      dttab$..isd..rx= dttab[[idvar[1]]]
      end0 = as.numeric(input$dvpredipredplotperpage)*as.numeric(input$dvpredipredplotspage)
      stt0 = end0 - as.numeric(input$dvpredipredplotperpage)+1
     dttab = dttab[dttab$..isd..rx %in% unique(dttab$..isd..rx)[c(stt0:end0)],]
     if(!nrow(dttab)){
       updateDVPIPheadreport(paste0("Page ",input$dvpredipredplotspage," does not exist."))
       exit()
     }
    }
    do.individual.GOF(dttab,
                      x=input$dvpredipredtime,
                      y=etalbs,
                      global.ggplot.options =plot.theme10,
                      per.page = input$dvpredipredplotperpage,
                      ylab = input$dvpredipredylab)
  }
})
