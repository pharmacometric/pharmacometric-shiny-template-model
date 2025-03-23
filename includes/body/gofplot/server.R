#########################################################################################
#########################################################################################
##
##  Document Path: includes/body/gofplot/server.R
##
##  R Version: R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################


GLOBAL$startplotting <- FALSE
GLOBAL$table <- c()
GLOBAL$plots <- c()
GLOBAL$selruns <- c()
GLOBAL$modfile <- c()
plot.this.model <- c()


observeEvent(input$refreshgofplots,{
  updateSelectInput(session, "modgofftype", selected = "")
  updateGOFheadreport()
  #modelnum.dt.tabs. <<- list()
  #GLOBAL$startplotting = !GLOBAL$startplotting
  })

output$dislayui <- renderTable({
  "hello"
  # system("qstat")
  # GLOBAL$table
})

commentafterplot = reactiveVal(TRUE)

output$plotgofsui <- renderPlot({
  GLOBAL$startplotting

  DV.var = input$gofdvvar#DV variable#DV"
  TM.var = input$goftimevar#Time variable#TIME"
  PRED.var = input$gofPREDvar#PRED variable#PRED"
  IPRED.var = input$gofIPREDvar#IPRED variable#IPRED"
  CWRES.var = input$gofcwresvar#CWRES variable#CWRES"

  if(not.empty(input$modgofftype) & all(not.empty(input$modelsgofshow))){
    showloader("plotgofsui")
    updateGOFheadreport("Plotting GOFs...")
      finalplot <- NULL
      for (i in intersect(names(modelnum.dt.tabs.),plot.this.model)) {
        runno <- i
        #xxxxxx<<-modelnum.dt.tabs.
        if(is.null(GLOBAL$plots[[runno]])){
          updateGOFheadreport(paste0("Generating plots for ", basename(i),"..."))
          GLOBAL$plots[[runno]] <- gof.plots.a(modelnum.dt.tabs.[[i]], exp.filter = trimws(input$filterpre), log = input$shouldLog,concuni=input$gofconcuni,tmuni=input$goftimeuni)
        }
        updateGOFheadreport(paste0("Adding plots for ", basename(i),"..."))

        # this plot
        thisplot <- GLOBAL$plots[[runno]][["linear"]][[input$modgofftype]]+ ggtitle(basename(i))

        #if(input$setSubby != "None") thisplot <- thisplot + aes_string(color = input$setSubby) + labs(color=input$setSubby)

        if(is.null(finalplot)) finalplot <- thisplot
        else finalplot <- finalplot + thisplot
      }

      updateGOFheadreport("Displaying plot...")
      finalplot + plot_layout(ncol = input$setcols, nrow = input$setrows)

  }
})



observe({
  if(is.null(GLOBAL$dir.files)) exit()
  extrmodfiles = basename(GLOBAL$dir.files)
  extrmodfiles = extrmodfiles[grep("\\.(mod|ctl)$",extrmodfiles)]
  updateSelectInput(session, "modelsgofshow", choices = extrmodfiles)
  if(length(extrmodfiles))updateGOFheadreport() else updateGOFheadreport("No model files found")
})


observeEvent(input$modelsgofshow,{
  # print(input$modelsgofshow)
  # print(length(input$modelsgofshow))
  updateSelectInput(session, "modgofftype", selected = "")
})

observeEvent(input$modgofftype,{
  if(not.empty(input$modgofftype) & all(not.empty(input$modelsgofshow))){
    plot.this.model <<- file.path(dirname(GLOBAL$dir.files)[1],input$modelsgofshow)
    modeltype = input$modgofftype
    combineStoreTabs(plot.this.model)
    GLOBAL$startplotting = !GLOBAL$startplotting
  }

})



# combtab = combtab %>% left_join(combtabnew[,c("ID",setdiff(names(combtabnew),names(combtab)))])

# model.path = "www/example/"
# runid = "101"
# sddat <- read.table(paste0(model.path, "sdtab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
# codat <- read.table(paste0(model.path, "cotab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
# cadat <- read.table(paste0(model.path, "catab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
# padat <- read.table(paste0(model.path, "patab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)


#############################################################################
###  SECTION: Download plots and code
#############################################################################

output$gofdownloadimgobj <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(data, file)
  }
)

#############################################################################

