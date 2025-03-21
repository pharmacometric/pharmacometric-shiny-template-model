############################################################################
############################################################################
##  Document Path: includes/body/server.R
##
##  Description: Server function for body
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################





observe({
  if(is.null(GLOBAL$dir.files)) exit()
  if(length(GLOBAL$dir.files)){
    extrmodfiles = basename(GLOBAL$dir.files)
    extrmodfiles = extrmodfiles[grep("\\.(mod|ctl)$",extrmodfiles)]
    updateSelectInput(session, "etaplotsfileuse", choices = c("Select file" = "", extrmodfiles))
    updateSelectInput(session, "etaplotsfileuse2", choices = c("Select file" = "", extrmodfiles))
    updateSelectInput(session, "editcreataction1", choices = c("Select file" = "", extrmodfiles))
    updateSelectInput(session, "qqplotsfileuse", choices = c("Select file" = "", extrmodfiles))
    updateSelectInput(session, "etapairplotsfileuse", choices = c("Select file" = "", extrmodfiles))
    updateSelectInput(session, "dvpredipredplotsfileuse", choices = c("Select file" = "", extrmodfiles))
  }
})



