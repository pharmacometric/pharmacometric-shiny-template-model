############################################################################
############################################################################
##  Document Path: includes/body/paths/server.R
##
##  Author: W.H
##
##  R Version: R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################
# observe changes in file path type, uploads or local path
observeEvent(input$checkGroupDatasetT, {
  refreshAllModelHolders()
  if(input$checkGroupDatasetT == "2")
    shinyjs::runjs("$('#dirfiletype1a').prop('disabled',true)")
  else shinyjs::runjs("$('#dirfiletype1a').prop('disabled',false)")
}, ignoreInit = TRUE)

GLOBAL$selectedfilesInput <- "dirfiletype1a"

# react to changes in local directory path for original
observeEvent(input$dirfiletype1a, {
  refreshAllModelHolders()
  fillisttxt <- c()
  if (!dir.exists(input$dirfiletype1a)) {
    updateDirStatus("Directory does not exist for Original")
    shinyjs::runjs(paste0("$('#dirfiletype1afiles').html(\'\')"))
    return("")
  }

  files3 <- sort_by_name_and_ext(list.files(path = input$dirfiletype1a, full.names = TRUE))


  treeStructure <- listFiles(3,input$dirfiletype1a)
  output$s2tree <- renderTree(treeStructure)


  GLOBAL$dir.files <- files3
#
#   for (kfil in files3) {
#     kfil = basename(kfil)
#     fileicon <- ifelse(is.image(kfil),
#       switchicons("png"),
#       switchicons(file_ext(kfil))
#     )
#     fillisttxt <- c(fillisttxt, paste0(
#       '<div class="filelistc1"><i class="fas fa-', fileicon %or% "file", '" role="presentation" aria-label="chart-area icon"></i> ',
#       kfil, "</div>"
#     ))
#   }
#   shinyjs::runjs(paste0("$('#dirfiletype1afiles').html(\'", paste(fillisttxt, collapse = ""), "\')"))
}, ignoreInit = TRUE)


# react to changes in uploaded directory path for original
GLOBAL$storageDir = tempdir()
observeEvent(input$ufileupd1a, {
  originalfilename <- input$ufileupd1a["name"]
  newFilename <- input$ufileupd1a$datapath
  extension <- tools::file_ext(originalfilename)
  acceptedextensions <- c("zip", "gz", "tgz")
  file.orginalfolder <- file.path(GLOBAL$storageDir, randString(1,10))
  if (extension %nin% acceptedextensions) {
    updateDirStatus("The file selected should be a .zip or .tgz or .tar.gz")
  } else {
    for (l in list.files(GLOBAL$storageDir, recursive = 1L, full.names = 1L)){
      if (!grepl("\\.(zip|tar|tgz)$", l, ignore.case = TRUE)) unlink(l)
    }
    if (!dir.exists(file.orginalfolder)) dir.create(file.orginalfolder)
    if (extension == acceptedextensions[1]) {
      unzip(newFilename, exdir = file.orginalfolder)
    } else {
      untar(newFilename, exdir = file.orginalfolder)
    }

    updateTextInput(inputId = "dirfiletype1a", value = file.orginalfolder)
    fillisttxt <- c()


    GLOBAL$dir.files <- list.files(file.orginalfolder, recursive = TRUE)

    # for (kfil in GLOBAL$dir.files) {
    #   kfil = basename(kfil)
    #   fileicon <- ifelse(is.image(kfil),
    #                      switchicons("png"),
    #                      switchicons(file_ext(kfil))
    #   )
    #   fillisttxt <- c(fillisttxt, paste0(
    #     '<div class="filelistc1"><i class="fas fa-', fileicon %or% "file", '" role="presentation" aria-label="chart-area icon"></i> ',
    #     kfil, "</div>"
    #   ))
    # }
    # shinyjs::runjs(paste0("$('#dirfiletype1afiles').html(\'", paste(fillisttxt, collapse = ""), "\')"))
  }
})






