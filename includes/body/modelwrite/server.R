############################################################################
############################################################################
##  Document Path: includes/body/modelwrite/server.R
##
##  Description: Server function for covariate relationship
##
##  R Version: 4.4.1 (2024-06-14 ucrt)
############################################################################
############################################################################


#############################################################################
###  SECTION: Ace Editor
#############################################################################



observeEvent(input$clearcodefile, {
  updateAceEditor(session, "aceeditor", value = "")
})


observe({
  updateAceEditor(
    session,
    "aceeditor",
    theme = input$codingtheme,
    mode = input$codingmode,
    tabSize = input$codingsize
  )
})

observeEvent(input$codingheight, {
  shinyjs::runjs(paste0("document.getElementById('aceeditor').style.height='", input$codingheight, "px'"))
})
observeEvent(input$editcreateend, {
  showModal(modalDialog(
    title = "Confirm Save File",
    paste0("Are you sure you want to save the content to the file? File path: ", GLOBAL$curr.edit.files),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmeditsave", "Confirm")
    )
  ))
  observeEvent(input$confirmeditsave,
    {
      removeModal() # Close the modal
      if (not.empty(GLOBAL$curr.edit.files)) {
        writeLines(input$aceeditor, GLOBAL$curr.edit.files)
        updateEditMsg(paste0("Content written to ", GLOBAL$curr.edit.files))
        # print(input$aceeditor_selection1) # selection
      }
    },
    once = TRUE
  )
})




observeEvent(input$editcreataction2, {
  if (!length(GLOBAL$dir.files)) exit()
  if (not.empty(input$editcreataction2)) {
    GLOBAL$curr.edit.files <- file.path(dirname(GLOBAL$dir.files)[1], input$editcreataction2)
  }
})
observeEvent(input$editcreataction1, {
  if (not.empty(input$editcreataction1)) {
    GLOBAL$curr.edit.files <- file.path(dirname(GLOBAL$dir.files)[1], input$editcreataction1)
  }
})
observe({
  if (input$editcreataction == "1" & file.exists(GLOBAL$curr.edit.files)) {
    # if editing a valid file
    updateAceEditor(session, "aceeditor", value = paste(readLines(GLOBAL$curr.edit.files), collapse = "\n"))
  }
  if (input$editcreataction == "2") {
    # if creating a new file
    updateAceEditor(session, "aceeditor", value = initnewcode)
  }
})




# # set data versions to use for plotting
# updateSelectInput(session, "datatoUseconcv1", choices = data.versions.names)
# updateSelectInput(session, "datatoUseconcv2", choices = data.versions.names)
#
#
#
# output$corrcovplots1 <- renderPlot({
#   covrel.data <- GLOBAL$data.versions[[input$datatoUseconcv1]]
#   if (is.null(covrel.data)) {
#     return("Awaiting data and covariate selection")
#   }
#
#   if (nrow(covrel.data) & all(input$corrcectouse %in% names(covrel.data))) {
#     varsr <- input$corrcectouse
#     covrel.data2 <- covrel.data %>%
#       filter(!duplicated(ID)) %>%
#       select(all_of(varsr))
#
#     ggpairs(covrel.data2) +
#       theme_bw() +
#       styler00 + styler01 +
#       styler03
#   }
# })
#
#
#
# output$corrcovplots2 <- renderPlot({
#   covrel.data <- GLOBAL$data.versions[[input$datatoUseconcv2]]
#   if (is.null(covrel.data)) {
#     return("Awaiting data and covariate selection")
#   }
#
#   if (nrow(covrel.data) & all(input$corrcectouse2 %in% names(covrel.data))) {
#     varsr <- input$corrcectouse2
#     covrel.data2 <- covrel.data %>%
#       filter(!duplicated(ID)) # %>%select(all_of(varsr))
#
#     pl1 <- ggplot(data = covrel.data2)
#     allplt <- lapply(varsr, function(o) {
#       pl1 + aes_string(x = o, y = input$corrcectoby2) +
#         geom_boxplot(outlier.shape = NA) +
#         geom_jitter(width = 0.2, alpha = 0.1) +
#         labs(y = input$labelycovcorr) +
#         theme_bw() +
#         styler00 + styler01 +
#         styler03 + theme(
#           legend.position = input$legendpositioncovcorr,
#           axis.text = element_text(size = 14)
#         )
#     })
#
#     patchwork::wrap_plots(allplt, ncol = input$ncollegendcovcorr)
#   }
# })
