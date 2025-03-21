observeEvent(input$aboutproject, {
  showModal(modalDialog(
    title = "About this app",
    "This data exploration app was created by Will Hane 2025. Use freely."
  ))
})


# observeEvent(input$moduleproj, {
#   showModal(modalDialog(
#     title = "Module Selector",
#     "Select Module to Display",
#     radioButtons("modelselectorchoice","Choose module to work with", choices = c(
#       "Exploratory data analysis" = 1,
#       "Modeling and post processing" = 2,
#       "Quality control" = 3,
#       "Report preparation" = 4,
#       "AI assisted analysis" = 5
#     ), selected = 2),
#     footer = tagList(
#       modalButton("Cancel"),
#       actionButton("proceedmodule", "Go to module")
#     )
#   ))
#
#
# })
#
#
# observeEvent(input$proceedmodule,{
#   removeModal()
#   print("Going to module")
#   shinyjs::runjs(paste0("window.location.href = '?module=",input$modelselectorchoice,"'"))
# })



