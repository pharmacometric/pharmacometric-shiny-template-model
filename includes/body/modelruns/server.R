############################################################################
############################################################################
##  Document Path: includes/body/modelruns/server.R
##
##  Description: Server function for model run list
##
##  R Version: 4.4.1 (2024-06-14 ucrt)
############################################################################
############################################################################

refreshAllModelHolders() # GLOBAL model holders
modelnum.compare.dt <- data.frame()
set.vals.proc <- function(key, nmfile,lstf) {
  modelnum.vals.[[key]] <<- c(key, nmfile,lstf)
  modelnum.proc.[[key]] <<- suppressWarnings(readLines(nmfile))
  modelnum.vals(number(1))
  modelnum.proc(number(1))
}
set.est.ofv <- function(key, value, nmfile, readlst) {
  modelnum.est.[[key]] <<- list(readlst$TAB, value)
  modelnum.ofv.[[key]] <<- readlst$SUMMARY["OFV"]
  modelnum.tabs.[[nmfile]] <<- readlst$TABLEOUTPUT
  modelnum.est(number(1))
  modelnum.ofv(number(1))
}
model.run.list <- function(key, value) {
  modelnum.run.[[key]] <<- c(value, as.numeric(Sys.time()), key, 0)
  GLOBAL$modelnum.run[[key]] <- TRUE
}

output$modeltabsrun <- renderUI({
  showloader("modeltabsrun")
  GLOBAL$refreshnmlist
  if (is.null(GLOBAL$selectedfilesInput)) exit()
  folderpath <- input[[GLOBAL$selectedfilesInput]]
  if (is.empty(GLOBAL$selectedfilesInput)) exit(paste0("Please enter a valid directory to search for models"))
  if (!dir.exists(folderpath)) exit(paste0("The selected directory does not exist: ", folderpath))
  # uniqueid = hash_string_to_numbers(folderpath)
  uniqueid <- round(runif(1, max = 100000))
  modfiles <- sort(list.files(path = folderpath, pattern = "\\.(mod|ctl)$", full.names = FALSE), decreasing = as.boolean(input$modellistsort, 2))
  if (!length(modfiles)){
    updateModListheadreport("No models found")
    exit(paste0("No NONMEM models in the selected directory: ", folderpath))
    }else{updateModListheadreport()}
  contenwrap <- c()
  for (sw in indexed(modfiles)) {
    sw$key <- sw$key + uniqueid
    descr <- problem <- "None"
    inputdata <- nsig <- ofv <- nmethod <- "NA"
    lstcheck <- converge <- covariance <- minimize <- gradient <- tickfailed
    nmfile <- file.path(folderpath, sw$value)
    listfile <- file.path(folderpath, gsub("\\.(mod|ctl)$", ".lst", sw$value))
    set.vals.proc(sw$key, nmfile,listfile)
    if (file.exists(listfile)) {
      readlst <- readLines(listfile) #parse lst files
      set.est.ofv(sw$key, sw$value, nmfile, readlst)
      ofv <- readlst$SUMMARY["OFV"]
      inputdata <- readlst$INPUT
      nmethod <- getNMmethod(readlst$CODE)
      lstcheck <- ticksuccess
      descr <- readlst$SUMMARY["PROBLEM"]
      nsig <- readlst$SUMMARY["NSIG"]
      if (as.boolean(readlst$SUMMARY["CovarianceStep"])) covariance <- ticksuccess
      if (readlst$SUMMARY["Success"] == "MINIMIZATION SUCCESSFUL") minimize <- ticksuccess
      if (as.boolean(readlst$SUMMARY["zerogradient"])) gradient <- ticksuccess
    }
    contenwrap <- paste0(
      contenwrap,
      tags$tr(
        tags$td(if (file.exists(listfile)) {
          tags$input(type = "checkbox", model = sw$key, class = "modelruncompare", id = paste0("selectmodel", sw$key))
        }),
        tags$td(sw$value, class = "text-left"),
        tags$td(lstcheck),
        tags$td(minimize),
        tags$td(covariance),
        tags$td(gradient),
        tags$td(ofv),
        tags$td(nsig),
        tags$td(nmethod),
        tags$td(descr, class = "text-left"),
        tags$td(inputdata, class = "text-left"),
        tags$td(
          actionButton(paste0("modecode", sw$key), "", icon = icon("code"), class = "rerunmode btn-primary py-1 px-2", style = "font-size:8px"),
          if (file.exists(listfile)) {
            tags$span(actionButton(paste0("modeparms", sw$key), "", icon = icon("list"), class = "rerunmode btn-primary py-1 px-2", style = "font-size:8px"),
            actionButton(paste0("lstf", sw$key), "", icon = icon("info"), class = "rerunmode btn-warning py-1 px-2", style = "font-size:8px"))
          },
          actionButton(paste0("rerunmod", sw$key), "", icon = icon("play"), class = "rerunmode btn-info py-1 px-2", style = "font-size:8px"),
          actionButton(paste0("chatnote", sw$key), "", icon = icon("comment"), class = "rerunmode btn-danger py-1 px-2", style = "font-size:8px")
        )
      )
    )
  }
  contenhead <- tags$tr(
    tags$th(""),
    tags$th("Models ", icon("sort"), class = "text-left"),
    tags$th("Completed ", icon("sort")),
    tags$th("Minimization", icon("sort")),
    tags$th("Covariance", icon("sort")),
    tags$th("ZeroGradient", icon("sort")),
    tags$th("OFV", icon("sort")),
    tags$th("NSIG", icon("sort")),
    tags$th("Method", class = "text-left"),
    tags$th("Description", class = "text-left"),
    tags$th("Dataset", class = "text-left"),
    tags$th("Options")
  )
  HTML(paste0('
       <table id="modelrunmain">
    <thead>', contenhead, "</thead>
    <tbody>", contenwrap, "</tbody>
</table>
       "))
})
observe({
  # if(not.null(input$activatemodelbuttons))
  #   if(input$activatemodelbuttons == "activate model buttons"){
  #     print("programming the juice...")
  if (!length(modelnum.vals())) exit()
  for (listitem in Filter(Negate(is.null), modelnum.vals.)) {
    if (is.null(modelnum.proc.[[listitem[1]]])) {
      modelnum.proc.[[listitem[1]]] <- TRUE
      eval(parse(text = paste0(
        'observeEvent(input[[paste0("modecode",', listitem[1], ')]],{
          showModal(modalDialog(
            title = paste0("Code for ","', basename(listitem[2]), '"),
            size="l", easyClose = TRUE,
            div(tags$code(paste(modelnum.proc.[[', listitem[1], ']], collapse="\n")))
          ))
        })
        observeEvent(input[[paste0("lstf",', listitem[1], ')]],{
          showModal(modalDialog(
            title = paste0("Summary (lst) file for ","', basename(listitem[2]), '"),
            size="l", easyClose = TRUE,
            div(tags$code(paste(readLines("', listitem[3], '"), collapse="\n")))
          ))
        })
        observeEvent(input[[paste0("chatnote",', listitem[1], ')]],{
          showModal(modalDialog(
            title = paste0("View/edit model comments for ","', basename(listitem[2]), '"),
            size="l", easyClose = FALSE,
            tags$textarea(id = "noteformodel', basename(listitem[2]), '","Sample note for the model...")
          ))
        })
        observeEvent(input[[paste0("modeparms",', listitem[1], ')]],{
          showModal(modalDialog(
            title = paste0("Parameter estimates for ","', basename(listitem[2]), '"),
            size="l", easyClose = FALSE,
            renderTable(modelnum.est.[[', listitem[1], ']][[1]])
          ))
        })
        observeEvent(input[[paste0("rerunmod",', listitem[1], ')]],{
          showModal(modalDialog(
    title = "Confirm Start Run",
    "Do you really want to start the NM run for model ', basename(listitem[2]), '?",
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm', listitem[1], '", "Confirm")
    )
  ))
          observeEvent(input[["confirm', listitem[1], '"]], {
  removeModal()  # Close the modal
  exec_run("', listitem[1], '", "', listitem[2], '")
},once=TRUE)
          })'
      )))
    } else {
      # print("is Programmed")
    }
  }
})
exec_run <- function(id, path) {
  cdpath <- dirname(path)
  nmfile <- basename(path)
  # print("--------")
  # print(path)
  lstfile <- gsub("\\.(mod|ctl)$", ".lst", nmfile)
  # rm -rf modelfit_*;
  system(paste0("cd ",cdpath,";rm -rf f-",lstfile,".*; psn74 execute ",nmfile," -nm_output=xml,cov,ext && psn74 sumo ",lstfile," & disown"), intern = FALSE)
  model.run.list(id, path)
}

jsselectedmodels = FALSE
observeEvent(input$comparemodelsest, {
  updateModListheadreport("<i class=\"fa-solid fa-triangle-exclamation\"></i> You have to select atleast two models to compare")
  session$sendCustomMessage("activatejsselectedmodels", as.numeric(Sys.time()))# send info for tracking input
  jsselectedmodels <<- TRUE
  # js will return input$jsselectedmodels with all selected models

})

observe({
  #execute model compare
  if (not.null(input$jsselectedmodels) & jsselectedmodels) {
    if(length(input$jsselectedmodels) <2)exit()
    updateModListheadreport("Creating comparison...")
    jsselectedmodels <<- FALSE # shut off recursive loop
    modelids <- input$jsselectedmodels
    modnmaes <- c()
    full.data. <- data.frame(X = 1)
    for (uu in modelids) {
      dt.1 <- modelnum.est.[[as.numeric(uu)]]
      modnmaes <- c(modnmaes,dt.1[[2]])
      # print(dt.1)
      dt.1.0 <- dt.1[[1]][, c("NAME", "TYPE")]
      dt.1.0[, paste0(basename(dt.1[[2]]))] <- dt.1[[1]]$EST
      # dt.1.0[,paste0("SE",uu)] = dt.1$SE
      dt.1.1 <- dt.1.0[1, ] %>% mutate(NAME = "OFV", TYPE = "")
      dt.1.1[, paste0(basename(dt.1[[2]]))] <- modelnum.ofv.[[as.numeric(uu)]]
      # dt.1.1[,paste0("SE",uu)] = ""
      dt.1 <- rbind(dt.1.1, dt.1.0)
      if (nrow(full.data.) == 1) full.data. <- dt.1 else full.data. <- full_join(full.data., dt.1)
    }
    modelnum.compare.dt <<- full.data.
    showModal(modalDialog(
      title = list(paste0("Comparison of models ", paste(modnmaes, collapse = ", ")), tags$div(class = "", selectInput("referencemodd", "Reference model", choices = modnmaes))),
      size = "l", easyClose = FALSE,
      tableOutput("modelcomparetableview"),
      updateModListheadreport('Model compare ready')
    ))


    }
})

modelcomparetable <- reactive({
  ref =input$referencemodd
  if(ref %nin% names(modelnum.compare.dt))exit("Reference model was not found in the list of selected models. Close and reopen modal.")
  othercol = setdiff(names(modelnum.compare.dt)[-c(1:2)],ref)
  modelnum.compare.dt  %>% mutate_at(othercol,{\(x,refm=.[[ref]]) #%>% arrange(desc(NAME))
    ifelse((refm == x) | is.na(refm) | is.na(x),
           paste0("<span class='modestneutral'>",x,"&nbsp;&nbsp;&nbsp;</span>"),
           ifelse(x > refm,
                  paste0("<span class='modestup'>",x,"&nbsp;&nbsp;▲</span>"),
                  paste0("<span class='modestdown'>",x,"&nbsp;&nbsp;▼</span>")
                  )
           )
  })%>% mutate(NAME=paste0("<b>",NAME,"</b>")) %>%
    rename(Parameter = NAME, Description = TYPE)
})
output$modelcomparetableview <- renderTable(modelcomparetable(), striped = FALSE, hover = TRUE, bordered = TRUE, align = "l", sanitize.text.function = identity)

# observeEvent(input$referencemodd,{
#   ref =input$referencemodd
#   if(ref %nin% names(modelnum.compare.dt))exit("Reference model was not found in the list of selected models. Close and reopen modal.")
#   othercol = setdiff(names(modelnum.compare.dt)[-c(1:2)],ref)
#   modelnum.compare.dt[[".ref."]] = modelnum.compare.dt[[ref]]
#   modelnum.compare.dt = modelnum.compare.dt %>% mutate_at(othercol,{\(x,refm=.$.ref.)
#     ifelse((refm == x) | is.na(refm) | is.na(x),
#            paste0("<span class='modestneutral'>",x,"&nbsp;&nbsp;&nbsp;</span>"),
#            ifelse(x > refm,
#                   paste0("<span class='modestup'>",x,"&nbsp;&nbsp;▲</span>"),
#                   paste0("<span class='modestdown'>",x,"&nbsp;&nbsp;▼</span>")
#            )
#     )
#   }) %>% select(-.ref.)
#
#
#   # modelnum.compare.dt = modelnum.compare.dt%>% rowwise() %>%
#   #   mutate(run105.mod = ifelse((run322.mod > run105.mod)%or%FALSE,paste0("<span class='modestdown'>",run105.mod,"</span>"),paste0("<span class='modestup'>",run105.mod,"</span>")))
#   output$modelcomparetableview <- renderTable(modelnum.compare.dt, striped = FALSE, hover = TRUE, bordered = TRUE, align = "l", sanitize.text.function = identity)
# })









# manage runs
# psn74 execute run322.mod -nm_output=xml,cov,ext && psn74 sumo run322.lst
# when its done, remove directory "modelfit_*"
# get_current_directory <- function() {
#   result <- system("cd www/runtest; ls f-*.*.*", intern = TRUE)
#   return(result)
# }
#
# system("cd www/runtest; psn74 execute run322.mod -nm_output=xml,cov,ext && psn74 sumo run322.lst & disown", intern = FALSE)
#
# # Example of using the function
# current_directory <- get_current_directory()
# print(current_directory)
# monitor runs

GLOBAL$refresh <- FALSE
GLOBAL$refreshnmlist <- FALSE
observeEvent(input$refreshruns, {
  GLOBAL$refresh <- !GLOBAL$refresh
  print("Refreshing runs...")
})
observeEvent(input$refreshmodest, {
  pathvalue = input$dirfiletype1a
  updateTextInput(inputId = "dirfiletype1a", value = "")
  updateTextInput(inputId = "dirfiletype1a", value = pathvalue)
  print("Refreshing runs...")
})
observeEvent(input$clearcptruns0, {
  for (dr in modelnum.run.) {
    if (dr[4] == "1") modelnum.run.[[dr[3]]] <<- NULL
    # GLOBAL$modelnum.run <- NULL
  }
  GLOBAL$refresh <- !GLOBAL$refresh
})
observeEvent(input$clearcptruns, {
  showModal(modalDialog(
    title = "Confirm Clear Runs",
    "Do you really want to clear the run list table? Keep in my that this will not end the runs in the linux terminal.",
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirmclearrunlist", "Confirm")
    )
  ))
  observeEvent(input$confirmclearrunlist,
    {
      removeModal() # Close the modal
      modelnum.run. <<- NULL
      GLOBAL$modelnum.run <- NULL
    },
    once = TRUE
  )
})
output$modelrunning <- renderUI({
  GLOBAL$refresh
  # trackfile <- system("cd www/runtest;ls -t f-run322.mod.* | head -n 1", intern = TRUE)
  if (is.null(GLOBAL$modelnum.run)) exit()
  if (!length(GLOBAL$modelnum.run)) exit()
  if (is.null(GLOBAL$selectedfilesInput)) exit()
  if (is.empty(GLOBAL$selectedfilesInput)) exit()
  if (!dir.exists(input[[GLOBAL$selectedfilesInput]])) exit()
  folderpath <- input[[GLOBAL$selectedfilesInput]]
  modfiles <- (list.files(path = folderpath, pattern = "\\.(mod|ctl)$", full.names = FALSE))
  contenwrap <- c()
  for (sc in modelnum.run.) {
    animexec <- tags$img(src = "running.gif", height = 20)
    executedone <- span(class = "label label-danger", "Incomplete")
    lstcheck <- tags$i(class = "fa fa-times-circle text-danger")
    lstcomment <- "None"
    modf <- basename(sc[1])
    listfile <- gsub("\\.(mod|ctl)$", ".lst", sc[1])
    if (file.exists(listfile)) {
      lstcheck <- tags$i(class = "fa fa-check-circle text-success")
    }
    trackfile <- system(paste0("ls -t ", dirname(sc[1]), "/f-", modf, ".* | head -n 1"), intern = TRUE)
    if (length(trackfile)) {
      if (file.exists(trackfile) &
        tools::file_ext(trackfile) != "") {
        if ((as.numeric(file.info(trackfile)["ctime"]) - as.numeric(sc[2])) > 0) { # check if the file was created after the run was started
          print("-------*****---------------")
          readtckf <- readLines(trackfile)
          jobnumb <- grep("^Job [0-9].+$", readtckf, value = TRUE)
          jobnumb2 <- grep(".+job [0-9].+$", readtckf, value = TRUE)
          print(jobnumb2)
          if (length(jobnumb)) {
            lstcomment <- jobnumb[1]
          }
          if (readtckf[length(readtckf)] == "execute done") {
            message(modelnum.run.[[sc[3]]][4])
            modelnum.run.[[sc[3]]][4] <<- 1
            executedone <- span(class = "label label-success", "Completed")
            animexec <- tags$img(src = "complete.gif", height = 20)
          } else {
            if (length(jobnumb2)) lstcomment <- jobnumb2[1]
          }
        } else {
          lstcomment <- "ERROR: Your run was not started. Check your NONMEM installation."
          executedone <- span(class = "label label-warning", "Not started")
        }
      }
    }
    contenwrap <- paste0(
      contenwrap,
      tags$tr(
        tags$td(modf, class = "text-left"),
        tags$td(animexec),
        tags$td(lstcheck),
        tags$td(executedone),
        tags$td(lstcomment)
      )
    )
  }
  contenhead <- tags$tr(
    tags$th("Model", class = "text-left"),
    tags$th("Status", width = 30),
    tags$th("lst file"),
    tags$th("Status Code"),
    tags$th("Comments", width = "50%")
  )
  HTML(paste0('
       <table id="modelrunmain">
    <thead>', contenhead, "</thead>
    <tbody>", contenwrap, "</tbody>
</table>
       "))
})



output$parmmodelsest1 <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep = "")
  },
  content = function(con) {
    write.csv(data, con)
  }
)


