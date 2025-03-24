############################################################################
############################################################################
##  Document Path: utils.R
##
##  Description: global functions and variables used by the app
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################
#quickcode::libraryAll(ggplot2, grid, ggthemes)
library(ggplot2)
library(grid)
library(ggthemes)
ggplot2::theme_set(theme_bw())
source.part <- function(path, which = c("ui", "server"), input = NULL, output = NULL, session = NULL) {
  which <- match.arg(which)
  for (h in list.files(path = path, pattern = paste0(which, ".R$"), full.names = 1L, recursive = 1L)) {
    this.path <- dirname(h)
    source(h, local = TRUE)
  }
}
listFiles <- function(maxDepth, path, currentDepth = 1) {
  dirs <- list.dirs(path, recursive = FALSE, full.names = FALSE)
  allFiles <- list.files(path, pattern = "\\.(mod|ctl|lst|tab|tbl|csv)$") #, list.dirs(recursive = FALSE, full.names = FALSE)
  files <- setdiff(allFiles, dirs)
  if (length(dirs) != 0 && (maxDepth == 0 || currentDepth < maxDepth)) {
    subtree <- append(lapply(
      dirs,
      function(nextDir) {
        nextDir <- structure(listFiles(maxDepth, file.path(path, nextDir), currentDepth + 1), sttype = "directory")
      }
    ), files)
    names(subtree) <- append(dirs, files)
    subtree
  } else {
    subtree <- append(lapply(
      dirs,
      function(nextDir) {
        structure(nextDir, sttype = "directory")
      }
    ), files)
    names(subtree) <- append(dirs, files)
    subtree
  }
}

exec_run <- function(id, path) {
  cdpath <- dirname(path)
  nmfile <- basename(path)
  lstfile <- gsub("\\.(mod|ctl)$", ".lst", nmfile)
  system(paste0("cd ",cdpath,";rm -rf f-",lstfile,".*; psn74 execute ",nmfile," -nm_output=xml,cov,ext && psn74 sumo ",lstfile," & disown"), intern = FALSE)
  model.run.list(id, path)
}

exec_run_del <- function(id, path) {
  cdpath <- dirname(path)
  nmfile <- basename(path)
  nmofile <- gsub("\\.(mod|ctl)$", "", nmfile)
  system(paste0("cd ",cdpath,";rm -rf modelfit_*;rm -rf *",nmofile,".* &"), intern = FALSE)
}

refreshAllModelHolders <- function() {
  print("Destroying all stores ...")
  ls. <- list()
  modelnum.ofv. <<- ls.
  modelnum.est. <<- ls.
  modelnum.vals. <<- ls.
  modelnum.proc. <<- ls.
  modelnum.run. <<- ls.
  modelnum.tabs. <<- ls.
  modelnum.dt.tabs. <<- ls.
  modelnum.ofv(ls.)
  modelnum.est(ls.)
  modelnum.vals(ls.)
  modelnum.proc(ls.)
  GLOBAL$modelnum.run <- ls.
}
createETAcontcov <- function(covdat,
                             eta,
                             ylab,
                             model,
                             covs) {
  covdat[covdat == "-99"] <- NA # missing
  plot_list <- lapply(1:length(covs), function(n) {
    var1 <- names(covs)[n]
    lab1 <- covs[n]
    covdat %>%
      ggplot(aes_string(x = var1, y = eta)) +
      geom_smooth(
        se = F,
        span = 0.75,
        color = "red"
      ) +
      geom_point(color = "cornflowerblue", alpha = 0.5) +
      scale_x_continuous() +
      labs(y = ylab, x = lab1) +
      theme_bw() +
      plot.theme10 +
      ggtitle(model)
  })
  plot_list
}
createETAcatcov <- function(covdat,
                            eta,
                            ylab,
                            model,
                            covs) {
  covdat[covdat == "-99"] <- NA # missing
  covdat <- covdat %>% mutate_if(is.numeric, as.character)
  covdat$eta00005 <- as.numeric(covdat[, eta])
  plot_list <- lapply(1:length(covs), function(n) {
    var1 <- names(covs)[n]
    lab1 <- covs[n]
    covdat %>%
      ggplot(aes(y = eta00005)) +
      aes_string(x = var1, group = var1) +
      geom_boxplot(color = "cornflowerblue") +
      labs(y = ylab, x = lab1) +
      stat_summary(
        fun.data = {
          \(x)c(y = max(x) * 1.07, label = length(x))
        },
        geom = "text",
        col = "red",
        size = 4
      ) +
      theme_bw() +
      plot.theme10 +
      ggtitle(model)
  })
  plot_list
}
combineStoreTabs <- function(gofmodelslist) {
  for (umod in gofmodelslist) {
    if (is.null(modelnum.dt.tabs.[[umod]])) {
      updateGOFheadreport(paste0("Reading output tables for ", basename(umod)))
      tabs <- file.path(dirname(GLOBAL$dir.files)[1], modelnum.tabs.[[umod]])
      combtab <- data.frame()
      for (umod2 in tabs) {
        combtabnew <- read.table(umod2, header = TRUE, sep = "", na.strings = ".", skip = 1)
        if (!nrow(combtab)) {
          combtab <- combtabnew
        } else {
          colna <- setdiff(names(combtabnew), names(combtab))
          if (length(colna)) {
            combtab <- cbind(combtab, combtabnew[colna])
          }
        }
      }
      modelnum.dt.tabs.[[umod]] <<- combtab
    }
  }
  updateGOFheadreport()
}
exit <- return
tickfailed <- shiny::tags$i(class = "fa fa-times-circle text-danger")
ticksuccess <- shiny::tags$i(class = "fa fa-check-circle text-success")
tickplay <- shiny::tags$i(class = "fa fa-play-circle text-info")
createSampleData <- function(N = 10) {}
togglebuffermsg <- function(id, t) {
  if (t) {
    shinyjs::runjs(paste0("$('#", id, "').html('<p><b style=\"color:red\">Loading other comparison measures and similarity percent..</b></p>')"))
  } else {
    shinyjs::runjs(paste0("$('#", id, "').html('')"))
  }
}
custom_hash <- function(input) {
  # Step 1: Convert input to a character string
  input_string <- as.character(input)
  # Step 2: Convert each character to its ASCII value, sum them, and apply an arbitrary operation
  char_values <- utf8ToInt(input_string)
  hash_value <- sum(char_values * seq_along(char_values)) %% 1000000 # Simple modulo to ensure a fixed size
  # Return the hash value as a string
  hash_value
}
hash_string_to_numbers <- function(input_string) {
  clean_string <- gsub("\\\\|/", "", input_string)
  custom_hash(clean_string)
}
getNMmethod <- function(res) {
  switch(res,
    "1" = "FOCE",
    "2" = "FOCEI",
    "3" = "IPOPT",
    "4" = "LAP",
    "5" = "SAEM",
    "6" = "Two-Stage",
    "7" = "BAYESIAN",
    res
  )
}


getNMmethodFull <- function(vector_items) {
  # Use grep to find indices of items that start with $EST or $ESTIMATION
  matched_indices <- grep("^\\$EST(IMATION)?", vector_items)
  # Extract the METHOD values from the matched items
  method_values <- sapply(vector_items[matched_indices], function(x) {
    # Use a regex to extract the METHOD value
    match <- regexec("METHOD=([^ ]+)", x)
    # Return the captured value
    res <- NA # If no METHOD found
    if (attr(match[[1]], "match.length")[1] != -1) {
      res <- regmatches(x, match)[[1]][2] # Extract the value capturing group
    }
    switch(res,
           "1" = "FOCE",
           "2" = "FOCEI",
           "3" = "IPOPT",
           "4" = "LAP",
           "5" = "SAEM",
           "6" = "Two-Stage",
           "7" = "BAYESIAN",
           res
    )
  })
  # Print the extracted METHOD values
  return(paste(method_values, collapse = ", "))
}

shwhdbtn <- function(id = "dirfiletype1afiles") {
  tags$div(tags$button("Show:hide files", class = "btn btn-default mb-2", onclick = paste0("document.querySelector('#", id, "').classList.toggle('hidethis')")))
}
updateDirStatus <- function(message = "") {
  shinyjs::runjs(paste0("$('#trackfileupdates').html('", message, "')"))
}
extract_pattern <- function(file) {
  extract_words_with_braces <- function(string) {
    pattern <- "\\{([A-Z]+)\\}"
    matches <- stringr::str_extract_all(string, pattern)
    if (length(matches) > 0) {
      return(unlist(matches))
    } else {
      return(NULL)
    }
  }
  # Read the file line by line
  lines <- readLines(file)
  replacebr <- c()
  for (line in lines) {
    replacebr <- c(replacebr, extract_words_with_braces(line))
  }
  replacebr
}
showloader <- function(id) {
  if (missing(id)) {
    tags$img(src = "loadweb.gif", height = "70")
  } else {
    shinyjs::runjs(paste0("$('#", id, "').html('<img src=\"loadweb.gif\" height=\"70\">')"))
  }
}
updatePAIRheadreport <- function(message = "") {
  shinyjs::runjs(paste0("$('#repetapairbioplot').html('", message, "')"))
}
updateDVPIPheadreport <- function(message = "") {
  shinyjs::runjs(paste0("$('#repdvpredipredbioplot').html('", message, "')"))
}
updateQQheadreport <- function(message = "") {
  shinyjs::runjs(paste0("$('#repqqbioplot').html('", message, "')"))
}
updateETAheadreport <- function(message = "") {
  shinyjs::runjs(paste0("$('#reportetaplotstats').html('", message, "')"))
}
updateGOFheadreport <- function(message = "") {
  shinyjs::runjs(paste0("$('#reportgofplotstats').html('", message, "')"))
}
updateModListheadreport <- function(message = "") {
  shinyjs::runjs(paste0("$('#reportmodliststats').html('", message, "')"))
}
updateSimStatus <- function(message = "No data updates have been made.") {
  shinyjs::runjs(paste0("$('#tracksimulations').html('", message, "')"))
}
updateGraphStatus <- function(message = "") {
  shinyjs::runjs(paste0("$('#reportgraphstatus').html('", message, "')"))
}
updateEtaPlot <- function(message = "") {
  shinyjs::runjs(paste0("$('#repetabioplot').html('", message, "')"))
}
updateGraphStatus3 <- function(message = "") {
  shinyjs::runjs(paste0("$('#reporthiststatus2').html('", message, "')"))
}
updateEditMsg <- function(message = "") {
  shinyjs::runjs(paste0("$('#editmessagerep').html('", message, "')"))
}
updateGraphStatus4 <- function(message = "") {
  shinyjs::runjs(paste0("$('#repttablstatus1').html('", message, "')"))
}
updateVariableHolder <- function(message = "") {
  shinyjs::runjs(paste0("$('#varnamesholder').html('", message, "')"))
}
disableSims <- function(is = "1L") {
  shinyjs::runjs(paste0('$("#runsimbutton").prop("disabled",', is, ")"))
}
plot.theme06 <- list(theme(
  axis.title.y = element_text(face = "bold"),
  panel.background = element_rect(colour = "#333333"),
  strip.text = element_text(face = "bold")
))
plot.theme00 <- list(theme(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.title.y = element_text(face = "bold"),
  panel.background = element_rect(colour = "#333333"),
  strip.text = element_text(face = "bold")
))
plot.theme03 <- theme(
  plot.title = element_text(
    face = "bold",
    hjust = 0.5, margin = margin(0, 0, 20, 0)
  ),
  text = element_text(),
  panel.background = element_rect(colour = NA),
  plot.background = element_rect(colour = NA),
  panel.border = element_rect(colour = NA),
  axis.title = element_text(face = "bold", size = rel(1)),
  axis.title.y = element_text(angle = 90, vjust = 2),
  axis.title.x = element_text(vjust = -0.2),
  axis.text = element_text(),
  axis.line.x = element_line(colour = "black"),
  axis.line.y = element_line(colour = "black"),
  axis.ticks = element_line(),
  legend.key = element_rect(colour = NA),
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = "vetical",
  legend.key.size = unit(0.5, "cm"),
  # legend.margin = unit(0, "cm"),
  legend.title = element_text(face = "italic"),
  plot.margin = unit(c(10, 5, 5, 5), "mm"),
  strip.background = element_rect(colour = "#000000", fill = "#f3f3f3", linewidth = rel(1.6)),
  strip.text = element_text(face = "bold")
)
plot.theme01 <- list(
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "bold", angle = 90),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_text(size = 14, face = "bold")
  )
)
getTimeV <- function(n, t0) {
  if (n > 1) {
    c(0, pop_off(cumsum(t0)))
  } else {
    0
  }
}
pop_off <- function(.) {
  .[1:{
    length(.) - 1
  }]
}
calculate_auc <- function(time, concentration) {
  # Check if inputs are of the same length
  if (length(time) != length(concentration)) {
    stop("Time and concentration vectors must be of the same length.")
  }
  # Sort the time and concentration data by time
  sorted_indices <- order(time)
  time <- time[sorted_indices]
  concentration <- concentration[sorted_indices]
  # Calculate the AUC using the trapezoidal rule
  auc <- sum((time[-1] - time[-length(time)]) * (concentration[-1] + concentration[-length(concentration)]) / 2)
  return(auc)
}
sampleplot <- function() {
  colss <- sample(grDevices::colors(), 5)
  plot(c(0, 20, 100),
    c(0, 20, 100),
    bg = colss[1:3],
    xlab = "Sample x",
    cex = 2,
    pch = 21,
    axes = 1L,
    ylab = "Sample y",
    bty = "n"
  )
  # box(lwd=2, col=colss[4])
  text(50, 50, "Click 'Generate data version' to get started", cex = 1.5, pos = 3, col = "red")
}
sort_by_name_and_ext <- function(file_paths) {
  # Split the file names into name and extension
  file_info <- data.frame(
    file_name = tools::file_path_sans_ext(basename(file_paths)),
    extension = tools::file_ext(file_paths),
    full_path = file_paths,
    stringsAsFactors = FALSE
  )
  # Sort by file name, then by extension
  sorted_files <- file_info[order(file_info$extension), ]
  # Return the sorted file paths
  return(sorted_files$full_path)
}
switchicons <- function(xfile = "") {
  # file 2
  # file-excel 2
  # file-export 1 solid
  # file-word 2
  # file-import 1 solid
  # file-pdf 2
  # file-powerpoint 1 solid
  # file-lines 1 solid
  # file-image 1 solid
  # file-csv 1 solid
  # file-code 2
  # file-circle-question 1 solid
  #
  # folder 2
  # folder-open 2
  # folder-closed 2
  # folder-tree 1
  .icon <- "file-circle-question"
  switch(xfile,
    "txt" = {
      .icon <- "file"
    },
    "xlsx" = {
      .icon <- "file-excel"
    },
    "xls" = {
      .icon <- "file-excel"
    },
    "export" = {
      .icon <- "file-export"
    },
    "doc" = {
      .icon <- "file-word"
    },
    "docx" = {
      .icon <- "file-word"
    },
    "import" = {
      .icon <- "file-import"
    },
    "pdf" = {
      .icon <- "file-pdf"
    },
    "ppt" = {
      .icon <- "file-powerpoint"
    },
    "pptx" = {
      .icon <- "file-powerpoint"
    },
    "lines" = {
      .icon <- "file-lines"
    },
    "png" = {
      .icon <- "file-image"
    },
    "jpg" = {
      .icon <- "file-image"
    },
    "tiff" = {
      .icon <- "file-image"
    },
    "csv" = {
      .icon <- "file-csv"
    },
    "code" = {
      .icon <- "file-code"
    },
    "folder" = {
      .icon <- "folder"
    },
    "fopen" = {
      .icon <- "folder-open"
    },
    "fclose" = {
      .icon <- "folder-closed"
    },
    "dtree" = {
      .icon <- "folder-tree"
    },
    {
      .icon <- "file"
    }
  )
  return(.icon)
}
data_summarised_overall <- function(dataa) {
  if (nrow(dataa)) {
    dataa %>%
      filter(not.na(.dv)) %>%
      group_by(.summ, .tm) %>%
      reframe(
        dv_mean = mean(.dv),
        dv_med = median(.dv),
        sd = sd(.dv),
        sem = sd(.dv) / sqrt(length((.dv))),
        q95 = quantile(.dv, probs = 0.95),
        q05 = quantile(.dv, probs = 0.05),
        q975 = quantile(.dv, probs = 0.975),
        q025 = quantile(.dv, probs = 0.025)
      )
  }
}
data_summarised_facet <- function(dataa) {
  if (nrow(dataa)) {
    dataa %>%
      filter(not.na(.dv)) %>%
      group_by(.summ, .tm) %>%
      reframe(
        .colv = unique(.colv)[1],
        dv_mean = mean(.dv),
        dv_med = median(.dv),
        sd = sd(.dv),
        sem = sd(.dv) / sqrt(length((.dv))),
        q95 = quantile(.dv, probs = 0.95),
        q05 = quantile(.dv, probs = 0.05),
        q975 = quantile(.dv, probs = 0.975),
        q025 = quantile(.dv, probs = 0.025)
      )
  }
}
extract_pattern <- function(file) {
  extract_words_with_braces <- function(string) {
    pattern <- "\\{([A-Z]+)\\}"
    matches <- stringr::str_extract_all(string, pattern)
    if (length(matches) > 0) {
      return(unlist(matches))
    } else {
      return(NULL)
    }
  }
  # Read the file line by line
  lines <- readLines(file)
  replacebr <- c()
  for (line in lines) {
    replacebr <- c(replacebr, extract_words_with_braces(line))
  }
  replacebr
}
# extracted_patterns = suppressMessages(extract_pattern("includes/body/conc.vs.time/code.tpl"))
#
# print(unique(extracted_patterns))
# i = gsub("\\{","\\\\{",extracted_patterns)
# i2 = gsub("\\}","\\\\}",i)
# for(u in unique(i2)){
#   message("'",u,"',1,'','',")
# }
# yy[nzchar(yy)]
# rpl values
# 0 - value is provided
# 1 - value available in input to replace
# 2 - To be replaced with "" or remove entirely
# 3 - no values available in input, get in variable
# 4 - value is in a reactive
# 5 - value available in input, two inputs exist so choose from 'ITYPE'
code_download_checks_df <- tibble::tribble(
  ~srh, ~rpl, ~with, ~with2,
  "\\{SCRIPTDATA\\}", 5, "datatoUseconc1", "datatoUseconc2",
  "\\{DVVAR\\}", 1, "depvar1", "",
  "\\{IDVAR\\}", 1, "idvar", "",
  "\\{TYMEVAR\\}", 5, "indepvar", "indepvar2",
  "\\{LIBRARIES\\}", 4, "code.convtm.libs.glue", "",
  "\\{DATAFILE\\}", 4, "data.orig.filename", "",
  "\\{STORAGEPATH\\}", 0, "./", "",
  "\\{TDATE\\}", 0, as.character(Sys.Date()), "",
  "\\{RVERS\\}", 0, version$version.string, "",
  "\\{FACETVAR\\}", 1, "cfacetvar", "",
  "\\{SUMMVAR\\}", 1, "summby", "",
  "\\{COLORVAR\\}", 1, "colvar3", "",
  "\\{CHOSENDATA\\}", 4, "data.versions.filter", "datatoUseconc1",
  "\\{LDVMEAN\\}", 2, "graphsummtype", "1:3",
  "\\{LDVMEDIAN\\}", 2, "graphsummtype", "4:6",
  "\\{LNOTSUMMARISEPLOT\\}", 2, "cgraphtype", "c(1,2,4,5)",
  "\\{LSUMMARISEPLOT\\}", 2, "cgraphtype", "c(3,6)",
  "\\{LEGENDCOLNUM\\}", 1, "ncollegend", "",
  "\\{ILABELX\\}", 5, "labelx", "labelx2",
  "\\{ILABELY\\}", 1, "labely", "",
  "\\{LMEANMEDIANALONE\\}", 2, "graphsummtype", "c(1,4)",
  "\\{LREMOVECOLORVAR\\}", 2, "cgraphtype", "c(1,4,7,8)",
  "\\{IDVAR\\}", 1, "idvar", "",
  "\\{LSPAGHETTIPLOT\\}", 2, "cgraphtype", "c(1,4,7)",
  "\\{LNOTSPAGHETTIPLOT\\}", 2, "cgraphtype", "c(2,3,5,6,8)",
  "\\{LINDVPLOT\\}", 2, "cgraphtype", "7:8",
  "\\{LSCATTERPLOT\\}", 2, "cgraphtype", "c(2,5,8)",
  "\\{LSUMMARYPLOT\\}", 2, "cgraphtype", "c(3,6)",
  "\\{LNOTMEANMEDIANALONE\\}", 2, "graphsummtype", "c(2,3,5,6)",
  "\\{LFACETPLOT\\}", 2, "cgraphtype", "4:6",
  "\\{FACETCOLNUM\\}", 1, "graphcolnum", "",
  "\\{LSUMMARYPLOTA\\}", 2, "graphsummtype", "2",
  "\\{LSUMMARYPLOTB\\}", 2, "graphsummtype", "3",
  "\\{LSUMMARYPLOTC\\}", 2, "graphsummtype", "5",
  "\\{LSUMMARYPLOTD\\}", 2, "graphsummtype", "6",
  "\\{LFACETPLOTSUMM\\}", 2, "cgraphtype", "6",
  "\\{LSEMILOGPLOT\\}", 2, "loglinear", '"semi-log"',
  "\\{TEXTFONT\\}", 1, "graphfont", "",
  "\\{FONTTICKSIZE\\}", 1, "fontxyticks", "",
  "\\{FONTXYSIZE\\}", 1, "fontxytitle", "",
  "\\{FONTSTRIPSIZE\\}", 1, "fontxystrip", "",
  "\\{LEGENDPOS\\}", 1, "legendposition", "",
  "\\{TYMEVAR0\\}", 3, "TYMEVAR0", "",
  "\\{GRAPHTYPE1\\}", 3, "GRAPHTYPE1", "",
  "\\{GRAPHTYPE2\\}", 3, "GRAPHTYPE2", "",
  "\\{GRAPHTYPE3\\}", 1, "loglinear", "",
  "\\{IMAGEWIDTH\\}", 1, "downimgw", "",
  "\\{IMAGEHEIGHT\\}", 1, "downimgh", "",
  "\\{IMAGEDPI\\}", 1, "downimgdpi", "",
  "\\{IMAGESCALE\\}", 1, "downimgs", ""
)
symbol <- list(
  pass = "✔️",
  fail = "❌",
  minus = "➖",
  plus = "➕",
  times = "✖️",
  divide = "➗",
  warn1 = "❕",
  warn2 = "❗",
  ques1 = "❔",
  ques2 = "❓"
)
plot.theme10 <- list(theme(
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16, face = "bold", angle = 90),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16),
  legend.text = element_text(size = 16),
  legend.title = element_text(size = 16),
  panel.background = element_rect(colour = "#000000"),
  strip.text.x = element_text(size = 16, face = "bold")
))
theme_set(theme_bw() + plot.theme10)
gof.plots.a <- function(datafile, exp.filter = NULL, log = TRUE, concuni, tmuni) {
  # sddat <- read.table(paste0(model.path, "sdtab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
  # codat <- read.table(paste0(model.path, "cotab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
  # cadat <- read.table(paste0(model.path, "catab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
  # padat <- read.table(paste0(model.path, "patab", runid), header = TRUE, sep = "", na.strings = ".", skip = 1)
  if (is.null(datafile)) updateGOFheadreport("File does not exist")
  dat <- datafile %>% dplyr::filter(DV != 0 & IPRED != 0)
  # filter if filter string is present
  if (not.empty(exp.filter)) {
    dat <- dat %>% dplyr::filter(rlang::eval_tidy(rlang::parse_expr(exp.filter)))
  }
  # GOF lists
  model.gof <- list(linear = list(), log = list())
  # DV vs. PRED
  ylab <- paste0("Observed concentrations (", concuni, ")")
  xlab <- paste0("Population prediction (", concuni, ")")
  p1 <- ggplot(data = dat, aes(x = PRED, y = DV)) +
    geom_point(aes(group = ID), size = 2.2, shape = 1, alpha = 0.8) +
    stat_smooth(method = "lm", span = 1.2, se = F, col = "red", linewidth = 1, linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1, col = "#000000") +
    xlab(xlab) +
    ylab(ylab)
  # linear
  model.gof[["linear"]]$dvpred <- p1 + theme_test() + plot.theme10
  # log
  if (log) {
    model.gof[["log"]]$dvpred <- p1 + theme_test() + plot.theme10 +
      scale_y_log10(breaks = c(0.1, 1, 10, 50), labels = c(0.1, 1, 10, 50), limits = c(0.01, 50)) +
      scale_x_log10(breaks = c(0.1, 1, 10, 50), labels = c(0.1, 1, 10, 50), limits = c(0.01, 50))
  }
  # DV vs. IPRED
  xlab <- paste0("Individual prediction (", concuni, ")")
  p1 <- ggplot(data = dat, aes(x = IPRED, y = DV)) +
    geom_point(aes(group = ID), size = 2.2, shape = 1, alpha = 0.8) +
    stat_smooth(method = "lm", span = 1.2, se = F, col = "red", linewidth = 1, linetype = "dashed") +
    geom_abline(intercept = 0, slope = 1, col = "#000000") +
    xlab(xlab) +
    ylab(ylab)
  # linear
  model.gof[["linear"]]$dvipred <- p1 + theme_test() + plot.theme10
  # log
  if (log) {
    model.gof[["log"]]$dvipred <- p1 + theme_test() + plot.theme10 +
      scale_y_log10(breaks = c(0.1, 1, 10, 50), labels = c(0.1, 1, 10, 50), limits = c(0.01, 50)) +
      scale_x_log10(breaks = c(0.1, 1, 10, 50), labels = c(0.1, 1, 10, 50), limits = c(0.01, 50))
  }
  # CWRES vs. Time
  ylab <- "CWRES"
  xlab <- paste0("Time Since First Dose (", tmuni, ")")
  model.gof[["linear"]]$cwrestime <- ggplot(data = dat, aes(x = TIME, y = CWRES)) +
    geom_point(aes(group = ID), size = 2.2, shape = 1, alpha = 0.8) +
    stat_smooth(method = "loess", span = 1.2, se = F, col = "red", linewidth = 1, linetype = "dashed") +
    geom_hline(yintercept = 0, size = 0.7) +
    geom_hline(yintercept = -6, size = 0.7, linetype = 2) +
    geom_hline(yintercept = 6, size = 0.7, linetype = 2) +
    xlab(xlab) +
    ylab(ylab) +
    theme_test() +
    plot.theme10 +
    scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-11, 11))
  # CWRES vs. population predictions
  ylab <- "CWRES"
  xlab <- paste0("Population prediction (", concuni, ")")
  model.gof[["linear"]]$cwrespred <- ggplot(data = dat, aes(x = PRED, y = CWRES)) +
    geom_point(aes(x = PRED, y = CWRES, group = ID), size = 2.2, shape = 1, alpha = 0.8) +
    stat_smooth(method = "loess", span = 1.2, se = F, col = "red", linewidth = 1, linetype = "dashed") +
    geom_hline(yintercept = 0, size = 0.7) +
    geom_hline(yintercept = -6, size = 0.7, linetype = 2) +
    geom_hline(yintercept = 6, size = 0.7, linetype = 2) +
    xlab(xlab) +
    ylab(ylab) +
    theme_test() +
    plot.theme10 +
    scale_y_continuous(breaks = seq(-10, 10, 5), limits = c(-11, 11))
  # return plots
  model.gof
}
outliers <- function(runno, directory, cwres.lim = 6) {
  filenam <- paste("sdtab", file.path(direcotory, runno), sep = "")
  TM1 <- read.table(filenam, skip = 1, sep = ",", header = T)
  tm <- subset(TM1, CWRES < cwres.lim & CWRES > -cwres.lim)
  outlier <- subset(TM1, CWRES >= cwres.lim | CWRES <= -cwres.lim)
  write.csv(outlier, paste(runno, "outlier1.csv", sep = ""), row.names = FALSE, na = ".", quote = FALSE)
  nID <- subset(TM1, CWRES >= cwres.lim | CWRES <= -cwres.lim)$ID
}
# create VPC plots
createPcVPCPlots <- function(
    dir,
    vpcinfo = "vpc_results.csv",
    vpctab = "vpctab1vpc",
    logY = TRUE,
    xLab = "Nominal Time After First Dose (weeks)",
    yLab = "Prediction Corrected\nConcentration (ug/mL)",
    title = NULL,
    title.t = NULL) {
  ##### plot setting  #############################################################
  # pcVPC_run
  vpc_info <- paste0(dir, "/", vpcinfo)
  vpctab <- paste0(dir, "/", vpctab)
  xpose.VPC(
    vpc.info = vpc_info,
    vpctab = vpctab,
    max.plots.per.page = 2,
    object = NULL,
    grid = F,
    col = "#4A333333",
    type = "p", cex = 0.4,
    PI = NULL,
    PI.ci = "area",
    PI.ci.area.smooth = T,
    PI.limits = c(0.05, 0.95),
    PI.real.up.col = "red",
    PI.real.med.col = "red",
    PI.real.down.col = "red",
    PI.identify.outliers = FALSE,
    main = title.t,
    main.sub = title,
    main.sub.cex = 1.25,
    xlb = xLab,
    ylb = yLab,
    layout = c(1, 1),
    logy = logY,
    x.cex = 1,
    y.cex = 1,
    ylim = c(0.01, 100),
    scales = ifelse(logY, list(y = list(cex = 1, limits = c(0.01, 100), at = c(0.01, 0.1, 1, 10, 100), alternating = FALSE), cex = 1.5),
      list(y = list(cex = 1, relation = "free", tck = 0), x = list(cex = 1, relation = "free", tck = 0), cex = 1.5)
    )
  )
}
parse_nm_lst <- function(filepath) {
  # Read the lines safely
  lines <- tryCatch(readLines(filepath, warn = FALSE),
                    error = function(e) character(0))
  # Initialize results
  minimization_success <- FALSE
  nsig <- NA_real_
  covariances <- NA_real_
  zerogradient <- FALSE
  descr <- meth <- "N/A"
  dnum <- runtime <- objective_function_value <- NA_real_
  etashrinksd <- etashrinkvr <- numeric(0)
  input_file <- "N/A"
  output_files <- NULL
  if (length(lines) > 0) {
    # Check for minimization success
    minimization_success <- any(grepl("0MINIMIZATION SUCCESSFUL", lines, fixed = TRUE))
    # Check for covariance success
    cov_st <- sub("0COVARIANCE STEP OMITTED: *([[:alpha:]]{1,}) *","\\1",grep("0COVARIANCE",lines, value = TRUE))
    if(length(cov_st)) covariances <- ifelse(cov_st=="NO", TRUE, FALSE)
    # Match line containing NSIG info
    sig_digits_line <- grep("NO\\. OF SIG\\. DIGITS IN FINAL EST", lines, value = TRUE)
    if (length(sig_digits_line) > 0) {
      # Extract the first number from the line
      nsig_match <- regmatches(sig_digits_line, regexpr("[0-9]+\\.?[0-9]*", sig_digits_line))
      nsig <- as.numeric(nsig_match)
    }
    # Match $EST or $ESTIMATION line
    est_line <- grep("^\\s*\\$(EST|ESTIMATION)\\b", lines, value = TRUE)
    if (length(est_line) > 0) {
      method_match <- regmatches(est_line, regexpr("METHOD=\\S+", est_line))
      if (length(method_match) > 0) {
        meth <- sub("METHOD=", "", method_match[1])
      }
    }
    # Extract OBJV if present
    objv_line <- grep("OBJV", lines, value = TRUE)
    if (length(objv_line) > 0) {
      ofv_match <- regmatches(objv_line, regexpr("-?\\d+\\.?\\d*", objv_line))
      if (length(ofv_match) > 0) {
        objective_function_value <- as.numeric(ofv_match)
      }
    }
    # Extract input data file
    data_line <- grep("^\\s*\\$DATA\\b", lines, value = TRUE)
    if (length(data_line) > 0) {
      # Remove $DATA and trim, then take first word (assumed to be the filename)
      content <- trimws(sub("^\\s*\\$DATA\\s+", "", data_line[1]))
      if (nchar(content) > 0) {
        input_file <- strsplit(content, "\\s+")[[1]][1]
      }
    }
    #output
    file_matches <- unlist(regmatches(lines, gregexpr("FILE=\\S+", lines)))
    if (length(file_matches) > 0) {
      output_files <- sub("FILE=", "", file_matches)
    }
    # DNUM + RUNTIME
    elapsed_index <- grep("Elapsed finaloutput time in seconds", lines)
    if (length(elapsed_index) > 0) {
      # RUNTIME
      elapsed_line <- lines[elapsed_index[1]]
      runtime_match <- regmatches(elapsed_line, regexpr("[0-9]+\\.?[0-9]*", elapsed_line))
      if (length(runtime_match) > 0) {
        runtime <- as.numeric(runtime_match)
      }
      # DNUM (ratio of last to first number from line above)
      for (i in seq(elapsed_index[1] - 1, 1)) {
        number_matches <- gregexpr("-?\\d+\\.?\\d*[Ee]?[-+]?\\d*", lines[i])
        numbers_found <- regmatches(lines[i], number_matches)[[1]]
        if (length(numbers_found) > 1) {
          numeric_vals <- as.numeric(numbers_found)
          if (!is.na(numeric_vals[1]) && numeric_vals[1] != 0) {
            dnum <- round(numeric_vals[length(numeric_vals)] / numeric_vals[1],1)
          } else {
            dnum <- NA_real_
          }
          break
        }
      }
    }
    # Check gradient lines
    gradient_lines <- grep("GRADIENT: ", lines, value = TRUE)
    if (length(gradient_lines) > 0) {
      all_numbers <- unlist(regmatches(gradient_lines, gregexpr("-?\\d+\\.?\\d*[Ee]?[-+]?\\d*", gradient_lines)))
      all_values <- as.numeric(all_numbers)
      if (any(all_values == 0)) {
        zerogradient <- TRUE
      }
    }
    # ETASHRINKSD (support scientific notation)
    shrinksd_line <- grep("ETASHRINKSD\\(\\%\\)", lines, value = TRUE)
    if (length(shrinksd_line) > 0) {
      etashrinksd <- as.numeric(unlist(regmatches(
        shrinksd_line,
        gregexpr("-?\\d+\\.?\\d*(?:[Ee][+-]?\\d+)?", shrinksd_line)
      )))
    }

    # ETASHRINKVR (support scientific notation)
    shrinkvr_line <- grep("ETASHRINKVR\\(\\%\\)", lines, value = TRUE)
    if (length(shrinkvr_line) > 0) {
      etashrinkvr <- as.numeric(unlist(regmatches(
        shrinkvr_line,
        gregexpr("-?\\d+\\.?\\d*(?:[Ee][+-]?\\d+)?", shrinkvr_line)
      )))
    }
    # Match $PROBLEM line
    problem_line <- grep("^\\s*\\$PROBLEM", lines, value = TRUE)
    if (length(problem_line) > 0) {
      # Remove the $PROBLEM part and trim the rest
      descr_raw <- sub("^\\s*\\$PROBLEM", "", problem_line[1])
      descr <- trimws(descr_raw)
      if (descr == "") {
        descr <- "N/A"
      }
    }
  }
  # Return the result list
  return(list(
    MIN = minimization_success,
    NSIG = nsig,
    COV = covariances,
    OFV = objective_function_value,
    INPUT = input_file,
    OUTPUT = output_files,
    ZEROGRADIENT = zerogradient,
    DESCR = descr,
    METH = meth,
    CONDNUM = dnum,
    RUNTIME = runtime,
    ETASHRINKSD = etashrinksd,
    ETASHRINKVR = etashrinkvr
  ))
}

covariates_titles <- c(
  AGE = "Age (years)",
  WT = "Weight (kg)",
  HEIGHT = "Height (cm)",
  SEX = "Gender (M/F)",
  BSA = "Body Surface Area (m²)",
  BMI = "Body Mass Index (kg/m²)",
  CLCR = "Creatinine Clearance (mL/min)",
  LIVFUNC = "Liver Function (score/system)",
  ALBUMIN = "Serum Albumin (g/dL)",
  TBILI = "Total Bilirubin (mg/dL)",
  ALT = "Alanine Aminotransferase (U/L)",
  AST = "Aspartate Aminotransferase (U/L)",
  DOSE = "Dose (mg)",
  ROUTE = "Route of Administration (oral, IV, etc.)",
  COVAR_DRUG = "Co-administered Drug (yes/no)",
  GENOTYPE = "Genotype (type)",
  ALLELE = "Allele Variant (type)",
  SMOKING = "Smoking Status (smoker/non-smoker)",
  ALCOHOL = "Alcohol Consumption (g/week)",
  DIABETES = "Diabetes Mellitus (yes/no)",
  HF = "Heart Failure (yes/no)",
  REGION = "Region (geographic area)"
)
