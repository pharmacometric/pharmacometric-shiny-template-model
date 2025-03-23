#########################################################################################
#########################################################################################
##
##  Document Path: global.R
##
##  R version: 4.2.0 (2022-04-22)
##
##  Program purpose: Global library loads, prep environment and load libs
##
#########################################################################################
#########################################################################################



# clear console, set dir, load libs and load files
quickcode::clean(source = c("utils.R"), clearPkgs = 1L)

# load libraries
libs = c("tools","shinyTree","nonmem2R","shinyAce","shiny","shinyjs","shinyStorePlus","DT","flextable","nlme","markdown","tibble","card.pro","dplyr","tidyr","ggplot2","magrittr","mrgsolve","quickcode","patchwork","table1","r2resize","rlang","grid","NMdata","GGally")
lapply(libs, function(l)library(l,character.only=1L))

# add all individual utils
for (ui_each in c(
  "includes/header",
  "includes/body",
  "includes/footer"
)) {
  this.path = ui_each
  source(file.path(ui_each,"libs.R"), local = T)
}


onStop(function() {
  clear(clearPkgs = TRUE) # on shiny stop, clean env
  .rs.restartR() # if using rstudio
})

# ace editor
modes <- getAceModes()
themes <- getAceThemes()

init <- "# sample code"
initnewcode <- paste0(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Document Path: newfile.mod
;;
;;  NONMEM version: 7.5
;;
;;  Date: ",Sys.time(),"
;;
;;  Author:
;;
;;  Description:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

$PROBLEM BasePK model 1
$INPUT ID TIME MDV EVID DV AMT  SEX WT ETN
$DATA sample.csv IGNORE=@
$SUBROUTINES ADVAN2 TRANS2
$PK
$ERROR
$EST
$COV
$TABLE
")

# declare the global reactive and regular values
GLOBAL= reactiveValues()
GLOBAL$lastsim = NULL
GLOBAL$start.sim = FALSE
seed.val = 67772
GLOBAL$objects = NULL
GLOBAL$data.orig.filename = "example.csv"
GLOBAL$modelnum.evstore = list()
GLOBAL$data.versions = list("original" = data.frame(),"dataV2" = data.frame(),"dataV3" = data.frame())
GLOBAL$data.versions.filter = list("original" = "","dataV2" = "","dataV3" = "")
data.versions.names = c("original","dataV2","dataV3")
font.family = c("sans","serif","mono","Comic Sans MS","AppleGothic","Optima","Luminari")
modelnum.ofv = modelnum.est = modelnum.vals = modelnum.proc = reactiveVal(list())
GLOBAL$modelnum.run = list()
GLOBAL$dir.files = c()
GLOBAL$curr.edit.files = ""
.defaultDir = "www/example"
.proceed = new.env()
