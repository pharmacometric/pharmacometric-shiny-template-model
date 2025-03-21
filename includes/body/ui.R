############################################################################
############################################################################
##  Document Path: includes/body/ui.R
##
##  Description: User interface for main body section
##
##  R version 4.4.1 (2024-06-14 ucrt)
##
#############################################################################
#############################################################################
# assemble right contents
body.panel.right = primePanel(
  width = 9,
  body.panel.right.moderun,
  body.panel.right.modelwrite,
  body.panel.right.gof,
  body.panel.right.etaplot,
  body.panel.right.qqplot,
  body.panel.right.etapair,
  body.panel.right.individ

  #body.panel.right.vpc
)

body.panel.left = primePanel(
  width = 3, body.panel.paths
)

body.main = moveable(
  body.top.guide, # script located in header folder
  body.panel.left, #
  body.panel.right #
)
