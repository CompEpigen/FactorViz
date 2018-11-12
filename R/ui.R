# This script contains the basic UI interface of FactorViz, UI code related to individual
# tabs can be found inside UI folder.

baseUI <- function(){
          #source('ui_home.R', local = T)
          #source('ui_k_selection.R', local = T)
          #source('ui_l_selection.R', local = T)
          #source('ui_lmc.R', local = T)
          #source('ui_proportions.R', local = T)
          tagList(
            navbarPage(
              theme = shinythemes::shinytheme("cosmo"),
              "FactorViz 2.0",
              home_tab(),
              k_selec(),
              l_selec(),
              lmc_tab(),
              proportion(),
              tabPanel("Meta Analysis", "Page Under Construction"),
              #tabPanel("Meta Analysis", "This panel is intentionally left blank"),
              tabPanel("Downloads", "Page Underconstruction")
            )
          )
        }
