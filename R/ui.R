# This script contains the basic UI interface of FactorViz, UI code related to individual
# tabs can be found inside UI folder.

baseUI <- function(){
          tagList(
            navbarPage(
              theme = shinythemes::shinytheme("cosmo"),
              "FactorViz 2.0",
              home_tab(),
              k_selec(),
              l_selec(),
              lmc_tab(),
              proportion(),
              #meta_analysis(),
              tabPanel("Meta Analysis", "Page Under construction"),
              tabPanel("Downloads", "Page Under construction")
            )
          )
        }
