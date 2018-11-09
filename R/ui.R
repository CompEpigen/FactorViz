# This script contains the basic UI interface of FactorViz, UI code related to individual
# tabs can be found inside UI folder.

baseUI <- function(){tagList(
            navbarPage(
              theme = shinythemes::shinytheme("cosmo"),
              "FactorViz 2.0",
              home_tab,
              k_selec,
              l_selec,
              lmc_tab,
              proportion,
              tabPanel("Meta Analysis", "This panel is intentionally left blank"),
              #tabPanel("Meta Analysis", "This panel is intentionally left blank"),
              tabPanel("Downloads", "This panel is intentionally left blank")
            )
          )
          }
