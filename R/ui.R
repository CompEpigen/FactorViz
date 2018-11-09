# This script contains the basic UI interface of FactorViz, UI code related to individual 
# tabs can be found inside UI folder.
source('ui/home.R', local = T)
source('ui/k_selection.R', local = T)
source('ui/l_selection.R', local = T)
source('ui/lmc.R', local = T)
source('ui/proportions.R', local = T)
baseUI <- tagList(
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

