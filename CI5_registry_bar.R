
app_folder <- "C:/CSU_shiny"
.libPaths(paste0(app_folder, "/pkg"))
library(shiny)
shiny::runApp(paste0(app_folder,"/CI5_registry_bar"), launch.browser = TRUE)
