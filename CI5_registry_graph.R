  pkg_folder <- "//inti/CIN/Xchange/Mathieu/CI5_shiny"

  app_folder <- "C:/CSU_shiny"
  #app_folder <- pkg_folder
  
  .libPaths(paste0(app_folder, "/pkg"))
  library(shiny)
  shiny::runApp(paste0(app_folder,"/CI5_registry_graph"), launch.browser = TRUE)


  