


#create  dictionnary#
fileRDS <- "CI5XI_country.rds"
dt_temp <- data.table(readRDS(paste0("data/", fileRDS)))
setnames(dt_temp, "country_code", "registry")
setnames(dt_temp, "country_label", "registry_lab")
dt_temp <- dt_temp[,c("registry", "registry_lab", "CI5_continent"),with=FALSE]
dt_temp <- unique(dt_temp)
dt_con <- read.csv("data/continent_lab.csv")
dt_temp <- merge(dt_temp, dt_con, by="CI5_continent")
write.csv(dt_temp,paste0("data/", "country_list.csv"))



#install Rcan from Github
.libPaths( "C:\\CSU_shiny\\CI5_registry_graph\\pkg")
detach(package:Rcan)
remove.packages("Rcan")

devtools::install_github("timat35/Rcan", subdir="Rcan")


#install package for shiny
.libPaths( "C:/Users/laversannem/Documents/R/win-library/3.4")
install.packages('rsconnect')


# run with setwd()
setwd("C:\\CSU_shiny\\CI5_registry_graph")
.libPaths( "C:/CSU_shiny/pkg")
shiny::runApp()

# deploy app on shinyapp.io
.libPaths( "C:/Users/laversannem/Documents/R/win-library/3.4")


setwd("C:\\CSU_shiny\\CI5_registry_graph")
rsconnect::setAccountInfo(name='timat',
                          token='6A73F9BD41F1508711A1A96A01E7417D',
                          secret='ycADL/cqsKPfdluDBxKCZ+C2wo7GJkxfZxCr6Ict')

rsconnect::deployApp()

#get log from shinyapp.io
rsconnect::showLogs(streaming = TRUE)

create_config

#Rinno installation without or R include
install.packages("RInno") 
library(RInno)

setwd("C:\\CSU_shiny\\CI5_registry_graph")
create_app(app_name = "CI5_graph",
           pkgs=c("shiny","shinydashboard", "shinyjs", "data.table", "ggplot2", "gridExtra", "Cairo", "officer"),
           remotes = c("timat35/Rcan/Rcan"),
           include_R = FALSE,
           app_version= "0.1.0",
           include_Chrome=TRUE,
           default_dir = "c:\\CI5_graph",
           setup_icon = "app.ico")

compile_iss()

#Debug Rinno app
setwd("C:/CI5_graph")
shiny::runApp()

