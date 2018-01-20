library(data.table)

source(paste(sep="/", app_folder, "source/Rcan_core.r"))

app_folder <- "c:/CSU_shiny/CI5_registry_bar"
dt_CI5 <- data.table(readRDS(paste0(app_folder, "/data/CI5XI.rds")))

#group by country
dt_selection <- read.csv(paste0(app_folder, "/_dict/registry_selection.csv"))
dt_selection <- data.table(dt_selection)
dt_CI5_country <- merge(dt_CI5, dt_selection, by=c("registry", "registry_lab", "ethnic_group", "CI5_continent"))
dt_CI5_country <- dt_CI5_country[selection ==1 ]

#correct mising age pop
dt_CI5_country[age ==19, py:=0]

#get country_code and label
dt_CI5_country <- CSU_country_info(dt_CI5_country, "registry", paste0(app_folder, "/_dict/"))
dt_CI5_country[, c("area_code", "area_label", "country_iso3", "notes", "selection") := NULL]

#create consistent number of age group within each country

dt_CI5_country[py >0, registry_nb_age:=max(age), by="registry"]
dt_CI5_country[, country_nb_age:=min(registry_nb_age, na.rm=TRUE), by="country_label"]
print(unique(dt_CI5_country[(registry_nb_age > country_nb_age) & !is.na(registry_nb_age), c("registry_lab", "country_nb_age", "registry_nb_age"), with=FALSE]), row.names = FALSE) # list country update

dt_CI5_country[country_nb_age <= 15 & age >= 15, py:=sum(py), by=c("registry", "registry_lab", "ethnic_group","sex", "cancer")]
dt_CI5_country[country_nb_age <= 16 & age >= 16, py:=sum(py), by=c("registry", "registry_lab", "ethnic_group","sex", "cancer")]
dt_CI5_country[country_nb_age <= 17 & age >= 17, py:=sum(py), by=c("registry", "registry_lab", "ethnic_group","sex", "cancer")]
dt_CI5_country[ age > country_nb_age, py:=0]

#keep America black and white apart
dt_CI5_country[country_code == 840 & ethnic_group == 10, country_code := 841]
dt_CI5_country[country_code == 840 & ethnic_group == 30, country_code := 842]

#rename country_label if only special ethnic
dt_CI5_country[ethnic_group < 99, country_label := substr(registry_lab, 0,nchar(as.character(registry_lab))-12)]

#regroup by country
dt_CI5_country <- dt_CI5_country[, list( cases=sum(cases), py=sum(py), regional=sum(regional)), by=c("age", "country_code", "country_label", "cancer", "cancer_lab", "sex", "CI5_continent")]
saveRDS(dt_CI5_country, (paste0(app_folder,"/data/CI5XI_country.rds")))


