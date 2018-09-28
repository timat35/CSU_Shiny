

#to test
 #input <- list()
 #input$select_registry <- 12001
# input$select_format <- "pdf"
# input$text_filename <- "test"
# input$selectCancerSite <- "lip"

shinyServer(function(input, output, session) {

  #app close when the session is stopped  
  session$onSessionEnded(function() {
    cat("back to canreg")
    stopApp()
  })
  
  
  nb_package <- 6
  #Loading packages
  withProgress(message = 'loading package', value = 0, {
    incProgress(0, detail="data.table")
    library(data.table)
    incProgress(1/nb_package, detail = "Rcan")
    library(Rcan)
    incProgress(1/nb_package, detail="ggplot2")
    library(ggplot2)
    incProgress(1/nb_package, detail="gridExtra")
    library(gridExtra)
    incProgress(1/nb_package, detail="Cairo")
    library(Cairo)
    incProgress(1/nb_package, detail="officer")
    library(officer)
    incProgress(1/nb_package, detail="Done")
    
  })
  

  source("source/Rcan_core.r")
  #source("c:/projects/Rcan/Rcan/R/helper.r")
  #source("c:/CSU_shiny/test_fonction.r")
  file_cancer_color_ci5 <- "data/color_cancer_ci5.csv"
  file_cancer_color_globo <- "data/color_cancer_globocan.csv"
  file_globocan_dict <- "data/cancer_ci5_globocan.csv"
  file_data <- "data/"
  file_pptx <-paste(sep="/","slide_template", "shiny_template.pptx")
  file_utf8 <- ""
  
  #Change for shinyappio: 
  file_utf8 <- "_UTF8"
  
  
  #Parametre and fixed variable
  
  graph_width <- 8
  graph_width_vertical <- 4
  
  dt_cancer_color_ci5 <- data.table(read.csv(file_cancer_color_ci5))
  dt_cancer_color_globo <- data.table(read.csv(file_cancer_color_globo))
  dt_globo_dict <- data.table(read.csv(file_globocan_dict))
  
  #reactive values init
  values <- reactiveValues(doc = NULL, nb_slide = 0, text= "Slide included")
  bool_rv <- reactiveValues(trigger1=FALSE)
  registry_info <- reactiveValues(data=NULL, label ="")
  progress_bar <- reactiveValues(object=NULL)
  cancer_group <- reactiveValues(select = "ci5")
  table <- reactiveValues(label="")

  #UI controller
  output$UI_registry <- renderUI({
    
    if (input$check_country) {
      fileCSV <- paste0("country_list", file_utf8, ".csv")
      title <- "Country"
    } else {
      fileCSV <-  paste0("registry_list", file_utf8, ".csv")
      title <- "Registry"
    }
    
    dt_list <- read.csv(paste0(file_data, fileCSV))
    dt_list <- dt_list[dt_list$continent_lab == input$select_continent,]
    
    registry_list <- list()
    for (i in 1:nrow(dt_list)) {
      registry_list[[as.character(dt_list$registry_lab[i])]] <- dt_list$registry[i]
    }
    
    registry_info$data <- data.table(dt_list)
    selectInput("select_registry", title,registry_list)
    
  })
  
  output$UI_nbSlide <- renderUI({
    
    
    if (values$nb_slide == 0) {
      
      
      
      values$text <- "Slide included:"
    } else {
      
      values$text <- paste0(isolate(values$text), '<br>',paste0(isolate(registry_info$label), " ", isolate(table$label)))
      
      if (isolate(input$select_table == 5)) {
        values$text <- paste(isolate(values$text), isolate(input$selectCancerSite))
      }
      
     
      
    }
    
    tags$div(id="divSlidelist", class="mat_text", checked=NA,
             tags$p(HTML(isolate(values$text)))
    )
    
  })
  
  output$UI_control1 <- renderUI({
    
    if (input$select_table==2) {
      
      radioButtons("radioAgeGroup", "Age-group division:",
                   c("0-4,5-9,...,80-84,85+" = 1,
                     "0-14, 15-29,30-49,50-69,70+" = 2)
      )
    }  
    
    else if  (input$select_table %in% c(3,6)) {
      
      radioButtons("radioValue", "Value:",
                   c("Age-standardized rate" = "asr",
                     "Number of cases" = "cases",
                     "Cumulative risk" = "cum")
      )
      
    }
    
    else if (input$select_table %in% c(4,5)) {
      
      radioButtons("radioLog", "y axes scale:",
                   c("Logarithmic" = "log",
                     "Normal" = "normal")
      )
      
    }
  })
  
  output$UI_control2 <- renderUI({
    
    if (input$select_table %in% c(3,4,5,6)) {
      
      temp <- isolate(cancer_group$select)
      
      radioButtons("radioCancer", "Cancer group:",
                   c("CI5 XI" = "ci5",
                     "Globocan" = "globocan"), 
                     selected = temp)
      
    }
    
  })
  
  output$UI_control3 <- renderUI({
    
    
    if  (input$select_table %in% c(3,6)) {
      
      sliderInput("slideNbTopBar", "Number of cancer sites:", 3, 20, 10)
      
      
    } 
    else if (input$select_table==4) {
      
      sliderInput("slideNbTopAgeSpe", "Number of cancer sites:", 1, 10, 5)
      
      
    } else if (input$select_table==5) {
      

      if (isolate(input$radioCancer) == "ci5") {
        fileCSV <- "color_cancer_ci5.csv"
      } else {
        fileCSV <- "color_cancer_globocan.csv"
      }
      
      dt_list <- read.csv(paste0(file_data, fileCSV))
      
      cancer_list <- dt_list$cancer_lab
      n <- length(cancer_list)
      cancer_list <- as.character(cancer_list)

      if (isolate(input$radioCancer == "ci5")) {
        cancer_list <- c(cancer_list[n], cancer_list[n-1], cancer_list[1:(n-2)])
      } else {
        cancer_list <- c(cancer_list[n], cancer_list[1:(n-1)])
      }
      
      selectInput("selectCancerSite", "Select cancer sites", cancer_list)
      
      
    }
    
  })
  
  output$UI_control4 <- renderUI({
    
    if (input$select_table %in% c(3,6)) {
      sliderInput("slideAgeRange", "Age group:", 0, 90, c(0,90), step=5)
    }
  })


    
  
  observeEvent(input$select_table,{
    
    
    if (input$select_table==1) {
      table$label <- "Population pyramid"
      hide(id="controls_COL1", anim=TRUE)
      hide(id="controls_COL2", anim=TRUE)
    } 
    else if (input$select_table==2) {
      table$label <- "Barchart cases by age"
      show(id="controls_COL1", anim=TRUE)
      hide(id="controls_COL2", anim=TRUE)
    }
    else if (input$select_table== 3) {
      table$label <- "Barchart Top cancer both sexes"
      show(id="controls_COL1", anim=TRUE)
      show(id="controls_COL2", anim=TRUE)
    }
    else if (input$select_table== 4) {
      table$label <- "Age specific trend top cancer"
      show(id="controls_COL1", anim=TRUE)
      show(id="controls_COL2", anim=TRUE)
    }
    
    else if (input$select_table== 5) {
      table$label <- "Age specific trend"
      show(id="controls_COL1", anim=TRUE)
      show(id="controls_COL2", anim=TRUE)
    }
    
    else if (input$select_table== 6) {
      table$label <- "Barchart Top cancer by sexes"
      show(id="controls_COL1", anim=TRUE)
      show(id="controls_COL2", anim=TRUE)
    }

  })
  
  observeEvent(values$nb_slide,{
    
    
    
    if (values$nb_slide==0) {
      hide(id="downloadPres", anim=TRUE)
      hide(id="pptx_filename", anim=TRUE)
      hide(id="UI_nbSlide", anim=TRUE)
    } else {
      show(id="downloadPres", anim=TRUE)
      show(id="pptx_filename", anim=TRUE)
      show(id="UI_nbSlide", anim=TRUE)
    } 
    
  })
  
  observeEvent(input$radioCancer , {
    
    if (input$radioCancer == "ci5") {
      fileCSV <- "color_cancer_ci5.csv"
    } else {
      fileCSV <- "color_cancer_globocan.csv"
    }
    
    dt_list <- read.csv(paste0(file_data, fileCSV))
    
    cancer_list <- dt_list$cancer_lab
    n <- length(cancer_list)
    cancer_list <- as.character(cancer_list)
    if (isolate(input$radioCancer == "ci5")) {
      cancer_list <- c(cancer_list[n], cancer_list[n-1], cancer_list[1:(n-2)])
    } else {
      cancer_list <- c(cancer_list[n], cancer_list[1:(n-1)])
    }
    
    
    cancer_list <- as.list(cancer_list)
    updateSelectInput(session, "selectCancerSite", "Select cancer sites", cancer_list)
    
    
  })
  

  observeEvent(input$radioValue , {
    
    
    if (input$radioValue == "cum") {
      vals <- 75
    } else {
      vals <- 90
    }
    
    # If the slide range value are not update, the trigger1 is turn on, so the graph will be update
    bool_rv$trigger1 = input$slideAgeRange[2] == vals
    updateSliderInput(session, "slideAgeRange", "Age group:", value=c(0,90), min=0, max=vals,step=5)
    
  })
  
  observeEvent(input$radioCancer , {
    
    # to avoid radio value to be reset each time
    cancer_group$select <- input$radioCancer
    
  })
  
  
  #Data management
  
  #Select CI5
  dt_CI5 <- reactive ({

  	if (input$check_country) {
  	  var_code <- "country_code"
  	  var_label <- "country_label"
  	  fileRDS <- "CI5XI_country.rds"
  	} else {
  	  var_code <- "registry"
  	  var_label <- "registry_lab"
  	  fileRDS <- "CI5XI.rds"
  	  
  	}
    
    isolate(progress_bar$object)$set(value = 10, message = 'Please wait:', detail = 'Load CI5 XI data')
  	
  	dt_temp <- data.table(readRDS(paste0(file_data, fileRDS)))
  	
  	setnames(dt_temp,var_code,"registry"  )
  	setnames(dt_temp,var_label,"registry_lab"  )
  	
  	#sex specific sites
  	
  	dt_temp[sex == 1 & cancer %in% 29:36, cases := 0]
  	dt_temp[sex == 2 & cancer %in% 38:41, cases := 0]
  	
  	return(dt_temp)

  })
  
  
  #select registry
  dt_select <- reactive({ 
    

    if (!is.null(input$select_registry)) {
      
      if (input$select_registry %in% dt_CI5()$registry) {
        
        isolate(progress_bar$object)$set(value = 20, message = 'Please wait:', detail = 'Select registry or country')
        
        dt_temp <- dt_CI5()
        
        dt_temp<- dt_temp[registry == input$select_registry] #for test
       
        registry_info$label <- isolate(registry_info$data)[registry==input$select_registry,]$registry_lab
        
        #drop population for missing age
        dt_temp <- dt_temp[age==19, py:=0]
        
        #create age group label
        dt_temp[py>0, age_group_label:=paste0((age-1)*5, "-", (age*5)-1)]
        
        
        dt_temp[py>0 ,temp_max:=max(age)]
        dt_temp[age==temp_max ,age_group_label:=paste0((age-1)*5, "+")]
        
        max_age <- max(dt_temp[py >0,]$age)
        
        
        return(list(data=dt_temp, max_age=max_age))
      }
    }
    else {
      return(NULL)
    }

  })
  
  #Calcul statistics
  dt_all <-  reactive({ 
    
    
    if (!is.null(dt_select())) {
	    
      isolate(progress_bar$object)$set(value = 30, message = 'Please wait:', detail = 'Calculate statistics')
      
      if (input$select_table==1) {
        
        dt_temp <-  dt_select()$data[, c("age", "sex", "py", "age_group_label"), with=FALSE]
        dt_temp <- unique(dt_temp)
        dt_temp <- dt_temp[py>0,]
        dt_temp[,Total:=sum(py)]
        dt_temp[,Percent:=py/sum(py)*100, by=sex]
        dt_temp[,Percent:=round(Percent,1)]
        dt_temp$sex <- factor(dt_temp$sex, levels=c(1,2), labels=c("Male", "Female"))
        dt_temp$age <- dt_temp$age-1
        
      }  
      else if (input$select_table==2) {
        
        
        if (!is.null(input$radioAgeGroup)) {
          
          age_group <- seq(0,85,5)
          if (input$radioAgeGroup == 2) {
            age_group <- c(15,30,50,70)
          }
        
        
        skin <- FALSE
        
        
        #drop missing age value
        dt_temp <- dt_select()$data[age != 19,]
        dt_temp[, first:= as.numeric(substring(age_group_label,0,regexpr("[^[:alnum:]]",age_group_label)-1)) ]
        dt_temp[, last:= as.numeric(substring(age_group_label,regexpr("[^[:alnum:]]",age_group_label)+1)) ]
        dt_temp[is.na(last), last:= 1000L ]
        dt_temp[, age_cut:=findInterval(last,age_group)]
        dt_temp[, first:=min(first, na.rm=TRUE), by=age_cut]
        dt_temp[, last:=max(last), by=age_cut]
        dt_temp[, group_label:=paste0(as.character(first),"-",as.character(last))]
        dt_temp[, group_label:= gsub("-1000", "+", group_label)]
        
        #drop skin if no ALL but skin
        if (!skin) {
          dt_temp <- dt_temp[cancer == 63,]
        } else {
          dt_temp <- dt_temp[cancer == 62,]
        }
        
        
        dt_temp <- dt_temp[,list(cases=sum(cases)), by=c("sex", "group_label", "age_cut")]
        
        #add sex label
        dt_temp$sex <- factor(dt_temp$sex, levels=c(1,2), labels=c("Male", "Female"))
        
        } else {
          dt_temp <- NULL
        }
        
        
      } 
      else if (input$select_table==3) {
        
        if (!is.null(input$slideAgeRange)) {
            
            
           
          dt_temp <- dt_select()$data
          
          if (input$radioCancer == "ci5") {
            
            dt_temp <- dt_temp[cancer != 63,]
            dt_temp <- dt_temp[cancer != 62,]
            dt_temp <- dt_temp[cancer != 25,]
            
          } else {
            
            dt_temp <- merge(dt_temp, dt_globo_dict, by=c("cancer", "cancer_lab"))
            dt_temp <- dt_temp[globocan_code != 99,]
            dt_temp <- dt_temp[,cancer := NULL]
            dt_temp <- dt_temp[,cancer_lab := NULL]
            setnames(dt_temp, "globocan_code", "cancer")
            setnames(dt_temp, "globocan_label", "cancer_lab")
            
            dt_temp <- dt_temp[cancer != 29,]
            
            group_by <- c("cancer_lab","cancer", "age","age_group_label", "sex")
            dt_temp <-  dt_temp[,list(cases=sum(cases), py=mean(py)), by=group_by]
              
            
          }
            
         
          dt_temp$sex <- factor(dt_temp$sex, levels=c(1,2), labels=c("Male", "Female"))
          dt_temp[, cancer :=factor(cancer)]
          
          first_age <- (input$slideAgeRange[1]/5)+1
          last_age <- input$slideAgeRange[2]/5
          
          if (last_age >= dt_select()$max_age) last_age <- 18
          
          if (isolate(input$radioValue) == "cum") {
            
            if (last_age > 15) last_age <-15
            
            dt_temp <- csu_cum_risk_core(df_data =dt_temp,
                                         var_age="age", var_cases="cases", var_py="py",
                                         group_by = c("cancer", "cancer_lab", "sex"), 
                                         missing_age = 19,
                                         age_label_list = NULL,
                                         last_age= last_age)

          } 
          else {
          
            dt_temp <- Rcan:::core.csu_asr(df_data =dt_temp,
                               var_age="age", var_cases="cases", var_py="py",
                               group_by = c("cancer", "cancer_lab", "sex"), 
                               first_age = first_age,
                               last_age= last_age,
                               missing_age = 19)
            
          }
          
       
          
          if (bool_rv$trigger1) {
            bool_rv$trigger1 <- FALSE
          }

        }
        else {
          dt_temp <- NULL
        }
       
      }
      else if (input$select_table==4) {
        
        dt_temp <- dt_select()$data
        
        if (input$radioCancer == "ci5") {
          
          dt_temp <- dt_temp[cancer != 63,]
          dt_temp <- dt_temp[cancer != 62,]
          dt_temp <- dt_temp[cancer != 25,]
          
        } else {
          
          dt_temp <- merge(dt_temp, dt_globo_dict, by=c("cancer", "cancer_lab"))
          dt_temp <- dt_temp[globocan_code != 99,]
          dt_temp <- dt_temp[,cancer := NULL]
          dt_temp <- dt_temp[,cancer_lab := NULL]
          setnames(dt_temp, "globocan_code", "cancer")
          setnames(dt_temp, "globocan_label", "cancer_lab")

          dt_temp <- dt_temp[cancer != 29,]
          group_by <- c("cancer_lab","cancer", "age","age_group_label", "sex")
          dt_temp <-  dt_temp[,list(cases=sum(cases), py=mean(py)), by=group_by]
          
          
        }
       
        
        dt_temp$sex <- factor(dt_temp$sex, levels=c(1,2), labels=c("Male", "Female"))
        
        
        if (isolate(input$radioCancer) == "ci5") {
          dt_color <- dt_cancer_color_ci5
        } else {
          dt_color <- dt_cancer_color_globo
        }
        
        
        
          dt_temp <- merge(dt_temp, dt_color, by=c("cancer_lab", "cancer"))
        
      }

      else if (input$select_table==5) {
        
        dt_temp <- dt_select()$data
       
        
        if (isolate(input$radioCancer == "globocan")) {
          
          dt_temp <- merge(dt_temp, dt_globo_dict, by=c("cancer", "cancer_lab"))
          dt_temp <- dt_temp[globocan_code != 99,]
          dt_temp <- dt_temp[,cancer := NULL]
          dt_temp <- dt_temp[,cancer_lab := NULL]
          setnames(dt_temp, "globocan_code", "cancer")
          setnames(dt_temp, "globocan_label", "cancer_lab")
          group_by <- c("cancer_lab","cancer", "age","age_group_label", "sex")
          dt_temp <-  dt_temp[,list(cases=sum(cases), py=mean(py)), by=group_by]
          
        }
        
        dt_temp <- dt_temp[cancer_lab == input$selectCancerSite,]
        
        dt_temp$sex <- factor(dt_temp$sex, levels=c(1,2), labels=c("Male", "Female"))
        

      }  
      else if (input$select_table==6) {
        
        if (!is.null(input$slideAgeRange)) {
          
          dt_temp <- dt_select()$data
          
          if (input$radioCancer == "ci5") {
            
            dt_temp <- dt_temp[cancer != 63,]
            dt_temp <- dt_temp[cancer != 62,]
            dt_temp <- dt_temp[cancer != 25,]
            
          }
          else {
            
            dt_temp <- merge(dt_temp, dt_globo_dict, by=c("cancer", "cancer_lab"))
            dt_temp <- dt_temp[globocan_code != 99,]
            dt_temp <- dt_temp[,cancer := NULL]
            dt_temp <- dt_temp[,cancer_lab := NULL]
            setnames(dt_temp, "globocan_code", "cancer")
            setnames(dt_temp, "globocan_label", "cancer_lab")
            
            dt_temp <- dt_temp[cancer != 29,]
            
            group_by <- c("cancer_lab","cancer", "age","age_group_label", "sex")
            dt_temp <-  dt_temp[,list(cases=sum(cases), py=mean(py)), by=group_by]
            
            
          }
          
          
          dt_temp$sex <- factor(dt_temp$sex, levels=c(1,2), labels=c("Male", "Female"))
          dt_temp[, cancer :=factor(cancer)]
          
          first_age <- (input$slideAgeRange[1]/5)+1
          last_age <- input$slideAgeRange[2]/5
          
          if (last_age >= dt_select()$max_age) last_age <- 18
          
          if (isolate(input$radioValue) == "cum") {
            
            if (last_age > 15) last_age <-15
            
            dt_temp <- csu_cum_risk_core(df_data =dt_temp,
                                         var_age="age", var_cases="cases", var_py="py",
                                         group_by = c("cancer", "cancer_lab", "sex"), 
                                         missing_age = 19,
                                         age_label_list = NULL,
                                         last_age= last_age)
            
          } 
          else {
            
            dt_temp <- Rcan:::core.csu_asr(df_data =dt_temp,
                                           var_age="age", var_cases="cases", var_py="py",
                                           group_by = c("cancer", "cancer_lab", "sex"), 
                                           first_age = first_age,
                                           last_age= last_age,
                                           missing_age = 19)
            
          }
          
          
          if (bool_rv$trigger1) {
            bool_rv$trigger1 <- FALSE
          }
          
        }
        else {
          dt_temp <- NULL
        }
        
      }
      
      
      return(dt_temp)
      
	}
    else {
      return(NULL)
    }
  })
  

  #Render plot
  output$plot <- renderPlot({ 
    
    progress_bar$object <- Progress$new(session, min=0, max=100)
    on.exit(progress_bar$object$close())

    if (!is.null(dt_all()))  {
      
      isolate(progress_bar$object)$set(value = 60,  message = 'Please wait:', detail = 'Render graph')
      
      
      if (isolate(input$select_table)==1) {
        

        canreg_population_pyramid(dt_all(), var_bar = "age_group_label",group_by = "sex",var_age_cut="age",canreg_header = isolate(registry_info$label))
        
      } 
      else if (isolate(input$select_table)==2) {
        
        skin <- FALSE
        
        
        canreg_cases_age_bar(
          df_data=dt_all(),
          var_cases = "cases",
          group_by = "sex",
          color_bar=c("Male" = "#2c7bb6", "Female" = "#b62ca1"),
          canreg_header = isolate(registry_info$label),
          skin=skin)
        
      } 
      else if (isolate(input$select_table)==3) {
        
        
        nb_top <- input$slideNbTopBar
        
        if (!is.null(nb_top)) {
          
          last_age <- (isolate(input$slideAgeRange)[2]/5)
          max_age <- dt_select()$max_age
          
          if (last_age < max_age) {
            age2 <- isolate(input$slideAgeRange)[2]-1
          } else {
            age2 <- paste0(((max_age-1)*5), "+")
          }
          

          if (isolate(input$radioValue) == "asr") {
            var_top <- "asr"
            digit <- 1
            ytitle <- paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","), ", ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
            
          
          } 
          else if (isolate(input$radioValue) == "cases"){
            var_top <- "cases"
            digit <- 0
            ytitle <-  paste0("Number of cases, ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
            
            
          }
          else if (isolate(input$radioValue) == "cum") {
            var_top <- "cum_risk"
            digit <- 2
            if (last_age >= 15) {
              age2 <- 74
            } else {
              age2 <- isolate(input$slideAgeRange)[2]-1
            }
            ytitle<-paste0("Cumulative incidence risk (percent), 0-",age2, " years old" )

            
          }
          


          canreg_bar_top(df_data=dt_all(),
                         var_top = var_top,
                         var_bar = "cancer_lab",
                         group_by = "sex",
                         color_bar=c("Male" = "#2c7bb6", "Female" = "#b62ca1"), nb_top = nb_top,nsmall = digit,
                         canreg_header  = isolate(registry_info$label),
                         ytitle=ytitle
          )
        }
        
      }
      else if (isolate(input$select_table)==4) {
       


        if (!is.null(input$radioLog )) {
          logscale <- (input$radioLog == "log")
          
          nb_top <- input$slideNbTopAgeSpe
          
          if (!is.null(nb_top)) {
    
          
            temp <- Rcan:::core.csu_ageSpecific_top(
              df_data=dt_all(),
              var_age="age",
              var_cases= "cases",
              var_py= "py",
              var_top = "cancer_lab",
              group_by="sex",
              missing_age=NULL,
              var_color="cancer_color",
              logscale = logscale,
              nb_top = nb_top,
              plot_title = isolate(registry_info$label)
            )
            
            
            temp$plotlist[[1]] <- temp$plotlist[[1]]+guides(color = guide_legend(override.aes = list(size=1), nrow=2,byrow=TRUE))
            temp$plotlist[[2]] <- temp$plotlist[[2]]+guides(color = guide_legend(override.aes = list(size=1), nrow=2,byrow=TRUE))
    
            
      
            grid.arrange(temp$plotlist[[1]], temp$plotlist[[2]], ncol=2)
          }
        
        }
        
      }
      else if (isolate(input$select_table)==5) {
        
         
        
        
          dt_temp <- dt_all()
         
          
          if (!is.null(input$radioLog )) {
            logscale <- (input$radioLog == "log")
            color_trend <- c("Male" = "#2c7bb6", "Female" = "#b62ca1")
            
            Rcan:::core.csu_ageSpecific(dt_temp,
                                        var_age="age",
                                        var_cases= "cases",
                                        var_py="py",
                                        group_by = "sex",
                                        plot_title = isolate(registry_info$label),
                                        plot_subtitle = isolate(input$selectCancerSite),
                                        color_trend = color_trend,
                                        logscale = logscale,
                                        age_label_list = unique(dt_temp[["age_label_list"]]))$csu_plot
          
            
          
          
          }
          
         
          
       
        
      }
      else if (isolate(input$select_table)==6) {
        
        
        nb_top <- input$slideNbTopBar
        
        if (!is.null(nb_top)) {
          
          last_age <- (isolate(input$slideAgeRange)[2]/5)
          max_age <- dt_select()$max_age
          
          if (last_age < max_age) {
            age2 <- isolate(input$slideAgeRange)[2]-1
          } else {
            age2 <- paste0(((max_age-1)*5), "+")
          }
          
          
          if (isolate(input$radioValue) == "asr") {
            var_top <- "asr"
            digit <- 1
            ytitle <- paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","), ", ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
            
            
          } 
          else if (isolate(input$radioValue) == "cases"){
            var_top <- "cases"
            digit <- 0
            ytitle <-  paste0("Number of cases, ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
            
            
          }
          else if (isolate(input$radioValue) == "cum") {
            var_top <- "cum_risk"
            digit <- 2
            if (last_age >= 15) {
              age2 <- 74
            } else {
              age2 <- isolate(input$slideAgeRange)[2]-1
            }
            ytitle<-paste0("Cumulative incidence risk (percent), 0-",age2, " years old" )
            
            
          }
          
          
          dt_temp <- Rcan:::core.csu_dt_rank(dt_all(),
                                             var_value = var_top,
                                             var_rank = "cancer_lab",
                                             group_by = "sex",
                                             number = nb_top)
          
          if (isolate(input$radioCancer) == "ci5") {
            dt_color <- dt_cancer_color_ci5
          } 
          else {
            dt_color <- dt_cancer_color_globo
          }
          dt_temp <- merge(dt_temp, dt_color, by=c("cancer_lab"))
          
          
          dt_temp$cancer_lab <-Rcan:::core.csu_legend_wrapper(dt_temp$cancer_lab, 15)
          
          #Use lapply to avoid loop (problem with ggplot lazy evaluation)
          plotlist <- lapply(levels(dt_temp$sex), function (i) {
            
            plot_title <- isolate(registry_info$label)
            plot_caption <- ""
            plot_subtitle <-  paste0("Top ",nb_top, " cancer sites\n",i)
            dt_plot <- dt_temp[sex == i]
            dt_label_order <- setkey(unique(dt_plot[, c("cancer_lab","cancer_color", "CSU_RANK"), with=FALSE]), CSU_RANK)
            dt_plot$cancer_lab <- factor(dt_plot$cancer_lab,levels = rev(dt_label_order$cancer_lab)) 
            color_cancer <- as.character(rev(dt_label_order$cancer_color))
            
            return(
              csu_bar_plot(
                dt_plot,var_top=var_top,var_bar="cancer_lab",
                plot_title=plot_title,plot_caption=plot_caption,plot_subtitle = plot_subtitle,
                color_bar=color_cancer,
                landscape=FALSE,digit=digit,
                xtitle=ytitle)
              )
            
          })

          grid.arrange( plotlist[[1]], plotlist[[2]], ncol=2)
          
        }
        
      }

    }
    
  })
  
  #Download file
  output$downloadFile <- downloadHandler(
    
    filename = function() {
      #cat(paste0(input$text_filename, ".", input$select_format))
      
      #multiple file
      if (input$select_table %in% c(4,6) & 
          input$select_format %in% c("png", "tiff", "svg")
          ) 
        {
        paste0(input$text_filename, ".", "zip")
      } 
      else {
        paste0(input$text_filename, ".", input$select_format)
      }
      
    },
    
    content = function(file) {
      
      file_temp <- substr(file,1, nchar(file)-nchar(input$select_format)-1)


      if (input$select_table==1) {
        
        #population pyramid
    
        canreg_output(output_type = input$select_format, filename =file_temp,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=canreg_population_pyramid,
                      df_data=dt_all(),
                      var_bar = "age_group_label",
                      group_by = "sex",
                      var_age_cut="age",
                      canreg_header = isolate(registry_info$label))
        
      } 
      else if (input$select_table==2) {
        
        skin <- FALSE
        
        
        
        canreg_output(output_type = input$select_format, filename =file_temp,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=canreg_cases_age_bar,
                      df_data=dt_all(),
                      var_cases = "cases",
                      group_by = "sex",
                      color_bar=c("Male" = "#2c7bb6", "Female" = "#b62ca1"),
                      canreg_header = isolate(registry_info$label),
                      skin=skin)

      }
      else if (input$select_table==3) {
        
        nb_top <- input$slideNbTopBar
        
        if (!is.null(nb_top)) {
          
          last_age <- (input$slideAgeRange[2]/5)
          max_age <- dt_select()$max_age
          
          if (last_age < max_age) {
            age2 <- input$slideAgeRange[2]-1
          } else {
            age2 <- paste0(((max_age-1)*5), "+")
          }
          
          
          if (input$radioValue == "asr") {
            var_top <- "asr"
            digit <- 1
            ytitle <- paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","), ", ", input$slideAgeRange[1], "-", age2, " years old" )
            
            
          } 
          else if (input$radioValue == "cases"){
            var_top <- "cases"
            digit <- 0
            ytitle <-  paste0("Number of cases, ", input$slideAgeRange[1], "-", age2, " years old" )
            
            
          }
          else if (input$radioValue == "cum") {
            var_top <- "cum_risk"
            digit <- 2
            if (last_age >= 15) {
              age2 <- 74
            } else {
              age2 <- input$slideAgeRange[2]-1
            }
            ytitle<-paste0("Cumulative incidence risk (percent), 0-",age2, " years old" )
            
            
          }
          
        

        canreg_output(output_type = input$select_format, filename =file_temp,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=canreg_bar_top,
                        df_data=dt_all(),
                        var_top = var_top,
                        var_bar = "cancer_lab",
                        group_by = "sex",
                        color_bar=c("Male" = "#2c7bb6", "Female" = "#b62ca1"), nb_top = nb_top,nsmall=digit,
                        canreg_header  = isolate(registry_info$label),
                        ytitle=ytitle
        )
        }
        
      }
      else if (input$select_table==4) {
        

        logscale <- (input$radioLog == "log")
        nb_top <- input$slideNbTopAgeSpe
        canreg_output(output_type = input$select_format, filename =file_temp,
                      landscape = FALSE,list_graph = TRUE,
                      FUN=canreg_ageSpecific_rate_top,
                        df_data=dt_all(),
                        var_age="age",
                        var_cases= "cases", 
                        var_py= "py",
                        group_by="sex",
                        var_top = "cancer_lab",
                        var_age_label_list = "age_group_label",
                        var_color="cancer_color",
                        logscale = logscale,
                        nb_top = nb_top,
                        plot_title = isolate(registry_info$label))
        
        if (input$select_format %in% c("png", "tiff", "svg")) {
          
          file_male <- paste0(file_temp, "001", ".", input$select_format)
          file_female <- paste0(file_temp, "002", ".", input$select_format)
          
          tempfile1 <- paste0(input$text_filename, "-male.", input$select_format)
          tempfile2 <- paste0(input$text_filename, "-female.", input$select_format)
          
          file.copy(file_male, tempfile1, overwrite = TRUE)
          file.copy(file_female,tempfile2, overwrite = TRUE)
        
          zip(file, c(tempfile1, tempfile2))
          
          file.remove(c(tempfile1,tempfile2))
          
          

          
        }
        
        
      }
      else if (input$select_table==5) {
        
        dt_temp <- dt_all()
        
        logscale <- (input$radioLog == "log")
        color_trend <- c("Male" = "#2c7bb6", "Female" = "#b62ca1")
        
        #create a fake function to print the graph and return data (adapt to canreg_output function)
        temp_fun <- function(landscape = TRUE,list_graph = FALSE, return_data = FALSE) {
          
          if (return_data) {
            
            dt_temp <- dt_temp[py > 0,]
            dt_temp[, rate := cases/py*10000]
            dt_temp[, age_group_label := paste0("'",age_group_label,"'")]
            dt_temp <- dt_temp[, c("cancer_lab",
                        "age",
                        "age_group_label",
                        "sex",
                        "cases",
                        "py",
                        "rate"), with=FALSE]
            setkeyv(dt_temp, c("sex","age"))
            return(dt_temp)
            stop() 
            
          } 
            
          plot<- Rcan:::core.csu_ageSpecific(dt_temp,
                                             var_age="age",
                                             var_cases= "cases",
                                             var_py="py",
                                             group_by = "sex",
                                             plot_title = isolate(registry_info$label),
                                             plot_subtitle = isolate(input$selectCancerSite),
                                             color_trend = color_trend,
                                             logscale = logscale,
                                             age_label_list = unique(dt_temp[["age_label_list"]]))$csu_plot
          print(plot)

        }
        
        canreg_output(output_type = input$select_format, filename =file_temp,
                      landscape = FALSE,list_graph = FALSE,
                      FUN=temp_fun)
    
      }
      else if (input$select_table==6) {
        
        dt_temp <- dt_all()
        
        nb_top <- input$slideNbTopBar
        
        last_age <- (isolate(input$slideAgeRange)[2]/5)
        max_age <- dt_select()$max_age
        
        if (last_age < max_age) {
          age2 <- isolate(input$slideAgeRange)[2]-1
        } else {
          age2 <- paste0(((max_age-1)*5), "+")
        }
        
        
        if (isolate(input$radioValue) == "asr") {
          var_top <- "asr"
          digit <- 1
          ytitle <- paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","), ", ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
          
          
        } 
        else if (isolate(input$radioValue) == "cases"){
          var_top <- "cases"
          digit <- 0
          ytitle <-  paste0("Number of cases, ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
          
          
        }
        else if (isolate(input$radioValue) == "cum") {
          var_top <- "cum_risk"
          digit <- 2
          if (last_age >= 15) {
            age2 <- 74
          } else {
            age2 <- isolate(input$slideAgeRange)[2]-1
          }
          ytitle<-paste0("Cumulative incidence risk (percent), 0-",age2, " years old" )
          
          
        }
        
        if (isolate(input$radioCancer) == "ci5") {
          dt_color <- dt_cancer_color_ci5
        } 
        else {
          dt_color <- dt_cancer_color_globo
        }
        
        dt_color$cancer <- as.factor(dt_color$cancer)
        dt_temp <- merge(dt_temp, dt_color, by=c("cancer_lab", "cancer"))
        
        canreg_output(output_type = input$select_format, filename =file_temp,
                      landscape = FALSE,list_graph = TRUE,
                      FUN=canreg_bar_top_single,
                      dt=dt_temp,var_top=var_top, var_bar = "cancer_lab" ,group_by = "sex",
                      var_color = "cancer_color",
                      nb_top = nb_top,
                      canreg_header = isolate(registry_info$label),
                      digit=digit,
                      xtitle=ytitle)
        
        if (input$select_format %in% c("png", "tiff", "svg")) {
          
          file_male <- paste0(file_temp, "001", ".", input$select_format)
          file_female <- paste0(file_temp, "002", ".", input$select_format)
          
          tempfile1 <- paste0(input$text_filename, "-male.", input$select_format)
          tempfile2 <- paste0(input$text_filename, "-female.", input$select_format)
          
          file.copy(file_male, tempfile1, overwrite = TRUE)
          file.copy(file_female,tempfile2, overwrite = TRUE)
          
          zip(file, c(tempfile1, tempfile2))
          
          file.remove(c(tempfile1,tempfile2))

        }
      
      }
    })
  
  #Action button: Add slide
  observeEvent(
    input$actionSlide,
    { 
      
      
      if (values$nb_slide == 0) {
        
		withProgress(message = 'create powerpoint', value = 0, {
        values$doc <- read_pptx(file_pptx)
        })
      }
      
	  withProgress(message = 'add powerpoint slide', value = 0, {
      filename <- paste0(tempdir(), "\\temp_graph",values$nb_slide+1)  
      
      if (input$select_table==1) {
        
        
        
        canreg_output(output_type = "png", filename =filename,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=canreg_population_pyramid,
                      dt_all(), var_bar = "age_group_label",group_by = "sex",var_age_cut="age",canreg_header = isolate(registry_info$label))
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        dims <- attr( png::readPNG (paste0(filename, ".png")), "dim" )
        values$doc <- ph_with_img(values$doc, paste0(filename, ".png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
      } 
      else if (input$select_table==2) {
        
        skin <- FALSE
        
        
        
        canreg_output(output_type = "png", filename =filename,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=canreg_cases_age_bar,
                      df_data=dt_all(),
                      var_cases = "cases",
                      group_by = "sex",
                      color_bar=c("Male" = "#2c7bb6", "Female" = "#b62ca1"),
                      canreg_header = isolate(registry_info$label),
                      skin=skin)
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        dims <- attr( png::readPNG (paste0(filename, ".png")), "dim" )
        values$doc <- ph_with_img(values$doc, paste0(filename, ".png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
      } 
      else if (input$select_table==3) {
        
        nb_top <- input$slideNbTopBar
        
        
        
        last_age <- (input$slideAgeRange[2]/5)
        max_age <- dt_select()$max_age
        
        if (last_age < max_age) {
          age2 <- input$slideAgeRange[2]-1
        } else {
          age2 <- paste0(((max_age-1)*5), "+")
        }
        
        
        
        if (input$radioValue == "asr") {
          var_top <- "asr"
          digit <- 1
          ytitle <- paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","), ", ", input$slideAgeRange[1], "-", age2, " years old" )
          
          
        } 
        else if (input$radioValue == "cases"){
          var_top <- "cases"
          digit <- 0
          ytitle <-  paste0("Number of cases, ", input$slideAgeRange[1], "-", age2, " years old" )
          
          
        }
        else if (input$radioValue == "cum") {
          var_top <- "cum_risk"
          digit <- 2
          if (last_age >= 15) {
            age2 <- 74
          } else {
            age2 <- input$slideAgeRange[2]-1
          }
          ytitle<-paste0("Cumulative incidence risk (percent), 0-",age2, " years old" )
          
          
        }
        
        
        
        canreg_output(output_type = "png", filename =filename,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=canreg_bar_top,
                      df_data=dt_all(),
                      var_top = var_top,
                      var_bar = "cancer_lab",
                      group_by = "sex",
                      color_bar=c("Male" = "#2c7bb6", "Female" = "#b62ca1"), nb_top = nb_top,nsmall = digit,
                      canreg_header  = isolate(registry_info$label),
                      ytitle=ytitle
        )
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        dims <- attr( png::readPNG (paste0(filename, ".png")), "dim" )
        values$doc <- ph_with_img(values$doc, paste0(filename, ".png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
        
      }
      else if (input$select_table==4) {
        
        
        logscale <- (input$radioLog == "log")
        nb_top <- input$slideNbTopAgeSpe
        
        canreg_output(output_type = "png", filename =filename,
                      landscape = TRUE,list_graph = TRUE,
                      FUN=canreg_ageSpecific_rate_top,
                      df_data=dt_all(),
                      var_age="age",
                      var_cases= "cases", 
                      var_py= "py",
                      group_by="sex",
                      var_top = "cancer_lab",
                      var_age_label_list = "age_group_label",
                      var_color="cancer_color",
                      logscale = logscale,
                      nb_top = nb_top,
                      plot_title =  isolate(registry_info$label))
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        #values$doc <- ph_with_text(values$doc, isolate(registry_info$label), type="title")
        dims <- attr( png::readPNG (paste0(filename, "001.png")), "dim" )
        values$doc <- ph_with_img(values$doc, paste0(filename, "001.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        #values$doc <- ph_with_text(values$doc, isolate(registry_info$label), type="title")
        values$doc <- ph_with_img(values$doc, paste0(filename, "002.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
        
        
      }
      else if (input$select_table==5) {
        
        dt_temp <- dt_all()
        
        logscale <- (input$radioLog == "log")
        color_trend <- c("Male" = "#2c7bb6", "Female" = "#b62ca1")
        
        #create a fake function to print the graph and return data (adapt to canreg_output function)
        temp_fun <- function(landscape = TRUE,list_graph = FALSE, return_data = FALSE) {
          
          if (return_data) {
            
            dt_temp[, rate := cases/py*10000]
            dt_temp[, age_group_label := paste0("'",age_group_label,"'")]
            dt_temp <- dt_temp[, c("cancer_label",
                                   "age",
                                   "age_group_label",
                                   "sex",
                                   "cases",
                                   "py",
                                   "rate"), with=FALSE]
            setkeyv(dt_temp, c(group_by,var_age))
            return(dt_temp)
            stop() 
            
          } 
          
          plot<- Rcan:::core.csu_ageSpecific(dt_temp,
                                             var_age="age",
                                             var_cases= "cases",
                                             var_py="py",
                                             group_by = "sex",
                                             plot_title = isolate(registry_info$label),
                                             plot_subtitle = isolate(input$selectCancerSite),
                                             color_trend = color_trend,
                                             logscale = logscale,
                                             age_label_list = unique(dt_temp[["age_label_list"]]))$csu_plot
          print(plot)
          
        }
        
        canreg_output(output_type = "png", filename =filename,
                      landscape = TRUE,list_graph = FALSE,
                      FUN=temp_fun)
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        dims <- attr( png::readPNG (paste0(filename, ".png")), "dim" )
        values$doc <- ph_with_img(values$doc, paste0(filename, ".png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
      }
      else if (input$select_table==6) {
        
        dt_temp <- dt_all()
        
        nb_top <- input$slideNbTopBar
        
        last_age <- (isolate(input$slideAgeRange)[2]/5)
        max_age <- dt_select()$max_age
        
        if (last_age < max_age) {
          age2 <- isolate(input$slideAgeRange)[2]-1
        } else {
          age2 <- paste0(((max_age-1)*5), "+")
        }
        
        
        if (isolate(input$radioValue) == "asr") {
          var_top <- "asr"
          digit <- 1
          ytitle <- paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","), ", ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
          
          
        } 
        else if (isolate(input$radioValue) == "cases"){
          var_top <- "cases"
          digit <- 0
          ytitle <-  paste0("Number of cases, ", isolate(input$slideAgeRange)[1], "-", age2, " years old" )
          
          
        }
        else if (isolate(input$radioValue) == "cum") {
          var_top <- "cum_risk"
          digit <- 2
          if (last_age >= 15) {
            age2 <- 74
          } else {
            age2 <- isolate(input$slideAgeRange)[2]-1
          }
          ytitle<-paste0("Cumulative incidence risk (percent), 0-",age2, " years old" )
          
          
        }
        
        if (isolate(input$radioCancer) == "ci5") {
          dt_color <- dt_cancer_color_ci5
        } 
        else {
          dt_color <- dt_cancer_color_globo
        }
        
        dt_color$cancer <- as.factor(dt_color$cancer)
        dt_temp <- merge(dt_temp, dt_color, by=c("cancer_lab", "cancer"))
        
        
        
        
        canreg_output(output_type = "png", filename =filename,
                      landscape = TRUE,list_graph = TRUE,
                      FUN=canreg_bar_top_single,
                      dt=dt_temp,var_top=var_top, var_bar = "cancer_lab" ,group_by = "sex",
                      var_color = "cancer_color",
                      nb_top = nb_top,
                      canreg_header = isolate(registry_info$label),
                      digit=digit,
                      xtitle=ytitle)
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        #values$doc <- ph_with_text(values$doc, isolate(registry_info$label), type="title")
        dims <- attr( png::readPNG (paste0(filename, "001.png")), "dim" )
        values$doc <- ph_with_img(values$doc, paste0(filename, "001.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
        
        values$doc <-  add_slide(values$doc, layout="Canreg_basic", master="Office Theme") ## add PPTX slide (Title + content)
        #values$doc <- ph_with_text(values$doc, isolate(registry_info$label), type="title")
        values$doc <- ph_with_img(values$doc, paste0(filename, "002.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
        
        
      }
      
      values$nb_slide <- values$nb_slide + 1
    
      })
    }
  )
  
  #Download presentation
  output$downloadPres <- downloadHandler(
    filename = function() {
      paste0(input$pptx_filename, ".", "pptx")
      
    },
    content = function(file) {
      invalidateLater(100)
      values$nb_slide <- 0
      print(isolate(values$doc), file) 
     
    }
    
  )
  
})
