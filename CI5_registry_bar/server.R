



#app_folder <- "//inti/CIN/Xchange/Mathieu/CSU_shiny/Shiny"
app_folder <- "c:/CSU_shiny/CI5_registry_bar"

source(paste(sep="/", app_folder, "source/Rcan_core.r"))




shinyServer(function(input, output) {
  
  output$continentControls <- renderUI({
    
    continent_list <- c( "Africa","South America","North America", "Asia", "Europe","Oceania")
    
    if (input$check_country) {

      continent_list <- c("All",continent_list)
    }
    
    selected_continent <- NULL
    if (length(input$continent) != 0) {
      if (input$continent != "All") {
      
        selected_continent <-  input$continent 
      }
    } 
      
    
    selectInput("continent", "Continent",as.list(continent_list), selected = selected_continent)
    
  })
  
  output$filenameControls <- renderUI({
    
    filename <- "CI5XI"
    
    if (input$check_country) {
      
      filename <- paste(filename, "country", sep = "_")
      
    } else {
      
      filename <- paste(filename, "registry", sep = "_")
      
    }
    
    if (length(input$continent) != 0) {
      if (input$continent != "All") {
        continent <- input$continent
        
        if (input$continent == "North America") {
          continent <- "NAmerica"
        } else if  (input$continent == "South America") {
          continent <- "SAmerica"
        }
        
        filename <- paste(filename, continent, sep = "_")
        
      }
    }
      
    
    if (input$bar_value == "ASR") {
      
      filename <- paste(filename, "asr", sep = "_")
      
    } else if (input$bar_value == "Cumulative risk") {
      
      filename <- paste(filename, "cumu", sep = "_")
      
    } else {
      
      filename <- paste(filename, "case", sep = "_")
      
    }
    
    if (input$sex == "Male") {
      filename <- paste(filename, "male", sep = "_")
    } else {
      filename <- paste(filename, "female", sep = "_")
      
    }
    
    
    if (input$color_cancer) {
      filename <- paste(filename, "color", sep = "_")
    }
    
    textInput("filename", "Name of the pptx", filename)
    
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".pptx")
    },
    content = function(file) {
      
      #to test
      # input <- list()
      # input$continent <- "Asia"
      # input$sex <- "Female"
      
      #dt_select<- dt_CI5[sex == 1]
      
      withProgress(message = 'loading package', value = 0, {
        incProgress(0, detail="data.table")
        library(data.table)
        incProgress(1/4, detail = "Rcan")
        library(Rcan)
        incProgress(1/4, detail="ReporteRs")
        library(ReporteRs)
        incProgress(1/4, detail="ggplot2")
        library(ggplot2)
        incProgress(1/4, detail="done")
      
      })

      
      withProgress(message = 'loading CI5 XI data', value = 0, {
        
        var_group <- "registry_lab"
        fileRDS <- "CI5XI.rds"
        
        if (input$check_country) {
          var_group <- "country_label"
          fileRDS <- "CI5XI_country.rds"
        }
          
        dt_CI5 <- readRDS(paste0(app_folder, "/data/", fileRDS))
        
        if (!input$check_country) {
          dt_CI5$regional <- 0
        }
        
        dt_continent <- read.csv(paste0(app_folder, "/data/continent_lab.csv"))
        dt_cancer_color <- read.csv(paste0(app_folder, "/data/color_cancer.csv"))
        dt_CI5 <- merge(dt_CI5, dt_continent, by="CI5_continent")
        dt_CI5 <- data.table(dt_CI5)
        dt_cancer_color <- data.table(dt_cancer_color)
     
        dt_select <- dt_CI5

        
        if (input$continent != "All") {
          dt_select <- dt_select[continent_lab == input$continent]
        }
        

        
        
        if (input$sex == "Male") {
          dt_select <- dt_select[sex == 1]
          color_temp <- "#2c7bb6"
        } else {
          dt_select <- dt_select[sex == 2]
          color_temp <- "#b62ca1"
        }
      
        # drop all site and other skin
        dt_select <- dt_select[cancer <62]
        dt_select <- dt_select[cancer != 25]
        
        
      incProgress(1/2, message=paste0("Calculated ", input$bar_value))
        
        if (input$bar_value == "ASR") {
          # asr
          dt_temp <- data.table(csu_asr(dt_select, group_by=c(var_group, "cancer", "cancer_lab", "regional", "CI5_continent"), missing_age = 19))
          var_top <- "asr" 
          ndigit <- 1
          xtitle<-paste0("Age-standardized incidence rate per ", formatC(100000, format="d", big.mark=","))
          
        } else if (input$bar_value == "Cumulative risk") {
          # cum risk 
          dt_temp <- data.table(csu_cum_risk_core(df_data = dt_select,var_age ="age",var_cases = "cases", var_py = "py",
                                                  group_by = c(var_group, "cancer", "cancer_lab", "regional", "CI5_continent"),
                                                  missing_age = 19))
          var_top <- "cum_risk"
          ndigit <- 2
          xtitle <-"Cumulative incidence risk (percent), 0-74 years old"
          
          
        } else {
          dt_temp<-  dt_select[,list( cases=sum(cases)), by=c(var_group, "cancer", "cancer_lab", "regional", "CI5_continent")]
          var_top <- "cases"
          ndigit <- 0
          xtitle <- "Number of cases"
        }
        

        
        
        #keep only top 10
        dt_temp <- Rcan:::core.csu_dt_rank(dt_temp, var_value = var_top,
                                           var_rank = "cancer_lab",
                                           group_by = c(var_group),
                                           number = 10)
      incProgress(1, message="Done")
      
      })

      #need to add function canreg_bar_top to rcan!!
      
      csu_ratio = 0.6 
      csu_bar_label_size = 4
      line_size <- 0.4
      text_size <- 14
      var_bar <- "cancer_lab"
      nb_top <- 10
      landscape <- TRUE

      graph_width <- 8
      png_width <- ifelse(landscape, 2339 , 1654 )
      png_height <- ifelse(landscape, 1654 , 2339 )

      plot_subtitle <-  paste0("Top ",nb_top, " cancer sites\n", input$sex)
      

      if (input$color_cancer) {
        dt_temp <- merge(dt_temp, dt_cancer_color, by=c("cancer_lab", "cancer"))
      } else {
        
        dt_temp[, cancer_color:=color_temp]
      }
      
      #Change label to factor
      
      dt_temp[,group_lab:= get(var_group)]

      # rename USA and UK and order levels
      dt_temp[group_lab== "United States of America",group_lab:= "USA"]
      dt_temp[group_lab== "United Kingdom of Great Britain and Northern Ireland",group_lab:= "United Kingdom"]
      
      # order group levels
      setkeyv(dt_temp, c("CI5_continent", "group_lab"))
      group_order <- unique(dt_temp$group_lab)
      dt_temp[,group_lab:= factor(group_lab, levels = group_order)]
      



      withProgress(message = 'create slide', value = 0, {
        
      doc <- pptx(template=paste(sep="/", app_folder,"slide_template", "shiny_template.pptx"))
      
      
        
        n <- length(levels(dt_temp$group_lab))
      
        for (i in levels(dt_temp$group_lab)) {
          
          
          #test graph for 1 registry
          #i <- levels(dt_temp$group_lab)[2]
          
          doc <- addSlide(doc, "Canreg_basic") ## add PPTX slide (Title + content)
          
          dt_plot <- dt_temp[group_lab == i]
          
          plot_title <- unique(dt_plot$group_lab)
          incProgress(0, detail=plot_title)
          plot_caption <- NULL
          
          if (max(dt_plot$regional) > 0) {
            plot_title <- paste0(plot_title, "*")
            plot_caption <- "*: Regional registries"
          }
          
          dt_plot$cancer_lab <-Rcan:::core.csu_legend_wrapper(dt_plot$cancer_lab, 15)
          dt_label_order <- setkey(unique(dt_plot[, c(var_bar,"cancer_color", "CSU_RANK"), with=FALSE]), CSU_RANK)
          dt_plot$cancer_lab <- factor(dt_plot$cancer_lab,levels = rev(dt_label_order$cancer_lab)) 
          color_cancer <- as.character(rev(dt_label_order$cancer_color))
          
          csu_plot <- csu_bar_plot(
            dt_plot,var_top=var_top,var_bar=var_bar,
            plot_title=plot_title,plot_caption=NULL,plot_subtitle = plot_subtitle,
            color_bar=color_cancer,
            landscape=TRUE,digit=ndigit,
            xtitle=xtitle)
          
          filename <- paste0(tempdir(), "\\temp_graph")
          png(paste0(filename,".png"),width = png_width, height = png_height, units = "px",res = 200) 
          print(csu_plot)
          dev.off()
          
          dims <- attr( png::readPNG (paste0(tempdir(), "\\temp_graph.png")), "dim" )
          doc <- addImage(doc, paste0(tempdir(), "\\temp_graph.png"),width=graph_width,height=graph_width*dims[1]/dims[2])
         
          incProgress(1/n, detail=plot_title) 
           
        }
        
        
      })

      writeDoc(doc,file)
    }
  )
  
  
  output$plot <- renderPlot({
    
    
    return()
  
  })
  
})