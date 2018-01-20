shinyUI(pageWithSidebar(
  
  headerPanel("CI5 XI slide generator"),
  
  sidebarPanel(
    uiOutput("continentControls"),
    selectInput("sex", "Sex", as.list(c("Male", "Female"))),
    selectInput("bar_value", "Value", as.list(c("ASR", "Cumulative risk", "Cases"))),
    checkboxInput("color_cancer", "Colored cancer site", value = FALSE, width = NULL),
    checkboxInput("check_country", "group by country", value = TRUE, width = NULL),
    uiOutput("filenameControls"),
    downloadButton('downloadData', 'Top 10 bar chart')
    #downloadButton('downloadData1', 'Top 5 age specific')
   
  ),
  
  
  mainPanel(
    

  )
))