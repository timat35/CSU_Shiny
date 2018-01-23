
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(

  
  dashboardHeader(title = "CI5 XI registry graphics",
                  titleWidth = 350),
  
  dashboardSidebar(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "registry_graph.css"),
    width = 350,
    
    tags$div(class="subHeader", checked=NA,
             tags$p("Graph Selection")
    ),
    
		selectInput("select_continent", "Continent", as.list(c( "Africa","South America","North America", "Asia", "Europe","Oceania"))),
		
		checkboxInput("check_country", "group by country", value = FALSE, width = NULL),
		
		uiOutput("UI_registry"),
		
		selectInput("select_table", "Graphics", 
		            c(
		              "Top cancer" = 3 ,
		              "Age-specific rates (Top Cancer Sites)" = 4,
		              "Age-specific rate by cancer sites" = 5,
		              "Barchart of cases by age" = 2, 
		              "Population pyramid" = 1
		              )
		              
		            ),
		
		tags$div(class="subHeader", checked=NA,
		         tags$p("Export Graph")
		),
		
		selectInput("select_format", "Format", as.list(c( "pdf","tiff","png", "svg", "ps","csv"))),
		
		textInput("text_filename", "Filename", "CI5_graph"),
		
		downloadButton('downloadFile', 'Export graph', class="mat_btn"),
		
		tags$div(class="subHeader", checked=NA,
		         tags$p("Powerpoint presentation")
		),
		
		actionButton('actionSlide', "Add to presentation",  class="mat_btn"),
		
		textInput("pptx_filename", "Powerpoint filename", "CI5_slide"),
		
		downloadButton('downloadPres', 'Create presentation',  class="mat_btn"),
		
		uiOutput("UI_nbSlide")
  	
	   
	 ),
	  
	  
	dashboardBody(
	  useShinyjs(),
	 
	  fluidRow(

	      plotOutput("plot", height ="600px")

	   ),
	  fluidRow(
	    box(id="controls_COL1",
	      uiOutput("UI_control1"),
	      uiOutput("UI_control2")
	    ),
	    box(id="controls_COL2",
  	    uiOutput("UI_control3"),
  	    uiOutput("UI_control4")
	    )
	  )
	)
)