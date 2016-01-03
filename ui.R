  #===============================
  ## Libraries and SQL Connections
  #===============================
	library(markdown)
	library(shiny)
	library(googleVis)
	library(sqldf)
	library(rpivotTable)

	#load ggplot
	library(ggplot2)

	#load RMySQL and connection variables
	library(DBI)
	library(RMySQL)
	#Close all prior connections MySQL
	lapply(dbListConnections(dbDriver( drv = "MySQL")), dbDisconnect)
	#Create new connection to database
	con <- dbConnect(MySQL(),username="root",password="guP8#swuGEda",dbname="demo",host="localhost")

	#create sql variable that can be used for both operator comparison as well as property comparisons within an operator
	sql <- "SELECT DISTINCT MSA, PropertyManager, PropertyName, Category, GLAccountDescription, Actual, Budgeted, Variance, Units, 		ActualPerUnit, BudgetedPerUnit, VariancePerUnit, Month FROM tactualbudgetcmp;"

	#Set master variable equal to SQL Result - subsequent results will be set to CategoryDataAdmin
	MasterCategoryDataAdmin <- dbGetQuery(con,sql)

  #===============================
  ## BubbleChart
  #===============================
	
	#sql statements to get proper matrix format
	sql <- "SELECT b.PropertyName, b.Units, b.PropertyManager, b.BudgetedPerUnit, b.HistoricalPerUnit, Q.MSA 
FROM tbubblechart b 
LEFT JOIN 
	(
	SELECT a.PropertyName, a.Anonymous, p.MSA FROM tpropertyanonl a
	LEFT JOIN tproperty p
	ON 	a.PropertyName = p.PropertyName)Q

ON b.PropertyName = Q.Anonymous"
	BubbleChart <- dbGetQuery(con,sql)

	shinyUI(
		navbarPage(title=div(img(style="max-width:100px; margin-left: -100px; margin-top: -7px;", src="logo.png")),
			theme="bootstrap.css",
  #===============================
  ## Dynamic actual to budget comparison -- output 2 on server page
  #===============================
	
		tabPanel('Financial Review',
               tags$head(tags$style("tfoot {display: table-header-group;}")),
			   fluidRow(  
					column(3,
					div(class="panel panel-default",
						div(class="panel-heading", 
							h4("Occupancy"),
							align="center"
							), 
						div(class="panel-body", 
							h1("95%", 
								icon("home")
								), 
							align="center"
							)
						)
					),
					column(3,
					div(class="panel panel-default",
						div(class="panel-heading", 
							h4("NOI Growth"),
							align="center"
							), 
						div(class="panel-body", 
							h1("3.7%", 
								icon("arrow-up"),
								`style`="color:green"
								), 
							align="center"
							)
						)
					),
					column(3,
					div(class="panel panel-default",
						div(class="panel-heading", 
							h4("Revenue Growth"),
							align="center"
							), 
						div(class="panel-body", 
							h1("1.7%", 
								icon("arrow-up"),
								`style`="color:green"
								), 
							align="center"
							)
						)
					),
					column(3,
					div(class="panel panel-default",
						div(class="panel-heading", 
							h4("Operating Expense Growth"),
							align="center"
							), 
						div(class="panel-body", 
							h1("2.3%", 
								icon("arrow-up"),
								`style`="color:red"
								), 
							align="center"
							)
						)
					)
				),
				fluidRow(
					column(12,
						div(class="panel panel-default",
						div(class="panel-heading", 
							h4("Portfolio Overview by Historical and Budget Variances Per Unit - NOI"),
							align="center"
							), 
						div(class="panel-body", 
							fluidRow(
                  column(5,
                         selectizeInput("MSA", "Choose MSA:", multiple = TRUE, 
						            choices = sort(unique(BubbleChart[,6]), decreasing = FALSE))
                         ),
                  column(2,h3("or")),
                  column(5,
                         selectizeInput("FinProperty", "Choose Property:", multiple = TRUE, 
						            choices = sort(unique(BubbleChart[,1]), decreasing = FALSE))
                         )
                       ),
							htmlOutput("view22"), 
							align="center"
							)
						)
					)
				),
				fluidRow(
					column(6,
						div(class="panel panel-default", 
						div(class="panel-body", 
							htmlOutput("view24"), 
							align="center"
							)
						)
					),
					column(6,
						div(class="panel panel-default", 
						div(class="panel-body", 
							htmlOutput("view25"), 
							align="center"
							)
						)
					)					
				  ),
				fluidRow(
					column(12,
						div(class="panel panel-default", 
						div(class="panel-body", 
							htmlOutput("view26"), 
							align="center"
							)
						)
					)					
				  ),
				fluidRow(
					column(12,
						div(class="panel panel-default", 
						div(class="panel-body",  
               dataTableOutput("mytable1"),
							align="center"
							)
						)
					)					
				  )				 
				),
               
	tabPanel('Operations',
         fluidRow(
					column(12,
						div(class="panel panel-default", 
						div(class="panel-body",
               selectizeInput("Property", "Choose Property:", multiple = TRUE, 
						choices = sort(unique(BubbleChart[,1]), decreasing = FALSE)),
							htmlOutput("view27"),
               htmlOutput("view30"),
               htmlOutput("view28"),
               htmlOutput("view31"),                
               htmlOutput("view29"),
               dataTableOutput("mytable4"),
							align="center"
							)
						)
					)					
				  )
        ),
    
	tabPanel('Property Manager Analysis',
             fluidRow(
               column(4,
                      div(class="panel panel-default",
                          div(class="panel-heading", 
                              h4("NOI Negative Variance Density"),
                              align="center"
                          ), 
                          div(class="panel-body",  
                              htmlOutput("view11"),						
                              align="center"
                          )
                      )
               ),
               column(8,
                      div(class="panel panel-default",
                          div(class="panel-heading", 
                              h4("Portfolio Overview by Historical and Budget Variances Per Unit - NOI"),
                              align="center"
                          ), 
                          div(class="panel-body", 
                              dataTableOutput("mytable3"), 
                              align="center"
                          )
                      )
               )
             ),
             fluidRow(
               column(4,
                      div(class="panel panel-default",
                          div(class="panel-heading", 
                              h4("NOI Positive Variance Density"),
                              align="center"
                          ), 
                          div(class="panel-body",  
                              htmlOutput("view10"),  					
                              align="center"
                          )
                      )
               ),
               column(8,
                      div(class="panel panel-default",
                          div(class="panel-heading", 
                              h4("Portfolio Overview by Historical and Budget Variances Per Unit - NOI"),
                              align="center"
                          ), 
                          div(class="panel-body", 
                              dataTableOutput("mytable2"), 
                              align="center"
                          )
                      )
               )
             )             
             ),
    
  	tabPanel('Predictive Analysis',
  	         rpivotTableOutput("pivotvar")
  	),
  
		tabPanel('Predictive Fracking',
                rpivotTableOutput("pivot")
			    )
	)
  )
