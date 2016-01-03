  #===============================
  ## Load libraries and set connection variables for RMySQL
  #===============================

	# Load libraries
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
	lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
	#Create new connection to database
	con <- dbConnect(MySQL(),username="root",password="guP8#swuGEda",dbname="demo",host="localhost")

  #===============================
  ## Variance to Budget for Properties
  #===============================

	#create sql variable that can be used for both operator comparison as well as property comparisons within an operator
	sql <- "SELECT DISTINCT MSA, PropertyManager, PropertyName, Category, GLAccountDescription, ROUND(Actual,2) AS Actual, ROUND(Budgeted,2) AS Budgeted, ROUND(Variance,2) AS Variance, Units, ActualPerUnit, BudgetedPerUnit, VariancePerUnit, Month FROM tactualbudgetcmp;"

	#Set master variable equal to SQL Result - subsequent results will be set to CategoryDataAdmin
	MasterCategoryDataAdmin <- dbGetQuery(con,sql)
	
  #===============================
  ## Create Heatmap Matrix
  #===============================

	#sql statements to get proper matrix format
	sql <- "SELECT PropertyManager, REVENUES, `OTHER INCOME`, PAYROLL, `CONTRACT SERVICES`, ADMINISTRATIVE, MARKETING, `REPAIRS & MAINTENANCE`, UTILITIES, TURNOVER, `MANAGEMENT FEES`, `INTERIOR REPLACEMENTS`, `NOI`, `EXPENSES` FROM tpropmgrmatrix"
	HeatMapMatrix <- dbGetQuery(con,sql)
  
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

  #===============================
  ## Occupancy Stats
  #===============================
	
	#sql statements to get proper matrix format
	sql <- "SELECT e.Month, e.PropertyName, MONTHNAME(STR_TO_DATE(e.Month,'%m')) AS MonthName, ifnull(mi.MoveIn,0) AS MoveIn, 
  ifnull(mo.MoveOut,0) AS MoveOut, ifnull(e.Expiration,0) AS Expiration,
  ifnull(p.Prospects,0) AS Prospect

  FROM

  (SELECT PropertyName, Month(MoveInDate) AS Month, count(*) AS MoveIn FROM ttmpresident
  WHERE YEAR(MoveInDate) = 2015
  GROUP BY Month(MoveInDate), PropertyName)mi

  RIGHT JOIN 

  (SELECT PropertyName, Month(MoveOutDate) AS Month, count(*) AS MoveOut FROM ttmpresident
  WHERE YEAR(MoveOutDate) = 2015
  GROUP BY Month(MoveOutDate), PropertyName)mo

  ON mo.Month = mi.Month
  AND mo.PropertyName = mi.PropertyName

  INNER JOIN

  (SELECT PropertyName, Month(ExpirationDate) AS Month, count(*) AS Expiration FROM ttmpresident
  WHERE YEAR(ExpirationDate) = 2015
  GROUP BY Month(ExpirationDate), PropertyName)e

  ON mo.Month = e.Month
  AND mo.PropertyName = e.PropertyName

  INNER JOIN

  (SELECT PropertyName, Month(DesiredDate) AS Month, count(*) AS Prospects FROM ttmpprospect
  WHERE YEAR(DesiredDate) = 2015
  GROUP BY Month(DesiredDate), PropertyName)p

  ON mo.Month = p.Month
  AND mo.PropertyName = p.PropertyName"
	OccupancyStats <- dbGetQuery(con,sql)
	
  ## Rent Trend Bubble Chart

  sql <- "SELECT 
    PropertyName, 
    UnitName,
    UnitType,
    MONTHNAME(MoveInDate) AS MonthName, 
    ROUND(AVG(SquareFeet),0) AS SquareFeet,
    ROUND(AVG(RentSF),2) AS Rent,
    ROUND(AVG(MarketRentSF),2) AS MarketRent
  FROM demo.ttmpresident
  WHERE YEAR(ExpirationDate) = YEAR(NOW())
  GROUP BY UnitName, MONTHNAME(MoveInDate)
  ORDER BY MoveInDate ASC;"

  RentTrend <- dbGetQuery(con,sql)
    
  ## Resident Name and Status
    
  sql <- "SELECT
      ResidentName AS Name,
      PropertyName,
      UnitType,
      Rent,
      MoveInDate,
      MoveOutDate,
      ExpirationDate,
      NULL AS DesiredDate,
      MONTHNAME(MoveInDate) AS MoveInMonth,
      MONTHNAME(MoveOutDate) AS MoveOutMonth,
      MONTHNAME(ExpirationDate) AS ExpirationMonth,
      NULL AS DesiredMonth
    FROM demo.ttmpresident
    WHERE YEAR(MoveOutDate) >= YEAR(NOW())

    UNION ALL

    SELECT 
      ProspectName AS Name, 
      PropertyName, 
      UnitType, 
      DesiredRent AS Rent,
      NULL AS MoveInDate,
      NULL AS MoveOutDate,
      NULL AS ExpirationDate,
      DesiredDate,
      NULL AS MoveInMonth,
      NULL AS MoveOutMonth,
      NULL AS ExpirationMonth,
      MONTHNAME(DesiredDate) AS DesiredMonth
    FROM demo.ttmpprospect;"
    
    ResidentRoll <- dbGetQuery(con,sql)

  #===============================
  ## Prediction Table
  #===============================
	
	#sql statements to get proper matrix format
	sql <- "SELECT PropertyName, PropertyManager, Month, Category, Account, Actual, Budget, Projected FROM tPredictionAggregate"
	PredictionAg <- dbGetQuery(con,sql)	

  #===============================
  ## Prediction Table Variance
  #===============================
  
  #sql statements to get proper matrix format
  sql <- "SELECT PropertyName, PropertyManager, `Month`, Category, Account, Actual, Budget, 
Projected, Actual - Projected AS Variance 
FROM tPredictionAggregate
WHERE Actual - Projected > 0"
  PredictionAgVar <- dbGetQuery(con,sql)	

  #===============================
  ## Savings Table
  #===============================	
	
	sql <- "SELECT * FROM 
	(
	SELECT *, ROUND(CASE WHEN 0 > (CASE WHEN Actual > Budget THEN Budget ELSE Actual END - Projected) THEN 0 ELSE CASE WHEN Actual > Budget THEN Budget ELSE Actual END - Projected END,0) AS Savings FROM `tPredictionAggregate`
	)Q	WHERE Savings > 0"
	PotentialSavings <- dbGetQuery(con,sql)
	
  #===============================
  ## Shiny Server Code
  #===============================

shinyServer(function(input, output) {
	
  #===============================
  ## Create rpivotTable Output
  #===============================	
  
  # Potential Savings
  
	output$pivot <- renderRpivotTable({
    rpivotTable(data =   PotentialSavings   ,  cols = "PropertyManager",
                vals = "Savings", aggregatorName = "Sum", rendererName = "Bar Chart C3")
	})
  
  # Detailed Information for Budget and Historical
  
  output$pivotvar <- renderRpivotTable({
    rpivotTable(data =   PredictionAgVar   ,  cols = "PropertyName",
                vals = "Variance", aggregatorName = "Sum", rendererName = "Heatmap")
  })
	
  #===============================
  ## Dataset filtered section
  #===============================
  
  datasetFiltered1 <- reactive({
	#on change of MSA selection, change dataset
	
	# if statement for msa selection
	if(is.null(input$MSA) & is.null(input$FinProperty))
    {
	 BubbleChart
	}
	else if(is.null(input$FinProperty))
	{
	 BubbleChart[BubbleChart$MSA %in% paste(input$MSA, sep=""),]
	}
  else
	{
	 BubbleChart[BubbleChart$PropertyName %in% paste(input$FinProperty, sep=""),]
	}  
  })
  
  datasetFiltered2 <- reactive({
	#on change of MSA selection, change dataset
	
	# if statement for msa selection
	if(is.null(input$MSA))
    {
	 MasterCategoryDataAdmin
	}
	else
	{
	 MasterCategoryDataAdmin[MasterCategoryDataAdmin$MSA %in% paste(input$MSA, sep=""),]
	}
  })
  
  datasetFiltered3 <- reactive({
	if(is.null(input$Property))
      {
	  OccupancyStats
	  }
	else
	  {
	 OccupancyStats[OccupancyStats$PropertyName %in% paste(input$Property, sep=""),]
	  }
  })
  
  datasetFiltered4 <- reactive({
	if(is.null(input$Property))
      {
	 RentTrend
	  }
	else
	  {
	 RentTrend[RentTrend$PropertyName %in% paste(input$Property, sep=""),]
	  }
  })
  
  datasetFiltered5 <- reactive({
	if(is.null(input$occstatname))
      {
	  return()
	  }
	else if(input$occstatname == "Move-Outs")
	  {
	 df <- ResidentRoll[ResidentRoll$MoveOutMonth == input$operatingmonth & ResidentRoll$PropertyName == input$propertyname2,c(1:7)]
   df <- sqldf("SELECT * FROM df WHERE Name IS NOT NULL", drv="SQLite")
   df
	  }
  else if(input$occstatname == "Expirations")
	  {
	 df <- ResidentRoll[ResidentRoll$ExpirationMonth == input$operatingmonth & ResidentRoll$PropertyName == input$propertyname2,c(1:7)]
   df <- sqldf("SELECT * FROM df WHERE Name IS NOT NULL", drv="SQLite")
   df    
	  }
	else if(input$occstatname == "Move-Ins")
	  {
	 df <- ResidentRoll[ResidentRoll$MoveInMonth == input$operatingmonth & ResidentRoll$PropertyName == input$propertyname2,c(1:7)]
   df <- sqldf("SELECT * FROM df WHERE Name IS NOT NULL", drv="SQLite")
   df    
	  }
	else if(input$occstatname == "Prospects")
	  {
	 df <- ResidentRoll[ResidentRoll$DesiredMonth == input$operatingmonth & ResidentRoll$PropertyName == input$propertyname2,c(1:4,8)]
   df <- sqldf("SELECT * FROM df WHERE Name IS NOT NULL", drv="SQLite")
   df    
	  }    
  })
  
  #===============================
  ## Render data table
  #===============================
  
  #Financial statement detail
  
  output$mytable1 <- renderDataTable({
    
    if(is.null(input$myaccount))
    {return()}
    else{
      
      plotdata <- datasetFiltered2()
      
	    plotdata <- plotdata[plotdata$GLAccountDescription == input$myaccount,]
      
      plotdata[,c(1:3,5,6,9,10,13)]
		
    }
  })  
  
  # best property managers
  
  output$mytable2 = renderDataTable({
    BubbleChart[order(-BubbleChart$BudgetedPerUnit),c(1,3:5)]
  }, options = list(orderClasses = TRUE, 
                    lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                    pageLength = 5))
  
  # worst property managers
  
  output$mytable3 = renderDataTable({
	BubbleChart[order(BubbleChart$BudgetedPerUnit),c(1,3:5)]
  }, options = list(orderClasses = TRUE, 
	lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
	pageLength = 5))
  
  # residentroll
  
  output$mytable4 = renderDataTable({
    datasetFiltered5()
  })
  
  #===============================
  ## Create variance graphs
  #===============================  
  
  output$view10 <- renderGvis({
    
    plotdata <- BubbleChart[,]
    
    plotdata <- sqldf("SELECT PropertyManager, ROUND(sum(BudgetedPerUnit)*sum(Units),0) AS Budgeted FROM plotdata GROUP BY PropertyManager ORDER BY sum(BudgetedPerUnit) DESC LIMIT 3", drv="SQLite")
    
    plotdata <- plotdata[plotdata[,2] > 0,]
    
    gvisPieChart(plotdata[,],
                 options=list(
                   legend="{position: 'bottom', textStyle: {fontSize: 10}}",
                   pieHole=0.25,
                   height=415
                 )
    )
  })
  
  output$view11 <- renderGvis({
	
	plotdata <- HeatMapMatrix[order(HeatMapMatrix[,13]),c(1,13)]
	
	plotdata <- plotdata[plotdata[,2] < 0,]
	
	plotdata[,2] <- -plotdata[,2]
	
	gvisPieChart(plotdata,
					options=list(
					legend="{position: 'bottom', textStyle: {fontSize: 10}}",
					pieHole=0.25,
					height=415
					)
					)
	})

  output$view22 <- renderGvis({   
	plotdata <- datasetFiltered1()
    #jscode for on click event
    jscode <- "Shiny.onInputChange('mydata', data.getValue(chart.getSelection()[0].row,0));  "
	
	gvisBubbleChart(plotdata,
					idvar="PropertyName",			
					xvar="BudgetedPerUnit",
					yvar="HistoricalPerUnit",
					colorvar="PropertyManager",
					sizevar="Units",
					options=list(
						bubble="{textStyle:{color: 'none'}}",
						explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
						legend="none",
						title="Size of Bubble is Number of Units of Property",
						#hAxis="{minValue:-400, maxValue:300}",
						#vAxis="{minValue:-200, maxValue:1000}",
						hAxes="[{title:'Budgeted Per Unit Variance'}]",
						vAxes="[{title:'Historical Per Unit Variance'}]",
						width="100%", height=500,
						gvis.listener.jscode=jscode
					)
					)
	})
  
  output$view24 <- renderGvis({
    
    if(is.null(input$mydata))
    {return()}
    else{
      plotdata <- MasterCategoryDataAdmin[MasterCategoryDataAdmin$PropertyName == input$mydata,]
      
	  jscode <- "Shiny.onInputChange('mycategory', data.getValue(chart.getSelection()[0].row,0));  "
		
      plotdata <- sqldf("SELECT Category, sum(Variance) AS 'Budget Variance' FROM plotdata GROUP BY Category", drv="SQLite")
      
      plotdata <- sqldf("SELECT 
                          Category, 
                        	CASE WHEN `Budget Variance` > 0 AND Category IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	WHEN `Budget Variance` < 0 AND Category NOT IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	ELSE 0 END AS 'Positive Variance',
                        	CASE WHEN `Budget Variance` < 0 AND Category IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	WHEN `Budget Variance` > 0 AND Category NOT IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	ELSE 0 END AS 'Negative Variance'
                        	FROM plotdata", drv="SQLite")
        
      gvisBarChart(plotdata[order(plotdata[,2],-plotdata[,3]),],
                      xvar="Category",
                      yvar=c("Positive Variance", "Negative Variance"),
                    options=list(
					 colors="['green','red']",
                      title=paste("Budget Variance by Category for ", input$mydata, sep="", collapse=""),
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      vAxes="[{title:'Category'}]",
                      hAxes="[{title:'Amount of Variance'}]",
                      width="100%", height=500,
					 `isStacked`=TRUE,
					 gvis.listener.jscode=jscode
                    )
      )
    }
  })
	
  output$view25 <- renderGvis({
    
    if(is.null(input$mycategory) | is.null(input$mydata))
    {return()}
    else{
      
	  plotdata <- MasterCategoryDataAdmin[MasterCategoryDataAdmin$PropertyName == input$mydata & MasterCategoryDataAdmin$Category == input$mycategory,]
      
	  jscode <- "Shiny.onInputChange('myaccount', data.getValue(chart.getSelection()[0].row,0));  "
		
      plotdata <- sqldf("SELECT GLAccountDescription, Category, sum(Variance) AS 'Budget Variance' FROM plotdata GROUP BY GLAccountDescription, Category", drv="SQLite")
      
      plotdata <- sqldf("SELECT 
                          GLAccountDescription, 
                        	CASE WHEN `Budget Variance` > 0 AND Category IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	WHEN `Budget Variance` < 0 AND Category NOT IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	ELSE 0 END AS 'Positive Variance',
                        	CASE WHEN `Budget Variance` < 0 AND Category IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	WHEN `Budget Variance` > 0 AND Category NOT IN ('OTHER INCOME', 'REVENUES') THEN `Budget Variance`
                        	ELSE 0 END AS 'Negative Variance'
                        	FROM plotdata", drv="SQLite")
        
      gvisBarChart(plotdata[order(plotdata[,2],-plotdata[,3]),],
                      xvar="GLAccountDescription",
                      yvar=c("Positive Variance", "Negative Variance"),
                    options=list(
					           colors="['green','red']",
                      title=paste(input$mycategory, " Variance by Account for ", input$mydata, sep="", collapse=""),
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      vAxes="[{title:'Account'}]",
                      hAxes="[{title:'Amount of Variance'}]",
                      width="100%", height=500,
					           `isStacked`=TRUE,
                      gvis.listener.jscode=jscode
                    )
      )
		
    }
  })
  
  output$view26 <- renderGvis({
    
    if(is.null(input$myaccount))
    {return()}
    else{
      
      plotdata <- datasetFiltered2()
      
	    plotdata <- plotdata[plotdata$GLAccountDescription == input$myaccount,]
      
      plotdataprop <- plotdata[plotdata$PropertyName == input$mydata,]
      
      plotdataprop <- sqldf("SELECT PropertyName, GLAccountDescription, sum(ActualPerUnit) AS 'Selected Property Per Unit', 0 AS 'Comparable Per Unit' FROM plotdataprop GROUP BY PropertyName, GLAccountDescription", drv="SQLite")
      
      plotdatagroup <- plotdata[plotdata$PropertyName != input$mydata,]
      
      plotdatagroup <- sqldf("SELECT PropertyName, GLAccountDescription, 0 AS 'Selected Property Per Unit', sum(ActualPerUnit) AS 'Comparable Per Unit' FROM plotdatagroup GROUP BY PropertyName, GLAccountDescription", drv="SQLite")
      
      chartdata <- rbind(plotdatagroup,plotdataprop)      
        
      gvisColumnChart(chartdata[order(chartdata$PropertyName),],
                      xvar="PropertyName",
                      yvar=c("Selected Property Per Unit","Comparable Per Unit"),
                      options=list(
					           title=paste("Comparison of Selected Properties (Selected MSAs Only) for ", input$myaccount, sep="", collapse=""),
                      colors="['#ffb300','#363636']",
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      hAxes="[{title:'Property', direction:-1, slantedText:true, slantedTextAngle:90, textStyle: {fontSize: 10}}]",
                      vAxes="[{title:'Amount per Unit'}]",
                      width="100%", height=500,
					           `isStacked`=TRUE                        
                    )
      )
		
    }
  })
  
  output$view27 <- renderGvis({
      
      plotdata <- datasetFiltered3()
    
      jscode <- "Shiny.onInputChange('operatingmonth', data.getValue(chart.getSelection()[0].row,0));  "
    
      plotdata <- sqldf("SELECT MonthName, ROUND(sum(MoveIn) - sum(MoveOut) -sum(Expiration)*(1-.45) + sum(Prospect)*.25,0) AS OccupancyChange FROM plotdata GROUP BY MonthName ORDER BY Month ASC", drv="SQLite")
        
      gvisColumnChart(plotdata,
                      xvar="MonthName",
                      yvar="OccupancyChange",
                      options=list(
					           title=paste("Projected Changes in Occupancy by Month", sep="", collapse=""),
                      colors="['#ffb300']",  
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      hAxes="[{title:'Month'}]",
                      vAxes="[{title:'Units'}]",
                      width="100%", height=300,
                      gvis.listener.jscode=jscode
                    )
      )
  
    })
    
  output$view28 <- renderGvis({
    
    if(is.null(input$operatingmonth))
    {return()}
    else{
    
      plotdata <- datasetFiltered3()
      
      plotdata <- plotdata[plotdata$MonthName == input$operatingmonth,]
    
      jscode <- "Shiny.onInputChange('propertyname2', data.getValue(chart.getSelection()[0].row,0));  "
    
      plotdata <- sqldf("SELECT PropertyName, ROUND(sum(MoveIn) - sum(MoveOut) -sum(Expiration)*(1-.45) + sum(Prospect)*.25,0) AS OccupancyChange FROM plotdata GROUP BY PropertyName", drv="SQLite")
        
      gvisColumnChart(plotdata[order(-plotdata$OccupancyChange),],
                      xvar="PropertyName",
                      yvar="OccupancyChange",
                      options=list(
					           title=paste("Projected Changes in Occupancy by Property in ", input$operatingmonth, sep="", collapse=""),
                      colors="['#ffb300']",  
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      hAxes="[{title:'Month', direction:-1, slantedText:true, slantedTextAngle:90, textStyle: {fontSize: 10}}]",
                      vAxes="[{title:'Units'}]",
                      width="100%", height=400,
                      gvis.listener.jscode=jscode
                    )
      )
      
    }
    
  })
  
  output$view29 <- renderGvis({
      
    if(is.null(input$propertyname2))
    {return()}
    else{
      
      plotdata <- datasetFiltered3()
      
      plotdata <- plotdata[plotdata$MonthName == input$operatingmonth & plotdata$PropertyName == input$propertyname2,]
    
      jscode <- "Shiny.onInputChange('occstatname', data.getValue(chart.getSelection()[0].row,0));  "
    
      plotdata <- sqldf("SELECT sum(MoveIn) AS MoveIns, sum(MoveOut) AS MoveOuts, sum(Expiration) AS Expirations, sum(Prospect) AS Prospects FROM plotdata", drv="SQLite")
      
      plotdata <- data.frame(Category = c("Move-Ins", "Move-Outs", "Expirations", "Prospects"), Units = c(plotdata$MoveIns, plotdata$MoveOuts, plotdata$Expirations, plotdata$Prospects))
      
      gvisColumnChart(plotdata,
                      xvar="Category",
                      yvar="Units",
                      options=list(
					           title=paste("Operating Statistics for ", input$propertyname2, " in ", input$operatingmonth, sep="", collapse=""),
                      colors="['#ffb300']",
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      hAxes="[{title:'Category'}]",
                      vAxes="[{title:'Units'}]",
                      width="100%", height=300,
                      gvis.listener.jscode=jscode
                    )
      )
		}
  })
  
  output$view30 <- renderGvis({
      
      plotdata <- datasetFiltered3()
    
      plotdata <- sqldf("SELECT Month, ROUND(sum(Expiration),0) AS Expirations FROM plotdata GROUP BY Month ORDER BY Month ASC", drv="SQLite")
        
      gvisScatterChart(plotdata,
                      options=list(
                      trendlines="{0: { type: 'polynomial',  
                       visibleInLegend: 'true', 
                       color: '#ffb300',
                       lineWidth: 10,
                       opacity: 0.5}}",
					           title=paste("Expirations by Month with Trend Line", sep="", collapse=""),
                      colors="['#363636']",  
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      hAxes="[{title:'Month'}]",
                      vAxes="[{title:'Units'}]",
                      width="100%", height=300
                    )
      )
  
    })
  
    output$view31 <- renderGvis({
      
      if(is.null(input$propertyname2))
      {return()}
      else{
      
      plotdata <- datasetFiltered4()
      
      plotdata <- plotdata[plotdata$MonthName == input$operatingmonth & plotdata$PropertyName == input$propertyname2,]
        
      gvisBubbleChart(plotdata,
					idvar="UnitName",			
					xvar="Rent",
					yvar="MarketRent",
					colorvar="UnitType",
					sizevar="SquareFeet",
					options=list(
						bubble="{textStyle:{color: 'none'}}",
						explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
						legend="none",
						title=paste("Market Rent vs Actual Rent Per Square Foot for All Units Expiring in ", input$operatingmonth, " at ", input$propertyname2, sep=""), 
						hAxis="{minValue:0.8, maxValue:1.2}",
						vAxis="{minValue:0.8, maxValue:1.2}",						
             hAxes="[{title:'Rent per Square Foot'}]",
						vAxes="[{title:'Market Rent per Square Foot'}]",
						width="100%", height=500
            )
       )
        
       }
  
    })
	

  
})
