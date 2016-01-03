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
        
      gvisColumnChart(plotdata[order(plotdata[,2],-plotdata[,3]),],
                      xvar="GLAccountDescription",
                      yvar=c("Positive Variance", "Negative Variance"),
                    options=list(
					 colors="['green','red']",
                      title=paste("Budget Variance by Account for ", input$mydata, sep="", collapse=""),
                      explorer="{actions: ['dragToZoom', 
                                          'rightClickToReset'],
                                maxZoomIn:0.05}",					
                      legend="none",
                      hAxes="[{title:'Account'}]",
                      vAxes="[{title:'Amount of Variance'}]",
                      width="100%", height=500,
					 `isStacked`=TRUE
                    )
      )