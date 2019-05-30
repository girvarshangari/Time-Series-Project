

final_table       =    data.frame(GL_IDB_DT = as.Date(character()),
                       PRODUCT              = character(),
                       STATE                = character(),
                       FUNDING              = character(),
                       FORECAST             = numeric(),
                       stringsAsFactors     = FALSE)

Generate_Forecast = function(dataset,Total_Products,Name)
{
  current_month     = month(Sys.Date())
  current_month
  last_month        = month(Sys.Date())-1
  last_month
  months_execute    = 12-(month(Sys.Date())-1)+12+2   #Remove +2 here
  #months_execute
  months_execute_CY = 12-(month(Sys.Date())-1)+2      #Remove +2 here
  #months_execute_CY
  months_label_CY   = month(Sys.Date())-1-2           #Remove -2 here
  #months_label_CY
  months_label_NY   = 12-(month(Sys.Date())-1)+2      #Remove +2 here
  #months_label_NY
  #nrow(Total_Products)
  for (i in 1:3)
    {
    temp_data                = dataset[dataset$Name == Name[i],] 
    temp_data                = sqldf("select * from temp_data order by GL_IDB_DT")
    MBR                      = temp_data$MBR
    data_frame_MBR           = as.data.frame(MBR)
    MBR_ts                   = ts(data_frame_MBR, frequency=12, start=c(min(temp_data$Year),01))
    model                    = hybridModel(MBR_ts)
    forecast_MBR             = forecast(model,h=months_execute)
    forecast_MBR             = as.data.frame(forecast_MBR)
    forecast_MBR             = forecast_MBR[1:months_execute,]
    forecast_MBR_2           = as.data.frame(round(forecast_MBR$`Point Forecast`)) 
    colnames(forecast_MBR_2) = "FORECAST" 
    forecast_MBR_2$PRODUCT   = word(Name[i],1,sep="-")
    forecast_MBR_2$STATE     = word(Name[i],2,sep="-")
    forecast_MBR_2$FUNDING   = word(Name[i],3,sep="-")
    
    for (j in 1:months_execute_CY)
       {
      new_row           = forecast_MBR_2[j,]
      new_row$GL_IDB_DT = as.Date(paste(year(Sys.Date()),j+months_label_CY,01,sep='-'),"%Y-%m-%d")
      final_table       = rbind(final_table,new_row)
       }
    
    for (j in (months_execute_CY+1):months_execute)
       {
      new_row           = forecast_MBR_2[j,]
      new_row$GL_IDB_DT = as.Date(paste(year(Sys.Date())+1,j-months_label_NY,01,sep='-'),"%Y-%m-%d")
      final_table       = rbind(final_table,new_row)
       }
    
  }
  
  final_table = final_table[,c("GL_IDB_DT","PRODUCT","STATE","FUNDING","FORECAST")] 
  final_table <<- final_table
}



