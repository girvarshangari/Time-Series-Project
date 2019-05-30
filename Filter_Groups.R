





filter_groups = function(dataset,YR_STRT,CY_M)
{
  dataset_subset          = dataset[0,]
  dataset_sample          = dataset 
  Names_Columns           = colnames(dataset)
  Names_Columns_Year      = Names_Columns[substr(Names_Columns,1,4) == "Year"]
  
  for(i in YR_STRT:(length(Names_Columns_Year)-1))
  {
    dataset_subset = dataset_sample[dataset_sample[[Names_Columns_Year[i]]] >= 8,]
    dataset_sample = dataset_subset 
  }
  
  dataset_subset = dataset_subset[dataset_subset[[Names_Columns_Year[length(Names_Columns_Year)]]] >= CY_M,] 
  more_than_60PC_subset <<- dataset_subset
  
  }

