

                      #########################################################
                      ##FUNCTION NAME: Count_Elements2
                      ##DESCRIPTION  : COUNTING TOTAL NUMBER OF ELEMENTS
                      ##               AND ELEMENTS IN EACH YEAR
                      ##DATE         : 4/22/2019
                      ##WRITTEN BY   : GIRWAR SHANGARI
                      #########################################################


##########FUNCTION ARGUMENTS DETAILS#####
#########################################
#dataset = Main dataset
#Name    = Unique combination of Product, 
#          State, Funding
##########################################


Count_Elements2 = function(dataset,Name)                
  
{

#############################################
##Calculating the Range of Years in the data
#############################################
  
range    =  max(dataset$Year)-min(dataset$Year)

#############################
## Defining an empty dataset
#############################

final_df =  data.frame(Name = character(),
                       Elements = integer(),
                       stringsAsFactors = F)

#####################################
## Defining additonal Year variables
## depending on the range
#####################################


for (i in 1:(range+1))
{
  x1       = data.frame(Year_i = integer())
  final_df = cbind(final_df,x1)
}

##################################
##Renaming the variables
##################################

j=0

for (i in 1:(range+1))
{
  colnames(final_df)[i+2] = paste("Year",min(dataset$Year)+j,sep = "_")
  j = j+1
}

temp_df = final_df

##################################
##Creating the final dataset
##################################

for (i in 1:length(unique(dataset$Name)))
{
  individual_df = dataset[dataset$Name == Name[i],]
  temp_df[1,c("Name")] = Name[i]
  temp_df[1,c("Elements")] = nrow(individual_df)
  k = 0
  for (j in 3:ncol(final_df))
  {
    
    temp_df[1,j] = sum(individual_df$Year == (min(dataset$Year)+k))
    k = k+1
    
  }
  final_df = rbind(final_df,temp_df)
}

final_df  <<- final_df
}
