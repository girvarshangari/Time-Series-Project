
                       

library(forecastHybrid)
library(lubridate)
library(data.table)
library(imputeTS)
library(stringr)
library(sqldf)
library(xlsx)

source("***/Count_Elements2.R")
source("***/Generate_Graphs.R")
source("***/Generate_Forecast.R")
source("***/Impute_Missing.R")     #Change Month year in the function
source("***/Greater_than_60PC_Filter_Groups.R")
source("***/Less_than_60PC_Filter_Groups.R")

out_dir = "**"
in_dir  = "**"

##########################
## READING THE EXCEL FILES
##########################

CS_data_mem=read.xlsx(paste0(in_dir,"Name.xlsx"),sheetName = "Name1")

####################################
## LISTING ALL CHARACTER VARIABLES
####################################

chrs    = sapply(CS_data_mem, is.factor)
chrCols = names(CS_data_mem[,chrs])

####################
## REMOVING SPACES
####################

for (i in 1:length(chrCols))
{
  CS_data_mem[[chrCols[i]]] = gsub(" ", "",CS_data_mem[[chrCols[i]]])
}

###############################
## CREATING THE NAME VARIABLE
###############################

for (i in 1:length(chrCols))
{
  CS_data_mem$Name = paste(CS_data_mem$Name,CS_data_mem[[chrCols[i]]],sep='-')
}

CS_data_mem$Name = sub('-','',CS_data_mem$Name)

#############################################################
## SUMMARIZING THE DATA BY DATE, PRODUCT, STATE AND FUNDING
#############################################################

CS_data_summary_mem = sqldf("select GL_IDB_DT,Name,UHCSB_PRDCT_ID,
                             UHCSB_GRP_SITUS_ST_CD,Funding_Arrangement,sum(MBR) as MBR
                            from CS_data_mem group by Name,UHCSB_PRDCT_ID,
                            UHCSB_GRP_SITUS_ST_CD,Funding_Arrangement,GL_IDB_DT")

###############################################################
##CREATING YEAR AND MONTH VARIABLE USING LUBRIDATE LIBRARY
###############################################################

CS_data_summary_mem$Year  = year(CS_data_summary_mem$GL_IDB_DT)
CS_data_summary_mem$Month = month(CS_data_summary_mem$GL_IDB_DT)

###########################################################
## ARRANGING THE DATA BY PRODUCT, STATE, FUNDING AND DATE
###########################################################

CS_data_summary_mem       = sqldf("select * from CS_data_summary_mem
                                   order by UHCSB_PRDCT_ID,UHCSB_GRP_SITUS_ST_CD,
                                   Funding_Arrangement,GL_IDB_DT")

##########################################
## SEPARATING THE GROUPS BY FI AND ASO
##########################################

CS_data_summary_mem_FI    = sqldf("select * from CS_data_summary_mem where Funding_Arrangement like 'FI'")

CS_data_summary_mem_ASO   = sqldf("select * from CS_data_summary_mem where Funding_Arrangement like 'ASO'")

########################################################
## ALLIGNING MEMBERSHIP ACROSS ACTUALS STATES
########################################################

State_Prod               = read.csv("**/State_Prod_new.csv")
State_Prod$Scion.Prod    = as.character(State_Prod$Scion.Prod)
State_Prod$Prod1         = ifelse(nchar(State_Prod$Scion.Prod) == 1,
                               paste("00",State_Prod$Scion.Prod,sep =""),
                               State_Prod$Scion.Prod) 
State_Prod$Prod          = ifelse(nchar(State_Prod$Prod1) == 2,
                               paste("0",State_Prod$Prod1,sep =""),
                               State_Prod$Prod1) 
State_Prod$Prod          = paste0("SDI00",State_Prod$Prod)
State_Prod               = State_Prod[,c(1,4)]
State_Prod$State         = gsub(" ","",State_Prod$State)
State_Prod               = sqldf("select distinct State,Prod
                               from State_Prod")


CS_data_summary_mem_FI_2 = sqldf("select GL_IDB_DT,UHCSB_PRDCT_ID,Funding_Arrangement,
                                  sum(MBR) as MBR from CS_data_summary_mem_FI
                                  group by GL_IDB_DT,UHCSB_PRDCT_ID,Funding_Arrangement")

CS_data_summary_mem_FI_3 = sqldf("select a.GL_IDB_DT,a.UHCSB_PRDCT_ID,
                                  b.State,a.Funding_Arrangement,a.MBR
                                  from CS_data_summary_mem_FI_2 a left join State_Prod b
                                  on a.UHCSB_PRDCT_ID=b.Prod")

CS_data_summary_mem_FI_Final  = sqldf("select GL_IDB_DT,UHCSB_PRDCT_ID,
                                                State as UHCSB_GRP_SITUS_ST_CD,
                                                Funding_Arrangement,sum(MBR) as MBR
                                                from CS_data_summary_mem_FI_3 group by GL_IDB_DT,
                                                UHCSB_PRDCT_ID,State,Funding_Arrangement")

CS_data_summary_mem_FI_Final$Year      = year(CS_data_summary_mem_FI_Final$GL_IDB_DT)
CS_data_summary_mem_FI_Final$Month     = month(CS_data_summary_mem_FI_Final$GL_IDB_DT)

#######################################
##TAKING THE DATA FROM 2016 ONWARDS
#######################################

CS_data_summary_mem_FI_Final           = sqldf("select * from CS_data_summary_mem_FI_Final where year>2015")

##################################################################
##EXPORTING THE PRODUCTS NOT IN THE FILE SHARED BY CHRISTOS/ JOE
##################################################################

Not_in_Prod_State_file                 = CS_data_summary_mem_FI_Final[is.na(CS_data_summary_mem_FI_Final
                                                                      $UHCSB_GRP_SITUS_ST_CD),]

write.csv(Not_in_Prod_State_file,file  = paste0(out_dir,"Groups_not_in_Prod_State_File.csv")) 

#######################################################################
##KEEPING ONLY THE PRODUCTS PRESENT IN THE FILE SHARED BY CHRISTOS/ JOE 
#######################################################################

CS_data_summary_mem_FI_Final           = CS_data_summary_mem_FI_Final[!(is.na(CS_data_summary_mem_FI_Final$UHCSB_GRP_SITUS_ST_CD)),]

CS_data_summary_mem_ASO                = sqldf("select GL_IDB_DT,UHCSB_PRDCT_ID,
                                                UHCSB_GRP_SITUS_ST_CD,Funding_Arrangement,sum(MBR) as MBR
                                                from CS_data_summary_mem_ASO group by GL_IDB_DT, 
                                                UHCSB_PRDCT_ID,UHCSB_GRP_SITUS_ST_CD,Funding_Arrangement") 

State_Prod_ASO            = read.csv("C:/Documents/Project - C&S Dental/Reinstated data/Claims_data/Prod_State_comb/State_Prod_new_ASO.csv")
State_Prod_ASO$Scion.Prod = as.character(State_Prod_ASO$Scion.Prod)
State_Prod_ASO$Prod1      = ifelse(nchar(State_Prod_ASO$Scion.Prod) == 1,
                                   paste("00",State_Prod_ASO$Scion.Prod,sep =""),
                                   State_Prod_ASO$Scion.Prod) 
State_Prod_ASO$Prod       = ifelse(nchar(State_Prod_ASO$Prod1) == 2,
                                   paste("0",State_Prod_ASO$Prod1,sep =""),
                                   State_Prod_ASO$Prod1) 


State_Prod_ASO$Prod       = paste0("SDI00",State_Prod_ASO$Prod)
State_Prod_ASO            = State_Prod_ASO[,c(1,4)]
State_Prod_ASO$State      = gsub(" ","",State_Prod_ASO$State)

State_Prod_ASO            = sqldf("select distinct State,Prod
                                   from State_Prod_ASO")

CS_data_summary_mem_ASO2  = sqldf("select GL_IDB_DT,UHCSB_PRDCT_ID,Funding_Arrangement,
                                   sum(MBR) as MBR from CS_data_summary_mem_ASO
                                   group by GL_IDB_DT,UHCSB_PRDCT_ID,Funding_Arrangement")

CS_data_summary_mem_ASO3  = sqldf("select a.GL_IDB_DT,a.UHCSB_PRDCT_ID,
                                   b.State,a.Funding_Arrangement,a.MBR
                                   from CS_data_summary_mem_ASO2 a left join State_Prod_ASO b
                                   on a.UHCSB_PRDCT_ID=b.Prod")

CS_data_summary_mem_ASO4  = sqldf("select GL_IDB_DT,UHCSB_PRDCT_ID,State as UHCSB_GRP_SITUS_ST_CD,
                                   Funding_Arrangement,sum(MBR) as MBR
                                   from CS_data_summary_mem_ASO3 group by GL_IDB_DT,
                                   UHCSB_PRDCT_ID,State,Funding_Arrangement")

######################################################################
##EXPORTING THE ASO PRODUCTS NOT IN THE FILE SHARED BY CHRISTOS/ JOE
######################################################################

No_State_ASO                = CS_data_summary_mem_ASO4[is.na(CS_data_summary_mem_ASO4$UHCSB_GRP_SITUS_ST_CD),]

write.csv(No_State_ASO,file = paste0(out_dir,"No_State_ASO.csv"))

################################################################################
##KEEPING ONLY THOSE ASO PRODUCTS PRESENT IN THE FILE SHARED BY CHRISTOS/ JOE 
################################################################################

CS_data_summary_mem_ASO5       = CS_data_summary_mem_ASO4[!is.na(CS_data_summary_mem_ASO4$UHCSB_GRP_SITUS_ST_CD),]
CS_data_summary_mem_ASO5$Year  = year(CS_data_summary_mem_ASO5$GL_IDB_DT)
CS_data_summary_mem_ASO5$Month = month(CS_data_summary_mem_ASO5$GL_IDB_DT)
CS_data_summary_mem_ASO_Final  = sqldf("select * from CS_data_summary_mem_ASO5 where year>2015")
CS_data_summary_Final          = rbind(CS_data_summary_mem_FI_Final,CS_data_summary_mem_ASO_Final)

write.csv(CS_data_summary_Final,file=paste0(out_dir,"Membership_Actuals.csv"))


#CS_data_summary_Final=sqldf("select * from CS_data_summary_Final where year=2016 or year=2017 or (year=2018 and month<=10)")   # Change this every month

CS_data_summary_Final$Name  = paste(CS_data_summary_Final$UHCSB_PRDCT_ID,
                                    CS_data_summary_Final$UHCSB_GRP_SITUS_ST_CD,
                                    CS_data_summary_Final$Funding_Arrangement,sep='-')
Name                        = unique(CS_data_summary_Final$Name)

CS_data_summary_Final$Year  = year(CS_data_summary_Final$GL_IDB_DT)
CS_data_summary_Final$Month = month(CS_data_summary_Final$GL_IDB_DT)

##########FUNCTION CALL################
#CALLING THE FUNCTION: COUNT ELEMENTS
#######################################

View(CS_data_summary_Final)
Count_Elements2(CS_data_summary_Final,Name)   
View(final_df)

#####################################################
#COUNTING THE NUMBER OF MONTHS ELAPSED SINCE JAN'16
#####################################################


months_elapsed = length(seq(from=as.Date("2016-01-01"), to=Sys.Date(), by='month')) - 1
months_elapsed

###################################
##PRODUCTS HAVING ALL DATA POINTS
###################################

Groups_All = final_df[which(final_df$Elements == 37),]
View(Groups_All)
write.csv(CS_data_summary_Final,file = paste0(out_dir,"Groups_All_Data_Points.csv"))

##########################################################
##PRODUCTS HAVING GREATER THAN 60% OF TOTAL DATA POINTS
##########################################################

more_than_60PC = final_df[which(final_df$Elements<37 & final_df$Elements>=37*.6),]
View(more_than_60PC)
write.csv(more_than_60PC,file=paste0(out_dir,"more_than_60PC.csv"))

##########################################################
##PRODUCTS HAVING LESS THAN 60% OF TOTAL DATA POINTS
##########################################################

less_than_60PC = final_df[which(final_df$Elements<37*.6 ),]
View(less_than_60PC)
write.csv(less_than_60PC,file = paste0(out_dir,"less_than_60PC.csv"))

###########################
##CREATING NAME VECTOR
###########################

Name = unique(Groups_All$Name)

##########FUNCTION CALL#################
#CALLING THE FUNCTION: GENERATE GRAPHS
########################################

Create_Graphs(CS_data_summary_Final,Groups_All,Name,file_name="ALL_DATA_POINTS.pptx")

                           ########################################################
                           ########################################################
                           ##GENERATING FORECAST FOR GROUPS WITH ALL DATA POINTS###
                           ########################################################
                           ########################################################


##########FUNCTION CALL###################
#CALLING THE FUNCTION: GENERATE FORECAST
##########################################

Generate_Forecast(CS_data_summary_Final,Groups_All,Name)


write.csv(final_table,file=paste0(out_dir,"FORECAST_FINAL_ALL_DATA_POINTS.csv"))

###########################
##CREATING NAME VECTOR
###########################

View(more_than_60PC)

##########FUNCTION CALL#################
#CALLING THE FUNCTION: FILTER GROUPS
########################################

More_than_60_PC_filter_groups(more_than_60PC,1,0)
View(more_than_60PC_subset)
Name = unique(more_than_60PC_subset$Name)

##########FUNCTION CALL#################
#CALLING THE FUNCTION: CREATE GRAPHS
########################################

Create_Graphs(CS_data_summary_Final,more_than_60PC_subset,Name,file_name="More_than_60_Part1.pptx")

##########FUNCTION CALL#################
#CALLING THE FUNCTION: IMPUTE MISSING
########################################

Years = c(2016,2017,2018,2019)
Impute_Missing(CS_data_summary_Final,more_than_60PC_subset,Name,Years)
View(Final_impute)

##########FUNCTION CALL##################
#CALLING THE FUNCTION: GENERATE FORECAST
#########################################

Generate_Forecast(Final_impute,more_than_60PC_subset,Name)

dt1 <- data.table(more_than_60PC, key="Name")
dt2 <- data.table(more_than_60PC_subset)
more_than_60PC_rest=dt1[!dt2]  

More_than_60_PC_filter_groups(more_than_60PC_rest,2,1)

##########FUNCTION CALL#################
#CALLING THE FUNCTION: GENERATE GRAPHS
########################################

Name = unique(more_than_60PC_subset$Name)
Create_Graphs(CS_data_summary_Final,more_than_60PC_subset,Name,file_name="GREATER_THAN_60_PART2.pptx")

##################################################
##CREATING YEAR VECTOR TO IMPUTE MISSING VALUES
##################################################

Years = c(2017,2018,2019)

Impute_Missing(CS_data_summary_Final,more_than_60PC_subset,Name,Years)
View(Final_impute)

Generate_Forecast(Final_impute,more_than_60PC_subset,Name)

dt1 <- data.table(more_than_60PC_rest, key="Name")
dt2 <- data.table(more_than_60PC_subset)
more_than_60PC_rest=dt1[!dt2]  
View(more_than_60PC_rest)


Name = unique(more_than_60PC_rest$Name)
Name
Years = c(2018,2019)

Impute_Missing(CS_data_summary_Final,more_than_60PC_rest,Name,Years)
View(Final_impute)


Generate_Forecast(Final_impute,more_than_60PC_rest,Name)



View(less_than_60PC)

##########FUNCTION CALL#################
#CALLING THE FUNCTION: FILTER GROUPS
########################################


Less_than_60_PC_filter_groups(less_than_60PC,2,0)
View(less_than_60PC_subset)
Name = unique(less_than_60PC_subset$Name)
Name

Years = c(2017,2018,2019)

Impute_Missing(CS_data_summary_Final,less_than_60PC_subset,Name,Years)
View(Final_impute)

Generate_Forecast(Final_impute,less_than_60PC_subset,Name)

dt1 <- data.table(less_than_60PC, key="Name")
dt2 <- data.table(less_than_60PC_subset)
less_than_60PC_rest=dt1[!dt2]  
View(less_than_60PC_rest)


Less_than_60_PC_filter_groups(less_than_60PC_rest,3,0)
View(less_than_60PC_subset)

Name = unique(less_than_60PC_subset$Name)
Name

Years = c(2018,2019)

Impute_Missing(CS_data_summary_Final,less_than_60PC_subset,Name,Years)
View(Final_impute)

Generate_Forecast(Final_impute,less_than_60PC_subset,Name)






