


                       #########################################################
                       ##FUNCTION NAME: GENERATE GRAPHS
                       ##DESCRIPTION  : GENERATING GRAPHS FOR EACH PRODUCT
                       ##               (MEMBERSHIP VS DATE)
                       ##DATE         : 4/22/2019
                       ##WRITTEN BY   : GIRWAR SHANGARI
                       #########################################################



##################################
##IMPORTING DIFFERENT LIBRARIES
##################################
                       
library(dplyr)
library(officer)
library(ggplot2) 
library(magrittr)
library(lubridate)
library(rvg)

                     

                       
##########FUNCTION ARGUMENTS DETAILS##################
######################################################
                       
#dataset   = Main dataset
#groups    = Total number of groups (Dataset) 
#Name      = Unique combination of Product, 
#            State, Funding (Vector)
#File_name = Name of final file
######################################################                       

                       
#########################################
##CREATING FUNCTION TO GENERATE GRAPHS
#########################################  
                      
Create_Graphs = function(dataset,groups,Name,file_name)
{
doc = read_pptx()  
for (i in 1:nrow(groups))
{
  data_graph_ind        = dataset[dataset$Name == Name[i],]
  
  data_graph_ind$year   = year(data_graph_ind$GL_IDB_DT)
  data_graph_ind$MonthN = as.numeric(month(data_graph_ind$GL_IDB_DT))
  graph                 = ggplot(data = data_graph_ind, aes(x = GL_IDB_DT, y = MBR)) + 
                          geom_line()+geom_point()
  doc                   = add_slide(doc, layout = "Two Content", master = "Office Theme")
  doc                   = doc %>% 
                          ph_with_text(type = "title", str = Name[i])
  
  doc                   = doc %>%
                          ph_with_vg(doc, ggobj=ggplot(data = data_graph_ind, aes(x = GL_IDB_DT, y = MBR)) + 
                          geom_line()+geom_point(), height=4, type="body")
  
  print(doc,target = paste0(out_dir,file_name))
}
}


