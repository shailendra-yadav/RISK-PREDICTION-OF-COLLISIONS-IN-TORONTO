############################### Exploratory Data Analysis ####################################
library(mice) # FOR MISSING VALUE IMPUTATION
library(VIM)  #FOR MISSING VALUE IMPUTATION 
library(openxlsx) # TO READ AND WRITE EXCEL FILES
library(data.table) # READ AND WRITE TEXT FILES WITH MINIMUM TIME
library(missForest)   # MISSING VALUE IMPUTATION LIBRARY
library(randomForest)    # FOR RANDOM FOREST ALGORITHM
library(PCAmixdata)  # PACKAGE FOR PRINCIPLE COMPONENT ANALYSIS HERE WE CAN USE FACTOR AND NUMERIC VARIABLES BOTH
library(woe) # PACKAGE TO CALCULATE VARIABLE IMPORTANCE
setwd("/Users/shailendra/Desktop/updated_MRP/code/") # TO SET DIRECTORY
list.files()   # TO SEE THE FILES OF THAT DIRECTORY

data<-fread("KSI.csv",na.strings=c("","NA"))  # READ RAW DATA
data_fields<-read.xlsx("Field Matrix.xlsx",sheet=1) # READ VARIABLES AND DESCRIPTION


summary(data) #PRINT SUMMARY 
str(data) # STRUCTURE OF THE DATA.
dim(data)  # TO CAPTURE DIMENSIONS OF THE DATA ROW AND COLUMNS
############################ VARIABLE SELECTION BY LOGICAL WAY ################################

First_lvl_vars<-subset(data_fields,Variable_Selection %in% 'Y') # SUBSET DESCRIPTION DATA WITH LOGICALLY SELECTED FLAGS, Y MEANS SELECTED
dim(First_lvl_vars)
dim(data_fields)
selected_Variables<-subset(First_lvl_vars,select=c(Field.Name,Description)) # SUBSET NAME AND DESCRIPTION FIELD
Rejected_Variables<-subset(data_fields,select=c(Field.Name,Description),Variable_Selection %in% 'N') # SUBSET REJECTED VARIABLES

First_lvl_Cln_Data<-subset(data,select=First_lvl_vars$Field.Name)
dim(First_lvl_Cln_Data)

######################### First Level Variable selection done ##########################

First_lvl_Cln_Data$DATE<-as.Date(First_lvl_Cln_Data$DATE,format = "%Y.%m.%d") # CONVERT DATE FIELD INTO DATE FORMAT
First_lvl_Cln_Data$Month<-format(First_lvl_Cln_Data$DATE,"%B") # EXTRACT MONTH FROM DATE FIELD



########################### Missing Report and Imputation ##################################

missing_count<-data.frame(sapply(First_lvl_Cln_Data, function(x) sum(is.na(x)))) # COUNT MISSING VALUES
names(missing_count)=c("No_of_Missing") # RENAME COLUMNS
missing_count$Pct_of_Missing<-round((missing_count$No_of_Missing/nrow(First_lvl_Cln_Data))*100,1) # PERCENTAGE CALCULATION OF MISSING VALUES
missing_count<-data.frame(Field.Name=row.names(missing_count),missing_count)
row.names(missing_count)=NULL
missing_count<-merge(missing_count,selected_Variables,by="Field.Name") # ADD DESCRIPTION FIELD
missing_count<-subset(missing_count,select=c(Field.Name,Description,No_of_Missing,Pct_of_Missing)) # ARRANGE VARIABLES
png(filename="Total Number of Missing Per Variables.png",width = 2000,height = 2000)
# Grouped Bar Plot
aggr(First_lvl_Cln_Data) # MISSING REPORT CREATION
dev.off()

############################## Write Report till Missing Report #############################

#

################################## Missing Value Report Done #####################################

################################## Missing Value imputation ######################################

Binary_Variables<-subset(data_fields,Missing_Type %in% 'B',select=c(Field.Name,Description)) # SUBSET BINARY VARIABLES

Binary_data<-subset(First_lvl_Cln_Data,select=Binary_Variables$Field.Name)
Binary_data[is.na(Binary_data)]<-"No" # BLANKS ARE REPLACED WITH No
Others_data<-subset(First_lvl_Cln_Data,select=names(First_lvl_Cln_Data)[names(First_lvl_Cln_Data) %in% Binary_Variables$Field.Name==F])
Append_data<-data.frame(Others_data,Binary_data)
Append_data<-subset(Append_data,select=names(First_lvl_Cln_Data))

################ 2ND PHASE MISSING VALUE REPORT #############

missing_count_Second<-data.frame(sapply(Append_data, function(x) sum(is.na(x))))
names(missing_count_Second)=c("No_of_Missing")
missing_count_Second$Pct_of_Missing<-round((missing_count_Second$No_of_Missing/nrow(First_lvl_Cln_Data))*100,1)
missing_count_Second<-data.frame(Field.Name=row.names(missing_count_Second),missing_count_Second)
row.names(missing_count_Second)=NULL
missing_count_Second<-merge(missing_count_Second,selected_Variables,by="Field.Name",all.x=T)
missing_count_Second<-subset(missing_count_Second,select=c(Field.Name,Description,No_of_Missing,Pct_of_Missing))

png(filename="Total Number of Missing Per Variables_2nd_lvl.png",width = 2000,height = 2000)
# Grouped Bar Plot
aggr(Append_data)
dev.off()

################################# MISSING IMPUTATION LOGICALLY ###############################


names(Append_data)

# here I have put some logic. If PEDESTRIAN == No then PEDTYPE is also no and replace with Not_Applicable

Append_data$PEDTYPE<-ifelse(Append_data$PEDESTRIAN=='No',"Not_Applicable",Append_data$PEDTYPE) # LOGICALLY IMPUTE MISSING VALUES WHEN NEEDED
Append_data$PEDTYPE[is.na(Append_data$PEDTYPE)]<-"Missing"

# # here I have put some logic. If PEDESTRIAN == No then PEDACT is also no and replace with Not_Applicable


Append_data$PEDACT<-ifelse(Append_data$PEDESTRIAN=='No',"Not_Applicable",Append_data$PEDACT)
Append_data$PEDACT[is.na(Append_data$PEDACT)]<-"Missing"

# # here I have put some logic. If PEDESTRIAN == No then PEDCOND is also no and replace with Not_Applicable

Append_data$PEDCOND<-ifelse(Append_data$PEDESTRIAN=='No',"Not_Applicable",Append_data$PEDCOND)
Append_data$PEDCOND[is.na(Append_data$PEDCOND)]<-"Missing"


# # here I have put some logic. If CYCLIST == No then CYCLISTYPE is also no and replace with Not_Applicable


Append_data$CYCLISTYPE<-ifelse(Append_data$CYCLIST=='No',"Not_Applicable",Append_data$CYCLISTYPE)
Append_data$CYCLISTYPE[is.na(Append_data$CYCLISTYPE)]<-"Missing"

# # here I have put some logic. If CYCLIST == No then CYCACT is also no and replace with Not_Applicable


Append_data$CYCACT<-ifelse(Append_data$CYCLIST=='No',"Not_Applicable",Append_data$CYCACT)
Append_data$CYCACT[is.na(Append_data$CYCACT)]<-"Missing"

# # here I have put some logic. If CYCLIST == No then CYCCOND is also no and replace with Not_Applicable

Append_data$CYCCOND<-ifelse(Append_data$CYCLIST=='No',"Not_Applicable",Append_data$CYCCOND)
Append_data$CYCCOND[is.na(Append_data$CYCCOND)]<-"Missing"

#install.packages("stringr", dependencies = TRUE)
library(stringr) # for str_extract_all() function

# To classify 'Driver' and 'No_driver' in INVTYPE
table(as.data.frame(unlist(str_extract_all(Append_data$INVTYPE,"Driver"))))

Driver<-data.frame(INVTYPE=unique(Append_data$INVTYPE),Driver_Flag=c("Driver","Driver",rep("No_Driver",6),"Driver",rep("No_Driver",3),"Driver",rep("No_Driver",7)))
Append_data <-merge(Append_data,Driver,by="INVTYPE",all.x=T)  # all.x is used for left join                                                            


# # here I have put some logic. If Driver_Flag == No then No_Driver is also no and replace with Not_Applicable
# If INVTYPE is not Driver then there should no vlaue in DRIVACT and DRIVCOND, then we are imputing Not_Applicable otherwise Missing
Append_data$DRIVACT<-ifelse(Append_data$Driver_Flag=='No_Driver',"Not_Applicable",Append_data$DRIVACT)
Append_data$DRIVACT[is.na(Append_data$DRIVACT)]<-"Missing"

Append_data$DRIVCOND<-ifelse(Append_data$Driver_Flag=='No_Driver',"Not_Applicable",Append_data$DRIVCOND)
Append_data$DRIVCOND[is.na(Append_data$DRIVCOND)]<-"Missing"


write.xlsx(Append_data,"Modified_KSI.xlsx") # WRITE IMPUTED DATA


Third_lvl_missing_data<-Append_data

missing_count_Third<-data.frame(sapply(Third_lvl_missing_data, function(x) sum(is.na(x))))
names(missing_count_Third)=c("No_of_Missing")
missing_count_Third$Pct_of_Missing<-round((missing_count_Third$No_of_Missing/nrow(First_lvl_Cln_Data))*100,1)
missing_count_Third<-data.frame(Field.Name=row.names(missing_count_Third),missing_count_Third)
row.names(missing_count_Third)=NULL
missing_count_Third<-merge(missing_count_Third,selected_Variables,by="Field.Name",all.x=T)
missing_count_Third<-subset(missing_count_Third,select=c(Field.Name,Description,No_of_Missing,Pct_of_Missing))


############## WRITE MISSING VALUE OUTPUT IN A EXCEL FILE WITH DIFFERENT SHEETS #########

wb<-createWorkbook()
addWorksheet(wb, "Select_Vars_1st_lvl")
addWorksheet(wb, "Reject_Vars_1st_lvl")
addWorksheet(wb, "missing_Percentage")
addWorksheet(wb, "missing_Percentage_2nd")
addWorksheet(wb, "missing_Percentage_3rd")

writeData(wb, 1, selected_Variables)
writeData(wb, 2, Rejected_Variables)
writeData(wb, 3, missing_count)
writeData(wb, 4, missing_count_Second)
writeData(wb, 5, missing_count_Third)
saveWorkbook(wb, file = "Variable_Selection_and_Missing_Report.xlsx", overwrite = TRUE)

png(filename="Total Number of Missing Per Variables_3rd_lvl.png",width = 2000,height = 2000)
# Grouped Bar Plot
aggr(Third_lvl_missing_data)
dev.off()


############################# Missing imputation by statistical method ######################

Third_lvl_missing_data[is.na(Third_lvl_missing_data)]<-"Missing"


############### remove duplicate values #################################


unique_records<-unique(subset(Third_lvl_missing_data,select=c(ACCNUM,DATE,ROAD_CLASS,District,LATITUDE,LONGITUDE,
                                                       LOCCOORD,TRAFFCTL,VISIBILITY,LIGHT,RDSFCOND,ACCLASS,
                                                       IMPACTYPE)))
dim(unique_records)
##############remove duplicate values done ##############################

setwd("/Users/shailendra/Desktop/updated_MRP/code/plots")

for(j in 1:5){
  for(i in sample(3:ncol(unique_records),size = 25,replace=T))
    try({
      print(i)
      if(class(unique_records[,i])=="character")
      {
        bar_data<-subset(unique_records,select=i)
        print(names(bar_data))
        #barplot(table(Third_lvl_missing_data[,i]),xlab = names(bar_data),ylab = "Frequency",col = 2:(dim(table(Third_lvl_missing_data[,i]))+1))
        png(filename=paste(names(bar_data),"png",sep="."),width = 2000,height = 2000)
        
        #dev.new()
        p1<-barplot(table(bar_data[,1]),xlab = names(bar_data),ylab = "Frequency",col = 2:(dim(table(Third_lvl_missing_data[,i]))+1), border =T,legend.text = F)
        ## Insert currently displayed plot to sheet 1, row 1, column 1
        print(p1) #plot needs to be showing
        
        dev.off()
        
      }
    },silent=T)
}

# Same plot as above but print in  xls sheet

for(j in 1:5){
  for(i in sample(3:ncol(unique_records),size = 25,replace=T))
    try({
      print(i)
      if(class(unique_records[,i])=="character")
      {
        bar_data<-subset(unique_records,select=i)
        print(names(bar_data))
        #barplot(table(Third_lvl_missing_data[,i]),xlab = names(bar_data),ylab = "Frequency",col = 2:(dim(table(Third_lvl_missing_data[,i]))+1))
        wb <- createWorkbook()
        addWorksheet(wb, names(bar_data), gridLines = FALSE)
        
        #dev.new()
        p1<-barplot(table(bar_data[,1]),xlab = names(bar_data),ylab = "Frequency",col = 2:(dim(table(Third_lvl_missing_data[,i]))+1), border =T,legend.text = F)
        ## Insert currently displayed plot to sheet 1, row 1, column 1
        print(p1) #plot needs to be showing
        insertPlot(wb, 1, width = 5, height = 3.5, fileType = "png", units = "in")
        saveWorkbook(wb, paste(paste("Barplot",names(bar_data),sep="_"),"xlsx",sep="."), overwrite = TRUE)
        dev.off()
        
      }
    },silent=T)
}


############################# FACTOR ANALYSIS #########################################



# data for factor analysis
#data preparation for factor analysis
Third_lvl_missing_data$ACCNUM<-as.character(Third_lvl_missing_data$ACCNUM)#Just converting ACCNUM into Character from Numeric 
data_class<-data.frame(sapply(Third_lvl_missing_data,"class")) # extract variable class for each variables
names(data_class)="Class"

data_class<-data.frame(Variables=row.names(data_class),data_class)
row.names(data_class)=NULL # omiting rownames

useful_data<-subset(data_class,Class %in% 'character' & Variables !='Month')

Factor_Analysis_Data<-subset(Third_lvl_missing_data,select=as.character(useful_data$Variables))
Factor_Analysis_Data[is.na(Factor_Analysis_Data)]<-"Missing"
Factor_Analysis_Data1<-sapply(Factor_Analysis_Data,"as.factor")




X.quanti <- splitmix(Factor_Analysis_Data1)$X.quanti # EXTRACT QUANTATIVE VARIABLES,
X.quali <- splitmix(Factor_Analysis_Data1)$X.quali    # EXTRACT QUALITATIVE VARIABLES
pca<-PCAmix(X.quanti,X.quali,ndim=4,rename.level=TRUE) # CREATING 4 DIMENSIONS FOR QUANTITIVE FACTORS
pca<-PCAmix(X.quanti,X.quali,ndim=4,graph=FALSE,rename.level=TRUE) # CREATING 4 DIMENSIONS FOR QUALITATIVE FACTORS
pca$eig                        # EXTRACT EIGEN VALUES
pca$ind$coord                   # EXTRACT SCORES FORM FACTOR ANALYSIS
pca$coef                          # EXTRACT LOADINGS
contrib_pct<-data.frame((pca$quali$contrib.pct)) # EXTRACT PERCENTAGE OF CONTRIBUTION
contrib<-data.frame((pca$quali$contrib))  # EXTRACT CONTRIBUTION NUMBERS
contrib$RowSum<-apply(contrib,1,sum)       # ROW WISE TOTAL CALCULATION
contrib$MostContributingVar<-round(contrib$RowSum/sum(contrib$RowSum)*100,1) # EXTRACT MOST CONTRIBUTING VARIABLES
contrib<-data.frame(Variables=row.names(contrib),contrib)                      
contrib<-arrange(contrib,-MostContributingVar) # ARRANGE BY CONTRIBUTION
contrib_pct$RowMax<-apply(contrib_pct,1,max)
# contrib_pct$Classification<-ifelse(contrib_pct$RowMax==contrib_pct$dim.1,"Dim1",ifelse(contrib_pct$RowMax==contrib_pct$dim.2,"Dim2",ifelse(contrib_pct$RowMax==contrib_pct$dim.3,"Dim3",
#                                                                                                                                           ifelse(contrib_pct$RowMax==contrib_pct$dim.4,"Dim4",ifelse(contrib_pct$RowMax==contrib_pct$dim.5,"Dim5","Dim6")))))

contrib_pct$Classification<-ifelse(contrib_pct$RowMax==contrib_pct$dim.1,"Dim1",
                                   ifelse(contrib_pct$RowMax==contrib_pct$dim.2,"Dim2",
                                      ifelse(contrib_pct$RowMax==contrib_pct$dim.3,"Dim3","Dim4"))) # ALLOCATING VARIABLES INTO DIFFERENT DIMENSIONS
                                                                                                                                           


contrib_pct<-data.frame(Variables=row.names(contrib_pct),contrib_pct)


library(dplyr) 
contrib_pct<-arrange(contrib_pct,Classification)

sum(contrib_pct$dim.1)




###### WRITE FACTOR ANALYSIS REPORT INTO EXCEL FILE ##

wb<-createWorkbook()
addWorksheet(wb, "Contribution")
addWorksheet(wb, "Group_Variables")
writeData(wb, 1, contrib)
writeData(wb, 2, contrib_pct)
saveWorkbook(wb, file = "Factor_Analysis_Report.xlsx", overwrite = TRUE)



######################### Visualization ACCIDENTS IN A MAP #################################################


setwd("/Users/shailendra/Desktop/updated_MRP/code/maps")
#library(plyr)
library(dplyr)
library(plotly)
library(highcharter)
library(lubridate)
library(ggthemes)
library(tidyr)
library(viridis)

col <- Append_data
str(col)

#############2. Identification of the most Collision Prone location?

# CONVERT BINARY FACTOR VARIABLES INTO 0 AND 1 BINARY CLASSES, HERE YES=1 AND NO=0 #######
######################### HERE MAPS ARE WRITTEN MANUALLY FROM PLOTS AND USING SAVE AS .PNG ################
library(ggmap)
Append_data$PEDESTRIANS.Collision<-ifelse(Append_data$PEDESTRIAN=="Yes",1,0) 
Append_data$CYCLISTS.Collision<-ifelse(Append_data$CYCLIST=="Yes",1,0)
Append_data$MOTORISTS.Collision<-ifelse(Append_data$MOTORCYCLE=="Yes",1,0)
Append_data$AUTOMOBILE.Collision<-ifelse(Append_data$AUTOMOBILE=="Yes",1,0)
Append_data$TRUCK.Collision<-ifelse(Append_data$TRUCK=="Yes",1,0)

Append_data$Total.collisions<-1

Append_data1<-unique(subset(Append_data,select=c(ACCNUM,LATITUDE,LONGITUDE,PEDESTRIANS.Collision,CYCLISTS.Collision,MOTORISTS.Collision,Total.collisions,AUTOMOBILE.Collision,TRUCK.Collision)))

df <- Append_data1 %>% select(LATITUDE,LONGITUDE,PEDESTRIANS.Collision,CYCLISTS.Collision,MOTORISTS.Collision,Total.collisions,AUTOMOBILE.Collision,TRUCK.Collision) %>% gather(type,value,3:8) %>% na.omit() %>% group_by(LATITUDE,LONGITUDE,type) %>% summarise(total=sum(value,na.rm=T)) %>% filter(total!=0)


nyc <- get_map("toronto",zoom=13,maptype = "terrain")

#png(filename="Pedestrian_Collision.png",width = 2000,height = 2000)
g1 <- ggmap(nyc)+geom_point(data=subset(df,type=="PEDESTRIANS.Collision"), 
                            aes(x=LONGITUDE, y=LATITUDE, colour=total^5),size=subset(df,type=="PEDESTRIANS.Collision")$total^2,alpha=0.4) +
  ggtitle("Pedestrians Collision")+scale_color_continuous(low = "red",  high = "black")
print(g1)
#dev.off()


g2 <- ggmap(nyc)+geom_point(data=subset(df,type=="CYCLISTS.Collision"), 
                            aes(x=LONGITUDE, y=LATITUDE, colour=total),size=1,alpha=0.2) +
  ggtitle("Cyclists Collision")+scale_color_continuous(low = "red",  high = "black")

g3 <- ggmap(nyc)+geom_point(data=subset(df,type=="MOTORISTS.Collision"), 
                            aes(x=LONGITUDE, y=LATITUDE, colour=total),size=1,alpha=0.2) +
  ggtitle("Motorists Collision")+scale_color_continuous(low = "red",  high = "black")

g3


g4 <- ggmap(nyc)+geom_point(data=subset(df,type=="Total.collisions"), 
                            aes(x=LONGITUDE, y=LATITUDE, colour=total),size=1,alpha=0.1) +
  ggtitle("Total Collisions")+scale_color_continuous(low = "red",  high = "black")

g4

g5 <- ggmap(nyc)+geom_point(data=subset(df,type=="AUTOMOBILE.Collision"), 
                            aes(x=LONGITUDE, y=LATITUDE, colour=total),size=1,alpha=0.1) +
  ggtitle("AUTOMOBILE Collision")+scale_color_continuous(low = "red",  high = "black")

g5


g6 <- ggmap(nyc)+geom_point(data=subset(df,type=="TRUCK.Collision"), 
                            aes(x=LONGITUDE, y=LATITUDE, colour=total),size=1.2,alpha=0.4) +
  ggtitle("TRUCK Collision")+scale_color_continuous(low = "red",  high = "black")

g6



############################## other plots for different types of vehicles ############################################
library(dygraphs)

########################## year month wise Vehicle accident plot ###################
library(dplyr) # used for dygraph()
library(ggplot2)

Year_Month_VEHTYPE_Plot<-function(VEHTYPE_1)
{
  dev.off()  #To close previous plot if exits
  dev.new()  #To open new plot window
  vehicle_data<-subset(Append_data,VEHTYPE %in% VEHTYPE_1)
  veh_data<-unique(subset(vehicle_data,select=c(ACCNUM,DATE,YEAR,Month)))
  #moto_data$Year_Month<-format(moto_data$DATE,"%Y-%b")
  veh_data<-subset(veh_data,YEAR %in% c(2015,2016))
  veh_data<-arrange(veh_data,DATE)
  
  vehicle_year_Month<-veh_data %>% group_by(YEAR,Month) %>% count(YEAR,Month)
  vehicle_year_Month$YEAR_Month<-as.Date(paste(vehicle_year_Month$YEAR,vehicle_year_Month$Month,"01",sep = "-"),format = "%Y-%B-%d")  #To convert into Date format, day is fixed as "01" then concatinate with year month.
  vehicle_year_Month<-data.frame(arrange(vehicle_year_Month,YEAR_Month))
  vehicle_year_Month$YEAR<-as.character(vehicle_year_Month$YEAR)
  
  
  vehicle_year_Month$Month_Number<-as.numeric(format(vehicle_year_Month$YEAR_Month,"%m"))
  
  return(list(ggplot(vehicle_year_Month, aes(reorder(Month,Month_Number), n, fill = YEAR)) + 
                geom_bar(stat="identity", position = "dodge") + 
                scale_fill_brewer(palette = "Set2")+scale_x_discrete(name ="Month")+scale_y_discrete(name ="Number of Accident"),vehicle_data))
  
  dev.off()
  
}


Year_Month_VEHTYPE_Plot(c("Motorcycle"))

Year_Month_VEHTYPE_Plot(c("Truck-Tractor","Truck - Dump","Pick Up Truck","Truck - Open","Truck - Tank",
                          "Truck - Closed (Blazer, etc)","Truck (other)","Tow Truck"))
Year_Month_VEHTYPE_Plot(c("Automobile, Station Wagon","Taxi","Passenger Van","Delivery Van","Pick Up Truck","Tow Truck"))
Year_Month_VEHTYPE_Plot(c("Bicycle","Moped"))

#cars, station wagons, taxis, passenger vans, delivery vans, pickup trucks, tow trucks, SUVs

########################### year-month invtype ################################

Year_Month_INVTYPE_Plot<-function(INVTYPE_1)
{
  dev.off()
  dev.new()
  vehicle_data<-subset(Append_data,INVTYPE %in% INVTYPE_1)
  veh_data<-unique(subset(vehicle_data,select=c(ACCNUM,DATE,YEAR,Month)))
  #moto_data$Year_Month<-format(moto_data$DATE,"%Y-%b")
  veh_data<-subset(veh_data,YEAR %in% c(2015,2016))
  veh_data<-arrange(veh_data,DATE)
  
  vehicle_year_Month<-veh_data %>% group_by(YEAR,Month) %>% count(YEAR,Month)
  vehicle_year_Month$YEAR_Month<-as.Date(paste(vehicle_year_Month$YEAR,vehicle_year_Month$Month,"01",sep = "-"),format = "%Y-%B-%d")
  vehicle_year_Month<-data.frame(arrange(vehicle_year_Month,YEAR_Month))
  vehicle_year_Month$YEAR<-as.character(vehicle_year_Month$YEAR)
  
  
  vehicle_year_Month$Month_Number<-as.numeric(format(vehicle_year_Month$YEAR_Month,"%m"))
  return(list(ggplot(vehicle_year_Month, aes(reorder(Month,Month_Number), n, fill = YEAR)) + 
                geom_bar(stat="identity", position = "dodge") + 
                scale_fill_brewer(palette = "Set2")+scale_x_discrete(name ="Month")+scale_y_discrete(name ="Number of Accident"),vehicle_data))
  dev.off()
}


Year_Month_INVTYPE_Plot(c("Pedestrian","Wheelchair","In-Line Skater"))

#Pedestrian->skateboard,skates,in-line skates,scooter,segway,stroller,wheelchair.


####################################################################


############################################ year wise 10 years trend accident plot #######################

################################### TREND VEHTYPE ##########################################


Trend_VEHTYPE<-function(VEHTYPE_2)
{
  dev.off()
  dev.new()
  Vehicle_data<-subset(data,VEHTYPE %in% VEHTYPE_2)
  
  #Truck_data<-subset(data,AUTOMOBILE %in% c("Yes"))
  
  vec_data<-unique(subset(Vehicle_data,select=c(ACCNUM,YEAR)))
  Vehicle_year<-vec_data %>% group_by(YEAR) %>% count(YEAR)
  
  
  Vehicle_year<-arrange(Vehicle_year,YEAR)
  
  Vehicle_year$YEAR<-as.Date(paste(Vehicle_year$YEAR,"01","01",sep = "-"),format = "%Y-%m-%d")
  library(xts)
  library(dygraphs)
  Vehicle_year<-xts(Vehicle_year,order.by = Vehicle_year$YEAR)
  return(dygraph(Vehicle_year, main = "Cyclist Collision in Toronto") %>% 
           dyRangeSelector(fillColor = "rgb(500,200,300)",strokeColor = "rgb(500,200,300)"))
  
}


Trend_VEHTYPE(c("Motorcycle"))

Trend_VEHTYPE(c("Truck-Tractor","Truck - Dump","Pick Up Truck","Truck - Open","Truck - Tank",
                "Truck - Closed (Blazer, etc)","Truck (other)","Tow Truck"))
Trend_VEHTYPE(c("Automobile, Station Wagon","Taxi","Passenger Van","Delivery Van","Pick Up Truck","Tow Truck"))
Trend_VEHTYPE(c("Bicycle","Moped"))


################################### TREND INVTYPE ##########################################
INVTYPE_2=c("Pedestrian","Wheelchair","In-Line Skater")
Trend_INVTYPE<-function(INVTYPE_2)
{
  dev.off()
  dev.new()
  Vehicle_data<-subset(data,INVTYPE %in% INVTYPE_2)
  
  
  vec_data<-unique(subset(Vehicle_data,select=c(ACCNUM,YEAR)))
  Vehicle_year<-vec_data %>% group_by(YEAR) %>% count(YEAR)
  
  
  Vehicle_year<-arrange(Vehicle_year,YEAR)
  
  Vehicle_year$YEAR_Month<-as.Date(paste(Vehicle_year$YEAR,"01","01",sep = "-"),format = "%Y-%m-%d")
  
  
  library(xts)
  library(dygraphs)
  Vehicle_year<-xts(Vehicle_year,order.by =Vehicle_year$YEAR_Month)
  Vehicle_year<-Vehicle_year[,-3]
  row.names(Vehicle_year)=Vehicle_year$YEAR
  Vehicle_year<-Vehicle_year[,-1]
  
  return(dygraph(Vehicle_year, main = "Cyclist Collision in Toronto") %>% 
           dyRangeSelector(fillColor = "rgb(500,200,300)",strokeColor = "rgb(500,200,300)"))
  
}


Trend_INVTYPE(c("Pedestrian","Wheelchair","In-Line Skater"))

################################ End Trend Graph ######################################## 



######################################## Weight of Evidence(WOE) part ##########################
names(Append_data)

variable_imp_data<-subset(Append_data,select=c(ACCNUM,ROAD_CLASS,LOCCOORD,TRAFFCTL,VISIBILITY,LIGHT,RDSFCOND,CYCLIST,PEDESTRIAN,AUTOMOBILE,MOTORCYCLE,TRUCK,SPEEDING,AG_DRIV,REDLIGHT,ALCOHOL,DISABILITY))
variable_imp_data[is.na(variable_imp_data)]<-"Missing"
vid_df <- unique(data.frame(variable_imp_data))
vid_df<-vid_df[,-1]  #to drop ACCNUM; no any significance for accident impact, only used for unique value

######################################### WOE FOR Cyclists #################################


ksl<-vid_df[,c(1:6,7,12:16)]
names(ksl) # print names of those variables that I have used
ksl$CYCLIST<-ifelse(ksl$CYCLIST=='Yes',1,0)


#####       IMPORTANT: 
#####                  1. Use R version >= 3.1.0
#####                  2. Open R Studio with "Run as Administrator" option - To be able to "install new packages"
#####                  3. Read through carefully to change:
#####                     i.   Input data path
#####                     ii.  Response/Target variable name
#####                     iii. Continuous & Categorical variable list in Flatfile OR Direct input names
#####                     iv.  Install "woe" package (one time) and include library
#####                     v.   Install "sqldf" package (one time) and include library
#####                     vi.  Output data path



# res_data<-read.csv("C:\\Users\\prm\\Desktop\\iv data\\75dfb018-0408-4f0a-a13f-93c7144b74db_iv_dump.csv",header = TRUE, sep = ",")
# res_data<-subset(res_data,select=-X)
indat = ksl

#####   Response/Target variable name in the "Input data"
respvar = c("CYCLIST")


IV<-function(indat,respvar)
{

varlist<-data.frame()
for(i in 1:ncol(indat))
{
  lst<-data.frame(Name=names(indat)[i],Type=ifelse(class(indat[,i])==c("character")|class(indat[,i])==c("factor"),2,1))
  varlist<-rbind(varlist,lst)
  varlist<-unique(varlist)
}

print("Test 3")

#####   Input Variables from Flat file: File contains Variable Name & Type (1=Continuous AND 2=Categorical)

cont = subset(varlist,Type == '1')
contvars = as.vector(cont$Name)
catg = subset(varlist,Type == '2')
catgvars = as.vector(catg$Name)

#############################################   OR   ######################################################

#####   Input Variables from Direct inputname: Continuous AND Categorical, two separate lists
# contvars = c("age","negative_balance")
# catgvars = c("coop_bank","local_market_current","mnths_18yr_bin","years_enrolment_bin")

#####   Install "woe" package (one time) and include library
#install.packages("woe")
library(woe)

#indat<-indat[1:1000,]

#####   For reference/input understanding only: No need to uncomment this portion

# woe(Data, Independent, Continuous, Dependent, C_Bin, Bad, Good)
# Data : Name of Data Set
# Independent : Name of the Independent Variable
# Continuous : True if the variable is continuous, False if variable is Ordinal or Nominal
# Dependent : Name of the Targer Variable
# C_Bin : Count of Bins to be computed -- Default 10 works
# Bad : Which categorical variable do you want to be bad
# Good : Which categorical variable do you want to be good

#####   Continuous variable loop: Binning and WOE & IV calculation

#####   Categorical variable loop: WOE & IV calculation
out_woe<-data.frame()
for (i in 1:length(catgvars))
{  
  var_working = catgvars[i]
  
  eval(parse(text=paste(paste("indat$",var_working,sep="")," = ",paste("as.character(indat$",var_working,")",sep=""))))
  
  out <- woe(Data=indat,var_working,FALSE,respvar,10,Bad=0,Good=1)
  out$MIN = 0                 #To match continuous output
  out$MAX = 0                 #To match continuous output
  out$Type = "Categorical"
  out$Varname = var_working
  colnames(out)[which(names(out) == "IV")] <- "MIV"
  
  out_woe = rbind(out_woe,out)
  
  out_woe$MIV = ifelse(is.na(out_woe$WOE)|out_woe$WOE == Inf|out_woe$WOE == -Inf,0,out_woe$MIV)
  out_woe$WOE = ifelse(is.na(out_woe$WOE)|out_woe$WOE == Inf|out_woe$WOE == -Inf,0,out_woe$WOE)
}

print("Test 4")

# R> df[ , -which(names(df) %in% c("z","u"))]
# R> subset(df, select=-c(z,u))

#####   Install "sqldf" package (one time) and include library
#install.packages("sqldf")
library(sqldf)

out_iv <- sqldf(' select Varname,Type,sum(MIV) as IV
                from out_woe
                group by Varname,Type
                order by Type,Varname
                ')
out_iv$Importance = ifelse(out_iv$IV > 0.3,"High",ifelse((0.2 < out_iv$IV & out_iv$IV <= 0.3),"Medium","Low"))
out_iv<-subset(out_iv,select=-Type)
return(list(out_woe=out_woe,out_iv=out_iv))
}

CYCLIST_IV<-IV(indat,respvar)
CYCLIST_IV$out_iv
################################## RANDOMFOREST ########################################
ksl$CYCLIST<-as.factor(ksl$CYCLIST)
ksl1<-data.frame(sapply(ksl,"as.factor"))
rf<-randomForest(CYCLIST~ROAD_CLASS+LOCCOORD+TRAFFCTL+VISIBILITY+LIGHT+RDSFCOND+SPEEDING+AG_DRIV+REDLIGHT+ALCOHOL+DISABILITY,data = ksl1,ntree=500)
library(caret)
imp_vars<-data.frame(varImp(rf))
imp_vars<-data.frame(Variables=row.names(imp_vars),imp_vars)
imp_vars<-arrange(imp_vars,-Overall)



varImpPlot(rf,type=2)

wb<-createWorkbook()
addWorksheet(wb, "CYCLIST_WOE")
addWorksheet(wb, "out_iv")
addWorksheet(wb, "VarImp_RF")
addWorksheet(wb, "VarImp_Plot", gridLines = FALSE) 
insertPlot(wb, 4, width = 5, height = 3.5, fileType = "png", units = "in")
writeData(wb, 1, CYCLIST_IV$out_woe)
writeData(wb, 2, CYCLIST_IV$out_iv)
writeData(wb, 3, imp_vars)

saveWorkbook(wb, file = "VARIABLE_SELECTION_CYCLIST.xlsx", overwrite = TRUE)




######################################### WOE FOR PEDESTRIAN #################################


ksl<-vid_df[,c(1:6,8,12:16)]
names(ksl)
ksl$PEDESTRIAN<-ifelse(ksl$PEDESTRIAN=='Yes',1,0)

indat = ksl

#####   Response/Target variable name in the "Input data"
respvar = c("PEDESTRIAN")

PEDESTRIAN_IV<-IV(indat,respvar)
PEDESTRIAN_IV$out_iv

################################## RANDOMFOREST ########################################


ksl$PEDESTRIAN<-as.factor(ksl$PEDESTRIAN)
ksl1<-data.frame(sapply(ksl,"as.factor"))
rf<-randomForest(PEDESTRIAN~ROAD_CLASS+LOCCOORD+TRAFFCTL+VISIBILITY+LIGHT+RDSFCOND+SPEEDING+AG_DRIV+REDLIGHT+ALCOHOL+DISABILITY,data = ksl1,ntree=500)
library(caret)
imp_vars<-data.frame(varImp(rf))
imp_vars<-data.frame(Variables=row.names(imp_vars),imp_vars)
imp_vars<-arrange(imp_vars,-Overall)




varImpPlot(rf,type=2)

wb<-createWorkbook()
addWorksheet(wb, "PEDESTRIAN_WOE")
addWorksheet(wb, "out_iv")
addWorksheet(wb, "VarImp_RF")
addWorksheet(wb, "VarImp_Plot", gridLines = FALSE) 
insertPlot(wb, 4, width = 5, height = 3.5, fileType = "png", units = "in")
writeData(wb, 1, PEDESTRIAN_IV$out_woe)
writeData(wb, 2, PEDESTRIAN_IV$out_iv)
writeData(wb, 3, imp_vars)

saveWorkbook(wb, file = "VARIABLE_SELECTION_PEDESTRIAN.xlsx", overwrite = TRUE)


######################################### WOE FOR AUTOMOBILE #################################


ksl<-vid_df[,c(1:6,9,12:16)]
names(ksl)
ksl$AUTOMOBILE<-ifelse(ksl$AUTOMOBILE=='Yes',1,0)

indat = ksl

respvar = c("AUTOMOBILE")


AUTOMOBILE_IV<-IV(indat,respvar)
AUTOMOBILE_IV$out_iv

################################## RANDOMFOREST ########################################

ksl$AUTOMOBILE<-as.factor(ksl$AUTOMOBILE)
ksl1<-data.frame(sapply(ksl,"as.factor"))
rf<-randomForest(AUTOMOBILE~ROAD_CLASS+LOCCOORD+TRAFFCTL+VISIBILITY+LIGHT+RDSFCOND+SPEEDING+AG_DRIV+REDLIGHT+ALCOHOL+DISABILITY,data = ksl1,ntree=500)
library(caret)
imp_vars<-data.frame(varImp(rf))
imp_vars<-data.frame(Variables=row.names(imp_vars),imp_vars)
imp_vars<-arrange(imp_vars,-Overall)




varImpPlot(rf,type=2)

wb<-createWorkbook()
addWorksheet(wb, "AUTOMOBILE_WOE")
addWorksheet(wb, "out_iv")
addWorksheet(wb, "VarImp_RF")
addWorksheet(wb, "VarImp_Plot", gridLines = FALSE) 
insertPlot(wb, 4, width = 5, height = 3.5, fileType = "png", units = "in")
writeData(wb, 1, AUTOMOBILE_IV$out_woe)
writeData(wb, 2, AUTOMOBILE_IV$out_iv)
writeData(wb, 3, imp_vars)

saveWorkbook(wb, file = "VARIABLE_SELECTION_AUTOMOBILE.xlsx", overwrite = TRUE)


######################################### WOE FOR MOTORCYCLE #################################


ksl<-vid_df[,c(1:6,10,12:16)]
names(vid_df)
ksl$MOTORCYCLE<-ifelse(ksl$MOTORCYCLE=='Yes',1,0)

indat = ksl

#####   Response/Target variable name in the "Input data"
respvar = c("MOTORCYCLE")


MOTORCYCLE_IV<-IV(indat,respvar)
MOTORCYCLE_IV$out_iv

################################## RANDOMFOREST ########################################


ksl$MOTORCYCLE<-as.factor(ksl$MOTORCYCLE)
ksl1<-data.frame(sapply(ksl,"as.factor"))
rf<-randomForest(MOTORCYCLE~ROAD_CLASS+LOCCOORD+TRAFFCTL+VISIBILITY+LIGHT+RDSFCOND+SPEEDING+AG_DRIV+REDLIGHT+ALCOHOL+DISABILITY,data = ksl1,ntree=500)
library(caret)
imp_vars<-data.frame(varImp(rf))
imp_vars<-data.frame(Variables=row.names(imp_vars),imp_vars)
imp_vars<-arrange(imp_vars,-Overall)

varImpPlot(rf,type=2)

wb<-createWorkbook()
addWorksheet(wb, "MOTORCYCLE_WOE")
addWorksheet(wb, "out_iv")
addWorksheet(wb, "VarImp_RF")
addWorksheet(wb, "VarImp_Plot", gridLines = FALSE) 
insertPlot(wb, 4, width = 5, height = 3.5, fileType = "png", units = "in")
writeData(wb, 1, MOTORCYCLE_IV$out_woe)
writeData(wb, 2, MOTORCYCLE_IV$out_iv)
writeData(wb, 3, imp_vars)

saveWorkbook(wb, file = "VARIABLE_SELECTION_MOTORCYCLE.xlsx", overwrite = TRUE)


######################################### WOE FOR TRUCK #################################

ksl<-vid_df[,c(1:6,11,12:16)]
names(vid_df)
ksl$TRUCK<-ifelse(ksl$TRUCK=='Yes',1,0)

indat = ksl

#####   Response/Target variable name in the "Input data"
respvar = c("TRUCK")
TRUCK_IV<-IV(indat,respvar)
TRUCK_IV$out_iv
################################## RANDOMFOREST ########################################
ksl$TRUCK<-as.factor(ksl$TRUCK)
ksl1<-data.frame(sapply(ksl,"as.factor"))
rf<-randomForest(TRUCK~ROAD_CLASS+LOCCOORD+TRAFFCTL+VISIBILITY+LIGHT+RDSFCOND+SPEEDING+AG_DRIV+REDLIGHT+ALCOHOL+DISABILITY,data = ksl1,ntree=500)
library(caret)
imp_vars<-data.frame(varImp(rf))
imp_vars<-data.frame(Variables=row.names(imp_vars),imp_vars)
imp_vars<-arrange(imp_vars,-Overall)
varImpPlot(rf,type=2)

wb<-createWorkbook()
addWorksheet(wb, "TRUCK_WOE")
addWorksheet(wb, "out_iv")
addWorksheet(wb, "VarImp_RF")
addWorksheet(wb, "VarImp_Plot", gridLines = FALSE) 
insertPlot(wb, 4, width = 5, height = 3.5, fileType = "png", units = "in")
writeData(wb, 1, TRUCK_IV$out_woe)
writeData(wb, 2, TRUCK_IV$out_iv)
writeData(wb, 3, imp_vars)

saveWorkbook(wb, file = "VARIABLE_SELECTION_TRUCK.xlsx", overwrite = TRUE)
########################################### correlation plot #########################

ksl<-vid_df[,c(1:6,11,12:16)]
names(vid_df)
ksl$TRUCK<-ifelse(ksl$TRUCK=='Yes',1,0)
vid_df1<-data.frame(sapply(vid_df,"as.factor"))
vid_df1[] <- lapply(vid_df1,as.integer)
library(sjPlot)
sjp.corr(vid_df1) # correlation plot
sjt.corr(vid_df1)


