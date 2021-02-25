##################################################################################################################################################
#### Package Libraries ####
# Set working directory folder
setwd('C:/Users/medsleea/OneDrive - University of Leeds/Dissertation/GEOG5160_Diss/Concise Files/')
#Loading Data Section
library(dplyr) # load dplyr package for joining datasets
library(MASS)  # load the MASS package for chi-squared
library(RVAideMemoire) #load the RVAideMemoire package for cramers test
library(ipfp) # load the ipfp package for IPF process
library(hydroGOF) #load the hydroGOF package for RMSE
library(reshape2) #load reshape for joining tables
library(tidyverse) 
library(foreach)
library(data.table)
# functions for spatial microsimulation, inc. int_trs, and TAE function (code from 'Spatial Microsimulation with R', Lovelace and Dumont, 2016)
source("functions.R") 

##################################################################################################################################################
#### Data Cleaning of USD and Census Dataset ####
# Census 2011 Constraint variables chosen as: age structure, sex, long-term disability, travel method to work, employment sector.
sex <- read.csv("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\Census Data\\Sex\\QS104ew_2011_oa\\QS104EWDATA.csv")
ltd <- read.csv("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\Census Data\\Health_Problems\\QS303ew_2011_oa\\QS303EWDATA.csv")
travel_work <- read.csv("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\Census Data\\Method_travel_Work\\QS701ew_2011_oa\\QS701EWDATA.csv")
household <- read.csv("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\Census Data\\Household_Comp\\KS105ew_2011_oa\\KS105EWDATA.csv")
# Create full Census dataset
Census <- merge(ltd, sex, by="GeographyCode")
Census <- merge(Census, travel_work, by="GeographyCode")
#Census <- merge(Census, household, by="GeographyCode")

rm(sex, ltd, travel_work, household) #tidy

# Subset the Census dataset to only Leeds (area of Study) 
lkup <- fread("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\Updated Dissertation Code\\data\\lookup.csv") # import the lookup of Geography Code (LSOA) to LSOA Name and LA Name 
lkup <- subset(lkup, select=c(LSOA11CD,LSOA11NM, LAD17CD, LAD17NM)) #subset to variables of interest

Census <- merge(Census, lkup, by.x="GeographyCode", by.y="LSOA11CD") #merge data together 
# Subset data to only data in Leeds Local Authority 
Census<- Census[Census$LAD17NM=="Leeds",]
# Remove duplicates (error in data set imported?)
Census <- Census[!duplicated(Census$GeographyCode),]
rm(lkup) #tidy
# Clean dataset for use 
Census <- subset(Census, select=c(GeographyCode, LSOA11NM, LAD17CD, LAD17NM, QS303EW0001, QS701EW0001, QS701EW0002, QS701EW0003, QS701EW0004, QS701EW0005, QS701EW0006, QS701EW0007, QS701EW0008,  QS701EW0009, QS701EW0010, QS701EW0011, QS701EW0012, QS701EW0013, QS303EW0002, QS303EW0003, QS303EW0004,QS104EW0002, QS104EW0003)) # Sort out the LTD VARIABLES
colnames(Census) <- (c("LSOA", "LSOA_Name", "LA_Code", "LA_Name", "POP_TOTAL", "Travel_Total", "Mainly_at_home", "Underground_Metro_LightRail_Tram", "Train", "Bus_Minibus_or_Coach", "Taxi", "Motorcycle", "Driving_car_or_van" ,"Passenger_car_or_van", "Bicycle", "On_foot", "Other", "Not_Employed", "Act_Lim_Lot", "Act_Lim_little", "Act_notlim", "Sexm", "Sexf"))                       

# LTD into two categories 
Census$LTD <- Census$Act_Lim_Lot+Census$Act_Lim_little
Census$NoLTD <- Census$Act_notlim

# Travel to work variable was subsetted into five categories: drive car or van, passenger in a car or van, TActive which included cyclists and those on foot, TPublicTrans which included train, bus, coach, or underground users and TOther. 
Census$TDCV <- Census$Driving_car_or_van
Census$TPCV <- Census$Passenger_car_or_van
Census$TActive <- Census$Bicycle + Census$On_foot 
Census$TPublicTrans <- Census$Train + Census$Bus_Minibus_or_Coach + Census$Underground_Metro_LightRail_Tram
Census$Tother <- Census$Other + Census$Not_Employed 

# Rescale Travel Variables to Total LSOA Populations 
Census$TMH <- round(Census$POP_TOTAL * (Census$Mainly_at_home/ Census$Travel_Total),0)
Census$TDCV <- round(Census$POP_TOTAL * (Census$TDCV/ Census$Travel_Total),0)
Census$TPCV <- round(Census$POP_TOTAL * (Census$TPCV/ Census$Travel_Total), 0)
Census$TActive <- round(Census$POP_TOTAL * (Census$TActive/ Census$Travel_Total), 0) 
Census$TPublicTrans <- round(Census$POP_TOTAL * (Census$TPublicTrans/ Census$Travel_Total), 0) 
Census$Tother <- round(Census$POP_TOTAL * (Census$Tother/ Census$Travel_Total), 0)
Census$Diff <- Census$POP_TOTAL - (Census$TMH + Census$TDCV + Census$TPCV + Census$TActive + Census$TPublicTrans + Census$Tother) # check difference between rescaled and actual LSOA Pop Total

# Subset to Smaller dataset
Census2 <- subset(Census,select=c(LSOA,LSOA_Name, LA_Code, LA_Name, POP_TOTAL, TMH, TDCV, TPCV, TActive, TPublicTrans, Tother, Diff, LTD, NoLTD, Sexm, Sexf))

# Randomly apportion difference across the categories (avoid bias)
# Check sums of Transport Census + Diff is equal to LSOA Population
before_sum <- rowSums(Census2[,6:11]) + Census2$Diff == Census2$POP_TOTAL
sum(before_sum=="FALSE") # check if any are false
# Define assignment function
assign_diff <- function(.df)
{
  # .df = test_df
  foreach(i = 1:nrow(.df),
          .combine = "rbind") %do%
    {
      # Select columns totals for addition
      col_totals <- sample(6:11, .df$Diff[i], replace = TRUE)
      
      # Sum totals
      new_additions <- col_totals %>% 
        enframe() %>% 
        group_by(value) %>% 
        summarise(Freq = n())
      
      # Add to columns
      .df[i, new_additions$value] <- .df[i, new_additions$value] + c(new_additions$Freq)
      
      # Return row
      .df[i,]
    }
}

# Fire up assignment
set.seed(123) # Important for reproducibility
output_df <- assign_diff(Census2)

# Check sums
replacement_sum <- (rowSums(output_df[,6:11]) == output_df$POP_TOTAL)
sum(replacement_sum=="FALSE") # check if any are false
Census <- subset(output_df, select=-c(Diff)) # remove difference column as scaled now

# Write out completed exported csv of Census Dataset
write.csv(Census, "C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\Updated Dissertation Code\\data\\Census_R.csv") #write out file

rm(Census2, output_df, before_sum, replacement_sum) #tidy

##################################################################################################################################################
#### Reformat USd variables to match Census. ####
# USd Constraint variables chosen as: sex, age, long-term disability, travel method to work, total annual income, employment sector. 
#install.packages("foreign")
library(foreign)
USd_9 <- read.spss("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\6614spss_56C747972BBC714FCBAAB572F2629DCB_V1\\UKDA-6614-spss\\spss\\spss24\\ukhls_w9\\i_indresp.sav", to.data.frame=TRUE)
# Subset to keep only sex, age, long-term disability, travel method to work, time travelling to work, total annual income, total monthly income, employment sector. NEED TO REMEMBER VARIABLE NAMES 
USd_9 <- subset(USd_9, select=c(i_hidp, pidp,i_sex, i_dvage, i_health, i_worktrav,i_jbttwt, i_paygu_dv, i_jbsoc10_cc, i_jsttwtb, i_jspl, i_nadoptch, i_nnatch, i_npns_dv, i_ngrp_dv, i_nnssib_dv))

# Long-term disability reclassified into two brackets 
USd_9$LTD <- 0 #set up variable
USd_9$NoLTD <- 0 #set up variable
USd_9$LTD[USd_9$i_health=="Yes"]<- 1 #set to 1 if health condition is yes
USd_9$NoLTD[USd_9$i_health=="No"]<- 1 #set to 1 if health condition is no

# Employment sector reclassified into 19 brackets
#USd_9$i_jbsoc10_cc <- as.character(USd_9$i_jbsoc10_cc)
USd_9$Employment.Sector <- NA
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="missing" | USd_9$i_jbsoc10_cc=="refusal" | USd_9$i_jbsoc10_cc =="don't know"] <- NA 
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Food Preparation and Hospitality Trades"| USd_9$i_jbsoc10_cc=="Other Elementary Service Occupations"| USd_9$i_jbsoc10_cc=="Managers and Proprietors in Hospitality and Leisure Services"]<-"Accomodation and Food Service"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Other Administrative Occupations" | USd_9$i_jbsoc10_cc=="Business, Research and Administrative Professionals" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Finance" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Records" | USd_9$i_jbsoc10_cc=="Functional Managers and Directors" | USd_9$i_jbsoc10_cc=="Secretarial and Related Occupations" | USd_9$i_jbsoc10_cc=="Elementary Administrative Occupations" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Office Managers and Supervisors" | USd_9$i_jbsoc10_cc=="Quality and Regulatory Professionals"] <- "Administrative and Support Service"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Agricultural and Related Trades" | USd_9$i_jbsoc10_cc=="Elementary Agricultural Occupations" | USd_9$i_jbsoc10_cc=="Managers and Proprietors in Agriculture Related Services"] <-"Agriculture, Forestry and Fishing"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Plant and Machine Operatives" | USd_9$i_jbsoc10_cc=="Elementary Process Plant Occupations" | USd_9$i_jbsoc10_cc=="Elementary Construction Occupations" | USd_9$i_jbsoc10_cc=="Construction and Building Trades Supervisors" | USd_9$i_jbsoc10_cc=="Assemblers and Routine Operatives" | USd_9$i_jbsoc10_cc=="Construction and Building Trades" | USd_9$i_jbsoc10_cc=="Process Operatives" | USd_9$i_jbsoc10_cc=="Construction Operatives"]<-"Construction"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="inapplicable"]<- "Economically Inactive"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Teaching and Educational Professionals" | USd_9$i_jbsoc10_cc=="Artistic, Literary and Media Occupations" | USd_9$i_jbsoc10_cc=="Librarians and Related Professionals"] <- "Education" 
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Business, Finance and Related Associate Professionals" | USd_9$i_jbsoc10_cc=="Financial Institution Managers and Directors"] <- "Financial and Insurance"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Welfare Professionals" | USd_9$i_jbsoc10_cc=="Caring Personal Services" | USd_9$i_jbsoc10_cc=="Health and Social Services Managers and Directors" | USd_9$i_jbsoc10_cc=="Childcare and Related Personal Services"| USd_9$i_jbsoc10_cc=="Nursing and Midwifery Professionals" | USd_9$i_jbsoc10_cc=="Health Associate Professionals" | USd_9$i_jbsoc10_cc=="Health Professionals" | USd_9$i_jbsoc10_cc=="Therary Professionals" | USd_9$i_jbsoc10_cc=="Hairdressers and Related Services" | USd_9$i_jbsoc10_cc=="Managers and Proprietors in Health and Care Services"] <-"Human Health and Social Work"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Information Technology Technicians" | USd_9$i_jbsoc10_cc=="Electrical and Electronic Trades"] <- "Information and Communication"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Metal Forming, Welding and Related Trades" | USd_9$i_jbsoc10_cc=="Engineering Professionals" | USd_9$i_jbsoc10_cc=="Metal Machining, Fitting and Instrument Making Trades" | USd_9$i_jbsoc10_cc=="Textiles and Garments Trades" | USd_9$i_jbsoc10_cc=="Building Finishing Trades" | USd_9$i_jbsoc10_cc=="Skilled Metal, Electrical and Electronic Trades Supervisors"] <- "Manufacturing"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Elementary Cleaning Occupations" | USd_9$i_jbsoc10_cc=="Managers and Proprietors in Other Services" | USd_9$i_jbsoc10_cc=="Sports and Fitness Occupations" | USd_9$i_jbsoc10_cc=="Other Skilled Trades" | USd_9$i_jbsoc10_cc=="Animal Care and Control Services" | USd_9$i_jbsoc10_cc=="Conservation and Environment Professionals" | USd_9$i_jbsoc10_cc=="Design Occupations" | USd_9$i_jbsoc10_cc=="Printing Trades" | USd_9$i_jbsoc10_cc=="Media Professionals" | USd_9$i_jbsoc10_cc=="Housekeeping and Related Services" | USd_9$i_jbsoc10_cc=="Cleaning and Housekeeping Managers and Supervisors" | USd_9$i_jbsoc10_cc=="Chief Executives and Senior Officials" | USd_9$i_jbsoc10_cc=="Conservation and Environmental Associate Professionals"]<-"Other"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Research and Development Managers" | USd_9$i_jbsoc10_cc=="Science, Engineering and Production Technicians" | USd_9$i_jbsoc10_cc=="Natural and Social Science Professionals"]<-"Professional and Scientific and Technical Activities"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Protective Service Occupations"| USd_9$i_jbsoc10_cc=="Public Services and Other Associate Professionals" | USd_9$i_jbsoc10_cc=="Elementary Security Occupations" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Government and Related Organisations" | USd_9$i_jbsoc10_cc=="Legal Professionals" | USd_9$i_jbsoc10_cc=="Legal Associate Professionals" | USd_9$i_jbsoc10_cc=="Senior Officers in Protective Services"]<- "Public Administration and Defence"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Draughtspersons and Related Architectural Technicians"| USd_9$i_jbsoc10_cc=="Architects, Town Planners and Surveyors" | USd_9$i_jbsoc10_cc=="Welfare and Housing Associate Professionals"]<- "Real Estate Services"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Mobile Machine Drivers and Operatives" | USd_9$i_jbsoc10_cc=="Road Transport Drivers" | USd_9$i_jbsoc10_cc=="Transport Associate Professionals" | USd_9$i_jbsoc10_cc=="Production Managers and Directors" | USd_9$i_jbsoc10_cc=="Elementary Storage Occupations" | USd_9$i_jbsoc10_cc=="Managers and Directors in Transport and Logistics" | USd_9$i_jbsoc10_cc=="Other Drivers and Transport Operatives"]<- "Transport and Storage"
USd_9$Employment.Sector[USd_9$i_jbsoc10_cc=="Customer Service Occupations" | USd_9$i_jbsoc10_cc=="Sales Assistants and Retail Cashiers" | USd_9$i_jbsoc10_cc=="Sales, Marketing and Related Associate Professionals" | USd_9$i_jbsoc10_cc=="Elementary Sales Occupations" | USd_9$i_jbsoc10_cc=="Sales Related Occupations" | USd_9$i_jbsoc10_cc=="Leisure and Travel Services" | USd_9$i_jbsoc10_cc=="Vehicle Trades" | USd_9$i_jbsoc10_cc=="Sales Supervisors" | USd_9$i_jbsoc10_cc=="Managers and Directors in Retail and Wholesale" | USd_9$i_jbsoc10_cc=="Customer Service Managers and Supervisors"]<- "Whole and Retail Trade, Repair of Motor Vehicles"

# Group Employment Sectors into 19 as per Census 
USd_9$Agr_Forest_Fishing <- 0
USd_9$Mining_Quarrying <- 0
USd_9$Manufacturing <- 0
USd_9$Electricity <- 0 
USd_9$Gas_Steam_AirCon <- 0
USd_9$Water_Waste <- 0 
USd_9$Construction <- 0 
USd_9$Whole_Retail_Trade <- 0 
USd_9$Transport_Storage <- 0 
USd_9$Accomodation <- 0
USd_9$IT <- 0 
USd_9$Finance_Insurance <- 0
USd_9$Real_Estate <- 0 
USd_9$Prof_Science_Tech <- 0 
USd_9$Admin <- 0
USd_9$Public_Admin_Defence <- 0 
USd_9$Education <- 0
USd_9$Human_Social_Work <- 0 
USd_9$Other <- 0
USd_9$Eco_Inactive <- 0 

USd_9$i_jbsoc10_cc[USd_9$i_jbsoc10_cc=="missing" | USd_9$i_jbsoc10_cc=="refusal" | USd_9$i_jbsoc10_cc =="don't know"] <- NA 
USd_9$Accomodation[USd_9$i_jbsoc10_cc=="Food Preparation and Hospitality Trades"| USd_9$i_jbsoc10_cc=="Other Elementary Service Occupations"| USd_9$i_jbsoc10_cc=="Managers and Proprietors in Hospitality and Leisure Services"]<-1
USd_9$Admin[USd_9$i_jbsoc10_cc=="Other Administrative Occupations" | USd_9$i_jbsoc10_cc=="Business, Research and Administrative Professionals" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Finance" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Records" | USd_9$i_jbsoc10_cc=="Functional Managers and Directors" | USd_9$i_jbsoc10_cc=="Secretarial and Related Occupations" | USd_9$i_jbsoc10_cc=="Elementary Administrative Occupations" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Office Managers and Supervisors" | USd_9$i_jbsoc10_cc=="Quality and Regulatory Professionals"] <- 1
USd_9$Agr_Forest_Fishing[USd_9$i_jbsoc10_cc=="Agricultural and Related Trades" | USd_9$i_jbsoc10_cc=="Elementary Agricultural Occupations" | USd_9$i_jbsoc10_cc=="Managers and Proprietors in Agriculture Related Services"] <-1
USd_9$Construction[USd_9$i_jbsoc10_cc=="Plant and Machine Operatives" | USd_9$i_jbsoc10_cc=="Elementary Process Plant Occupations" | USd_9$i_jbsoc10_cc=="Elementary Construction Occupations" | USd_9$i_jbsoc10_cc=="Construction and Building Trades Supervisors" | USd_9$i_jbsoc10_cc=="Assemblers and Routine Operatives" | USd_9$i_jbsoc10_cc=="Construction and Building Trades" | USd_9$i_jbsoc10_cc=="Process Operatives" | USd_9$i_jbsoc10_cc=="Construction Operatives"]<-1 
USd_9$Eco_Inactive[USd_9$i_jbsoc10_cc=="inapplicable"]<- 1
USd_9$Education[USd_9$i_jbsoc10_cc=="Teaching and Educational Professionals" | USd_9$i_jbsoc10_cc=="Artistic, Literary and Media Occupations" | USd_9$i_jbsoc10_cc=="Librarians and Related Professionals"] <- 1 
USd_9$Finance_Insurance[USd_9$i_jbsoc10_cc=="Business, Finance and Related Associate Professionals" | USd_9$i_jbsoc10_cc=="Financial Institution Managers and Directors"] <- 1
USd_9$Human_Social_Work[USd_9$i_jbsoc10_cc=="Welfare Professionals" | USd_9$i_jbsoc10_cc=="Caring Personal Services" | USd_9$i_jbsoc10_cc=="Health and Social Services Managers and Directors" | USd_9$i_jbsoc10_cc=="Childcare and Related Personal Services"| USd_9$i_jbsoc10_cc=="Nursing and Midwifery Professionals" | USd_9$i_jbsoc10_cc=="Health Associate Professionals" | USd_9$i_jbsoc10_cc=="Health Professionals" | USd_9$i_jbsoc10_cc=="Therary Professionals" | USd_9$i_jbsoc10_cc=="Hairdressers and Related Services" | USd_9$i_jbsoc10_cc=="Managers and Proprietors in Health and Care Services"] <-1
USd_9$IT[USd_9$i_jbsoc10_cc=="Information Technology Technicians" | USd_9$i_jbsoc10_cc=="Electrical and Electronic Trades"] <- 1
USd_9$Manufacturing[USd_9$i_jbsoc10_cc=="Metal Forming, Welding and Related Trades" | USd_9$i_jbsoc10_cc=="Engineering Professionals" | USd_9$i_jbsoc10_cc=="Metal Machining, Fitting and Instrument Making Trades" | USd_9$i_jbsoc10_cc=="Textiles and Garments Trades" | USd_9$i_jbsoc10_cc=="Building Finishing Trades" | USd_9$i_jbsoc10_cc=="Skilled Metal, Electrical and Electronic Trades Supervisors"] <- 1
USd_9$Other[USd_9$i_jbsoc10_cc=="Elementary Cleaning Occupations" | USd_9$i_jbsoc10_cc=="Managers and Proprietors in Other Services" | USd_9$i_jbsoc10_cc=="Sports and Fitness Occupations" | USd_9$i_jbsoc10_cc=="Other Skilled Trades" | USd_9$i_jbsoc10_cc=="Animal Care and Control Services" | USd_9$i_jbsoc10_cc=="Conservation and Environment Professionals" | USd_9$i_jbsoc10_cc=="Design Occupations" | USd_9$i_jbsoc10_cc=="Printing Trades" | USd_9$i_jbsoc10_cc=="Media Professionals" | USd_9$i_jbsoc10_cc=="Housekeeping and Related Services" | USd_9$i_jbsoc10_cc=="Cleaning and Housekeeping Managers and Supervisors" | USd_9$i_jbsoc10_cc=="Chief Executives and Senior Officials" | USd_9$i_jbsoc10_cc=="Conservation and Environmental Associate Professionals"]<-1
USd_9$Prof_Science_Tech[USd_9$i_jbsoc10_cc=="Research and Development Managers" | USd_9$i_jbsoc10_cc=="Science, Engineering and Production Technicians" | USd_9$i_jbsoc10_cc=="Natural and Social Science Professionals"]<-1
USd_9$Public_Admin_Defence[USd_9$i_jbsoc10_cc=="Protective Service Occupations"| USd_9$i_jbsoc10_cc=="Public Services and Other Associate Professionals" | USd_9$i_jbsoc10_cc=="Elementary Security Occupations" | USd_9$i_jbsoc10_cc=="Administrative Occupations: Government and Related Organisations" | USd_9$i_jbsoc10_cc=="Legal Professionals" | USd_9$i_jbsoc10_cc=="Legal Associate Professionals" | USd_9$i_jbsoc10_cc=="Senior Officers in Protective Services"]<-1
USd_9$Real_Estate[USd_9$i_jbsoc10_cc=="Draughtspersons and Related Architectural Technicians"| USd_9$i_jbsoc10_cc=="Architects, Town Planners and Surveyors" | USd_9$i_jbsoc10_cc=="Welfare and Housing Associate Professionals"]<-1
USd_9$Transport_Storage[USd_9$i_jbsoc10_cc=="Mobile Machine Drivers and Operatives" | USd_9$i_jbsoc10_cc=="Road Transport Drivers" | USd_9$i_jbsoc10_cc=="Transport Associate Professionals" | USd_9$i_jbsoc10_cc=="Production Managers and Directors" | USd_9$i_jbsoc10_cc=="Elementary Storage Occupations" | USd_9$i_jbsoc10_cc=="Managers and Directors in Transport and Logistics" | USd_9$i_jbsoc10_cc=="Other Drivers and Transport Operatives"]<-1
USd_9$Whole_Retail_Trade[USd_9$i_jbsoc10_cc=="Customer Service Occupations" | USd_9$i_jbsoc10_cc=="Sales Assistants and Retail Cashiers" | USd_9$i_jbsoc10_cc=="Sales, Marketing and Related Associate Professionals" | USd_9$i_jbsoc10_cc=="Elementary Sales Occupations" | USd_9$i_jbsoc10_cc=="Sales Related Occupations" | USd_9$i_jbsoc10_cc=="Leisure and Travel Services" | USd_9$i_jbsoc10_cc=="Vehicle Trades" | USd_9$i_jbsoc10_cc=="Sales Supervisors" | USd_9$i_jbsoc10_cc=="Managers and Directors in Retail and Wholesale" | USd_9$i_jbsoc10_cc=="Customer Service Managers and Supervisors"]<- 1

sum(USd_9$Accomodation + USd_9$Admin + USd_9$Agr_Forest_Fishing + USd_9$Construction + USd_9$Eco_Inactive + USd_9$Education + USd_9$Finance_Insurance + USd_9$Human_Social_Work + USd_9$IT + USd_9$Manufacturing + USd_9$Other + USd_9$Prof_Science_Tech + USd_9$Public_Admin_Defence + USd_9$Real_Estate + USd_9$Transport_Storage + USd_9$Whole_Retail_Trade)

# Group Travel to Work Method
USd_9$TMH <- 0
USd_9$TDCV <- 0
USd_9$TPCV <- 0
USd_9$TActive <- 0 
USd_9$TPublicTrans <-0
USd_9$Tother <-  0

USd_9$i_worktrav[USd_9$i_worktrav=="refusal" | USd_9$i_worktrav=="don't know" | USd_9$i_worktrav=="inapplicable" | USd_9$i_worktrav=="proxy" | USd_9$i_worktrav=="missing"] <- NA
USd_9$TDCV[USd_9$i_worktrav=="Drive myself by car or van" | USd_9$i_worktrav=="Motorcycle/moped/scooter"]<-1
USd_9$TPCV[USd_9$i_worktrav=="Get a lift with someone from household" |USd_9$i_worktrav=="Get a lift with someone outside household"] <- 1
USd_9$TActive[USd_9$i_worktrav=="Cycle" | USd_9$i_worktrav=="Walk"] <- 1
USd_9$TPublicTrans[USd_9$i_worktrav=="Taxi/minicab" | USd_9$i_worktrav=="Bus/coach" | USd_9$i_worktrav=="Underground/Metro/Tram/Light railway" | USd_9$i_worktrav=="Train"]<-1
USd_9$Tother[USd_9$i_worktrav=="Other"] <- 1

# Expand sex variable 
USd_9$Sexm <- 0 
USd_9$Sexf <- 0
USd_9$Sexm[USd_9$i_sex=="male"]<-1
USd_9$Sexf[USd_9$i_sex=="female"]<-1

#### Annual Income ####
USd_9$Income_Val <- as.numeric(USd_9$i_paygu_dv)*12
# Band Incomes into categories also 
USd_9$Income <- NA 
USd_9$Income[USd_9$Income_Val>0 & USd_9$Income_Val<=3599] <- "0-3599" 
USd_9$Income[USd_9$Income_Val>=3600 & USd_9$Income_Val<=6599] <- "3600-6599" 
USd_9$Income[USd_9$Income_Val>=6600 & USd_9$Income_Val<=9599] <- "6600-9599" 
USd_9$Income[USd_9$Income_Val>=9600 & USd_9$Income_Val<=12599] <- "9600-12599" 
USd_9$Income[USd_9$Income_Val>=12600 & USd_9$Income_Val<=15599] <- "12600-15599"
USd_9$Income[USd_9$Income_Val>=15600 & USd_9$Income_Val<=19199] <- "15600-19199" 
USd_9$Income[USd_9$Income_Val>=19200 & USd_9$Income_Val<=23999] <- "19200-23999" 
USd_9$Income[USd_9$Income_Val>=24000 & USd_9$Income_Val<=29999] <- "24000-29999" 
USd_9$Income[USd_9$Income_Val>=30000 & USd_9$Income_Val<=35999] <- "30000-35999" 
USd_9$Income[USd_9$Income_Val>=36000 & USd_9$Income_Val<=42999] <- "36000-42999" 
USd_9$Income[USd_9$Income_Val>=43000 & USd_9$Income_Val<=49999] <- "43000-49999" 
USd_9$Income[USd_9$Income_Val>=50000] <- "50000+" 

#### Final Generation of Dataset ####

# Read in Wave 8 For Transportation Data (missing in Wave 9) 
USd_8 <- read.spss("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\6614spss_56C747972BBC714FCBAAB572F2629DCB_V1\\UKDA-6614-spss\\spss\\spss24\\ukhls_w8\\h_indresp.sav", to.data.frame=TRUE)
USd_8 <- subset(USd_8, select=c(h_hidp, pidp,h_jsttwtb, h_jspl, h_workdis)) # subset to keep on identifier, and travel to work and work location

# Join Wave 8 to Wave 9 dataset by unique identifier 
USd_9 <- merge (USd_9, USd_8, by="pidp")

# Set to NA missing Commuting Time
USd_9$i_jbttwt[USd_9$i_jbttwt=="missing" | USd_9$i_jbttwt=="inapplicable" | USd_9$i_jbttwt=="proxy" | USd_9$i_jbttwt=="refusal" | USd_9$i_jbttwt=="don't know"]<- NA

# Subset to NA missing Work Distance
USd_9$h_workdis[USd_9$h_workdis=="missing" | USd_9$h_workdis=="inapplicable" | USd_9$h_workdis=="proxy" | USd_9$h_workdis=="refusal" | USd_9$h_workdis=="don't know"]<- NA

# Group Age Variable 
USd_9$Age <- NA 
USd_9$i_dvage<-as.numeric(USd_9$i_dvage)
USd_9$Age[USd_9$i_dvage>=20 & USd_9$i_dvage<=29] <- "20-29"
USd_9$Age[USd_9$i_dvage>=30 & USd_9$i_dvage<=44] <- "30-44"
USd_9$Age[USd_9$i_dvage>=45 & USd_9$i_dvage<=59] <- "45-59"
USd_9$Age[USd_9$i_dvage>=60 & USd_9$i_dvage<=74] <- "60-74"
USd_9$Age[USd_9$i_dvage>=75] <- "75+"

# Create Sex Variable 
USd_9$Sex <- NA 
USd_9$Sex[USd_9$i_sex=="male"]<- "Male"
USd_9$Sex[USd_9$i_sex=="female"]<- "Female"

# Create Long-Term Disability Variable 
USd_9$LTD_2 <- NA 
USd_9$LTD_2[USd_9$i_health=="Yes"]<- "Long-Term Disability"
USd_9$LTD_2[USd_9$i_health=="No"]<- "No Long-Term Disability"

# Create Distance to Work Variable 
USd_9$Distance.To.Work <- as.numeric(USd_9$h_workdis)
# Create Work Location Variable 
USd_9$Work.Location <- as.character(USd_9$h_jspl)
# Create Travel to Work Variable
USd_9$Travel <- USd_9$i_worktrav

# Group Commuting Time Variable 
USd_9$Commuting.Time <- NA 
USd_9$i_jbttwt <- as.numeric(USd_9$i_jbttwt)
USd_9$Commuting.Time[USd_9$i_jbttwt>0 & USd_9$i_jbttwt<=15] <- "0-15 mins"
USd_9$Commuting.Time[USd_9$i_jbttwt>=16 & USd_9$i_jbttwt<=30] <- "16-30 mins"
USd_9$Commuting.Time[USd_9$i_jbttwt>=31 & USd_9$i_jbttwt<=45] <- "31-45 mins"
USd_9$Commuting.Time[USd_9$i_jbttwt>=46 & USd_9$i_jbttwt<=60] <- "46-60 mins"
USd_9$Commuting.Time[USd_9$i_jbttwt>=61 & USd_9$i_jbttwt<=90] <- "61-90 mins"
USd_9$Commuting.Time[USd_9$i_jbttwt>=91 & USd_9$i_jbttwt<=120] <- "91-120 mins"
USd_9$Commuting.Time[USd_9$i_jbttwt>=121] <- "121 mins +"

# Group Work Location Variable 
USd_9$Work.Location[USd_9$Work.Location=="refusal" | USd_9$Work.Location=="don't know" | USd_9$Work.Location=="inapplicable" | USd_9$Work.Location=="proxy" | USd_9$Work.Location=="missing"] <- NA
USd_9$Work.Location[USd_9$Work.Location=="From their own home" | USd_9$Work.Location=="At home"] <- "At Home"
USd_9$Work.Location[USd_9$Work.Location=="Driving or travelling around"] <- "Driving or Travelling Around"
USd_9$Work.Location[USd_9$Work.Location=="From separate business premises" | USd_9$Work.Location=="From client's or customer's premises"] <- "From Client's or Customer's Premise"
USd_9$Work.Location[USd_9$Work.Location=="Or from some place?" | USd_9$Work.Location=="From van or stall"] <- "Other Location"

# Create household size variable
USd_9$i_nadoptch <- as.character(USd_9$i_nadoptch)
USd_9$i_nadoptch[USd_9$i_nadoptch=="proxy" | USd_9$i_nadoptch=="don't know" | USd_9$i_nadoptch=="inapplicable"]<- 0

USd_9$i_nnatch <- as.character(USd_9$i_nnatch)
USd_9$i_nnatch[USd_9$i_nnatch=="proxy" | USd_9$i_nnatch=="don't know" | USd_9$i_nnatch=="inapplicable"]<-0

USd_9$i_npns_dv <- as.character(USd_9$i_npns_dv)
USd_9$i_npns_dv[USd_9$i_npns_dv=="none"]<-0

USd_9$i_ngrp_dv <- as.character(USd_9$i_ngrp_dv)
USd_9$i_ngrp_dv[USd_9$i_ngrp_dv=="none"]<-0

USd_9$i_nnssib_dv <- as.character(USd_9$i_nnssib_dv)
USd_9$i_nnssib_dv[USd_9$i_nnssib_dv=="none"]<-0

# Convert to numeric to enable summing
USd_9$i_nadoptch <- as.numeric(USd_9$i_nadoptch)
USd_9$i_nnatch <- as.numeric(USd_9$i_nnatch)
USd_9$i_ngrp_dv <- as.numeric(USd_9$i_ngrp_dv)
USd_9$i_npns_dv <- as.numeric(USd_9$i_npns_dv)
USd_9$i_nnssib_dv <- as.numeric(USd_9$i_nnssib_dv)

# Calculate Household Size
USd_9$HH.Size <- (USd_9$i_nadoptch + USd_9$i_nnatch + USd_9$i_npns_dv + USd_9$i_ngrp_dv + USd_9$i_nnssib_dv) 
USd_9$HH.Size[USd_9$HH.Size==0] <- 1 #cannot be an empty household

# Calculate Household Energy Spend and PP Energy Spend Variable (HH level Wave 8 data to do so)
USd_HH_8 <- read.spss("C:\\Users\\medsleea\\OneDrive - University of Leeds\\Dissertation\\GEOG5160_Diss\\6614spss_56C747972BBC714FCBAAB572F2629DCB_V1\\UKDA-6614-spss\\spss\\spss24\\ukhls_w8\\h_hhresp.sav", to.data.frame=TRUE)
USd_HH_8 <-  subset(USd_HH_8, select=c(h_hidp, h_xpduely))

# Join Household level data to Individuals in Wave 8 dataset using HH identifier
USd_9 <- merge(USd_9, USd_HH_8, by.x="h_hidp", by.y="h_hidp")

USd_9$h_xpduely <- as.character(USd_9$h_xpduely)
USd_9$h_xpduely[USd_9$h_xpduely=="inapplicable" | USd_9$h_xpduely=="don't know"] <- NA 
USd_9$h_xpduely <- as.numeric(USd_9$h_xpduely)
USd_9$h_xpduely[is.na(USd_9$h_xpduely)]<- mean(USd_9$h_xpduely, na.rm=TRUE) # if not provided set to mean 
USd_9$PP.Energy.Usage <- (USd_9$h_xpduely/USd_9$HH.Size) * 100
USd_9$KWH.Usage.pp.yr <- USd_9$PP.Energy.Usage/9.09 #9.09 used for average value of energy
USd_9$GCO2.Usage<- (USd_9$KWH.Usage.pp.yr* 281.1) # 281.1 carbon emission intensity

#### FINAL DATASETS ####

# SUBSETTING TO REMOVE MISSING DATA
# Remove any respondents with no Travel to Work 
USd_9 <- USd_9[!(USd_9$TMH + USd_9$TDCV + USd_9$TActive + USd_9$Tother + USd_9$TPCV + USd_9$TPublicTrans==0),]

# Remove any respondents without work location 
USd_9 <- USd_9[!is.na(USd_9$Work.Location), ]

# Remove any respondents without annual income 
# Remove any respondents without mode of transport to work 
USd_9 <- USd_9[!is.na(USd_9$i_worktrav),]

# Remove any respondents without Commuting Time
USd_9 <- USd_9[!is.na(USd_9$i_jbttwt),]

# Remove any respondents without Distance to Work
USd_9 <- USd_9[!is.na(USd_9$h_workdis),]

# Remove any respondents with Age<16 
USd_9 <- USd_9[!is.na(USd_9$Age),] 

# Remove any respondents with no employment sector
USd_9 <- USd_9[!is.na(USd_9$Employment.Sector), ]

# Remove any respondents without LTD or Sex 
USd_9 <- USd_9[!(USd_9$Sexf+ USd_9$Sexm==0),]
USd_9 <- USd_9[!(USd_9$LTD+ USd_9$NoLTD==0),]

# Population ends up too small- so synthetically generate more (just for full pipeline production)
USd_9 <- subset(USd_9, select=-c(i_paygu_dv, i_jbsoc10_cc, h_workdis))
synth_ind <- syn(USd_9, k=4000)  # generate arbitrary 4000 observations
USd_9 <- synth_ind$syn

# Final dataset in wide form for Spatial Microsimulation
ind_cat <- subset(USd_9, select=c(TMH, TDCV, TPCV, TActive,TPublicTrans,Tother, LTD, NoLTD, Sexm, Sexf))
# Final dataset ind for IPF 
ind <- subset(USd_9, select=c(Sex, Age, LTD_2, Travel, Employment.Sector, Income, Work.Location, Commuting.Time, Distance.To.Work, Income_Val, KWH.Usage.pp.yr, GCO2.Usage))
colnames(ind)  <- c("Sex", "Age", "LTD", "Travel", "Employment.Sector", "Income", "Work.Location", "Commuting.Time", "Distance.To.Work", "Income_Val", "KWH.Usage.pp.yr", "GCO2.Usage")

##################################################################################################################################################
#### Previous Method where Data Cleaned in Excel (Amending as not Truly Open Science) ####
#Load in the Understanding Society Wave 9 Dataset after Data Cleaning in Excel.
# SORT OUT THE DATA READ IN- SHOULD REALLY BE JUST THE INDIVIDUAL (UNCLEANED DATASET) AND THE CENSUS (UNCLEANED DATASET)
ind2 <-read.csv("USD.csv")
ind2 <- subset(ind2, select=-c(X, HH))
colnames(ind2) <- c("Sex", "Age", "LTD", "Travel" ,"Employment.Sector", "Income","Work.Location", "Commuting.Time","Distance.To.Work", "Income_Val", "KWH.Usage.pp.yr", "GCO2.Usage")
#Load in the Understanding Society Wave 9 Dataset constraint Data after Data Cleaning in Excel.
#ind_cat <- read.csv("USD_CAT.csv",colClasses=c('numeric'))
#Load in the 2011 Census Dataset from Data Cleaning in Excel.
cons <- read.csv("CENSUS.csv")
#Load the chi squared dataset for statistically testing
#ind_freq <- read.csv("USD_CHI.csv")
#Load the PCA Budget Data for VPCA creation. 
PCA <- read.csv("PCA_Budgets.csv")
#Housing<-read.csv("Demographics.csv")
#DemographicsData<-read.csv("Demographics2.csv")

# delete columns 1,2,3, to match variable number of 10 with ind_cat.
cons <- Census[, -c(1:5)]
#Print the USd Dataset
ind
#Print the Census dataset
cons
# Take a quick look at the data
head(ind)
head(cons)
##################################################################################################################################################
#### Chi Squared Testing for Multi-collinearity ####
#Chi squared Section#
#Created tables for all variables included against Commuting Time (important in Personal Carbon Usage)
tbl_agecom <- table(ind$Age, ind$Commuting.Time) 
tbl_sexcom <- table(ind$Sex, ind$Commuting.Time) 
tbl_empcom <- table(ind$Employment.Sector, ind$Commuting.Time)
tbl_ltdcom <- table(ind$LTD, ind$Commuting.Time) 
tbl_travcom <- table(ind$Travel, ind$Commuting.Time) 

#Run cramer test for strength and chi squared assocation between variables
cramer.test(tbl_agecom,nrep = 1000, conf.level = 95) #chi squared and cramer's V for age vs commuting time
cramer.test(tbl_sexcom,nrep = 1000, conf.level = 95)#chi squared and cramer's V for sex vs commuting time
cramer.test(tbl_empcom,nrep = 1000, conf.level = 95)#chi squared and cramer's V for sex vs commuting time
cramer.test(tbl_ltdcom,nrep = 1000, conf.level = 95)#chi squared and cramer's V for disability vs commuting time
cramer.test(tbl_travcom,nrep = 1000, conf.level = 95)#chi squared and cramer's V for travel vs commuting time

#Found that Sex, Employment and Travel significant as <0.05, so test against each other to measure collinearity. 
tbl_sexemp <- table(ind$Sex, ind$Employment.Sector) 
tbl_sextrav <- table(ind$Sex, ind$Travel) 
tbl_travemp <- table(ind$Travel, ind$Employment.Sector) 
tbl_ltdemp <- table(ind$LTD, ind$Employment.Sector) 
tbl_ltdtrav <- table(ind$LTD, ind$Travel) 
tbl_ltdsex <- table(ind$LTD, ind$Sex) 

#Testing significant variables against commuting time against each other to measure collinearity.
cramer.test(tbl_sexemp,nrep = 1000, conf.level = 95) #chi squared and cramer's V for sex vs employment
cramer.test(tbl_sextrav,nrep = 1000, conf.level = 95)#chi squared and cramer's V for sex vs travel
cramer.test(tbl_ltdtrav,nrep = 1000, conf.level = 95)#chi squared and cramer's V for disability vs travel
cramer.test(tbl_ltdsex,nrep = 1000, conf.level = 95)#chi squared and cramer's V for disability vs sex
cramer.test(tbl_ltdemp,nrep = 1000, conf.level = 95)#chi squared and cramer's V for disability vs employment
cramer.test(tbl_travemp,nrep = 1000, conf.level = 95)# chi squared and cramer's V for travel vs employment

rm(tbl_agecom, tbl_empcom, tbl_ltdcom, tbl_ltdemp, tbl_ltdsex, tbl_ltdtrav, tbl_sexcom, tbl_sexemp, tbl_sextrav, tbl_travcom, tbl_travemp) #tidy

# Found that travel vs employment (SIG), and sex vs employment (SIG) so removed employment constraint
# delete Employment.Sector Column from ind
#ind <- subset(ind, select=-c(Employment.Sector))
##################################################################################################################################################
#### Constraint Validation Section ####
#Constraint Validation Section#
# Load constraints separately
con_1 <- cons[1:6] #load the transport constraint 
con_2 <- cons[7:8] #load the disability constraint
con_3 <- cons[9:10] #load the sex constraint
#Check that all constraints have the same total value.
sum(con_1)
sum(con_2)
sum(con_3)

#Hold the labels for each category from cons into a new variable. 
cat_labs <- names(cons)

# Check constraint totals- should return TRUE
# Checking to see if the totals of the constraint match
sum(ind_cat[,1:ncol(con_1)]) == nrow(ind) # THIS SHOULD RETURN TRUE
sum(ind_cat[,1:ncol(con_1)]) # THIS SHOULD RETURN TRUE
nrow(ind) # THIS SHOULD RETURN TRUE

# is the number in each category correct?
sum(ind_cat[,ncol(con_1)+1:ncol(con_2)]) == nrow(ind) # THIS SHOULD RETURN TRUE

#### Vulnerability Measure Creation ####
#Vulnerability Measure Creation#
#Calulate proxy for hours worked- using median wage in the UK (Statista, 2020).
ind$Hours<- ind$Income_Val / 14.8
# Calculate proxy for days worked- using Hours worked / 8 (typical work day length).
ind$Days <- ind$Hours /8

#If they earn significantly more than minimum wage, may appear they work more days than is possible. 
ind$Days[ind$Days>337]<-337
#Find unique Travel Values and Multiplier and create data frame of this.
Travel<- unique(ind$Travel)

#VPIS Multiplier
ind$Travel <- as.character(ind$Travel)
ind$Travel[ind$Travel=="Drive myself by car or van" | ind$Travel=="Motorcycle/moped/scooter"] <- "Drive Car or Van" 
ind$Travel[ind$Travel=="Get a lift with someone from household" | ind$Travel=="Get a lift with someone outside the household"] <- "Passenger in a Car or Van"
ind$Travel[ind$Travel=="Walk"] <- "Walking" 
ind$Travel[ind$Travel=="Underground/Metro/Tram/Light railway"] <- "Light Rail, Tube, Underground"
ind$Travel[ind$Travel=="Cycle"] <- "Bike" 
ind$Travel[ind$Travel=="Bus/coach"] <-"Bus, Minibus, Coach" 

VPISVal<- data.frame("Travel" =c("Light Rail, Tube, Underground", "Train", "Bus, Minibus, Coach", "Bike", "Drive Car or Van", "Passenger in a Car or Van", "Walking"), "Value" =c(0.36, 0.25, 0.32, 0.05, 0.95, 0.00001, 0.00001))
# Join VPIS Value to the ind dataset by the Travel Type
ind<- merge(x = ind, y = VPISVal, by.x = "Travel", by.y="Travel", all.x=TRUE)
rm(VPISVal) # Tidy 

#Set as numeric type to ensure multiplication works
ind$Distance.To.Work<- as.numeric(ind$Distance.To.Work)
ind$Days<-as.numeric(ind$Days)
ind$Income_Val<-as.numeric(ind$Income_Val)

#Calculate the VPIS value.
ind$VPIS<- (ind$Value * (ind$Distance.To.Work*2*ind$Days) / ind$Income_Val)*100
# Checking mean and median of individual spending on travel to work.
summary(ind$VPIS)
# Individuals that spend more than 10% income on travel, given value of 1.
ind$VPISBIN<- 0
ind$VPISBIN[ind$VPIS>=10] <- 1 

# VPEB Calculation
VPEBVal<- data.frame("Travel" =c("Light Rail, Tube, Underground", "Train", "Bus, Minibus, Coach", "Bike", "Drive Car or Van", "Passenger in a Car or Van", "Walking"), "Value" =c(21.63, 22.75, 45.67, 5.16, 56.01, 56.01, 7.09))
#Create data frame of Travel Type and Multiplier value
ind<- merge(x = ind, y = VPEBVal, by.x = "Travel", by.y="Travel", all.x=TRUE)
rm(VPEBVal) # Tidy 

# Calculate the VPCE and VPEB value
ind$VPCE<- ind$Value.y * (ind$Distance.To.Work*2*ind$Days)
ind$VPEB <- (ind$VPCE/ind$GCO2.Usage)*100
# Individuals that spend more than 10% of energy budget on travel given value of 1.
ind$VPEBBIN <- 0
ind$VPEBBIN[ind$VPEB>=10] <-1

#VPCA Individual Level Calculation of Various Scenarios
ind$PCA2019<-(((ind$VPCE + ind$GCO2.Usage)/PCA$ï..2019PCA)*100)
ind$PCA802030<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2030PCA80)*100)
ind$PCA802040<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2040PCA80)*100)
ind$PCA802050<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2050PCA80)*100)
ind$PCA602030<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2030PCA60)*100)
ind$PCA602040<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2040PCA60)*100)
ind$PCA602050<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2050PCA60)*100)
ind$PCA402030<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2030PCA40)*100)
ind$PCA402040<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2040PCA40)*100)
ind$PCA402050<-(((ind$VPCE + ind$GCO2.Usage)/PCA$X2050PCA40)*100)
  
#VPCA >100 give it value of 1 for Various Scenarios
ind$VPCA2019BIN <- 0
ind$VPCA802030BIN <- 0
ind$VPCA802040BIN <-0
ind$VPCA802050BIN <-0
ind$VPCA602030BIN <-0
ind$VPCA602040BIN <-0
ind$VPCA602050BIN <-0
ind$VPCA402030BIN <-0
ind$VPCA402040BIN <-0
ind$VPCA402050BIN <-0

ind$VPCA2019BIN[ind$PCA2019>=100] <- 1
ind$VPCA802030BIN[ind$PCA802030>=100] <- 1
ind$VPCA802040BIN[ind$PCA802040>=100] <-1
ind$VPCA802050BIN[ind$PCA802050>=100] <-1
ind$VPCA602030BIN[ind$PCA602030>=100] <-1
ind$VPCA602040BIN[ind$PCA602040>=100] <-1
ind$VPCA602050BIN[ind$PCA602050>=100] <-1
ind$VPCA402030BIN[ind$PCA402030>=100] <-1
ind$VPCA402040BIN[ind$PCA402040>=100] <-1
ind$VPCA402050BIN[ind$PCA402050>=100] <-1
rm(cat_labs, Travel, VPEBVal, VPISVal) #tidy 

##################################################################################################################################################
#### Iterative Proportional Fitting Step ####
#IPF SECTION#
# Create 2D weight matrix (individuals, areas)
weights <- array(NA, dim=c(nrow(ind),nrow(cons))) 

# Convert survey data into aggregates to compare with census 
ind_agg <- matrix(colSums(ind_cat), nrow(cons), ncol(cons), byrow = T)

# Iterative proportional fitting (IPF) stage
cons <- apply(cons, 2, as.numeric) # convert the constraints to 'numeric'
ind_catt <- t(ind_cat) # transpose the dummy variables for ipfp
colnames(ind_catt) <- c(paste0("V", 1:ncol(ind_catt))) # rename columns to V1, V2 etc

x0 <- rep(1, nrow(ind)) # set the initial weights
weights <- apply(cons, 1, function(x) ipfp(x, ind_catt, x0, maxit = 20,v=T))


### Convert back to aggregates
ind_agg <- t(apply(weights, 2, function(x) colSums(x * ind_cat)))

# test results for first row (not necessary for model)
ind_agg[1,1:10] - cons[1,1:10] # should be zero or close to zero
cor(as.numeric(ind_agg), as.numeric(cons)) # fit between contraints and estimate
#### Internal Validation ####
#Internal Validation Section#
#Index of the maximum error
which(abs(ind_agg-cons) == max(abs(ind_agg-cons)), 
      arr.ind = TRUE)
#Minimum error
min(abs(ind_agg-cons))
#Index of the minimum error
which(abs(ind_agg-cons) == min(abs(ind_agg-cons)), 
      arr.ind = TRUE)
#RMSE Validation
rmse(cons, ind_agg)
#Currently lots of results, appears to be column 11 and 23. 

# Vectorised solution of code above, calculates the correlation between each row of cons and ind_agg
CorVec<-diag(cor(t(cons),t(ind_agg)))
#summarise this correlation data
summary(CorVec)
#which is the zone with the worst fit
which.min(CorVec)
#top 5 worst fitted zones
head(order(CorVec),n=5)

#Created tae function to allow for TAE to be calculated between synthetic and actual values
tae <- function(observed, simulated){
  obs_vec <- as.numeric(observed)
  sim_vec <- as.numeric(simulated)
  sum(abs(obs_vec - sim_vec))
}
#Calculate overall TAE for all zones
tae(cons,ind_agg)

#Calculate RE
re<-tae(cons,ind_agg)/(sum(cons))
#Display RE
re

#Percentage Relative Error (re * 100)
reperc<-re*100
#Display reperc
reperc

# Initialize the vectors for TAE and RE for each zone
TAEVec <- rep(0, nrow(cons))
REVec <- rep(0, nrow(cons))

# calculate the correlation for each zone
for (i in 1:nrow(cons)){
  TAEVec[i] <- tae (cons[i,], ind_agg[i,])
  REVec[i] <- TAEVec[i] / sum(cons[i,])
}

# Summarise the basic statistics for TAEVec and REVec (TAE and RE for each zone)
summary(TAEVec)
summary(REVec)
# Maximum TAEVEC for zone
which.max(TAEVec)
# Maximum REVec for zone
which.max(REVec)
# Worst 5 zones for TAE
tail(order(TAEVec), n = 5)
# Worst 5 zones for RE
tail(order(REVec), n = 5)
# How much error comes from the worst zoneï
worst_zone <- tail(order(TAEVec), n = 1)
# Display ID of worst zone
worst_zone
# Calculate % of error attributed to this zone
ERRORTOTAL<-sum(TAEVec[worst_zone] / sum(TAEVec)*100)
ERRORTOTAL

# Correlation between TAEVec and REVec, the measures are highly correlated
cor(TAEVec, REVec) 

rm(x0, i) #tidy
##################################################################################################################################################
#### Deep Dive into Zones with High Error ####
# differences for zone 217
RudeDiff <- cons[217,] - ind_agg[217,] 
# interesting differences
diff <- round( abs(RudeDiff) ) 
# printing the differences bigger than 1000
diff[diff > 1000] 

# differences for zone 129
RudeDiff <- cons[168,] - ind_agg[168,] 
# interesting differences
diff <- round( abs(RudeDiff) ) 
# printing the differences bigger than 1000
diff[diff > 1000] 

# Worst 3 REVec in the zones
worst <- tail(order(REVec), n = 3)
# constraint for 3 worst zones, in Tother and TActive
cons[worst, c("Tother","TActive")] 
# Break up of individuals by LTD
table( ind[,4] )
# Find which individuals travel by Other Transport
which(ind$Travel=="Other Transport") 
ind[132,1:8]
ind[422,1:8]

# Best 5 REVec in the zones
best <- head(order(REVec), n = 5)
# constraint for 3 best zones
cons[best, c("Sexm","Sexf","LTD","NoLTD")]
rm(ind_agg, ind_cat, ind_catt, ind_freq, cons_full, con_1, con_2, con_3) #tidy
##################################################################################################################################################
#### Integerisation using TRS Methodology ####
#Integerisation using TRS Methodology#
# generate integerised result for weighting for each individual into each zone
ints <- unlist(apply(weights, 2, function(x) int_expand_vector(int_trs(x)))) 
# Gives each zone an ID and creates data frame with individual ID and Zone ID
ints_df <- data.frame(id = ints, zone = rep(1:nrow(cons), round(colSums(weights))))
# assign each individual an ID, from initial 444 respondents
ind$id <- 1:nrow(ind) 

# Create spatial microdata, by joining the ids with associated attributes
ints_df <- inner_join(ints_df, ind) 

##################################################################################################################################################
#### Aggregate Level Vulnerability Measures ####
#Aggregate Level Vulnerability Measures
#VPEBAGG and VPISAGG and VHAGG Calculations. VHAGG is divided by the population.
VAGG<-ints_df %>% group_by(zone) %>% summarize(sum(VPEBBIN==1),sum(VPEBBIN==0),sum(VPISBIN==1),sum(VPISBIN==0),sum(PCA2019/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA802030/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA802040/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA802050/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA602030/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA602040/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA602050/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA402030/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA402040/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(PCA402050/(sum(VPEBBIN==0)+ sum(VPEBBIN==1))),sum(VPCA2019BIN==1),sum(VPCA2019BIN==0),sum(VPCA802030BIN==1),sum(VPCA802030BIN==0),sum(VPCA802040BIN==1),sum(VPCA802040BIN==0),sum(VPCA802050BIN==1),sum(VPCA802050BIN==0),sum(VPCA602030BIN==1),sum(VPCA602030BIN==0),sum(VPCA602040BIN==1),sum(VPCA602040BIN==0),sum(VPCA602050BIN==1),sum(VPCA602050BIN==0),sum(VPCA402030BIN==1),sum(VPCA402030BIN==0),sum(VPCA402040BIN==1),sum(VPCA402040BIN==0),sum(VPCA402050BIN==1),sum(VPCA402050BIN==0))
VAGG <- VAGG %>% 
  rename(Zone = 1, VPEBAGG = 2,VPEBAGG0=3,VPISAGG=4,VPISAGG0=5,VPCAAGG=6,VPCA802030AGG=7,VPCA802040AGG=8,VPCA802050AGG=9,VPCA602030AGG=10,VPCA602040AGG=11,VPCA602050AGG=12,VPCA402030AGG=13,VPCA402040AGG=14,VPCA402050AGG=15,VPCAAGG1=16,VPCAAGG0=17, VPCA802030AGG1=18,VPCA802030AGG0=19,VPCA802040AGG1=20,VPCA802040AGG0=21,VPCA802050AGG1=22,VPCA802050AGG0=23,VPCA602030AGG1=24,VPCA602030AGG0=25,VPCA602040AGG1=26,VPCA602040AGG0=27,VPCA602050AGG1=28,VPCA602050AGG0=29,VPCA402030AGG1=30,VPCA402030AGG0=31,VPCA402040AGG1=32,VPCA402040AGG0=33,VPCA402050AGG1=34,VPCA4050AGG0=35)

##################################################################################################################################################
#### Demographic Calculations and Summarising ####
#Demographic Calculations#
#Converting all categorical variables in ints_df to numeric to enable summarising.
out<-data.matrix(ints_df)
demo<-as.data.frame(out)
# Aggregated statistics for each zone, total population, income, VPIS, emissions, VPCA, VPCE (DOESN'T WORK)
INCOMEAGG<- demo %>% group_by(zone) %>% summarize(ZonePop=sum(ï..Sex==1)+sum(ï..Sex==2),INC=sum(IncomeVal/ZonePop),VPIS=sum(VPIS/ZonePop),GCO2=sum(GCO2.Usage/1000/ZonePop), VPCA= sum(VPCA2019BIN/ZonePop),VPCE=sum(VPCE/1000/ZonePop))
# Aggregated statistics for each zone, of demographics 
Demographics<-demo %>% group_by(zone) %>% summarize(ZonePop=sum(ï..Sex==1)+sum(ï..Sex==2),LTD=(sum(LTD==1)/ZonePop * 100),NOLTD=(100-LTD),Male=(sum(ï..Sex==1)/ZonePop * 100),Female=(sum(ï..Sex==2)/ZonePop * 100),A20_29=(sum(Age==1)/ZonePop * 100),A30_44=(sum(Age==2)/ZonePop * 100),A45_59=(sum(Age==3)/ZonePop * 100),A60_74=(sum(Age==4)/ZonePop * 100),A75=(sum(Age==5)/ZonePop * 100),OnePerson65=(sum(HH==1)/ZonePop * 100),OnePersonOther=(sum(HH==2)/ZonePop * 100),OneFamily65=(sum(HH==3)/ZonePop * 100),OneFamilyNoKids=(sum(HH==4)/ZonePop * 100),OneFamilyKids=(sum(HH==5)/ZonePop * 100),OtherKids=(sum(HH==6)/ZonePop * 100),LoneParentKids=(sum(HH==7)/ZonePop * 100),OtherOther=(sum(HH==8)/ZonePop * 100),UMLR=(sum(Travel==1)/ZonePop * 100),TT=(sum(Travel==2)/ZonePop * 100),BMC=(sum(Travel==3)/ZonePop * 100),MSM=(sum(Travel==4)/ZonePop * 100),DCV=(sum(Travel==5)/ZonePop * 100),TPCV=(sum(Travel==6)/ZonePop * 100),TB=(sum(Travel==7)/ZonePop * 100),TOF=(sum(Travel==8)/ZonePop * 100),Tother=(sum(Travel==9)/ZonePop * 100),JAFF=(sum(Employment.Sector==1)/ZonePop * 100),JMQ=(sum(Employment.Sector==2)/ZonePop * 100),JM=(sum(Employment.Sector==3)/ZonePop * 100),JWSWMR=(sum(Employment.Sector==4)/ZonePop * 100),JC=(sum(Employment.Sector==5)/ZonePop * 100),JWRT=(sum(Employment.Sector==6)/ZonePop * 100),JTS=(sum(Employment.Sector==7)/ZonePop * 100),JAFS=(sum(Employment.Sector==8)/ZonePop * 100),JIC=(sum(Employment.Sector==9)/ZonePop * 100),JFI=(sum(Employment.Sector==10)/ZonePop * 100),JPSTA=(sum(Employment.Sector==11)/ZonePop * 100),JASS=(sum(Employment.Sector==12)/ZonePop * 100),JPAD=(sum(Employment.Sector==13)/ZonePop * 100),JE=(sum(Employment.Sector==14)/ZonePop * 100),JHHSW=(sum(Employment.Sector==15)/ZonePop * 100),JO=(sum(Employment.Sector==16)/ZonePop * 100),JEI=(sum(Employment.Sector==17)/ZonePop * 100),CT0_15=(sum(Commuting.Time==1)/ZonePop * 100),CT16_30=(sum(Commuting.Time==2)/ZonePop * 100),CT31_45=(sum(Commuting.Time==3)/ZonePop * 100),CT46_60=(sum(Commuting.Time==4)/ZonePop * 100),CT61_90=(sum(Commuting.Time==5)/ZonePop * 100),CT91_120=(sum(Commuting.Time==6)/ZonePop * 100),CT121=(sum(Commuting.Time==7)/ZonePop * 100),Dist=(sum(Distance.To.Work)/ZonePop),GCO2=((sum(GCO2.Usage)/ZonePop)/1000),PCA2019=(sum(PCA2019)/ZonePop))

#Summarising aggregated values to give descriptive statistics
inc_summary <- summary(INCOMEAGG)
vpca_summary<- summary(VAGG$VPCAAGG)
gco2_summary <- summary(ind$GCO2.Usage/1000)
pca402050ints_df_summary <- summary(ints_df3$PCA402050)
vpca402050_summary<- summary(VAGG$VPCA402050AGG)
vpca602050_summary<- summary(VAGG$VPCA602050AGG)
vpca802050_summary<- summary(VAGG$VPCA802050AGG)
vpce_summary<- summary(ints_df$VPCE/1000)
# Correlation check between synthetic popualtion and PCA for selected demographics
cor(demo$Income, demo$PCA2019) # -0.157
cor(demo$ï..Sex, demo$PCA2019) # 0.0515
cor(demo$Age, demo$PCA2019) # 0.0816
cor(demo$LTD, demo$PCA2019) # -0.1752

cramer.test(demo$ï..Sex, demo$PCA2019) # Sig
cramer.test(demo$Age, demo$PCA2019) # Sig
cramer.test(demo$LTD, demo$PCA2019) # Sig
cramer.test(demo$Income, demo$PCA2019) # Sig
##################################################################################################################################################
#### Exporting the data to file ####
#WRITE OUT TO FILE#
#Write out to a csv
write.csv(ints_df,"SpatialMicrosimulation_ind.csv", row.names = TRUE)
#Write out to a csv
write.csv(VAGG,"Vulnerability_LSOA.csv", row.names = TRUE)
#Write out to a csv
write.csv(INCOMEAGG,"Income_LSOA.csv", row.names = TRUE)
#Write out to a csv
write.csv(Demographics,"Demographics_LSOA.csv", row.names = TRUE)

##################################################################################################################################################
#### Map Data in R #### 
# Import sf library for shapefile reading
library (sf)
# Import tidyverse for ggplot
library (tidyverse)
# Read in the Leeds LSOA shapefile
LSOA_shp <- st_read('LEEDS.shp')

# Merge Spatial Microsimulation Data with LSOA_shp
LSOA_shp_VAGG<- merge(LSOA_shp, VAGG, by.x='zone', by.y='Zone')
LSOA_shp_INCAGG<- merge(LSOA_shp, INCOMEAGG, by.x='zone', by.y='zone')
LSOA_shp_Demog <- merge(LSOA_shp, Demographics, by.x='zone', by.y='zone')
# Geographic Transform from BNG to WGS84 for Leaflet Mapping
LSOA_shp_VAGG = st_transform(LSOA_shp_VAGG, 4326)
LSOA_shp_INCAGG = st_transform(LSOA_shp_INCAGG, 4326)
# Library
library(leaflet)
#install.packages('crosstalk')
library(crosstalk)
# Create the PopUp Data
mytext <- paste( 
  "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>", 
  "2019 VPCA: ", round(LSOA_shp_VAGG$VPCAAGG,2)," (",
  round((LSOA_shp_VAGG$VPCAAGG-87.22),2), ifelse(round((LSOA_shp_VAGG$VPCAAGG-87.22),2)>0, "% Above ", "% Below "), "Leeds Average) <br/>",
  "VPIS: ", round(LSOA_shp_VAGG$VPISAGG,2)," (",
  round((LSOA_shp_VAGG$VPISAGG-707.7),2),ifelse(round((LSOA_shp_VAGG$VPISAGG-707.7),2)>0, " Above ", " Below "), "Leeds Average) <br/>",
  "VPEB: ", round(LSOA_shp_VAGG$VPEBAGG,2), " (",
  round((LSOA_shp_VAGG$VPEBAGG-537.43),2), ifelse(round((LSOA_shp_VAGG$VPEBAGG-537.43),2)>0, " Above ", " Below "), "Leeds Average) <br/>",
  "2030 : ", round(LSOA_shp_VAGG$VPCA802030AGG,2), "<br/>",
  "2030 AV: ", round(sum(LSOA_shp_VAGG$VPCA802030AGG/482),2), "<br/>",
  "2040 : ", round(LSOA_shp_VAGG$VPCA802040AGG,2), "<br/>",
  "2040 AV: ", round(sum(LSOA_shp_VAGG$VPCA802040AGG/482),2), "<br/>",
  "2050 : ", round(LSOA_shp_VAGG$VPCA802050AGG,2), "<br/>",
  "2050 AV: ", round(sum(LSOA_shp_VAGG$VPCA802050AGG/482),2), "<br/>",
  sep="") %>%
lapply(htmltools::HTML)
VPCAtext <- paste( 
  "LSOA Name: ", LSOA_shp_VAGG$name, "<br/>", 
  "2019 VPCA: ", round(LSOA_shp_VAGG$VPCAAGG,2)," (",
  round((LSOA_shp_VAGG$VPCAAGG-87.22),2), ifelse(round((LSOA_shp_VAGG$VPCAAGG-87.22),2)>0, "% Above ", "% Below "), "Leeds Average)",
  sep="") %>%
  lapply(htmltools::HTML)

# Create bins for all figures from Dissertation

binsPCA2019 <- c(75, 80,85, 90, 95, 100) # Ok
binsFM <-c(34, 38, 40, 42, 44, 46, 48, 50, 52) # Returns same values as DISS! Ok
binshouse <- c(80, 140, 200, 275, 383, 630) # Returns same values at DISS! Ok
binsinc <-c(19000, 19500, 20000, 20500, 21000, 21500, 22000) # Ok 
binsdist <-c(12, 13, 14, 15, 16, 17, 18, 19, 20) # Ok
binsvpis <-c(17, 17.5, 18, 18.5, 19, 19.5, 20) # Ok
binspcte <-c(210, 220, 230, 240, 250, 260, 270, 280, 290) # Ok
binshhe <- c(2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700) # Ok 
bins75 <- c(0, 1, 1.5, 2, 3, 4, 5, 6) # Ok
binsltd <-c(25, 27.5, 30, 32.5, 35) # Ok
bins40 <- c(100, 110, 120, 130, 140) # Ok
bins60 <- c(160, 170, 180, 190, 200, 210) # Ok
bins80 <- c(320, 330, 340, 350, 360, 370, 380, 390, 400, 420) # Ok


# Create colour palettes for all figures from Dissertation
palPCA2019 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCAAGG, bins = binsPCA2019) #Value DOESN'T match, min should be 75.6, max of 79.99
palFM <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$FAMFAM, bins = binsFM) #Value matches Diss!
palhouse <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$HouseP1, bins=binshouse) #Value matches Diss!
palinc <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$INC, bins = binsinc) #Value DOESN'T match, min should be 17093, max of 19639
paldist <- colorBin("YlOrRd", domain = LSOA_shp_Demog$Dist, bins = binsdist)
palvpis <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$VPIS, bins=binsvpis) #Value DOESN'T match, min should be 9.04, max of 18.49
palpcte <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$VPCE, bins=binspcte) #Value DOESN'T match, min should be 89.74, max of 202.34
palhhe <- colorBin("YlOrRd", domain = LSOA_shp_INCAGG$GCO2, bins=binshhe) #Value DOESN'T match, min should be 2803, max of 3016
pal75 <- colorBin("YlOrRd", domain = LSOA_shp_Demog$A75, bins=bins75) #Value DOESN'T match, min should be 2, max of 16.7
palltd <- colorBin("YlOrRd", domain = LSOA_shp_Demog$LTD, bins=binsltd) #Value DOESN'T match, min should be 3.8, max of 37.4
pal40 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCA402050AGG, bins=bins40) # Value DOESN'T match, min should be 105, max of 111
pal60 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCA602050AGG, bins=bins60) # Value DOESN'T match, min should be 157.5, max of 166.5
pal80 <- colorBin("YlOrRd", domain = LSOA_shp_VAGG$VPCA802050AGG, bins=bins80) # Value DOESN'T match, min should be 314.9, max of 333.1


 
