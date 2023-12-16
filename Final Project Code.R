
################################################################################
# Title: Final Project                                                         #
# Author: Adin Hammond                                                         #
# Date: May 16, 2022                                                           #
                                                                               #
################################################################################

#==============================================================================#
#                           Script Preparation                                 #
#==============================================================================#

#------------------------------------------------------------------------------#
#                              Library packages                                #
#------------------------------------------------------------------------------#

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringdist)
library(zipcodeR)
library(Hmisc)
library(tinytex)
#------------------------------------------------------------------------------#
#                   Set Global Variables and Parameters                        #
#------------------------------------------------------------------------------#



#==============================================================================#
#                        Question 1: Download the Data.                        #
#==============================================================================#

getwd()
setwd("/Users/cookiebearco/Downloads/ECON 370/Final Project /hcris_raw")
if(file.exists("rpt1998.csv")){
  
} else{
  for(i in 1998:2010){
    download.file(paste0("http://www.nber.org/hcris/265-94/rnl_rpt265_94_",i, ".csv"),
                  destfile = paste0("rpt",i,".csv"))
    download.file(paste0("http://www.nber.org/hcris/265-94/rnl_nmrc265_94_",i, "_long.csv"),
                  destfile = paste0("nmrc",i,".csv"))
    download.file(paste0("http://www.nber.org/hcris/265-94/rnl_alpha265_94_",i, "_long.csv"),
                  destfile = paste0("alpha",i,".csv"))
    
  }
}
# We used a loop to download the raw data from the websites.


#==============================================================================#
#                         Question 2:Cleaning the Data                         #
#==============================================================================#

#------------------------------------------------------------------------------#
#                       Reformatting the Raw Data                              #
#------------------------------------------------------------------------------#

codes_non_ownership = fread("~/Downloads/codes_non_ownership.csv")
#Read in code_non_ownership

codes_non_ownership[,line := stringr::str_pad(line,5,"left","0")]
#This is getting rid of the extra zeros in the variable line 

codes_non_ownership[,column := stringr::str_pad(column,4,"left","0")]
#This is getting rid of the extra zeros in the variable column 

codes_non_ownership[,variable_key := paste0(worksheet,line,column)]
# Creating a new column named variable key. This will include the values
#in columns  worksheet, line, column

codes_non_ownership = codes_non_ownership[,.(variable,variable_key)]
# Getting ride of the worksheet, line, and column variables and keeping the 
#variable name and and variable_key columns 

for (i in 1998:2010) {
  
  setwd("/Users/cookiebearco/Downloads/ECON 370/Final Project /hcris_raw")
  
  nmrc_temp  = fread(paste0("nmrc",i,".csv"))
  alpha_temp = fread(paste0("alpha",i,".csv"))
  rpt_temp = fread(paste0("rpt", i, ".csv"))
  #This is reading in the data for the ith year
  
  rpt_temp = rpt_temp[,.(rpt_rec_num, fy_bgn_dt, fy_end_dt) ]
  #This is keeping the only the variable columns we need in the rpt data.
  
  nmrc_temp[,line_num := stringr::str_pad(line_num,5,"left","0")]
  nmrc_temp[,clmn_num := stringr::str_pad(clmn_num,4,"left","0")]
  nmrc_temp[,variable_key := paste0(wksht_cd,line_num,clmn_num)]
  # Getting rid of extra zeros in the line and column variables, then pasting 
  #them together in a new column named variable_key
  
  nmrc_temp=merge(
    nmrc_temp,codes_non_ownership,    
    by = "variable_key"
  )
  #Merging the nmrc and codes_non_owndershio datasets by the variable_key
  
  nmrc_temp = nmrc_temp[,.(rpt_rec_num,variable,itm_val_num)]
  #This keeps only the columns in nmrc that we need
  
  nmrc_temp = dcast(nmrc_temp, rpt_rec_num ~ variable, value.var = "itm_val_num" )
  #We are making this long data wide data with variables as columns and itm_val_num
  # as the values filling up the rows
  
  alpha_temp[,line_num := stringr::str_pad(line_num,5,"left","0")]
  alpha_temp[,clmn_num := stringr::str_pad(clmn_num,4,"left","0")]
  alpha_temp[,variable_key := paste0(wksht_cd,line_num,clmn_num)]
  # Getting rid of extra zeros in the line and column variables, then pasting 
  #them together in a new column named variable_key
  
  
  alpha_temp=merge(
    alpha_temp,codes_non_ownership,    
    by = "variable_key"
  )
  #Merging the alpha and codes_non_owndershio datasets by the variable_key
  
  alpha_temp = alpha_temp[,.(rpt_rec_num,variable,alphnmrc_itm_txt)]
  # Getting rid of the columns that are uneeded. 
  
  alpha_temp = dcast(alpha_temp, rpt_rec_num ~ variable, value.var = "alphnmrc_itm_txt" )
  #We are making this long data wide data with variables as columns and alphnmrc_itm_txt
  # as the values filling up the rows
  
  alpha_temp = merge(alpha_temp,rpt_temp, by = "rpt_rec_num", all.x = T )
  #Merging the alpha and rpt data by the variable_key
  
  setwd("/Users/cookiebearco/Downloads/ECON 370/Final Project /hcris_cleaned")
  fwrite(alpha_temp,paste0("hcris",i,".csv"))
  #putting all these files in a folder "hcris_cleaned" and naming them
  
  
  if(i==1998){
    hcris_panel = merge(nmrc_temp, alpha_temp, by = "rpt_rec_num",all=T)
    hcris_panel[,year:=i]
  }else{
    hcris_temp = merge(nmrc_temp, alpha_temp, by = "rpt_rec_num",all=T)
    hcris_temp[,year:=i]
    # This code is saying that if the i=1998 then merge the nmrc and alpha and 
    #create a column "year". Name the panel hcris_panel
    
    #But if it is any other year, do the same and call it hcris_temp. 
    
    hcris_panel= rbind(hcris_panel,hcris_temp, fill = T )
    #Append it to hcris_panel.Each year is then appended sequentially to hcris_panel
  }
}


#------------------------------------------------------------------------------#
#                       Cleaning the Reformatted Data                          #
#------------------------------------------------------------------------------#

#1. 
hcris_panel = hcris_panel[!is.na(hcris_panel$prvdr_num), ]
#Keeping the provider numbers that are nor missing 
hcris_panel[,prvdr_num:=as.numeric(prvdr_num)]
#Making all the provider numbers numeric instead of vector


#2. 

hcris_panel$epo_net = abs(hcris_panel$epo_cost)
hcris_panel$epo_net_cost = abs(hcris_panel$epo_net_cost)
hcris_panel$epo_rebates = abs(hcris_panel$epo_rebates)
#Taking the absolute value of the  variables epo_cost, epo_net_cost, and epo_rebates

#3. 
hcris_panel$epo_rebates[is.na(hcris_panel$epo_rebates)]= 0
#if the epo_rebates are missing, then fill them with 0

#4. 
#(a)
hcris_panel[is.na(epo_cost) & !is.na(epo_net_cost) & epo_rebates== 0, epo_cost := epo_net_cost]
#Where epo_cost is missing, epo_net_cost is not missing, and epo_rebates is equal to zero, make epo_cost = epo_net_cost.
#(b)
hcris_panel[is.na(epo_cost) & !is.na(epo_net_cost) & epo_rebates!=0, epo_cost:=epo_rebates]
#Where epo_cost is missing, epo_net_cost is not missing, and epo_rebates does not equal zero, make epo_cost = epo_rebates.
#(c)
hcris_panel[is.na(epo_cost) & is.na(epo_net_cost) & epo_rebates== 0, `:=`(epo_cost=0,epo_net_cost=0)]
#Where epo_cost is missing, epo_net_cost is missing, and epo_rebates equals zero, make epo_cost = 0 and epo_net_cost=0 at the same time.
#(e)
hcris_panel[!is.na(epo_cost) & is.na(epo_net_cost), epo_net_cost:= epo_cost - epo_rebates]
#Where epo_cost is not missing, epo_net_cost is missing, make epo_net_cost = epo_cost - epo_rebates

#5
hcris_panel[epo_cost<epo_net_cost, ':='(epo_cost=epo_net_cost, epo_net_cost = epo_cost)]
#Where epo_cost is less than epo_net_cost, make epo_cost equal to epo_net_cost and make epo_net_cost equal to epo_cost at the same time.

#6
hcris_panel[prvdr_num==322664, prvdr_num:= 342664]
# When provider number is equal to 322664 change it to 342664

#7
mdy(hcris_panel$fy_bgn_dt)
mdy(hcris_panel$fy_end_dt)
mdy(hcris_panel$report_start_date)
mdy(hcris_panel$report_end_date)
# Used lubridate to made the fiscal year beginning and end data and report start and end date uniform

#8
hcris_panel[,report_start_date := NULL]
hcris_panel[,report_end_date := NULL]
# We got rid of the report start and end data columns 

#9
#(a)
hcris_panel[, zip_code := trimws(zip_code)]
hcris_panel[, zip_code := substr(zip_code,1,5)]
#We trimmed the zipcode to get rid of spaces.
#Then we used substr() to keep only the 1-5 values.

#(b)

hcris_panel[ , Nzip := length(unique(zip_code[!is.na(zip_code)])), by= prvdr_num]
hcris_panel[Nzip == 1, zip_code := unique(zip_code[!is.na(zip_code)]), by = prvdr_num] 
sum(is.na(hcris_panel$zip_code))
# We made an additional column named Nzip that counted the number of unique zipcodes there were. 
# Then where Nzip was == 1 (no including Nas), we assigned that unique zip code 
#to missing values with the same provider number
#We then used the third line of code to check if ther were less missing zipcodes.

#(c)
# But if there are multiple unique values then use the modal zip code to fill in the NAs
mode = function(x){
  uniqx = unique(x[!is.na(x)])
  uniqx[which.max(tabulate(match(x,uniqx)))]
}
hcris_panel[ Nzip > 1, zip_code:= mode(zip_code), by= prvdr_num]
#If Nzip was greater than 1, then we found the most common zip code for that provider number 
#We then assigned the missing zipcodes by provider number to the modal zipcode.

#10
hcris_panel[ Nzip!=0 ,state := reverse_zipcode(zip_code[!is.na(zip_code)])$state, by = prvdr_num]
# When Nzip did not equal zero, we used the reverse zipcode function to fill in the 
#states with the corresponding zipcode(non missing zipcodes) , by provider number.

#11
hcris_panel[, sort(unique(hcris_panel$chain_identity))]
hcris_panel[grepl("^FRE", chain_identity), chain_identity := "FRESENIUS" ]
hcris_panel[grepl("^FER", chain_identity), chain_identity := "FRESENIUS" ]
hcris_panel[grepl("^FES", chain_identity), chain_identity := "FRESENIUS" ]
hcris_panel[grepl("^FEN", chain_identity), chain_identity := "FRESENIUS" ]
hcris_panel[grepl("^FRS", chain_identity), chain_identity := "FRESENIUS" ]
hcris_panel[grepl("^FR4", chain_identity), chain_identity := "FRESENIUS" ]
hcris_panel[grepl("^FRR", chain_identity), chain_identity := "FRESENIUS" ]

hcris_panel[grepl("^DAV", chain_identity), chain_identity := "DAVITA" ]
hcris_panel[grepl("^DAT", chain_identity), chain_identity := "DAVITA" ]
hcris_panel[grepl("^DAN", chain_identity), chain_identity := "DAVITA" ]
hcris_panel[grepl("^DAC", chain_identity), chain_identity := "DAVITA" ]
# This is how we accounted for misspellings in Fresenius and Davita. 


hcris_panel[is.na(chain_indicator), chain_indicator := "N"]
# If the chain indicator was missing we assigned  it "N'
hcris_panel[chain_indicator == "N" , chain_id := 0 ]
#If the chain indicator was "N", than we made a new column and assigned filled in zeros
hcris_panel[chain_indicator == "Y" & chain_identity != c("DAVITA", "FRESENIUS" ) , chain_id := 1]
#If the chain indicator was "Y" and the Chain identity was not Davita or Fresenius, then we made the chain id 1
hcris_panel[chain_id == 0, chain_identity := "Independent"]
#If the chain id was zero then we made the chain identity "Independent"
hcris_panel[chain_id == 1 , chain_identity :=  "Other"]
#If the chain id was was hen we made the chain identity "Other"
hcris_panel[chain_identity == "FRESENIUS", chain_id := 3 ]
#If the chain identity was "Fresenius" then we made the chain id = 3
hcris_panel[chain_identity == "DAVITA", chain_id := 2 ]
#If the chain identity was was Davita then we made the chain id = 2

#12

hcris_panel[, chain_indicator := chain_identity]
#We made chain indicator the same as chain identity
hcris_panel[, chain_indicator:= NULL ]
#We deleted the chain idicator colums 
clean = hcris_panel
Cleaned_data = hcris_panel
#I added new names for the hcris_panel so that future code would call the same data.



